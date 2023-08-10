use std::convert::Infallible;

use aiahr_core::{
    diagnostic::{tc::TypeCheckDiagnostic, Citation},
    id::VarId,
    ident::Ident,
    pretty::{PrettyPrint, PrettyWithCtx},
    span::Span,
};
use aiahr_ty::{
    infer::{InArena, InferTy, ScopedInferRow, SimpleInferRow, UnifierToTcVarError},
    row::{Row, RowLabel, ScopedClosedRow, SimpleClosedRow},
    ScopedRowVarOf, SimpleRowVarOf, TypeVarOf,
};

use pretty::{docs, DocAllocator, RcAllocator};

/// Errors that may be produced during type checking
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TypeCheckError<'ctx> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    TypeMismatch(InferTy<'ctx>, InferTy<'ctx>),
    TypeOccursCheckFailed(TypeVarOf<InArena<'ctx>>, InferTy<'ctx>),
    DataRowOccursCheckFailed(
        SimpleRowVarOf<InArena<'ctx>>,
        SimpleClosedRow<InArena<'ctx>>,
    ),
    EffectRowOccursCheckFailed(
        ScopedRowVarOf<InArena<'ctx>>,
        ScopedClosedRow<InArena<'ctx>>,
    ),
    UnifierToTcVar(UnifierToTcVarError),
    RowsNotDisjoint(
        SimpleClosedRow<InArena<'ctx>>,
        SimpleClosedRow<InArena<'ctx>>,
        RowLabel,
    ),
    DataRowsNotEqual(SimpleInferRow<'ctx>, SimpleInferRow<'ctx>),
    EffectRowsNotEqual(ScopedInferRow<'ctx>, ScopedInferRow<'ctx>),
    UndefinedEffectSignature(SimpleClosedRow<InArena<'ctx>>),
    UndefinedEffect(Ident),
    UnsolvedHandle {
        handler: SimpleRowVarOf<InArena<'ctx>>,
        eff: ScopedRowVarOf<InArena<'ctx>>,
    },
}

impl From<Infallible> for TypeCheckError<'_> {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}
impl<'ctx>
    From<(
        SimpleClosedRow<InArena<'ctx>>,
        SimpleClosedRow<InArena<'ctx>>,
    )> for TypeCheckError<'ctx>
{
    fn from(
        (left, right): (
            SimpleClosedRow<InArena<'ctx>>,
            SimpleClosedRow<InArena<'ctx>>,
        ),
    ) -> Self {
        Self::DataRowsNotEqual(Row::Closed(left), Row::Closed(right))
    }
}
impl<'ctx>
    From<(
        ScopedClosedRow<InArena<'ctx>>,
        ScopedClosedRow<InArena<'ctx>>,
    )> for TypeCheckError<'ctx>
{
    fn from(
        (left, right): (
            ScopedClosedRow<InArena<'ctx>>,
            ScopedClosedRow<InArena<'ctx>>,
        ),
    ) -> Self {
        Self::EffectRowsNotEqual(Row::Closed(left), Row::Closed(right))
    }
}

impl<'ty> From<UnifierToTcVarError> for TypeCheckError<'ty> {
    fn from(err: UnifierToTcVarError) -> Self {
        Self::UnifierToTcVar(err)
    }
}
impl<'ctx> From<(InferTy<'ctx>, InferTy<'ctx>)> for TypeCheckError<'ctx> {
    fn from((left, right): (InferTy<'ctx>, InferTy<'ctx>)) -> Self {
        TypeCheckError::TypeMismatch(left, right)
    }
}

pub(crate) fn into_diag(
    db: &dyn crate::Db,
    err: TypeCheckError<'_>,
    span: Span,
) -> TypeCheckDiagnostic {
    let d = &RcAllocator;
    // TODO: We should probably store the doc in diagnostic directly so we can lay it out at print
    // time when we have more info
    let width = 80;
    match err {
        TypeCheckError::TypeMismatch(left, right) => {
            let doc = d
                .as_string("Type mismatch:")
                .append(d.softline())
                .append(
                    docs![
                        d,
                        left.pretty_with(&(db.as_ty_db(), ())).pprint(),
                        d.softline(),
                        "!=",
                        d.softline(),
                        right.pretty_with(&(db.as_ty_db(), ())).pprint()
                    ]
                    .nest(2),
                )
                .into_doc();
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Type Mismatch",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::TypeOccursCheckFailed(var, ty) => {
            let doc = d
                .intersperse(
                    [
                        d.text("cycle detected for type variable"),
                        pretty::Pretty::pretty(var, d),
                        d.text("with inferred type"),
                        ty.pretty_with(&(db.as_ty_db(), ())).pprint(),
                    ],
                    d.space(),
                )
                .into_doc();
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Cyclic Type",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::DataRowOccursCheckFailed(var, row) => {
            let doc = d
                .intersperse(
                    [
                        d.text("cycle detected for row variable"),
                        pretty::Pretty::pretty(var, d),
                        d.text("with inferred row"),
                        row.pretty_with(&(db.as_ty_db(), ())).pprint(),
                    ],
                    d.space(),
                )
                .into_doc();
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Cyclic Data Row",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::EffectRowOccursCheckFailed(var, row) => {
            let message = d
                .intersperse(
                    [
                        d.text("cycle detected for row variable"),
                        pretty::Pretty::pretty(var, d),
                        d.text("with inferred row"),
                        row.pretty_with(&(db.as_ty_db(), ())).pprint(),
                    ],
                    d.space(),
                )
                .into_doc()
                .pretty(width)
                .to_string();
            TypeCheckDiagnostic {
                name: "Cyclic Effect Row",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::RowsNotDisjoint(left, right, lbl) => {
            let doc = d
                .text("rows overlap on label:")
                .append(d.space())
                .append(lbl.pretty_with(db).pprint())
                .append(
                    d.hardline()
                        .append(left.pretty_with(&(db.as_ty_db(), ())).pprint())
                        .append(d.hardline())
                        .append(right.pretty_with(&(db.as_ty_db(), ())).pprint())
                        .nest(2),
                );
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Data Rows not Disjoint",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::DataRowsNotEqual(left, right) => {
            let message = d
                .text("expected data rows to be equal")
                .append(d.hardline())
                .append(
                    left.pretty_with(&(db.as_ty_db(), ()))
                        .pprint()
                        .append(d.hardline())
                        .append(right.pretty_with(&(db.as_ty_db(), ())).pprint())
                        .nest(2),
                )
                .pretty(width)
                .to_string();
            TypeCheckDiagnostic {
                name: "Data Rows Mismatch",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::EffectRowsNotEqual(left, right) => {
            let message = d
                .text("expected effect rows to be equal")
                .append(d.hardline())
                .append(
                    left.pretty_with(&(db.as_ty_db(), ()))
                        .pprint()
                        .append(d.hardline())
                        .append(right.pretty_with(&(db.as_ty_db(), ())).pprint())
                        .nest(2),
                )
                .pretty(width)
                .to_string();
            TypeCheckDiagnostic {
                name: "Effect Rows Mismatch",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::UndefinedEffectSignature(signature) => {
            let doc = d
                .text("could not find an effect signature matching handler:")
                .append(d.softline())
                .append(signature.pretty_with(&(db.as_ty_db(), ())).pprint());
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Unexpected Handler",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::UndefinedEffect(eff_name) => TypeCheckDiagnostic {
            name: "Undefined Effect",
            principal: Citation {
                span,
                message: format!(
                    "could not find an effect defintion for: {}",
                    eff_name.text(db.as_core_db())
                ),
            },
        },
        TypeCheckError::UnsolvedHandle { .. } => {
            // TODO figure out a better error message this
            TypeCheckDiagnostic {
                name: "Ambiguous Handler Type",
                principal: Citation {
                    span,
                    message: "could not infer a type of handler, consider adding an annotation"
                        .to_string(),
                },
            }
        }
        // ICE
        TypeCheckError::UnifierToTcVar(_) => {
            panic!("InternalCompilerError: UnifierToTcVar encountered")
        }
        // TODO: Requires being able to map IDs back to names
        TypeCheckError::VarNotDefined(var) => TypeCheckDiagnostic {
            name: "Undefined Variable",
            principal: Citation {
                span,
                message: format!("undefined variable v<{}>", var.0),
            },
        },
    }
}
