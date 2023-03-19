use std::convert::Infallible;

use aiahr_core::{
    diagnostic::{Citation, Diagnostic},
    displayer::Displayer,
    id::{ItemId, ModuleId, VarId},
    ident::Ident,
    span::Span,
    ty::{
        row::{ClosedRow, RowLabel},
        TypeVarOf,
    },
};
use pretty::{docs, DocAllocator};

use aiahr_core::ty::infer::{InArena, InferRow, InferTy, UnifierToTcVarError};

/// Errors that may be produced during type checking
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TypeCheckError<'ctx> {
    /// A variable we expected to exist with a type, did not
    VarNotDefined(VarId),
    ItemNotDefined((ModuleId, ItemId)),
    TypeMismatch(InferTy<'ctx>, InferTy<'ctx>),
    OccursCheckFailed(TypeVarOf<InArena<'ctx>>, InferTy<'ctx>),
    UnifierToTcVar(UnifierToTcVarError),
    RowsNotDisjoint(ClosedRow<InArena<'ctx>>, ClosedRow<InArena<'ctx>>, RowLabel),
    RowsNotEqual(InferRow<'ctx>, InferRow<'ctx>),
    UndefinedEffectSignature(ClosedRow<InArena<'ctx>>),
    UndefinedEffect(Ident),
    UnsolvedHandle {
        handler: TypeVarOf<InArena<'ctx>>,
        eff: TypeVarOf<InArena<'ctx>>,
    },
}

pub(crate) fn into_diag(
    db: &dyn crate::Db,
    err: TypeCheckError<'_>,
    span: Span,
) -> TypeCheckDiagnostic {
    let d: pretty::Arena<'_, ()> = pretty::Arena::new();
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
                        &d,
                        left.pretty(&d, db.as_core_db(), &()),
                        d.softline(),
                        "!=",
                        d.softline(),
                        right.pretty(&d, db.as_core_db(), &())
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
        TypeCheckError::OccursCheckFailed(var, ty) => {
            let doc = d
                .intersperse(
                    [
                        d.text("cycle detected for type variable"),
                        pretty::Pretty::pretty(var, &d),
                        d.text("with inferred type"),
                        ty.pretty(&d, db.as_core_db(), &()),
                    ],
                    d.space(),
                )
                .into_doc();
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Infinite Type",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::RowsNotDisjoint(left, right, lbl) => {
            let doc = d
                .text("rows overlap on label:")
                .append(d.space())
                .append(d.text(lbl.text(db.as_core_db())))
                .append(
                    d.hardline()
                        .append(left.pretty(&d, db.as_core_db(), &()))
                        .append(d.hardline())
                        .append(right.pretty(&d, db.as_core_db(), &()))
                        .nest(2),
                );
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Data Rows not Disjoint",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::RowsNotEqual(left, right) => {
            let doc = d
                .text("expected rows to be equal")
                .append(d.hardline())
                .append(
                    left.pretty(&d, db.as_core_db(), &())
                        .append(d.hardline())
                        .append(right.pretty(&d, db.as_core_db(), &()))
                        .nest(2),
                );
            let mut message = String::new();
            doc.render_fmt(width, &mut message).unwrap();
            TypeCheckDiagnostic {
                name: "Rows Mismatch",
                principal: Citation { span, message },
            }
        }
        TypeCheckError::UndefinedEffectSignature(signature) => {
            let doc = d
                .text("could not find an effect signature matching handler:")
                .append(d.softline())
                .append(signature.pretty(&d, db.as_core_db(), &()));
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
        TypeCheckError::ItemNotDefined(_) => todo!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCheckDiagnostic {
    pub name: &'static str,
    pub principal: Citation,
}
impl Diagnostic for TypeCheckDiagnostic {
    fn name(&self) -> &'static str {
        self.name
    }

    fn principal<M: Displayer<ModuleId>>(&self, _: &M) -> Citation {
        self.principal.clone()
    }

    fn additional<M: Displayer<ModuleId>>(&self, _: &M) -> Vec<Citation> {
        // TODO: allow for additional citations
        vec![]
    }
}

impl<'ty> From<Infallible> for TypeCheckError<'ty> {
    fn from(_never: Infallible) -> Self {
        panic!("Function with Infallible parameter was called.")
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
