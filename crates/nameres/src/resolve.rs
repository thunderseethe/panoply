use crate::{
    base::BaseNames,
    name::{BaseName, ModuleName, Name, NameKinded},
    names::Names,
    ops::{IdOps, InsertResult},
};
use ::base::{
    diagnostic::{
        nameres::{NameKind, NameKinds, NameResolutionError, RejectionReason, Suggestion},
        DiagnosticSink,
    },
    id::{EffectName, EffectOpName, TermName, TyVarId, VarId},
    ident::Ident,
    indexed::{HasArenaRef, IdxAlloc},
    modules::Module,
    option::Transpose,
    span::{Span, SpanOf, Spanned},
};
use bumpalo::Bump;
use cst::{
    nameres::{self as nst, AllocItem, NstIndxAlloc},
    Annotation, CstIndxAlloc, Field, IdField,
};
use la_arena::Idx;

const TERM_KINDS: NameKinds = NameKinds::EFFECT_OP
    .union(NameKinds::ITEM)
    .union(NameKinds::VAR);

pub(crate) struct NameResCtx<'a, 'b, 'c, E> {
    int_ty_ident: Ident,
    cst_alloc: &'b CstIndxAlloc,
    arena: &'a Bump,
    alloc: &'b mut NstIndxAlloc,
    names: &'b mut Names<'c>,
    errors: &'b mut E,
}

impl<'b, E> NameResCtx<'_, 'b, '_, E> {
    fn view<T>(&self, idx: Idx<T>) -> &'b T
    where
        CstIndxAlloc: HasArenaRef<T>,
    {
        &self.cst_alloc.arena()[idx]
    }
}

impl<'a, E> NameResCtx<'a, '_, '_, E> {
    // Tries to map the given function over the elements of `separated`, returning all errors.
    fn resolve_separated<A, B, F>(
        &mut self,
        separated: &cst::Separated<A>,
        f: F,
    ) -> Option<nst::Separated<B>>
    where
        F: FnMut(&mut Self, &A) -> Option<B>,
    {
        let mut f = f;
        let first = f(self, &separated.first);
        let elems = separated
            .elems
            .iter()
            .map(|(c, a)| Some((*c, f(self, a)?)))
            .collect::<Option<Vec<_>>>()
            .unwrap_or_default();
        Some(nst::Separated {
            first: first?,
            elems,
            comma: separated.comma,
        })
    }

    // Tries to map the given function over the targets of `prod`.
    fn resolve_product_row<A, B, F>(
        &mut self,
        prod: &cst::ProductRow<A>,
        f: F,
    ) -> Option<nst::ProductRow<B>>
    where
        F: FnMut(&mut Self, &A) -> Option<B>,
    {
        let mut f = f;
        let fields = if let Some(cs) = &prod.fields {
            Some(self.resolve_separated(cs, |me, field| resolve_id_field(field, |a| f(me, a)))?)
        } else {
            None
        };
        Some(nst::ProductRow {
            lbrace: prod.lbrace,
            fields,
            rbrace: prod.rbrace,
        })
    }
}

// Tries to map the given function over the target.
fn resolve_id_field<A, B, F>(field: &IdField<A>, f: F) -> Option<IdField<B>>
where
    F: FnOnce(&A) -> Option<B>,
{
    Some(IdField {
        label: field.label,
        sep: field.sep,
        target: f(&field.target)?,
    })
}

// Tries to map the given function over the target of `sum`.
fn resolve_sum_row<A, B, F>(sum: &cst::SumRow<A>, f: F) -> Option<nst::SumRow<B>>
where
    F: FnOnce(&A) -> Option<B>,
{
    Some(nst::SumRow {
        langle: sum.langle,
        field: resolve_id_field(&sum.field, f)?,
        rangle: sum.rangle,
    })
}

// The possible meanings of a `DotAccess` term.
#[derive(Debug)]
enum DotResolution {
    Module(Module),
    Effect(EffectName),
    EffectOp(EffectOpName),
    Item(TermName),
    FieldAccess {
        base: Idx<nst::Term>,
        dot: Span,
        field: SpanOf<Ident>,
    },
}

impl From<BaseName> for DotResolution {
    fn from(base: BaseName) -> Self {
        match base {
            BaseName::Module(m) => DotResolution::Module(m),
            BaseName::Effect(e) => DotResolution::Effect(e),
            BaseName::EffectOp(o) => DotResolution::EffectOp(o),
            BaseName::Item(i) => DotResolution::Item(i),
        }
    }
}

// Resolves a symbol or suggests names that the user might have intended.
fn resolve_or_suggest<I, N, T, F>(
    iterator: I,
    names: &Names<'_>,
    mut f: F,
) -> Result<T, Vec<Suggestion>>
where
    N: Copy,
    I: Iterator<Item = SpanOf<N>>,
    F: FnMut(N) -> Result<T, RejectionReason>,
    Name: From<N>,
{
    let mut suggestions = Vec::new();
    for name in iterator {
        match f(name.value) {
            Ok(x) => {
                return Ok(x);
            }
            Err(why_not) => {
                suggestions.push(Suggestion {
                    name: names.get(name.value),
                    why_not,
                });
            }
        }
    }
    Err(suggestions)
}

impl<'a, E> NameResCtx<'a, '_, '_, E>
where
    E: DiagnosticSink<NameResolutionError>,
{
    // Resolves a symbol to a value of type `T`, using the given function to decide which names are
    // valid for the symbol.
    fn resolve_symbol<T, F>(&mut self, var: SpanOf<Ident>, f: F) -> Option<T>
    where
        F: FnMut(Name) -> Result<T, RejectionReason>,
    {
        match resolve_or_suggest(self.names.find(var.value), self.names, f) {
            Ok(x) => Some(x),
            Err(suggestions) => {
                self.errors.add(NameResolutionError::NotFound {
                    name: var,
                    context_module: None,
                    suggestions,
                });
                None
            }
        }
    }

    // Resolves a symbol in a given module to a value of type `T`, using the given function to decide
    // which names are valid for the symbol.
    fn resolve_symbol_in<T, F>(&mut self, module: Module, var: SpanOf<Ident>, f: F) -> Option<T>
    where
        F: FnMut(BaseName) -> Result<T, RejectionReason>,
    {
        match resolve_or_suggest(self.names.find_in(module, var.value), self.names, f) {
            Ok(x) => Some(x),
            Err(suggestions) => {
                self.errors.add(NameResolutionError::NotFound {
                    name: var,
                    context_module: None,
                    suggestions,
                });
                None
            }
        }
    }
    // Inserts a symbol into the current scope using the given function.
    fn insert_symbol<I, F>(&mut self, kind: NameKind, var: SpanOf<Ident>, f: F) -> SpanOf<I>
    where
        F: FnOnce(&mut Names<'_>, SpanOf<Ident>) -> InsertResult<I>,
    {
        let InsertResult { id, existing } = f(self.names, var);
        if let Some(orig) = existing {
            self.errors.add(NameResolutionError::Duplicate {
                name: var.value,
                kind,
                original: orig.span(),
                duplicate: var.span(),
            });
        }
        var.span().of(id)
    }

    // Inserts a variable into the current scope.
    fn insert_ty_var(&mut self, var: SpanOf<Ident>) -> SpanOf<TyVarId> {
        self.insert_symbol(NameKind::TyVar, var, |names, v| names.insert_ty_var(v))
    }
    // Inserts a variable into the current scope.
    fn insert_var(&mut self, var: SpanOf<Ident>) -> SpanOf<VarId> {
        self.insert_symbol(NameKind::Var, var, |names, v| names.insert_var(v))
    }

    // Resolves an effect operation symbolto a value of type `T`, using the given function to decide which names are valid for the symbol.
    fn resolve_operation_symbol<T, F>(
        &mut self,
        effect: EffectName,
        var: SpanOf<Ident>,
        f: F,
    ) -> Option<T>
    where
        F: FnMut(EffectOpName) -> Result<T, RejectionReason>,
    {
        match resolve_or_suggest(
            self.names.get_effect(&effect).find(var.value),
            self.names,
            f,
        ) {
            Ok(x) => Some(x),
            Err(suggestions) => {
                self.errors.add(NameResolutionError::NotFound {
                    name: var,
                    context_module: None,
                    suggestions,
                });
                None
            }
        }
    }

    // Resolves a symbol to a type name.
    fn resolve_type_symbol(&mut self, var: SpanOf<Ident>) -> Option<SpanOf<TyVarId>> {
        Some(var.map(|id| {
            let existing = self.names.find(id).find_map(|sn| match sn.value {
                Name::TyVar(t) => Some(t),
                _ => None,
            });
            existing.unwrap_or_else(|| self.insert_ty_var(var).value)
        }))
    }

    // Resolves a type-level row.
    fn resolve_row<C, D, F>(
        &mut self,
        row: &cst::Row<Ident, C>,
        mut f: F,
    ) -> Option<nst::Row<TyVarId, D>>
    where
        F: FnMut(&mut Self, &C) -> Option<D>,
    {
        Some(match row {
            cst::Row::Concrete(concrete) => {
                nst::Row::Concrete(self.resolve_separated(concrete, |me, c| f(me, c))?)
            }
            cst::Row::Variable(variables) => nst::Row::Variable(
                self.resolve_separated(variables, |me, var| me.resolve_type_symbol(*var))?,
            ),
            cst::Row::Mixed {
                concrete,
                vbar,
                variables,
            } => {
                let concrete = self.resolve_separated(concrete, |me, c| f(me, c));
                let variables =
                    self.resolve_separated(variables, |me, var| me.resolve_type_symbol(*var));
                nst::Row::Mixed {
                    concrete: concrete?,
                    vbar: *vbar,
                    variables: variables?,
                }
            }
        })
    }

    fn mk_ty(&mut self, type_: nst::Type<TyVarId>) -> Idx<nst::Type<TyVarId>> {
        self.alloc.alloc(type_)
    }

    /// Resolves a type.
    pub fn resolve_type(
        &mut self,
        type_: Idx<cst::Type<Ident>>,
    ) -> Option<Idx<nst::Type<TyVarId>>> {
        let type_ = match self.view(type_) {
            cst::Type::Int(_) => unreachable!("We parse Int as a Named type"),
            cst::Type::Named(var) if var.value == self.int_ty_ident => nst::Type::Int(var.span()),
            cst::Type::Named(var) => nst::Type::Named(self.resolve_type_symbol(*var)?),
            cst::Type::Sum {
                langle,
                variants,
                rangle,
            } => nst::Type::Sum {
                langle: *langle,
                variants: self.resolve_type_row(variants)?,
                rangle: *rangle,
            },
            cst::Type::Product {
                lbrace,
                fields,
                rbrace,
            } => nst::Type::Product {
                lbrace: *lbrace,
                fields: fields
                    .as_ref()
                    .map(|fields| self.resolve_type_row(fields))
                    .transpose()?,
                rbrace: *rbrace,
            },
            cst::Type::Function {
                domain,
                arrow,
                codomain,
            } => {
                let domain = self.resolve_type(*domain);
                let codomain = self.resolve_type(*codomain);
                nst::Type::Function {
                    domain: domain?,
                    arrow: *arrow,
                    codomain: codomain?,
                }
            }
            cst::Type::Parenthesized { lpar, type_, rpar } => nst::Type::Parenthesized {
                lpar: *lpar,
                type_: self.resolve_type(*type_)?,
                rpar: *rpar,
            },
        };
        Some(self.mk_ty(type_))
    }

    // Resolves a row of types.
    fn resolve_type_row(&mut self, row: &cst::TypeRow<Ident>) -> Option<nst::TypeRow<TyVarId>> {
        self.resolve_row(row, |me, field| {
            resolve_id_field(field, |ty| me.resolve_type(*ty))
        })
    }

    // Resolves a row atom.
    fn resolve_row_atom(&mut self, atom: &cst::RowAtom<Ident>) -> Option<nst::RowAtom<TyVarId>> {
        Some(match atom {
            cst::RowAtom::Concrete { lpar, fields, rpar } => nst::RowAtom::Concrete {
                lpar: *lpar,
                fields: self.resolve_separated(fields, |me, field| {
                    resolve_id_field(field, |ty| me.resolve_type(*ty))
                })?,
                rpar: *rpar,
            },
            cst::RowAtom::Variable(var) => nst::RowAtom::Variable(self.resolve_type_symbol(*var)?),
        })
    }

    // Resolves a type constraint.
    fn resolve_constraint(
        &mut self,
        constraint: &cst::Constraint<Ident>,
    ) -> Option<nst::Constraint<TyVarId>> {
        Some(match constraint {
            cst::Constraint::RowSum {
                lhs,
                plus,
                rhs,
                eq,
                goal,
            } => {
                let lhs = self.resolve_row_atom(lhs);
                let rhs = self.resolve_row_atom(rhs);
                let goal = self.resolve_row_atom(goal);
                nst::Constraint::RowSum {
                    lhs: lhs?,
                    plus: *plus,
                    rhs: rhs?,
                    eq: *eq,
                    goal: goal?,
                }
            }
        })
    }

    // Resolves a set of quantifiers.
    fn resolve_qualifiers(
        &mut self,
        qualifiers: &cst::Qualifiers<Ident>,
    ) -> Option<nst::Qualifiers<TyVarId>> {
        Some(nst::Qualifiers {
            constraints: self.resolve_separated(&qualifiers.constraints, |me, constraint| {
                me.resolve_constraint(constraint)
            })?,
            arrow: qualifiers.arrow,
        })
    }

    // Resolves a quantifier.
    fn resolve_quantifier(
        &mut self,
        quantifier: &cst::Quantifier<Ident>,
    ) -> cst::Quantifier<TyVarId> {
        cst::Quantifier {
            forall: quantifier.forall,
            var: self.insert_ty_var(quantifier.var),
            dot: quantifier.dot,
        }
    }

    /// Resolves a polymorphic type.
    pub fn resolve_scheme(&mut self, scheme: &cst::Scheme<Ident>) -> Option<nst::Scheme<TyVarId>> {
        self.subscope(|scope| {
            let quantifiers = scheme
                .quantifiers
                .iter()
                .map(|quant| scope.resolve_quantifier(quant))
                .collect();
            let qualifiers = scheme
                .qualifiers
                .as_ref()
                .map(|quals| scope.resolve_qualifiers(quals))
                .transpose();
            let type_ = scope.resolve_type(scheme.type_);
            Some(nst::Scheme {
                quantifiers,
                qualifiers: qualifiers?,
                type_: type_?,
            })
        })
    }

    fn subscope<A>(&mut self, body: impl FnOnce(&mut NameResCtx<'a, '_, '_, E>) -> A) -> A {
        self.names.subscope(|names| {
            let mut ctx = NameResCtx {
                int_ty_ident: self.int_ty_ident,
                cst_alloc: self.cst_alloc,
                arena: self.arena,
                alloc: self.alloc,
                errors: self.errors,
                names,
            };
            body(&mut ctx)
        })
    }

    fn mk_pat(&mut self, pat: nst::Pattern) -> Idx<nst::Pattern> {
        self.alloc.alloc(pat)
    }

    /// Resolves the given pattern, accumulating bindings into `names`.
    ///
    /// Note that this currently cannot return `None`, although it can emit errors.
    pub fn resolve_pattern(&mut self, pattern: Idx<cst::Pattern>) -> Option<Idx<nst::Pattern>> {
        let pat = match self.view(pattern) {
            cst::Pattern::ProductRow(pr) => nst::Pattern::ProductRow(
                self.resolve_product_row(pr, |me, target| me.resolve_pattern(*target))?,
            ),
            cst::Pattern::SumRow(sr) => {
                nst::Pattern::SumRow(resolve_sum_row(sr, |target| self.resolve_pattern(*target))?)
            }
            cst::Pattern::Whole(var) => nst::Pattern::Whole(self.insert_var(*var)),
        };
        Some(self.mk_pat(pat))
    }

    // Resolves a type annotation.
    fn resolve_type_annotation(
        &mut self,
        annotation: &cst::TypeAnnotation<Ident>,
    ) -> Option<nst::TypeAnnotation<TyVarId>> {
        Some(Annotation {
            colon: annotation.colon,
            type_: self.resolve_type(annotation.type_)?,
        })
    }

    // Resolves a scheme annotation.
    fn resolve_scheme_annotation(
        &mut self,
        annotation: &cst::SchemeAnnotation<Ident>,
    ) -> Option<nst::SchemeAnnotation<TyVarId>> {
        Some(Annotation {
            colon: annotation.colon,
            type_: self.resolve_scheme(&annotation.type_)?,
        })
    }

    fn span_of(&self, term: Idx<cst::Term>) -> Span {
        self.view(term).spanned(self.cst_alloc).span()
    }

    // Resolves nested `DotAccess` terms.
    fn resolve_nested_dots(
        &mut self,
        base: Idx<cst::Term>,
        dot: Span,
        field: SpanOf<Ident>,
    ) -> Option<DotResolution> {
        Some(match self.view(base) {
            // (base2 . field2) . field
            cst::Term::DotAccess {
                base: base2,
                dot: dot2,
                field: field2,
            } => match self.resolve_nested_dots(*base2, *dot2, *field2)? {
                // m . field
                DotResolution::Module(m) => {
                    self.resolve_symbol_in(m, field, |name| Ok(DotResolution::from(name)))?
                }
                DotResolution::Effect(e) => {
                    self.resolve_operation_symbol(e, field, |o| Ok(DotResolution::EffectOp(o)))?
                }
                DotResolution::EffectOp(_) => {
                    self.errors.add(NameResolutionError::WrongKind {
                        expr: self.span_of(base),
                        actual: NameKind::EffectOp,
                        expected: !NameKinds::EFFECT_OP,
                    });
                    return None;
                }
                DotResolution::Item(i) => DotResolution::FieldAccess {
                    base: self.mk_term(nst::Term::ItemRef(self.span_of(base).of(i))),
                    dot,
                    field,
                },
                // (base2n . field2n) . field
                DotResolution::FieldAccess {
                    base: base2n,
                    dot: dot2n,
                    field: field2n,
                } => DotResolution::FieldAccess {
                    base: self.mk_term(nst::Term::FieldAccess {
                        base: base2n,
                        dot: dot2n,
                        field: field2n,
                    }),
                    dot,
                    field,
                },
            },
            // n . field
            cst::Term::SymbolRef(n) => {
                // Resolution of a symbol to some definition
                #[derive(Clone, Debug)]
                enum SymbolRes {
                    Module(Module),
                    Effect(EffectName),
                    Term(nst::Term),
                }

                let base_span = self.span_of(base);
                let module_or_term = self.resolve_symbol(*n, |name| match name {
                    Name::Module(m) => Ok(SymbolRes::Module(m)),
                    Name::Item(i) => Ok(SymbolRes::Term(nst::Term::ItemRef(base_span.of(i)))),
                    Name::Var(v) => Ok(SymbolRes::Term(nst::Term::VariableRef(base_span.of(v)))),
                    Name::Effect(e) => Ok(SymbolRes::Effect(e)),
                    _ => Err(RejectionReason::WrongKind {
                        actual: name.kind(),
                        expected: NameKinds::MODULE | TERM_KINDS,
                    }),
                })?;
                match module_or_term {
                    SymbolRes::Module(m) => {
                        self.resolve_symbol_in(m, field, |name| Ok(DotResolution::from(name)))?
                    }
                    SymbolRes::Effect(e) => {
                        self.resolve_operation_symbol(e, field, |o| Ok(DotResolution::EffectOp(o)))?
                    }
                    SymbolRes::Term(value) => DotResolution::FieldAccess {
                        base: self.mk_term(value),
                        dot,
                        field,
                    },
                }
            }
            // (expr) . field
            _ => DotResolution::FieldAccess {
                base: self.resolve_term(base)?,
                dot,
                field,
            },
        })
    }

    fn mk_term(&mut self, term: nst::Term) -> Idx<nst::Term> {
        self.alloc.alloc(term)
    }

    /// Resolves the given term.
    pub fn resolve_term(&mut self, term: Idx<cst::Term>) -> Option<Idx<nst::Term>> {
        let term = match self.view(term) {
            cst::Term::Binding {
                var,
                annotation,
                eq,
                value,
                semi,
                expr,
            } => {
                let value = self.resolve_term(*value);
                self.subscope(|ctx| {
                    let var = ctx.insert_var(*var);
                    let annotation = annotation
                        .map(|annotation| ctx.resolve_type_annotation(&annotation))
                        .transpose();
                    let expr = ctx.resolve_term(*expr);

                    Some(nst::Term::Binding {
                        var,
                        annotation: annotation?,
                        eq: *eq,
                        value: value?,
                        semi: *semi,
                        expr: expr?,
                    })
                })?
            }
            cst::Term::Handle {
                with,
                handler,
                do_,
                expr,
            } => {
                let handler = self.resolve_term(*handler);
                let expr = self.resolve_term(*expr);
                nst::Term::Handle {
                    with: *with,
                    handler: handler?,
                    do_: *do_,
                    expr: expr?,
                }
            }
            cst::Term::Abstraction {
                lbar,
                arg,
                annotation,
                rbar,
                body,
            } => self.subscope(|scope| {
                let arg = scope.insert_var(*arg);
                let annotation = annotation
                    .map(|annotation| scope.resolve_type_annotation(&annotation))
                    .transpose();
                let body = scope.resolve_term(*body);
                Some(nst::Term::Abstraction {
                    lbar: *lbar,
                    arg,
                    annotation: annotation?,
                    rbar: *rbar,
                    body: body?,
                })
            })?,
            cst::Term::Application {
                func,
                lpar,
                arg,
                rpar,
            } => {
                let func = self.resolve_term(*func);
                let arg = self.resolve_term(*arg);
                nst::Term::Application {
                    func: func?,
                    lpar: *lpar,
                    arg: arg?,
                    rpar: *rpar,
                }
            }
            cst::Term::ProductRow(pr) => nst::Term::ProductRow(
                self.resolve_product_row(pr, |me, target| me.resolve_term(*target))?,
            ),
            cst::Term::SumRow(sr) => {
                nst::Term::SumRow(resolve_sum_row(sr, |target| self.resolve_term(*target))?)
            }
            cst::Term::DotAccess { base, dot, field } => {
                match self.resolve_nested_dots(*base, *dot, *field)? {
                    DotResolution::Module(_) => {
                        self.errors.add(NameResolutionError::WrongKind {
                            expr: self.span_of(term),
                            actual: NameKind::Module,
                            expected: TERM_KINDS,
                        });
                        return None;
                    }
                    DotResolution::Effect(_) => {
                        self.errors.add(NameResolutionError::WrongKind {
                            expr: self.span_of(term),
                            actual: NameKind::Effect,
                            expected: TERM_KINDS,
                        });
                        return None;
                    }
                    DotResolution::EffectOp(o) => nst::Term::EffectOpRef(self.span_of(term).of(o)),
                    DotResolution::Item(i) => nst::Term::ItemRef(self.span_of(term).of(i)),
                    DotResolution::FieldAccess { base, dot, field } => {
                        nst::Term::FieldAccess { base, dot, field }
                    }
                }
            }
            cst::Term::Match {
                match_,
                langle,
                cases,
                rangle,
            } => nst::Term::Match {
                match_: *match_,
                langle: *langle,
                cases: self.resolve_separated(cases, |me, field| {
                    me.subscope(|scope| {
                        let pattern = scope.resolve_pattern(field.label);
                        let target = scope.resolve_term(field.target);
                        Some(Field {
                            label: pattern?,
                            sep: field.sep,
                            target: target?,
                        })
                    })
                })?,
                rangle: *rangle,
            },
            cst::Term::SymbolRef(var) => self.resolve_symbol(*var, |name| match name {
                Name::Item(i) => Ok(nst::Term::ItemRef(var.span().of(i))),
                Name::Var(v) => Ok(nst::Term::VariableRef(var.span().of(v))),
                _ => Err(RejectionReason::WrongKind {
                    actual: name.kind(),
                    expected: TERM_KINDS,
                }),
            })?,
            cst::Term::Parenthesized { lpar, term, rpar } => nst::Term::Parenthesized {
                lpar: *lpar,
                term: self.resolve_term(*term)?,
                rpar: *rpar,
            },
            cst::Term::Concat {
                left,
                concat,
                right,
            } => nst::Term::Concat {
                left: self.resolve_term(*left)?,
                concat: *concat,
                right: self.resolve_term(*right)?,
            },
            cst::Term::Int(int) => nst::Term::Int(*int),
        };
        Some(self.mk_term(term))
    }

    /// Resolves an effect operation signature.
    fn resolve_effect_op(
        &mut self,
        opid: EffectOpName,
        op: &cst::EffectOp<Ident, Ident>,
    ) -> Option<nst::EffectOp<EffectOpName, TyVarId>> {
        Some(nst::EffectOp {
            name: op.name.span().of(opid),
            colon: op.colon,
            type_: self.resolve_type(op.type_)?,
        })
    }

    /// Resolves the given effect.
    fn resolve_effect(
        &mut self,
        eff_name: EffectName,
        eff: &cst::EffectDefn,
    ) -> Option<nst::EffectDefn> {
        Some(nst::EffectDefn {
            effect: eff.effect,
            name: eff.name.span().of(eff_name),
            lbrace: eff.lbrace,
            ops: self
                .names
                .get_effect(&eff_name)
                .iter()
                .collect::<Vec<_>>()
                .into_iter()
                .zip(eff.ops.iter())
                .map(|(opid, op)| self.resolve_effect_op(opid, op))
                .collect(),
            rbrace: eff.rbrace,
        })
    }

    /// Resolves the given item.
    pub fn resolve_term_item(
        &mut self,
        id: TermName,
        term: &cst::TermDefn,
    ) -> Option<nst::TermDefn> {
        Some(nst::TermDefn {
            name: term.name.span().of(id),
            annotation: term
                .annotation
                .as_ref()
                .map(|annotation| self.resolve_scheme_annotation(annotation))
                .transpose()?,
            eq: term.eq,
            value: self.resolve_term(term.value)?,
        })
    }
}

pub(crate) struct ModuleResolution {
    pub(crate) terms: Vec<Option<nst::AllocItem<nst::TermDefn>>>,
    pub(crate) effects: Vec<Option<nst::AllocItem<nst::EffectDefn>>>,
}

/// Resolves the given module.
pub(crate) fn resolve_module<'a, 'b: 'a, E>(
    db: &'a dyn crate::Db,
    arena: &'a Bump,
    module: &cst::CstModule,
    base: BaseNames<'_>,
    errors: &mut E,
) -> ModuleResolution
where
    E: DiagnosticSink<NameResolutionError>,
{
    let mut terms = Vec::new();
    let mut effects = Vec::new();
    for (name, item) in base.iter().zip(module.items.iter()) {
        let mut names = Names::new(&base);
        let mut alloc = NstIndxAlloc::default();
        let mut ctx = NameResCtx {
            int_ty_ident: db.ident_str("Int"),
            cst_alloc: &module.indices,
            arena,
            errors,
            alloc: &mut alloc,
            names: &mut names,
        };
        match (&name, &item) {
            (ModuleName::Effect(e), cst::Item::Effect(ref eff)) => {
                effects.push(ctx.resolve_effect(*e, eff).map(|eff| AllocItem {
                    alloc,
                    local_ids: names.into_local_ids(),
                    item: eff,
                }));
            }
            (ModuleName::Item(i), cst::Item::Term(ref term)) => {
                terms.push(ctx.resolve_term_item(*i, term).map(|term| AllocItem {
                    alloc,
                    local_ids: names.into_local_ids(),
                    item: term,
                }));
            }
            _ => panic!(
                "Expected same kinds of names, got {:?} and {:?}",
                name, item
            ),
        }
    }
    ModuleResolution { terms, effects }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use assert_matches::assert_matches;
    use base::{
        diagnostic::nameres::{NameKind, NameResolutionError},
        file::{FileId, SourceFile, SourceFileSet},
        indexed::ReferenceAllocate,
        span::Span,
        span_of,
    };
    use bumpalo::Bump;
    use cst::nameres::LocalIds;

    use crate::{module::ModuleNames, ops::IdOps, Db as NameResDb};

    use test_utils::{
        assert_ident_text_matches_name, field, id_field, nitem_term, npat_prod, npat_var,
        nst::NstRefAlloc, nterm_abs, nterm_app, nterm_dot, nterm_int, nterm_item, nterm_local,
        nterm_match, nterm_prod, nterm_sum, nterm_var, nterm_with, quant, scheme, type_func,
        type_int, type_named, type_prod,
    };

    #[derive(Default)]
    #[salsa::db(crate::Jar, base::Jar, parser::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    type TestNstItem<'a> = (test_utils::nst::Item<'a>, &'a LocalIds);

    fn parse_resolve_module<'a, S: ToString>(
        db: &'a TestDatabase,
        arena: &'a Bump,
        input: S,
    ) -> (
        Vec<Option<TestNstItem<'a>>>,
        &'a ModuleNames,
        Vec<NameResolutionError>,
    ) {
        let file_id = FileId::new(db, PathBuf::from("test"));
        let file = SourceFile::new(db, file_id, input.to_string());
        let _ = SourceFileSet::new(db, vec![file]);

        let nameres_module = db.nameres_module_for_file(file);
        let terms = nameres_module.terms(db).iter().map(|ot| {
            ot.map(|term| {
                let mut ref_alloc = NstRefAlloc::new(arena, term.alloc(db));
                (
                    test_utils::nst::Item::Term(term.data(db).clone().ref_alloc(&mut ref_alloc)),
                    term.locals(db),
                )
            })
        });
        let effects = nameres_module.effects(db).iter().map(|oe| {
            oe.map(|effect| {
                let mut ref_alloc = NstRefAlloc::new(arena, effect.alloc(db));
                (
                    test_utils::nst::Item::Effect(
                        effect.data(db).clone().ref_alloc(&mut ref_alloc),
                    ),
                    effect.locals(db),
                )
            })
        });

        let errors = db
            .all_nameres_errors()
            .into_iter()
            .map(|err| match err {
                base::diagnostic::error::PanoplyError::NameResolutionError(name_res) => name_res,
                _ => unreachable!(),
            })
            .collect();

        (
            terms.chain(effects).collect(),
            &nameres_module.names(db)[&nameres_module.module(db)],
            errors,
        )
    }

    fn parse_resolve_term<'a>(
        db: &'a TestDatabase,
        arena: &'a Bump,
        input: &str,
    ) -> (
        Option<(&'a test_utils::nst::Term<'a>, &'a LocalIds)>,
        Vec<NameResolutionError>,
    ) {
        // Wrap our term in an item def
        let mut content = "item = ".to_string();
        content.push_str(input);

        let (items, _, errors) = parse_resolve_module(db, arena, content);

        (
            items
                .first()
                .unwrap()
                .as_ref()
                .map(|(item, locals)| match item {
                    // We hard code wrap our input in an item def, an effect isn't possible here
                    test_utils::nst::Item::Effect(_) => unreachable!(),
                    test_utils::nst::Item::Term(term) => (term.value, *locals),
                }),
            errors,
        )
    }

    #[test]
    fn test_local_binding() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x = {}; y = {}; x");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some((nterm_local!(
                x,
                nterm_prod!(),
                nterm_local!(y, nterm_prod!(), nterm_var!(x1))
            ), locals)) => {
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[y].value.text(&db), "y");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_local_binding_types() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x: a = {}; y: {} = {}; x");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some((nterm_local!(
                x,
                type_named!(a),
                nterm_prod!(),
                nterm_local!(y, type_prod!(), nterm_prod!(), nterm_var!(x1))
            ), locals)) => {
                assert_eq!(locals.ty_vars[a].value.text(&db), "a");
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[y].value.text(&db), "y");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_local_binding_shadowing() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x = {}; x = x; x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_local!(
                x_out,
                nterm_prod!(),
                nterm_local!(x_in, nterm_var!(x1), nterm_var!(x2))
            ), locals)) => {
                assert_eq!(locals.vars[x_out].value.text(&db), "x");
                assert_eq!(locals.vars[x_in].value.text(&db), "x");
                assert_eq!(x1, x_out);
                assert_eq!(x2, x_in);
            }
        );
    }

    #[test]
    fn test_local_binding_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x = y; z");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(y),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(z),
                    context_module: None,
                    ..
                }
            ] => {
                assert_eq!(y.text(&db), "y");
                assert_eq!(z.text(&db), "z");
            }
        );
    }

    #[test]
    fn test_handler() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x = {}; with x do x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_local!(
                x,
                nterm_prod!(),
                nterm_with!(nterm_var!(x1), nterm_var!(x2))
            ), locals)) => {
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_handler_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "with h do x");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(h),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(x),
                    context_module: None,
                    ..
                }
            ] => {
                assert_eq!(h.text(&db), "h");
                assert_eq!(x.text(&db), "x");
            }
        );
    }

    #[test]
    fn test_abstraction() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "|x| |y| y(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_abs!(
                x,
                nterm_abs!(
                    y,
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            ), locals)) => {
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[y].value.text(&db), "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
    }

    #[test]
    fn test_abstraction_types() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "|x: {}| |y: a -> b| y(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_abs!(
                x,
                type_prod!(),
                nterm_abs!(
                    y,
                    type_func!(type_named!(a), type_named!(b)),
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            ), locals)) => {
                assert_eq!(locals.ty_vars[a].value.text(&db), "a");
                assert_eq!(locals.ty_vars[b].value.text(&db), "b");
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[y].value.text(&db), "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
            }
        );
    }

    #[test]
    fn test_abstraction_shadowing() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "|x| |x| x(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_abs!(
                x_out,
                nterm_abs!(
                    x_in,
                    nterm_app!(nterm_var!(x1), nterm_var!(x2))
                )
            ), locals)) => {
                assert_eq!(locals.vars[x_out].value.text(&db), "x");
                assert_eq!(locals.vars[x_in].value.text(&db), "x");
                assert_eq!(x1, x_in);
                assert_eq!(x2, x_in);
            }
        );
    }

    #[test]
    fn test_application() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "|x| x(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_abs!(
                x,
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            ), locals)) => {
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_application_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "f(x)");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(f),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(x),
                    context_module: None,
                    ..
                }
            ] => {
                assert_eq!(f.text(&db), "f");
                assert_eq!(x.text(&db), "x");
            }
        );
    }

    #[test]
    fn test_product_row() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x = {}; {a = x, b = {x = x}}");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_local!(x_var, nterm_prod!(),
                nterm_prod!(
                id_field!(a, nterm_var!(x1)),
                id_field!(
                    b,
                    nterm_prod!(id_field!(x, nterm_var!(x2)))
                ),
            )), locals)) => {
                assert_ident_text_matches_name!(db, [a, b, x]);
                assert_eq!(locals.vars[x_var].value.text(&db), "x");
                assert_eq!(x_var, x1);
                assert_eq!(x_var, x2);
            }
        );
    }

    #[test]
    fn test_product_row_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "{x = y, z = x}");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(y),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(x),
                    context_module: None,
                    ..
                }
            ] => {
                assert_eq!(y.text(&db), "y");
                assert_eq!(x.text(&db), "x");
            }
        );
    }

    #[test]
    fn test_sum_row() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "|x| <a = x>");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_abs!(x, nterm_sum!(id_field!(a, nterm_var!(x1)))), locals)) => {
                assert_ident_text_matches_name!(db, a);
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_sum_row_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "<x = x>");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [NameResolutionError::NotFound {
                name: span_of!(x),
                context_module: None,
                ..
            }] => {
                assert_eq!(x.text(&db), "x");
            }
        );
    }

    #[test]
    fn test_dot_access() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "id = |x| x; {x = id}.x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_local!(
                id,
                nterm_abs!(x_var, nterm_var!(x_var1)),
                nterm_dot!(
                    nterm_prod!(id_field!(x, nterm_var!(id1))),
                    x_id1
                )
            ), locals)) => {
                assert_eq!(locals.vars[id].value.text(&db), "id");
                assert_eq!(locals.vars[x_var].value.text(&db), "x");
                assert_eq!(x_var1, x_var);
                assert_eq!(id1, id);
                assert_ident_text_matches_name!(db, x);
                assert_eq!(x, x_id1);
            }
        );
    }

    #[test]
    fn test_dot_access_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x.a");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [NameResolutionError::NotFound {
                name: span_of!(x),
                context_module: None,
                ..
            }] => {
                assert_eq!(x.text(&db), "x");
            }
        );
    }

    #[test]
    fn test_match() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "match <{a = x} => x, y => y>");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some((nterm_match!(
                field!(npat_prod!(id_field!(a, npat_var!(x))), nterm_var!(x1)),
                field!(npat_var!(y), nterm_var!(y1))
            ), locals)) => {
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[y].value.text(&db), "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
                assert_ident_text_matches_name!(db, [a]);
            }
        );
    }

    #[test]
    fn test_match_error_name_not_found() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "match <{a = x} => f(x), {} => z>");
        assert_matches!(term, None);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(f),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(z),
                    context_module: None,
                    ..
                }
            ] => {
                assert_eq!(f.text(&db), "f");
                assert_eq!(z.text(&db), "z");
            }
        );
    }

    #[test]
    fn test_match_error_duplicate_name() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "match <{a = x, b = x} => x(x)>");
        assert_matches!(
            errs[..],
            [NameResolutionError::Duplicate {
                name: x,
                kind: NameKind::Var,
                original: Span { end, ..},
                duplicate: Span { start, ..},
            }] => {
                assert!(end.byte < start.byte, "{} < {}", end.byte, start.byte);
                assert_eq!(x.text(&db), "x");
            }
        );
        assert_matches!(
            term,
            Some((nterm_match!(field!(
                npat_prod!(
                    id_field!(a, npat_var!(x)),
                    id_field!(b, npat_var!(x_again))
                ),
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            )), locals)) => {
                assert_ident_text_matches_name!(db, [a, b]);
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[x_again].value.text(&db), "x");
                assert_eq!(x1, x);
                assert_eq!(x2, x);
            }
        );
    }

    #[test]
    fn test_mixed_shadowing() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, errs) = parse_resolve_term(&db, &arena, "x = {}; |x| match <x => x>");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some((nterm_local!(x_top, nterm_prod!(), nterm_abs!(x_mid, nterm_match!(field!(npat_var!(x_bot), nterm_var!(x1))))), locals)) => {
                assert_eq!(locals.vars[x_top].value.text(&db), "x");
                assert_eq!(locals.vars[x_mid].value.text(&db), "x");
                assert_eq!(locals.vars[x_bot].value.text(&db), "x");
                assert_eq!(x1, x_bot);
            }
        );
    }

    #[test]
    fn test_schemes() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (res, ids, errs) = parse_resolve_module(&db, &arena, "foo : forall a. a -> a = |x| x");
        assert_matches!(errs[..], []);
        assert_matches!(
            res[..],
            [
                Some((nitem_term!(foo, scheme!(quant!(a), None, type_func!(type_named!(a1), type_named!(a2))), nterm_abs!(x, nterm_var!(x1))), locals)),
            ] => {
                assert_eq!(ids.get(foo).value.text(&db), "foo");
                assert_eq!(locals.ty_vars[a].value.text(&db), "a");
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(a1, a);
                assert_eq!(a2, a);
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_int_ty() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (res, ids, errs) = parse_resolve_module(&db, &arena, "foobar : Int = 5");

        assert_matches!(errs[..], []);
        assert_matches!(
            res[..],
            [
                Some((nitem_term!(foobar, scheme!(type_int!()), nterm_int!(5)), _))
            ] => {
                assert_eq!(ids.get(foobar).value.text(&db), "foobar");
            }
        );
    }

    #[test]
    fn test_top_level_letrec() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (res, _, errs) = parse_resolve_module(&db, &arena, "foo = bar\nbar = foo");
        assert_matches!(errs[..], []);
        assert_matches!(
            res[..],
            [
                Some((nitem_term!(foo, nterm_item!(bar1)), _)),
                Some((nitem_term!(bar, nterm_item!(foo1)), _))
            ] => {
                assert_eq!(foo.name(&db).text(&db), "foo");
                assert_eq!(bar.name(&db).text(&db), "bar");
                assert_eq!(foo1.module(&db), bar1.module(&db));
                assert_eq!(bar1, bar);
                assert_eq!(foo1, foo);
            }
        );
    }

    #[test]
    fn test_top_level_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (res, _, errs) = parse_resolve_module(&db, &arena, "foo = x\nbar = y");
        assert_matches!(res[..], [None, None]);
        assert_matches!(
            errs[..],
            [
                NameResolutionError::NotFound {
                    name: span_of!(x),
                    context_module: None,
                    ..
                },
                NameResolutionError::NotFound {
                    name: span_of!(y),
                    context_module: None,
                    ..
                }
            ] => {
                assert_eq!(x.text(&db), "x");
                assert_eq!(y.text(&db), "y");
            }
        );
    }
}
