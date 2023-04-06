use std::iter;

use crate::{
    base::BaseNames,
    name::{BaseName, ModuleName, Name, NameKinded},
    names::{LocalIds, Names},
    ops::{IdOps, InsertResult},
};
use aiahr_core::{
    cst::{
        self, Annotation, Constraint, CstModule, EffectOp, Field, IdField, ProductRow, Qualifiers,
        Quantifier, Row, RowAtom, Scheme, SchemeAnnotation, Separated, SumRow, Type,
        TypeAnnotation, TypeRow,
    },
    diagnostic::{
        nameres::{NameKind, NameKinds, NameResolutionError, RejectionReason, Suggestion},
        DiagnosticSink,
    },
    id::{EffectId, EffectOpId, ItemId, ModuleId, TyVarId, VarId},
    ident::Ident,
    nst,
    option::Transpose,
    span::{Span, SpanOf, Spanned},
};
use bumpalo::Bump;

const TERM_KINDS: NameKinds = NameKinds::EFFECT_OP
    .union(NameKinds::ITEM)
    .union(NameKinds::VAR);

// Allocates the items in `iter` on the given arena, but only if they are all `Some(..)`.
fn alloc_all<T, I>(arena: &Bump, iter: I) -> Option<&[T]>
where
    I: Iterator<Item = Option<T>>,
{
    iter.collect::<Option<Vec<T>>>()
        .map(|v| arena.alloc_slice_fill_iter(v.into_iter()) as &[T])
}

impl<'a, E> NameResCtx<'a, '_, '_, E> {
    // Tries to map the given function over the elements of `separated`, returning all errors.
    fn resolve_separated<A, B, F>(
        &mut self,
        separated: &Separated<'_, A>,
        f: F,
    ) -> Option<Separated<'a, B>>
    where
        F: FnMut(&mut Self, &A) -> Option<B>,
    {
        let mut f = f;
        let first = f(self, &separated.first);
        let elems = alloc_all(
            self.arena,
            separated.elems.iter().map(|(c, a)| Some((*c, f(self, a)?))),
        );
        Some(Separated {
            first: first?,
            elems: elems.unwrap_or(&[]),
            comma: separated.comma,
        })
    }

    // Tries to map the given function over the targets of `prod`.
    fn resolve_product_row<A, B, F>(
        &mut self,
        prod: &ProductRow<'_, A>,
        f: F,
    ) -> Option<ProductRow<'a, B>>
    where
        F: FnMut(&mut Self, &A) -> Option<B>,
    {
        let mut f = f;
        let fields = if let Some(cs) = &prod.fields {
            Some(self.resolve_separated(cs, |me, field| resolve_id_field(field, |a| f(me, a)))?)
        } else {
            None
        };
        Some(ProductRow {
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
fn resolve_sum_row<A, B, F>(sum: &SumRow<A>, f: F) -> Option<SumRow<B>>
where
    F: FnOnce(&A) -> Option<B>,
{
    Some(SumRow {
        langle: sum.langle,
        field: resolve_id_field(&sum.field, f)?,
        rangle: sum.rangle,
    })
}

// A module or some other value.
#[derive(Clone, Copy, Debug)]
enum ModuleOr<T> {
    Module(ModuleId),
    Value(T),
}

// The possible meanings of a `DotAccess` term.
#[derive(Debug)]
enum DotResolution<'a> {
    Module(ModuleId),
    Effect(ModuleId, EffectId),
    EffectOp(ModuleId, EffectId, EffectOpId),
    Item(ModuleId, ItemId),
    FieldAccess {
        base: &'a nst::Term<'a>,
        dot: Span,
        field: SpanOf<Ident>,
    },
}

impl<'a> From<BaseName> for DotResolution<'a> {
    fn from(base: BaseName) -> Self {
        match base {
            BaseName::Module(m) => DotResolution::Module(m),
            BaseName::Effect(m, e) => DotResolution::Effect(m, e),
            BaseName::EffectOp(m, e, o) => DotResolution::EffectOp(m, e, o),
            BaseName::Item(m, i) => DotResolution::Item(m, i),
        }
    }
}

// Resolves a symbol or suggests names that the user might have intended.
fn resolve_or_suggest<I, N, T, F>(
    iterator: I,
    names: &Names<'_, '_>,
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

pub(crate) struct NameResCtx<'a, 'b, 'c, E> {
    arena: &'a Bump,
    names: &'b mut Names<'c, 'a>,
    errors: &'b mut E,
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
    fn resolve_symbol_in<T, F>(&mut self, module: ModuleId, var: SpanOf<Ident>, f: F) -> Option<T>
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
        F: FnOnce(&mut Names<'_, 'a>, SpanOf<Ident>) -> InsertResult<I>,
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
        module: ModuleId,
        effect: EffectId,
        var: SpanOf<Ident>,
        mut f: F,
    ) -> Option<T>
    where
        F: FnMut(EffectOpId) -> Result<T, RejectionReason>,
    {
        match resolve_or_suggest(
            self.names
                .get_effect(module, effect)
                .find(var.value)
                .map(|sn| sn.map(|o| (module, effect, o))),
            self.names,
            |(_, _, o)| f(o),
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
        row: &Row<'_, Ident, C>,
        mut f: F,
    ) -> Option<Row<'a, TyVarId, D>>
    where
        F: FnMut(&mut Self, &C) -> Option<D>,
    {
        Some(match row {
            Row::Concrete(concrete) => {
                Row::Concrete(self.resolve_separated(concrete, |me, c| f(me, c))?)
            }
            Row::Variable(variables) => Row::Variable(
                self.resolve_separated(variables, |me, var| me.resolve_type_symbol(*var))?,
            ),
            Row::Mixed {
                concrete,
                vbar,
                variables,
            } => {
                let concrete = self.resolve_separated(concrete, |me, c| f(me, c));
                let variables =
                    self.resolve_separated(variables, |me, var| me.resolve_type_symbol(*var));
                Row::Mixed {
                    concrete: concrete?,
                    vbar: *vbar,
                    variables: variables?,
                }
            }
        })
    }

    fn mk_ty(&self, type_: Type<'a, TyVarId>) -> &'a Type<'a, TyVarId> {
        self.arena.alloc(type_)
    }

    /// Resolves an Aiahr type.
    pub fn resolve_type(&mut self, type_: &Type<'_, Ident>) -> Option<&'a Type<'a, TyVarId>> {
        let type_ = match type_ {
            Type::Named(var) => Type::Named(self.resolve_type_symbol(*var)?),
            Type::Sum {
                langle,
                variants,
                rangle,
            } => Type::Sum {
                langle: *langle,
                variants: self.resolve_type_row(variants)?,
                rangle: *rangle,
            },
            Type::Product {
                lbrace,
                fields,
                rbrace,
            } => Type::Product {
                lbrace: *lbrace,
                fields: fields
                    .map(|fields| self.resolve_type_row(&fields))
                    .transpose()?,
                rbrace: *rbrace,
            },
            Type::Function {
                domain,
                arrow,
                codomain,
            } => {
                let domain = self.resolve_type(domain);
                let codomain = self.resolve_type(codomain);
                Type::Function {
                    domain: domain?,
                    arrow: *arrow,
                    codomain: codomain?,
                }
            }
            Type::Parenthesized { lpar, type_, rpar } => Type::Parenthesized {
                lpar: *lpar,
                type_: self.resolve_type(type_)?,
                rpar: *rpar,
            },
        };
        Some(self.mk_ty(type_))
    }

    // Resolves a row of types.
    fn resolve_type_row(&mut self, row: &TypeRow<'_, Ident>) -> Option<TypeRow<'a, TyVarId>> {
        self.resolve_row(row, |me, field| {
            resolve_id_field(field, |ty| me.resolve_type(ty))
        })
    }

    // Resolves a row atom.
    fn resolve_row_atom(&mut self, atom: &RowAtom<'_, Ident>) -> Option<RowAtom<'a, TyVarId>> {
        Some(match atom {
            RowAtom::Concrete { lpar, fields, rpar } => RowAtom::Concrete {
                lpar: *lpar,
                fields: self.resolve_separated(fields, |me, field| {
                    resolve_id_field(field, |ty| me.resolve_type(ty))
                })?,
                rpar: *rpar,
            },
            RowAtom::Variable(var) => RowAtom::Variable(self.resolve_type_symbol(*var)?),
        })
    }

    // Resolves a type constraint.
    fn resolve_constraint(
        &mut self,
        constraint: &Constraint<'_, Ident>,
    ) -> Option<Constraint<'a, TyVarId>> {
        Some(match constraint {
            Constraint::RowSum {
                lhs,
                plus,
                rhs,
                eq,
                goal,
            } => {
                let lhs = self.resolve_row_atom(lhs);
                let rhs = self.resolve_row_atom(rhs);
                let goal = self.resolve_row_atom(goal);
                Constraint::RowSum {
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
        qualifiers: &Qualifiers<'_, Ident>,
    ) -> Option<Qualifiers<'a, TyVarId>> {
        Some(Qualifiers {
            constraints: self.resolve_separated(&qualifiers.constraints, |me, constraint| {
                me.resolve_constraint(constraint)
            })?,
            arrow: qualifiers.arrow,
        })
    }

    // Resolves a quantifier.
    fn resolve_quantifier(&mut self, quantifier: &Quantifier<Ident>) -> Quantifier<TyVarId> {
        Quantifier {
            forall: quantifier.forall,
            var: self.insert_ty_var(quantifier.var),
            dot: quantifier.dot,
        }
    }

    /// Resolves a polymorphic type.
    pub fn resolve_scheme(
        &mut self,
        scheme: &Scheme<'_, Ident>,
    ) -> Option<&'a Scheme<'a, TyVarId>> {
        self.subscope(|scope| {
            let quantifiers = scope.arena.alloc_slice_fill_iter(
                scheme
                    .quantifiers
                    .iter()
                    .map(|quant| scope.resolve_quantifier(quant)),
            ) as &[_];
            let qualifiers = scheme
                .qualifiers
                .map(|quals| scope.resolve_qualifiers(&quals))
                .transpose();
            let type_ = scope.resolve_type(scheme.type_);
            Some(scope.arena.alloc(Scheme {
                quantifiers,
                qualifiers: qualifiers?,
                type_: type_?,
            }) as &_)
        })
    }

    fn subscope<A>(&mut self, body: impl FnOnce(&mut NameResCtx<'a, '_, '_, E>) -> A) -> A {
        self.names.subscope(|names| {
            let mut ctx = NameResCtx {
                arena: self.arena,
                errors: self.errors,
                names,
            };
            body(&mut ctx)
        })
    }

    fn mk_pat(&self, pat: nst::Pattern<'a>) -> &'a nst::Pattern<'a> {
        self.arena.alloc(pat)
    }

    /// Resolves the given pattern, accumulating bindings into `names`.
    ///
    /// Note that this currently cannot return `None`, although it can emit errors.
    pub fn resolve_pattern(&mut self, pattern: &cst::Pattern<'_>) -> Option<&'a nst::Pattern<'a>> {
        let pat = match pattern {
            cst::Pattern::ProductRow(pr) => nst::Pattern::ProductRow(
                self.resolve_product_row(pr, |me, target| me.resolve_pattern(target))?,
            ),
            cst::Pattern::SumRow(sr) => {
                nst::Pattern::SumRow(resolve_sum_row(sr, |target| self.resolve_pattern(target))?)
            }
            cst::Pattern::Whole(var) => nst::Pattern::Whole(self.insert_var(*var)),
        };
        Some(self.mk_pat(pat))
    }

    // Resolves a type annotation.
    fn resolve_type_annotation(
        &mut self,
        annotation: &TypeAnnotation<'_, Ident>,
    ) -> Option<TypeAnnotation<'a, TyVarId>> {
        Some(Annotation {
            colon: annotation.colon,
            type_: self.resolve_type(annotation.type_)?,
        })
    }

    // Resolves a scheme annotation.
    fn resolve_scheme_annotation(
        &mut self,
        annotation: &SchemeAnnotation<'_, Ident>,
    ) -> Option<SchemeAnnotation<'a, TyVarId>> {
        Some(Annotation {
            colon: annotation.colon,
            type_: self.resolve_scheme(annotation.type_)?,
        })
    }

    // Resolves nested `DotAccess` terms.
    fn resolve_nested_dots(
        &mut self,
        base: &cst::Term<'_>,
        dot: Span,
        field: SpanOf<Ident>,
    ) -> Option<DotResolution<'a>> {
        Some(match base {
            // (base2 . field2) . field
            cst::Term::DotAccess {
                base: base2,
                dot: dot2,
                field: field2,
            } => match self.resolve_nested_dots(base2, *dot2, *field2)? {
                // m . field
                DotResolution::Module(m) => {
                    self.resolve_symbol_in(m, field, |name| Ok(DotResolution::from(name)))?
                }
                DotResolution::Effect(m, e) => self.resolve_operation_symbol(m, e, field, |o| {
                    Ok(DotResolution::EffectOp(m, e, o))
                })?,
                DotResolution::EffectOp(_, _, _) => {
                    self.errors.add(NameResolutionError::WrongKind {
                        expr: base.span(),
                        actual: NameKind::EffectOp,
                        expected: !NameKinds::EFFECT_OP,
                    });
                    return None;
                }
                DotResolution::Item(m, i) => DotResolution::FieldAccess {
                    base: self.mk_term(nst::Term::ItemRef(base.span().of((m, i)))),
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
                match self.resolve_symbol(*n, |name| match name {
                    Name::Module(m) => Ok(ModuleOr::Module(m)),
                    Name::Item(m, i) => {
                        Ok(ModuleOr::Value(nst::Term::ItemRef(base.span().of((m, i)))))
                    }
                    Name::Var(v) => Ok(ModuleOr::Value(nst::Term::VariableRef(base.span().of(v)))),
                    _ => Err(RejectionReason::WrongKind {
                        actual: name.kind(),
                        expected: NameKinds::MODULE | TERM_KINDS,
                    }),
                })? {
                    ModuleOr::Module(m) => {
                        self.resolve_symbol_in(m, field, |name| Ok(DotResolution::from(name)))?
                    }
                    ModuleOr::Value(value) => DotResolution::FieldAccess {
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

    fn mk_term(&self, term: nst::Term<'a>) -> &'a nst::Term<'a> {
        self.arena.alloc(term)
    }

    /// Resolves the given term.
    pub fn resolve_term(&mut self, term: &cst::Term<'_>) -> Option<&'a nst::Term<'a>> {
        let term = match term {
            cst::Term::Binding {
                var,
                annotation,
                eq,
                value,
                semi,
                expr,
            } => {
                let value = self.resolve_term(value);
                self.subscope(|ctx| {
                    let var = ctx.insert_var(*var);
                    let annotation = annotation
                        .map(|annotation| ctx.resolve_type_annotation(&annotation))
                        .transpose();
                    let expr = ctx.resolve_term(expr);

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
                let handler = self.resolve_term(handler);
                let expr = self.resolve_term(expr);
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
                let body = scope.resolve_term(body);
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
                let func = self.resolve_term(func);
                let arg = self.resolve_term(arg);
                nst::Term::Application {
                    func: func?,
                    lpar: *lpar,
                    arg: arg?,
                    rpar: *rpar,
                }
            }
            cst::Term::ProductRow(pr) => nst::Term::ProductRow(
                self.resolve_product_row(pr, |me, target| me.resolve_term(target))?,
            ),
            cst::Term::SumRow(sr) => {
                nst::Term::SumRow(resolve_sum_row(sr, |target| self.resolve_term(target))?)
            }
            t @ cst::Term::DotAccess { base, dot, field } => {
                match self.resolve_nested_dots(base, *dot, *field)? {
                    DotResolution::Module(_) => {
                        self.errors.add(NameResolutionError::WrongKind {
                            expr: t.span(),
                            actual: NameKind::Module,
                            expected: TERM_KINDS,
                        });
                        return None;
                    }
                    DotResolution::Effect(_, _) => {
                        self.errors.add(NameResolutionError::WrongKind {
                            expr: t.span(),
                            actual: NameKind::Effect,
                            expected: TERM_KINDS,
                        });
                        return None;
                    }
                    DotResolution::EffectOp(m, e, o) => {
                        nst::Term::EffectOpRef(t.span().of((m, e, o)))
                    }
                    DotResolution::Item(m, i) => nst::Term::ItemRef(t.span().of((m, i))),
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
                Name::Item(m, i) => Ok(nst::Term::ItemRef(var.span().of((m, i)))),
                Name::Var(v) => Ok(nst::Term::VariableRef(var.span().of(v))),
                _ => Err(RejectionReason::WrongKind {
                    actual: name.kind(),
                    expected: TERM_KINDS,
                }),
            })?,
            cst::Term::Parenthesized { lpar, term, rpar } => nst::Term::Parenthesized {
                lpar: *lpar,
                term: self.resolve_term(term)?,
                rpar: *rpar,
            },
        };
        Some(self.mk_term(term))
    }

    /// Resolves an effect operation signature.
    fn resolve_effect_op(
        &mut self,
        opid: EffectOpId,
        op: &EffectOp<'_, Ident, Ident>,
    ) -> Option<EffectOp<'a, EffectOpId, TyVarId>> {
        Some(EffectOp {
            name: op.name.span().of(opid),
            colon: op.colon,
            type_: self.resolve_type(op.type_)?,
        })
    }

    /// Resolves the given effect.
    fn resolve_effect(
        &mut self,
        module: ModuleId,
        eid: EffectId,
        effect: Span,
        name: SpanOf<Ident>,
        lbrace: Span,
        ops: &[EffectOp<'_, Ident, Ident>],
        rbrace: Span,
    ) -> Option<nst::Item<'a>> {
        Some(nst::Item::Effect {
            effect,
            name: name.span().of(eid),
            lbrace,
            ops: self.arena.alloc_slice_fill_iter(
                iter::zip(
                    self.names
                        .get_effect(module, eid)
                        .iter()
                        .collect::<Vec<_>>()
                        .into_iter(),
                    ops.iter(),
                )
                .map(|(opid, op)| self.resolve_effect_op(opid, op)),
            ),
            rbrace,
        })
    }

    /// Resolves the given item.
    pub fn resolve_term_item<'d>(
        &mut self,
        id: ItemId,
        name: SpanOf<Ident>,
        annotation: Option<SchemeAnnotation<'d, Ident>>,
        eq: Span,
        value: &cst::Term<'_>,
    ) -> Option<nst::Item<'a>> {
        Some(nst::Item::Term {
            name: name.span().of(id),
            annotation: annotation
                .map(|annotation| self.resolve_scheme_annotation(&annotation))
                .transpose()?,
            eq,
            value: self.resolve_term(value)?,
        })
    }
}

/// Data generated by resolving a module.
#[derive(Debug)]
pub struct ModuleResolution<'a> {
    pub locals: LocalIds,
    pub resolved_items: &'a [Option<nst::Item<'a>>],
}

/// Resolves the given module.
pub fn resolve_module<'a, 'b: 'a, E>(
    arena: &'a Bump,
    module: CstModule<'b>,
    base: BaseNames<'_, 'a>,
    errors: &mut E,
) -> ModuleResolution<'a>
where
    E: DiagnosticSink<NameResolutionError>,
{
    let mut names = Names::new(&base);
    let mut ctx = NameResCtx {
        arena,
        errors,
        names: &mut names,
    };
    let resolved_items = ctx
        .arena
        .alloc_slice_fill_iter(base.iter().zip(module.items.iter()).map(|(name, item)| {
            match (name, item) {
                (
                    ModuleName::Effect(e),
                    &cst::Item::Effect {
                        effect,
                        name,
                        lbrace,
                        ops,
                        rbrace,
                    },
                ) => ctx.resolve_effect(base.me(), *e, effect, name, lbrace, ops, rbrace),
                (
                    ModuleName::Item(i),
                    &cst::Item::Term {
                        name,
                        annotation,
                        eq,
                        value,
                    },
                ) => ctx.resolve_term_item(*i, name, annotation, eq, value),
                _ => panic!(
                    "Expected same kinds of names, got {:?} and {:?}",
                    name, item
                ),
            }
        }));
    ModuleResolution {
        locals: names.into_local_ids(),
        resolved_items,
    }
}

#[cfg(test)]
mod tests {
    use aiahr_core::{
        diagnostic::nameres::{NameKind, NameResolutionError},
        field,
        id::ModuleId,
        id_field,
        modules::ModuleTree,
        nitem_term, npat_prod, npat_var, nst, nterm_abs, nterm_app, nterm_dot, nterm_item,
        nterm_local, nterm_match, nterm_prod, nterm_sum, nterm_var, nterm_with, quant, scheme,
        span::Span,
        span_of, type_func, type_named, type_prod, Db,
    };
    use aiahr_parser::{
        lexer::aiahr_lexer,
        parser::{aiahr_parser, term, to_stream},
    };
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use chumsky::Parser;
    use rustc_hash::FxHashMap;

    use crate::{
        module::ModuleNames,
        names::{LocalIds, Names},
        ops::IdOps,
        top_level::BaseBuilder,
    };

    use super::{resolve_module, /*resolve_term,*/ ModuleResolution, NameResCtx};

    use aiahr_test::assert_ident_text_matches_name;

    const MODNAME: &str = "test_module";

    #[derive(Default)]
    #[salsa::db(crate::Jar, aiahr_core::Jar, aiahr_parser::Jar)]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn parse_resolve_term<'a>(
        db: &TestDatabase,
        arena: &'a Bump,
        input: &str,
    ) -> (
        Option<&'a nst::Term<'a>>,
        LocalIds,
        Vec<NameResolutionError>,
    ) {
        let (m, modules) = {
            let mut modules = ModuleTree::new();
            let m = modules.add_package(db.ident_str(MODNAME));
            (m, modules)
        };

        let (tokens, eoi) = aiahr_lexer(db).lex(m, input).unwrap();
        let unresolved = term(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();

        let mut module_names = FxHashMap::default();
        let base = BaseBuilder::new().build(arena, m, &modules, &mut module_names);
        let mut names = Names::new(&base);

        let mut nameres_ctx = NameResCtx {
            arena,
            names: &mut names,
            errors: &mut errors,
        };
        let resolved = nameres_ctx.resolve_term(unresolved);
        (resolved, names.into_local_ids(), errors)
    }

    fn parse_resolve_module<'a>(
        db: &TestDatabase,
        arena: &'a Bump,
        input: &str,
    ) -> (
        ModuleResolution<'a>,
        &'a ModuleNames,
        Vec<NameResolutionError>,
    ) {
        let (m, modules) = {
            let mut modules = ModuleTree::new();
            let m = modules.add_package(db.ident_str(MODNAME));
            (m, modules)
        };

        let (tokens, eoi) = aiahr_lexer(db).lex(m, input).unwrap();
        let unresolved = aiahr_parser(arena).parse(to_stream(tokens, eoi)).unwrap();
        let mut errors = Vec::new();

        let mut module_names = FxHashMap::default();
        let base = BaseBuilder::new()
            .add_slice(unresolved.items, &mut errors)
            .build(arena, m, &modules, &mut module_names);

        (
            resolve_module(arena, unresolved, base, &mut errors),
            module_names[&m],
            errors,
        )
    }

    #[test]
    fn test_local_binding() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "x = {}; y = {}; x");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_local!(y, nterm_prod!(), nterm_var!(x1))
            )) => {
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "x: a = {}; y: {} = {}; x");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some(nterm_local!(
                x,
                type_named!(a),
                nterm_prod!(),
                nterm_local!(y, type_prod!(), nterm_prod!(), nterm_var!(x1))
            )) => {
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "x = {}; x = x; x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(
                x_out,
                nterm_prod!(),
                nterm_local!(x_in, nterm_var!(x1), nterm_var!(x2))
            )) => {
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
        let (term, _, errs) = parse_resolve_term(&db, &arena, "x = y; z");
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "x = {}; with x do x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(
                x,
                nterm_prod!(),
                nterm_with!(nterm_var!(x1), nterm_var!(x2))
            )) => {
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
        let (term, _, errs) = parse_resolve_term(&db, &arena, "with h do x");
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "|x| |y| y(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_abs!(
                    y,
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            )) => {
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "|x: {}| |y: a -> b| y(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                type_prod!(),
                nterm_abs!(
                    y,
                    type_func!(type_named!(a), type_named!(b)),
                    nterm_app!(nterm_var!(y1), nterm_var!(x1))
                )
            )) => {
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "|x| |x| x(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x_out,
                nterm_abs!(
                    x_in,
                    nterm_app!(nterm_var!(x1), nterm_var!(x2))
                )
            )) => {
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "|x| x(x)");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(
                x,
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            )) => {
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
        let (term, _, errs) = parse_resolve_term(&db, &arena, "f(x)");
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "x = {}; {a = x, b = {x = x}}");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(x_var, nterm_prod!(),
                nterm_prod!(
                id_field!(a, nterm_var!(x1)),
                id_field!(
                    b,
                    nterm_prod!(id_field!(x, nterm_var!(x2)))
                ),
            ))) => {
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
        let (term, _, errs) = parse_resolve_term(&db, &arena, "{x = y, z = x}");
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "|x| <a = x>");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_abs!(x, nterm_sum!(id_field!(a, nterm_var!(x1))))) => {
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
        let (term, _, errs) = parse_resolve_term(&db, &arena, "<x = x>");
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "id = |x| x; {x = id}.x");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_local!(
                id,
                nterm_abs!(x_var, nterm_var!(x_var1)),
                nterm_dot!(
                    nterm_prod!(id_field!(x, nterm_var!(id1))),
                    x_id1
                )
            )) => {
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
        let (term, _, errs) = parse_resolve_term(&db, &arena, "x.a");
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "match <{a = x} => x, y => y>");
        assert_matches!(errs[..], []);
        assert_matches!(term,
            Some(nterm_match!(
                field!(npat_prod!(id_field!(a, npat_var!(x))), nterm_var!(x1)),
                field!(npat_var!(y), nterm_var!(y1))
            )) => {
                assert_eq!(locals.vars[x].value.text(&db), "x");
                assert_eq!(locals.vars[y].value.text(&db), "y");
                assert_eq!(x1, x);
                assert_eq!(y1, y);
                assert_ident_text_matches_name!(db, [a]);
            }
        );
    }

    #[test]
    fn test_match_errors() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (term, _, errs) = parse_resolve_term(&db, &arena, "match <{a = x} => f(x), {} => z>");
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

        let (term, locals, errs) =
            parse_resolve_term(&db, &arena, "match <{a = x, b = x} => x(x)>");
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
            Some(nterm_match!(field!(
                npat_prod!(
                    id_field!(a, npat_var!(x)),
                    id_field!(b, npat_var!(x_again))
                ),
                nterm_app!(nterm_var!(x1), nterm_var!(x2))
            ))) => {
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
        let (term, locals, errs) = parse_resolve_term(&db, &arena, "x = {}; |x| match <x => x>");
        assert_matches!(errs[..], []);
        assert_matches!(
            term,
            Some(nterm_local!(x_top, nterm_prod!(), nterm_abs!(x_mid, nterm_match!(field!(npat_var!(x_bot), nterm_var!(x1)))))) => {
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
            res.resolved_items[..],
            [
                Some(nitem_term!(foo, scheme!(quant!(a), None, type_func!(type_named!(a1), type_named!(a2))), nterm_abs!(x, nterm_var!(x1)))),
            ] => {
                assert_eq!(ids.get(foo).value.text(&db), "foo");
                assert_eq!(res.locals.ty_vars[a].value.text(&db), "a");
                assert_eq!(res.locals.vars[x].value.text(&db), "x");
                assert_eq!(a1, a);
                assert_eq!(a2, a);
                assert_eq!(x1, x);
            }
        );
    }

    #[test]
    fn test_top_level_letrec() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (res, ids, errs) = parse_resolve_module(&db, &arena, "foo = bar\nbar = foo");
        assert_matches!(errs[..], []);
        assert_matches!(
            res.resolved_items[..],
            [
                Some(nitem_term!(foo, nterm_item!(mbar, bar1))),
                Some(nitem_term!(bar, nterm_item!(mfoo, foo1)))
            ] => {
                assert_eq!(ids.get(foo).value.text(&db), "foo");
                assert_eq!(ids.get(bar).value.text(&db), "bar");
                assert_eq!(mbar, ModuleId(0));
                assert_eq!(mfoo, ModuleId(0));
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
        assert_matches!(res.resolved_items[..], [None, None]);
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
