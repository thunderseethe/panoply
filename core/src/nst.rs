use crate::{
    cst::{Field, Pattern, ProductRow, Separated, SumRow},
    id::{ItemId, ModuleId, VarId},
    span::{Span, SpanOf, Spanned},
};

/// An Aiahr term with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 'i> {
    Binding {
        var: SpanOf<VarId>,
        eq: Span,
        value: &'a Term<'a, 'i>,
        semi: Span,
        expr: &'a Term<'a, 'i>,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a, 'i>,
        do_: Span,
        expr: &'a Term<'a, 'i>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<VarId>,
        rbar: Span,
        body: &'a Term<'a, 'i>,
    },
    Application {
        func: &'a Term<'a, 'i>,
        lpar: Span,
        arg: &'a Term<'a, 'i>,
        rpar: Span,
    },
    ProductRow(ProductRow<'a, 'i, &'a Term<'a, 'i>>),
    SumRow(SumRow<'i, &'a Term<'a, 'i>>),
    FieldAccess {
        base: &'a Term<'a, 'i>,
        dot: Span,
        field: SpanOf<&'i str>,
    },
    Match {
        match_: Span,
        langle: Span,
        cases: Separated<'a, Field<&'a Pattern<'a, 'i, VarId>, &'a Term<'a, 'i>>>,
        rangle: Span,
    },
    ItemRef(SpanOf<(ModuleId, ItemId)>),
    VariableRef(SpanOf<VarId>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a, 'i>,
        rpar: Span,
    },
}

impl<'a, 'i> Spanned for Term<'a, 'i> {
    fn span(&self) -> Span {
        match self {
            Term::Binding { var, expr, .. } => Span {
                start: var.start(),
                end: expr.end(),
            },
            Term::Handle { with, expr, .. } => Span {
                start: with.start(),
                end: expr.end(),
            },
            Term::Abstraction { lbar, body, .. } => Span {
                start: lbar.start(),
                end: body.end(),
            },
            Term::Application { func, rpar, .. } => Span {
                start: func.start(),
                end: rpar.end(),
            },
            Term::ProductRow(p) => p.span(),
            Term::SumRow(s) => s.span(),
            Term::FieldAccess { base, field, .. } => Span {
                start: base.start(),
                end: field.end(),
            },
            Term::Match { match_, rangle, .. } => Span {
                start: match_.start(),
                end: rangle.end(),
            },
            Term::ItemRef(i) => i.span(),
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span {
                start: lpar.start(),
                end: rpar.end(),
            },
        }
    }
}

/// A top-level item in an Aiahr source file with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a, 'i> {
    Term {
        name: SpanOf<ItemId>,
        eq: Span,
        value: &'a Term<'a, 'i>,
    },
}

impl<'a, 'i> Spanned for Item<'a, 'i> {
    fn span(&self) -> Span {
        match self {
            Item::Term { name, value, .. } => Span {
                start: name.start(),
                end: value.end(),
            },
        }
    }
}

#[macro_export]
macro_rules! nterm_local {
    ($var:pat, $value:pat, $expr:pat) => {
        &$crate::nst::Term::Binding {
            var: $crate::span_of!($var),
            value: $value,
            expr: $expr,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_with {
    ($handler:pat, $expr:pat) => {
        &$crate::nst::Term::Handle {
            handler: $handler,
            expr: $expr,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_abs {
    ($arg:pat, $body:pat) => {
        &$crate::nst::Term::Abstraction {
            arg: $crate::span_of!($arg),
            body: $body,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_app {
    ($func:pat, $arg:pat) => {
        &$crate::nst::Term::Application {
            func: $func,
            arg: $arg,
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_prod {
    ($($fields:pat),* $(,)?) => {
        &$crate::nst::Term::ProductRow($crate::prod!($($fields,)*))
    };
    () => {
        &$crate::nst::Term::ProductRow($crate::prod!())
    };
}

#[macro_export]
macro_rules! nterm_sum {
    ($field:pat) => {
        &$crate::nst::Term::SumRow($crate::sum!($field))
    };
}

#[macro_export]
macro_rules! nterm_dot {
    ($base:pat, $field:pat) => {
        &$crate::nst::Term::FieldAccess {
            base: $base,
            field: $crate::span_of!($field),
            ..
        }
    };
}

#[macro_export]
macro_rules! nterm_match {
    ($($cases:pat),+ $(,)?) => {
        &$crate::nst::Term::Match { cases: $crate::separated!($($cases),+), .. }
    };
}

#[macro_export]
macro_rules! nterm_toplvl {
    ($mod_:pat, $item:pat) => {
        &$crate::nst::Term::TopLevelRef($crate::span_of!(($mod_, $item)))
    };
}

#[macro_export]
macro_rules! nterm_item {
    ($mod_:pat, $item:pat) => {
        &$crate::nst::Term::ItemRef($crate::span_of!(($mod_, $item)))
    };
}

#[macro_export]
macro_rules! nterm_var {
    ($var:pat) => {
        &$crate::nst::Term::VariableRef($crate::span_of!($var))
    };
}

#[macro_export]
macro_rules! nterm_paren {
    ($term:pat) => {
        &$crate::nst::Term::Parenthesized { term: $term, .. }
    };
}

#[macro_export]
macro_rules! nitem_term {
    ($name:pat, $value:pat) => {
        $crate::nst::Item::Term {
            name: $crate::span_of!($name),
            value: $value,
            ..
        }
    };
}
