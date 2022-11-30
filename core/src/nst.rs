use crate::{
    cst::{Field, ProductRow, Separated, SumRow},
    id::{ItemId, ModuleId, VarId},
    memory::handle::RefHandle,
    span::{Span, SpanOf, Spanned},
};

/// A pattern with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Pattern<'a, 's> {
    ProductRow(ProductRow<'a, 's, &'a Pattern<'a, 's>>),
    SumRow(SumRow<'s, &'a Pattern<'a, 's>>),
    Whole(SpanOf<VarId>),
}

impl<'a, 's> Spanned for Pattern<'a, 's> {
    fn span(&self) -> Span {
        match self {
            Pattern::ProductRow(p) => p.span(),
            Pattern::SumRow(s) => s.span(),
            Pattern::Whole(v) => v.span(),
        }
    }
}

/// An Aiahr term with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Term<'a, 's> {
    Binding {
        var: SpanOf<VarId>,
        eq: Span,
        value: &'a Term<'a, 's>,
        semi: Span,
        expr: &'a Term<'a, 's>,
    },
    Handle {
        with: Span,
        handler: &'a Term<'a, 's>,
        do_: Span,
        expr: &'a Term<'a, 's>,
    },
    Abstraction {
        lbar: Span,
        arg: SpanOf<VarId>,
        rbar: Span,
        body: &'a Term<'a, 's>,
    },
    Application {
        func: &'a Term<'a, 's>,
        lpar: Span,
        arg: &'a Term<'a, 's>,
        rpar: Span,
    },
    ProductRow(ProductRow<'a, 's, &'a Term<'a, 's>>),
    SumRow(SumRow<'s, &'a Term<'a, 's>>),
    FieldAccess {
        base: &'a Term<'a, 's>,
        dot: Span,
        field: SpanOf<RefHandle<'s, str>>,
    },
    Match {
        match_: Span,
        langle: Span,
        cases: Separated<'a, Field<&'a Pattern<'a, 's>, &'a Term<'a, 's>>>,
        rangle: Span,
    },
    ItemRef(SpanOf<(ModuleId, ItemId)>),
    VariableRef(SpanOf<VarId>),
    Parenthesized {
        lpar: Span,
        term: &'a Term<'a, 's>,
        rpar: Span,
    },
}

impl<'a, 's> Spanned for Term<'a, 's> {
    fn span(&self) -> Span {
        match self {
            Term::Binding { var, expr, .. } => Span::join(var, expr),
            Term::Handle { with, expr, .. } => Span::join(with, expr),
            Term::Abstraction { lbar, body, .. } => Span::join(lbar, body),
            Term::Application { func, rpar, .. } => Span::join(func, rpar),
            Term::ProductRow(p) => p.span(),
            Term::SumRow(s) => s.span(),
            Term::FieldAccess { base, field, .. } => Span::join(base, field),
            Term::Match { match_, rangle, .. } => Span::join(match_, rangle),
            Term::ItemRef(i) => i.span(),
            Term::VariableRef(v) => v.span(),
            Term::Parenthesized { lpar, rpar, .. } => Span::join(lpar, rpar),
        }
    }
}

/// A top-level item in an Aiahr source file with names resolved.
#[derive(Clone, Copy, Debug)]
pub enum Item<'a, 's> {
    Term {
        name: SpanOf<ItemId>,
        eq: Span,
        value: &'a Term<'a, 's>,
    },
}

impl<'a, 's> Spanned for Item<'a, 's> {
    fn span(&self) -> Span {
        match self {
            Item::Term { name, value, .. } => Span::join(name, value),
        }
    }
}

#[macro_export]
macro_rules! npat_prod {
    ($($fields:pat),* $(,)?) => {
        &$crate::nst::Pattern::ProductRow($crate::prod!($($fields,)+))
    };
}

#[macro_export]
macro_rules! npat_sum {
    ($field:pat) => {
        &$crate::nst::Pattern::SumRow($crate::sum!($field))
    };
}

#[macro_export]
macro_rules! npat_var {
    ($var:pat) => {
        &$crate::nst::Pattern::Whole($crate::span_of!($var))
    };
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
            field: $crate::span_of!($crate::h!($field)),
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
