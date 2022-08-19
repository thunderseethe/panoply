use std::{fmt::Debug, ops::Range};

use crate::loc::Loc;

/// A span of a source text.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

pub type SpanOf<T> = (T, Span);

impl Span {
    /// Returns `self` but with the given value.
    pub fn of<T>(&self, val: T) -> SpanOf<T> {
        (val, *self)
    }
}

impl chumsky::Span for Span {
    type Context = ();
    type Offset = Loc;

    fn new(_: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}
    fn start(&self) -> Self::Offset {
        self.start
    }
    fn end(&self) -> Self::Offset {
        self.end
    }
}

/// An item that can be located in an interval of a source text.
pub trait Spanned {
    fn span(&self) -> Span;
    fn start(&self) -> Loc {
        self.span().start
    }
    fn end(&self) -> Loc {
        self.span().end
    }
    fn span_map<T, F>(&self, f: F) -> SpanOf<T>
    where
        F: FnOnce(&Self) -> T,
    {
        (f(self), self.span())
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl<T> Spanned for SpanOf<T> {
    fn span(&self) -> Span {
        self.1
    }
}

impl<T> Spanned for &T
where
    T: Spanned,
{
    fn span(&self) -> Span {
        (*self).span()
    }
}
