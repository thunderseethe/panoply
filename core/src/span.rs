use std::{fmt::Debug, ops::Range};

use crate::loc::Loc;

/// A span of a source text.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    /// Returns a span consisting of a single character at the given location.
    pub fn char(loc: Loc) -> Span {
        Span {
            start: loc,
            end: loc.next(),
        }
    }

    /// Returns `self` but with the given value.
    pub fn of<T>(&self, value: T) -> SpanOf<T> {
        SpanOf {
            start: self.start,
            value,
            end: self.end,
        }
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

/// An object with a span in source code.
#[derive(Clone, Copy, Debug)]
pub struct SpanOf<T> {
    pub start: Loc,
    pub value: T,
    pub end: Loc,
}

impl<T> SpanOf<T> {
    /// Converts a `Span<T>` into a `Span<&T>`.
    pub fn as_ref<'a>(&'a self) -> SpanOf<&'a T> {
        self.span().of(&self.value)
    }

    /// Applies the function to the contained value.
    pub fn map<U, F>(self, f: F) -> SpanOf<U>
    where
        F: FnOnce(T) -> U,
    {
        self.span().of(f(self.value))
    }
}

/// An item that can be located in an interval of a source text.
pub trait Spanned {
    /// The location of the item in source text.
    fn span(&self) -> Span;

    /// A synonym for `self.span().start`.
    fn start(&self) -> Loc {
        self.span().start
    }

    /// A synonym for `self.span().end`.
    fn end(&self) -> Loc {
        self.span().end
    }

    /// Applies the given function to `self`, wrapping the returned value in `self`'s span.
    fn span_map<T, F>(&self, f: F) -> SpanOf<T>
    where
        F: FnOnce(&Self) -> T,
    {
        self.span().of(f(self))
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl<T> Spanned for SpanOf<T> {
    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.end,
        }
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

/// Matches a `SpanOf` by value alone.
#[macro_export]
macro_rules! span_of {
    ($value:pat) => {
        $crate::span::SpanOf { value: $value, .. }
    };
}
