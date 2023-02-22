use std::{cmp::Ordering, fmt::Debug, ops::Range};

use crate::loc::Loc;

/// A span of a source text.
///
/// The following properties are required:
///
///   * `start.module == end.module`
///   * `start.byte <= end.byte`
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    /// Returns a zero-width span at the given location.
    pub fn zero(loc: Loc) -> Span {
        Span {
            start: loc,
            end: loc,
        }
    }

    /// Returns the smallest span containing the left and right spans, where `left` must entirely
    /// precede `right` and both must lie in the same module.
    pub fn join<L, R>(left: &L, right: &R) -> Span
    where
        L: Spanned,
        R: Spanned,
    {
        Span {
            start: left.start(),
            end: right.end(),
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

    /// Returns a value wrapping `self` but implementing `PartialOrd` by inclusion.
    pub fn by_inclusion(&self) -> ByInclusion {
        ByInclusion(*self)
    }

    /// Returns a value wrapping `self` but implementing `PartialOrd` by precedence.
    pub fn by_precedence(&self) -> ByPrecedence {
        ByPrecedence(*self)
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

/// Compares spans by inclusion.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ByInclusion(Span);

impl PartialOrd for ByInclusion {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (
            self.0.start.partial_cmp(&other.0.start)?,
            self.0.end.partial_cmp(&other.0.end)?,
        ) {
            (Ordering::Equal, Ordering::Equal) => Some(Ordering::Equal),
            (Ordering::Less | Ordering::Equal, Ordering::Equal | Ordering::Greater) => {
                Some(Ordering::Greater)
            }
            (Ordering::Equal | Ordering::Greater, Ordering::Less | Ordering::Equal) => {
                Some(Ordering::Less)
            }
            _ => None,
        }
    }
}

/// Compares spans by precedence.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ByPrecedence(Span);

impl PartialOrd for ByPrecedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.0 == other.0 {
            Some(Ordering::Equal)
        } else if self.0.end <= other.0.start {
            Some(Ordering::Less)
        } else if other.0.end <= self.0.start {
            Some(Ordering::Greater)
        } else {
            None
        }
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
    pub fn as_ref(&self) -> SpanOf<&T> {
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

impl<T> SpanOf<Option<T>> {
    /// Converts a `SpanOf<Option<T>>` to an `Option<SpanOf<T>>`.
    pub fn transpose(self) -> Option<SpanOf<T>> {
        let s = self.span();
        self.value.map(|val| s.of(val))
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

/// Matches a [`SpanOf`] by value alone.
#[macro_export]
macro_rules! span_of {
    ($value:pat) => {
        $crate::span::SpanOf { value: $value, .. }
    };
}
