use std::{fmt::Debug, ops::Range};

use crate::loc::Loc;

/// An item that can be located in an interval of a source text.
pub trait Spanned {
    fn start(&self) -> Loc;
    fn end(&self) -> Loc;
    fn span(&self) -> Span {
        Span {
            start: self.start(),
            end: self.end(),
        }
    }
}

/// A span of a source text.
#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

pub type WithSpan<T> = (T, Span);

impl Span {
    /// Returns `self` but with the given value.
    pub fn wrap<T>(&self, val: T) -> WithSpan<T> {
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

impl Spanned for Span {
    fn start(&self) -> Loc {
        self.start
    }

    fn end(&self) -> Loc {
        self.end
    }
}

impl<T> Spanned for WithSpan<T> {
    fn start(&self) -> Loc {
        self.1.start
    }

    fn end(&self) -> Loc {
        self.1.end
    }
}
