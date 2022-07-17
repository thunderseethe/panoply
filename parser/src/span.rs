use std::fmt::Debug;

use crate::loc::Loc;

/// An item that can be located in an interval of source code.
pub trait Spanned {
    fn start(&self) -> Loc;
    fn end(&self) -> Loc;
}

/// Wraps `T` in a `start` and `end` `Loc`.
#[derive(Debug)]
pub struct Span<T: Debug> {
    pub start: Loc,
    pub val: T,
    pub end: Loc,
}

impl Span<()> {
    /// Returns `self` but with the given value.
    pub fn wrap<T: Debug>(&self, val: T) -> Span<T> {
        Span {
            start: self.start,
            val: val,
            end: self.end,
        }
    }
}

impl<T: Debug> Span<T> {
    /// Returns `self` with the value removed.
    pub fn unit(&self) -> Span<()> {
        Span {
            start: self.start,
            val: (),
            end: self.end,
        }
    }
}

impl<T: Clone + Debug> Clone for Span<T> {
    fn clone(&self) -> Self {
        Span {
            start: self.start,
            val: self.val.clone(),
            end: self.end,
        }
    }
}

impl<T: Copy + Debug> Copy for Span<T> {}

impl<T: Debug> Spanned for Span<T> {
    fn start(&self) -> Loc {
        self.start
    }

    fn end(&self) -> Loc {
        self.end
    }
}

impl chumsky::Span for Span<()> {
    type Context = ();
    type Offset = Loc;

    fn new(_: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span {
            start: range.start,
            val: (),
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
