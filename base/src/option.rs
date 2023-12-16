pub trait IfNone: Sized {
    fn if_none<F: FnOnce()>(self, f: F) -> Self;
}

impl<T> IfNone for Option<T> {
    fn if_none<F: FnOnce()>(self, f: F) -> Self {
        if self.is_none() {
            f()
        }
        self
    }
}

/// TODO(rust-lang/rust#93050): Remove in favor of standard library version.
pub trait IsSomeAnd<T>: Sized {
    #[allow(clippy::wrong_self_convention)]
    fn is_some_and(self, f: impl FnOnce(T) -> bool) -> bool;
}

impl<T> IsSomeAnd<T> for Option<T> {
    fn is_some_and(self, f: impl FnOnce(T) -> bool) -> bool {
        match self {
            Some(x) => f(x),
            None => false,
        }
    }
}

/// Transposes `F<G<T>>` to `G<F<T>>`.
pub trait Transpose<B>: Sized {
    fn transpose(self) -> Self;
}

impl<T> Transpose<Option<Option<T>>> for Option<Option<T>> {
    fn transpose(self) -> Self {
        match self {
            None => Some(None),
            Some(None) => None,
            _ => self,
        }
    }
}
