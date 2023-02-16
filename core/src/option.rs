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
