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
