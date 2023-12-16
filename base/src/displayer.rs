use std::fmt::Display;

/// An object that can display objects of another type.
pub trait Displayer<T> {
    type Output<'a>: Display
    where
        T: 'a;

    fn show<'a>(&self, value: &'a T) -> Self::Output<'a>;
}
