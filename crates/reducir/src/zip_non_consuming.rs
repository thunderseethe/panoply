/// A zip that does not own it's iterators and does not consume elements when only one iterator has
/// elements remaining.
pub struct LentZip<'a, A, B>
where
  A: Iterator,
  B: Iterator,
{
  a_iter: &'a mut std::iter::Peekable<A>,
  b_iter: &'a mut std::iter::Peekable<B>,
}
impl<'a, A, B> Iterator for LentZip<'a, A, B>
where
  A: Iterator,
  B: Iterator,
{
  type Item = (A::Item, B::Item);

  fn next(&mut self) -> Option<Self::Item> {
    match (self.a_iter.peek(), self.b_iter.peek()) {
      (Some(_), Some(_)) => Some((self.a_iter.next().unwrap(), self.b_iter.next().unwrap())),
      _ => None,
    }
  }
}

/// Zips two iterators without consuming them.
/// This allows for iterating over the overlap of two iterators, and then consuming whatever
/// remains in the two iterators separately after the fact.
pub trait ZipNonConsuming {
  type Iter: Iterator;

  fn zip_non_consuming<'a, J>(
    &'a mut self,
    other: &'a mut std::iter::Peekable<J>,
  ) -> LentZip<'a, Self::Iter, J>
  where
    J: Sized + Iterator;
}
impl<I: Iterator> ZipNonConsuming for std::iter::Peekable<I> {
  type Iter = I;

  fn zip_non_consuming<'a, J>(
    &'a mut self,
    other: &'a mut std::iter::Peekable<J>,
  ) -> LentZip<'a, I, J>
  where
    Self: Sized + Iterator,
    J: Sized + Iterator,
  {
    LentZip {
      a_iter: self,
      b_iter: other,
    }
  }
}
