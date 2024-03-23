use std::fmt::{Display, Formatter, Result};

// Implements `Display` by displaying each element of an iterator in order.
pub(crate) struct DisplayIter<I>(I);

impl<I> DisplayIter<I> {
  #[allow(dead_code)]
  pub(crate) fn new(iter: I) -> DisplayIter<I> {
    DisplayIter(iter)
  }
}

impl<I> Display for DisplayIter<I>
where
  I: Clone + Iterator,
  I::Item: Display,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    self.0.clone().try_for_each(|item| item.fmt(f))
  }
}

// Implements `Display` by displaying each element of an iterator in order, separated by a
// separator.
pub(crate) struct DisplayIterSeparated<I, S> {
  iter: I,
  separator: S,
}

impl<I, S> DisplayIterSeparated<I, S> {
  pub(crate) fn new(iter: I, separator: S) -> DisplayIterSeparated<I, S> {
    DisplayIterSeparated { iter, separator }
  }
}

impl<I, S> Display for DisplayIterSeparated<I, S>
where
  I: Clone + Iterator,
  I::Item: Display,
  S: Display,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> Result {
    let mut iter = self.iter.clone();
    iter
      .clone()
      .next()
      .map(|item| item.fmt(f))
      .map_or(Ok(()), |res| {
        res
          .and_then(|()| iter.try_for_each(|item| item.fmt(f).and_then(|()| self.separator.fmt(f))))
      })
  }
}
