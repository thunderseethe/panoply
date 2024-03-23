/// Adds iterator extensions for creating English text.
pub(crate) trait EnglishIterExt: Iterator {
  /// Returns English text for a list of the contained items using the given conjunction (e.g.,
  /// "and" or "or"). Always uses an Oxford comma.
  fn english_list(self, conjunction: &str) -> String;
}

#[derive(Debug)]
enum ListAccumulator<'a> {
  Empty,
  One(&'a str),
  Two(&'a str, &'a str),
  ThreeOrMore { firsts: String, last: &'a str },
}

impl<'a> ListAccumulator<'a> {
  fn add(self, item: &'a str) -> ListAccumulator<'a> {
    match self {
      ListAccumulator::Empty => ListAccumulator::One(item),
      ListAccumulator::One(first) => ListAccumulator::Two(first, item),
      ListAccumulator::Two(first, second) => ListAccumulator::ThreeOrMore {
        firsts: format!("{}, {}", first, second),
        last: item,
      },
      ListAccumulator::ThreeOrMore { firsts, last } => {
        let mut firsts = firsts;
        firsts.extend([", ", last]);
        ListAccumulator::ThreeOrMore { firsts, last: item }
      }
    }
  }

  fn finish(self, conjunction: &str) -> String {
    match self {
      ListAccumulator::Empty => "".to_string(),
      ListAccumulator::One(one) => one.to_string(),
      ListAccumulator::Two(first, second) => format!("{} {} {}", first, conjunction, second),
      ListAccumulator::ThreeOrMore { firsts, last } => {
        let mut firsts = firsts;
        // Use an Oxford comma.
        firsts.extend([", ", conjunction, " ", last]);
        firsts
      }
    }
  }
}

impl<'a, T> EnglishIterExt for T
where
  T: Iterator<Item = &'a str>,
{
  fn english_list(self, conjunction: &str) -> String {
    self
      .fold(ListAccumulator::Empty, |acc, item| acc.add(item))
      .finish(conjunction)
  }
}
