use base::{
  loc::Loc,
  span::{Span, SpanOf},
};

pub fn random_loc() -> Loc {
  Loc {
    byte: rand::random(),
  }
}
pub fn random_span() -> Span {
  Span {
    start: random_loc(),
    end: random_loc(),
  }
}
pub fn random_span_of<T>(value: T) -> SpanOf<T> {
  let span = random_span();
  SpanOf {
    start: span.start,
    value,
    end: span.end,
  }
}
