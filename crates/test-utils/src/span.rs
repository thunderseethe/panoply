use base::{
  file::FileId,
  loc::Loc,
  span::{Span, SpanOf},
};
use salsa::AsId;

pub fn random_loc() -> Loc {
  Loc {
    file: FileId::from_id(salsa::Id::from_u32(0)),
    byte: rand::random(),
    line: rand::random(),
    col: rand::random(),
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
