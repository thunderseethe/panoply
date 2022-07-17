use std::ops::Range;

use crate::{loc::Loc, span::Span, token::Token};

// TODO: design a better error type
#[derive(Debug)]
pub enum ParseError<'i> {
    WrongToken {
        loc: Loc,
        got: Option<Token<'i>>, // None indicates EOF
        want_any: Vec<Option<Token<'i>>>,
    },
}

impl<'i> chumsky::Error<Span<Token<'i>>> for Vec<ParseError<'i>> {
    type Span = Range<usize>;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Span<Token<'i>>>>>(
        _: Self::Span,
        expected: Iter,
        found: Option<Span<Token<'i>>>,
    ) -> Self {
        vec![ParseError::WrongToken {
            loc: found.as_ref().map_or(
                Loc {
                    byte: 0,
                    line: 0,
                    col: 0,
                },
                |s| s.start,
            ),
            got: found.map(|s| s.val),
            want_any: expected.into_iter().map(|o| o.map(|s| s.val)).collect(),
        }]
    }

    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    fn merge(self, other: Self) -> Self {
        let mut errors = self;
        let mut other = other;
        errors.append(&mut other);
        errors
    }
}
