use crate::{loc::Loc, span::Span, token::Token};

// TODO: design a better error type
#[derive(Debug, Clone)]
pub enum ParseError<'i> {
    WrongToken {
        loc: Loc,
        got: Option<Token<'i>>, // None indicates EOF
        want_any: Vec<Option<Token<'i>>>,
    },
}

#[derive(Debug)]
pub struct ParseErrors<'i>(Vec<ParseError<'i>>);

impl<'i> chumsky::Error<Token<'i>> for ParseErrors<'i> {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'i>>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<Token<'i>>,
    ) -> Self {
        ParseErrors(vec![ParseError::WrongToken {
            loc: span.start,
            got: found.map(|s| s),
            want_any: expected.into_iter().collect(),
        }])
    }

    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    fn merge(self, other: Self) -> Self {
        let ParseErrors(mut errors) = self;
        let ParseErrors(mut other) = other;
        errors.append(&mut other);
        ParseErrors(errors)
    }
}
