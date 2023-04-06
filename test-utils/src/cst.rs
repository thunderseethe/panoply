use aiahr_core::cst::{self, Field};

use crate::span::random_span;

pub fn random_field<L, T>(label: L, target: T) -> Field<L, T> {
    Field {
        label,
        sep: random_span(),
        target,
    }
}

pub fn random_sep<T, II>(elems: impl IntoIterator<IntoIter = II>) -> cst::indexed::Separated<T>
where
    II: Iterator<Item = T> + ExactSizeIterator,
{
    let mut iter = elems.into_iter();
    let first = iter.next().unwrap();
    cst::indexed::Separated {
        first,
        elems: iter.map(|t| (random_span(), t)).collect(),
        comma: None,
    }
}
