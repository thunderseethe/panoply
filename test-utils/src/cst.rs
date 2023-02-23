use aiahr_core::cst::{Field, Separated};
use bumpalo::Bump;

use crate::span::random_span;


pub fn random_field<L, T>(label: L, target: T) -> Field<L, T> {
    Field {
        label,
        sep: random_span(),
        target,
    }
}

pub fn random_sep<'a, T, II>(
    arena: &'a Bump,
    elems: impl IntoIterator<IntoIter = II>,
) -> Separated<'a, T>
where
    II: Iterator<Item = T> + ExactSizeIterator,
{
    let mut iter = elems.into_iter();
    let first = iter.next().unwrap();
    Separated {
        first,
        elems: arena.alloc_slice_fill_iter(iter.map(|t| (random_span(), t))),
        comma: None,
    }
}
