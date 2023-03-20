use aiahr_core::id::{Id, ItemId};
use aiahr_core::nst::{Item, Term};

use crate::span::{random_span, random_span_of};

pub fn random_term_item<'n>(term: &'n Term<'n>) -> Item<'n> {
    Item::Term {
        name: random_span_of(ItemId::from_raw(rand::random())),
        annotation: None,
        eq: random_span(),
        value: term,
    }
}
