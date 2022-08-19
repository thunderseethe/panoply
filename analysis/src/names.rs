use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Debug,
    iter::{self, FusedIterator},
};

use aiahr_core::{
    error::{Errors, NameResolutionError},
    handle::Handle,
    span::SpanOf,
};

// Returns a hash set of `names`. If `names` contains duplicates, then errors are reported to
// `errors` and `false` is returned in the second value.
fn to_hash_set<'i, I, E>(names: I, errors: &mut E) -> (HashSet<&'i str>, bool)
where
    I: IntoIterator<Item = SpanOf<&'i str>>,
    E: Errors<NameResolutionError<'i>>,
{
    let mut map = HashMap::new();
    let ok = names.into_iter().all(|name| match map.entry(name.0) {
        Entry::Occupied(o) => {
            errors.push(NameResolutionError::Duplicate {
                original: *o.get(),
                duplicate: name,
            });
            false
        }
        Entry::Vacant(v) => {
            v.insert(name);
            true
        }
    });
    (HashSet::from_iter(map.into_keys()), ok)
}

/// A map from names to handles for them. Supports shadowing.
#[derive(Debug)]
pub struct Names<'n, 'i> {
    locals: HashSet<&'i str>,
    next: Option<&'n Names<'n, 'i>>,
}

impl<'n, 'i> Names<'n, 'i> {
    /// Returns a new `Names` with the given top-level names. If `top_level` contains duplicates,
    /// errors are reported to `errors` and `false` is returned in the second value.
    pub fn new<I, E>(top_level: I, errors: &mut E) -> (Names<'n, 'i>, bool)
    where
        I: IntoIterator<Item = SpanOf<&'i str>>,
        E: Errors<NameResolutionError<'i>>,
    {
        let (locals, ok) = to_hash_set(top_level, errors);
        (
            Names {
                locals: locals,
                next: None,
            },
            ok,
        )
    }

    /// Returns a new `Names` with the given name in addition to those in `self`. If `name` already
    /// exists in `self`, then the new instance will shadow the old one.
    pub fn subscope<'m>(&'m self, name: &'i str) -> Names<'m, 'i> {
        self.subscope_set(HashSet::from([name]))
    }

    /// As `subscope`, but with a set of new names.
    pub fn subscope_set<'m>(&'m self, scope: HashSet<&'i str>) -> Names<'m, 'i> {
        Names {
            locals: scope,
            next: Some(self),
        }
    }

    // An iterator over the layers of names, from front to back.
    fn layers<'a>(
        &'a self,
    ) -> impl Clone + Debug + FusedIterator + Iterator<Item = &'a HashSet<&'i str>> {
        iter::successors(Some(self), |n| n.next).map(|n| &n.locals)
    }

    /// Gets the handle for the given name, or reports an error to `errors`.
    pub fn get<'a, E>(
        &'a self,
        name: SpanOf<&'i str>,
        errors: &mut E,
    ) -> Option<SpanOf<Handle<'i, str>>>
    where
        E: Errors<NameResolutionError<'i>>,
    {
        let (n, s) = name;
        let out = self
            .layers()
            .find_map(|set| set.get(n))
            .map(|orig| (Handle(*orig), s));
        if let None = out {
            errors.push(NameResolutionError::NotFound(name));
        }
        out
    }
}
