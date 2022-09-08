use std::{
    collections::HashMap,
    fmt::Debug,
    iter::{self, FusedIterator},
};

use aiahr_core::{
    diagnostic::{nameres::NameResolutionError, DiagnosticSink},
    handle::Handle,
    span::{Span, SpanOf, Spanned},
};

// Constructs a new layer from `names`. If `names` contains duplicates, then errors are reported to
// `errors` and `false` is returned in the second value.
fn iter_layer<'i, I, E>(names: I, errors: &mut E) -> (HashMap<&'i str, Span>, bool)
where
    I: IntoIterator<Item = SpanOf<&'i str>>,
    E: DiagnosticSink<NameResolutionError<'i>>,
{
    let mut layer = HashMap::<_, Span>::new();
    let ok = names.into_iter().all(|name| {
        if let Some((&n, &s)) = layer.get_key_value(name.value) {
            errors.add(NameResolutionError::Duplicate {
                original: s.of(n),
                duplicate: name,
            });
            false
        } else {
            layer.insert(name.value, name.span());
            true
        }
    });
    (layer, ok)
}

/// A map from names to handles for them. Supports shadowing.
#[derive(Debug)]
pub struct Names<'n, 'i> {
    locals: HashMap<&'i str, Span>,
    next: Option<&'n Names<'n, 'i>>,
}

impl<'n, 'i> Names<'n, 'i> {
    /// Returns a new `Names` with the given top-level names. If `top_level` contains duplicates,
    /// errors are reported to `errors` and `false` is returned in the second value.
    pub fn with_top_level<I, E>(top_level: I, errors: &mut E) -> (Names<'n, 'i>, bool)
    where
        I: IntoIterator<Item = SpanOf<&'i str>>,
        E: DiagnosticSink<NameResolutionError<'i>>,
    {
        let (locals, ok) = iter_layer(top_level, errors);
        (Names { locals, next: None }, ok)
    }

    /// Returns a new subscope of `self`. Names added to the returned object will shadow those in
    /// `self`.
    pub fn subscope<'m>(&'m self) -> Names<'m, 'i> {
        self.subscope_with_map(HashMap::new())
    }

    /// Returns a new subscope of `self` with the given name. `name` and other names added to the
    /// returned object will shadow those in `self`.
    pub fn subscope_with_one<'m>(&'m self, name: SpanOf<&'i str>) -> Names<'m, 'i> {
        self.subscope_with_map(HashMap::from([(name.value, name.span())]))
    }

    fn subscope_with_map<'m>(&'m self, layer: HashMap<&'i str, Span>) -> Names<'m, 'i> {
        Names {
            locals: layer,
            next: Some(self),
        }
    }

    // An iterator over the layers of names, from front to back.
    fn layers<'a>(
        &'a self,
    ) -> impl Clone + Debug + FusedIterator + Iterator<Item = &'a HashMap<&'i str, Span>> {
        iter::successors(Some(self), |n| n.next).map(|n| &n.locals)
    }

    /// Gets the handle for the given name, or reports an error to `errors`.
    pub fn get<E: DiagnosticSink<NameResolutionError<'i>>>(
        &self,
        name: SpanOf<&'i str>,
        errors: &mut E,
    ) -> Option<SpanOf<Handle<'i, str>>> {
        let out = self
            .layers()
            .find_map(|layer| layer.get_key_value(name.value))
            .map(|(&orig, _)| name.span().of(Handle(orig)));
        if let None = out {
            errors.add(NameResolutionError::NotFound(name));
        }
        out
    }

    /// Inserts the new element into the frontmost layer, or reports a `Duplicate` error. Returns
    /// a handle to the new name if successful.
    pub fn insert<E: DiagnosticSink<NameResolutionError<'i>>>(
        &mut self,
        name: SpanOf<&'i str>,
        errors: &mut E,
    ) -> Option<SpanOf<Handle<'i, str>>> {
        if let Some((&orig, &s)) = self.locals.get_key_value(name.value) {
            errors.add(NameResolutionError::Duplicate {
                original: s.of(orig),
                duplicate: name,
            });
            None
        } else {
            self.locals.insert(name.value, name.span());
            Some(name.map(Handle))
        }
    }
}
