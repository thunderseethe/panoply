use std::{collections::HashMap, fmt::Debug, iter, mem::ManuallyDrop};

use aiahr_core::{
    diagnostic::{nameres::NameResolutionError, DiagnosticSink},
    id::{ItemId, ModuleId, VarId},
    span::{SpanOf, Spanned},
};

use crate::{base::BaseNames, modules::Member};

/// The result of resolving a name in context.
#[derive(Clone, Copy, Debug)]
pub enum Name {
    Module(ModuleId),
    Item(ModuleId, ItemId),
    Variable(VarId),
}

#[derive(Debug)]
enum Data<'n, 'a, 'i> {
    Base(&'n BaseNames<'a, 'i>),
    Scope {
        locals: HashMap<&'i str, SpanOf<VarId>>,
        next: &'n Names<'n, 'a, 'i>,
    },
}

/// A reserved slot for a variable name. Does not implement [`Drop`], so it must be returned to the
/// [`Names`] so that the slot can be filled.
#[derive(Debug)]
pub struct VarSlot(ManuallyDrop<VarId>);

/// The names visible from a given context in a module. Supports shadowing.
#[derive(Debug)]
pub struct Names<'n, 'a, 'i>(Data<'n, 'a, 'i>);

impl<'n, 'a, 'i> Names<'n, 'a, 'i> {
    /// Returns a new [`Names`] with the given base.
    pub fn new(base: &'n BaseNames<'a, 'i>) -> Names<'n, 'a, 'i> {
        Names(Data::Base(base))
    }

    /// Returns a new subscope of `self`. Names added to the returned object will shadow those in
    /// `self`.
    pub fn subscope<'m>(&'m self) -> Names<'m, 'a, 'i> {
        Names(Data::Scope {
            locals: HashMap::new(),
            next: self,
        })
    }

    // The `BaseNames` at the root of this object.
    fn base(&self) -> &BaseNames<'a, 'i> {
        match &self.0 {
            Data::Base(b) => b,
            Data::Scope { next, .. } => next.base(),
        }
    }

    // An iterator over the layers of names, from front to back.
    fn layers(&self) -> impl Iterator<Item = &Data<'n, 'a, 'i>> {
        iter::successors(Some(self), |names| match &names.0 {
            Data::Base { .. } => None,
            Data::Scope { next, .. } => Some(next),
        })
        .map(|names| &names.0)
    }

    /// Gets the ID of the owning module.
    pub fn this(&self) -> ModuleId {
        self.base().this()
    }

    /// Resolves the given name, or reports an error to `errors`.
    pub fn get<E>(&self, name: SpanOf<&'i str>, errors: &mut E) -> Option<SpanOf<Name>>
    where
        E: DiagnosticSink<NameResolutionError<'i>>,
    {
        let out = self
            .layers()
            .find_map(|layer| match layer {
                Data::Base(base) => base.get(name.value).map(|memb| match memb {
                    Member::Module(m) => Name::Module(m),
                    Member::Item(i) => Name::Item(self.this(), i),
                }),
                Data::Scope { locals, .. } => locals
                    .get(name.value)
                    .map(|v| Name::Variable(v.value.clone())),
            })
            .map(|n| name.span().of(n));
        if out.is_none() {
            errors.add(NameResolutionError::NotFound(name));
        }
        out
    }

    /// Resolves the given name as a member of the given module, or reports an error to `errors`.
    pub fn get_in<E>(
        &self,
        module: ModuleId,
        name: SpanOf<&'i str>,
        errors: &mut E,
    ) -> Option<Member>
    where
        E: DiagnosticSink<NameResolutionError<'i>>,
    {
        let out = self.base().get_in(module, name.value);
        if out.is_none() {
            errors.add(NameResolutionError::NotFound(name));
        }
        out
    }

    /// Reserves an ID for the given variable.
    pub fn reserve(&self, name: SpanOf<&'i str>) -> VarSlot {
        VarSlot(ManuallyDrop::new(self.base().make_id(name)))
    }

    /// Inserts the new variable into the current scope, or reports a `Duplicate` error. Returns the
    /// ID of the variable.
    ///
    /// If `slot` is given, uses the provided slot for the variable.
    pub fn insert<E: DiagnosticSink<NameResolutionError<'i>>>(
        &mut self,
        name: SpanOf<&'i str>,
        slot: Option<VarSlot>,
        errors: &mut E,
    ) -> SpanOf<VarId> {
        let id = match slot {
            Some(sl) => ManuallyDrop::into_inner(sl.0),
            None => self.base().make_id(name),
        };
        match &mut self.0 {
            // TODO: we can do better than panicking here
            Data::Base(..) => panic!("Cannot insert names into the base Names layer"),
            Data::Scope { ref mut locals, .. } => {
                let out = name.span().of(id);
                if let Some((&orig, v)) = locals.get_key_value(name.value) {
                    errors.add(NameResolutionError::Duplicate {
                        original: v.span().of(orig),
                        duplicate: name,
                    });
                } else {
                    locals.insert(name.value, out);
                }
                out
            }
        }
    }
}
