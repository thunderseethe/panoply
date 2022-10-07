use std::{collections::HashMap, fmt::Debug, iter};

use aiahr_core::{
    id::{ItemId, ModuleId, VarId},
    memory::handle::RefHandle,
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
enum Data<'n, 'a, 's> {
    Base(&'n BaseNames<'a, 's>),
    Scope {
        locals: HashMap<RefHandle<'s, str>, VarId>,
        next: &'n Names<'n, 'a, 's>,
    },
}

/// The names visible from a given context in a module. Supports shadowing.
#[derive(Debug)]
pub struct Names<'n, 'a, 's>(Data<'n, 'a, 's>);

impl<'n, 'a, 's> Names<'n, 'a, 's> {
    /// Returns a new [`Names`] with the given base.
    pub fn new(base: &'n BaseNames<'a, 's>) -> Names<'n, 'a, 's> {
        Names(Data::Base(base))
    }

    /// Returns a new subscope of `self`. Names added to the returned object will shadow those in
    /// `self`.
    pub fn subscope<'m>(&'m self) -> Names<'m, 'a, 's> {
        Names(Data::Scope {
            locals: HashMap::new(),
            next: self,
        })
    }

    // The `BaseNames` at the root of this object.
    fn base(&self) -> &BaseNames<'a, 's> {
        match &self.0 {
            Data::Base(b) => b,
            Data::Scope { next, .. } => next.base(),
        }
    }

    // An iterator over the layers of names, from front to back.
    fn layers(&self) -> impl Iterator<Item = &Data<'n, 'a, 's>> {
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

    /// Resolves the given name.
    pub fn get(&self, name: RefHandle<'s, str>) -> Option<Name> {
        self.layers().find_map(|layer| match layer {
            Data::Base(base) => base.get(name).map(|memb| match memb {
                Member::Module(m) => Name::Module(m),
                Member::Item(i) => Name::Item(self.this(), i),
            }),
            Data::Scope { locals, .. } => locals.get(&name).map(|v| Name::Variable(*v)),
        })
    }

    /// Resolves the given name as a member of the given module.
    pub fn get_in(&self, module: ModuleId, name: RefHandle<'s, str>) -> Option<Member> {
        self.base().get_in(module, name)
    }

    /// Inserts the new variable into the current scope with the given ID. If the name is a
    /// duplicate in the current scope, returns the ID of the original variable with that name.
    pub fn insert(&mut self, name: RefHandle<'s, str>, id: VarId) -> Option<VarId> {
        match &mut self.0 {
            // TODO: we can do better than panicking here
            Data::Base(..) => panic!("Cannot insert names into the base Names layer"),
            Data::Scope { ref mut locals, .. } => {
                if let Some((_, orig)) = locals.get_key_value(&name) {
                    Some(*orig)
                } else {
                    locals.insert(name, id);
                    None
                }
            }
        }
    }
}
