use aiahr_ast::{Ast, Direction, Term, Term::*};
use aiahr_core::{
    id::{Id, ItemId},
    span::Span,
};
use la_arena::{Arena, Idx};
use rustc_hash::FxHashMap;

use std::{cell::RefCell, hash::Hash};

use crate::span::random_span;

pub trait MkTerm<'a, Var> {
    fn mk_abs(&self, arg: Var, body: Term<Var>) -> Term<Var>;
    fn mk_app(&self, fun: Term<Var>, arg: Term<Var>) -> Term<Var>;
    fn mk_label(&self, label: &str, term: Term<Var>) -> Term<Var>;
    fn mk_unlabel(&self, label: &str, term: Term<Var>) -> Term<Var>;
    fn mk_concat(&self, left: Term<Var>, right: Term<Var>) -> Term<Var>;
    fn mk_project(&self, direction: Direction, term: Term<Var>) -> Term<Var>;
    fn mk_branch(&self, left: Term<Var>, right: Term<Var>) -> Term<Var>;
    fn mk_inject(&self, direction: Direction, term: Term<Var>) -> Term<Var>;
    fn mk_handler(&self, handler: Term<Var>, body: Term<Var>) -> Term<Var>;

    fn mk_abss<II>(&self, args: II, body: Term<Var>) -> Term<Var>
    where
        II: IntoIterator,
        II::IntoIter: DoubleEndedIterator<Item = Var>,
    {
        args.into_iter()
            .rfold(body, |body, arg| self.mk_abs(arg, body))
    }
}

/// AstBuilder makes it easier to create Ast instances in tests while maintaing expected
/// invariants.
/// It will arena allocate all nodes in the AST and create random spans for them in the final
/// AST.
pub struct AstBuilder<'a, Var> {
    db: &'a dyn aiahr_core::Db,
    name: ItemId,
    //arena: &'a Bump,
    terms: RefCell<Arena<Term<Var>>>,
    // TODO: This is bad but it's annoying to fix because rust won't let you take a mutable borrow
    // and then a second mutable borrow passed as a parameter to the original. Even though that'll
    // work if you store the value in a temporary.
    spans: RefCell<FxHashMap<Idx<Term<Var>>, Span>>,
}

impl<'a, Var> AstBuilder<'a, Var> {
    pub fn new(db: &'a dyn aiahr_core::Db) -> Self {
        Self {
            db,
            name: ItemId::from_raw(0),
            terms: RefCell::new(Arena::default()),
            spans: RefCell::new(FxHashMap::default()),
        }
    }

    pub fn with_name(db: &'a dyn aiahr_core::Db, name: ItemId) -> Self {
        Self {
            db,
            name,
            terms: RefCell::new(Arena::default()),
            spans: RefCell::new(FxHashMap::default()),
        }
    }
}
impl<'a, Var: Eq + Hash> AstBuilder<'a, Var> {
    fn mk_term(&self, term: Term<Var>) -> Idx<Term<Var>> {
        let t = self.terms.borrow_mut().alloc(term);
        self.spans.borrow_mut().insert(t, random_span());
        t
    }

    pub fn build(self, root: Term<Var>) -> Ast<Var> {
        let root = self.mk_term(root);
        Ast::with_untyped(
            self.name,
            self.spans.into_inner(),
            self.terms.into_inner(),
            root,
        )
    }

    pub fn with_builder(
        db: &'a dyn aiahr_core::Db,
        op: impl FnOnce(&Self) -> Term<Var>,
    ) -> Ast<Var> {
        let builder = Self::new(db);
        let root = op(&builder);
        builder.build(root)
    }
}
impl<'a, Var: Eq + Hash> MkTerm<'a, Var> for AstBuilder<'a, Var> {
    fn mk_abs(&self, arg: Var, body: Term<Var>) -> Term<Var> {
        Abstraction {
            arg,
            body: self.mk_term(body),
        }
    }

    fn mk_app(&self, fun: Term<Var>, arg: Term<Var>) -> Term<Var> {
        Application {
            func: self.mk_term(fun),
            arg: self.mk_term(arg),
        }
    }

    fn mk_label(&self, label: &str, term: Term<Var>) -> Term<Var> {
        Label {
            label: self.db.ident(label.to_string()),
            term: self.mk_term(term),
        }
    }

    fn mk_unlabel(&self, label: &str, term: Term<Var>) -> Term<Var> {
        Unlabel {
            label: self.db.ident(label.to_string()),
            term: self.mk_term(term),
        }
    }

    fn mk_concat(&self, left: Term<Var>, right: Term<Var>) -> Term<Var> {
        Concat {
            left: self.mk_term(left),
            right: self.mk_term(right),
        }
    }

    fn mk_project(&self, direction: Direction, term: Term<Var>) -> Term<Var> {
        Project {
            direction,
            term: self.mk_term(term),
        }
    }

    fn mk_branch(&self, left: Term<Var>, right: Term<Var>) -> Term<Var> {
        Branch {
            left: self.mk_term(left),
            right: self.mk_term(right),
        }
    }

    fn mk_inject(&self, direction: Direction, term: Term<Var>) -> Term<Var> {
        Inject {
            direction,
            term: self.mk_term(term),
        }
    }

    fn mk_handler(&self, handler: Term<Var>, body: Term<Var>) -> Term<Var> {
        Handle {
            handler: self.mk_term(handler),
            body: self.mk_term(body),
        }
    }
}
