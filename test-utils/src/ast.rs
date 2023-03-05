use aiahr_core::{
    ast::{Ast, Direction, Term, Term::*},
    span::Span,
    AsCoreDb,
};
use bumpalo::Bump;
use rustc_hash::FxHashMap;

use std::{cell::RefCell, hash::Hash};

use crate::span::random_span;

pub trait MkTerm<'a, Var> {
    fn mk_abs(&self, arg: Var, body: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_app(&self, fun: Term<'a, Var>, arg: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_label(&self, label: &str, term: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_unlabel(&self, label: &str, term: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_concat(&self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_project(&self, direction: Direction, term: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_branch(&self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_inject(&self, direction: Direction, term: Term<'a, Var>) -> Term<'a, Var>;
    fn mk_handler(&self, handler: Term<'a, Var>, body: Term<'a, Var>) -> Term<'a, Var>;

    fn mk_abss<II>(&self, args: II, body: Term<'a, Var>) -> Term<'a, Var>
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
    arena: &'a Bump,
    // TODO: This is bad but it's annoying to fix because rust won't let you take a mutable borrow
    // and then a second mutable borrow passed as a parameter to the original. Even though that'll
    // work if you store the value in a temporary.
    spans: RefCell<FxHashMap<&'a Term<'a, Var>, Span>>,
}

impl<'a, Var> AstBuilder<'a, Var> {
    pub fn new(db: &'a dyn aiahr_core::Db, arena: &'a Bump) -> Self {
        Self {
            db,
            arena,
            spans: RefCell::new(FxHashMap::default()),
        }
    }
}
impl<'a, Var: Eq + Hash> AstBuilder<'a, Var> {
    fn mk_term(&self, term: Term<'a, Var>) -> &'a Term<'a, Var> {
        let t = self.arena.alloc(term);
        self.spans.borrow_mut().insert(t, random_span());
        t
    }

    pub fn build(self, root: Term<'a, Var>) -> Ast<'a, Var> {
        let root = self.mk_term(root);
        Ast::new(self.spans.into_inner(), root)
    }

    pub fn with_builder(
        db: &'a dyn aiahr_core::Db,
        arena: &'a Bump,
        op: impl FnOnce(&Self) -> Term<'a, Var>,
    ) -> Ast<'a, Var> {
        let builder = Self::new(db, arena);
        let root = op(&builder);
        builder.build(root)
    }
}
impl<'a, Var: Eq + Hash> MkTerm<'a, Var> for AstBuilder<'a, Var> {
    fn mk_abs(&self, arg: Var, body: Term<'a, Var>) -> Term<'a, Var> {
        Abstraction {
            arg,
            body: self.mk_term(body),
        }
    }

    fn mk_app(&self, fun: Term<'a, Var>, arg: Term<'a, Var>) -> Term<'a, Var> {
        Application {
            func: self.mk_term(fun),
            arg: self.mk_term(arg),
        }
    }

    fn mk_label(&self, label: &str, term: Term<'a, Var>) -> Term<'a, Var> {
        Label {
            label: self.db.ident(label),
            term: self.mk_term(term),
        }
    }

    fn mk_unlabel(&self, label: &str, term: Term<'a, Var>) -> Term<'a, Var> {
        Unlabel {
            label: self.db.ident(label),
            term: self.mk_term(term),
        }
    }

    fn mk_concat(&self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var> {
        Concat {
            left: self.mk_term(left),
            right: self.mk_term(right),
        }
    }

    fn mk_project(&self, direction: Direction, term: Term<'a, Var>) -> Term<'a, Var> {
        Project {
            direction,
            term: self.mk_term(term),
        }
    }

    fn mk_branch(&self, left: Term<'a, Var>, right: Term<'a, Var>) -> Term<'a, Var> {
        Branch {
            left: self.mk_term(left),
            right: self.mk_term(right),
        }
    }

    fn mk_inject(&self, direction: Direction, term: Term<'a, Var>) -> Term<'a, Var> {
        Inject {
            direction,
            term: self.mk_term(term),
        }
    }

    fn mk_handler(&self, handler: Term<'a, Var>, body: Term<'a, Var>) -> Term<'a, Var> {
        Handle {
            handler: self.mk_term(handler),
            body: self.mk_term(body),
        }
    }
}
