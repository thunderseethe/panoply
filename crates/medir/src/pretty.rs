use base::{id::Id, pretty::PrettyWithCtx};
use pretty::{DocAllocator, Pretty};

use crate::{
  Atom, Call, Defn, Locals, MedIr, MedIrItemName, MedIrKind, MedIrTy, MedIrTyKind, MedIrVar,
};

impl<DB: ?Sized + reducir::Db> PrettyWithCtx<DB> for MedIrItemName {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    alloc: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    self.0.pretty(ctx, alloc)
  }
}

impl<'a, D, A: 'a> pretty::Pretty<'a, D, A> for &MedIrVar
where
  D: DocAllocator<'a, A>,
{
  fn pretty(self, a: &'a D) -> pretty::DocBuilder<'a, D, A> {
    a.text("V").append(a.as_string(self.id.raw()))
  }
}

#[allow(dead_code)]
impl MedIrVar {
  fn with_type(self) -> PrettyWithType {
    PrettyWithType(self)
  }
}

struct PrettyWithType(MedIrVar);

impl<Db: ?Sized + crate::Db> PrettyWithCtx<Db> for PrettyWithType {
  fn pretty<'a>(
    &self,
    db: &Db,
    a: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    a.text("V")
      .append(a.as_string(self.0.id.raw()))
      .append(": ")
      .append(self.0.ty.pretty(db, a))
  }
}

impl<'a, D, A: 'a> pretty::Pretty<'a, D, A> for &Atom
where
  D: DocAllocator<'a, A>,
{
  fn pretty(self, a: &'a D) -> pretty::DocBuilder<'a, D, A> {
    match self {
      Atom::Var(v) => v.pretty(a),
      Atom::Int(i) => a.as_string(i),
    }
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for Call {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    alloc: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    match self {
      Call::Known(item) => item.name.pretty(ctx, alloc),
      Call::Unknown(v) => alloc.text("apply_closure").append(v.pretty(alloc).parens()),
    }
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for MedIrKind {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    a: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    match self {
      MedIrKind::Atom(atom) => atom.pretty(a),
      MedIrKind::Blocks(elems) => a
        .intersperse(elems.iter().map(|elem| elem.pretty(a)), a.text(", "))
        .brackets(),
      MedIrKind::BlockAccess(v, indx) => v.pretty(a).append(a.as_string(indx).brackets()),
      MedIrKind::Switch(scrutinee, branches) => a
        .text("switch")
        .append(scrutinee.pretty(a).enclose(a.space(), a.space()))
        .append("<")
        .append(
          a.line()
            .append(a.intersperse(
              branches.iter().enumerate().map(|(i, branch)| {
                a.text("branch")
                  .append(a.space())
                  .append(a.as_string(i))
                  .append(a.space())
                  .append(branch.pretty(ctx, a))
              }),
              a.line(),
            ))
            .nest(2),
        )
        .append(a.line())
        .append(">"),
      MedIrKind::Call(fun, args) => fun.pretty(ctx, a).append(
        a.intersperse(
          args.iter().map(|arg| arg.pretty(a)),
          a.text(",").append(a.space()),
        )
        .parens(),
      ),
      MedIrKind::Closure(item, env) => {
        let env_doc = a
          .intersperse(
            env.iter().map(|v| v.pretty(a)),
            a.text(",").append(a.space()),
          )
          .brackets();
        a.text("make_closure").append(
          item
            .name
            .pretty(ctx, a)
            .append(a.text(","))
            .append(env_doc)
            .parens(),
        )
      }
      MedIrKind::Typecast(ty, term) => a
        .text("typecast")
        .append(ty.pretty(ctx, a).angles())
        .append(term.pretty(ctx, a).parens()),
    }
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for MedIr {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    alloc: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    self.kind.pretty(ctx, alloc)
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for Locals {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    a: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    a.line()
      .append(a.concat(self.binds.iter().map(|(var, defn)| {
        a.text("let")
          .append(a.space())
          .append(var.pretty(a))
          .append(a.space())
          .append("=")
          .append(a.space())
          .append(defn.pretty(ctx, a))
          .append(a.text(";"))
          .append(a.line())
      })))
      .append(self.body.pretty(ctx, a))
      .nest(2)
      .append(a.line())
      .braces()
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for Defn {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    a: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    a.text("defn")
      .append(a.space())
      .append(self.name.pretty(ctx, a))
      .append(
        a.intersperse(
          self.params.iter().map(|var| pretty::Pretty::pretty(var, a)),
          a.text(",").append(a.space()),
        )
        .parens(),
      )
      .append(a.space())
      .append(self.body.pretty(ctx, a))
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for MedIrTyKind {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    alloc: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    match self {
      MedIrTyKind::IntTy => alloc.text("Int"),
      MedIrTyKind::BlockTy(tys) => alloc
        .intersperse(tys.iter().map(|ty| ty.pretty(ctx, alloc)), ",")
        .brackets(),
      MedIrTyKind::FunTy(args, ret) => alloc
        .intersperse(args.iter().map(|ty| ty.pretty(ctx, alloc)), ",")
        .parens()
        .append(alloc.space())
        .append("->")
        .append(alloc.space())
        .append(ret.pretty(ctx, alloc)),
    }
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for MedIrTy {
  fn pretty<'a>(
    &self,
    ctx: &DB,
    alloc: &'a pretty::RcAllocator,
  ) -> pretty::DocBuilder<'a, pretty::RcAllocator> {
    self.kind(ctx).pretty(ctx, alloc)
  }
}
