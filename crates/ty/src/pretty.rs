use base::pretty::PrettyWithCtx;
use pretty::{docs, DocAllocator, Pretty, RcAllocator};

use crate::{AccessTy, Evidence, Ty, TyScheme, TypeAlloc, TypeKind};

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for Ty<A>
where
  A: TypeAlloc + 'ty,
  Acc: AccessTy<'ty, A>,
  Db: ?Sized + crate::Db,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'doc>(
    &self,
    ctx: &(&Db, Acc),
    alloc: &'doc pretty::RcAllocator,
  ) -> pretty::DocBuilder<'doc, pretty::RcAllocator> {
    ctx.1.kind(self).pretty_with(ctx).pretty(alloc)
  }
}

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for TypeKind<A>
where
  A: TypeAlloc + 'ty,
  Acc: AccessTy<'ty, A>,
  Db: ?Sized + crate::Db,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'doc>(
    &self,
    ctx: &(&Db, Acc),
    a: &'doc pretty::RcAllocator,
  ) -> pretty::DocBuilder<'doc, pretty::RcAllocator> {
    match self {
      TypeKind::ErrorTy => a.as_string("Error"),
      TypeKind::IntTy => a.as_string("Int"),
      TypeKind::VarTy(tv) => tv.clone().pretty(a),
      TypeKind::RowTy(simple_row) => simple_row
        .pretty_with(ctx)
        .pretty(a)
        .nest(2)
        .parens()
        .group(),
      TypeKind::FunTy(arg, eff, ret) => arg.pretty_with(ctx).pretty(a).append(
        docs![
          a,
          a.softline(),
          "->",
          a.softline().append(eff.pretty(ctx, a)).append(a.space()),
          &ret.pretty_with(ctx)
        ]
        .nest(2),
      ),
      TypeKind::ProdTy(row) => row
        .pretty_with(ctx)
        .pretty(a)
        .enclose(a.softline(), a.softline())
        .braces()
        .group(),
      TypeKind::SumTy(row) => row
        .pretty_with(ctx)
        .pretty(a)
        .enclose(a.softline(), a.softline())
        .angles()
        .group(),
    }
  }
}

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for Evidence<A>
where
  A: TypeAlloc + 'ty,
  Acc: AccessTy<'ty, A>,
  Db: ?Sized + crate::Db,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'a>(
    &self,
    ctx: &(&Db, Acc),
    a: &'a RcAllocator,
  ) -> pretty::DocBuilder<'a, RcAllocator> {
    match self {
      Evidence::DataRow { left, right, goal } => left
        .pretty(ctx, a)
        .append(a.text("⊙").enclose(a.space(), a.space()))
        .append(right.pretty(ctx, a))
        .append(a.text("~data~").enclose(a.space(), a.space()))
        .append(goal.pretty(ctx, a))
        .parens(),
      Evidence::EffRow { left, right, goal } => left
        .pretty(ctx, a)
        .group()
        .append(a.text("⊙").enclose(a.softline(), a.space()))
        .append(right.pretty(ctx, a).group())
        .append(a.text("~eff~").enclose(a.softline(), a.space()))
        .append(goal.pretty(ctx, a).group())
        .parens(),
    }
  }
}

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for TyScheme<A>
where
  A: TypeAlloc + 'ty,
  Acc: AccessTy<'ty, A>,
  Db: ?Sized + crate::Db,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
  <A as TypeAlloc>::SimpleRowVar: for<'a> Pretty<'a, RcAllocator>,
  <A as TypeAlloc>::ScopedRowVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'a>(
    &self,
    ctx: &(&Db, Acc),
    a: &'a RcAllocator,
  ) -> pretty::DocBuilder<'a, RcAllocator> {
    let mut doc = a.nil();
    fn pretty_foralls<'a>(
      a: &'a RcAllocator,
      preamble: &'a str,
      var_docs: impl Iterator<Item = pretty::DocBuilder<'a, RcAllocator>>,
    ) -> pretty::DocBuilder<'a, RcAllocator> {
      a.text(preamble)
        .append(a.space())
        .append(a.intersperse(var_docs, a.space()))
        .append(a.text(".").enclose(a.space(), a.softline()))
    }
    let mut have_foralls = false;
    if !self.bound_ty.is_empty() {
      doc = doc.append(pretty_foralls(
        a,
        "forall<Ty>",
        self.bound_ty.iter().map(|var| var.clone().pretty(a)),
      ));
      have_foralls = true;
    }
    if !self.bound_data_row.is_empty() {
      doc = doc.append(pretty_foralls(
        a,
        "forall<Data>",
        self.bound_data_row.iter().map(|var| var.clone().pretty(a)),
      ));
      have_foralls = true;
    }
    if !self.bound_eff_row.is_empty() {
      doc = doc.append(pretty_foralls(
        a,
        "forall<Eff>",
        self.bound_eff_row.iter().map(|var| var.clone().pretty(a)),
      ));
      have_foralls = true;
    }

    if have_foralls {
      doc = doc.append(a.line()).nest(2);
    }

    doc = doc.append(a.concat(self.constrs.iter().map(|constr| {
      constr
        .pretty(ctx, a)
        .append(a.softline())
        .append("=>")
        .append(a.space())
        .group()
    })));

    doc
      .append(self.ty.pretty_with(ctx).pretty(a))
      .append(a.space())
      .append("|")
      .append(a.space())
      .append(self.eff.pretty_with(ctx).pretty(a))
  }
}
