use base::pretty::PrettyWithCtx;
use pretty::{docs, DocAllocator, Pretty, RcAllocator};

use crate::{AccessTy, Ty, TypeAlloc, TypeKind};

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
      TypeKind::FunTy(arg, ret) => arg
        .pretty_with(ctx)
        .pretty(a)
        .append(docs![a, a.softline(), "->", a.softline(), &ret.pretty_with(ctx)].nest(2)),
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
