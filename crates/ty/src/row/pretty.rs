use base::pretty::PrettyWithCtx;
use pretty::{docs, DocAllocator, DocBuilder, Pretty, RcAllocator};
use std::fmt::Debug;

use crate::{AccessTy, TypeAlloc};

use super::closed_row::ClosedRow;
use super::{Row, RowSema, ScopedClosedRow, SimpleClosedRow};

impl<A: TypeAlloc, Sema: RowSema, Ctx> PrettyWithCtx<Ctx> for Row<Sema, A>
where
  Sema::Open<A>: Debug,
  Sema::Closed<A>: PrettyWithCtx<Ctx>,
{
  fn pretty<'a>(&self, ctx: &Ctx, alloc: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    match self {
      Row::Open(tv) => alloc.text(format!("{:?}", tv)),
      Row::Closed(row) => row.pretty_with(ctx).pretty(alloc),
    }
  }
}

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for ClosedRow<A>
where
  A: TypeAlloc + 'ty,
  Db: ?Sized + crate::Db,
  Acc: AccessTy<'ty, A>,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'a>(
    &self,
    ctx @ (db, acc): &(&Db, Acc),
    a: &'a pretty::RcAllocator,
  ) -> DocBuilder<'a, RcAllocator> {
    if acc.row_fields(&self.fields).is_empty() {
      return a.as_string("∅");
    }
    let docs = acc
      .row_fields(&self.fields)
      .iter()
      .zip(acc.row_values(&self.values).iter())
      .map(|(field, value)| {
        docs![
          a,
          &field.pretty_with(*db),
          a.space(),
          "|>",
          a.softline(),
          &value.pretty_with(ctx)
        ]
        .group()
      });
    a.intersperse(
      docs,
      a.concat([a.softline_(), a.as_string(","), a.space()])
        .into_doc(),
    )
  }
}

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for SimpleClosedRow<A>
where
  A: TypeAlloc + 'ty,
  Db: ?Sized + crate::Db,
  Acc: AccessTy<'ty, A>,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'a>(
    &self,
    ctx: &(&Db, Acc),
    alloc: &'a ::pretty::RcAllocator,
  ) -> ::pretty::DocBuilder<'a, ::pretty::RcAllocator> {
    self.0.pretty_with(ctx).pretty(alloc)
  }
}

impl<'ty, A, Db, Acc> PrettyWithCtx<(&Db, Acc)> for ScopedClosedRow<A>
where
  A: TypeAlloc + 'ty,
  Db: ?Sized + crate::Db,
  Acc: AccessTy<'ty, A>,
  <A as TypeAlloc>::TypeVar: for<'a> Pretty<'a, RcAllocator>,
{
  fn pretty<'a>(&self, ctx: &(&Db, Acc), alloc: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    self.0.pretty_with(ctx).pretty(alloc)
  }
}
