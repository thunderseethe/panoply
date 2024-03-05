use std::ops::Deref;

use base::pretty::PrettyWithCtx;
use pretty::{docs, DocAllocator, DocBuilder, Pretty, RcAllocator};

use crate::ty::{Kind, ReducIrVarTy};
use crate::{
  Bind, DelimCont, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrTyErr, ReducIrVar,
  TypeCheck,
};

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for DelimCont {
  fn pretty<'a>(&self, db: &DB, arena: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    match self {
      DelimCont::NewPrompt(p_var, body) => docs![
        arena,
        "new_prompt",
        arena.space(),
        p_var.pretty(arena).brackets(),
        arena.space(),
        body.deref().kind.pretty(db, arena)
      ]
      .parens(),
      DelimCont::Prompt {
        marker,
        upd_evv,
        ret,
        body,
      } => arena
        .as_string("prompt")
        .append(
          arena
            .softline()
            .append(marker.deref().pretty(db, arena))
            .nest(2),
        )
        .append(arena.softline().append(upd_evv.pretty(db, arena)).nest(2))
        .append(arena.softline().append(ret.pretty(db, arena).nest(2)))
        .append(
          arena
            .softline()
            .append(body.deref().pretty(db, arena))
            .nest(2),
        )
        .parens(),
      DelimCont::Yield {
        marker,
        body,
        ret_ty,
      } => arena
        .as_string("yield")
        .append(ret_ty.pretty(db, arena).angles())
        .append(
          arena
            .softline()
            .append(marker.deref().kind.pretty(db, arena))
            .nest(2),
        )
        .append(
          arena
            .softline()
            .append(body.deref().kind.pretty(db, arena))
            .nest(2),
        )
        .parens(),
      DelimCont::AbsE(arg, eff, body) => docs![
        arena,
        "fun",
        eff.pretty(db, arena).angles(),
        arena.space(),
        arg.pretty(arena).brackets(),
        arena.line().append(body.pretty(db, arena)).nest(2).group(),
      ]
      .parens(),
    }
  }
}

impl<DB: ?Sized + crate::Db, Ext: PrettyWithCtx<DB> + TypeCheck<Ext = Ext> + Clone>
  PrettyWithCtx<DB> for ReducIr<Ext>
{
  fn pretty<'a>(&self, db: &DB, allocator: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    self.kind.pretty(db, allocator)
  }
}

impl<DB: ?Sized + crate::Db, Ext: PrettyWithCtx<DB> + TypeCheck<Ext = Ext> + Clone>
  PrettyWithCtx<DB> for ReducIrKind<Ext>
{
  fn pretty<'a>(&self, db: &DB, arena: &'a RcAllocator) -> pretty::DocBuilder<'a, RcAllocator> {
    use ReducIrKind::*;
    fn gather_ty_abs<'a, Ext>(
      vars: &mut Vec<ReducIrVarTy>,
      kind: &'a ReducIrKind<Ext>,
    ) -> &'a ReducIrKind<Ext> {
      match kind {
        TyAbs(arg, body) => {
          vars.push(*arg);
          gather_ty_abs(vars, &body.deref().kind)
        }
        _ => kind,
      }
    }
    match self {
      Int(i) => i.to_string().pretty(arena),
      Var(v) => v.pretty(arena),
      //Var(v) => v.pretty_with_type(db, arena),
      //Var(v) => v.pretty_with_local(db, arena),
      Abs(vars, body) => {
        let param_single = arena.space().append(
          arena
            .intersperse(
              vars.iter().map(|v| v.pretty(arena)),
              arena.text(",").append(arena.space()),
            )
            .brackets(),
        );
        let param_multi = arena
          .hardline()
          .append(
            arena
              .intersperse(
                vars.iter().map(|v| v.pretty(arena)),
                arena.hardline().append(","),
              )
              .brackets(),
          )
          .nest(2);

        docs![
          arena,
          "fun",
          param_multi.flat_alt(param_single).group(),
          arena.line().append(body.pretty(db, arena)).nest(2).group(),
        ]
        .parens()
      }
      App(func, args) => {
        match &func.deref().kind {
          // If we see App(Abs(_, _), _) print this as a let binding
          Abs(vars, body) => {
            let pretty_binds = |binds: Vec<(&ReducIrVar, &ReducIr<Ext>)>, body: &ReducIr<Ext>| {
              let binds_len = binds.len();
              let mut binds_iter = binds.into_iter().map(|(var, defn)| {
                var
                  .pretty(arena)
                  .append(arena.space())
                  .append(defn.pretty(db, arena))
                  .parens()
              });
              let binds = if binds_len == 1 {
                binds_iter.next().unwrap()
              } else {
                arena
                  .space()
                  .append(
                    arena.intersperse(binds_iter, arena.line().append(",").append(arena.space())),
                  )
                  .append(arena.line())
                  .brackets()
              };

              docs![
                arena,
                "let",
                arena.line().append(binds).nest(2).group(),
                arena.line().append(body.pretty(db, arena)).nest(2).group()
              ]
              .parens()
            };

            let mut binds = vec![];
            let mut args_iter = args.iter().peekable();
            let mut vars_iter = vars.iter().peekable();
            loop {
              match (args_iter.peek(), vars_iter.peek()) {
                (Some(_), Some(_)) => {
                  binds.push((vars_iter.next().unwrap(), args_iter.next().unwrap()));
                }
                (Some(_), None) => {
                  let func = pretty_binds(binds, body);
                  break func
                    .append(
                      arena
                        .line()
                        .append(
                          arena
                            .intersperse(args_iter.map(|arg| arg.pretty(db, arena)), arena.line()),
                        )
                        .nest(2)
                        .group(),
                    )
                    .parens();
                }
                (None, Some(_)) => {
                  break pretty_binds(
                    binds,
                    &ReducIr::abss(vars_iter.copied(), body.deref().clone()),
                  )
                }
                (None, None) => break pretty_binds(binds, body),
              }
            }
          }
          // Print application as normal
          func => func
            .pretty(db, arena)
            .append(
              arena
                .line()
                .append(
                  arena
                    .intersperse(args.iter().map(|arg| arg.pretty(db, arena)), arena.line())
                    .align(),
                )
                .nest(2)
                .group(),
            )
            .parens(),
        }
      }
      Locals(binds, body) => {
        let binds_len = binds.len();
        let mut binds_iter = binds.iter().map(|Bind { var, defn }| {
          var
            .pretty(arena)
            .append(arena.space())
            .append(defn.pretty(db, arena))
            .parens()
        });
        let binds = if binds_len == 1 {
          binds_iter.next().unwrap()
        } else {
          arena
            .space()
            .append(arena.intersperse(binds_iter, arena.line().append(",").append(arena.space())))
            .append(arena.line())
            .brackets()
        };

        docs![
          arena,
          "let",
          arena.line().append(binds).nest(2).group(),
          arena.line().append(body.pretty(db, arena)).nest(2).group()
        ]
        .parens()
      }
      TyAbs(tyvar, body) => {
        let mut tyvars = vec![*tyvar];
        let body = gather_ty_abs(&mut tyvars, &body.deref().kind);
        let params = arena
          .softline()
          .append(
            arena
              .intersperse(tyvars.iter().map(|tv| tv.pretty(arena)), arena.space())
              .align()
              .brackets(),
          )
          .nest(2);
        let body_doc = arena.softline().append(body.pretty(db, arena)).nest(2);
        docs![arena, "forall", params.group(), body_doc.group()].parens()
      }
      Struct(elems) => arena
        .intersperse(
          elems.iter().map(|elem| elem.pretty(db, arena)),
          arena.text(",").append(arena.space()),
        )
        .enclose(arena.softline_(), arena.softline_())
        .braces()
        .group(),
      FieldProj(idx, term) => term
        .deref()
        .pretty(db, arena)
        .append(arena.as_string(idx).brackets()),
      Tag(_, tag, term) => docs![
        arena,
        arena.as_string(tag),
        arena.text(":"),
        arena.space(),
        term.pretty(db, arena).nest(2)
      ]
      .angles(),
      Case(_, discr, branches) => docs![
        arena,
        "case",
        arena.space(),
        discr.pretty(db, arena).nest(2).group(),
        arena
          .line()
          .append(arena.intersperse(branches.iter().map(|b| b.pretty(db, arena)), arena.line()))
          .nest(2)
      ]
      .parens(),
      TyApp(body, tys) => body
        .pretty(db, arena)
        .append(arena.space())
        .append(arena.as_string("@"))
        .append(arena.space())
        .append(
          arena
            .intersperse(
              tys.iter().map(|ty| ty.pretty(db, arena)),
              arena.text(",").append(arena.space()),
            )
            .brackets(),
        )
        .parens(),
      Coerce(ty, body) => docs![
        arena,
        "coerce",
        arena.softline(),
        body.pretty(db, arena).parens(),
        arena.softline(),
        "as",
        arena.space(),
        ty.pretty(db, arena).parens()
      ]
      .parens(),
      X(xt) => xt.pretty(db, arena),
      Item(occ) => arena.text(occ.name.name(db).text(db.as_core_db()).clone()),
    }
  }
}
impl<'ir, DB: ?Sized + crate::Db, Ext: PrettyWithCtx<DB> + TypeCheck<Ext = Ext> + Clone>
  PrettyWithCtx<DB> for ReducIrTyErr<'ir, Ext>
{
  fn pretty<'a>(&self, db: &DB, a: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    match self {
      ReducIrTyErr::TyMismatch {
        left_ty,
        left_ir,
        right_ty,
        right_ir,
      } => a
        .text("Type Mismatch:")
        .append(a.line())
        .append(
          left_ir
            .pretty(db, a)
            .parens()
            .append(a.text(":"))
            .append(a.line())
            .append(left_ty.pretty(db, a).parens().group()),
        )
        .append(a.line())
        .append(
          right_ir
            .pretty(db, a)
            .parens()
            .append(a.text(":"))
            .append(a.softline())
            .append(right_ty.pretty(db, a)),
        ),
      ReducIrTyErr::ArgMismatch {
        fun_ir,
        arg_ir,
        expected_ty,
        actual_ty,
      } => a
        .text("Function argument:")
        .append(
          a.softline()
            .append(
              arg_ir
                .pretty(db, a)
                .append(a.text(":"))
                .append(a.line())
                .append(actual_ty.pretty(db, a).parens().group()),
            )
            .nest(2),
        )
        .append(
          a.line()
            .append(a.text("expected to have type:"))
            .append(a.softline().append(expected_ty.pretty(db, a)).nest(2)),
        )
        .append(
          a.line().append(
            a.text("from function:")
              .append(a.softline().append(fun_ir.pretty(db, a)).nest(2)),
          ),
        )
        .append(a.line())
        .append({
          let mut actual = String::new();
          actual_ty.pretty(db, a).render_fmt(80, &mut actual).unwrap();
          let mut expected = String::new();
          expected_ty
            .pretty(db, a)
            .render_fmt(80, &mut expected)
            .unwrap();
          a.text(difference::Changeset::new(&actual, &expected, "").to_string())
        }),
      ReducIrTyErr::KindMistmatch(lhs, rhs) => a.text("KindMistmatch").append(
        lhs
          .pretty(a)
          .append(a.text(","))
          .append(a.softline())
          .append(rhs.pretty(a))
          .parens(),
      ),
      ReducIrTyErr::ExpectedFunTy { ty, func } => a
        .text("Expected a function type, but got:")
        .append(a.softline())
        .append(func.pretty(db, a))
        .append(a.text(":"))
        .append(a.softline())
        .append(ty.pretty(db, a)),
      ReducIrTyErr::ExpectedForallTy {
        forall, forall_ty, ..
      } => a
        .text("Expected a forall type, but got:")
        .append(a.softline())
        .append(forall.pretty(db, a))
        .append(":")
        .append(a.softline())
        .append(forall_ty.pretty(db, a)),
      ReducIrTyErr::ExpectedProdTy(ty, strukt) => a
        .text("Expected a prod type, but got:")
        .append(a.softline())
        .append(strukt.pretty(db, a))
        .append(":")
        .append(a.softline())
        .append(ty.pretty(db, a)),
      ReducIrTyErr::ExpectedCoprodTy(ty, discr) => a
        .text("Expected a coprod type, but got:")
        .append(a.softline())
        .append(ty.pretty(db, a))
        .append(a.text(" for discriminant "))
        .append(discr.pretty(db, a)),
      ReducIrTyErr::ExpectedMarkerTy(ty) => a
        .text("Expected a marker type, but got:")
        .append(a.softline())
        .append(ty.pretty(db, a)),
    }
  }
}
impl<'a, A: 'a, D: ?Sized + DocAllocator<'a, A>> Pretty<'a, D, A> for &ReducIrVarTy {
  fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
    allocator
      .as_string(self.var.0)
      .append(allocator.text(":"))
      .append(allocator.space())
      .append(allocator.text(match self.kind {
        Kind::Type => "Type",
        Kind::SimpleRow => "SimpleRow",
        Kind::ScopedRow => "ScopedRow",
      }))
      .parens()
  }
}

impl<'a, A: 'a, D> ::pretty::Pretty<'a, D, A> for ReducIrLocal
where
  D: ::pretty::DocAllocator<'a, A>,
{
  fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D, A> {
    self.id.pretty(allocator)
  }
}

impl<'a, A: 'a, D: ?Sized + DocAllocator<'a, A>> Pretty<'a, D, A> for &ReducIrVar {
  fn pretty(self, arena: &'a D) -> DocBuilder<'a, D, A> {
    arena.text("V").append(arena.as_string(self.var.id.0))
  }
}
impl ReducIrVar {
  pub fn pretty_with_local<'a, DB: ?Sized + crate::Db>(
    &self,
    db: &DB,
    arena: &'a RcAllocator,
  ) -> DocBuilder<'a, RcAllocator> {
    arena
      .text("V")
      .append(arena.as_string(self.var.id.0))
      .append(arena.text(":"))
      .append(&self.var.top_level.pretty_with(db))
  }

  #[allow(dead_code)]
  pub fn pretty_with_type<'a, DB: ?Sized + crate::Db>(
    &self,
    db: &DB,
    arena: &'a RcAllocator,
  ) -> DocBuilder<'a, RcAllocator> {
    arena
      .text("V")
      .append(arena.as_string(self.var.id.0))
      .append(arena.text(":"))
      .append(arena.softline())
      .append(self.ty.pretty_with(db).pretty(arena))
      .parens()
  }
}

impl<DB: ?Sized + crate::Db> PrettyWithCtx<DB> for ReducIrTermName {
  fn pretty<'a>(&self, ctx: &DB, alloc: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
    alloc.as_string(self.name(ctx).text(ctx.as_core_db()))
  }
}
