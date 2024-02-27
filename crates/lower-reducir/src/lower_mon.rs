use std::convert::Infallible;

use base::{
  id::{Id, IdSupply, ReducIrTyVarId, ReducIrVarId},
  pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx},
};
use reducir::{
  ty::{
    default_fold_tykind, FoldReducIrTy, Kind, MkReducIrTy, ReducIrRow, ReducIrTy, ReducIrTyApp,
    ReducIrTyKind, ReducIrVarTy, Subst, UnwrapMonTy,
  },
  Bind, ReducIrFold,
};
use reducir::{
  DelimCont, DelimReducIr, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar,
  TypeCheck, P,
};

use ReducIrKind::*;
use ReducIrTyKind::*;

pub(crate) struct LowerMonCtx<'a> {
  db: &'a dyn crate::Db,
  var_conv: &'a mut IdSupply<ReducIrVarId>,
  current: ReducIrTermName,
  evv_var_id: ReducIrVarId,
}

impl MkReducIrTy for LowerMonCtx<'_> {
  fn mk_reducir_ty(&self, kind: ReducIrTyKind) -> ReducIrTy {
    self.db.mk_reducir_ty(kind)
  }

  fn mk_fun_ty(
    &self,
    args: impl IntoIterator<Item = impl reducir::ty::IntoReducIrTy>,
    ret: impl reducir::ty::IntoReducIrTy,
  ) -> ReducIrTy {
    self.db.mk_fun_ty(args, ret)
  }

  fn mk_yield_ty(
    &self,
    evv_ty: impl reducir::ty::IntoReducIrTy,
    a_ty: impl reducir::ty::IntoReducIrTy,
  ) -> ReducIrTy {
    self.db.mk_yield_ty(evv_ty, a_ty)
  }
}

impl<'a> LowerMonCtx<'a> {
  pub(crate) fn new(
    db: &'a dyn crate::Db,
    var_conv: &'a mut IdSupply<ReducIrVarId>,
    current: ReducIrTermName,
    evv_var_id: ReducIrVarId,
  ) -> Self {
    Self {
      db,
      var_conv,
      current,
      evv_var_id,
    }
  }
}

impl LowerMonCtx<'_> {
  pub(crate) fn lower_monadic_entry_point(
    &mut self,
    name: ReducIrTermName,
    ir: &DelimReducIr,
  ) -> ReducIr {
    let evv_ty = self.db.mk_prod_ty(vec![]);
    let reducir_db = self.db.as_reducir_db();
    let mon_ir = self.lower_monadic(evv_ty, ir);
    match mon_ir
      .type_check(reducir_db)
      .map_err_pretty_with(reducir_db)
      .expect("Monadic lowered IR to type check")
      .try_unwrap_monadic(reducir_db)
    {
      Ok(UnwrapMonTy { a_ty, evv_ty }) => {
        // If our value is a monad then apply our evv to it
        // We do this so the return value of our item is
        // `Ctl m a` and not `evv -> Ctl m a`
        // The latter would cause our item to have an overall type
        // like:
        // `evv -> evv -> Ctl m a` since our item already has evv as a
        // paramter.
        ReducIr::case(
          a_ty,
          ReducIr::app(mon_ir, [ReducIr::new(ReducIrKind::Struct(vec![]))]),
          [
            {
              let mut supply = IdSupply::default();
              let x = ReducIrVar::new(
                ReducIrLocal {
                  top_level: name,
                  id: supply.supply_id(),
                },
                a_ty,
              );
              ReducIr::abss([x], ReducIr::var(x))
            },
            {
              let mut supply = IdSupply::default();
              let x = ReducIrVar::new(
                ReducIrLocal {
                  top_level: name,
                  id: supply.supply_id(),
                },
                self.mk_yield_ty(evv_ty, a_ty),
              );
              ReducIr::abss([x], ReducIr::new(Int(5467)))
            },
          ],
        )
      }
      Err(_) => mon_ir,
    }
  }

  pub(crate) fn lower_monadic_top_level(&mut self, ir: &DelimReducIr) -> ReducIr {
    match ir.kind() {
      ReducIrKind::Abs(vars, body) => {
        match vars.iter().find(|var| var.var.id == self.evv_var_id) {
          Some(evv_var) => {
            let reducir_db = self.db.as_reducir_db();
            let body = self.lower_monadic(self.lower_monadic_ty(evv_var.ty), body);
            ReducIr::abss(
              vars
                .iter()
                .map(|var| var.map_type(|ty| self.lower_monadic_ty(ty))),
              {
                match body
                  .type_check(reducir_db)
                  .map_err_pretty_with(reducir_db)
                  .expect("Monadic lowered IR to type check")
                  .try_unwrap_monadic(reducir_db)
                {
                  Ok(_) => {
                    // If our value is a monad then apply our evv to it
                    // We do this so the return value of our item is
                    // `Ctl m a` and not `evv -> Ctl m a`
                    // The latter would cause our item to have an overall type
                    // like:
                    // `evv -> evv -> Ctl m a` since our item already has evv as a
                    // paramter.
                    ReducIr::app(body, [ReducIr::var(*evv_var)])
                  }
                  Err(ty) => {
                    // If our type isn't already a monad wrap it as a Pure value
                    ReducIr::new(ReducIrKind::Tag(
                      reducir_db.mk_reducir_ty(ControlTy(evv_var.ty, ty)),
                      0,
                      P::new(body),
                    ))
                  }
                }
              },
            )
          }
          None => {
            panic!("{}", ir.pretty_with(self.db).pprint().pretty(80));
          }
        }
      }
      ReducIrKind::TyAbs(ty_var, ir) => ReducIr::new(ReducIrKind::TyAbs(
        *ty_var,
        P::new(self.lower_monadic_top_level(ir)),
      )),
      ReducIrKind::TyApp(ir, ty_app) => ReducIr::new(ReducIrKind::TyApp(
        P::new(self.lower_monadic_top_level(ir)),
        ty_app.clone(),
      )),
      kind => panic!("expected top level abs during monadic lowering: {:?}", kind),
    }
  }

  fn generate_local(&mut self) -> ReducIrLocal {
    ReducIrLocal {
      top_level: self.current,
      id: self.var_conv.supply_id(),
    }
  }

  fn fresh_marker_item(&mut self) -> ReducIr {
    let ret_ty = self.mk_reducir_ty(VarTy(0));
    ReducIr::new(ReducIrKind::item(
      ReducIrTermName::gen(self.db, "__mon_freshm", self.current.module(self.db)),
      self.mk_forall_ty(
        [Kind::Type, Kind::Type],
        self.mk_fun_ty(
          [self.mk_fun_ty(
            [self.mk_reducir_ty(MarkerTy(self.mk_reducir_ty(VarTy(1))))],
            ret_ty,
          )],
          ret_ty,
        ),
      ),
    ))
  }

  /// Prompt handles "installing" our prompt into the stack and running an action under an
  /// updated effect row
  fn prompt_item(&mut self) -> ReducIr {
    /*
      prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼 p a
      prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
        Pure x -> Pure x
        Yield m' f k ->
            case m == m' of
                False -> Yield m' f (\x . prompt upd m h (k x))
                True -> f (\x . prompt upd m h (k x)) w
    */
    let m = self.mk_reducir_ty(VarTy(3));
    let upd_m = self.mk_reducir_ty(VarTy(2));
    let a = self.mk_reducir_ty(VarTy(1));
    let b = self.mk_reducir_ty(VarTy(0));

    let mark = self.mk_reducir_ty(MarkerTy(b));

    let prompt_type = self.mk_forall_ty(
      [Kind::Type, Kind::Type, Kind::Type, Kind::Type],
      self.mk_fun_ty(
        [
          mark,
          self.mk_fun_ty([m], upd_m),
          self.mk_fun_ty([a], self.mk_mon_ty(m, b)),
          self.mk_mon_ty(upd_m, a),
        ],
        self.mk_mon_ty(m, b),
      ),
    );

    ReducIr::new(ReducIrKind::item(
      ReducIrTermName::gen(self.db, "__mon_prompt", self.current.module(self.db)),
      prompt_type,
    ))
  }

  fn bind_item(&mut self) -> ReducIr {
    /*
    (e: Mon m a) |> (g : a -> Mon m b) : Mon m b =
       𝜆w. case e w of
           Pure x → g x w (monadic bind)
           Yield m f k → Yield m f (𝜆x. g x |> k)
    */
    let m = self.mk_reducir_ty(VarTy(2));
    let a = self.mk_reducir_ty(VarTy(1));
    let b = self.mk_reducir_ty(VarTy(0));
    let mon_m_b = self.mk_mon_ty(m, b);

    let bind_type = self.mk_forall_ty(
      [Kind::Type, Kind::Type, Kind::Type],
      self.mk_fun_ty(
        [self.mk_mon_ty(m, a), self.mk_fun_ty([a], mon_m_b)],
        mon_m_b,
      ),
    );
    ReducIr::new(ReducIrKind::item(
      ReducIrTermName::gen(self.db, "__mon_bind", self.current.module(self.db)),
      bind_type,
    ))
  }

  fn bind(
    &mut self,
    ir: ReducIr,
    derive_out_ty: impl FnOnce(ReducIrTy) -> ReducIrTy,
    body: impl FnOnce(&mut Self, ReducIrVar) -> ReducIr,
  ) -> ReducIr {
    let ir_db = self.db.as_reducir_db();
    let ty = ir.type_check(ir_db).map_err_pretty_with(ir_db).unwrap();
    let mon_ty = ty
      .try_unwrap_monadic(ir_db)
      .map_err_pretty_with(ir_db)
      .unwrap();
    let tmp = ReducIrVar::new(self.generate_local(), mon_ty.a_ty);
    let bind = self.bind_item();
    ReducIr::app(
      ReducIr::ty_app(
        bind,
        [
          ReducIrTyApp::Ty(mon_ty.evv_ty),
          ReducIrTyApp::Ty(mon_ty.a_ty),
          ReducIrTyApp::Ty(derive_out_ty(mon_ty.a_ty)),
        ],
      ),
      [ir, ReducIr::abss([tmp], body(self, tmp))],
    )
  }

  fn lower_monadic_ty(&self, ty: ReducIrTy) -> ReducIrTy {
    struct LowerMon<'db> {
      db: &'db dyn crate::Db,
    }
    impl<'db> FoldReducIrTy<'db> for LowerMon<'db> {
      fn db(&self) -> &'db dyn reducir::Db {
        self.db.as_reducir_db()
      }

      fn fold_ty_kind(&mut self, kind: ReducIrTyKind) -> ReducIrTy {
        match kind {
          FunETy(arg, eff, ret) => self.db.mk_fun_ty(
            [self.fold_ty(arg)],
            self.db.mk_mon_ty(self.fold_ty(eff), self.fold_ty(ret)),
          ),
          _ => default_fold_tykind(self, kind),
        }
      }
    }

    LowerMon { db: self.db }.fold_ty(ty)
  }

  fn lower_monadic_tyapp(&self, tyapp: &ReducIrTyApp) -> ReducIrTyApp {
    let lower_mon_row = |row: &ReducIrRow| match row {
      ReducIrRow::Open(i) => ReducIrRow::Open(*i),
      ReducIrRow::Closed(row) => {
        ReducIrRow::Closed(row.iter().map(|ty| self.lower_monadic_ty(*ty)).collect())
      }
    };
    match tyapp {
      ReducIrTyApp::Ty(ty) => ReducIrTyApp::Ty(self.lower_monadic_ty(*ty)),
      ReducIrTyApp::DataRow(row) => ReducIrTyApp::DataRow(lower_mon_row(row)),
      ReducIrTyApp::EffRow(row) => ReducIrTyApp::EffRow(lower_mon_row(row)),
    }
  }

  fn lower_monadic_ty_only(&self, delim: &DelimReducIr) -> ReducIr {
    struct LowerTys<'a, 'b> {
      this: &'a LowerMonCtx<'b>,
    }
    impl ReducIrFold for LowerTys<'_, '_> {
      type InExt = DelimCont;

      type OutExt = Infallible;

      fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
      }

      fn fold_ir(&mut self, kind: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        match kind {
          Var(v) => ReducIr::var(v.map_type(|ty| self.this.lower_monadic_ty(ty))),
          Abs(vars, body) => ReducIr::abss(
            vars
              .iter()
              .map(|v| v.map_type(|ty| self.this.lower_monadic_ty(ty))),
            body.into_inner(),
          ),
          Locals(binds, body) => ReducIr::locals(
            binds.into_iter().map(|bind| {
              Bind::new(
                bind.var.map_type(|ty| self.this.lower_monadic_ty(ty)),
                bind.defn,
              )
            }),
            body.into_inner(),
          ),
          TyApp(body, tyapps) => ReducIr::ty_app(
            body.into_inner(),
            tyapps
              .iter()
              .map(|tyapp| self.this.lower_monadic_tyapp(tyapp)),
          ),
          kind => ReducIr::new(kind),
        }
      }
    }
    delim.fold(&mut LowerTys { this: self })
  }

  fn lower_monadic(&mut self, evv_ty: ReducIrTy, ir: &ReducIr<DelimCont>) -> ReducIr {
    let reducir_db = self.db.as_reducir_db();
    let pure = |this: &mut Self, ir: ReducIr| {
      let ty = ir
        .type_check(reducir_db)
        .map_err_pretty_with(reducir_db)
        .expect("ICE: lower_monadic type check error");
      ReducIr::abss(
        [ReducIrVar::new(this.generate_local(), evv_ty)],
        ReducIr::new(ReducIrKind::Tag(
          reducir_db.mk_reducir_ty(ControlTy(evv_ty, ty)),
          0,
          P::new(ir),
        )),
      )
    };
    match ir.kind() {
      Int(i) => ReducIr::new(Int(*i)),
      Var(v) => ReducIr::var(v.map_type(|ty| self.lower_monadic_ty(ty))),
      Abs(vars, body) => {
        let evv_ty = vars
          .iter()
          .find(|v| v.var.id == self.evv_var_id)
          .map(|evv_var| self.lower_monadic_ty(evv_var.ty))
          .unwrap_or(evv_ty);
        let body = self.lower_monadic(evv_ty, body);
        ReducIr::abss(
          vars
            .iter()
            .map(|var| var.map_type(|ty| self.lower_monadic_ty(ty))),
          body,
        )
      }
      App(func, args) => {
        let ir_db = self.db.as_reducir_db();
        let func_mon = self.lower_monadic(evv_ty, func);
        let func_ty = func_mon
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();

        return args
          .iter()
          .fold((func_mon, func_ty), |(func, func_ty), arg| {
            let mon_arg = self.lower_monadic(evv_ty, arg);
            let mon_arg_ty = mon_arg.type_check(self.db).unwrap();

            let new_func = match (
              func_ty.try_unwrap_monadic(self.db),
              mon_arg_ty.try_unwrap_monadic(self.db),
            ) {
              (Err(_), Err(_)) => ReducIr::app(func, [mon_arg]),
              (Ok(UnwrapMonTy { a_ty, evv_ty }), Err(_)) => {
                let bind = self.bind_item();
                let var = ReducIrVar::new(self.generate_local(), a_ty);
                let body = ReducIr::app(ReducIr::var(var), [mon_arg]);
                let body_ty = body.type_check(ir_db).map_err_pretty_with(ir_db).unwrap();
                let body_ty = match body_ty.try_unwrap_monadic(ir_db) {
                  Ok(UnwrapMonTy { a_ty, .. }) => a_ty,
                  Err(ty) => ty,
                };
                ReducIr::app(
                  ReducIr::ty_app(
                    bind,
                    [
                      ReducIrTyApp::Ty(evv_ty),
                      ReducIrTyApp::Ty(a_ty),
                      ReducIrTyApp::Ty(body_ty),
                    ],
                  ),
                  [func, ReducIr::abss([var], body)],
                )
              }
              (Err(_), Ok(_)) => {
                let ret_ty = func_ty.drop_args(ir_db, 1).unwrap();
                let mut needs_pure = false;
                let ret_ty = match ret_ty.try_unwrap_monadic(ir_db) {
                  Ok(UnwrapMonTy { a_ty, .. }) => a_ty,
                  Err(ty) => {
                    needs_pure = true;
                    ty
                  }
                };
                self.bind(
                  mon_arg,
                  |_| ret_ty,
                  |this, arg_var| {
                    let app = ReducIr::app(func, [ReducIr::var(arg_var)]);
                    if needs_pure {
                      pure(this, app)
                    } else {
                      app
                    }
                  },
                )
              }
              (Ok(UnwrapMonTy { a_ty, .. }), Ok(_)) => {
                let ret_ty = a_ty.drop_args(ir_db, 1).unwrap();
                let ret_ty = match ret_ty.try_unwrap_monadic(ir_db) {
                  Ok(UnwrapMonTy { a_ty, .. }) => a_ty,
                  Err(ty) => ty,
                };
                self.bind(
                  func,
                  |_| ret_ty,
                  |this, func_var| {
                    this.bind(
                      mon_arg,
                      |_| ret_ty,
                      |_, arg_var| ReducIr::app(ReducIr::var(func_var), [ReducIr::var(arg_var)]),
                    )
                  },
                )
              }
            };
            let new_func_ty = new_func
              .type_check(self.db)
              .map_err_pretty_with(self.db)
              .unwrap();
            (new_func, new_func_ty)
          })
          .0;
      }
      Locals(binds, body) => {
        let mut needs_mon_bind = vec![];
        let binds = binds
          .iter()
          .filter_map(|bind| {
            let defn = self.lower_monadic(evv_ty, &bind.defn);
            let defn_ty = defn
              .type_check(self.db)
              .map_err_pretty_with(self.db)
              .expect("Monadic lowered defn should type check");
            match defn_ty.try_unwrap_monadic(self.db) {
              Ok(UnwrapMonTy { .. }) => {
                needs_mon_bind.push(defn);
                None
              }
              // No monadic type we can just bind this normally
              Err(_) => Some(Bind::new(
                bind.var.map_type(|ty| self.lower_monadic_ty(ty)),
                defn,
              )),
            }
          })
          .collect::<Vec<_>>();

        let body = self.lower_monadic(evv_ty, body);
        let body_ty = body
          .type_check(self.db)
          .map_err_pretty_with(self.db)
          .unwrap();
        // Handle whether or not body is monadic
        let (mut body, body_ty) = match body_ty.try_unwrap_monadic(self.db) {
          Ok(UnwrapMonTy { a_ty, .. }) => (body, a_ty),
          _ => (pure(self, body), body_ty),
        };
        if !needs_mon_bind.is_empty() {
          body = needs_mon_bind.into_iter().fold(body, |body, mon_defn| {
            self.bind(mon_defn, |_| body_ty, |_, _| body)
          });
        }
        ReducIr::locals(binds, body)
      }
      TyAbs(tyvar, ir) => ReducIr::ty_abs([*tyvar], self.lower_monadic(evv_ty, ir)),
      TyApp(ir, ty_apps) => ReducIr::ty_app(
        self.lower_monadic(evv_ty, ir),
        ty_apps
          .iter()
          .map(|ty_app| self.lower_monadic_tyapp(ty_app)),
      ),
      Struct(elems) => {
        let mut binds = vec![];
        let mut is_mon = false;
        let vars = elems
          .iter()
          .map(|elem| match elem.kind() {
            Var(v) => {
              if v.ty.try_unwrap_monadic(reducir_db).is_ok() {
                is_mon = true;
              }
              *v
            }
            _ => {
              let v = ReducIrVar::new(
                self.generate_local(),
                elem
                  .type_check(reducir_db)
                  .map_err_pretty_with(reducir_db)
                  .unwrap(),
              );
              binds.push((v, elem));
              v
            }
          })
          .collect::<Vec<_>>();
        // If all our elements are variables we just return the pure Struct
        if binds.is_empty() {
          if is_mon {
            return pure(
              self,
              ReducIr::new(Struct(vars.into_iter().map(ReducIr::var).collect())),
            );
          } else {
            return ReducIr::new(Struct(vars.into_iter().map(ReducIr::var).collect()));
          }
        }
        let anf = binds.into_iter().fold(
          ReducIr::new(Struct(vars.into_iter().map(ReducIr::var).collect())),
          |body, (var, defn)| ReducIr::local(var, defn.clone(), body),
        );
        self.lower_monadic(evv_ty, &anf)
      }
      FieldProj(indx, strukt) => {
        let strukt = self.lower_monadic(evv_ty, strukt);
        let strukt_ty = strukt
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();
        match strukt_ty.try_unwrap_monadic(reducir_db) {
          Ok(_) => self.bind(
            strukt,
            |ty| match ty.kind(reducir_db) {
              ProductTy(ref elems) => elems[*indx],
              _ => unreachable!(),
            },
            |this, s| pure(this, ReducIr::field_proj(*indx, ReducIr::var(s))),
          ),
          // No bind required
          Err(_) => ReducIr::field_proj(*indx, strukt),
        }
      }
      Tag(ty, tag, ir) => {
        let ir = self.lower_monadic(evv_ty, ir);
        let mon_ty = self.lower_monadic_ty(*ty);
        self.bind(
          ir,
          |_| mon_ty,
          |this, t| pure(this, ReducIr::tag(mon_ty, *tag, ReducIr::var(t))),
        )
      }
      Case(ty, disc, branches) => {
        let disc = self.lower_monadic(evv_ty, disc);
        let ty = self.lower_monadic_ty(*ty);
        self.bind(
          disc,
          |_| ty,
          |ctx, d| {
            ReducIr::case_on_var(ty, d, branches.iter().map(|b| ctx.lower_monadic(evv_ty, b)))
          },
        )
      }
      // TODO: do we need to handle this specially? item should be lowered monadically so it
      // already returns a monad when we call it here
      Item(occ) => match occ.name {
        ReducIrTermName::Term(term) => {
          let mi = self.db.lower_reducir_mon_item_of(term);
          let mon_item = mi.item(self.db.as_reducir_db());
          let mon_ty = mon_item
            .type_check(self.db.as_reducir_db())
            .expect("Type checking must succeed for item to exist");
          ReducIr::new(ReducIrKind::Item(occ.map_ty(|_| mon_ty)))
        }
        // Generated items don't have monadic type (they are row evidence).
        // So they can reuse their type unmodified.
        ReducIrTermName::Gen(_) => ReducIr::new(ReducIrKind::Item(*occ)),
      },
      Coerce(_, _) => {
        unreachable!("Coerce should only be used for primitives")
      }
      X(DelimCont::NewPrompt(mark_var, ir)) => {
        let x = self.lower_monadic(evv_ty, ir);
        let a_ty = match mark_var.ty.kind(reducir_db) {
          MarkerTy(a_ty) => a_ty,
          _ => unreachable!(),
        };
        let x_ty = x
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();
        ReducIr::app(
          ReducIr::ty_app(
            self.fresh_marker_item(),
            [ReducIrTyApp::Ty(a_ty), ReducIrTyApp::Ty(x_ty)],
          ),
          [ReducIr::abss([*mark_var], x)],
        )
      }
      X(DelimCont::Prompt {
        marker,
        upd_evv,
        ret,
        body,
      }) => {
        let update_evv_fn_ty = upd_evv
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();
        // Invariant that this is a function from evv to upd_evv type.
        let (_, upd_evv_ty) = match update_evv_fn_ty.kind(reducir_db) {
          FunTy(args, ret) => (args[0], ret),
          _ => unreachable!(),
        };
        let upd_evv_ty = self.lower_monadic_ty(upd_evv_ty);

        // Reuse the var id we got from lowering so we can use
        let (w, mon_body) = //self.lower_monadic(upd_evv_ty, body);
            // This invariant is maintained by lowering so we can trust body will have this shape
            match body.kind() {
                ReducIrKind::Abs(vars, body) => {
                    (vars[0].map_type(|ty| self.lower_monadic_ty(ty)), self.lower_monadic(upd_evv_ty, body))
                },
                _ => unreachable!(),
            };
        let mon_marker = self.lower_monadic(evv_ty, marker);
        let mon_body_ty = mon_body
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();
        let mon_ret = self.lower_monadic(evv_ty, ret);
        let ret_ty = match ret
          .type_check(self.db)
          .unwrap()
          .kind(self.db.as_reducir_db())
        {
          FunETy(_, _, ret) => self.lower_monadic_ty(ret),
          _ => unreachable!("return clause of handler must be a function"),
        };
        let UnwrapMonTy {
          evv_ty: _,
          a_ty: body_ty,
        } = match mon_body_ty.try_unwrap_monadic(reducir_db) {
          Ok(upd_mon) => upd_mon,
          Err(_) => unreachable!(),
        };
        ReducIr::app(
          ReducIr::ty_app(
            self.prompt_item(),
            [
              ReducIrTyApp::Ty(evv_ty),
              ReducIrTyApp::Ty(upd_evv_ty),
              ReducIrTyApp::Ty(body_ty),
              ReducIrTyApp::Ty(ret_ty),
            ],
          ),
          [mon_marker, self.lower_monadic_ty_only(upd_evv), mon_ret, {
            //let w = ReducIrVar::new(self.generate_local(), upd_evv_ty);
            ReducIr::abss([w], ReducIr::app(mon_body, [ReducIr::var(w)]))
          }],
        )
      }
      X(DelimCont::Yield {
        ret_ty,
        marker,
        body: f,
      }) => {
        let w = ReducIrVar::new(self.generate_local(), evv_ty);
        let ret_ty = self.lower_monadic_ty(*ret_ty);
        let x = ReducIrVar::new(self.generate_local(), ret_ty);
        let [a, b, c] = [
          ReducIrVarTy {
            var: ReducIrTyVarId::from_raw(0),
            kind: Kind::Type,
          },
          ReducIrVarTy {
            var: ReducIrTyVarId::from_raw(1),
            kind: Kind::Type,
          },
          ReducIrVarTy {
            var: ReducIrTyVarId::from_raw(2),
            kind: Kind::Type,
          },
        ];
        ReducIr::abss(
          [w],
          ReducIr::tag(
            self.mk_reducir_ty(ControlTy(evv_ty, ret_ty)),
            1,
            // Wrap this struct in type variables.
            // These are used to instantiate the existentials during optimization.
            ReducIr::ty_abs(
              [a, b, c],
              ReducIr::new(Struct(vec![
                self.lower_monadic(evv_ty, marker),
                self.lower_monadic(evv_ty, f),
                ReducIr::abss([x], pure(self, ReducIr::var(x))),
              ]))
              .subst(self.db, Subst::Inc(3)),
            ),
          ),
        )
      }
      X(DelimCont::AbsE(arg, evv_ty, body)) => {
        let evv_ty = self.lower_monadic_ty(*evv_ty);
        let mon_body = self.lower_monadic(evv_ty, body);
        let mon_body_ty = mon_body
          .type_check(self.db)
          .map_err_pretty_with(self.db)
          .unwrap();
        ReducIr::abss(
          [arg.map_type(|ty| self.lower_monadic_ty(ty))],
          match mon_body_ty.try_unwrap_monadic(self.db) {
            Ok(_) => mon_body,
            Err(_) => pure(self, mon_body),
          },
        )
      }
    }
  }
}
