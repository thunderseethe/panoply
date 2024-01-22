use base::{
  id::{Id, IdSupply, ReducIrTyVarId, ReducIrVarId},
  pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx},
};
use reducir::{
  ty::{
    Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrVarTy, Subst, UnwrapMonTy,
  },
  Bind,
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
      Ok(UnwrapMonTy { a_ty, .. }) => {
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
                a_ty,
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
            let body = self.lower_monadic(evv_var.ty, body);
            ReducIr::abss(vars.iter().copied(), {
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
            })
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
    ReducIr::new(Item(
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
      prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
      prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
        Pure x -> Pure x
        Yield m' f k ->
            case m == m' of
                False -> Yield m' f (\x . prompt upd m h (k x))
                True -> f (\x . prompt upd m h (k x)) w
    */
    let m = self.mk_reducir_ty(VarTy(2));
    let upd_m = self.mk_reducir_ty(VarTy(1));
    let a = self.mk_reducir_ty(VarTy(0));

    let mark = self.mk_reducir_ty(MarkerTy(a));

    let prompt_type = self.mk_forall_ty(
      [Kind::Type, Kind::Type, Kind::Type],
      self.mk_fun_ty(
        [
          mark,
          self.mk_fun_ty([m], upd_m),
          self.mk_fun_ty([upd_m], self.mk_reducir_ty(ControlTy(upd_m, a))),
        ],
        self.mk_fun_ty([m], self.mk_reducir_ty(ControlTy(m, a))),
      ),
    );

    ReducIr::new(Item(
      ReducIrTermName::gen(self.db, "__mon_prompt", self.current.module(self.db)),
      prompt_type,
    ))
  }
  /// TODO: Return an item representing the bind implementation of our delimited continuation
  /// monad
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
    ReducIr::new(Item(
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

  fn lower_monadic(&mut self, evv_ty: ReducIrTy, ir: &ReducIr<DelimCont>) -> ReducIr {
    let reducir_db = self.db.as_reducir_db();
    let evv_var_id = ReducIrLocal {
      top_level: self.current,
      id: self.evv_var_id,
    };
    let pure = |ir: ReducIr| {
      let ty = ir
        .type_check(reducir_db)
        .expect("ICE: lower_monadic type check error");
      ReducIr::abss(
        [ReducIrVar::new(evv_var_id, evv_ty)],
        ReducIr::new(ReducIrKind::Tag(
          reducir_db.mk_reducir_ty(ControlTy(evv_ty, ty)),
          0,
          P::new(ir),
        )),
      )
    };
    match ir.kind() {
      Int(i) => ReducIr::new(Int(*i)),
      Var(v) => ReducIr::var(*v),
      Abs(vars, body) => {
        let evv_ty = vars
          .iter()
          .find(|v| v.var.id == self.evv_var_id)
          .map(|evv_var| evv_var.ty)
          .unwrap_or(evv_ty);
        ReducIr::abss(vars.iter().copied(), self.lower_monadic(evv_ty, body))
      }
      App(func, args) => {
        let bind = self.bind_item();
        let func_mon = self.lower_monadic(evv_ty, func);
        let func_ty = func_mon
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();

        let mut bind_args = vec![];
        let mon_args = args
          .iter()
          .map(|arg| {
            let mon_arg = self.lower_monadic(evv_ty, arg);
            let mon_arg_ty = match mon_arg
              .type_check(reducir_db)
              .map_err_pretty_with(reducir_db)
            {
              Ok(ty) => ty,
              Err(err) => {
                panic!("{:?}", err);
              }
            };
            match mon_arg_ty.try_unwrap_monadic(reducir_db) {
              Ok(_) => {
                let arg_ty = arg
                  .type_check(reducir_db)
                  .map_err_pretty_with(reducir_db)
                  .unwrap();
                let arg_var = ReducIrVar::new(self.generate_local(), arg_ty);
                bind_args.push((mon_arg, arg_var));
                ReducIr::var(arg_var)
              }
              // We don't need to do anything for a non monadic arg
              Err(_) => mon_arg,
            }
          })
          .collect::<Vec<_>>();

        let (body, ret_ty) = match func_ty.try_fun_returns_monadic(reducir_db) {
          // Our function might take some number of argument and then return another
          // function wrapped in our monad:
          //    a -> b -> {evv} -> Ctl {evv} (c -> d -> e)
          // To handle this case we split our args based on how many args appear before
          // the monadic type. This application is then passed to bind to produce the
          // underlying function which is applied to any remaining args and then wrapped
          // up in our monad again.
          Ok((arg_count, mon)) => {
            let mut mon_args = mon_args;
            let post_mon_args = mon_args.split_off(arg_count);
            let pre_mon_args = mon_args;

            let f = ReducIrVar::new(self.generate_local(), mon.a_ty);

            if post_mon_args.is_empty() {
              (ReducIr::app(func_mon, pre_mon_args), mon.a_ty)
            } else {
              let applied_fun_ty = mon.a_ty.drop_args(reducir_db, post_mon_args.len()).unwrap();
              let body = ReducIr::app(
                ReducIr::ty_app(
                  bind.clone(),
                  [
                    ReducIrTyApp::Ty(evv_ty),
                    ReducIrTyApp::Ty(mon.a_ty),
                    ReducIrTyApp::Ty(applied_fun_ty),
                  ],
                ),
                [
                  ReducIr::app(func_mon, pre_mon_args),
                  ReducIr::abss([f], {
                    let y = ReducIrVar::new(self.generate_local(), applied_fun_ty);
                    ReducIr::local(
                      y,
                      ReducIr::app(ReducIr::var(f), post_mon_args),
                      pure(ReducIr::var(y)),
                    )
                  }),
                ],
              );
              (body, applied_fun_ty)
            }
          }
          Err(ty) => {
            // If we have no monadic args and our function isn't monadic return early
            // with normal function application
            if bind_args.is_empty() {
              //return ReducIr::new(ReducIrKind::Tag(reducir_db.mk_reducir_ty(ControlTy(evv_ty, ty)), 0, P::new(ReducIr::app(func_mon, mon_args))));
              return ReducIr::app(func_mon, mon_args);
            }

            let applied_fun_ty = ty.drop_args(reducir_db, mon_args.len()).unwrap();

            // Otherwise lift our return value into our monad
            let y = ReducIrVar::new(self.generate_local(), applied_fun_ty);

            let body = ReducIr::local(y, ReducIr::app(func_mon, mon_args), pure(ReducIr::var(y)));

            (body, applied_fun_ty)
          }
        };

        bind_args.into_iter().rfold(body, |body, (arg, arg_var)| {
          ReducIr::app(
            ReducIr::ty_app(
              bind.clone(),
              [
                ReducIrTyApp::Ty(evv_ty),
                ReducIrTyApp::Ty(arg_var.ty),
                ReducIrTyApp::Ty(ret_ty),
              ],
            ),
            [arg, ReducIr::abss([arg_var], body)],
          )
        })
      }
      Locals(binds, body) => {
        let mut needs_mon_bind = vec![];
        let binds = binds
          .iter()
          .filter_map(|bind| {
            let defn = self.lower_monadic(evv_ty, &bind.defn);
            let defn_ty = defn
              .type_check(self.db)
              .expect("Monadic lowered defn should type check");
            match defn_ty.try_unwrap_monadic(self.db) {
              Ok(UnwrapMonTy { .. }) => {
                needs_mon_bind.push(defn);
                None
              }
              // No monadic type we can just bind this normally
              Err(_) => Some(Bind::new(bind.var, defn)),
            }
          })
          .collect::<Vec<_>>();

        let mut body = self.lower_monadic(evv_ty, body);
        let body_ty = body
          .type_check(self.db)
          .map_err_pretty_with(self.db)
          .unwrap();
        if !needs_mon_bind.is_empty() {
          body = needs_mon_bind
            .into_iter()
            .fold(pure(body), |body, mon_defn| {
              self.bind(mon_defn, |_| body_ty, |_, _| body)
            });
        }
        ReducIr::locals(binds, body)
      }
      TyAbs(tyvar, ir) => ReducIr::new(TyAbs(*tyvar, P::new(self.lower_monadic(evv_ty, ir)))),
      TyApp(ir, ty) => ReducIr::new(TyApp(P::new(self.lower_monadic(evv_ty, ir)), ty.clone())),
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
            return pure(ReducIr::new(Struct(
              vars.into_iter().map(ReducIr::var).collect(),
            )));
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
            |_, s| pure(ReducIr::field_proj(*indx, ReducIr::var(s))),
          ),
          // No bind required
          Err(_) => ReducIr::field_proj(*indx, strukt),
        }
      }
      Tag(ty, tag, ir) => {
        let ir = self.lower_monadic(evv_ty, ir);
        self.bind(
          ir,
          |_| *ty,
          |_, t| pure(ReducIr::new(Tag(*ty, *tag, P::new(ReducIr::var(t))))),
        )
      }
      Case(ty, disc, branches) => {
        let disc = self.lower_monadic(evv_ty, disc);
        self.bind(
          disc,
          |_| *ty,
          |ctx, d| {
            ReducIr::case_on_var(
              ctx.mk_mon_ty(evv_ty, *ty),
              d,
              branches.iter().map(|b| ctx.lower_monadic(evv_ty, b)),
            )
          },
        )
      }
      // TODO: do we need to handle this specially? item should be lowered monadically so it
      // already returns a monad when we call it here
      Item(item, ty) => match item {
        ReducIrTermName::Term(term) => {
          let mi = self.db.lower_reducir_mon_item_of(*term);
          let mon_item = mi.item(self.db.as_reducir_db());
          let mon_ty = mon_item
            .type_check(self.db.as_reducir_db())
            .expect("Type checking must succeed for item to exist");
          ReducIr::new(Item(*item, mon_ty))
        }
        // Generated items don't have monadic type (they are row evidence).
        // So they can reuse their type unmodified.
        ReducIrTermName::Gen(_) => ReducIr::new(Item(*item, *ty)),
      },
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
      X(DelimCont::Prompt(marker, upd_evv, body)) => {
        let update_evv_fn_ty = upd_evv
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();
        // Invariant that this is a function from evv to upd_evv type.
        let (_, upd_evv_ty) = match update_evv_fn_ty.kind(reducir_db) {
          FunTy(args, ret) => (args[0], ret),
          _ => unreachable!(),
        };

        let evv_var = ReducIrVar::new(
          ReducIrLocal {
            top_level: self.current,
            id: self.evv_var_id,
          },
          upd_evv_ty,
        );
        let mon_body = self.lower_monadic(upd_evv_ty, body);
        let mon_body = ReducIr::abss([evv_var], ReducIr::app(mon_body, [ReducIr::var(evv_var)]));
        let mon_marker = self.lower_monadic(evv_ty, marker);
        let mon_body_ty = mon_body
          .type_check(reducir_db)
          .map_err_pretty_with(reducir_db)
          .unwrap();
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
            ],
          ),
          [mon_marker, upd_evv.assume_no_ext(), mon_body],
        )
      }
      X(DelimCont::Yield(ty, mark, f)) => {
        let w = ReducIrVar::new(
          ReducIrLocal {
            top_level: self.current,
            id: self.evv_var_id,
          },
          evv_ty,
        );
        let x = ReducIrVar::new(self.generate_local(), *ty);
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
            self.mk_reducir_ty(ControlTy(evv_ty, *ty)),
            1,
            // Wrap this struct in type variables.
            // These are used to instantiate the existentials during optimization.
            ReducIr::ty_abs(
              [a, b, c],
              ReducIr::new(Struct(vec![
                self.lower_monadic(evv_ty, mark),
                self.lower_monadic(evv_ty, f),
                ReducIr::abss([x], pure(ReducIr::var(x))),
              ]))
              .subst(self.db, Subst::Inc(3)),
            ),
          ),
        )
      }
    }
  }
}
