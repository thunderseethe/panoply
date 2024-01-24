use std::convert::Infallible;
use std::ops::Deref;

use base::id::{IdSupply, ReducIrTyVarId, ReducIrVarId, TermName};
use base::modules::Module;
use base::pretty::PrettyErrorWithDb;
use reducir::mon::MonReducIrRowEv;
use reducir::ty::{
  IntoPayload, Kind, MkReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrVarTy, Subst,
};
use reducir::{
  Bind, ReducIr, ReducIrFold, ReducIrItemOccurence, ReducIrKind, ReducIrLocal, ReducIrTermName,
  ReducIrTyErr, ReducIrVar, TypeCheck,
};
use rustc_hash::FxHashMap;

use crate::occurrence::{occurence_analysis, Occurrence};
use crate::subst::Inline;

/// True if an ir term is a value (contains no computations), false if the term does require
/// computation that can be done
fn is_value(ir: &ReducIr) -> bool {
  match ir.kind() {
    ReducIrKind::Int(_)
    | ReducIrKind::Var(_)
    | ReducIrKind::Item(_)
    | ReducIrKind::Abs(_, _)
    | ReducIrKind::TyAbs(_, _)
    | ReducIrKind::X(_) => true,
    ReducIrKind::App(_, _)
    | ReducIrKind::Case(_, _, _)
    | ReducIrKind::TyApp(_, _)
    | ReducIrKind::Locals(_, _) => false,
    ReducIrKind::Struct(elems) => elems.iter().all(is_value),
    ReducIrKind::FieldProj(_, base) => is_value(base),
    ReducIrKind::Tag(_, _, base) => is_value(base),
  }
}

struct Simplify<'a> {
  db: &'a dyn crate::Db,
  builtin_evs: FxHashMap<ReducIrTermName, &'a ReducIr>,
  //occs: Occurrences,
}
impl ReducIrFold for Simplify<'_> {
  type InExt = Infallible;

  type OutExt = Infallible;

  fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
    unreachable!()
  }

  fn fold_ir(&mut self, ir: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
    use reducir::ReducIrKind::*;
    fn apply_tyabs<'a>(
      body: &'a ReducIr,
      in_app: &mut Vec<ReducIrTyApp>,
      mut out_app: Vec<ReducIrTyApp>,
    ) -> (&'a ReducIr, Vec<ReducIrTyApp>) {
      match body.kind() {
        TyAbs(_, inner) => match in_app.pop() {
          Some(app) => {
            out_app.push(app);
            apply_tyabs(inner, in_app, out_app)
          }
          None => (body, out_app),
        },
        _ => (body, out_app),
      }
    }
    match ir {
      TyApp(body, mut ty_app) => {
        ty_app.reverse();
        let (body, apps) = apply_tyabs(&body, &mut ty_app, vec![]);
        ty_app.reverse();
        let body = if apps.is_empty() {
          body.clone()
        } else {
          // only perform a substitution if we actually applied types
          body.subst(
            self.db,
            apps.into_iter().fold(Subst::Inc(0), |subst, app| {
              subst.cons(app.into_payload(self.db))
            }),
          )
        };
        ReducIr::ty_app(body, ty_app)
      }
      FieldProj(indx, ref base) => match base.kind() {
        Struct(elems) => self.traverse_ir(&elems[indx]),
        _ => ReducIr::new(ir),
      },
      Locals(binds, body) => {
        let occs = occurence_analysis(&Locals(binds.clone(), body.clone()));
        let mut env = FxHashMap::default();
        let binds = binds
          .into_iter()
          .filter_map(|bind| {
            let arg = bind.defn.fold(&mut Inline::new(self.db, &env)).fold(self);
            if is_value(&arg)
              || occs[bind.var] == Occurrence::Once
              || occs[bind.var] == Occurrence::ManyBranch
            {
              env.insert(bind.var.var, arg);
              None
            } else {
              Some(Bind::new(bind.var, arg))
            }
          })
          .collect::<Vec<_>>();
        ReducIr::locals(binds, body.fold(&mut Inline::new(self.db, &env)).fold(self))
      }
      App(head, spine) => {
        let mut app_binds = vec![];
        let spine = spine
          .into_iter()
          .map(|ir| match ir.kind {
            ReducIrKind::Locals(binds, body) => {
              app_binds.extend(binds);
              body.into_inner()
            }
            _ => ir,
          })
          .collect::<Vec<_>>();
        let app = match head.kind() {
          Abs(ref args, body) => {
            let mut binds = vec![];

            let mut args_iter = args.iter().copied();
            let mut spine_iter = spine.into_iter();

            let body = loop {
              match (args_iter.next(), spine_iter.next()) {
                (Some(var), Some(arg)) => {
                  binds.push(Bind::new(var, arg));
                }
                (None, Some(arg)) => {
                  let mut apps = vec![arg];
                  apps.extend(spine_iter);
                  break ReducIr::app(body.deref().clone(), apps);
                }
                (Some(var), None) => {
                  let mut vars = vec![var];
                  vars.extend(args_iter);
                  break ReducIr::abss(vars, body.deref().clone());
                }
                (None, None) => break body.deref().clone(),
              }
            };

            self.traverse_ir(&ReducIr::locals(binds, body))
          }
          // Let floating for app
          Locals(binds, body) => ReducIr::locals(
            binds.iter().cloned(),
            self.traverse_ir(&ReducIr::new(App(body.clone(), spine))),
          ),
          _ => ReducIr::new(App(head, spine)),
        };
        ReducIr::locals(app_binds, app)
      }
      Case(ty, discr, branches) => match discr.kind() {
        Tag(_, tag, val) => self.traverse_ir(&ReducIr::app(
          branches[*tag].clone(),
          [val.clone().into_inner()],
        )),
        // Let floating for case
        Locals(binds, body) => ReducIr::locals(
          binds.iter().cloned(),
          self.traverse_ir(&ReducIr::case(
            ty,
            body.clone().into_inner(),
            branches.iter().cloned(),
          )),
        ),
        _ => ReducIr::new(Case(ty, discr, branches)),
      },
      // Always inline row evidence
      Item(occ) => match self.builtin_evs.remove(&occ.name) {
        Some(builtin) => self.traverse_ir(builtin),
        None => ReducIr::new(ir),
      },
      ir => ReducIr::new(ir),
    }
  }
}

pub(crate) fn simplify(
  db: &dyn crate::Db,
  name: TermName,
  row_evs: &[MonReducIrRowEv],
  ir: &ReducIr,
) -> ReducIr {
  let reducir_db = db.as_reducir_db();

  let mut builtin_evs = row_evs
    .iter()
    .flat_map(|row_ev| {
      let simple_item = row_ev.simple(reducir_db);
      let scoped_item = row_ev.scoped(reducir_db);

      let simple_name = ReducIrTermName::Gen(simple_item.name(reducir_db));
      let scoped_name = ReducIrTermName::Gen(scoped_item.name(reducir_db));

      let simple = (simple_name, simple_item.item(reducir_db));
      let scoped = (scoped_name, scoped_item.item(reducir_db));
      [simple, scoped]
    })
    .collect::<FxHashMap<_, _>>();

  let module = name.module(db.as_core_db());
  let bind_name = ReducIrTermName::gen(db, "__mon_bind", module);
  let bind = bind_term(db, bind_name);
  debug_assert!(bind.type_check(db).map_err_pretty_with(db).is_ok());
  builtin_evs.insert(bind_name, &bind);

  let prompt_name = ReducIrTermName::gen(db, "__mon_prompt", module);
  let prompt = prompt_term(db, module, prompt_name);
  prompt
    .type_check(db)
    .map_err_pretty_with(db)
    .expect("Prompt should type check");
  builtin_evs.insert(prompt_name, &prompt);

  let freshm_name = ReducIrTermName::gen(db, "__mon_freshm", module);
  let freshm = freshm_term(db, module, freshm_name);
  builtin_evs.insert(freshm_name, &freshm);

  ir.fold(&mut Simplify { db, builtin_evs })
}

fn freshm_term(db: &dyn crate::Db, module: Module, top_level: ReducIrTermName) -> ReducIr {
  let a = ReducIrVarTy {
    var: ReducIrTyVarId(0),
    kind: Kind::Type,
  };
  let b = ReducIrVarTy {
    var: ReducIrTyVarId(1),
    kind: Kind::Type,
  };
  let var_ty0 = db.mk_reducir_ty(ReducIrTyKind::VarTy(0));
  let var_ty1 = db.mk_reducir_ty(ReducIrTyKind::VarTy(1));
  let f = ReducIrVar::new(
    ReducIrLocal {
      top_level,
      id: ReducIrVarId(0),
    },
    db.mk_fun_ty(
      [db.mk_reducir_ty(ReducIrTyKind::MarkerTy(var_ty1))],
      var_ty0,
    ),
  );

  let marker = ReducIr::new(ReducIrKind::item(
    ReducIrTermName::gen(db, "__mon_generate_marker", module),
    db.mk_forall_ty(
      [Kind::Type],
      db.mk_fun_ty(
        [db.mk_prod_ty(vec![])],
        db.mk_reducir_ty(ReducIrTyKind::MarkerTy(
          var_ty1.shift(db.as_reducir_db(), 1),
        )),
      ),
    ),
  ));

  ReducIr::ty_abs(
    [a, b],
    ReducIr::abss(
      [f],
      ReducIr::app(
        ReducIr::var(f),
        [ReducIr::app(
          ReducIr::ty_app(marker, [ReducIrTyApp::Ty(var_ty1)]),
          [ReducIr::new(ReducIrKind::Struct(vec![]))],
        )],
      ),
    ),
  )
}

/// Prompt handles "installing" our prompt into the stack and running an action under an
/// updated effect row
pub(super) fn prompt_term(db: &dyn crate::Db, module: Module, name: ReducIrTermName) -> ReducIr {
  use ReducIrTyKind::*;
  let m_ty = db.mk_reducir_ty(VarTy(2));
  let upd_m = db.mk_reducir_ty(VarTy(1));
  let a = db.mk_reducir_ty(VarTy(0));

  let mark_ty = db.mk_reducir_ty(MarkerTy(a));
  let upd_fun_ty = db.mk_fun_ty([m_ty], upd_m);
  let body_fun_ty = db.mk_mon_ty(upd_m, a);
  let ret_ty = db.mk_reducir_ty(ControlTy(m_ty, a));

  let prompt_type = db.mk_forall_ty(
    [Kind::Type, Kind::Type, Kind::Type],
    db.mk_fun_ty(
      [mark_ty, upd_fun_ty, body_fun_ty],
      db.mk_fun_ty([m_ty], ret_ty),
    ),
  );

  let mut var_gen = IdSupply::default();
  let mut gen_local = || ReducIrLocal {
    top_level: name,
    id: var_gen.supply_id(),
  };
  let mark = ReducIrVar::new(gen_local(), mark_ty);
  let upd = ReducIrVar::new(gen_local(), upd_fun_ty);
  let body = ReducIrVar::new(gen_local(), body_fun_ty);
  let evv = ReducIrVar::new(gen_local(), m_ty);

  let reinstall_prompt = || {
    let prompt_item = ReducIr::new(ReducIrKind::<Infallible>::Item(
      ReducIrItemOccurence::with_inline(name, prompt_type, false),
    ));
    ReducIr::app(
      ReducIr::ty_app(
        prompt_item,
        [
          ReducIrTyApp::Ty(m_ty),
          ReducIrTyApp::Ty(upd_m),
          ReducIrTyApp::Ty(a),
        ],
      ),
      [ReducIr::var(mark), ReducIr::var(upd)],
    )
  };

  let exists_b = db.mk_reducir_ty(VarTy(2));
  let exists_m = db.mk_reducir_ty(VarTy(1));
  let exists_r = db.mk_reducir_ty(VarTy(0));
  let exists_mon_m_r = db.mk_mon_ty(exists_m, exists_r);
  let exists_body_fun_ty = db.mk_mon_ty(VarTy(4), VarTy(3));
  let y_var = ReducIrVar::new(
    gen_local(),
    db.mk_forall_ty(
      [Kind::Type, Kind::Type, Kind::Type],
      db.mk_prod_ty(vec![
        db.mk_reducir_ty(MarkerTy(exists_r)),
        db.mk_fun_ty([db.mk_fun_ty([exists_b], exists_mon_m_r)], exists_mon_m_r),
        db.mk_fun_ty([exists_b], exists_body_fun_ty),
      ]),
    ),
  );
  let y = ReducIr::ty_app(
    ReducIr::var(y_var),
    [
      ReducIrTyApp::Ty(a),
      ReducIrTyApp::Ty(m_ty),
      ReducIrTyApp::Ty(a),
    ],
  );

  let unit_ty = db.mk_prod_ty(vec![]);
  let x = ReducIrVar::new(gen_local(), a);
  let unused = ReducIrVar::new(gen_local(), unit_ty);

  let meq = ReducIr::new(ReducIrKind::item(
    ReducIrTermName::gen(db, "__mon_eqm", module),
    db.mk_fun_ty(
      [mark_ty, mark_ty],
      db.mk_reducir_ty(CoproductTy(vec![unit_ty, unit_ty])),
    ),
  ));

  fn compose<'a, DB: ?Sized + crate::Db>(
    db: &DB,
    mut gen_local: impl FnMut() -> ReducIrLocal,
    a_fn: ReducIr,
    b_fn: &'a ReducIr,
  ) -> Result<ReducIr, ReducIrTyErr<'a, Infallible>> {
    let b_fn_ty = b_fn.type_check(db)?;

    let b_arg = match b_fn_ty.kind(db.as_reducir_db()) {
      FunTy(args, _) => args[0],
      _ => {
        return Err(ReducIrTyErr::ExpectedFunTy {
          ty: b_fn_ty,
          func: b_fn,
        })
      }
    };

    let x = ReducIrVar::new(gen_local(), b_arg);

    Ok(ReducIr::abss(
      [x],
      ReducIr::app(a_fn, [ReducIr::app(b_fn.clone(), [ReducIr::var(x)])]),
    ))
  }

  /*
  prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
  prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
    Pure x -> Pure x
    Yield m' f k ->
      case m == m' of
        False -> Yield m' f (\x . prompt upd m h (k x))
        True -> f (\x . prompt upd m h (k x)) w
  */
  let m_ = ReducIr::field_proj(0, y.clone());
  let f = ReducIr::field_proj(1, y.clone());
  let k = ReducIr::field_proj(2, y);
  let mut tyvar_supply = IdSupply::default();
  ReducIr::ty_abs(
    [
      ReducIrVarTy {
        var: tyvar_supply.supply_id(),
        kind: Kind::Type,
      },
      ReducIrVarTy {
        var: tyvar_supply.supply_id(),
        kind: Kind::Type,
      },
      ReducIrVarTy {
        var: tyvar_supply.supply_id(),
        kind: Kind::Type,
      },
    ],
    ReducIr::abss(
      [mark, upd, body, evv],
      ReducIr::case(
        ret_ty,
        ReducIr::app(
          ReducIr::var(body),
          [ReducIr::app(ReducIr::var(upd), [ReducIr::var(evv)])],
        ),
        [
          ReducIr::abss([x], ReducIr::tag(ret_ty, 0, ReducIr::var(x))),
          ReducIr::abss(
            [y_var],
            ReducIr::case(
              ret_ty,
              ReducIr::app(meq, [ReducIr::var(mark), m_.clone()]),
              [
                // False branch
                ReducIr::abss(
                  [unused],
                  ReducIr::tag(
                    ret_ty,
                    1,
                    ReducIr::new(ReducIrKind::Struct(vec![
                      m_,
                      f.clone(),
                      compose(db, &mut gen_local, reinstall_prompt(), &k)
                        .map_err_pretty_with(db)
                        .expect("Failed to compose"),
                      k.clone(),
                    ])),
                  ),
                ),
                // True branch
                ReducIr::abss(
                  [unused],
                  ReducIr::app(
                    f,
                    [
                      compose(db, &mut gen_local, reinstall_prompt(), &k)
                        .expect("Failed to compose"),
                      ReducIr::var(evv),
                    ],
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    ),
  )
}

/// TODO: Return an item representing the bind implementation of our delimited continuation
/// monad
pub(super) fn bind_term<DB: ?Sized + crate::Db>(db: &DB, name: ReducIrTermName) -> ReducIr {
  use ReducIrTyKind::*;
  /*
  (e: Mon m a) |> (g : a -> Mon m b) : Mon m b =
     𝜆w. case e w of
         Pure x → g x w (monadic bind)
         Yield m f k → Yield m f (𝜆x. g x |> k)
  */
  let m = db.mk_reducir_ty(VarTy(2));
  let a = db.mk_reducir_ty(VarTy(1));
  let b = db.mk_reducir_ty(VarTy(0));
  let ctl_m_b = db.mk_reducir_ty(ControlTy(m, b));
  let mon_m_b = db.mk_mon_ty(m, b);
  let bind_type = db.mk_forall_ty(
    [Kind::Type, Kind::Type, Kind::Type],
    db.mk_fun_ty([db.mk_mon_ty(m, a), db.mk_fun_ty([a], mon_m_b)], mon_m_b),
  );

  let bind_item = ReducIr::new(ReducIrKind::Item(ReducIrItemOccurence::with_inline(
    name, bind_type, false,
  )));
  let mut tyvar_supply: IdSupply<ReducIrTyVarId> = IdSupply::default();

  let mut supply: IdSupply<ReducIrVarId> = IdSupply::default();
  let mut gen_local = |ty| {
    ReducIrVar::new(
      ReducIrLocal {
        top_level: name,
        id: supply.supply_id(),
      },
      ty,
    )
  };

  let e = gen_local(db.mk_mon_ty(m, a));
  let g = gen_local(db.mk_fun_ty([a], mon_m_b));
  let w = gen_local(m);
  let x = gen_local(a);

  let exists_b = db.mk_reducir_ty(VarTy(2));
  let exists_m = db.mk_reducir_ty(VarTy(1));
  let exists_r = db.mk_reducir_ty(VarTy(0));
  let exists_mon_m_r = db.mk_mon_ty(exists_m, exists_r);
  let exists_body_fun_ty = db.mk_mon_ty(VarTy(5), VarTy(4));
  let y_var = gen_local(db.mk_forall_ty(
    [Kind::Type, Kind::Type, Kind::Type],
    db.mk_prod_ty(vec![
      db.mk_reducir_ty(MarkerTy(exists_r)),
      db.mk_fun_ty([db.mk_fun_ty([exists_b], exists_mon_m_r)], exists_mon_m_r),
      db.mk_fun_ty([exists_b], exists_body_fun_ty),
    ]),
  ));
  let y = ReducIr::ty_app(
    ReducIr::var(y_var),
    [
      ReducIrTyApp::Ty(a),
      ReducIrTyApp::Ty(m),
      ReducIrTyApp::Ty(b),
    ],
  );
  let mut gen_tyvar = || ReducIrVarTy {
    var: tyvar_supply.supply_id(),
    kind: Kind::Type,
  };
  let [tv0, tv1, tv2, tv3, tv4, tv5] = [
    gen_tyvar(),
    gen_tyvar(),
    gen_tyvar(),
    gen_tyvar(),
    gen_tyvar(),
    gen_tyvar(),
  ];

  ReducIr::ty_abs(
    [tv0, tv1, tv2],
    ReducIr::abss(
      [e, g, w],
      ReducIr::case(
        ctl_m_b,
        ReducIr::app(ReducIr::var(e), [ReducIr::var(w)]),
        [
          ReducIr::abss(
            [x],
            ReducIr::app(ReducIr::var(g), [ReducIr::var(x), ReducIr::var(w)]),
          ),
          ReducIr::abss([y_var], {
            let yield_ = gen_local(y.type_check(db).expect("y should typecheck"));

            let mon = ReducIr::field_proj(0, ReducIr::var(yield_));
            let f = ReducIr::field_proj(1, ReducIr::var(yield_));
            let k = ReducIr::field_proj(2, ReducIr::var(yield_));
            ReducIr::local(
              yield_,
              y,
              ReducIr::tag(
                ctl_m_b,
                1,
                ReducIr::ty_abs(
                  [tv3, tv4, tv5],
                  ReducIr::new(ReducIrKind::Struct(vec![
                    mon,
                    f,
                    ReducIr::abss(
                      [x],
                      ReducIr::app(
                        ReducIr::ty_app(
                          bind_item,
                          [
                            ReducIrTyApp::Ty(m),
                            ReducIrTyApp::Ty(a),
                            ReducIrTyApp::Ty(b),
                          ],
                        ),
                        [ReducIr::app(k, [ReducIr::var(x)]), ReducIr::var(g)], //[ReducIr::app(ReducIr::var(g), [ReducIr::var(x)]), k],
                      ),
                    ),
                  ]))
                  .subst(db, Subst::Inc(3)),
                ),
              ),
            )
          }),
        ],
      ),
    ),
  )
}
