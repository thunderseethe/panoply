use std::convert::Infallible;
use std::ops::Deref;

use base::id::{Id, IdSupply, ReducIrTyVarId, ReducIrVarId, TermName};
use base::modules::Module;
use base::pretty::PrettyErrorWithDb;
use reducir::mon::MonReducIrRowEv;
use reducir::ty::{
  IntoPayload, Kind, MkReducIrTy, ReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrVarTy, Subst,
  UnwrapMonTy,
};
use reducir::{
  default_endotraverse_ir, Bind, ReducIr, ReducIrEndoFold, ReducIrFold, ReducIrItemOccurence,
  ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar, TypeCheck,
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
    | ReducIrKind::Locals(_, _)
    | ReducIrKind::Coerce(_, _) => false,
    ReducIrKind::Struct(elems) => elems.iter().all(is_value),
    ReducIrKind::FieldProj(_, base) => is_value(base),
    ReducIrKind::Tag(_, _, base) => is_value(base),
  }
}

struct Simplify<'a> {
  db: &'a dyn crate::Db,
  builtin_evs: FxHashMap<ReducIrTermName, &'a ReducIr>,
  supply: &'a mut IdSupply<ReducIrVarId>,
  evv_var_id: ReducIrVarId,
  top_level: ReducIrTermName,
}
impl ReducIrEndoFold for Simplify<'_> {
  type Ext = Infallible;
  fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
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
    match kind {
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
        _ => ReducIr::new(kind),
      },
      Locals(binds, body) => {
        let occs = occurence_analysis(&Locals(binds.clone(), body.clone()));
        let mut env = FxHashMap::default();
        let binds = binds
          .into_iter()
          .filter_map(|bind| {
            let arg = bind
              .defn
              .fold(&mut Inline::new(self.db, &mut env, self.evv_var_id))
              .fold(self);
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
        ReducIr::locals(
          binds,
          body
            .fold(&mut Inline::new(self.db, &mut env, self.evv_var_id))
            .fold(self),
        )
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
          Item(occ)
            if occ.name.name(self.db) == self.db.ident_str("__mon_eqm")
              && spine.as_slice().chunks(2).all(|c| c[0] == c[1]) =>
          {
            let unit = self.db.mk_reducir_ty(ReducIrTyKind::ProductTy(vec![]));
            ReducIr::tag(
              self.db.mk_coprod_ty(vec![unit, unit]),
              1,
              ReducIr::new(Struct(vec![])),
            )
          }
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
      Item(occ) if occ.inline => match self.builtin_evs.get(&occ.name) {
        Some(builtin) => builtin.fold(&mut RenumberVars {
          supply: self.supply,
          top_level: self.top_level,
          subst: FxHashMap::default(),
          evv_var_id: self.evv_var_id,
        }),
        None => ReducIr::new(kind),
      },
      kind => ReducIr::new(kind),
    }
  }
}

struct RenumberVars<'a> {
  supply: &'a mut IdSupply<ReducIrVarId>,
  top_level: ReducIrTermName,
  subst: FxHashMap<ReducIrLocal, ReducIrLocal>,
  evv_var_id: ReducIrVarId,
}
impl<'a> RenumberVars<'a> {
  fn gen_local(&mut self) -> ReducIrLocal {
    ReducIrLocal {
      id: self.supply.supply_id(),
      top_level: self.top_level,
    }
  }
}
impl<'a> ReducIrEndoFold for RenumberVars<'a> {
  type Ext = Infallible;

  fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
    // TODO Replace bound variables with new variables and add them to the substitution
    match kind {
      ReducIrKind::Var(v) => match self.subst.get(&v.var) {
        Some(new_v) => ReducIr::var(ReducIrVar::new(*new_v, v.ty)),
        None => ReducIr::var(v),
      },
      kind => ReducIr::new(kind),
    }
  }

  fn endotraverse_ir(&mut self, ir: &ReducIr<Self::Ext>) -> ReducIr<Self::Ext> {
    // TODO Replace binders and add them to the substitution
    match ir.kind() {
      ReducIrKind::Locals(binds, body) => {
        let body = body.fold(self);
        let mut removals = vec![];
        let binds = binds
          .iter()
          .map(|local| {
            // Don't renumber evv
            if local.var.var.id == self.evv_var_id {
              return local.fold(self);
            }
            let new_var = self.gen_local();
            self.subst.insert(local.var.var, new_var);
            removals.push(local.var.var);
            let mut local = local.fold(self);
            local.var = ReducIrVar::new(new_var, local.var.ty);
            local
          })
          .collect::<Vec<_>>();
        let kind = ReducIr::locals(binds, body.fold(self)).kind;
        for var in removals {
          self.subst.remove(&var);
        }
        self.fold_ir(kind)
      }
      ReducIrKind::Abs(vars, body) => {
        let body = body.fold(self);
        let mut removals = vec![];
        let vars = vars
          .iter()
          .map(|var| {
            // Don't renumber evv
            if var.var.id == self.evv_var_id {
              return *var;
            }
            let new_var = self.gen_local();
            self.subst.insert(var.var, new_var);
            removals.push(var.var);
            ReducIrVar::new(new_var, var.ty)
          })
          .collect::<Vec<_>>();
        // We do it this way to reuse smart constructor logic
        let kind = ReducIr::abss(vars, body.fold(self)).kind;
        for var in removals {
          self.subst.remove(&var);
        }
        self.fold_ir(kind)
      }
      _ => default_endotraverse_ir(self, ir),
    }
  }
}

struct EtaExpand<'db, DB: ?Sized> {
  db: &'db DB,
  supply: &'db mut IdSupply<ReducIrVarId>,
  name: ReducIrTermName,
}
impl<DB: ?Sized> EtaExpand<'_, DB> {
  fn gen_var(&mut self, ty: ReducIrTy) -> ReducIrVar {
    ReducIrVar::new(
      ReducIrLocal {
        top_level: self.name,
        id: self.supply.supply_id(),
      },
      ty,
    )
  }
}
impl<DB: ?Sized + crate::Db> ReducIrEndoFold for EtaExpand<'_, DB> {
  type Ext = Infallible;

  fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
    fn try_as_item(ir: &ReducIr) -> Option<&ReducIrItemOccurence> {
      match ir.kind() {
        ReducIrKind::TyApp(ir, _) => try_as_item(ir),
        ReducIrKind::Item(occ) => Some(occ),
        _ => None,
      }
    }
    match kind {
      ReducIrKind::App(head, mut spine) => match try_as_item(&head) {
        Some(_occ) => {
          // We do this in case our head is ty_app
          if let ReducIrTyKind::FunTy(params, ret) = head
            .type_check(self.db)
            .unwrap()
            .kind(self.db.as_reducir_db())
          {
            let mut params = params.into_vec();
            //  TODO: Clean this up as some kind of combinator
            if let Ok(UnwrapMonTy { evv_ty, .. }) = ret.try_unwrap_monadic(self.db) {
              params.push(evv_ty);
            }
            if params.len() > spine.len() {
              let vars = params
                .iter()
                .skip(spine.len())
                .map(|ty| self.gen_var(*ty))
                .collect::<Vec<_>>();
              spine.extend(vars.iter().map(|v| ReducIr::var(*v)));
              ReducIr::abss(vars, ReducIr::app(head.into_inner(), spine))
            } else {
              ReducIr::app(head.into_inner(), spine)
            }
          } else {
            ReducIr::app(head.into_inner(), spine)
          }
        }
        None => ReducIr::app(head.into_inner(), spine),
      },
      // Just in case
      ReducIrKind::Abs(vars, body) => ReducIr::abss(vars.iter().copied(), body.into_inner()),
      _ => ReducIr::new(kind),
    }
  }
}

pub(crate) fn simplify(
  db: &dyn crate::Db,
  name: TermName,
  row_evs: &[MonReducIrRowEv],
  ir: &ReducIr,
  supply: &mut IdSupply<ReducIrVarId>,
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
  bind
    .type_check(db)
    .map_err_pretty_with(db)
    .expect("Bind should type check");
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

  ir.fold(&mut Simplify {
    db,
    builtin_evs: builtin_evs.clone(),
    supply,
    evv_var_id: ReducIrVarId::from_raw(0),
    top_level: ReducIrTermName::Term(name),
  })
  .fold(&mut EtaExpand {
    db,
    supply,
    name: ReducIrTermName::Term(name),
  })
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
  let m_ty = db.mk_reducir_ty(VarTy(3));
  let upd_m = db.mk_reducir_ty(VarTy(2));
  let a = db.mk_reducir_ty(VarTy(1));
  let b = db.mk_reducir_ty(VarTy(0));

  let mark_ty = db.mk_reducir_ty(MarkerTy(b));
  let upd_fun_ty = db.mk_fun_ty([m_ty], upd_m);
  let body_fun_ty = db.mk_mon_ty(upd_m, a);
  let ret_clause_ty = db.mk_fun_ty([a], db.mk_mon_ty(m_ty, b));
  let ret_ty = db.mk_mon_ty(m_ty, b);

  let mut var_gen = IdSupply::default();
  let mut gen_local = || ReducIrLocal {
    top_level: name,
    id: var_gen.supply_id(),
  };
  // Make sure evv is var id 0.
  let evv = ReducIrVar::new(gen_local(), m_ty);
  let mark = ReducIrVar::new(gen_local(), mark_ty);
  let upd = ReducIrVar::new(gen_local(), upd_fun_ty);
  let ret = ReducIrVar::new(gen_local(), ret_clause_ty);
  let body = ReducIrVar::new(gen_local(), body_fun_ty);

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

  /*
  prompt : ∀𝜇 𝜇'. ∀𝛼  h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
  prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
    Pure x -> Pure x
    Yield m' f k ->
      case m == m' of
        False -> Yield m' f (\x . prompt upd m h (k x))
        True -> f (\x . prompt upd m h (k x)) w
  */
  let y_var = ReducIrVar::new(gen_local(), db.mk_yield_ty(VarTy(2), VarTy(1)));
  let mut tyvar_supply = IdSupply::default();
  let mut tyvars = |n| {
    std::iter::from_fn(|| {
      Some(ReducIrVarTy {
        var: tyvar_supply.supply_id(),
        kind: Kind::Type,
      })
    })
    .take(n)
    .collect::<Vec<_>>()
  };
  let tag_ty = db.mk_reducir_ty(ControlTy(m_ty, b));
  ReducIr::ty_abs(
    tyvars(4),
    ReducIr::abss(
      [mark, upd, ret, body, evv],
      ReducIr::case(
        tag_ty,
        ReducIr::app(
          ReducIr::var(body),
          [ReducIr::app(ReducIr::var(upd), [ReducIr::var(evv)])],
        ),
        [
          ReducIr::abss(
            [x],
            ReducIr::app(ReducIr::var(ret), [ReducIr::var(x), ReducIr::var(evv)]),
          ),
          {
            // Instantiate y as if our markers are equal
            let y = ReducIr::ty_app(
              ReducIr::var(y_var),
              [
                ReducIrTyApp::Ty(a), // TODO figure out what beta should be here
                ReducIrTyApp::Ty(m_ty),
                ReducIrTyApp::Ty(b),
              ],
            );
            ReducIr::abss(
              [y_var],
              ReducIr::case(
                tag_ty,
                ReducIr::app(meq, [ReducIr::var(mark), ReducIr::field_proj(0, y.clone())]),
                [
                  // False branch
                  ReducIr::abss(
                    [unused],
                    ReducIr::tag(
                      tag_ty,
                      1,
                      ReducIr::ty_abs(tyvars(3), {
                        let beta_ty = db.mk_reducir_ty(VarTy(2));
                        // Re-instantiate y but in terms of the type variables for yield
                        // We know m != m_ here so we're repacking an existential into yield.
                        let y = ReducIr::ty_app(
                          ReducIr::var(y_var).subst(db, Subst::Inc(3)),
                          [
                            ReducIrTyApp::Ty(beta_ty),
                            ReducIrTyApp::Ty(db.mk_reducir_ty(VarTy(1))),
                            ReducIrTyApp::Ty(db.mk_reducir_ty(VarTy(0))),
                          ],
                        );
                        let y_ty = y.type_check(db).unwrap();
                        let reinst_y = ReducIrVar::new(gen_local(), y_ty);
                        ReducIr::local(
                          reinst_y,
                          y,
                          ReducIr::new(ReducIrKind::Struct(vec![
                            ReducIr::field_proj(0, ReducIr::var(reinst_y)),
                            ReducIr::field_proj(1, ReducIr::var(reinst_y)),
                            {
                              let x = ReducIrVar::new(gen_local(), beta_ty);
                              let evv =
                                ReducIrVar::new(gen_local(), evv.ty.subst(db, Subst::Inc(3)));
                              let prompt_ty = db.mk_forall_ty(
                                [Kind::Type, Kind::Type, Kind::Type, Kind::Type],
                                db.mk_fun_ty(
                                  [mark_ty, upd_fun_ty, ret_clause_ty, body_fun_ty],
                                  ret_ty,
                                ),
                              );
                              let prompt_item = ReducIr::new(ReducIrKind::Item(
                                ReducIrItemOccurence::with_inline(name, prompt_ty, false),
                              ));
                              ReducIr::abss(
                                [x, evv],
                                ReducIr::app(
                                  ReducIr::ty_app(
                                    prompt_item,
                                    [
                                      ReducIrTyApp::Ty(m_ty),
                                      ReducIrTyApp::Ty(upd_m),
                                      ReducIrTyApp::Ty(a),
                                      ReducIrTyApp::Ty(b),
                                    ],
                                  )
                                  .subst(db, Subst::Inc(3)),
                                  [
                                    ReducIr::var(mark).subst(db, Subst::Inc(3)),
                                    ReducIr::var(upd).subst(db, Subst::Inc(3)),
                                    ReducIr::var(ret).subst(db, Subst::Inc(3)),
                                    ReducIr::app(
                                      ReducIr::field_proj(2, ReducIr::var(reinst_y)),
                                      [ReducIr::var(x)],
                                    ),
                                    ReducIr::var(evv),
                                  ],
                                ),
                              )
                            },
                          ])),
                        )
                      }),
                    ),
                  ),
                  // True branch
                  ReducIr::abss([unused], {
                    ReducIr::app(
                      ReducIr::field_proj(1, y.clone()),
                      [
                        {
                          let x = ReducIrVar::new(gen_local(), a);
                          let evv = ReducIrVar::new(gen_local(), evv.ty);
                          let prompt_ty = db.mk_forall_ty(
                            [Kind::Type, Kind::Type, Kind::Type, Kind::Type],
                            db.mk_fun_ty([mark_ty, upd_fun_ty, ret_clause_ty, body_fun_ty], ret_ty),
                          );
                          let prompt_item = ReducIr::new(ReducIrKind::Item(
                            ReducIrItemOccurence::with_inline(name, prompt_ty, false),
                          ));
                          ReducIr::abss(
                            [x, evv],
                            ReducIr::app(
                              ReducIr::ty_app(
                                prompt_item,
                                [
                                  ReducIrTyApp::Ty(m_ty),
                                  ReducIrTyApp::Ty(upd_m),
                                  ReducIrTyApp::Ty(a),
                                  ReducIrTyApp::Ty(b),
                                ],
                              ),
                              [
                                ReducIr::var(mark),
                                ReducIr::var(upd),
                                ReducIr::var(ret),
                                ReducIr::app(ReducIr::field_proj(2, y.clone()), [ReducIr::var(x)]),
                                ReducIr::var(evv),
                              ],
                            ),
                          )
                        },
                        ReducIr::var(evv),
                      ],
                    )
                  }),
                ],
              ),
            )
          },
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

  let y_var = gen_local(db.mk_yield_ty(VarTy(2), VarTy(1)));
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
            let beta_ty = db.mk_reducir_ty(VarTy(2));
            let yield_m_ty = db.mk_reducir_ty(VarTy(1));
            let ret_ty = db.mk_reducir_ty(VarTy(0));
            let y = ReducIr::ty_app(
              ReducIr::var(y_var).subst(db, Subst::Inc(3)),
              [
                ReducIrTyApp::Ty(beta_ty),
                ReducIrTyApp::Ty(yield_m_ty),
                ReducIrTyApp::Ty(ret_ty),
              ],
            );
            let yield_ = gen_local(y.type_check(db).expect("y should typecheck"));

            let mon = ReducIr::field_proj(0, ReducIr::var(yield_));
            let f = ReducIr::field_proj(1, ReducIr::var(yield_));
            let k = ReducIr::field_proj(2, ReducIr::var(yield_));
            let x = gen_local(beta_ty);
            ReducIr::tag(
              ctl_m_b,
              1,
              ReducIr::ty_abs(
                [tv3, tv4, tv5],
                ReducIr::local(
                  yield_,
                  y,
                  ReducIr::new(ReducIrKind::Struct(vec![
                    mon,
                    f,
                    ReducIr::abss(
                      [x],
                      ReducIr::app(
                        ReducIr::ty_app(
                          bind_item,
                          [
                            ReducIrTyApp::Ty(m.subst(db, Subst::Inc(3))),
                            ReducIrTyApp::Ty(a.subst(db, Subst::Inc(3))),
                            ReducIrTyApp::Ty(b.subst(db, Subst::Inc(3))),
                          ],
                        ),
                        [
                          ReducIr::app(k, [ReducIr::var(x)]),
                          ReducIr::var(g).subst(db, Subst::Inc(3)),
                        ],
                      ),
                    ),
                  ])),
                ),
              ),
            )
          }),
        ],
      ),
    ),
  )
}
