use base::{id::MedIrVarId, id_converter::IdConverter, pretty::PrettyErrorWithDb};
use medir::MedIrItemName;
use medir::{Atom, Locals, MedIr, MedIrKind, MedIrTy, MedIrTyKind, MedIrTypedItem, MedIrVar};
use reducir::ty::{ReducIrTy, ReducIrTyKind};
use reducir::{ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar, TypeCheck};

pub(crate) struct LowerCtx<'a> {
  db: &'a dyn crate::Db,
  current: ReducIrTermName,
  var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
  pub(crate) lifts: Vec<medir::Defn>,
}

fn from_reducir_ty<DB: ?Sized + crate::Db>(db: &DB, ty: ReducIrTy) -> MedIrTy {
  match ty.kind(db.as_reducir_db()) {
    ReducIrTyKind::IntTy | ReducIrTyKind::MarkerTy(_) => db.mk_medir_ty(MedIrTyKind::IntTy),
    ReducIrTyKind::FunTy(args, ret) => db.mk_medir_ty(MedIrTyKind::FunTy(
      args.iter().map(|arg| from_reducir_ty(db, *arg)).collect(),
      from_reducir_ty(db, ret),
    )),
    ReducIrTyKind::ProductTy(tys) => db.mk_medir_ty(MedIrTyKind::BlockTy(
      tys.iter().map(|ty| from_reducir_ty(db, *ty)).collect(),
    )),
    ReducIrTyKind::CoproductTy(tys) => {
      let int = db.mk_medir_ty(MedIrTyKind::IntTy);
      tys
        .iter()
        .map(|ty| from_reducir_ty(db, *ty))
        .max_by(|a, b| a.block_size(db).cmp(&b.block_size(db)))
        .map(|ty| {
          db.mk_medir_ty(MedIrTyKind::BlockTy(
            (0..(ty.block_size(db) + 1)).map(|_| int).collect(),
          ))
        })
        .unwrap_or_else(|| db.mk_medir_ty(MedIrTyKind::BlockTy(vec![])))
    }
    ReducIrTyKind::ControlTy(_, _) => {
      let int = db.mk_medir_ty(MedIrTyKind::IntTy);
      db.mk_medir_ty(MedIrTyKind::BlockTy(vec![int, int, int, int]))
    }
    ReducIrTyKind::ForallTy(_, ty) => from_reducir_ty(db, ty),
    // TODO: Remove this once we monomorphize
    ReducIrTyKind::VarTy(_) => db.mk_medir_ty(MedIrTyKind::IntTy),
    ReducIrTyKind::ProdVarTy(_) => db.mk_medir_ty(MedIrTyKind::IntTy),
    ReducIrTyKind::CoprodVarTy(_) => db.mk_medir_ty(MedIrTyKind::IntTy),
  }
}

impl<'a> LowerCtx<'a> {
  pub(crate) fn new(
    db: &'a dyn crate::Db,
    current: ReducIrTermName,
    var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
  ) -> Self {
    Self {
      db,
      current,
      var_converter,
      lifts: vec![],
    }
  }

  pub(crate) fn lower_item(&mut self, reducir: &ReducIr) -> medir::Defn {
    let mut binds = vec![];
    let (params, body) = match reducir.try_top_level_def() {
      Ok(top_level) => {
        let params = top_level
          .vars
          .iter()
          .map(|var| {
            MedIrVar::new(
              self.var_converter.convert(var.var),
              from_reducir_ty(self.db, var.ty),
            )
          })
          .collect();
        (params, self.lower_binds(top_level.body, &mut binds))
      }
      Err(body) => (vec![], self.lower_binds(body, &mut binds)),
    };
    medir::Defn {
      name: MedIrItemName::new(self.current),
      params,
      body: medir::Locals { binds, body },
    }
  }

  fn mk_atom(&mut self, body: MedIr, binds: &mut Vec<(MedIrVar, MedIr)>) -> Atom {
    match body.kind {
      MedIrKind::Atom(atom) => atom,
      _ => {
        let v = MedIrVar::new(self.var_converter.generate(), body.type_of(self.db));
        binds.push((v, body));
        Atom::Var(v)
      }
    }
  }

  fn lower_binds(&mut self, body: &ReducIr, binds: &mut Vec<(MedIrVar, MedIr)>) -> MedIr {
    match body.kind() {
      ReducIrKind::Int(i) => MedIr::int(*i),
      ReducIrKind::Var(v) => {
        let var = self.var_converter.convert(v.var);
        MedIr::var(MedIrVar::new(var, from_reducir_ty(self.db, v.ty)))
      }
      ReducIrKind::Item(occ) => MedIr::new(MedIrKind::Closure(
        medir::MedIrTypedItem::new(
          MedIrItemName::new(occ.name),
          from_reducir_ty(self.db, occ.ty),
        ),
        vec![],
      )),
      ReducIrKind::Abs(vars, body) => {
        let (name, free_vars) = self.closure_convert(vars, body);
        MedIr::new(MedIrKind::Closure(name, free_vars))
      }
      ReducIrKind::App(head, spine) => {
        let medir_spine = spine
          .iter()
          .map(|arg| {
            let medir = self.lower_binds(arg, binds);
            match &medir.kind {
              MedIrKind::Atom(atom) => *atom,
              _ => {
                let var = MedIrVar::new(self.var_converter.generate(), medir.type_of(self.db));
                binds.push((var, medir));
                medir::Atom::Var(var)
              }
            }
          })
          .collect();
        let med_head = self.lower_binds(head, binds);
        MedIr::new(match med_head.kind {
          MedIrKind::Atom(medir::Atom::Var(v)) => {
            MedIrKind::Call(medir::Call::Unknown(v), medir_spine)
          }
          MedIrKind::Closure(item, args) => {
            let (item_arg_tys, ret) = item
              .ty
              .try_as_fun_ty(self.db)
              .expect("Medir item should have function type when applied");
            let arity = item_arg_tys.len();
            let args = args
              .into_iter()
              .map(medir::Atom::Var)
              .chain(medir_spine)
              .collect::<Vec<_>>();
            match args.len().cmp(&arity) {
              // Call isn't saturated return another closure
              std::cmp::Ordering::Less => {
                let args = args
                  .into_iter()
                  .map(|atom| match atom {
                    Atom::Var(v) => v,
                    Atom::Int(i) => {
                      let v = MedIrVar::new(
                        self.var_converter.generate(),
                        self.db.mk_medir_ty(MedIrTyKind::IntTy),
                      );
                      binds.push((v, MedIr::new(MedIrKind::Atom(Atom::Int(i)))));
                      v
                    }
                  })
                  .collect();
                MedIrKind::Closure(item, args)
              }
              // Call is saturated return result
              std::cmp::Ordering::Equal => MedIrKind::Call(medir::Call::Known(item), args),
              // Call is oversaturated allocate closure and apply
              std::cmp::Ordering::Greater => {
                let var = MedIrVar::new(self.var_converter.generate(), ret);
                let mut args = args;
                let oversat_args = args.split_off(arity);
                binds.push((
                  var,
                  MedIr::new(MedIrKind::Call(medir::Call::Known(item), args)),
                ));
                MedIrKind::Call(medir::Call::Unknown(var), oversat_args)
              }
            }
          }
          kind => {
            let medir = MedIr::new(kind);
            let v = MedIrVar::new(self.var_converter.generate(), medir.type_of(self.db));
            binds.push((v, medir));
            MedIrKind::Call(medir::Call::Unknown(v), medir_spine)
          }
        })
      }
      ReducIrKind::Locals(reducir_binds, body) => {
        for bind in reducir_binds.iter() {
          let defn = self.lower_binds(&bind.defn, binds);
          binds.push((
            MedIrVar::new(
              self.var_converter.convert(bind.var.var),
              defn.type_of(self.db),
            ),
            defn,
          ));
        }
        self.lower_binds(body, binds)
      }
      // We get rid of types in this IR
      ReducIrKind::TyAbs(_, body) | ReducIrKind::TyApp(body, _) => self.lower_binds(body, binds),
      ReducIrKind::Struct(elems) => {
        // TODO: Flatten?
        MedIr::new(MedIrKind::Blocks(
          elems
            .iter()
            .map(|elem| {
              let e = self.lower_binds(elem, binds);
              self.mk_atom(e, binds)
            })
            .collect(),
        ))
      }
      ReducIrKind::FieldProj(indx, body) => {
        let base = self.lower_binds(body, binds);
        let base_var = match self.mk_atom(base, binds) {
          Atom::Var(v) => v,
          _ => unreachable!(),
        };
        MedIr::new(MedIrKind::BlockAccess(base_var, *indx))
      }
      ReducIrKind::Tag(reducir_ty, tag, body) => {
        // TODO: Flatten?
        let medir_ty = from_reducir_ty(self.db, *reducir_ty);
        let value = self.lower_binds(body, binds);
        let value_atom = self.mk_atom(value, binds);
        MedIr::new(MedIrKind::Typecast(
          medir_ty,
          Box::new(MedIr::new(MedIrKind::Blocks(vec![
            Atom::Int(*tag),
            value_atom,
          ]))),
        ))
      }
      ReducIrKind::Case(_, scrutinee, branches) => {
        let medir_scrutinee = self.lower_binds(scrutinee, binds);
        let scrutinee_var = match self.mk_atom(medir_scrutinee, binds) {
          Atom::Var(v) => v,
          Atom::Int(_) => unreachable!(),
        };
        let discr_var = MedIrVar::new(
          self.var_converter.generate(),
          self.db.mk_medir_ty(MedIrTyKind::IntTy),
        );
        binds.push((
          discr_var,
          MedIr::new(MedIrKind::BlockAccess(scrutinee_var, 0)),
        ));
        let cases = branches
          .iter()
          .map(|branch| {
            match branch.kind() {
              ReducIrKind::Abs(vars, body) => {
                let branch_var = MedIrVar::new(
                  self.var_converter.convert(vars[0].var),
                  from_reducir_ty(self.db, vars[0].ty),
                );
                let scrutinee_value = MedIr::new(MedIrKind::BlockAccess(scrutinee_var, 1));
                //TODO: Handle flattening?
                let mut binds = vec![(branch_var, scrutinee_value)];
                let body = self.lower_binds(body, &mut binds);
                medir::Locals { binds, body }
              }
              _ => unreachable!(),
            }
          })
          .collect();
        MedIr::new(MedIrKind::Switch(Atom::Var(discr_var), cases))
      }
      ReducIrKind::Coerce(ty, body) => {
        let medir = self.lower_binds(body, binds);
        MedIr::new(MedIrKind::Typecast(
          from_reducir_ty(self.db, *ty),
          Box::new(medir),
        ))
      }
      ReducIrKind::X(_) => unreachable!(),
    }
  }

  fn closure_convert(
    &mut self,
    vars: &[ReducIrVar],
    body: &ReducIr,
  ) -> (MedIrTypedItem, Vec<MedIrVar>) {
    let mut free_vars = body
      .free_var_set()
      .into_iter()
      .filter(|var| !vars.contains(var))
      .map(|var| {
        MedIrVar::new(
          self.var_converter.convert(var.var),
          from_reducir_ty(self.db, var.ty),
        )
      })
      .collect::<Vec<_>>();
    free_vars.sort();

    let params = free_vars
      .iter()
      .copied()
      .chain(vars.iter().copied().map(|var| {
        MedIrVar::new(
          self.var_converter.convert(var.var),
          from_reducir_ty(self.db, var.ty),
        )
      }))
      .collect::<Vec<_>>();
    let param_tys = params.iter().map(|var| var.ty).collect();
    let mut binds = vec![];

    let ty = body
      .type_check(self.db.as_reducir_db())
      .map_err_pretty_with(self.db)
      .expect("ReducIR type_check should succeed in lower-medir");
    let body = self.lower_binds(body, &mut binds);

    let num = self.lifts.len();
    let current_name = self.current.name(self.db).text(self.db.as_core_db());
    let module = self.current.module(self.db);
    let lift_name = ReducIrTermName::gen(self.db, format!("{}_lam_{}", current_name, num), module);
    let name = MedIrItemName::new(lift_name);
    self.lifts.push(medir::Defn {
      name,
      params,
      body: Locals { binds, body },
    });
    let item_ty = self
      .db
      .mk_medir_ty(MedIrTyKind::FunTy(param_tys, from_reducir_ty(self.db, ty)));
    (MedIrTypedItem::new(name, item_ty), free_vars)
  }
}
