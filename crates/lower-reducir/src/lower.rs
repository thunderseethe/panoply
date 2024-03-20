use ast::{Ast, Direction, Term};
use base::{
  id::{ReducIrTyVarId, ReducIrVarId, TermName, TyVarId, VarId},
  id_converter::IdConverter,
  modules::Module,
  pretty::{PrettyErrorWithDb, PrettyWithCtx},
};
use la_arena::Idx;
use reducir::{
  ty::{
    Kind, MkReducIrTy, ReducIrRow, ReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrTyKind::*,
    ReducIrVarTy, RowReducIrKind,
  },
  DelimCont, DelimReducIr, ReducIr, ReducIrKind,
  ReducIrKind::*,
  ReducIrLocal, ReducIrTermName, ReducIrVar, TypeCheck, P,
};
use rustc_hash::FxHashMap;
use tc::{EffectInfo, OpSelector, TyChkRes};
use ty::{
  row::{Row, RowOps, RowSema, Scoped, ScopedClosedRow, Simple, SimpleClosedRow},
  AccessTy, Evidence, InDb, MkTy, RowFields, Ty, TyScheme, TypeKind, Wrapper,
};

use crate::{
  evidence::{EvidenceMap, PartialEv},
  lower_row_ev, ReducIrEffectInfo, ReducIrRowEv,
};

/// Unwrap a type into it a product and return the product's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_prod_ty<'a, A: ?Sized + AccessTy<'a, InDb>>(db: &A, ty: Ty) -> Row<Simple> {
  ty.try_as_prod_row(db).unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type into a sum and return the sum's row.
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_sum_ty<'a>(db: &(impl ?Sized + AccessTy<'a, InDb>), ty: Ty) -> Row<Simple> {
  ty.try_as_sum_row(db).unwrap_or_else(|_| unreachable!())
}

/// Unwrap a type as a branch type, returning the row of the branch.
/// A branch type is a `FunTy(SumTy(row), VarTy(_))` and is used as the argument to branch
/// statements. We return the `row` from that type
///
/// Because we are lowering from a type checked AST we would've failed with a type error already if
/// this operation would fail.
fn expect_branch_ty<'a>(db: &(impl ?Sized + AccessTy<'a, InDb>), ty: Ty) -> Row<Simple> {
  ty.try_as_fn_ty(db)
    .and_then(|(arg, _, _)| arg.try_as_sum_row(db))
    .unwrap_or_else(|_| unreachable!())
}

pub trait ItemWrappers {
  fn lookup_wrapper(&self, term_name: TermName, term: Idx<Term<VarId>>) -> &Wrapper;
}

pub trait ItemSchemes {
  fn lookup_scheme(&self, term: TermName) -> TyScheme;
}
pub trait VarTys {
  fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty;
}
pub trait TermTys {
  fn lookup_term(&self, term_name: TermName, term: Idx<Term<VarId>>) -> TyChkRes<InDb>;
}

// TODO: Wip name
pub(crate) struct Evidenceless;
pub(crate) struct Evidentfull;

pub(crate) struct LowerCtx<'a, 'b, State = Evidenceless> {
  db: &'a dyn crate::Db,
  current: ReducIrTermName,
  var_conv: &'b mut IdConverter<VarId, ReducIrVarId>,
  op_sel: &'a FxHashMap<Idx<Term<VarId>>, OpSelector>,
  ty_ctx: LowerTyCtx<'a, 'b>,
  ev_map: EvidenceMap,
  _marker: std::marker::PhantomData<State>,
}

pub(crate) struct LowerTySchemeCtx<'a, 'b> {
  db: &'a dyn crate::Db,
  tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
}
impl<'a, 'b> LowerTySchemeCtx<'a, 'b> {
  pub(crate) fn new(
    db: &'a dyn crate::Db,
    tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
  ) -> Self {
    Self { db, tyvar_conv }
  }

  fn create_ty_ctx(self, module: Module, tyvar_env: FxHashMap<TyVarId, i32>) -> LowerTyCtx<'a, 'b> {
    LowerTyCtx {
      db: self.db,
      module,
      tyvar_conv: self.tyvar_conv,
      tyvar_env,
    }
  }

  pub(crate) fn lower_scheme(
    self,
    module: Module,
    scheme: &TyScheme,
  ) -> (ReducIrTy, LowerTyCtx<'a, 'b>) {
    // We bind foralls in Type -> SimpleRow -> ScopedRow order
    // So we need to build our tyvar_env in reverse ordering so indices are correct
    let foralls = scheme
      .bound_ty
      .iter()
      .map(|tyvar| (*tyvar, Kind::Type))
      .chain(
        scheme
          .bound_eff_row
          .iter()
          .map(|tyvar| (*tyvar, Kind::ScopedRow)),
      )
      .chain(
        scheme
          .bound_data_row
          .iter()
          .map(|tyvar| (*tyvar, Kind::SimpleRow)),
      )
      .collect::<Vec<_>>();
    let tyvar_env = foralls
      .iter()
      .rev()
      .enumerate()
      .map(|(i, (tyvar, _))| (*tyvar, i as i32))
      .collect::<FxHashMap<_, _>>();

    let mut ty_ctx: LowerTyCtx<'a, 'b> = self.create_ty_ctx(module, tyvar_env);

    let ir_ty = ty_ctx.lower_ty(scheme.ty);

    // Add parameter to type for each constraint
    let constrs_ty = ty_ctx.db.mk_fun_ty(
      scheme
        .constrs
        .iter()
        .map(|constr| ty_ctx.row_evidence_ir_ty(constr)),
      ir_ty,
    );

    let ir_ty: ReducIrTy = foralls.into_iter().rfold(constrs_ty, |ir_ty, (_, kind)| {
      ty_ctx.db.mk_reducir_ty(ForallTy(kind, ir_ty))
    });
    // Add each type variable around type
    (ir_ty, ty_ctx)
  }
}

/// A single row is represented by two types in ReducIr
/// The row's product type, and it's coproduct type.
pub(crate) struct RowEvIrTy {
  pub(crate) prod: ReducIrTy,
  pub(crate) coprod: ReducIrTy,
}

// This exists as a standalone struct so we can construct it and lower types without having to
// construct a full LowerCtx
pub(crate) struct LowerTyCtx<'a, 'b> {
  db: &'a dyn crate::Db,
  module: Module,
  tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
  /// Maps type variables to debruijn index
  tyvar_env: FxHashMap<TyVarId, i32>,
}
impl<'a, 'b> LowerTyCtx<'a, 'b> {
  pub(crate) fn new(
    db: &'a dyn crate::Db,
    module: Module,
    tyvar_conv: &'b mut IdConverter<TyVarId, ReducIrTyVarId>,
    tyvar_env: FxHashMap<TyVarId, i32>,
  ) -> Self {
    Self {
      db,
      module,
      tyvar_conv,
      tyvar_env,
    }
  }

  fn row_ir_tys<Sema: RowReducrIrEvidence>(&mut self, row: &Row<Sema>) -> RowEvIrTy
  where
    Self: RowVarConvert<Sema>,
    Sema::Open<InDb>: Copy,
  {
    match row {
      Row::Open(row_var) => {
        let var = self.lookup_row_var(row_var);
        RowEvIrTy {
          prod: self.db.mk_reducir_ty(ProdVarTy(var)),
          coprod: self.db.mk_reducir_ty(CoprodVarTy(var)),
        }
      }
      Row::Closed(row) => {
        let elems = row
          .values(self.db)
          .iter()
          .map(|ty| self.lower_ty(*ty))
          .collect::<Vec<_>>();
        // Unwrap singleton rows
        RowEvIrTy {
          prod: self.db.mk_prod_ty(elems.clone()),
          coprod: self.db.mk_coprod_ty(elems),
        }
      }
    }
  }

  fn row_evidence_ir_ty(&mut self, ev: &Evidence) -> ReducIrTy {
    let (left, right, goal) = match ev {
      Evidence::DataRow { left, right, goal } => (
        self.row_ir_tys(left),
        self.row_ir_tys(right),
        self.row_ir_tys(goal),
      ),
      Evidence::EffRow { left, right, goal } => (
        self.eff_row_into_evv_ty(*left),
        self.eff_row_into_evv_ty(*right),
        self.eff_row_into_evv_ty(*goal),
      ),
    };

    let branch_var_ty = self.db.mk_reducir_ty(VarTy(0));

    self.db.mk_prod_ty_ref(&[
      self.db.mk_fun_ty([left.prod, right.prod], goal.prod),
      self.db.mk_reducir_ty(ForallTy(
        Kind::Type,
        // We introduce an internal forall here so we have to manually shift our types
        self.db.mk_fun_ty(
          [
            self
              .db
              .mk_fun_ty([left.coprod.shift(self.db, 1)], branch_var_ty),
            self
              .db
              .mk_fun_ty([right.coprod.shift(self.db, 1)], branch_var_ty),
            goal.coprod.shift(self.db, 1),
          ],
          branch_var_ty,
        ),
      )),
      self.db.mk_prod_ty(vec![
        self.db.mk_fun_ty([goal.prod], left.prod),
        self.db.mk_fun_ty([left.coprod], goal.coprod),
      ]),
      self.db.mk_prod_ty(vec![
        self.db.mk_fun_ty([goal.prod], right.prod),
        self.db.mk_fun_ty([right.coprod], goal.coprod),
      ]),
    ])
  }

  /// Lowers an effect row into it's corresponding reducir product type.
  /// The effect row stores a unit type for all effects, the reducir product type replaces these
  /// with the effect's handler type.
  fn eff_row_into_evv_ty(&mut self, eff: Row<Scoped>) -> RowEvIrTy {
    match eff {
      Row::Open(v) => RowEvIrTy {
        prod: self.db.mk_reducir_ty(ProdVarTy(self.tyvar_env[&v])),
        coprod: self.db.mk_reducir_ty(CoprodVarTy(self.tyvar_env[&v])),
      },
      Row::Closed(row) => {
        let elems = row
          .fields(self.db)
          .iter()
          .zip(row.values(self.db).iter())
          .map(|(eff_id, ret_ty)| {
            let eff = self
              .db
              .lookup_effect_by_name(self.module, *eff_id)
              .expect("Effect Ident had no associated effect in lowering");
            let (outer_eff, ret_ty) = ret_ty
              .try_as_eff_row_val(self.db)
              .expect("Effect row value should be {eff, ret}");
            // Am I worried about this? Yeah, I should say so.
            let outer_eff_ty = self.eff_row_into_evv_ty(outer_eff).prod;
            self
              .db
              .effect_handler_ir_ty(eff)
              .reduce_forall(self.db, ReducIrTyApp::Ty(outer_eff_ty))
              .reduce_forall(self.db, ReducIrTyApp::Ty(self.lower_ty(ret_ty)))
          })
          .collect::<Vec<_>>();
        RowEvIrTy {
          prod: self.db.mk_prod_ty(elems.clone()),
          coprod: self.db.mk_coprod_ty(elems),
        }
      }
    }
  }

  fn lower_row<Sema: RowReducIrKind>(&mut self, row: Row<Sema>) -> ReducIrRow
  where
    Self: RowVarConvert<Sema>,
  {
    match row {
      Row::Open(var) => ReducIrRow::Open(self.lookup_row_var(&var)),
      Row::Closed(row) => ReducIrRow::Closed(
        row
          .values(self.db)
          .iter()
          .map(|ty| self.lower_ty(*ty))
          .collect::<Vec<_>>(),
      ),
    }
  }

  fn lower_ty(&mut self, ty: Ty) -> ReducIrTy {
    match self.db.kind(&ty) {
      TypeKind::RowTy(row) => {
        // This is a hack. When we have singleton products it produces an unadorned label term
        // which is of RowTy.
        // TODO: Fix this up so we actually don't produce row types instead of pretending we
        // don't and casting them to product types.
        if row.len(self.db) == 1 {
          self.lower_ty(row.values(self.db)[0])
        } else {
          unreachable!()
        }
      }
      TypeKind::ErrorTy => unreachable!(),
      TypeKind::IntTy => self.db.mk_reducir_ty(ReducIrTyKind::IntTy),
      TypeKind::VarTy(var) => self
        .db
        .mk_reducir_ty(ReducIrTyKind::VarTy(self.tyvar_env[var])),
      TypeKind::FunTy(arg, eff, ret) => self.db.mk_reducir_ty(ReducIrTyKind::FunETy(
        self.lower_ty(*arg),
        self.eff_row_into_evv_ty(*eff).prod,
        self.lower_ty(*ret),
      )),
      TypeKind::SumTy(Row::Open(row_var)) => self
        .db
        .mk_reducir_ty(ReducIrTyKind::CoprodVarTy(self.tyvar_env[row_var])),
      TypeKind::ProdTy(Row::Open(row_var)) => self
        .db
        .mk_reducir_ty(ReducIrTyKind::ProdVarTy(self.tyvar_env[row_var])),
      TypeKind::ProdTy(Row::Closed(row)) => {
        let elems = row
          .values(self.db)
          .iter()
          .map(|ty| self.lower_ty(*ty))
          .collect::<Vec<_>>();
        self.db.mk_prod_ty(elems)
      }
      TypeKind::SumTy(Row::Closed(row)) => {
        let elems = row
          .values(self.db)
          .iter()
          .map(|ty| self.lower_ty(*ty))
          .collect::<Vec<_>>();
        self.db.mk_coprod_ty(elems)
      }
    }
  }
}
pub(crate) trait RowVarConvert<Sema: RowReducIrKind> {
  fn convert_row_var(&mut self, row_var: Sema::Open<InDb>) -> ReducIrTyVarId;
  fn lookup_row_var(&mut self, row_var: &Sema::Open<InDb>) -> i32;
}
impl RowVarConvert<Simple> for LowerTyCtx<'_, '_> {
  fn convert_row_var(&mut self, row_var: <Simple as RowSema>::Open<InDb>) -> ReducIrTyVarId {
    self.tyvar_conv.convert(row_var)
  }

  fn lookup_row_var(&mut self, row_var: &TyVarId) -> i32 {
    self.tyvar_env[row_var]
  }
}
impl RowVarConvert<Scoped> for LowerTyCtx<'_, '_> {
  fn convert_row_var(&mut self, row_var: <Scoped as RowSema>::Open<InDb>) -> ReducIrTyVarId {
    self.tyvar_conv.convert(row_var)
  }

  fn lookup_row_var(&mut self, row_var: &TyVarId) -> i32 {
    self.tyvar_env[row_var]
  }
}

impl<'a, 'b, S> MkReducIrTy for LowerCtx<'a, 'b, S> {
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

impl<'a, S> LowerCtx<'a, '_, S> {
  fn lookup_term(&self, term: Idx<Term<VarId>>) -> TyChkRes<InDb> {
    match self.current {
      ReducIrTermName::Term(name) => self.db.lookup_term(name, term),
      ReducIrTermName::Gen(_) => panic!("ICE: Called lookup term on a generated term"),
    }
  }

  fn lookup_var(&self, var_id: VarId) -> Ty {
    match self.current {
      ReducIrTermName::Term(name) => self.db.lookup_var(name, var_id),
      ReducIrTermName::Gen(_) => panic!("ICE: Called lookup var on a generated term"),
    }
  }

  fn lookup_wrapper(&self, term: Idx<Term<VarId>>) -> &'a Wrapper {
    match self.current {
      ReducIrTermName::Term(name) => self.db.lookup_wrapper(name, term),
      ReducIrTermName::Gen(_) => panic!("ICE: Called lookup wrapper on a generated term"),
    }
  }

  // TODO: Clean this up
  pub(crate) fn tyvar_conv(&mut self) -> &mut IdConverter<TyVarId, ReducIrTyVarId> {
    self.ty_ctx.tyvar_conv
  }

  pub(crate) fn evv_var(&mut self, ast: &Ast<VarId>, evv_var_id: ReducIrVarId) -> ReducIrVar {
    let term_infer = self.lookup_term(ast.root());
    ReducIrVar::new(
      ReducIrLocal {
        top_level: self.current,
        id: evv_var_id,
      },
      self.ty_ctx.eff_row_into_evv_ty(term_infer.eff).prod,
    )
  }
}

#[derive(Clone, Copy)]
pub(crate) enum RowIndx {
  Left(usize, Ty<InDb>),
  Right(usize, Ty<InDb>),
}

pub(crate) trait RowReducrIrEvidence: RowReducIrKind {
  fn merge<Db: ?Sized + crate::Db>(
    db: &Db,
    left: Self::Closed<InDb>,
    right: Self::Closed<InDb>,
  ) -> Box<[RowIndx]>;

  fn diff_left<Db: ?Sized + crate::Db>(
    db: &Db,
    goal: Self::Closed<InDb>,
    left: Self::Closed<InDb>,
  ) -> Box<[(usize, Ty<InDb>)]>;

  fn diff_right<Db: ?Sized + crate::Db>(
    db: &Db,
    goal: Self::Closed<InDb>,
    right: Self::Closed<InDb>,
  ) -> Box<[(usize, Ty<InDb>)]>;
}

impl RowReducrIrEvidence for Simple {
  fn merge<Db: ?Sized + crate::Db>(
    db: &Db,
    left: Self::Closed<InDb>,
    right: Self::Closed<InDb>,
  ) -> Box<[RowIndx]> {
    let left_fields = left.fields(db);
    let right_fields = right.fields(db);

    let left_indxs = left
      .values(db)
      .iter()
      .enumerate()
      .map(|(i, ty)| RowIndx::Left(i, *ty))
      .collect::<Vec<_>>();
    let right_indxs = right
      .values(db)
      .iter()
      .enumerate()
      .map(|(i, ty)| RowIndx::Right(i, *ty))
      .collect::<Vec<_>>();

    SimpleClosedRow::<InDb>::merge_rowlikes(
      (left_fields, left_indxs.as_slice()),
      (right_fields, right_indxs.as_slice()),
    )
    .unwrap_or_else(|_| unreachable!("ICE: Type checked simple rows should be disjoint"))
    .1
  }

  fn diff_left<Db: ?Sized + crate::Db>(
    db: &Db,
    goal: Self::Closed<InDb>,
    left: Self::Closed<InDb>,
  ) -> Box<[(usize, Ty<InDb>)]> {
    let goal_fields = goal.fields(db);
    let goal_indxs = goal
      .values(db)
      .iter()
      .copied()
      .enumerate()
      .collect::<Vec<_>>();
    SimpleClosedRow::<InDb>::difference_rowlikes((goal_fields, &goal_indxs), left.fields(db)).1
  }

  fn diff_right<Db: ?Sized + crate::Db>(
    db: &Db,
    goal: Self::Closed<InDb>,
    right: Self::Closed<InDb>,
  ) -> Box<[(usize, Ty<InDb>)]> {
    let goal_fields = goal.fields(db);
    let goal_indxs = goal
      .values(db)
      .iter()
      .copied()
      .enumerate()
      .collect::<Vec<_>>();
    SimpleClosedRow::<InDb>::difference_rowlikes((goal_fields, &goal_indxs), right.fields(db)).1
  }
}

impl RowReducrIrEvidence for Scoped {
  fn merge<Db: ?Sized + crate::Db>(
    db: &Db,
    left: Self::Closed<InDb>,
    right: Self::Closed<InDb>,
  ) -> Box<[RowIndx]> {
    let left_fields = left.fields(db);
    let right_fields = right.fields(db);

    let left_indxs = left
      .values(db)
      .iter()
      .enumerate()
      .map(|(i, ty)| RowIndx::Left(i, *ty))
      .collect::<Vec<_>>();
    let right_indxs = right
      .values(db)
      .iter()
      .enumerate()
      .map(|(i, ty)| RowIndx::Right(i, *ty))
      .collect::<Vec<_>>();

    ScopedClosedRow::<InDb>::merge_rowlikes(
      (left_fields, &left_indxs),
      (right_fields, &right_indxs),
    )
    .1
  }

  fn diff_left<Db: ?Sized + crate::Db>(
    db: &Db,
    goal: Self::Closed<InDb>,
    left: Self::Closed<InDb>,
  ) -> Box<[(usize, Ty<InDb>)]> {
    let goal_fields = goal.fields(db);
    let goal_indxs = goal
      .values(db)
      .iter()
      .copied()
      .enumerate()
      .collect::<Vec<_>>();

    ScopedClosedRow::<InDb>::diff_left_rowlikes((goal_fields, &goal_indxs), left.fields(db)).1
  }

  fn diff_right<Db: ?Sized + crate::Db>(
    db: &Db,
    goal: Self::Closed<InDb>,
    right: Self::Closed<InDb>,
  ) -> Box<[(usize, Ty<InDb>)]> {
    let goal_fields = goal.fields(db);
    let goal_indxs = goal
      .values(db)
      .iter()
      .copied()
      .enumerate()
      .collect::<Vec<_>>();

    ScopedClosedRow::<InDb>::diff_right_rowlikes((goal_fields, &goal_indxs), right.fields(db)).1
  }
}

impl<'a, 'b, S> LowerCtx<'a, 'b, S> {
  fn generate_local(&mut self) -> ReducIrLocal {
    ReducIrLocal {
      top_level: self.current,
      id: self.var_conv.generate(),
    }
  }

  pub(crate) fn row_evidence_ir<Sema: RowReducrIrEvidence>(
    &mut self,
    left: Sema::Closed<InDb>,
    right: Sema::Closed<InDb>,
    goal: Sema::Closed<InDb>,
  ) -> DelimReducIr
  where
    LowerTyCtx<'a, 'b>: RowVarConvert<Sema>,
    Sema::Closed<InDb>: Copy,
    Sema::Open<InDb>: Copy,
  {
    let left_ir = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(left));
    let right_ir = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(right));
    let goal_ir = self.ty_ctx.row_ir_tys(&Row::<Sema>::Closed(goal));

    let left_var_id = self.generate_local();
    let right_var_id = self.generate_local();
    let goal_var_id = self.generate_local();

    // Helpers to handle when we need to unwrap trivial single field structs
    let prj = |index, len, prod| {
      if len == 1 {
        prod
      } else {
        ReducIr::field_proj(index, prod)
      }
    };
    let inj = |index, len, ty, coprod| {
      if len == 1 {
        coprod
      } else {
        ReducIr::tag(ty, index, coprod)
      }
    };

    let indxs = Sema::merge(self.db, left, right);

    let left_len = left.len(self.db);
    let right_len = right.len(self.db);
    let goal_len = goal.len(self.db);
    debug_assert_eq!(left_len + right_len, goal_len);

    let concat = {
      let left_prod_var = ReducIrVar::new(left_var_id, left_ir.prod);
      let right_prod_var = ReducIrVar::new(right_var_id, right_ir.prod);
      ReducIr::abss([left_prod_var, right_prod_var], {
        let mut elems = indxs.iter().map(|indx| match indx {
          RowIndx::Left(i, _) => prj(*i, left_len, ReducIr::var(left_prod_var)),
          RowIndx::Right(i, _) => prj(*i, right_len, ReducIr::var(right_prod_var)),
        });
        if goal_len == 1 {
          elems.next().unwrap()
        } else {
          ReducIr::new(Struct(elems.collect()))
        }
      })
    };

    let branch = {
      let branch_tyvar = ReducIrVarTy {
        var: self.ty_ctx.tyvar_conv.generate(),
        kind: Kind::Type,
      };
      let branch_var_ty = self.mk_reducir_ty(VarTy(0));
      let ir_db = self.db.as_reducir_db();
      let left_coprod = left_ir.coprod.shift(self.db, 1);
      let right_coprod = right_ir.coprod.shift(self.db, 1);
      let left_branch_var =
        ReducIrVar::new(left_var_id, self.mk_fun_ty([left_coprod], branch_var_ty));
      let right_branch_var =
        ReducIrVar::new(right_var_id, self.mk_fun_ty([right_coprod], branch_var_ty));
      let goal_branch_var = ReducIrVar::new(goal_var_id, goal_ir.coprod.shift(ir_db, 1));
      ReducIr::ty_abs(
        [branch_tyvar],
        ReducIr::abss([left_branch_var, right_branch_var, goal_branch_var], {
          let case_var_id = self.generate_local();
          let mut elems = indxs.iter().map(|indx| {
            let (i, ty, coprod_ty, length, branch_var) = match indx {
              RowIndx::Left(i, ty) => (i, ty, left_coprod, left_len, left_branch_var),
              RowIndx::Right(i, ty) => (i, ty, right_coprod, right_len, right_branch_var),
            };

            let case_var = ReducIrVar::new(case_var_id, self.ty_ctx.lower_ty(*ty).shift(ir_db, 1));
            ReducIr::abss(
              [case_var],
              ReducIr::app(
                ReducIr::var(branch_var),
                [inj(*i, length, coprod_ty, ReducIr::var(case_var))],
              ),
            )
          });

          if indxs.len() == 1 {
            // Don't emit a case when we
            ReducIr::app(elems.next().unwrap(), [ReducIr::var(goal_branch_var)])
          } else {
            ReducIr::case_on_var(branch_var_ty, goal_branch_var, elems)
          }
        }),
      )
    };

    let goal_prod_var = ReducIrVar::new(goal_var_id, goal_ir.prod);

    let left_indxs = Sema::diff_right(self.db, goal, right);

    let prj_l = {
      ReducIr::abss([goal_prod_var], {
        let mut elems = left_indxs
          .iter()
          .map(|(i, _)| prj(*i, goal_len, ReducIr::var(goal_prod_var)));
        if left_len == 1 {
          elems.next().unwrap()
        } else {
          ReducIr::new(Struct(elems.collect()))
        }
      })
    };
    let inj_l = {
      let left_coprod_var = ReducIrVar::new(left_var_id, left_ir.coprod);
      ReducIr::abss([left_coprod_var], {
        let case_var_id = self.generate_local();
        let mut elems = left_indxs.iter().map(|(i, ty)| {
          let y = ReducIrVar::new(case_var_id, self.ty_ctx.lower_ty(*ty));
          ReducIr::abss([y], inj(*i, goal_len, goal_ir.coprod, ReducIr::var(y)))
        });
        if left_len == 1 {
          ReducIr::app(elems.next().unwrap(), [ReducIr::var(left_coprod_var)])
        } else {
          ReducIr::case_on_var(goal_ir.coprod, left_coprod_var, elems)
        }
      })
    };

    let right_indxs = Sema::diff_left(self.db, goal, left);
    let prj_r = {
      ReducIr::abss([goal_prod_var], {
        let mut elems = right_indxs
          .iter()
          .map(|(i, _)| prj(*i, goal_len, ReducIr::var(goal_prod_var)));
        if right_len == 1 {
          elems.next().unwrap()
        } else {
          ReducIr::new(Struct(elems.collect()))
        }
      })
    };
    let inj_r = {
      let right_coprod_var = ReducIrVar::new(right_var_id, right_ir.coprod);
      ReducIr::abss([right_coprod_var], {
        let case_var_id = self.generate_local();
        let mut elems = right_indxs.iter().map(|(i, ty)| {
          let y = ReducIrVar::new(case_var_id, self.ty_ctx.lower_ty(*ty));
          ReducIr::abss([y], inj(*i, goal_len, goal_ir.coprod, ReducIr::var(y)))
        });
        if right_len == 1 {
          ReducIr::app(elems.next().unwrap(), [ReducIr::var(right_coprod_var)])
        } else {
          ReducIr::case_on_var(goal_ir.coprod, right_coprod_var, elems)
        }
      })
    };

    ReducIr::new(Struct(vec![
      concat,
      branch,
      ReducIr::new(Struct(vec![prj_l, inj_l])),
      ReducIr::new(Struct(vec![prj_r, inj_r])),
    ]))
  }
}

type LowerOutput<'a, 'b> = (
  LowerCtx<'a, 'b, Evidentfull>,
  Vec<(ReducIrVar, DelimReducIr)>,
  Vec<ReducIrVar>,
  Vec<ReducIrRowEv>,
);

impl<'a, 'b> LowerCtx<'a, 'b, Evidenceless> {
  pub(crate) fn new(
    db: &'a dyn crate::Db,
    var_conv: &'b mut IdConverter<VarId, ReducIrVarId>,
    ty_ctx: LowerTyCtx<'a, 'b>,
    op_sel: &'a FxHashMap<Idx<Term<VarId>>, OpSelector>,
    current: ReducIrTermName,
  ) -> Self {
    Self {
      db,
      current,
      var_conv,
      ty_ctx,
      op_sel,
      ev_map: EvidenceMap::default(),
      _marker: std::marker::PhantomData,
    }
  }

  pub(crate) fn collect_evidence_params<'ev>(
    mut self,
    ev_iter: impl Iterator<Item = &'ev Evidence>,
  ) -> LowerOutput<'a, 'b> {
    let ty_db = self.db.as_ty_db();
    let mut evs = ev_iter.collect::<Vec<_>>();

    evs.sort();
    let mut ev_items = vec![];
    let mut solved = vec![];
    let mut params = vec![];
    for ev in evs {
      let param = self.lower_evidence(ev);
      self.ev_map.insert(*ev, param);
      let (ir_item, ty_vals_iter) = match ev {
        Evidence::DataRow {
          left: Row::Closed(left),
          right: Row::Closed(right),
          goal: Row::Closed(goal),
        } => {
          let ir_row_ev = lower_row_ev(
            self.db,
            self.current.module(self.db),
            left.raw_fields(),
            right.raw_fields(),
            goal.raw_fields(),
          );
          ev_items.push(ir_row_ev);
          (
            ir_row_ev.simple(self.db.as_reducir_db()),
            left
              .values(ty_db)
              .iter()
              .chain(right.values(ty_db))
              .map(|ty| self.ty_ctx.lower_ty(*ty))
              .collect::<Vec<_>>(),
          )
        }
        Evidence::EffRow {
          left: Row::Closed(left),
          right: Row::Closed(right),
          goal: Row::Closed(goal),
        } => {
          let ir_row_ev = lower_row_ev(
            self.db,
            self.current.module(self.db),
            left.raw_fields(),
            right.raw_fields(),
            goal.raw_fields(),
          );
          ev_items.push(ir_row_ev);
          let left_row_iter = left.fields(ty_db).iter().zip(left.values(ty_db).iter());
          let right_row_iter = right.fields(ty_db).iter().zip(right.values(ty_db).iter());
          let ty_vals = left_row_iter
            .chain(right_row_iter)
            .map(|(eff_label, eff_ret_ty)| {
              let eff = self
                .db
                .lookup_effect_by_name(self.current.module(self.db), *eff_label)
                .expect("Effect Ident had no associated effect in lowering");
              let (outer_eff, ret_ty) = eff_ret_ty.try_as_eff_row_val(self.db).unwrap();

              let outer_eff_ty = self.ty_ctx.eff_row_into_evv_ty(outer_eff).prod;
              self
                .db
                .effect_handler_ir_ty(eff)
                .reduce_forall(self.db, ReducIrTyApp::Ty(outer_eff_ty))
                .reduce_forall(self.db, ReducIrTyApp::Ty(self.ty_ctx.lower_ty(ret_ty)))
            })
            .collect::<Vec<_>>();
          (ir_row_ev.scoped(self.db.as_reducir_db()), ty_vals)
        }
        _ => {
          params.push(param);
          // Not a solved evidence so don't push an IR term
          continue;
        }
      };
      let item = ir_item.item(self.db.as_reducir_db());
      let ir_ty = item
        .type_check(self.db.as_reducir_db())
        .map_err_pretty_with(self.db)
        .expect("ICE: Generated effect row evidence didn't type check");
      let ir = ReducIr::new(ReducIrKind::item(
        ReducIrTermName::Gen(ir_item.name(self.db.as_reducir_db())),
        ir_ty,
      ));

      let ir = ReducIr::app(
        ReducIr::ty_app(ir, ty_vals_iter.into_iter().map(ReducIrTyApp::Ty)),
        [ReducIr::new(Struct(vec![]))],
      );
      solved.push((param, ir));
    }

    (LowerCtx::with_evidenceless(self), solved, params, ev_items)
  }

  fn lower_evidence(&mut self, ev: &Evidence) -> ReducIrVar {
    let ev_term = self.generate_local();
    let row_ev_ty = self.ty_ctx.row_evidence_ir_ty(ev);
    ReducIrVar::new(ev_term, row_ev_ty)
  }
}

impl<'a, 'b> LowerCtx<'a, 'b, Evidentfull> {
  fn with_evidenceless(prior: LowerCtx<'a, 'b, Evidenceless>) -> Self {
    Self {
      db: prior.db,
      current: prior.current,
      var_conv: prior.var_conv,
      ty_ctx: prior.ty_ctx,
      op_sel: prior.op_sel,
      ev_map: prior.ev_map,
      _marker: std::marker::PhantomData,
    }
  }

  fn apply_wrapper(&mut self, wrapper: &Wrapper, ir: DelimReducIr) -> DelimReducIr {
    // This needs to be done in the reverse order that we add our Abs and TyAbs when we lower
    let ir = ReducIr::ty_app(
      ir,
      wrapper
        .tys
        .iter()
        .map(|ty| ReducIrTyApp::Ty(self.ty_ctx.lower_ty(*ty))),
    );
    let ir = ReducIr::ty_app(
      ir,
      wrapper.eff_rows.iter().map(|row| {
        ReducIrTyApp::EffRow({
          let ty = self.ty_ctx.eff_row_into_evv_ty(*row).prod;
          match ty.kind(self.db.as_reducir_db()) {
            ProdVarTy(var) => ReducIrRow::Open(var),
            ProductTy(row) if row.is_empty() => ReducIrRow::Closed(vec![]),
            // If the first item of the row is a marker then the row is a handler,
            // not an evv and we should wrap it treat it as a type when passing to effect row.
            ProductTy(row) => match row[0].kind(self.db.as_reducir_db()) {
              MarkerTy(_) => ReducIrRow::Closed(vec![ty]),
              _ => ReducIrRow::Closed(row),
            },
            _ => unreachable!("Product of effrow must be product type"),
          }
        })
      }),
    );
    let ir = ReducIr::ty_app(
      ir,
      wrapper
        .data_rows
        .iter()
        .map(|row| ReducIrTyApp::DataRow(self.ty_ctx.lower_row(*row))),
    );
    let ir = ReducIr::app(
      ir,
      wrapper
        .constrs
        .iter()
        .map(|ev| ReducIr::var(self.ev_map[ev])),
    );
    ir
  }

  pub(crate) fn lower_term(
    &mut self,
    ast: &Ast<VarId>,
    term: Idx<Term<VarId>>,
    evv_var_id: ReducIrVarId,
  ) -> DelimReducIr {
    use Term::*;
    match ast.view(term) {
      Unit => ReducIr::new(Struct(vec![])),
      Abstraction { arg, body } => {
        let arg_ty = self.lookup_var(*arg);
        let ty = self.ty_ctx.lower_ty(arg_ty);
        let var = ReducIrVar::new(
          ReducIrLocal {
            top_level: self.current,
            id: self.var_conv.convert(*arg),
          },
          ty,
        );
        let body_infer = self.lookup_term(*body);
        ReducIr::ext(DelimCont::AbsE(
          var,
          self.ty_ctx.eff_row_into_evv_ty(body_infer.eff).prod,
          P::new(self.lower_term(ast, *body, evv_var_id)),
        ))
      }
      Application { func, arg } => {
        let mut func = *func;
        let mut args = vec![self.lower_term(ast, *arg, evv_var_id)];
        while let Application { func: next, arg } = ast.view(func) {
          args.push(self.lower_term(ast, *arg, evv_var_id));
          func = *next;
        }
        ReducIr::app(
          self.lower_term(ast, func, evv_var_id),
          args.into_iter().rev(),
        )
      }
      Variable(var) => {
        let ty = self.lookup_var(*var);
        ReducIr::var(ReducIrVar::new(
          ReducIrLocal {
            top_level: self.current,
            id: self.var_conv.convert(*var),
          },
          self.ty_ctx.lower_ty(ty),
        ))
      }
      Term::Int(i) => ReducIr::new(ReducIrKind::Int(*i)),
      Item(term_name) => {
        let wrapper = self.lookup_wrapper(term);
        let scheme = self
          .db
          .type_scheme_of(*term_name)
          .ty_scheme(self.db.as_tc_db());
        // Construct an ad hoc LowerTySchemeCtx to lower our scheme
        let ty_scheme_ctx = LowerTySchemeCtx::new(self.db, self.ty_ctx.tyvar_conv);
        let (ir_scheme, _) = ty_scheme_ctx.lower_scheme(self.current.module(self.db), &scheme);
        let ir = ReducIr::new(ReducIrKind::item(
          ReducIrTermName::Term(*term_name),
          ir_scheme,
        ));
        self.apply_wrapper(wrapper, ir)
      }
      // At this level Label/Unlabel are removed
      Label { term, .. } => self.lower_term(ast, *term, evv_var_id),
      Unlabel { term, .. } => self.lower_term(ast, *term, evv_var_id),
      // Row stuff
      Concat { left, right } => {
        let goal_row = expect_prod_ty(&self.db, self.lookup_term(term).ty);
        let left_row = expect_prod_ty(&self.db, self.lookup_term(*left).ty);
        let right_row = expect_prod_ty(&self.db, self.lookup_term(*right).ty);
        let ev = Evidence::DataRow {
          left: left_row,
          right: right_row,
          goal: goal_row,
        };
        let param = self.ev_map[&ev];
        let concat = ReducIr::field_proj(0, ReducIr::var(param));

        ReducIr::app(
          concat,
          [
            self.lower_term(ast, *left, evv_var_id),
            self.lower_term(ast, *right, evv_var_id),
          ],
        )
      }
      Branch { left, right } => {
        let left_row = expect_branch_ty(&self.db, self.lookup_term(*left).ty);
        let right_row = expect_branch_ty(&self.db, self.lookup_term(*right).ty);
        let goal_row = expect_branch_ty(&self.db, self.lookup_term(term).ty);

        let param = self.ev_map[&(Evidence::DataRow {
          left: left_row,
          right: right_row,
          goal: goal_row,
        })];
        let branch = ReducIr::field_proj(1, ReducIr::var(param));

        ReducIr::app(
          branch,
          [
            self.lower_term(ast, *left, evv_var_id),
            self.lower_term(ast, *right, evv_var_id),
          ],
        )
      }
      Project {
        direction,
        term: subterm,
      } => {
        let goal = expect_prod_ty(&self.db, self.lookup_term(*subterm).ty);
        let other = expect_prod_ty(&self.db, self.lookup_term(term).ty);

        let (param, full_ev) = self.ev_map[&PartialEv::Data { goal, other }];
        let idx = match full_ev {
          Evidence::DataRow { left, .. } => {
            if other == left {
              2
            } else {
              3
            }
          }
          _ => unreachable!(),
        };

        let prj = ReducIr::field_proj(0, ReducIr::field_proj(idx, ReducIr::var(param)));
        ReducIr::app(prj, [self.lower_term(ast, *subterm, evv_var_id)])
      }
      Inject {
        direction,
        term: subterm,
      } => {
        let goal = expect_sum_ty(&self.db, self.lookup_term(term).ty);
        let other = expect_sum_ty(&self.db, self.lookup_term(*subterm).ty);

        let (param, _) = self.ev_map[&PartialEv::Data { other, goal }];
        let idx = match direction {
          Direction::Left => 2,
          Direction::Right => 3,
        };

        let inj = ReducIr::field_proj(1, ReducIr::field_proj(idx, ReducIr::var(param)));
        ReducIr::app(inj, [self.lower_term(ast, *subterm, evv_var_id)])
      }
      // Effect stuff
      Operation(op) => {
        let term_infer = self.lookup_term(term);
        let (value_ty, _, op_ret) = term_infer
          .ty
          .try_as_fn_ty(&self.db)
          .unwrap_or_else(|_| unreachable!());
        let value_var = ReducIrVar::new(self.generate_local(), self.ty_ctx.lower_ty(value_ty));
        let eff = op.effect(self.db.as_core_db());

        let eff_name = eff.name(self.db.as_core_db());
        let (eff_vals, eff_ev_param, _) = self
          .ev_map
          .match_right_eff_ev(
            RowFields::new(self.db.as_ty_db(), vec![eff_name]),
            term_infer.eff,
          )
          .expect("Evidence for operation to exist");

        // We know this is safe because we looked up a RowFields with one field
        let (outer_eff, kont_ret_ty) = eff_vals
          .values(self.db.as_ty_db())
          .first()
          .unwrap()
          .try_as_eff_row_val(self.db)
          .unwrap();
        let kont_ret_ty = self.ty_ctx.lower_ty(kont_ret_ty);
        let outer_eff_ty = self.ty_ctx.eff_row_into_evv_ty(outer_eff).prod;
        let handle_var = ReducIrVar::new(
          self.generate_local(),
          self
            .db
            .effect_handler_ir_ty(eff)
            .reduce_forall(self.db, ReducIrTyApp::Ty(outer_eff_ty))
            .reduce_forall(self.db, ReducIrTyApp::Ty(kont_ret_ty)),
        );
        let op_ret = self.ty_ctx.lower_ty(op_ret);
        let kont_var = ReducIrVar::new(
          self.generate_local(),
          self.mk_reducir_ty(ReducIrTyKind::FunETy(op_ret, outer_eff_ty, kont_ret_ty)),
        );

        // Always project out the right one for row evidence because we want the innermost
        // scoped effect handler.
        let prj = ReducIr::field_proj(0, ReducIr::field_proj(3, ReducIr::var(eff_ev_param)));
        let eff_ty = self.ty_ctx.eff_row_into_evv_ty(term_infer.eff).prod;
        let eff_var = ReducIrVar::new(
          ReducIrLocal {
            top_level: self.current,
            id: evv_var_id,
          },
          eff_ty,
        );
        let eff_handler = ReducIr::app(prj, [ReducIr::var(eff_var)]);

        // TODO: How do we get rows here to determine this?
        let op_sel = &self.op_sel[&term];
        let (op_param, op_ev) = self.ev_map[&PartialEv::Data {
          other: op_sel.op_row,
          goal: op_sel.handler_row,
        }];
        let (handler_index, _) = match op_ev {
          Evidence::DataRow { left, right, .. } => {
            if left == op_sel.op_row {
              (2, 3)
            } else if right == op_sel.op_row {
              (3, 2)
            } else {
              unreachable!()
            }
          }
          Evidence::EffRow { .. } => unreachable!(),
        };
        let handler = ReducIr::field_proj(
          0,
          ReducIr::field_proj(handler_index, ReducIr::var(op_param)),
        );
        let handler = ReducIr::app(handler, [ReducIr::field_proj(1, ReducIr::var(handle_var))]);
        ReducIr::abss(
          [value_var],
          ReducIr::local(
            handle_var,
            eff_handler,
            ReducIr::ext(DelimCont::Yield {
              ret_ty: op_ret,
              marker: P::new(ReducIr::field_proj(0, ReducIr::var(handle_var))),
              body: P::new(ReducIr::abss(
                [kont_var],
                ReducIr::app(handler, [ReducIr::var(value_var), ReducIr::var(kont_var)]),
              )),
            }),
          ),
        )
      }
      Handle { handler, body } => {
        let handler_infer = self.lookup_term(*handler);

        let handler_row = match expect_prod_ty(&self.db, handler_infer.ty) {
          Row::Closed(row) => row,
          Row::Open(_) => {
            panic!("ICE: Handler should be solved to closed row during type checking")
          }
        };
        let ret_label = self.db.ident_str("return");
        let ret_idx = handler_row
          .fields(self.db)
          .binary_search(&ret_label)
          .unwrap_or_else(|_| panic!("ICE: Type checked handler should contain 'return' field"));
        let handler_ret_ty = handler_row.values(self.db)[ret_idx];
        let handler_ret_row = self.db.single_row(ret_label, handler_ret_ty);
        let (handler_ret_param, handler_ret_ev) = self.ev_map[&PartialEv::Data {
          other: Row::Closed(handler_ret_row),
          goal: Row::Closed(handler_row),
        }];

        let (handler_ret_idx, handler_sig_idx) = match handler_ret_ev {
          Evidence::DataRow { left, right, .. } => {
            if left == Row::Closed(handler_ret_row) {
              (2, 3)
            } else if right == Row::Closed(handler_ret_row) {
              (3, 2)
            } else {
              unreachable!()
            }
          }
          Evidence::EffRow { .. } => unreachable!(),
        };

        let handler_var = ReducIrVar::new(
          self.generate_local(),
          self.ty_ctx.lower_ty(handler_infer.ty),
        );
        let handler_prj_ret = ReducIr::app(
          ReducIr::field_proj(
            0,
            ReducIr::field_proj(handler_ret_idx, ReducIr::var(handler_ret_param)),
          ),
          [ReducIr::var(handler_var)],
        );
        let handler_prj_sig = ReducIr::app(
          ReducIr::field_proj(
            0,
            ReducIr::field_proj(handler_sig_idx, ReducIr::var(handler_ret_param)),
          ),
          [ReducIr::var(handler_var)],
        );
        let handler_ir = self.lower_term(ast, *handler, evv_var_id);

        let term_infer = self.lookup_term(term);

        let body_infer = self.lookup_term(*body);
        let eff_ev = self.ev_map[&Evidence::EffRow {
          left: term_infer.eff,
          right: handler_infer.eff,
          goal: body_infer.eff,
        }];
        let inner_evv_id = self.generate_local();
        let inner_evv_var = ReducIrVar::new(
          inner_evv_id,
          self.ty_ctx.eff_row_into_evv_ty(body_infer.eff).prod,
        );
        let body = self.lower_term(ast, *body, inner_evv_id.id);

        // Peel off one arg of handler_ret_ty because it will be applied to body.
        let handler_ret_ty = self
          .ty_ctx
          .lower_ty(handler_ret_ty)
          .drop_args(self.db.as_reducir_db(), 1)
          .expect("Handler return type must be a function");
        let prompt_var = ReducIrVar::new(
          self.generate_local(),
          self.mk_reducir_ty(MarkerTy(handler_ret_ty)),
        );
        let update_evv = {
          let outer_evv_var = ReducIrVar::new(
            self.generate_local(),
            self.ty_ctx.eff_row_into_evv_ty(term_infer.eff).prod,
          );
          ReducIr::abss(
            [outer_evv_var],
            ReducIr::app(
              ReducIr::field_proj(0, ReducIr::var(eff_ev)),
              [
                ReducIr::var(outer_evv_var),
                ReducIr::new(Struct(vec![ReducIr::var(prompt_var), handler_prj_sig])),
              ],
            ),
          )
        };
        let install_prompt = ReducIr::ext(DelimCont::NewPrompt(
          prompt_var,
          P::new(
            // Install prompt and wrap handler body in handler's return function
            ReducIr::ext(DelimCont::Prompt {
              marker: P::new(ReducIr::var(prompt_var)),
              upd_evv: P::new(update_evv),
              ret: P::new(handler_prj_ret),
              body: P::new(ReducIr::abss([inner_evv_var], body)),
            }),
          ),
        ));

        ReducIr::local(handler_var, handler_ir, install_prompt)
      }
      Annotated { term, .. } => {
        // We type checked so this is handled, we can just unwrap here.
        self.lower_term(ast, *term, evv_var_id)
      }
    }
  }
}
