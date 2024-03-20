use ast::{AstModule, AstTerm, Term};
use base::{
  id::{EffectName, Id, IdSupply, ReducIrVarId, TermName, TyVarId, VarId},
  id_converter::IdConverter,
  ident::Ident,
  modules::Module,
};
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};
use reducir::{
  mon::{MonReducIrGenItem, MonReducIrItem, MonReducIrModule, MonReducIrRowEv},
  ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyKind, ReducIrVarTy},
  Bind, GeneratedReducIrName, ReducIr, ReducIrGenItem, ReducIrItem,
  ReducIrKind::*,
  ReducIrLocal, ReducIrModule, ReducIrRowEv, ReducIrTermName, ReducIrVar, P,
};
use tc::EffectInfo;
use ty::{
  row::{Scoped, Simple},
  InDb, MkTy, RowFields, Ty, TyScheme, Wrapper,
};

use rustc_hash::FxHashMap;

use crate::lower::{LowerTyCtx, LowerTySchemeCtx};

use self::{
  lower::{ItemWrappers, RowReducrIrEvidence, RowVarConvert},
  lower_mon::LowerMonCtx,
};

pub(crate) mod evidence;

mod lower;
mod lower_mon;

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait ReducIrEffectInfo: EffectInfo {
  fn effect_handler_ir_ty(&self, effect: EffectName) -> ReducIrTy;
}

#[salsa::jar(db = Db)]
pub struct Jar(
  lower_module,
  lower_item,
  lower_row_ev,
  lower_mon_item,
  lower_mon_module,
  effect_handler_ir_ty,
);
pub trait Db: salsa::DbWithJar<Jar> + tc::Db + reducir::Db {
  fn as_lower_reducir_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }

  /*
   * AST -> ReducIr Lowering Methods
   */

  fn lower_reducir_module(&self, module: AstModule) -> ReducIrModule {
    lower_module(self.as_lower_reducir_db(), module)
  }

  fn lower_reducir_item(&self, ast_term: AstTerm) -> ReducIrItem {
    lower_item(self.as_lower_reducir_db(), ast_term)
  }

  fn reducir_var_supply(&self, term_name: TermName) -> &IdSupply<ReducIrVarId> {
    self
      .lower_reducir_item_of(term_name)
      .var_supply(self.as_reducir_db())
  }

  fn lower_reducir_item_of(&self, term_name: TermName) -> ReducIrItem {
    let ast_term = self.desugar_term_of(term_name);
    self.lower_reducir_item(ast_term)
  }

  fn lower_reducir_module_of(&self, module: Module) -> ReducIrModule {
    let ast_module = self.desugar_module_of(module);
    self.lower_reducir_module(ast_module)
  }

  fn lower_reducir_module_for_path(&self, path: std::path::PathBuf) -> ReducIrModule {
    let module = self.root_module_for_path(path);
    let ast_module = self.desugar_module_of(module);
    self.lower_reducir_module(ast_module)
  }

  fn lower_reducir_item_for_file_name(
    &self,
    path: std::path::PathBuf,
    item: Ident,
  ) -> Option<ReducIrItem> {
    let module = self.root_module_for_path(path);
    let term_name = self.id_for_name(module, item)?;
    Some(self.lower_reducir_item_of(term_name))
  }

  /*
   * ReducIr -> Monadic ReducIr Lowering Methods
   */

  fn lower_reducir_mon_module(&self, module: ReducIrModule) -> MonReducIrModule {
    lower_mon_module(self.as_lower_reducir_db(), module)
  }

  fn lower_reducir_mon_module_of(&self, module: Module) -> MonReducIrModule {
    let reducir_module = self.lower_reducir_module_of(module);
    self.lower_reducir_mon_module(reducir_module)
  }

  fn lower_reducir_mon_item(&self, item: ReducIrItem) -> MonReducIrItem {
    lower_mon_item(self.as_lower_reducir_db(), item)
  }

  fn lower_reducir_mon_item_of(&self, name: TermName) -> MonReducIrItem {
    let item = self.lower_reducir_item_of(name);
    self.lower_reducir_mon_item(item)
  }

  fn lower_reducir_mon_item_for_file_name(
    &self,
    path: std::path::PathBuf,
    item: Ident,
  ) -> Option<MonReducIrItem> {
    let module = self.root_module_for_path(path);
    let term_name = self.id_for_name(module, item)?;
    Some(self.lower_reducir_mon_item_of(term_name))
  }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + tc::Db + reducir::Db {}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: AstModule) -> ReducIrModule {
  let ast_db = db.as_ast_db();
  let items = module
    .terms(ast_db)
    .iter()
    .map(|term| lower_item(db, *term))
    .collect();
  ReducIrModule::new(db.as_reducir_db(), module.module(ast_db), items)
}

#[salsa::tracked]
fn lower_row_ev(
  db: &dyn crate::Db,
  module: Module,
  left: RowFields,
  right: RowFields,
  goal: RowFields,
) -> ReducIrRowEv {
  ReducIrRowEv::new(
    db.as_reducir_db(),
    lower_row_ev_item::<Simple>(db, module, "simple", left, right, goal),
    lower_row_ev_item::<Scoped>(db, module, "scoped", left, right, goal),
  )
}

fn lower_row_ev_item<Sema: RowReducrIrEvidence>(
  db: &dyn crate::Db,
  module: Module,
  mark: &str,
  left: RowFields,
  right: RowFields,
  goal: RowFields,
) -> ReducIrGenItem
where
  for<'a, 'b> LowerTyCtx<'a, 'b>: RowVarConvert<Sema>,
  Sema::Closed<InDb>: Copy,
  Sema::Open<InDb>: Copy,
{
  let ty_db = db.as_ty_db();
  let left_fields = left.fields(ty_db);
  let left_field_str: String = left_fields
    .iter()
    .map(|ident| ident.text(db.as_core_db()).as_str())
    .collect();
  let right_fields = right.fields(ty_db);
  let right_field_str: String = right_fields
    .iter()
    .map(|ident| ident.text(db.as_core_db()).as_str())
    .collect();
  let row_ev_ident = db.ident(format!(
    "_row_{}_{}_{}",
    mark, left_field_str, right_field_str
  ));
  let row_ev_name = GeneratedReducIrName::new(db.as_reducir_db(), row_ev_ident, module);

  let mut id_count: i32 = -1;
  let mut left_values = left_fields
    .iter()
    .map(|_| {
      id_count += 1;
      id_count
    })
    .map(|id| TyVarId::from_raw(id.try_into().unwrap()))
    .collect::<Vec<_>>();
  let right_values = right_fields
    .iter()
    .map(|_| {
      id_count += 1;
      id_count
    })
    .map(|id| TyVarId::from_raw(id.try_into().unwrap()))
    .collect::<Vec<_>>();

  let left_row = db.mk_row_iter::<Sema::Closed<InDb>>(
    left_fields.iter().copied(),
    left_values
      .iter()
      .map(|id| db.mk_ty(ty::TypeKind::VarTy(*id))),
  );
  let right_row = db.mk_row_iter::<Sema::Closed<InDb>>(
    right_fields.iter().copied(),
    right_values
      .iter()
      .map(|id| db.mk_ty(ty::TypeKind::VarTy(*id))),
  );

  let goal_row_indices = Sema::merge(db, left_row, right_row);
  let goal_values = goal_row_indices
    .iter()
    .map(|indx| match indx {
      lower::RowIndx::Left(_, ty) => *ty,
      lower::RowIndx::Right(_, ty) => *ty,
    })
    .collect::<Vec<_>>();

  let goal_row =
    db.mk_row::<Sema::Closed<InDb>>(goal.fields(ty_db).as_slice(), goal_values.as_slice());

  left_values.extend(right_values);
  let order_of_tys = left_values;
  let tyvar_env = order_of_tys
    .iter()
    .zip((0..(left_fields.len() + right_fields.len())).rev())
    .map(|(ty, i)| (*ty, i as i32))
    .collect::<FxHashMap<_, _>>();

  let mut var_conv = IdConverter::new();
  let mut tyvar_conv = IdConverter::new();
  let op_sel = FxHashMap::default();
  let mut lower_ctx = LowerCtx::new(
    db,
    &mut var_conv,
    LowerTyCtx::new(db, module, &mut tyvar_conv, tyvar_env),
    &op_sel,
    ReducIrTermName::Gen(row_ev_name),
  );
  let ir = lower_ctx.row_evidence_ir::<Sema>(left_row, right_row, goal_row);

  let unit_param = ReducIrVar::new(
    ReducIrLocal {
      top_level: ReducIrTermName::Gen(row_ev_name),
      id: ReducIrVarId::from_raw(0),
    },
    db.mk_prod_ty(vec![]),
  );
  let ir = ReducIr::ty_abs(
    order_of_tys.into_iter().map(|var| ReducIrVarTy {
      var: tyvar_conv.convert(var),
      kind: Kind::Type,
    }),
    ReducIr::abss([unit_param], ir),
  );

  ReducIrGenItem::new(db.as_reducir_db(), row_ev_name, ir)
}

/// Lower an `Ast` into an `ReducIr`.
/// TODO: Real documentation.
#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term: AstTerm) -> ReducIrItem {
  let ast_db = db.as_ast_db();
  let name = term.name(ast_db);
  let ast = term.data(ast_db);
  let typed_item = db.type_scheme_of(name);
  let tc_db = db.as_tc_db();
  let mut var_conv = IdConverter::new();
  let mut tyvar_conv = IdConverter::new();
  let scheme = typed_item.ty_scheme(db.as_tc_db());

  let (_, ty_ctx) =
    LowerTySchemeCtx::new(db, &mut tyvar_conv).lower_scheme(name.module(db.as_core_db()), &scheme);

  let required_evidence = typed_item.required_evidence(tc_db);
  let op_sel = typed_item.operation_selectors(tc_db);
  let evv_var_id = var_conv.generate();
  let (mut lower_ctx, ev_solved, ev_params, ev_row_items) = LowerCtx::new(
    db,
    &mut var_conv,
    ty_ctx,
    op_sel,
    ReducIrTermName::Term(name),
  )
  .collect_evidence_params(required_evidence.iter());

  let body = lower_ctx.lower_term(ast, ast.root(), evv_var_id);
  // TODO: Bit of a hack. Eventually we'd like to generate our solved row ev in a central location.
  // Add row evidence as parameters of the term
  let body = body.map_within_abss(|body| {
    ReducIr::locals(
      ev_solved
        .into_iter()
        .map(|(var, defn)| Bind::new(var, defn)),
      body,
    )
  });
  // Wrap our term in any unsolved row evidence params we need
  let is_entry_point = name.name(db.as_core_db()) == db.ident_str("main");
  let evv_param = if is_entry_point {
    vec![]
  } else {
    let evv_var = lower_ctx.evv_var(ast, evv_var_id);
    vec![evv_var]
  };
  let body = ReducIr::abss_with_innermost(ev_params.into_iter(), evv_param, body);

  // Finally wrap our term in any type/row variables it needs to bind
  let body = scheme
    .bound_data_row
    .iter()
    .rfold(body, |acc, simple_row_var| {
      ReducIr::new(TyAbs(
        ReducIrVarTy {
          var: lower_ctx.tyvar_conv().convert(*simple_row_var),
          kind: Kind::SimpleRow,
        },
        P::new(acc),
      ))
    });
  let body = scheme
    .bound_eff_row
    .iter()
    .rfold(body, |acc, scoped_row_var| {
      ReducIr::new(TyAbs(
        ReducIrVarTy {
          var: lower_ctx.tyvar_conv().convert(*scoped_row_var),
          kind: Kind::ScopedRow,
        },
        P::new(acc),
      ))
    });
  let ir = scheme.bound_ty.iter().rfold(body, |acc, ty_var| {
    ReducIr::new(TyAbs(
      ReducIrVarTy {
        var: lower_ctx.tyvar_conv().convert(*ty_var),
        kind: Kind::Type,
      },
      P::new(acc),
    ))
  });

  ReducIrItem::new(
    db.as_reducir_db(),
    name,
    ir,
    ev_row_items,
    var_conv.into(),
    tyvar_conv.into(),
  )
}

#[salsa::tracked]
fn lower_mon_module(db: &dyn crate::Db, module: ReducIrModule) -> MonReducIrModule {
  let reducir_db = db.as_reducir_db();
  let items = module
    .items(reducir_db)
    .iter()
    .map(|item| db.lower_reducir_mon_item(*item))
    .collect();
  MonReducIrModule::new(reducir_db, module.module(reducir_db), items)
}

#[salsa::tracked]
fn lower_mon_item(db: &dyn crate::Db, item: ReducIrItem) -> MonReducIrItem {
  let reducir_db = db.as_reducir_db();
  let name = item.name(reducir_db);
  let mut supply = IdSupply::start_from(item.var_supply(reducir_db));
  // TODO: Figure out a better way to do evv_id
  let mut ctx = LowerMonCtx::new(
    db,
    &mut supply,
    ReducIrTermName::Term(name),
    ReducIrVarId(0),
  );
  let is_entry_point = name.name(db.as_core_db()) == db.ident_str("main");
  let ir = item.item(reducir_db);
  let mon_ir = if is_entry_point {
    ctx.lower_monadic_entry_point(ReducIrTermName::Term(name), ir)
  } else {
    ctx.lower_monadic_top_level(ir)
  };
  let row_evs = item
    .row_evs(reducir_db)
    .iter()
    .map(|row| {
      let simple = row.simple(reducir_db);
      let mon_simple = MonReducIrGenItem::new(
        reducir_db,
        simple.name(reducir_db),
        simple.item(reducir_db).assume_no_ext(),
        IdSupply::start_from(item.var_supply(reducir_db)),
      );
      let scoped = row.scoped(reducir_db);
      let mon_scoped = MonReducIrGenItem::new(
        reducir_db,
        scoped.name(reducir_db),
        scoped.item(reducir_db).assume_no_ext(),
        IdSupply::start_from(item.var_supply(reducir_db)),
      );
      MonReducIrRowEv::new(reducir_db, mon_simple, mon_scoped)
    })
    .collect();

  MonReducIrItem::new(reducir_db, name, mon_ir, row_evs, supply)
}

#[salsa::tracked]
fn effect_handler_ir_ty(db: &dyn crate::Db, effect: EffectName) -> ReducIrTy {
  let mut tyvar_conv = IdConverter::new();

  let varp_ty = db.mk_reducir_ty(ReducIrTyKind::VarTy(0));
  let eff_ty = db.mk_reducir_ty(ReducIrTyKind::VarTy(1));
  // TODO: Produce members in order so we don't have to sort or get names here.
  let mut members = db
    .effect_members(effect)
    .iter()
    .map(|op| {
      let lower_ty_ctx = LowerTySchemeCtx::new(db.as_lower_reducir_db(), &mut tyvar_conv);
      let scheme = db.effect_member_sig(*op);
      let (ir_ty_scheme, _) = lower_ty_ctx.lower_scheme(effect.module(db.as_core_db()), &scheme);
      let ir_ty_scheme = match ir_ty_scheme.kind(db.as_reducir_db()) {
        ReducIrTyKind::ForallTy(Kind::ScopedRow, ty) => match ty.kind(db.as_reducir_db()) {
          ReducIrTyKind::FunETy(arg, _, ret) => {
            // We're cheating like crazy here
            // These types are gonna be wrapped in a forall at the end, so
            // we always know what our effect should be here.
            let mk_fune = |arg, ret| db.mk_reducir_ty(ReducIrTyKind::FunETy(arg, eff_ty, ret));
            mk_fune(arg, mk_fune(mk_fune(ret, varp_ty), varp_ty))
          }
          _ => unreachable!(),
        },
        ty => panic!("{:?}", ty),
      };
      (db.effect_member_name(*op), ir_ty_scheme)
    })
    .collect::<Vec<_>>();

  members.sort_by(|a, b| a.0.cmp(&b.0));

  db.mk_forall_ty(
    [Kind::Type, Kind::Type],
    db.mk_prod_ty(vec![
      db.mk_reducir_ty(ReducIrTyKind::MarkerTy(varp_ty)),
      db.mk_prod_ty(members.into_iter().map(|(_, ir_ty)| ir_ty).collect()),
    ]),
  )
}

impl<DB> ReducIrEffectInfo for DB
where
  DB: ?Sized + crate::Db,
{
  fn effect_handler_ir_ty(&self, effect: EffectName) -> ReducIrTy {
    effect_handler_ir_ty(self.as_lower_reducir_db(), effect)
  }
}
impl<DB> ItemWrappers for DB
where
  DB: ?Sized + crate::Db,
{
  fn lookup_wrapper(&self, term_name: TermName, term: Idx<Term<VarId>>) -> &Wrapper {
    let typed_item = self.type_scheme_of(term_name);
    let wrappers = typed_item.item_to_wrappers(self.as_tc_db());
    &wrappers[&term]
  }
}

impl<DB> ItemSchemes for DB
where
  DB: ?Sized + crate::Db,
{
  fn lookup_scheme(&self, term: TermName) -> TyScheme {
    let typed_item = self.type_scheme_of(term);
    typed_item.ty_scheme(self.as_tc_db())
  }
}
impl<DB> VarTys for DB
where
  DB: ?Sized + crate::Db,
{
  fn lookup_var(&self, term: TermName, var_id: VarId) -> Ty {
    let typed_item = self.type_scheme_of(term);
    typed_item.var_to_tys(self.as_tc_db())[&var_id]
  }
}
impl<DB> TermTys for DB
where
  DB: ?Sized + crate::Db,
{
  fn lookup_term(&self, name: TermName, term: Idx<Term<VarId>>) -> tc::TyChkRes<InDb> {
    let typed_item = self.type_scheme_of(name);
    typed_item.term_to_tys(self.as_tc_db())[&term]
  }
}

#[cfg(test)]
mod tests {

  use std::path::PathBuf;

  use crate::Db as LowerIrDb;

  use base::{
    file::{FileId, SourceFile, SourceFileSet},
    pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx},
    Db,
  };
  use expect_test::expect;
  use parser::Db as ParserDb;
  use pretty::RcAllocator;
  use reducir::{
    mon::MonReducIrModule, ty::ReducIrTy, DelimReducIr, ReducIr, ReducIrTyErr, TypeCheck,
  };

  #[derive(Default)]
  #[salsa::db(
    crate::Jar,
    ast::Jar,
    base::Jar,
    desugar::Jar,
    reducir::Jar,
    nameres::Jar,
    parser::Jar,
    tc::Jar,
    ty::Jar
  )]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  impl salsa::Database for TestDatabase {}

  fn lower_function<R>(
    db: &TestDatabase,
    input: &str,
    fn_name: &str,
    op: impl FnOnce(&TestDatabase, PathBuf, &str) -> Option<R>,
  ) -> R {
    let path = std::path::PathBuf::from("test");
    let mut contents = r#"
effect State {
    put : Int -> {},
    get : {} -> Int
}

effect Reader {
    ask : {} -> Int
}

"#
    .to_string();
    contents.push_str(input);
    let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
    SourceFileSet::new(db, vec![file]);

    match op(db, path, fn_name) {
      Some(term) => term,
      None => {
        dbg!(db.all_parse_errors());
        panic!("Errors occurred")
      }
    }
  }

  /// Lower a snippet and return the produced IR
  fn lower_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db DelimReducIr {
    let main = format!("f = {}", input);
    lower_function(db, &main, "f", |db, path, fn_name| {
      db.lower_reducir_item_for_file_name(path, db.ident_str(fn_name))
    })
    .item(db)
  }

  fn lower_mon_module(db: &TestDatabase, input: &str) -> MonReducIrModule {
    let path = std::path::PathBuf::from("test");
    let mut contents = r#"
effect State {
    put : Int -> {},
    get : {} -> Int
}

effect Reader {
    ask : {} -> Int
}

"#
    .to_string();
    contents.push_str(input);
    let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
    SourceFileSet::new(db, vec![file]);

    db.lower_reducir_mon_module_of(db.root_module_for_file(file))
  }

  fn lower_mon_snippet<'db>(db: &'db TestDatabase, input: &str) -> &'db ReducIr {
    let main = format!("f = {}", input);
    lower_function(db, &main, "f", |db, path, fn_name| {
      db.lower_reducir_mon_item_for_file_name(path, db.ident_str(fn_name))
    })
    .item(db)
  }

  trait PrettyTyErr {
    fn to_pretty(self, db: &TestDatabase) -> String;
  }
  impl<'a, Ext: PrettyWithCtx<TestDatabase> + TypeCheck<Ext = Ext> + Clone> PrettyTyErr
    for Result<ReducIrTy, ReducIrTyErr<'a, Ext>>
  {
    fn to_pretty(self, db: &TestDatabase) -> String {
      match self {
        Ok(ty) => ty.pretty(db, &RcAllocator).pretty(80).to_string(),
        Err(err) => err.pretty(db, &RcAllocator).pretty(80).to_string(),
      }
    }
  }

  #[test]
  fn lower_id() {
    let db = TestDatabase::default();
    let ir = lower_snippet(&db, "|x| x");
    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

    let expect = expect!["(forall [(1: Type) (0: ScopedRow)] (fun [V0] (fun<{0}> [V1] V1)))"];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["forall Type . forall ScopedRow . {0} -> T1 -> {0} T1"];
    let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn lower_singleton_product() {
    let db = TestDatabase::default();
    let ir = lower_snippet(&db, "|m| { fst = m }");

    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();
    let expect = expect!["(forall [(1: Type) (0: ScopedRow)] (fun [V0] (fun<{0}> [V1] V1)))"];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["forall Type . forall ScopedRow . {0} -> T1 -> {0} T1"];
    let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn lower_product_literal() {
    let db = TestDatabase::default();
    let ir = lower_snippet(&db, "|a| { x = a, y = a }");
    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

    let expect = expect![[r#"
        (forall [(1: Type) (0: ScopedRow)] (fun [V0]
            (let (V1 ((_row_simple_x_y @ [..]) {})) (fun<{0}> [V2] (V1[0] V2 V2)))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["forall Type . forall ScopedRow . {0} -> T1 -> {0} {T1, T1}"];
    let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn lower_wand() {
    let db = TestDatabase::default();
    let ir = lower_snippet(&db, "|m| |n| (m ,, n).x");
    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

    let expect = expect![[r#"
        (forall
          [(5: Type) (4: ScopedRow) (3: SimpleRow) (2: SimpleRow) (1: SimpleRow) (0: SimpleRow)]
          (fun [V1, V2, V0] (fun<{4}> [V3] (fun<{4}> [V4] (V2[3][0] (V1[0] V3 V4))))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall Type .
          forall ScopedRow .
            forall SimpleRow .
              forall SimpleRow .
                forall SimpleRow .
                  forall SimpleRow .
                    { {3} -> {2} -> {1}
                    , forall Type . (<4> -> T0) -> (<3> -> T0) -> <2> -> T0
                    , {{1} -> {3}, <3> -> <1>}
                    , {{1} -> {2}, <2> -> <1>}
                    } -> { {0} -> T5 -> {1}
                         , forall Type . (<1> -> T0) -> (T6 -> T0) -> <2> -> T0
                         , {{1} -> {0}, <0> -> <1>}
                         , {{1} -> T5, T5 -> <1>}
                         } -> {4} -> {3} -> {4} {2} -> {4} T5"#]];
    let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn lower_reader_ask() {
    let db = TestDatabase::default();

    let ir = lower_mon_module(
      &db,
      r#"
main = with {
    ask = |x| |k| k(374),
    return = |x| x
} do Reader.ask({})"#,
    )
    .items(&db)
    .iter()
    .find(|item| item.name(&db).name_text(&db) == "main")
    .unwrap()
    .item(&db);

    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();
    let expect = expect![[r#"
        (case ((let
            [ (V1 ((_row_scoped__Reader @ [..]) {}))
            , (V2 ((_row_simple__ask @ [..]) {}))
            , (V3 ((_row_simple_ask_return @ [..]) {}))
            , (V4 ((_row_simple_return_ask @ [..]) {}))
            , (V5 (V3[0]
              (fun [V6, V15] <0: (fun [V7] (V7 374))>)
              (fun [V8, V16] <0: V8>)))
            ]
            ((__mon_freshm @ [..])
              (fun [V13]
                ((__mon_prompt @ [..])
                  V13
                  (fun [V14] (V1[0] V14 {V13, (V4[3][0] V5)}))
                  (V4[2][0] V5)
                  (fun [V9]
                    ((let [ (V10 {}) , (V11 (V1[3][0] V9)) ]
                      (fun [V17]
                        <1: (forall [(0: Type) (1: Type) (2: Type)] {V11[0], (fun [V12]
                              ((__mon_bind @ [..])
                                (V2[3][0] V11[1] V10)
                                (fun [V19] (V19 V12)))), (fun [V18, V20] <0: V18>)})>))
                      V9)))))) {})
          (fun [V0] V0)
          (fun [V0] 5467))"#]];
    expect.assert_eq(pretty_ir.as_str());

    let expect_ty = expect!["Int"];
    let pretty_ir_ty = ir
      .type_check(&db)
      .map_err_pretty_with(&db)
      .unwrap()
      .pretty_with(&db)
      .pprint()
      .pretty(80)
      .to_string();
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn lower_state_get() {
    let db = TestDatabase::default();

    let ir = lower_snippet(
      &db,
      r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(5)"#,
    );
    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

    let expect = expect![[r#"
        (forall [(1: ScopedRow) (0: ScopedRow)] (fun [V1, V0]
            (let
              [ (V2 ((_row_simple_put_get @ [..]) {}))
              , (V3 ((_row_simple_get_put @ [..]) {}))
              , (V4 ((_row_simple_state_value @ [..]) {}))
              , (V5 ((_row_simple_putget_return @ [..]) {}))
              , (V6 ((_row_simple_return_putget @ [..]) {}))
              ]
              ((let
                (V7 (V5[0]
                  (V3[0]
                    (fun<{1}> [V8] (fun<{1}> [V9] (fun<{1}> [V10] (V9 V10 V10))))
                    (fun<{1}> [V11] (fun<{1}> [V12] (fun<{1}> [V13] (V12 {} V11)))))
                  (fun<{1}> [V14] (fun<{1}> [V15] (V4[0] V15 V14)))))
                (new_prompt [V20] (prompt V20 (fun [V21]
                    (V1[0] V21 {V20, (V6[3][0] V7)})) (V6[2][0] V7) (fun [V16]
                    (let [ (V17 {}) , (V18 (V1[3][0] V16)) ]
                      (yield<Int> V18[0] (fun [V19] (V3[2][0] V18[1] V17 V19))))))))
                5))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall ScopedRow .
          forall ScopedRow .
            { {1} -> { (Marker Int -> {1} {Int, Int})
                     , { Int -> {1} {} -> {1} Int -> {1} {Int, Int} -> {1} Int ->
                       {1} {Int, Int}
                       , {} -> {1} Int -> {1} Int -> {1} {Int, Int} -> {1} Int ->
                       {1} {Int, Int}
                       }
                     } -> {0}
            , forall Type .
              (<2> -> T0) -> ({ (Marker Int -> {2} {Int, Int})
                              , { Int -> {2} {} -> {2} Int -> {2} {Int, Int} ->
                                {2} Int -> {2} {Int, Int}
                                , {} -> {2} Int -> {2} Int -> {2} {Int, Int} ->
                                {2} Int -> {2} {Int, Int}
                                }
                              } -> T0) -> <1> -> T0
            , {{0} -> {1}, <1> -> <0>}
            , { {0} -> { (Marker Int -> {1} {Int, Int})
                       , { Int -> {1} {} -> {1} Int -> {1} {Int, Int} -> {1} Int ->
                         {1} {Int, Int}
                         , {} -> {1} Int -> {1} Int -> {1} {Int, Int} -> {1} Int ->
                         {1} {Int, Int}
                         }
                       }
              , { (Marker Int -> {1} {Int, Int})
                , { Int -> {1} {} -> {1} Int -> {1} {Int, Int} -> {1} Int -> {1} { Int
                                                                                 , Int
                                                                                 }
                  , {} -> {1} Int -> {1} Int -> {1} {Int, Int} -> {1} Int -> {1} { Int
                                                                                 , Int
                                                                                 }
                  }
                } -> <0>
              }
            } -> {1} -> {Int, Int}"#]];
    let pretty_ir_ty = {
      let this = ir.type_check(&db);
      let db = &db;
      match this {
        Ok(ty) => ty.pretty(db, &RcAllocator).pretty(80).to_string(),
        Err(err) => {
          println!("{}", err.pretty(db, &RcAllocator).pretty(80));
          panic!();
        }
      }
    };
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn lower_multi_effect_fun() {
    let db = TestDatabase::default();
    let ir = lower_snippet(
      &db,
      r#"
(with  {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do (with {
  ask = |x| |k| k(16777215),
  return = |x| x,
} do w = Reader.ask({}); State.put(w)))(14).state
"#,
    );

    let pretty_ir = ir.pretty_string(&db, 80);

    let expect = expect![[r#"
        (forall
          [(5: Type) (4: ScopedRow) (3: ScopedRow) (2: ScopedRow) (1: ScopedRow) (0: ScopedRow)]
          (fun [V1, V2, V3, V0]
            (let
              [ (V4 ((_row_simple__ask @ [..]) {}))
              , (V5 ((_row_simple_get_put @ [..]) {}))
              , (V6 ((_row_simple_get_put @ [..]) {}))
              , (V7 ((_row_simple_value_state @ [..]) {}))
              , (V8 ((_row_simple_state_value @ [..]) {}))
              , (V9 ((_row_simple_ask_return @ [..]) {}))
              , (V10 ((_row_simple_return_ask @ [..]) {}))
              , (V11 ((_row_simple_putget_return @ [..]) {}))
              , (V12 ((_row_simple_return_putget @ [..]) {}))
              ]
              (V8[2][0]
                ((let
                  (V13 (V11[0]
                    (V5[0]
                      (fun<{4}> [V14] (fun<{4}> [V15] (fun<{4}> [V16] (V15 V16 V16))))
                      (fun<{4}> [V17] (fun<{4}> [V18] (fun<{4}> [V19] (V18 {} V17)))))
                    (fun<{4}> [V20] (fun<{4}> [V21] (V8[0] V21 V20)))))
                  (new_prompt [V37] (prompt V37 (fun [V38]
                      (V1[0] V38 {V37, (V12[3][0] V13)})) (V12[2][0] V13) (fun [V22]
                      (let
                        (V23 (V9[0]
                          (fun<{3}> [V24] (fun<{3}> [V25] (V25 16777215)))
                          (fun<{3}> [V26] V26)))
                        (new_prompt [V35] (prompt V35 (fun [V36]
                            (V2[0] V36 {V35, (V10[3][0] V23)})) (V10[2][0] V23) (fun
                            [V27]
                            ((fun<{2}> [V31]
                              (let [ (V32 V31) , (V33 (V3[3][0] V27)) ]
                                (yield<{}> V33[0] (fun [V34]
                                    (V6[3][0] V33[1] V32 V34)))))
                              (let [ (V28 {}) , (V29 (V2[3][0] V27)) ]
                                (yield<Int> V29[0] (fun [V30]
                                    (V4[3][0] V29[1] V28 V30))))))))))))) 14)))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall Type .
          forall ScopedRow .
            forall ScopedRow .
              forall ScopedRow .
                forall ScopedRow .
                  forall ScopedRow .
                    { {4} -> { (Marker Int -> {4} {Int, {}})
                             , { Int -> {4} {} -> {4} Int -> {4} {Int, {}} -> {4} Int ->
                               {4} {Int, {}}
                               , {} -> {4} Int -> {4} Int -> {4} {Int, {}} -> {4} Int ->
                               {4} {Int, {}}
                               }
                             } -> {3}
                    , forall Type .
                      (<5> -> T0) -> ({ (Marker Int -> {5} {Int, {}})
                                      , { Int -> {5} {} -> {5} Int -> {5} {Int, {}} ->
                                        {5} Int -> {5} {Int, {}}
                                        , {} -> {5} Int -> {5} Int -> {5} {Int, {}} ->
                                        {5} Int -> {5} {Int, {}}
                                        }
                                      } -> T0) -> <4> -> T0
                    , {{3} -> {4}, <4> -> <3>}
                    , { {3} -> { (Marker Int -> {4} {Int, {}})
                               , { Int -> {4} {} -> {4} Int -> {4} {Int, {}} ->
                                 {4} Int -> {4} {Int, {}}
                                 , {} -> {4} Int -> {4} Int -> {4} {Int, {}} ->
                                 {4} Int -> {4} {Int, {}}
                                 }
                               }
                      , { (Marker Int -> {4} {Int, {}})
                        , { Int -> {4} {} -> {4} Int -> {4} {Int, {}} -> {4} Int ->
                          {4} {Int, {}}
                          , {} -> {4} Int -> {4} Int -> {4} {Int, {}} -> {4} Int ->
                          {4} {Int, {}}
                          }
                        } -> <3>
                      }
                    } -> { {3} -> {(Marker {}), {} -> {3} Int -> {3} {} -> {3} {}} ->
                         {2}
                         , forall Type .
                           (<4> -> T0) -> ({ (Marker {})
                                           , {} -> {4} Int -> {4} {} -> {4} {}
                                           } -> T0) -> <3> -> T0
                         , {{2} -> {3}, <3> -> <2>}
                         , { {2} -> {(Marker {}), {} -> {3} Int -> {3} {} -> {3} {}}
                           , {(Marker {}), {} -> {3} Int -> {3} {} -> {3} {}} -> <2>
                           }
                         } -> { {0} -> { (Marker T5)
                                       , { Int -> {1} {} -> {1} T5 -> {1} T5
                                         , {} -> {1} Int -> {1} T5 -> {1} T5
                                         }
                                       } -> {2}
                              , forall Type .
                                (<1> -> T0) -> ({ (Marker T6)
                                                , { Int -> {2} {} -> {2} T6 -> {2} T6
                                                  , {} -> {2} Int -> {2} T6 -> {2} T6
                                                  }
                                                } -> T0) -> <3> -> T0
                              , {{2} -> {0}, <0> -> <2>}
                              , { {2} -> { (Marker T5)
                                         , { Int -> {1} {} -> {1} T5 -> {1} T5
                                           , {} -> {1} Int -> {1} T5 -> {1} T5
                                           }
                                         }
                                , { (Marker T5)
                                  , { Int -> {1} {} -> {1} T5 -> {1} T5
                                    , {} -> {1} Int -> {1} T5 -> {1} T5
                                    }
                                  } -> <2>
                                }
                              } -> {4} -> Int"#]];
    let pretty_ir_ty = {
      let this = ir.type_check(&db).map_err_pretty_with(&db);
      match this {
        Ok(ty) => ty.pretty_string(&db, 80),
        Err(err) => {
          println!("{:?}", err);
          panic!();
        }
      }
    };
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn monadic_lower_state_get() {
    let db = TestDatabase::default();

    let ir = lower_mon_snippet(
      &db,
      r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(5)"#,
    );

    let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

    let expect = expect![[r#"
        (forall [(1: ScopedRow) (0: ScopedRow)] (fun [V1, V0]
            ((let
              [ (V2 ((_row_simple_put_get @ [..]) {}))
              , (V3 ((_row_simple_get_put @ [..]) {}))
              , (V4 ((_row_simple_state_value @ [..]) {}))
              , (V5 ((_row_simple_putget_return @ [..]) {}))
              , (V6 ((_row_simple_return_putget @ [..]) {}))
              ]
              ((__mon_bind @ [..])
                (let
                  (V7 (V5[0]
                    (V3[0]
                      (fun [V8, V24]
                        <0: (fun [V9, V23]
                            <0: (fun [V10]
                                ((__mon_bind @ [..])
                                  (V9 V10)
                                  (fun [V22] (V22 V10))))>)>)
                      (fun [V11, V27]
                        <0: (fun [V12, V26]
                            <0: (fun [V13]
                                ((__mon_bind @ [..])
                                  (V12 {})
                                  (fun [V25] (V25 V11))))>)>))
                    (fun [V14, V29] <0: (fun [V15, V28] <0: (V4[0] V15 V14)>)>)))
                  ((__mon_freshm @ [..])
                    (fun [V20]
                      ((__mon_prompt @ [..])
                        V20
                        (fun [V21] (V1[0] V21 {V20, (V6[3][0] V7)}))
                        (V6[2][0] V7)
                        (fun [V16]
                          ((let [ (V17 {}) , (V18 (V1[3][0] V16)) ]
                            (fun [V30]
                              <1: (forall [(0: Type) (1: Type) (2: Type)] {V18[0], (fun
                                    [V19]
                                    ((__mon_bind @ [..])
                                      (V3[2][0] V18[1] V17)
                                      (fun [V32] (V32 V19)))), (fun [V31, V33] <0: V31>)
                                  })>)) V16))))))
                (fun [V34] (V34 5)))) V0)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall ScopedRow .
          forall ScopedRow .
            { {1} -> { (Marker Int -> (Mon {1} {Int, Int}))
                     , { Int -> (Mon {1} ({} -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       , {} -> (Mon {1} (Int -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       }
                     } -> {0}
            , forall Type .
              (<2> -> T0) -> ({ (Marker Int -> (Mon {2} {Int, Int}))
                              , { Int -> (Mon {2} ({} -> (Mon {2} Int -> (Mon {2} { Int
                                                                                  , Int
                                                                                  })))
                                -> (Mon {2} Int -> (Mon {2} {Int, Int})))
                                , {} -> (Mon {2} (Int -> (Mon {2} Int -> (Mon {2} { Int
                                                                                  , Int
                                                                                  })))
                                -> (Mon {2} Int -> (Mon {2} {Int, Int})))
                                }
                              } -> T0) -> <1> -> T0
            , {{0} -> {1}, <1> -> <0>}
            , { {0} -> { (Marker Int -> (Mon {1} {Int, Int}))
                       , { Int -> (Mon {1} ({} -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         , {} -> (Mon {1} (Int -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         }
                       }
              , { (Marker Int -> (Mon {1} {Int, Int}))
                , { Int -> (Mon {1} ({} -> (Mon {1} Int -> (Mon {1} {Int, Int}))) ->
                  (Mon {1} Int -> (Mon {1} {Int, Int})))
                  , {} -> (Mon {1} (Int -> (Mon {1} Int -> (Mon {1} {Int, Int}))) ->
                  (Mon {1} Int -> (Mon {1} {Int, Int})))
                  }
                } -> <0>
              }
            } -> (Mon {1} {Int, Int})"#]];
    let pretty_ty = ir
      .type_check(&db)
      .map_err_pretty_with(&db)
      .unwrap()
      .pretty_string(&db, 80);
    expect_ty.assert_eq(&pretty_ty);
  }

  #[test]
  fn monadic_lower_multi_effect() {
    let db = TestDatabase::default();
    let ir = lower_mon_snippet(
      &db,
      r#"
(with  {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do (with {
  ask = |x| |k| k(16777215),
  return = |x| x,
} do w = Reader.ask({}); State.put(w)))(14).state
"#,
    );

    let pretty_ir = ir.pretty_string(&db, 80);

    let expect = expect![[r#"
        (forall
          [(5: Type) (4: ScopedRow) (3: ScopedRow) (2: ScopedRow) (1: ScopedRow) (0: ScopedRow)]
          (fun [V1, V2, V3, V0]
            ((let
              [ (V4 ((_row_simple__ask @ [..]) {}))
              , (V5 ((_row_simple_get_put @ [..]) {}))
              , (V6 ((_row_simple_get_put @ [..]) {}))
              , (V7 ((_row_simple_value_state @ [..]) {}))
              , (V8 ((_row_simple_state_value @ [..]) {}))
              , (V9 ((_row_simple_ask_return @ [..]) {}))
              , (V10 ((_row_simple_return_ask @ [..]) {}))
              , (V11 ((_row_simple_putget_return @ [..]) {}))
              , (V12 ((_row_simple_return_putget @ [..]) {}))
              ]
              ((__mon_bind @ [..])
                ((__mon_bind @ [..])
                  (let
                    (V13 (V11[0]
                      (V5[0]
                        (fun [V14, V41]
                          <0: (fun [V15, V40]
                              <0: (fun [V16]
                                  ((__mon_bind @ [..])
                                    (V15 V16)
                                    (fun [V39] (V39 V16))))>)>)
                        (fun [V17, V44]
                          <0: (fun [V18, V43]
                              <0: (fun [V19]
                                  ((__mon_bind @ [..])
                                    (V18 {})
                                    (fun [V42] (V42 V17))))>)>))
                      (fun [V20, V46] <0: (fun [V21, V45] <0: (V8[0] V21 V20)>)>)))
                    ((__mon_freshm @ [..])
                      (fun [V37]
                        ((__mon_prompt @ [..])
                          V37
                          (fun [V38] (V1[0] V38 {V37, (V12[3][0] V13)}))
                          (V12[2][0] V13)
                          (fun [V22]
                            ((let
                              (V23 (V9[0]
                                (fun [V24, V47] <0: (fun [V25] (V25 16777215))>)
                                (fun [V26, V48] <0: V26>)))
                              ((__mon_freshm @ [..])
                                (fun [V35]
                                  ((__mon_prompt @ [..])
                                    V35
                                    (fun [V36] (V2[0] V36 {V35, (V10[3][0] V23)}))
                                    (V10[2][0] V23)
                                    (fun [V27]
                                      ((__mon_bind @ [..])
                                        (let [ (V28 {}) , (V29 (V2[3][0] V27)) ]
                                          (fun [V53]
                                            <1: (forall [(0: Type) (1: Type) (2: Type)]
                                                {V29[0], (fun [V30]
                                                  ((__mon_bind @ [..])
                                                    (V4[3][0] V29[1] V28)
                                                    (fun [V55] (V55 V30)))), (fun
                                                  [V54
                                                  ,V56] <0: V54>)})>))
                                        (fun [V57]
                                          (let
                                            [ (V31 V57)
                                            , (V32 V31)
                                            , (V33 (V3[3][0] V27))
                                            ]
                                            (fun [V49]
                                              <1: (forall
                                                  [(0: Type) (1: Type) (2: Type)] {
                                                  V33[0], (fun [V34]
                                                    ((__mon_bind @ [..])
                                                      (V6[3][0] V33[1] V32)
                                                      (fun [V51] (V51 V34)))), (fun
                                                    [V50
                                                    ,V52] <0: V50>)})>)))
                                        V27)))))) V22))))))
                  (fun [V58] (V58 14)))
                (fun [V59, V60] <0: (V8[2][0] V59)>))) V0)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall Type .
          forall ScopedRow .
            forall ScopedRow .
              forall ScopedRow .
                forall ScopedRow .
                  forall ScopedRow .
                    { {4} -> { (Marker Int -> (Mon {4} {Int, {}}))
                             , { Int -> (Mon {4} ({} -> (Mon {4} Int -> (Mon {4} { Int
                                                                                 , {}
                                                                                 }))) ->
                               (Mon {4} Int -> (Mon {4} {Int, {}})))
                               , {} -> (Mon {4} (Int -> (Mon {4} Int -> (Mon {4} { Int
                                                                                 , {}
                                                                                 }))) ->
                               (Mon {4} Int -> (Mon {4} {Int, {}})))
                               }
                             } -> {3}
                    , forall Type .
                      (<5> -> T0) -> ({ (Marker Int -> (Mon {5} {Int, {}}))
                                      , { Int -> (Mon {5} ({} -> (Mon {5} Int ->
                                        (Mon {5} {Int, {}}))) -> (Mon {5} Int ->
                                        (Mon {5} {Int, {}})))
                                        , {} -> (Mon {5} (Int -> (Mon {5} Int ->
                                        (Mon {5} {Int, {}}))) -> (Mon {5} Int ->
                                        (Mon {5} {Int, {}})))
                                        }
                                      } -> T0) -> <4> -> T0
                    , {{3} -> {4}, <4> -> <3>}
                    , { {3} -> { (Marker Int -> (Mon {4} {Int, {}}))
                               , { Int -> (Mon {4} ({} -> (Mon {4} Int -> (Mon {4} { Int
                                                                                   , {}
                                                                                   })))
                                 -> (Mon {4} Int -> (Mon {4} {Int, {}})))
                                 , {} -> (Mon {4} (Int -> (Mon {4} Int -> (Mon {4} { Int
                                                                                   , {}
                                                                                   })))
                                 -> (Mon {4} Int -> (Mon {4} {Int, {}})))
                                 }
                               }
                      , { (Marker Int -> (Mon {4} {Int, {}}))
                        , { Int -> (Mon {4} ({} -> (Mon {4} Int -> (Mon {4} {Int, {}})))
                          -> (Mon {4} Int -> (Mon {4} {Int, {}})))
                          , {} -> (Mon {4} (Int -> (Mon {4} Int -> (Mon {4} {Int, {}})))
                          -> (Mon {4} Int -> (Mon {4} {Int, {}})))
                          }
                        } -> <3>
                      }
                    } -> { {3} -> { (Marker {})
                                  , {} -> (Mon {3} (Int -> (Mon {3} {})) ->
                                  (Mon {3} {}))
                                  } -> {2}
                         , forall Type .
                           (<4> -> T0) -> ({ (Marker {})
                                           , {} -> (Mon {4} (Int -> (Mon {4} {})) ->
                                           (Mon {4} {}))
                                           } -> T0) -> <3> -> T0
                         , {{2} -> {3}, <3> -> <2>}
                         , { {2} -> { (Marker {})
                                    , {} -> (Mon {3} (Int -> (Mon {3} {})) ->
                                    (Mon {3} {}))
                                    }
                           , { (Marker {})
                             , {} -> (Mon {3} (Int -> (Mon {3} {})) -> (Mon {3} {}))
                             } -> <2>
                           }
                         } -> { {0} -> { (Marker T5)
                                       , { Int -> (Mon {1} ({} -> (Mon {1} T5)) ->
                                         (Mon {1} T5))
                                         , {} -> (Mon {1} (Int -> (Mon {1} T5)) ->
                                         (Mon {1} T5))
                                         }
                                       } -> {2}
                              , forall Type .
                                (<1> -> T0) -> ({ (Marker T6)
                                                , { Int -> (Mon {2} ({} -> (Mon {2} T6))
                                                  -> (Mon {2} T6))
                                                  , {} -> (Mon {2} (Int -> (Mon {2} T6))
                                                  -> (Mon {2} T6))
                                                  }
                                                } -> T0) -> <3> -> T0
                              , {{2} -> {0}, <0> -> <2>}
                              , { {2} -> { (Marker T5)
                                         , { Int -> (Mon {1} ({} -> (Mon {1} T5)) ->
                                           (Mon {1} T5))
                                           , {} -> (Mon {1} (Int -> (Mon {1} T5)) ->
                                           (Mon {1} T5))
                                           }
                                         }
                                , { (Marker T5)
                                  , { Int -> (Mon {1} ({} -> (Mon {1} T5)) ->
                                    (Mon {1} T5))
                                    , {} -> (Mon {1} (Int -> (Mon {1} T5)) ->
                                    (Mon {1} T5))
                                    }
                                  } -> <2>
                                }
                              } -> (Mon {4} Int)"#]];
    let pretty_ir_ty = {
      let this = ir.type_check(&db).map_err_pretty_with(&db);
      match this {
        Ok(ty) => ty.pretty_string(&db, 80),
        Err(err) => {
          println!("{:?}", err);
          panic!();
        }
      }
    };
    expect_ty.assert_eq(&pretty_ir_ty);
  }

  #[test]
  fn monadic_lower_applied_wand() {
    let db = TestDatabase::default();
    let module = lower_mon_module(
      &db,
      r#"
            wand = |m| |n| (m ,, n).x
            main = w = wand({ x = 5 })({ y = {} }); w
            "#,
    );

    let items = vec![
      // wand
      expect![[r#"
          (forall
            [(5: Type) (4: ScopedRow) (3: SimpleRow) (2: SimpleRow) (1: SimpleRow) (0: SimpleRow)]
            (fun [V1, V2, V0]
              <0: (fun [V3, V6] <0: (fun [V4, V5] <0: (V2[3][0] (V1[0] V3 V4))>)>)>))"#]],
      // main
      expect![[r#"
          (case ((let
              [ (V1 ((_row_simple_x_y @ [..]) {}))
              , (V2 ((_row_simple_y_x @ [..]) {}))
              ]
              ((__mon_bind @ [..])
                ((__mon_bind @ [..])
                  ((__mon_bind @ [..]) ((wand @ [..]) V1 V2) (fun [V5] (V5 5)))
                  (fun [V6] (V6 {})))
                (fun [V7] (let (V3 V7) (fun [V4] <0: V3>))))) {})
            (fun [V0] V0)
            (fun [V0] 5467))"#]],
    ];
    let tys = vec![
      // wand
      expect![[r#"
          forall Type .
            forall ScopedRow .
              forall SimpleRow .
                forall SimpleRow .
                  forall SimpleRow .
                    forall SimpleRow .
                      { {3} -> {2} -> {1}
                      , forall Type . (<4> -> T0) -> (<3> -> T0) -> <2> -> T0
                      , {{1} -> {3}, <3> -> <1>}
                      , {{1} -> {2}, <2> -> <1>}
                      } -> { {0} -> T5 -> {1}
                           , forall Type . (<1> -> T0) -> (T6 -> T0) -> <2> -> T0
                           , {{1} -> {0}, <0> -> <1>}
                           , {{1} -> T5, T5 -> <1>}
                           } -> (Mon {4} {3} -> (Mon {4} {2} -> (Mon {4} T5)))"#]],
      // main
      expect!["Int"],
    ];
    for ((item, expect), expect_ty) in module
      .items(&db)
      .iter()
      .zip(items.into_iter())
      .zip(tys.into_iter())
    {
      let ir = item.item(&db);
      let item_str = ir.pretty_with(&db).pprint().pretty(80).to_string();
      expect.assert_eq(&item_str);

      let item_ty_str = ir.type_check(&db).to_pretty(&db);
      expect_ty.assert_eq(&item_ty_str)
    }
  }

  #[test]
  fn monadic_lower_state_in_main() {
    let db = TestDatabase::default();
    let module = lower_mon_module(
      &db,
      r#"
main = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(825).value"#,
    );

    let items = vec![
      // main
      expect![[r#"
          (case ((let
              [ (V1 ((_row_scoped__State @ [..]) {}))
              , (V2 ((_row_simple_put_get @ [..]) {}))
              , (V3 ((_row_simple_get_put @ [..]) {}))
              , (V4 ((_row_simple_state_value @ [..]) {}))
              , (V5 ((_row_simple_putget_return @ [..]) {}))
              , (V6 ((_row_simple_return_putget @ [..]) {}))
              ]
              ((__mon_bind @ [..])
                ((__mon_bind @ [..])
                  (let
                    (V7 (V5[0]
                      (V3[0]
                        (fun [V8, V24]
                          <0: (fun [V9, V23]
                              <0: (fun [V10]
                                  ((__mon_bind @ [..])
                                    (V9 V10)
                                    (fun [V22] (V22 V10))))>)>)
                        (fun [V11, V27]
                          <0: (fun [V12, V26]
                              <0: (fun [V13]
                                  ((__mon_bind @ [..])
                                    (V12 {})
                                    (fun [V25] (V25 V11))))>)>))
                      (fun [V14, V29] <0: (fun [V15, V28] <0: (V4[0] V15 V14)>)>)))
                    ((__mon_freshm @ [..])
                      (fun [V20]
                        ((__mon_prompt @ [..])
                          V20
                          (fun [V21] (V1[0] V21 {V20, (V6[3][0] V7)}))
                          (V6[2][0] V7)
                          (fun [V16]
                            ((let [ (V17 {}) , (V18 (V1[3][0] V16)) ]
                              (fun [V30]
                                <1: (forall [(0: Type) (1: Type) (2: Type)] {V18[0], (fun
                                      [V19]
                                      ((__mon_bind @ [..])
                                        (V3[2][0] V18[1] V17)
                                        (fun [V32] (V32 V19)))), (fun [V31, V33] <0: V31>)
                                    })>)) V16))))))
                  (fun [V34] (V34 825)))
                (fun [V35, V36] <0: (V4[3][0] V35)>))) {})
            (fun [V0] V0)
            (fun [V0] 5467))"#]],
    ];
    let tys = vec![
      // main
      expect!["Int"],
    ];
    for ((item, expect), expect_ty) in module
      .items(&db)
      .iter()
      .zip(items.into_iter())
      .zip(tys.into_iter())
    {
      let ir = item.item(&db);
      let item_str = ir.pretty_with(&db).pprint().pretty(80).to_string();
      expect.assert_eq(&item_str);

      let item_ty_str = ir.type_check(&db).to_pretty(&db);
      expect_ty.assert_eq(&item_ty_str)
    }
  }
}
