use ast::{AstModule, AstTerm, Term};
use base::{
    id::{EffectName, EffectOpName, Id, IdSupply, ReducIrVarId, TermName, TyVarId, VarId},
    id_converter::IdConverter,
    ident::Ident,
    modules::Module,
};
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};
use reducir::{
    mon::{MonReducIrGenItem, MonReducIrItem, MonReducIrModule, MonReducIrRowEv},
    ty::{Kind, MkReducIrTy, ReducIrTy, ReducIrTyKind, ReducIrVarTy},
    GeneratedReducIrName, ReducIr, ReducIrGenItem, ReducIrItem,
    ReducIrKind::*,
    ReducIrModule, ReducIrRowEv, ReducIrTermName, P,
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
    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize;

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
        self.lower_reducir_item_of(term_name)
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
        .enumerate()
        .map(|(i, ty)| (*ty, i as i32))
        .collect::<FxHashMap<_, _>>();

    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let mut lower_ctx = LowerCtx::new(
        db,
        &mut var_conv,
        LowerTyCtx::new(db, &mut tyvar_conv, tyvar_env),
        ReducIrTermName::Gen(row_ev_name),
    );
    let ir = lower_ctx.row_evidence_ir::<Sema>(left_row, right_row, goal_row);

    let ir = order_of_tys.into_iter().rfold(ir, |ir, var| {
        let var = ReducIrVarTy {
            var: tyvar_conv.convert(var),
            kind: Kind::Type,
        };
        ReducIr::new(TyAbs(var, P::new(ir)))
    });

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

    let (_, ty_ctx) = LowerTySchemeCtx::new(db, &mut tyvar_conv)
        .lower_scheme(name.module(db.as_core_db()), &scheme);

    let required_evidence = typed_item.required_evidence(tc_db);
    let (mut lower_ctx, ev_solved, ev_params, ev_row_items) =
        LowerCtx::new(db, &mut var_conv, ty_ctx, ReducIrTermName::Term(name))
            .collect_evidence_params(required_evidence.iter());

    let body = lower_ctx.lower_term(ast, ast.root());
    // TODO: Bit of a hack. Eventually we'd like to generate our solved row ev in a central location.
    // Add row evidence as parameters of the term
    let body = ev_solved
        .into_iter()
        .rfold(body, |body, (arg, term)| ReducIr::local(arg, term, body));
    // Wrap our term in any unsolved row evidence params we need
    let is_entry_point = name.name(db.as_core_db()) == db.ident_str("main");
    let evv_param = if is_entry_point {
        vec![]
    } else {
        let evv_var = lower_ctx.evv_var(ast);
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
    // TODO: Produce members in order so we don't have to sort or get names here.
    let mut members = db
        .effect_members(effect)
        .iter()
        .map(|op| {
            let lower_ty_ctx = LowerTySchemeCtx::new(db.as_lower_reducir_db(), &mut tyvar_conv);
            let scheme = db.effect_member_sig(*op);
            let (ir_ty_scheme, _) =
                lower_ty_ctx.lower_scheme(effect.module(db.as_core_db()), &scheme);
            let ir_ty_scheme = match ir_ty_scheme.kind(db.as_reducir_db()) {
                ReducIrTyKind::FunTy(args, ret) => {
                    let (arg, rest) = args.split_at(1);

                    let mut ty = ret;
                    if !rest.is_empty() {
                        ty = db.mk_fun_ty(rest.iter().copied(), ret);
                    }
                    db.mk_fun_ty([arg[0], db.mk_fun_ty([ty], varp_ty)], varp_ty)
                }
                ty => panic!("{:?}", ty),
            };
            (db.effect_member_name(*op), ir_ty_scheme)
        })
        .collect::<Vec<_>>();

    members.sort_by(|a, b| a.0.cmp(&b.0));

    db.mk_reducir_ty(ReducIrTyKind::ForallTy(
        Kind::Type,
        db.mk_reducir_ty(ReducIrTyKind::ProductTy(vec![
            db.mk_reducir_ty(ReducIrTyKind::MarkerTy(varp_ty)),
            db.mk_reducir_ty(ReducIrTyKind::ProductTy(
                members.into_iter().map(|(_, ir_ty)| ir_ty).collect(),
            )),
        ])),
    ))
}

impl<DB> ReducIrEffectInfo for DB
where
    DB: ?Sized + crate::Db,
{
    fn effect_handler_op_index(&self, effect_op: EffectOpName) -> usize {
        nameres::effect_handler_op_index(self.as_nameres_db(), effect_op)
    }

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
    put : {} -> {},
    get : {} -> {}
}

effect Reader {
    ask : {} -> {}
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
    put : {} -> {},
    get : {} -> {}
}

effect Reader {
    ask : {} -> {}
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
    impl<'a, Ext: PrettyWithCtx<TestDatabase> + Clone> PrettyTyErr
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

        let expect = expect!["(forall [(T1: Type) (T0: ScopedRow)] (fun [V1, V0] V1))"];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . T1 -> {0} -> T1"];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_singleton_product() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|m| { fst = m }");

        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();
        let expect = expect!["(forall [(T1: Type) (T0: ScopedRow)] (fun [V1, V0] V1))"];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . T1 -> {0} -> T1"];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
        expect_ty.assert_eq(&pretty_ir_ty);
    }

    #[test]
    fn lower_product_literal() {
        let db = TestDatabase::default();
        let ir = lower_snippet(&db, "|a| { x = a, y = a }");
        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: Type) (T0: ScopedRow)] (fun [V0]
                (let (V1 (_row_simple_x_y @ [Ty(T1), Ty(T1)])) (fun [V2] (V1[0] V2 V2)))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect!["forall Type . forall ScopedRow . {0} -> T1 -> {T1, T1}"];
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
              [(T5: Type) (T4: ScopedRow) (T3: SimpleRow) (T2: SimpleRow) (T1: SimpleRow) (T0: SimpleRow)]
              (fun [V1, V2, V3, V4, V0] (V2[3][0] (V1[0] V3 V4))))"#]];
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
                             } -> {3} -> {2} -> {4} -> T5"#]];
        let pretty_ir_ty = ir.type_check(&db).to_pretty(&db);
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
} do State.get({}))({})"#,
        );
        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                (let
                  [ (V2 (_row_simple_state_value @ [Ty({}), Ty({})]))
                  , (V3 (_row_simple_return_putget @ [Ty({} -> ({} -> {} -> {{}, {}}) -> {}
                  -> {{}, {}}), Ty({} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}), Ty({} ->
                  {} -> {{}, {}})]))
                  , (V4 (_row_simple_putget_return @ [Ty({} -> {} -> {{}, {}}), Ty({} -> ({}
                  -> {} -> {{}, {}}) -> {} -> {{}, {}}), Ty({} -> ({} -> {} -> {{}, {}}) ->
                  {} -> {{}, {}})]))
                  , (V5 (_row_simple_get_put @ [Ty({} -> ({} -> {} -> {{}, {}}) -> {} ->
                  {{}, {}}), Ty({} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}})]))
                  ]
                  ((let
                    (V6 (V4[0]
                      (V5[0]
                        (fun [V7, V8, V9] (V8 V9 V9))
                        (fun [V10, V11, V12] (V11 {} V10)))
                      (fun [V13, V14] (V2[0] V14 V13))))
                    (new_prompt [V18] (prompt V18 (fun [V0] (V1[0] V0 {V18, (V4[2][0] V6)}))
                      (V4[3][0]
                        V6
                        (let (V15 {})
                          (let (V16 (V1[3][0] V0))
                            (yield V16[0] (fun [V17] (V16[1][1] V15 V17))))))))) {}))))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect![[r#"
            forall ScopedRow .
              forall ScopedRow .
                { {1} -> { (Marker {} -> {{}, {}})
                         , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                           , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                           }
                         } -> {0}
                , forall Type .
                  (<2> -> T0) -> ({ (Marker {} -> {{}, {}})
                                  , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                    , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                    }
                                  } -> T0) -> <1> -> T0
                , {{0} -> {1}, <1> -> <0>}
                , { {0} -> { (Marker {} -> {{}, {}})
                           , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             }
                           }
                  , { (Marker {} -> {{}, {}})
                    , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                      , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                      }
                    } -> <0>
                  }
                } -> {1} -> {{}, {}}"#]];
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
    fn monadic_lower_state_get() {
        let db = TestDatabase::default();

        let ir = lower_mon_snippet(
            &db,
            r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))({})"#,
        );

        let pretty_ir = ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                ((let
                  [ (V2 (_row_simple_state_value @ [Ty({}), Ty({})]))
                  , (V3 (_row_simple_return_putget @ [Ty({} -> ({} -> {} -> {{}, {}}) -> {}
                  -> {{}, {}}), Ty({} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}), Ty({} ->
                  {} -> {{}, {}})]))
                  , (V4 (_row_simple_putget_return @ [Ty({} -> {} -> {{}, {}}), Ty({} -> ({}
                  -> {} -> {{}, {}}) -> {} -> {{}, {}}), Ty({} -> ({} -> {} -> {{}, {}}) ->
                  {} -> {{}, {}})]))
                  , (V5 (_row_simple_get_put @ [Ty({} -> ({} -> {} -> {{}, {}}) -> {} ->
                  {{}, {}}), Ty({} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}})]))
                  ]
                  ((__mon_bind @ [Ty({1}), Ty({} -> {{}, {}}), Ty({{}, {}})])
                    (let
                      (V6 (V4[0]
                        (V5[0]
                          (fun [V7, V8, V9] (V8 V9 V9))
                          (fun [V10, V11, V12] (V11 {} V10)))
                        (fun [V13, V14] (V2[0] V14 V13))))
                      ((__mon_freshm @ [Ty({} -> {{}, {}}), Ty({1} -> (Control {1} {} ->
                      {{}, {}}))])
                        (fun [V18]
                          ((__mon_prompt @ [Ty({1}), Ty({0}), Ty({} -> {{}, {}})])
                            V18
                            (fun [V0] (V1[0] V0 {V18, (V4[2][0] V6)}))
                            ((__mon_bind @ [Ty({0}), Ty({}), Ty({} -> {{}, {}})])
                              (let (V15 {})
                                (let (V16 (V1[3][0] V0))
                                  (fun [V0]
                                    <1: {V16[0], (fun [V17] (V16[1][1] V15 V17)), (fun [V0]
                                        V0)}>)))
                              (fun [V21]
                                (let (V22 (V4[3][0] V6 V21)) (fun [V0] <0: V22>))))))))
                    (fun [V23] (let (V24 (V23 {})) (fun [V0] <0: V24>))))) V0)))"#]];
        expect.assert_eq(&pretty_ir);

        let expect_ty = expect![[r#"
            forall ScopedRow .
              forall ScopedRow .
                { {1} -> { (Marker {} -> {{}, {}})
                         , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                           , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                           }
                         } -> {0}
                , forall Type .
                  (<2> -> T0) -> ({ (Marker {} -> {{}, {}})
                                  , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                    , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                                    }
                                  } -> T0) -> <1> -> T0
                , {{0} -> {1}, <1> -> <0>}
                , { {0} -> { (Marker {} -> {{}, {}})
                           , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                             }
                           }
                  , { (Marker {} -> {{}, {}})
                    , { {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                      , {} -> ({} -> {} -> {{}, {}}) -> {} -> {{}, {}}
                      }
                    } -> <0>
                  }
                } -> {1} -> (Control {1} {{}, {}})"#]];
        let pretty_ty = ir
            .type_check(&db)
            .map_err_pretty_with(&db)
            .unwrap()
            .pretty_with(&db)
            .pprint()
            .pretty(80)
            .to_string();
        expect_ty.assert_eq(&pretty_ty);
    }

    #[test]
    fn monadic_lower_applied_wand() {
        let db = TestDatabase::default();
        let module = lower_mon_module(
            &db,
            r#"
            wand = |m| |n| (m ,, n).x
            main = w = wand({ x = {} })({ y = {} }); {}
            "#,
        );

        let items = vec![
            // wand
            expect![[r#"
                (forall
                  [(T5: Type) (T4: ScopedRow) (T3: SimpleRow) (T2: SimpleRow) (T1: SimpleRow) (T0: SimpleRow)]
                  (fun [V1, V2, V3, V4, V0] <0: (V2[3][0] (V1[0] V3 V4))>))"#]],
            // main
            expect![[r#"
                (case ((let
                    [ (V1 (_row_simple_x_y @ [Ty({}), Ty({})]))
                    , (V2 (_row_simple_y_x @ [Ty({}), Ty({})]))
                    ]
                    ((__mon_bind @ [Ty({}), Ty({}), Ty({})])
                      ((wand @ [Ty({}), Eff([]), Data([{}]), Data([{}]), Data([{},{}]), Data([{}])])
                        V1
                        V2
                        {}
                        {})
                      (fun [V5] (let (V6 (let (V3 V5) {})) (fun [V0] <0: V6>))))) {})
                  (fun [V0] V0))"#]],
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
                                 } -> {3} -> {2} -> {4} -> (Control {4} T5)"#]],
            // main
            expect!["{}"],
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
