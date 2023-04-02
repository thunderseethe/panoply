use aiahr_analysis::name::ModuleName;
use aiahr_core::ast::AstModule;
use aiahr_core::id::{EffectId, EffectOpId};
use aiahr_core::ident::Ident;
use aiahr_core::ir::indexed::{IrTy, IrTyKind, MkIrTy};
use aiahr_core::ir::{Ir, IrKind::*, IrVarTy, Kind, P};
use aiahr_core::modules::Module;
use aiahr_core::ty::TyScheme;
use aiahr_core::Top;
use aiahr_core::{
    ast::indexed::{Ast, Term},
    id::{IrTyVarId, ItemId, ModuleId, VarId},
    ty::{row::ClosedRow, AccessTy, InDb, MkTy, Ty, TypeKind},
};
use aiahr_tc::EffectInfo;
use la_arena::Idx;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};
use rustc_hash::FxHashMap;

pub(crate) mod id_converter;
use id_converter::IdConverter;

use self::lower::ItemSelector;

pub(crate) mod evidence;

mod lower;

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait IrEffectInfo: EffectInfo {
    fn effect_handler_return_index(&self, mod_id: ModuleId, eff_id: EffectId) -> usize;
    fn effect_handler_op_index(
        &self,
        mod_id: ModuleId,
        eff_id: EffectId,
        op_id: EffectOpId,
    ) -> usize;
    fn effect_vector_index(&self, mod_id: ModuleId, eff_id: EffectId) -> usize;

    fn effect_handler_ir_ty(&self, mod_id: ModuleId, eff_id: EffectId) -> IrTy;
}

#[salsa::jar(db = Db)]
pub struct Jar(
    IrModule,
    IrItem,
    lower_module,
    lower_item,
    effect_handler_ir_ty,
);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_tc::Db {
    fn as_lower_ir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn lower_module(&self, module: AstModule) -> IrModule {
        lower_module(self.as_lower_ir_db(), module)
    }

    fn lower_item(&self, module_id: ModuleId, item_id: ItemId) -> IrItem {
        lower_item(self.as_lower_ir_db(), self.top(), module_id, item_id)
    }

    fn lower_module_of(&self, module_id: ModuleId) -> IrModule {
        let module = aiahr_core::modules::module_of(self.as_core_db(), self.top(), module_id);
        let ast_module = self.desugar_module_of(module);
        self.lower_module(ast_module)
    }

    fn lower_module_for_path(&self, path: std::path::PathBuf) -> IrModule {
        let module_id = aiahr_core::file::module_id_for_path(self.as_core_db(), self.top(), path);
        self.lower_module_of(module_id)
    }

    fn lower_item_for_file_name(&self, path: std::path::PathBuf, item: Ident) -> Option<IrItem> {
        let module_id = aiahr_core::file::module_id_for_path(self.as_core_db(), self.top(), path);
        let item_id = self.id_for_name(module_id, item)?;
        Some(self.lower_item(module_id, item_id))
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_tc::Db {}

#[salsa::tracked]
pub struct IrModule {
    #[id]
    pub module: Module,
    #[return_ref]
    pub items: Vec<IrItem>,
}

#[salsa::tracked]
pub struct IrItem {
    #[id]
    pub name: ModuleName,
    #[return_ref]
    pub item: Ir,
}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: AstModule) -> IrModule {
    let core_db = db.as_core_db();
    let items = module
        .items(core_db)
        .iter()
        .map(|salsa_item| {
            let ast_item = salsa_item.item(core_db);
            match ast_item {
                aiahr_core::ast::indexed::Item::Effect(_) => todo!(),
                aiahr_core::ast::indexed::Item::Function(ast) => {
                    let mod_id = module.module(core_db).name(core_db);
                    let scheme = db.lookup_scheme(mod_id, ast.name);
                    let ir = lower(db, mod_id, &scheme, &ast);
                    IrItem::new(db, ModuleName::from(ast.name), ir)
                }
            }
        })
        .collect();
    IrModule::new(db, module.module(core_db), items)
}

#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, _top: Top, module_id: ModuleId, item_id: ItemId) -> IrItem {
    let ir_module = db.lower_module_of(module_id);
    *ir_module
        .items(db)
        .iter()
        .find(|item| item.name(db) == ModuleName::from(item_id))
        .unwrap_or_else(|| panic!("ICE: No IrItem constructed for ItemId {:?}", item_id))
}

#[salsa::tracked]
fn effect_handler_ir_ty(
    db: &dyn crate::Db,
    _top: Top,
    module_id: ModuleId,
    effect_id: EffectId,
) -> IrTy {
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let bogus_item = ItemId(usize::MAX);
    // TODO: Clean this up so we can access lower ty without requiring a full LowerCtx
    // TODO: Do we need to create a new tyvar_conv per scheme we lower?
    let mut lower_ctx = LowerCtx::new(
        db.as_lower_ir_db(),
        &mut var_conv,
        &mut tyvar_conv,
        ItemSelector {
            module: module_id,
            item: bogus_item,
        },
    );

    // TODO: Produce members in order so we don't have to sort or get names here.
    let mut members = db
        .effect_members(module_id, effect_id)
        .into_iter()
        .map(|op_id| {
            let scheme = db.effect_member_sig(module_id, effect_id, *op_id);
            (
                db.effect_member_name(module_id, effect_id, *op_id),
                lower_ctx.lower_scheme(&scheme),
            )
        })
        .collect::<Vec<_>>();
    members.sort_by(|a, b| a.0.cmp(&b.0));
    let handler_ty = db.mk_prod_ty(
        members
            .into_iter()
            .map(|(_, ir_ty)| ir_ty)
            .collect::<Vec<_>>()
            .as_slice(),
    );
    db.mk_prod_ty(&[db.mk_ir_ty(IrTyKind::IntTy), handler_ty])
}

impl<DB> IrEffectInfo for DB
where
    DB: ?Sized + crate::Db,
{
    fn effect_handler_return_index(&self, mod_id: ModuleId, eff_id: EffectId) -> usize {
        aiahr_analysis::effect_handler_return_index(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            mod_id,
            eff_id,
        )
    }

    fn effect_handler_op_index(
        &self,
        mod_id: ModuleId,
        eff_id: EffectId,
        op_id: EffectOpId,
    ) -> usize {
        aiahr_analysis::effect_handler_op_index(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            mod_id,
            eff_id,
            op_id,
        )
    }

    fn effect_vector_index(&self, mod_id: ModuleId, eff_id: EffectId) -> usize {
        aiahr_analysis::effect_vector_index(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            mod_id,
            eff_id,
        )
    }

    fn effect_handler_ir_ty(&self, mod_id: ModuleId, eff_id: EffectId) -> IrTy {
        effect_handler_ir_ty(self.as_lower_ir_db(), self.top(), mod_id, eff_id)
    }
}
impl<DB> ItemSchemes for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_scheme(&self, module_id: ModuleId, item_id: ItemId) -> TyScheme {
        let typed_item = aiahr_tc::type_scheme_of(
            self.as_tc_db(),
            Top::new(self.as_core_db()),
            module_id,
            item_id,
        );
        typed_item.ty_scheme(self.as_tc_db())
    }
}
impl<DB> VarTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_var(&self, module_id: ModuleId, item_id: ItemId, var_id: VarId) -> Ty {
        let typed_item = aiahr_tc::type_scheme_of(
            self.as_tc_db(),
            Top::new(self.as_core_db()),
            module_id,
            item_id,
        );
        typed_item.var_to_tys(self.as_tc_db())[&var_id]
    }
}
impl<DB> TermTys for DB
where
    DB: ?Sized + crate::Db,
{
    fn lookup_term(
        &self,
        module_id: ModuleId,
        item_id: ItemId,
        term: Idx<Term<VarId>>,
    ) -> aiahr_tc::TyChkRes<InDb> {
        let typed_item = aiahr_tc::type_scheme_of(
            self.as_tc_db(),
            Top::new(self.as_core_db()),
            module_id,
            item_id,
        );
        typed_item.term_to_tys(self.as_tc_db())[&term]
    }
}

/// Lower an `Ast` into an `Ir`.
/// TODO: Real documentation.
pub fn lower<'a, 'ctx, Db>(db: &'a Db, module: ModuleId, scheme: &TyScheme, ast: &Ast<VarId>) -> Ir
where
    Db: ?Sized + ItemSchemes + VarTys + TermTys + MkTy<InDb> + MkIrTy + IrEffectInfo + 'a,
    &'a Db: AccessTy<'a, InDb>,
{
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let (mut lower_ctx, ev_locals, ev_params) = LowerCtx::new(
        db,
        &mut var_conv,
        &mut tyvar_conv,
        ItemSelector {
            module,
            item: ast.name,
        },
    )
    .collect_evidence_params(ast.root().row_ev_terms(ast.arena()), scheme.constrs.iter());

    let body = lower_ctx.lower_term(ast, ast.tree);
    // Bind all unique solved row evidence to local variables at top of the term
    let body = ev_locals.into_iter().fold(body, |body, (ev_param, ev_ir)| {
        Ir::app(Ir::abss([ev_param], body), [ev_ir])
    });
    // Add unsolved row evidence as parameters of the term
    let body = ev_params
        .into_iter()
        .rfold(body, |body, arg| Ir::new(Abs(arg, P::new(body))));

    // Finally wrap our term in any type variables it needs to bind
    scheme.bound.iter().rfold(body, |acc, ty_var| {
        Ir::new(TyAbs(
            IrVarTy {
                var: tyvar_conv.convert(*ty_var),
                kind: Kind::Type,
            },
            P::new(acc),
        ))
    })
}

pub mod test_utils {
    use aiahr_core::ty::{TyData, TypeAlloc};
    use aiahr_tc::test_utils::DummyEff;
    use aiahr_tc::TyChkRes;
    use la_arena::Idx;

    use super::*;

    pub struct LowerDb<'a> {
        pub db: &'a dyn crate::Db,
        var_tys: FxHashMap<VarId, Ty>,
        term_tys: FxHashMap<Idx<Term<VarId>>, TyChkRes<InDb>>,
        eff_info: DummyEff<'a>,
    }

    impl<'a> LowerDb<'a> {
        pub fn new(
            db: &'a dyn crate::Db,
            var_tys: FxHashMap<VarId, Ty>,
            term_tys: FxHashMap<Idx<Term<VarId>>, TyChkRes<InDb>>,
        ) -> Self {
            Self {
                db,
                var_tys,
                term_tys,
                eff_info: DummyEff(db.as_core_db()),
            }
        }
    }
    impl VarTys for LowerDb<'_> {
        fn lookup_var(&self, _module_id: ModuleId, _item_id: ItemId, var_id: VarId) -> Ty {
            self.var_tys[&var_id]
        }
    }
    impl TermTys for LowerDb<'_> {
        fn lookup_term(
            &self,
            _module_id: ModuleId,
            _item_id: ItemId,
            term: Idx<Term<VarId>>,
        ) -> TyChkRes<InDb> {
            self.term_tys[&term]
        }
    }
    impl ItemSchemes for LowerDb<'_> {
        fn lookup_scheme(&self, _module_id: ModuleId, _item_id: ItemId) -> TyScheme {
            todo!()
        }
    }
    impl EffectInfo for LowerDb<'_> {
        fn effect_name(&self, mod_id: ModuleId, eff: aiahr_core::id::EffectId) -> Ident {
            self.eff_info.effect_name(mod_id, eff)
        }

        fn effect_members(
            &self,
            mod_id: ModuleId,
            eff: aiahr_core::id::EffectId,
        ) -> &[aiahr_core::id::EffectOpId] {
            self.eff_info.effect_members(mod_id, eff)
        }

        fn effect_member_sig(
            &self,
            module: ModuleId,
            eff: aiahr_core::id::EffectId,
            member: aiahr_core::id::EffectOpId,
        ) -> TyScheme {
            self.eff_info.effect_member_sig(module, eff, member)
        }

        fn effect_member_name(
            &self,
            module: ModuleId,
            eff: aiahr_core::id::EffectId,
            member: aiahr_core::id::EffectOpId,
        ) -> Ident {
            self.eff_info.effect_member_name(module, eff, member)
        }

        fn lookup_effect_by_member_names<'a>(
            &self,
            module: ModuleId,
            members: &[Ident],
        ) -> Option<(ModuleId, EffectId)> {
            self.eff_info.lookup_effect_by_member_names(module, members)
        }

        fn lookup_effect_by_name(
            &self,
            module: ModuleId,
            name: Ident,
        ) -> Option<(ModuleId, EffectId)> {
            self.eff_info.lookup_effect_by_name(module, name)
        }
    }

    impl IrEffectInfo for LowerDb<'_> {
        fn effect_handler_return_index(&self, _: ModuleId, eff_id: EffectId) -> usize {
            match eff_id {
                DummyEff::STATE_ID => 2,
                DummyEff::READER_ID => 1,
                _ => unimplemented!(),
            }
        }

        fn effect_handler_op_index(
            &self,
            _mod_id: ModuleId,
            _eff_id: EffectId,
            op_id: EffectOpId,
        ) -> usize {
            match op_id {
                DummyEff::GET_ID | DummyEff::ASK_ID => 0,
                DummyEff::PUT_ID => 1,
                _ => unimplemented!(),
            }
        }

        fn effect_vector_index(&self, _: ModuleId, eff_id: EffectId) -> usize {
            match eff_id {
                DummyEff::STATE_ID => 0,
                DummyEff::READER_ID => 1,
                _ => unimplemented!(),
            }
        }

        fn effect_handler_ir_ty(&self, _mod_id: ModuleId, eff_id: EffectId) -> IrTy {
            use aiahr_core::ir::indexed::IrTyKind::*;
            // Find a better solution for this
            // Guessing a big enough irvartyid that we won't hit it in tests is flaky.
            static R: IrVarTy = IrVarTy {
                var: IrTyVarId(1024),
                kind: Kind::Type,
            };
            static A: IrVarTy = IrVarTy {
                var: IrTyVarId(1025),
                kind: Kind::Type,
            };
            let ret_ty: IrTy = self.db.mk_ir_ty(VarTy(R));
            let unit_ty: IrTy = self.db.mk_prod_ty(&[]);
            let int_ty: IrTy = self.db.mk_ir_ty(IntTy);
            let unit_kont_ty: IrTy = self.db.mk_ir_ty(FunTy(unit_ty, ret_ty));
            let int_kont_ty: IrTy = self.db.mk_ir_ty(FunTy(int_ty, ret_ty));

            let return_ty: IrTy = self.db.mk_ir_ty(ForallTy(
                A,
                self.db.mk_ir_ty(FunTy(
                    self.db.mk_ir_ty(VarTy(A)),
                    self.db.mk_ir_ty(VarTy(R)),
                )),
            ));
            let state_handler_ty: IrTy = self.db.mk_ir_ty(ForallTy(
                R,
                self.db.mk_prod_ty(&[
                    self.db.mk_ir_ty(FunTy(
                        unit_ty,
                        self.db.mk_ir_ty(FunTy(unit_kont_ty, ret_ty)),
                    )),
                    self.db
                        .mk_ir_ty(FunTy(int_ty, self.db.mk_ir_ty(FunTy(int_kont_ty, ret_ty)))),
                    return_ty,
                ]),
            ));

            let reader_handler_ty: IrTy = self.db.mk_ir_ty(ForallTy(
                R,
                self.db.mk_prod_ty(&[
                    self.db.mk_ir_ty(FunTy(
                        unit_ty,
                        self.db.mk_ir_ty(FunTy(unit_kont_ty, ret_ty)),
                    )),
                    return_ty,
                ]),
            ));

            match eff_id {
                DummyEff::STATE_ID => state_handler_ty,
                DummyEff::READER_ID => reader_handler_ty,
                _ => unimplemented!(),
            }
        }
    }

    impl<'a> AccessTy<'a, InDb> for LowerDb<'a> {
        fn kind(&self, ty: &Ty<InDb>) -> &'a TypeKind<InDb> {
            self.db.as_tc_db().kind(ty)
        }

        fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'a [Ident] {
            self.db.as_tc_db().row_fields(row)
        }

        fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'a [Ty<InDb>] {
            self.db.as_tc_db().row_values(row)
        }
    }
    impl<'a> AccessTy<'a, InDb> for &LowerDb<'a> {
        fn kind(&self, ty: &Ty<InDb>) -> &'a TypeKind<InDb> {
            self.db.as_tc_db().kind(ty)
        }

        fn row_fields(&self, row: &<InDb as TypeAlloc>::RowFields) -> &'a [Ident] {
            self.db.as_tc_db().row_fields(row)
        }

        fn row_values(&self, row: &<InDb as TypeAlloc>::RowValues) -> &'a [Ty<InDb>] {
            self.db.as_tc_db().row_values(row)
        }
    }
    impl MkIrTy for LowerDb<'_> {
        fn mk_ir_ty(&self, kind: aiahr_core::ir::indexed::IrTyKind) -> IrTy {
            self.db.mk_ir_ty(kind)
        }

        fn mk_prod_ty(&self, elems: &[IrTy]) -> IrTy {
            self.db.mk_prod_ty(elems)
        }

        fn mk_coprod_ty(&self, elems: &[IrTy]) -> IrTy {
            self.db.mk_coprod_ty(elems)
        }
    }

    impl MkTy<InDb> for LowerDb<'_> {
        fn mk_ty(&self, kind: TypeKind<InDb>) -> Ty<InDb> {
            Ty(TyData::new(self.db.as_core_db(), kind))
        }

        fn mk_label(&self, label: &str) -> Ident {
            self.db.ident_str(label)
        }

        fn mk_row(&self, fields: &[Ident], values: &[Ty<InDb>]) -> ClosedRow<InDb> {
            self.db.as_core_db().mk_row(fields, values)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use crate::lower;

    use super::test_utils::LowerDb;
    use aiahr_core::ast::indexed::{Ast, Term};
    use aiahr_core::ast::Direction;
    use aiahr_core::file::{SourceFile, SourceFileSet};
    use aiahr_core::id::{ItemId, ModuleId, VarId};
    use aiahr_core::ir::indexed::IrTyKind::*;
    use aiahr_core::ir::IrKind::{self, *};
    use aiahr_core::ty::TyScheme;
    use aiahr_core::Top;
    use aiahr_tc::type_scheme_of;
    use aiahr_test::ast::{AstBuilder, MkTerm};
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use ir_matcher::ir_matcher;

    const MOD: ModuleId = ModuleId(0);

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_core::Jar,
        aiahr_tc::Jar,
        aiahr_desugar::Jar,
        aiahr_analysis::Jar,
        aiahr_parser::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    /// Compile an input string up to (but not including) the lower stage.
    fn compile_upto_lower<'a: 'ctx, 'ctx>(
        db: &'a TestDatabase,
        input: &str,
    ) -> (LowerDb<'a>, TyScheme, Ast<VarId>) {
        let file = SourceFile::new(
            db,
            ModuleId(0),
            std::path::PathBuf::from("test.aiahr"),
            input.to_string(),
        );
        SourceFileSet::new(db, vec![file]);

        let top = Top::new(db);
        let salsa_typed_item = type_scheme_of(db, top, MOD, ItemId(0));
        let item = aiahr_desugar::desugar_item_of_id(db, top, MOD, ItemId(0));
        let ast = match item.item(db) {
            aiahr_core::ast::indexed::Item::Effect(_) => unreachable!(),
            aiahr_core::ast::indexed::Item::Function(ast) => ast,
        };
        (
            LowerDb::new(
                db,
                salsa_typed_item.var_to_tys(db).clone(),
                salsa_typed_item.term_to_tys(db).clone(),
            ),
            salsa_typed_item.ty_scheme(db),
            ast,
        )
    }

    #[test]
    fn lower_id() {
        let db = TestDatabase::default();
        let (db, scheme, ast) = compile_upto_lower(&db, "id = |x| x");

        let ir = lower(&db, MOD, &scheme, &ast);

        ir_matcher!(ir, TyAbs([ty_var], Abs([var], Var(var))) => {
            assert_eq!(var.ty.kind(db.db.as_core_db()), VarTy(*ty_var));
        })
    }

    #[test]
    fn lower_product_literal() {
        let db = TestDatabase::default();
        let (db, scheme, ast) = compile_upto_lower(&db, "f = |a| { x = a, y = a }");

        let ir = lower(&db, MOD, &scheme, &ast);
        ir_matcher!(ir,
            TyAbs([_ty_var],
                  App([
                      Abs([ev, a], App([FieldProj(0, Var(ev)), Var(a), Var(a)])),
                      Struct(ev_terms)])) => {
            let ir = &ev_terms[0];
            ir_matcher!(ir, Abs([m, n], Struct(splat)) => {
                assert_matches!(splat[0].deref().kind, Var(_m) => { assert_eq!(*m, _m); });
                assert_matches!(splat[1].deref().kind, Var(_n) => { assert_eq!(*n, _n); });
            });
        });
    }

    #[test]
    fn lower_wand() {
        let db = TestDatabase::default();
        let m = VarId(0);
        let n = VarId(1);
        let ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abss(
                [m, n],
                builder.mk_unlabel(
                    "x",
                    builder.mk_project(
                        Direction::Left,
                        builder.mk_concat(Term::Variable(m), Term::Variable(n)),
                    ),
                ),
            )
        });
        let arena = Bump::new();
        let (ref_ast, map) = ast.ref_alloc(&arena);
        let (var_tys, term_tys, scheme, _) =
            aiahr_tc::type_check(&db, &aiahr_tc::test_utils::DummyEff(&db), MOD, &ref_ast);

        //let (db, scheme, ast) = compile_upto_lower(&db, "wand m n = {m, n}.x");
        let term_tys = term_tys
            .into_iter()
            .map(|(key, value)| (map[&key], value))
            .collect();
        let ir = lower(&LowerDb::new(&db, var_tys, term_tys), MOD, &scheme, &ast);
        ir_matcher!(
            ir,
            TyAbs(
                [_a, _b, _c, _d, _e],
                Abs(
                    [w, x, y, z],
                    App([
                        FieldProj(0, FieldProj(2, Var(w))),
                        App([FieldProj(0, Var(x)), Var(y), Var(z)])
                    ])
                )
            )
        );
    }
}
