use aiahr_core::id::{EffectId, EffectOpId};
use aiahr_core::ident::Ident;
use aiahr_core::ir::indexed::{IrTy, MkIrTy};
use aiahr_core::ir::{Ir, IrKind::*, IrVarTy, Kind, P};
use aiahr_core::ty::TyScheme;
use aiahr_core::Top;
use aiahr_core::{
    ast::{Ast, Term},
    id::{IrTyVarId, ItemId, ModuleId, VarId},
    memory::handle::RefHandle,
    ty::{row::ClosedRow, AccessTy, InDb, MkTy, Ty, TypeKind},
};
use aiahr_tc::EffectInfo;
use lower::{ItemSchemes, LowerCtx, TermTys, VarTys};
use rustc_hash::FxHashMap;

pub(crate) mod id_converter;
use id_converter::IdConverter;

pub(crate) mod evidence;

mod lower;

/// Slightly lower level of information than required by EffectInfo.
/// However this is all calculatable off of the effect definition
pub trait IrEffectInfo<'ctx>: EffectInfo<'ctx> {
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
pub struct Jar();
pub trait Db: salsa::DbWithJar<Jar> + aiahr_tc::Db {}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_tc::Db {}

impl<'db> EffectInfo<'db> for &'db dyn crate::Db {
    fn effect_name(&self, module: ModuleId, eff: EffectId) -> Ident {
        self.as_tc_db().effect_name(module, eff)
    }

    fn effect_members(&self, module: ModuleId, eff: EffectId) -> RefHandle<'db, [EffectOpId]> {
        self.as_tc_db().effect_members(module, eff)
    }

    fn lookup_effect_by_member_names(
        &self,
        module: ModuleId,
        members: &[Ident],
    ) -> Option<(ModuleId, EffectId)> {
        self.as_tc_db()
            .lookup_effect_by_member_names(module, members)
    }

    fn lookup_effect_by_name(&self, module: ModuleId, name: Ident) -> Option<(ModuleId, EffectId)> {
        self.as_tc_db().lookup_effect_by_name(module, name)
    }

    fn effect_member_sig(&self, module: ModuleId, eff: EffectId, member: EffectOpId) -> TyScheme {
        self.as_tc_db().effect_member_sig(module, eff, member)
    }

    fn effect_member_name(&self, module: ModuleId, eff: EffectId, member: EffectOpId) -> Ident {
        self.as_tc_db().effect_member_name(module, eff, member)
    }
}
impl<'db> IrEffectInfo<'db> for &'db dyn crate::Db {
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

    fn effect_handler_ir_ty(&self, _mod_id: ModuleId, _eff_id: EffectId) -> IrTy {
        todo!()
    }
}

/// Lower an `Ast` into an `Ir`.
/// TODO: Real documentation.
pub fn lower<'a, 'ctx, Db>(
    db: &'a Db,
    module: ModuleId,
    scheme: &TyScheme,
    ast: &Ast<'ctx, VarId>,
) -> Ir
where
    Db: ItemSchemes<'ctx> + VarTys<'ctx> + TermTys<'ctx> + IrEffectInfo<'ctx> + MkTy<InDb> + 'a,
    Db: AccessTy<'a, InDb> + MkIrTy,
{
    let mut var_conv = IdConverter::new();
    let mut tyvar_conv = IdConverter::new();
    let (mut lower_ctx, ev_locals, ev_params) =
        LowerCtx::new(db, &mut var_conv, &mut tyvar_conv, module)
            .collect_evidence_params(ast.root().row_ev_terms(), scheme.constrs.iter());

    let body = lower_ctx.lower_term(ast.root());
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

    use super::*;

    pub struct LowerDb<'a, 'ctx> {
        pub db: &'a dyn crate::Db,
        var_tys: FxHashMap<VarId, Ty>,
        term_tys: FxHashMap<&'ctx Term<'ctx, VarId>, TyChkRes<InDb>>,
        eff_info: DummyEff<'a>,
    }

    impl<'a, 'ctx> LowerDb<'a, 'ctx> {
        pub fn new(
            db: &'a dyn crate::Db,
            var_tys: FxHashMap<VarId, Ty>,
            term_tys: FxHashMap<&'ctx Term<'ctx, VarId>, TyChkRes<InDb>>,
        ) -> Self {
            Self {
                db,
                var_tys,
                term_tys,
                eff_info: DummyEff(db.as_core_db()),
            }
        }
    }
    impl<'ctx> VarTys<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_var(&self, var_id: VarId) -> Ty {
            self.var_tys[&var_id]
        }
    }
    impl<'ctx> TermTys<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_term(&self, term: &'ctx Term<'ctx, VarId>) -> TyChkRes<InDb> {
            self.term_tys[term]
        }
    }
    impl<'ctx> ItemSchemes<'ctx> for LowerDb<'_, 'ctx> {
        fn lookup_scheme(&self, _module_id: ModuleId, _item_id: ItemId) -> TyScheme {
            todo!()
        }
    }
    impl<'ctx> EffectInfo<'ctx> for LowerDb<'_, 'ctx> {
        fn effect_name(&self, mod_id: ModuleId, eff: aiahr_core::id::EffectId) -> Ident {
            self.eff_info.effect_name(mod_id, eff)
        }

        fn effect_members(
            &self,
            mod_id: ModuleId,
            eff: aiahr_core::id::EffectId,
        ) -> RefHandle<'static, [aiahr_core::id::EffectOpId]> {
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

    impl<'ctx> IrEffectInfo<'ctx> for LowerDb<'_, 'ctx> {
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

    impl<'a> AccessTy<'a, InDb> for LowerDb<'a, '_> {
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
    impl<'a> AccessTy<'a, InDb> for &LowerDb<'a, '_> {
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
    impl MkIrTy for LowerDb<'_, '_> {
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

    impl MkTy<InDb> for LowerDb<'_, '_> {
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
    use aiahr_analysis::names::Names;
    use aiahr_analysis::resolve::resolve_term;
    use aiahr_analysis::top_level::BaseBuilder;
    use aiahr_core::ast::{Ast, Direction, Term};
    use aiahr_core::id::{ModuleId, VarId};
    use aiahr_core::ir::indexed::IrTyKind::*;
    use aiahr_core::ir::IrKind::{self, *};
    use aiahr_core::modules::ModuleTree;
    use aiahr_core::ty::TyScheme;
    use aiahr_core::Db;
    use aiahr_tc::test_utils::DummyEff;
    use aiahr_test::ast::*;
    use aiahr_test::nst::random_term_item;
    use assert_matches::assert_matches;
    use bumpalo::Bump;
    use ir_matcher::ir_matcher;
    use rustc_hash::FxHashMap;

    const MODNAME: &str = "test_module";
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
        arena: &'ctx Bump,
        input: &str,
    ) -> (LowerDb<'a, 'ctx>, TyScheme, Ast<'ctx, VarId>) {
        let (m, modules) = {
            let mut modules = ModuleTree::new();
            let m = modules.add_package(db.ident_str(MODNAME));
            (m, modules)
        };

        let unresolved = aiahr_parser::parser::test_utils::parse_term(db, arena, input);
        let mut errors: Vec<aiahr_core::diagnostic::nameres::NameResolutionError> = Vec::new();

        let mut module_names = FxHashMap::default();
        let base = BaseBuilder::new().build(arena, m, &modules, &mut module_names);
        let mut names = Names::new(&base);

        let resolved = resolve_term(arena, unresolved, &mut names, &mut errors)
            .expect("Name resolution to succeed");

        let (vars, ty_vars) = names.into_ids();
        let mut vars = vars.into_iter().map(|_| false).collect();
        let mut ty_vars = ty_vars.into_iter().map(|_| false).collect();

        let ast = aiahr_desugar::desugar(
            db,
            arena,
            &mut vars,
            &mut ty_vars,
            random_term_item(resolved),
        )
        .unwrap()
        .unwrap_func();

        let (var_tys, term_tys, scheme, _) =
            aiahr_tc::type_check(db, &DummyEff(db), ModuleId(0), &ast);
        (LowerDb::new(db, var_tys, term_tys), scheme, ast)
    }

    #[test]
    fn lower_id() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (db, scheme, ast) = compile_upto_lower(&db, &arena, "|x| x");

        let ir = lower(&db, MOD, &scheme, &ast);

        ir_matcher!(ir, TyAbs([ty_var], Abs([var], Var(var))) => {
            assert_eq!(var.ty.kind(db.db.as_core_db()), VarTy(*ty_var));
        })
    }

    #[test]
    fn lower_product_literal() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let (db, scheme, ast) = compile_upto_lower(&db, &arena, "|a| { x = a, y = a }");

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
        let arena = Bump::new();
        let db = TestDatabase::default();
        let m = VarId(0);
        let n = VarId(1);
        let ast = AstBuilder::with_builder(&db, &arena, |builder| {
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
        let (var_tys, term_ress, scheme, _) =
            aiahr_tc::type_check(&db, &aiahr_tc::test_utils::DummyEff(&db), MOD, &ast);

        let ir = lower(&LowerDb::new(&db, var_tys, term_ress), MOD, &scheme, &ast);

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
