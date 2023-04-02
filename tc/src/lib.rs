use aiahr_core::{
    ast::{self, Ast, Term},
    id::{EffectId, EffectOpId, Id, ItemId, ModuleId, TyVarId, VarId},
    ident::Ident,
    modules::module_of,
    ty::row::Row,
    Top,
};
use bumpalo::Bump;
use diagnostic::TypeCheckDiagnostic;
use ena::unify::{InPlaceUnificationTable, UnifyKey};
use la_arena::Idx;
use rustc_hash::FxHashMap;

use aiahr_core::ty::infer::{InArena, TcUnifierVar, TyCtx};
use aiahr_core::ty::*;

mod unsolved_row;

use crate::infer::InferCtx;

mod diagnostic;

pub(crate) mod folds;
use folds::zonker::Zonker;

mod infer;
pub use infer::TyChkRes;

/// Information we need about effects during type checking
pub trait EffectInfo {
    /// Lookup the name of an effect from it's ID
    fn effect_name(&self, module: ModuleId, eff: EffectId) -> Ident;
    /// Lookup effect members from it's ID
    fn effect_members(&self, module: ModuleId, eff: EffectId) -> &[EffectOpId];

    /// Look up an effect by the name of it's members, this may fail if an invalid list of member
    /// names is passed.
    fn lookup_effect_by_member_names(
        &self,
        module: ModuleId,
        members: &[Ident],
    ) -> Option<(ModuleId, EffectId)>;
    fn lookup_effect_by_name(&self, module: ModuleId, name: Ident) -> Option<(ModuleId, EffectId)>;
    /// Lookup the type signature of an effect's member
    fn effect_member_sig(&self, module: ModuleId, eff: EffectId, member: EffectOpId) -> TyScheme;
    /// Lookup the name of an effect's member
    fn effect_member_name(&self, module: ModuleId, eff: EffectId, member: EffectOpId) -> Ident;
}

#[salsa::jar(db = Db)]

pub struct Jar(SalsaTypedItem, type_scheme_of);
pub trait Db:
    salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_analysis::Db + aiahr_desugar::Db
{
    fn as_tc_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }
}
impl<DB> Db for DB where
    DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_core::Db + aiahr_analysis::Db + aiahr_desugar::Db
{
}

impl<DB> EffectInfo for DB
where
    DB: ?Sized + crate::Db,
{
    fn effect_name(&self, module: ModuleId, eff: EffectId) -> Ident {
        aiahr_analysis::effect_name(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            module,
            eff,
        )
    }

    fn effect_members(&self, module: ModuleId, eff: EffectId) -> &[EffectOpId] {
        aiahr_analysis::effect_members(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            module,
            eff,
        )
        .as_slice()
    }

    fn lookup_effect_by_member_names(
        &self,
        module: ModuleId,
        members: &[Ident],
    ) -> Option<(ModuleId, EffectId)> {
        aiahr_analysis::lookup_effect_by_member_names(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            module,
            members.to_vec().into_boxed_slice(),
        )
    }

    fn lookup_effect_by_name(&self, module: ModuleId, name: Ident) -> Option<(ModuleId, EffectId)> {
        aiahr_analysis::lookup_effect_by_name(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            module,
            name,
        )
    }

    fn effect_member_sig(&self, module: ModuleId, eff: EffectId, member: EffectOpId) -> TyScheme {
        aiahr_desugar::effect_op_tyscheme_of(
            self.as_desugar_db(),
            Top::new(self.as_core_db()),
            module,
            eff,
            member,
        )
    }

    fn effect_member_name(&self, module: ModuleId, eff: EffectId, member: EffectOpId) -> Ident {
        aiahr_analysis::effect_member_name(
            self.as_analysis_db(),
            Top::new(self.as_core_db()),
            module,
            eff,
            member,
        )
    }
}

#[salsa::tracked]
pub struct SalsaTypedItem {
    #[id]
    pub item_id: ItemId,
    #[return_ref]
    pub var_to_tys: FxHashMap<VarId, Ty<InDb>>,
    #[return_ref]
    pub term_to_tys: FxHashMap<Idx<ast::indexed::Term<VarId>>, TyChkRes<InDb>>,
    pub ty_scheme: TyScheme,
}

#[salsa::tracked]
pub fn type_scheme_of(
    db: &dyn crate::Db,
    top: Top,
    module_id: ModuleId,
    item_id: ItemId,
) -> SalsaTypedItem {
    let core_db = db.as_core_db();
    let module = module_of(core_db, top, module_id);
    let ast_module = db.desugar_module_of(module);
    let ast = ast_module
        .items(core_db)
        .iter()
        .find_map(|item| match item.item(core_db) {
            ast::indexed::Item::Effect(_) => None,
            ast::indexed::Item::Function(ast) => (ast.name == item_id).then_some(ast),
        })
        .unwrap_or_else(|| {
            dbg!(ast_module.items(db.as_core_db()));
            panic!(
                "ICE: Constructed ItemId {:?} without associated item",
                item_id
            )
        });

    let ref_arena = Bump::new();
    let (ast, ref_to_indx) = ast.ref_alloc(&ref_arena);
    let (var_to_tys, terms_to_tys, ty_scheme, diags) = type_check(db, db, module_id, &ast);

    //TODO: Push errors to diagnostic
    drop(diags);
    SalsaTypedItem::new(
        db,
        item_id,
        var_to_tys,
        terms_to_tys
            .into_iter()
            .map(|(ref_term, value)| (ref_to_indx[ref_term], value))
            .collect(),
        ty_scheme,
    )
}

pub fn type_check<'ty, 's, 'eff, E>(
    db: &dyn crate::Db,
    eff_info: &E,
    module: ModuleId,
    ast: &Ast<'ty, VarId>,
) -> (
    FxHashMap<VarId, Ty<InDb>>,
    FxHashMap<&'ty Term<'ty, VarId>, TyChkRes<InDb>>,
    TyScheme,
    Vec<TypeCheckDiagnostic>,
)
where
    E: ?Sized + EffectInfo,
{
    let arena = Bump::new();
    let infer_ctx = TyCtx::new(db.as_core_db(), &arena);
    tc_term(db, &infer_ctx, eff_info, module, ast)
}

fn tc_term<'ty, 'infer, 's, 'eff, II, E>(
    db: &dyn crate::Db,
    infer_ctx: &II,
    eff_info: &E,
    module: ModuleId,
    ast: &Ast<'ty, VarId>,
) -> (
    FxHashMap<VarId, Ty<InDb>>,
    FxHashMap<&'ty Term<'ty, VarId>, TyChkRes<InDb>>,
    TyScheme,
    Vec<TypeCheckDiagnostic>,
)
where
    II: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
    E: ?Sized + EffectInfo,
{
    let term = ast.root();
    let infer = InferCtx::new(db, infer_ctx, module, ast);

    // Infer types for all our variables and the root term.
    let (infer, gen_storage, result) = infer.infer(eff_info, term);

    // Solve constraints into the unifiers mapping.
    let (mut unifiers, unsolved_eqs, errors) = infer.solve(eff_info);

    //print_root_unifiers(&mut unifiers);
    // Zonk the variable -> type mapping and the root term type.
    let mut zonker = Zonker {
        ctx: db,
        unifiers: &mut unifiers,
        free_vars: vec![],
    };
    let zonked_infer = result.try_fold_with(&mut zonker).unwrap();

    let zonked_var_tys = gen_storage
        .var_tys
        .into_iter()
        .map(|(var, ty)| (var, ty.try_fold_with(&mut zonker).unwrap()))
        .collect::<FxHashMap<_, _>>();

    let zonked_term_tys = gen_storage
        .term_tys
        .into_iter()
        .map(|(term, ty)| (term, ty.try_fold_with(&mut zonker).unwrap()))
        .collect::<FxHashMap<_, _>>();

    let constrs = unsolved_eqs
        .into_iter()
        .map(|eq| Evidence::from(eq).try_fold_with(&mut zonker).unwrap())
        .collect();

    let scheme = TyScheme {
        bound: zonker
            .free_vars
            .into_iter()
            .enumerate()
            .map(|(i, _)| TyVarId::from_raw(i))
            .collect(),
        constrs,
        eff: zonked_infer.eff,
        ty: zonked_infer.ty,
    };
    (zonked_var_tys, zonked_term_tys, scheme, errors)
}

#[allow(dead_code)]
fn print_root_unifiers(uni: &mut InPlaceUnificationTable<TcUnifierVar<'_>>) {
    println!("UnificationTable [");
    for uv in (0..uni.len()).map(|i| TcUnifierVar::from_index(i as u32)) {
        let root = uni.find(uv);
        if root != uv {
            continue;
        }
        if let Some(candidate) = uni.probe_value(root) {
            println!("\t{} => {:?}", uv.index(), candidate);
        }
    }
    println!("]");
}

pub mod test_utils {
    use aiahr_core::id::{EffectId, EffectOpId, ModuleId, TyVarId};
    use aiahr_core::ident::Ident;

    use crate::{EffectInfo, MkTy, Row, TyScheme};

    // Utility trait to remove a lot of the intermediate allocation when creating ASTs
    // Helps make tests a little more readable

    pub struct DummyEff<'a>(pub &'a dyn aiahr_core::Db);
    impl<'a> DummyEff<'a> {
        pub const STATE_ID: EffectId = EffectId(0);
        pub const READER_ID: EffectId = EffectId(1);

        pub const GET_ID: EffectOpId = EffectOpId(0);
        pub const PUT_ID: EffectOpId = EffectOpId(1);
        pub const ASK_ID: EffectOpId = EffectOpId(2);
    }
    impl<'ctx> EffectInfo for DummyEff<'_> {
        fn effect_name(&self, _: ModuleId, eff: EffectId) -> Ident {
            match eff {
                DummyEff::STATE_ID => self.0.ident_str("State"),
                DummyEff::READER_ID => self.0.ident_str("Reader"),
                _ => unimplemented!(),
            }
        }

        fn effect_members(&self, _: ModuleId, eff: EffectId) -> &[EffectOpId] {
            match eff {
                DummyEff::STATE_ID => &[DummyEff::GET_ID, DummyEff::PUT_ID],
                DummyEff::READER_ID => &[DummyEff::ASK_ID],
                _ => unimplemented!(),
            }
        }

        fn lookup_effect_by_member_names<'a>(
            &self,
            _: ModuleId,
            members: &[Ident],
        ) -> Option<(ModuleId, EffectId)> {
            members
                .get(0)
                .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                    "ask" => Some((ModuleId(0), DummyEff::READER_ID)),
                    "get" => {
                        members
                            .get(1)
                            .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                                "put" => Some((ModuleId(0), DummyEff::STATE_ID)),
                                _ => None,
                            })
                    }
                    "put" => {
                        members
                            .get(1)
                            .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                                "get" => Some((ModuleId(0), DummyEff::STATE_ID)),
                                _ => None,
                            })
                    }
                    _ => None,
                })
        }

        fn lookup_effect_by_name(&self, _: ModuleId, name: Ident) -> Option<(ModuleId, EffectId)> {
            match name.text(self.0.as_core_db()).as_str() {
                "State" => Some((ModuleId(0), DummyEff::STATE_ID)),
                "Reader" => Some((ModuleId(0), DummyEff::READER_ID)),
                _ => None,
            }
        }

        fn effect_member_sig(&self, _: ModuleId, _eff: EffectId, member: EffectOpId) -> TyScheme {
            use crate::TypeKind::*;
            match member {
                // get: forall 0 . {} -{0}-> Int
                DummyEff::GET_ID => TyScheme {
                    bound: vec![TyVarId(0)],
                    constrs: vec![],
                    eff: Row::Open(TyVarId(0)),
                    ty: self
                        .0
                        .mk_ty(FunTy(self.0.empty_row_ty(), self.0.mk_ty(IntTy))),
                },
                // put: forall 0 . Int -{0}-> {}
                DummyEff::PUT_ID => TyScheme {
                    bound: vec![TyVarId(0)],
                    constrs: vec![],
                    eff: Row::Open(TyVarId(0)),
                    ty: self
                        .0
                        .mk_ty(FunTy(self.0.mk_ty(IntTy), self.0.empty_row_ty())),
                },
                // ask: forall 0 1. {} -{0}-> 1
                DummyEff::ASK_ID => TyScheme {
                    bound: vec![TyVarId(0), TyVarId(1)],
                    constrs: vec![],
                    eff: Row::Open(TyVarId(0)),
                    ty: self.0.mk_ty(FunTy(
                        self.0.empty_row_ty(),
                        self.0.mk_ty(VarTy(TyVarId(1))),
                    )),
                },
                _ => unimplemented!(),
            }
        }

        fn effect_member_name(&self, _: ModuleId, _eff: EffectId, member: EffectOpId) -> Ident {
            match member {
                DummyEff::GET_ID => self.0.ident_str("get"),
                DummyEff::PUT_ID => self.0.ident_str("put"),
                DummyEff::ASK_ID => self.0.ident_str("ask"),
                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use aiahr_core::{
        ast::indexed::Term::*,
        ast::Direction,
        id::{EffectId, EffectOpId, ModuleId, TyVarId, VarId},
        ty::{
            row::{ClosedRow, Row},
            AccessTy, TypeKind,
            TypeKind::*,
        },
    };
    use aiahr_test::ast::{AstBuilder, MkTerm};
    use assert_matches::assert_matches;
    use bumpalo::Bump;

    use crate::type_check;
    use crate::{diagnostic::TypeCheckDiagnostic, test_utils::DummyEff, Evidence};

    macro_rules! assert_matches_unit_ty {
        ($db:expr, $term:expr) => {
            assert_matches!($db.kind($term), TypeKind::ProdTy(Row::Closed(ClosedRow { fields, values })) => {
                assert!(fields.fields($db).is_empty());
                assert!(values.values($db).is_empty());
            });
        }
    }

    macro_rules! assert_vec_matches {
        ($vec: expr, [$($elem:pat),*]) => {{
            let mut tmp = $vec;
            tmp.sort();
            assert_matches!(tmp.as_slice(), [$($elem),*]);
        }};
        ($vec: expr, [$($elem:pat),*] => $body:expr) => {{
            let mut tmp = $vec;
            tmp.sort();
            assert_matches!(tmp.as_slice(), [$($elem),*] => $body)
        }};
    }

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_analysis::Jar,
        aiahr_parser::Jar
    )]
    pub(crate) struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    const MOD: ModuleId = ModuleId(0);

    #[test]
    fn test_tc_unlabel() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(
                x,
                builder.mk_unlabel("start", builder.mk_label("start", Variable(x))),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        let db = &db;
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret) => {
                assert_matches!((db.kind(arg), db.kind(ret)), (VarTy(a), VarTy(b)) => {
                    assert_eq!(a, b);
                });
            }
        );
    }

    #[test]
    fn test_tc_unlabel_fails_on_wrong_label() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(
                x,
                builder.mk_unlabel("start", builder.mk_label("end", Variable(x))),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, _, errors) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        assert_matches!(
            errors[0],
            // TODO: Figure out how to check these errors
            TypeCheckDiagnostic {
                name: "Type Mismatch",
                principal: _
            }
        );
    }

    #[test]
    fn test_tc_label() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(x, builder.mk_label("start", Variable(x)))
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        let db = &db;
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret)
            => {
                assert_matches!((db.kind(arg), db.kind(ret)),
                    (VarTy(a), RowTy(ClosedRow { fields, values })) => {
                        assert_eq!(fields.fields(db).get(0).map(|start| start.text(db).as_str()), Some("start"));
                        assert_eq!(values.values(db).get(0).map(|val| db.kind(val)), Some(&VarTy(*a)));
                });
            }
        );
    }

    #[test]
    fn test_tc_abs() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let x = VarId(0);
        let y = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(x, builder.mk_abs(y, Variable(x)))
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (var_to_tys, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        let db = &db;
        assert_matches!(
            var_to_tys.get(&VarId(0)).map(|ty| db.kind(ty)),
            Some(&VarTy(TyVarId(0)))
        );
        assert_matches!(
            var_to_tys.get(&VarId(1)).map(|ty| db.kind(ty)),
            Some(&VarTy(TyVarId(1)))
        );
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret) => {
                assert_matches!((db.kind(arg), db.kind(ret)), (VarTy(a), FunTy(arg, ret)) => {
                    assert_matches!((db.kind(arg), db.kind(ret)), (VarTy(_), VarTy(b)) => {
                        assert_eq!(a, b);
                    })
                })
            }
        );
    }

    #[test]
    fn test_tc_sum_literal() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let t = VarId(0);
        let f = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_branch(
                builder.mk_abs(t, builder.mk_unlabel("true", Variable(t))),
                builder.mk_abs(f, builder.mk_unlabel("false", Variable(f))),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            let ty_var = assert_matches!(
                db.kind(arg),
                SumTy(Row::Closed(ClosedRow { fields, values })) => {
                    assert_matches!(fields.fields(db).as_slice(), [true_, false_] => {
                        assert_eq!(false_.text(db), "false");
                        assert_eq!(true_.text(db), "true");
                    });
                    assert_matches!(values.values(db).as_slice(), [a, b] => {
                        assert_eq!(a, b);
                        a
                    })
                }
            );
            assert_eq!(ret, ty_var);
        });
    }

    #[test]
    fn test_tc_product_literal() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(
                x,
                builder.mk_concat(
                    builder.mk_concat(
                        builder.mk_label("a", Variable(x)),
                        builder.mk_label("b", Variable(x)),
                    ),
                    builder.mk_concat(
                        builder.mk_label("c", Variable(x)),
                        builder.mk_label("d", Variable(x)),
                    ),
                ),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            assert_matches!(db.kind(ret), ProdTy(Row::Closed(ClosedRow { fields, values })) => {
                assert_matches!(fields.fields(db).as_slice(), [a, b, c, d] => {
                    assert_eq!(a.text(db), "a");
                    assert_eq!(b.text(db), "b");
                    assert_eq!(c.text(db), "c");
                    assert_eq!(d.text(db), "d");
                });
                assert_matches!(values.values(db).as_slice(), [a, b, c, d] => {
                    assert_eq!(a, arg);
                    assert_eq!(b, arg);
                    assert_eq!(c, arg);
                    assert_eq!(d, arg);
                })
            })
        });
    }

    #[test]
    fn test_tc_product_wand() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let m = VarId(0);
        let n = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abss(
                [m, n],
                builder.mk_unlabel(
                    "x",
                    builder.mk_project(
                        Direction::Right,
                        builder.mk_concat(Variable(m), Variable(n)),
                    ),
                ),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        let ty = assert_vec_matches!(
            scheme.constrs,
            [
                Evidence::Row {
                    left: Row::Open(_),
                    right: Row::Open(_),
                    goal: Row::Open(b)
                },
                Evidence::Row {
                    left: Row::Closed(ClosedRow { fields, values }),
                    right: Row::Open(_),
                    goal: Row::Open(a)
                }
            ] => {
                assert_eq!(a, b);
                assert_matches!(fields.fields(&db).as_slice(), [x] => {
                    assert_eq!(x.text(&db), "x");
                });
                assert_matches!(values.values(&db).as_slice(), [ty] => ty)
            }
        );
        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
            assert_matches!(db.kind(ret), FunTy(arg, ret) => {
                assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
                assert_eq!(ret, ty)
            })
        });
    }

    #[test]
    fn test_tc_applied_wand() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let m = VarId(0);
        let n = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_app(
                builder.mk_abss(
                    [m, n],
                    builder.mk_unlabel(
                        "x",
                        builder.mk_project(
                            Direction::Right,
                            builder.mk_concat(Variable(m), Variable(n)),
                        ),
                    ),
                ),
                builder.mk_label("x", Unit),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        assert_vec_matches!(
            scheme.constrs,
            [Evidence::Row {
                left: Row::Closed(ClosedRow { fields, values }),
                right: Row::Open(_),
                goal: Row::Open(_),
            }] => {
                assert_matches!(fields.fields(&db).as_slice(), [x] => {
                    assert_eq!(x.text(&db), "x");
                });
                assert_matches!(values.values(&db).as_slice(), [unit] => {
                    assert_matches_unit_ty!(&db, unit);
                });
            }
        );

        let db = &db;
        assert_matches!(db.kind(&scheme.ty), FunTy(arg, ret) => {
            assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
            assert_matches_unit_ty!(db, &ret);
        })
    }

    #[test]
    fn test_tc_eff_operation_infers_correct_effect() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_app(Operation((ModuleId(0), EffectId(0), EffectOpId(0))), Unit)
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);
        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        assert_matches!(scheme.eff, Row::Closed(ClosedRow { fields, values }) => {
            assert_matches!(fields.fields(&db).as_slice(), [state] => {
                assert_eq!(state.text(&db), "State");
            });
            assert_matches!(values.values(&db).as_slice(), [unit] => {
                assert_matches_unit_ty!(&db, unit);
            });
        });
        let db = &db;
        assert_matches!(db.kind(&scheme.ty), IntTy);
    }

    #[test]
    fn test_tc_eff_handler_removes_correct_effect() {
        let arena = Bump::new();
        let db = TestDatabase::default();

        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_handler(
                builder.mk_concat(
                    builder.mk_concat(
                        builder.mk_label(
                            "get",
                            builder.mk_abss(
                                [VarId(0), VarId(3)],
                                builder.mk_app(Variable(VarId(3)), Int(3)),
                            ),
                        ),
                        builder.mk_label(
                            "put",
                            builder.mk_abss(
                                [VarId(1), VarId(3)],
                                builder.mk_app(Variable(VarId(3)), Unit),
                            ),
                        ),
                    ),
                    builder.mk_label("return", builder.mk_abs(VarId(2), Variable(VarId(2)))),
                ),
                builder.mk_app(
                    Operation((ModuleId(0), DummyEff::STATE_ID, DummyEff::PUT_ID)),
                    builder.mk_app(
                        Operation((ModuleId(0), DummyEff::READER_ID, DummyEff::ASK_ID)),
                        Unit,
                    ),
                ),
            )
        });
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, scheme, errors) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        assert_eq!(errors, vec![]);
        assert_matches!(
            scheme.eff,
            Row::Closed(ClosedRow { fields, values }/*row!([reader], [ty!(ty_pat!({}))])*/) => {
                assert_matches!(fields.fields(&db).as_slice(), [reader] => {
                    assert_eq!(reader.text(&db), "Reader");
                });
                assert_matches!(values.values(&db).as_slice(), [unit] => {
                    assert_matches_unit_ty!(&db, unit);
                });
            }
        );
        let db = &db;
        assert_matches!(db.kind(&scheme.ty), RowTy(ClosedRow { fields, values }) => {
            assert!(fields.fields(db).is_empty());
            assert!(values.values(db).is_empty());
        });
    }

    #[test]
    fn test_tc_undefined_var_fails() {
        let arena = Bump::new();
        let db = TestDatabase::default();
        let untyped_ast = AstBuilder::with_builder(&db, |_| Variable(VarId(0)));
        let (untyped_ast, _) = untyped_ast.ref_alloc(&arena);

        let (_, _, _, errors) = type_check(&db, &DummyEff(&db), MOD, &untyped_ast);

        assert_matches!(
            &errors[0],
            TypeCheckDiagnostic {
                name: "Undefined Variable",
                principal: _,
            }
        );
    }
}
