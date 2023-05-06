use aiahr_ast::{self as ast, Ast, Term};
use aiahr_core::{
    diagnostic::{
        aiahr::{AiahrcError, AiahrcErrors},
        tc::TypeCheckDiagnostic,
    },
    file::FileId,
    id::{EffectName, EffectOpName, Id, TermName, TyVarId, VarId},
    ident::Ident,
    modules::Module,
};
use bumpalo::Bump;
use ena::unify::{InPlaceUnificationTable, UnifyKey};
use la_arena::Idx;
use rustc_hash::FxHashMap;

use aiahr_ty::{
    infer::{InArena, TcUnifierVar, TyCtx},
    *,
};

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
    fn effect_name(&self, effect: EffectName) -> Ident;
    /// Lookup effect members from it's ID
    fn effect_members(&self, effect: EffectName) -> &[EffectOpName];

    /// Look up an effect by the name of it's members, this may fail if an invalid list of member
    /// names is passed.
    fn lookup_effect_by_member_names(
        &self,
        module: Module,
        members: &[Ident],
    ) -> Option<EffectName>;
    fn lookup_effect_by_name(&self, module: Module, name: Ident) -> Option<EffectName>;
    /// Lookup the type signature of an effect's member
    fn effect_member_sig(&self, op_name: EffectOpName) -> TyScheme;
    /// Lookup the name of an effect's member
    fn effect_member_name(&self, op_name: EffectOpName) -> Ident;
}

#[salsa::jar(db = Db)]
pub struct Jar(TypedItem, type_scheme_of);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_desugar::Db {
    fn as_tc_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn type_scheme_of(&self, term_name: TermName) -> TypedItem {
        type_scheme_of(self.as_tc_db(), term_name)
    }

    fn type_check_errors(&self, file_id: FileId) -> Vec<AiahrcError> {
        let nameres_module = self.nameres_module_for_file_id(file_id);
        nameres_module
            .terms(self.as_nameres_db())
            .iter()
            .filter_map(|term| term.as_ref())
            .flat_map(|term| {
                type_scheme_of::accumulated::<AiahrcErrors>(
                    self.as_tc_db(),
                    term.name(self.as_nameres_db()),
                )
            })
            .collect()
    }
}
impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> + aiahr_desugar::Db {}

impl<DB> EffectInfo for DB
where
    DB: ?Sized + crate::Db,
{
    fn effect_name(&self, effect: EffectName) -> Ident {
        aiahr_nameres::effect_name(self.as_nameres_db(), effect)
    }

    fn effect_members(&self, effect: EffectName) -> &[EffectOpName] {
        aiahr_nameres::effect_members(self.as_nameres_db(), effect).as_slice()
    }

    fn lookup_effect_by_member_names(
        &self,
        module: Module,
        members: &[Ident],
    ) -> Option<EffectName> {
        aiahr_nameres::lookup_effect_by_member_names(
            self.as_nameres_db(),
            module,
            members.to_vec().into_boxed_slice(),
        )
    }

    fn lookup_effect_by_name(&self, module: Module, name: Ident) -> Option<EffectName> {
        aiahr_nameres::lookup_effect_by_name(self.as_nameres_db(), module, name)
    }

    fn effect_member_sig(&self, effect_op: EffectOpName) -> TyScheme {
        aiahr_desugar::effect_op_tyscheme_of(self.as_desugar_db(), effect_op)
    }

    fn effect_member_name(&self, effect_op: EffectOpName) -> Ident {
        aiahr_nameres::effect_member_name(self.as_nameres_db(), effect_op)
    }
}

#[salsa::tracked]
pub struct TypedItem {
    #[id]
    pub name: TermName,
    #[return_ref]
    pub var_to_tys: FxHashMap<VarId, Ty<InDb>>,
    #[return_ref]
    pub term_to_tys: FxHashMap<Idx<ast::Term<VarId>>, TyChkRes<InDb>>,
    pub ty_scheme: TyScheme,
}

#[salsa::tracked]
pub(crate) fn type_scheme_of(db: &dyn crate::Db, term_name: TermName) -> TypedItem {
    let module = term_name.module(db.as_core_db());
    let ast_db = db.as_ast_db();
    let ast_module = db.desugar_module_of(module);
    let term = ast_module
        .terms(ast_db)
        .iter()
        .find(|term| term.name(db.as_ast_db()) == term_name)
        .unwrap_or_else(|| {
            panic!(
                "ICE: Constructed TermName {:?} without associated term",
                term_name.name(db.as_core_db()).text(db.as_core_db())
            )
        });

    let (var_to_tys, terms_to_tys, ty_scheme, diags) =
        type_check(db, db, module, term.data(db.as_ast_db()));

    for diag in diags {
        AiahrcErrors::push(db, diag.into())
    }
    TypedItem::new(db, term_name, var_to_tys, terms_to_tys, ty_scheme)
}

type TypeCheckOutput = (
    FxHashMap<VarId, Ty<InDb>>,
    FxHashMap<Idx<Term<VarId>>, TyChkRes<InDb>>,
    TyScheme,
    Vec<TypeCheckDiagnostic>,
);

pub fn type_check<E>(
    db: &dyn crate::Db,
    eff_info: &E,
    module: Module,
    ast: &Ast<VarId>,
) -> TypeCheckOutput
where
    E: ?Sized + EffectInfo,
{
    let arena = Bump::new();
    let infer_ctx = TyCtx::new(db.as_ty_db(), &arena);
    tc_term(db, &infer_ctx, eff_info, module, ast)
}

fn tc_term<'ty, 'infer, 's, 'eff, II, E>(
    db: &dyn crate::Db,
    infer_ctx: &II,
    eff_info: &E,
    module: Module,
    ast: &Ast<VarId>,
) -> TypeCheckOutput
where
    II: MkTy<InArena<'infer>> + AccessTy<'infer, InArena<'infer>>,
    E: ?Sized + EffectInfo,
{
    let term = ast.root();
    let infer = InferCtx::new(db, infer_ctx, module, ast);

    // Infer types for all our variables and the root term.
    let (infer, gen_storage, result) = infer.infer(eff_info, term);

    // Solve constraints into the unifiers mapping.
    let (mut ty_unifiers, mut data_row_unifiers, mut eff_row_unifiers, unsolved_eqs, errors) =
        infer.solve(eff_info);

    //print_root_unifiers(&mut unifiers);
    // Zonk the variable -> type mapping and the root term type.
    let mut zonker = Zonker {
        ctx: db,
        free_vars: vec![],
        free_data_rows: vec![],
        free_eff_rows: vec![],
        ty_unifiers: &mut ty_unifiers,
        datarow_unifiers: &mut data_row_unifiers,
        effrow_unifiers: &mut eff_row_unifiers,
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
        .data_eqns
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
    use aiahr_core::id::{EffectName, EffectOpName};
    use aiahr_core::ident::Ident;
    use aiahr_core::modules::Module;

    use crate::{EffectInfo, TyScheme};

    pub(crate) struct DummyEff<'a>(pub &'a dyn crate::Db);
    impl EffectInfo for DummyEff<'_> {
        fn effect_name(&self, eff: EffectName) -> Ident {
            self.0.effect_name(eff)
        }

        fn effect_members(&self, eff: EffectName) -> &[EffectOpName] {
            self.0.effect_members(eff)
        }

        fn lookup_effect_by_member_names<'a>(
            &self,
            module: Module,
            members: &[Ident],
        ) -> Option<EffectName> {
            members
                .get(0)
                .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                    "ask" => Some(EffectName::new(
                        self.0.as_core_db(),
                        self.0.ident_str("Reader"),
                        module,
                    )),
                    "get" => {
                        members
                            .get(1)
                            .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                                "put" => Some(EffectName::new(
                                    self.0.as_core_db(),
                                    self.0.ident_str("State"),
                                    module,
                                )),
                                _ => None,
                            })
                    }
                    "put" => {
                        members
                            .get(1)
                            .and_then(|id| match id.text(self.0.as_core_db()).as_str() {
                                "get" => Some(EffectName::new(
                                    self.0.as_core_db(),
                                    self.0.ident_str("State"),
                                    module,
                                )),
                                _ => None,
                            })
                    }
                    _ => None,
                })
        }

        fn lookup_effect_by_name(&self, module: Module, name: Ident) -> Option<EffectName> {
            match name.text(self.0.as_core_db()).as_str() {
                "State" | "Reader" => Some(EffectName::new(self.0.as_core_db(), name, module)),
                _ => None,
            }
        }

        fn effect_member_sig(&self, eff_op: EffectOpName) -> TyScheme {
            self.0.effect_member_sig(eff_op)
        }

        fn effect_member_name(&self, eff_op: EffectOpName) -> Ident {
            eff_op.name(self.0.as_core_db())
        }
    }
}

#[cfg(test)]
mod tests {

    use std::path::PathBuf;

    use aiahr_ast::{Direction, Term::*};
    use aiahr_core::{
        diagnostic::tc::TypeCheckDiagnostic,
        file::{FileId, SourceFile, SourceFileSet},
        id::{TermName, TyVarId, VarId},
        modules::Module,
        Db,
    };
    use aiahr_parser::Db as ParserDb;
    use aiahr_test::ast::{AstBuilder, MkTerm};
    use aiahr_ty::{
        row::{Row, RowOps},
        AccessTy, TypeKind,
        TypeKind::*,
    };
    use assert_matches::assert_matches;
    use salsa::AsId;

    use crate::{test_utils::DummyEff, Evidence};
    use crate::{type_check, type_scheme_of};

    macro_rules! assert_matches_unit_ty {
        ($db:expr, $term:expr) => {
            assert_matches!($db.kind($term), TypeKind::ProdTy(Row::Closed(closed)) => {
                assert!(closed.is_empty(&$db))
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
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_nameres::Jar,
        aiahr_parser::Jar,
        aiahr_ty::Jar
    )]
    pub(crate) struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn dummy_mod() -> Module {
        Module::from_id(salsa::Id::from_u32(0))
    }

    #[test]
    fn test_tc_unlabel() {
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(
                x,
                builder.mk_unlabel("start", builder.mk_label("start", Variable(x))),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

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
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(
                x,
                builder.mk_unlabel("start", builder.mk_label("end", Variable(x))),
            )
        });

        let (_, _, _, errors) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

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
        let db = TestDatabase::default();
        let x = VarId(0);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(x, builder.mk_label("start", Variable(x)))
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

        let db = &db;
        assert_matches!(
            db.kind(&scheme.ty),
            FunTy(arg, ret)
            => {
                assert_matches!((db.kind(arg), db.kind(ret)),
                    (VarTy(a), RowTy(closed)) => {
                        assert_eq!(closed.fields(&db).get(0).map(|start| start.text(db).as_str()), Some("start"));
                        assert_eq!(closed.values(&db).get(0).map(|val| db.kind(val)), Some(&VarTy(*a)));
                });
            }
        );
    }

    #[test]
    fn test_tc_abs() {
        let db = TestDatabase::default();
        let x = VarId(0);
        let y = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_abs(x, builder.mk_abs(y, Variable(x)))
        });

        let (var_to_tys, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

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
        let db = TestDatabase::default();
        let t = VarId(0);
        let f = VarId(1);
        let untyped_ast = AstBuilder::with_builder(&db, |builder| {
            builder.mk_branch(
                builder.mk_abs(t, builder.mk_unlabel("true", Variable(t))),
                builder.mk_abs(f, builder.mk_unlabel("false", Variable(f))),
            )
        });

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            let ty_var = assert_matches!(
                db.kind(arg),
                SumTy(Row::Closed(closed)) => {
                    assert_matches!(closed.fields(&db), [true_, false_] => {
                        assert_eq!(false_.text(db), "false");
                        assert_eq!(true_.text(db), "true");
                    });
                    assert_matches!(closed.values(&db), [a, b] => {
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

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

        let db = &db;
        assert_matches!(
        db.kind(&scheme.ty),
        FunTy(arg, ret) => {
            assert_matches!(db.kind(ret), ProdTy(Row::Closed(closed)) => {
                assert_matches!(closed.fields(&db), [a, b, c, d] => {
                    assert_eq!(a.text(db), "a");
                    assert_eq!(b.text(db), "b");
                    assert_eq!(c.text(db), "c");
                    assert_eq!(d.text(db), "d");
                });
                assert_matches!(closed.values(&db), [a, b, c, d] => {
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

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

        let ty = assert_vec_matches!(
            scheme.constrs,
            [
                Evidence::Row {
                    left: Row::Open(_),
                    right: Row::Open(_),
                    goal: Row::Open(b)
                },
                Evidence::Row {
                    left: Row::Closed(closed),
                    right: Row::Open(_),
                    goal: Row::Open(a)
                }
            ] => {
                assert_eq!(a, b);
                assert_matches!(closed.fields(&&db), [x] => {
                    assert_eq!(x.text(&db), "x");
                });
                assert_matches!(closed.values(&&db), [ty] => ty)
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

        let (_, _, scheme, _) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

        assert_vec_matches!(
            scheme.constrs,
            [Evidence::Row {
                left: Row::Closed(closed),
                right: Row::Open(_),
                goal: Row::Open(_),
            }] => {
                assert_matches!(closed.fields(&&db), [x] => {
                    assert_eq!(x.text(&db), "x");
                });
                assert_matches!(closed.values(&&db), [unit] => {
                    assert_matches_unit_ty!(&db, unit);
                });
            }
        );

        let db = &db;
        assert_matches!(db.kind(&scheme.ty), FunTy(arg, ret) => {
            assert_matches!(db.kind(arg), ProdTy(Row::Open(_)));
            assert_matches_unit_ty!(db, ret);
        })
    }

    //#[test]
    fn test_tc_eff_operation_infers_correct_effect() {
        let db = TestDatabase::default();
        let content = r#"
effect State {
    put : {} -> {},
    get : {} -> {}
}

effect Reader {
    ask : {} -> {}
}

f = State.get({}) 
"#;

        let file_id = FileId::new(&db, PathBuf::from("test"));
        let source_file = SourceFile::new(&db, file_id, content.to_string());
        let _ = SourceFileSet::new(&db, vec![source_file]);

        let module = db.root_module_for_file(source_file);
        let term_name = TermName::new(&db, db.ident_str("f"), module);
        let typed_item = type_scheme_of(&db, term_name);

        let scheme = typed_item.ty_scheme(&db);

        let db = &db;
        assert_matches!(scheme.eff, Row::Closed(closed) => {
            assert_matches!(closed.fields(&db), [state] => {
                assert_eq!(state.text(db), "State");
            });
            assert_matches!(closed.values(&db), [unit] => {
                assert_matches_unit_ty!(db, unit);
            });
        });
        assert_matches!(db.kind(&scheme.ty), ProdTy(Row::Closed(closed)) => {
            assert!(closed.is_empty(&db));
            assert!(closed.is_empty(&db));
        });
    }

    //#[test]
    fn test_tc_eff_handler_removes_correct_effect() {
        let db = TestDatabase::default();
        let content = r#"
effect State {
    put : {} -> {},
    get : {} -> {}
}

effect Reader {
    ask : {} -> {}
}

f = with {
    put = |x| |k| {},
    get = |x| |k| {},
    return = |x| x
} do State.put(Reader.ask({}))
"#;

        let file_id = FileId::new(&db, PathBuf::from("test"));
        let source_file = SourceFile::new(&db, file_id, content.to_string());
        let _ = SourceFileSet::new(&db, vec![source_file]);

        let module = db.root_module_for_file(source_file);
        let term_name = TermName::new(&db, db.ident_str("f"), module);
        let typed_item = type_scheme_of(&db, term_name);

        let scheme = typed_item.ty_scheme(&db);
        let db = &db;
        assert_matches_unit_ty!(db, &scheme.ty);
        assert_matches!(
            scheme.eff,
            Row::Closed(closed) => {
                assert_matches!(closed.fields(&db), [reader] => {
                    assert_eq!(reader.text(db), "Reader");
                });
                assert_matches!(closed.values(&db), [unit] => {
                    assert_matches_unit_ty!(db, unit);
                });
            }
        );
    }

    #[test]
    fn test_tc_undefined_var_fails() {
        let db = TestDatabase::default();
        let untyped_ast = AstBuilder::with_builder(&db, |_| Variable(VarId(0)));

        let (_, _, _, errors) = type_check(&db, &DummyEff(&db), dummy_mod(), &untyped_ast);

        assert_matches!(
            &errors[0],
            TypeCheckDiagnostic {
                name: "Undefined Variable",
                principal: _,
            }
        );
    }
}
