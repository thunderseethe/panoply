use std::any::type_name;

use aiahr_ast::{self as ast, Ast, Term};
use aiahr_core::{
    diagnostic::{
        aiahr::{AiahrcError, AiahrcErrors},
        tc::TypeCheckDiagnostic,
    },
    file::FileId,
    id::{EffectName, EffectOpName, TermName, VarId},
    ident::Ident,
    modules::Module,
    pretty::{PrettyPrint, PrettyWithCtx},
};
use bumpalo::Bump;
use ena::unify::{InPlaceUnificationTable, UnifyKey};
use la_arena::Idx;
use pretty::RcAllocator;
use rustc_hash::{FxHashMap, FxHashSet};

use aiahr_ty::{
    infer::{TcUnifierVar, TyCtx, UnifierKind},
    *,
};

mod unsolved_row;

use crate::{folds::zonker::FreeTyVars, infer::InferCtx};

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
    #[return_ref]
    pub required_evidence: FxHashSet<Evidence<InDb>>,
    #[return_ref]
    pub item_to_wrappers: FxHashMap<Idx<ast::Term<VarId>>, Wrapper>,
    pub ty_scheme: TyScheme,
}

#[salsa::tracked]
pub(crate) fn type_scheme_of(db: &dyn crate::Db, term_name: TermName) -> TypedItem {
    let name = term_name.name(db.as_core_db());
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

    // TODO: This should be more configurable  than just a hardcoded check for main
    let is_entry_point = name == db.ident_str("main");

    let ty_chk_out = type_check(db, module, term.data(db.as_ast_db()), is_entry_point);

    for diag in ty_chk_out.diags {
        AiahrcErrors::push(db, diag.into())
    }
    TypedItem::new(
        db,
        term_name,
        ty_chk_out.var_tys,
        ty_chk_out.term_tys,
        ty_chk_out.required_ev,
        ty_chk_out.item_wrappers,
        ty_chk_out.scheme,
    )
}

pub(crate) struct TypeCheckOutput {
    var_tys: FxHashMap<VarId, Ty<InDb>>,
    term_tys: FxHashMap<Idx<Term<VarId>>, TyChkRes<InDb>>,
    required_ev: FxHashSet<Evidence<InDb>>,
    item_wrappers: FxHashMap<Idx<Term<VarId>>, Wrapper<InDb>>,
    scheme: TyScheme,
    diags: Vec<TypeCheckDiagnostic>,
}

pub(crate) fn type_check(
    db: &dyn crate::Db,
    module: Module,
    ast: &Ast<VarId>,
    is_entry_point: bool,
) -> TypeCheckOutput {
    let arena = Bump::new();
    let infer_ctx = TyCtx::new(db.as_ty_db(), &arena);

    let infer_ctx = &infer_ctx;
    let term = ast.root();
    let infer = InferCtx::new(db, infer_ctx, module, ast);

    let (infer, gen_storage, result) = if is_entry_point {
        let expected = infer.entry_point_expected();
        let (infer, gen_storage) = infer.check(term, expected);
        (infer, gen_storage, expected)
    } else {
        // Infer types for all our variables and the root term.
        infer.infer(term)
    };

    // Solve constraints into the unifiers mapping.
    let (mut ty_unifiers, mut data_row_unifiers, mut eff_row_unifiers, unsolved_eqs, errors) =
        infer.solve();

    // Zonk the variable -> type mapping and the root term type.
    let mut zonker = Zonker::new(
        db,
        &mut ty_unifiers,
        &mut data_row_unifiers,
        &mut eff_row_unifiers,
    );
    let zonked_infer = result.try_fold_with(&mut zonker).unwrap();

    let zonked_var_tys = gen_storage.var_tys.try_fold_with(&mut zonker).unwrap();

    let zonked_term_tys = gen_storage.term_tys.try_fold_with(&mut zonker).unwrap();

    let zonked_item_wrappers = gen_storage
        .item_wrappers
        .try_fold_with(&mut zonker)
        .unwrap();

    let zonked_required_ev = gen_storage
        .required_ev
        .into_iter()
        .map(|ev| ev.try_fold_with(&mut zonker).unwrap())
        .collect::<FxHashSet<_>>();

    let mut constrs = unsolved_eqs
        .data_eqns
        .into_iter()
        .map(Evidence::from)
        .chain(unsolved_eqs.eff_eqns.into_iter().map(Evidence::from))
        .map(|ev| ev.try_fold_with(&mut zonker).unwrap())
        .collect::<Vec<_>>();

    // We want constraints to be in a consistent order
    constrs.sort();
    // We also to remove any duplicates introduced during zonking
    constrs.dedup();

    // Any unifier variables that weren't solved are free and should be generalized in the type
    // scheme
    let free_ty_vars: FreeTyVars = zonker.into();

    let scheme = TyScheme {
        bound_ty: free_ty_vars.ty,
        bound_data_row: free_ty_vars.data,
        bound_eff_row: free_ty_vars.eff,
        constrs,
        eff: zonked_infer.eff,
        ty: zonked_infer.ty,
    };
    TypeCheckOutput {
        var_tys: zonked_var_tys,
        term_tys: zonked_term_tys,
        required_ev: zonked_required_ev,
        item_wrappers: zonked_item_wrappers,
        scheme,
        diags: errors,
    }
}

#[allow(dead_code)]
fn print_root_unifiers<'ctx, K: UnifierKind>(
    db: &dyn crate::Db,
    uni: &mut InPlaceUnificationTable<TcUnifierVar<'ctx, K>>,
) where
    K::UnifyValue<'ctx>: for<'db> PrettyWithCtx<(&'db (dyn crate::Db + 'db), ())>,
{
    println!("UnificationTable<{}> [", type_name::<K>());
    for uv in (0..uni.len()).map(|i| TcUnifierVar::from_index(i as u32)) {
        match uni.probe_value(uv) {
            Some(candidate) => {
                println!(
                    "\t{} => {}",
                    uv.index(),
                    candidate.pretty_with(&(db, ())).pprint().pretty(80)
                );
            }
            None => {
                let doc: pretty::RcDoc =
                    pretty::Pretty::pretty(uni.find(uv), &RcAllocator).into_doc();
                println!("\t{} => {}", uv.index(), doc.pretty(80));
            }
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
    use aiahr_core::{
        diagnostic::tc::TypeCheckDiagnostic,
        file::{FileId, SourceFile, SourceFileSet},
        id::{TermName, TyVarId},
        Db,
    };
    use aiahr_parser::Db as ParserDb;
    use aiahr_ty::{
        row::{Row, RowOps},
        AccessTy, TyScheme, TypeKind,
        TypeKind::*,
    };
    use assert_matches::assert_matches;

    use crate::Db as TcDb;
    use crate::Evidence;

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

    fn type_errors(db: &TestDatabase) -> Vec<TypeCheckDiagnostic> {
        let path = std::path::PathBuf::from("test.aiahr");
        let file_id = FileId::new(db, path.clone());
        db.type_check_errors(file_id)
            .into_iter()
            .map(|err| match err {
                aiahr_core::diagnostic::aiahr::AiahrcError::TypeCheckError(err) => err,
                _ => unreachable!(),
            })
            .collect()
    }

    fn type_check_file(db: &TestDatabase, name: &str, contents: impl ToString) -> TyScheme {
        let path = std::path::PathBuf::from("test.aiahr");
        let file_id = FileId::new(db, path.clone());
        let file = SourceFile::new(db, file_id, contents.to_string());
        SourceFileSet::new(db, vec![file]);

        let module = db.root_module_for_path(path);
        db.type_scheme_of(TermName::new(db, db.ident_str(name), module))
            .ty_scheme(db)
    }

    fn type_check_snippet(db: &TestDatabase, snippet: &str) -> TyScheme {
        let mut contents = "f = ".to_string();
        contents.push_str(snippet);
        type_check_file(db, "f", contents)
    }

    #[test]
    fn test_tc_unlabel() {
        let db = TestDatabase::default();
        let scheme = type_check_snippet(&db, "|x| { start = x }.start");

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

        let _ = type_check_snippet(&db, "|x| { end = x }.start");
        let diags = type_errors(&db);

        assert_matches!(
            diags[0],
            // TODO: Figure out how to check these errors
            TypeCheckDiagnostic {
                name: "Data Rows Mismatch",
                principal: _
            }
        );
    }

    #[test]
    fn test_tc_label() {
        let db = TestDatabase::default();
        let scheme = type_check_snippet(&db, "|x| { start = x }");

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

        let scheme = type_check_snippet(&db, "|x||y| x");

        let db = &db;
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

        let scheme = type_check_snippet(
            &db,
            r#"
match <
 <true = t> => t,
 <false = f> => f
>"#,
        );

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

        let scheme = type_check_snippet(&db, "|x| { a = x, b = x } ,, { c = x, d = x }");

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

        let scheme = type_check_snippet(&db, "|m||n| (m ,, n).x");

        let ty = assert_vec_matches!(
            scheme.constrs,
            [
                Evidence::DataRow {
                    left: Row::Open(_),
                    right: Row::Open(_),
                    goal: Row::Open(b)
                },
                Evidence::DataRow {
                    left: Row::Open(_),
                    right: Row::Closed(closed),
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

        let scheme = type_check_snippet(&db, "(|m||n| (m ,, n).x)({ x = {} })");

        assert_vec_matches!(
            scheme.constrs,
            [Evidence::DataRow {
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

    #[test]
    fn test_tc_eff_operation_infers_correct_effect() {
        let db = TestDatabase::default();
        let content = r#"
effect State {
    put : {} -> {},
    get : {} -> {}
}

f = State.get({})
"#;

        let db = &db;
        let scheme = type_check_file(db, "f", content);

        assert_matches!(scheme.constrs.as_slice(), [
            Evidence::EffRow {
                left: Row::Open(_),
                right: Row::Closed(state),
                goal: Row::Open(TyVarId(0))
            }
        ] => {
            assert_eq!(state.fields(&db), &[db.ident_str("State")]);
        });
        assert_eq!(scheme.eff, Row::Open(TyVarId(0)));
        assert_matches_unit_ty!(db, &scheme.ty);
    }

    #[test]
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

        let db = &db;
        let scheme = type_check_file(db, "f", content);
        assert_matches_unit_ty!(db, &scheme.ty);
        assert_matches!(scheme.constrs.as_slice(), [
            Evidence::EffRow {
                left: Row::Open(TyVarId(0)),
                right: Row::Closed(state),
                goal: Row::Open(a)
            },
            Evidence::EffRow {
                left: Row::Open(_),
                right: Row::Closed(reader),
                goal: Row::Open(b)
            }
        ] => {
            assert_eq!(a, b);
            assert_eq!(state.fields(&db), &[db.ident_str("State")]);
            assert_eq!(reader.fields(&db), &[db.ident_str("Reader")]);
        });
        assert_matches!(scheme.eff, Row::Open(TyVarId(0)));
    }

    #[test]
    fn test_entry_point_has_expected_type() {
        let db = TestDatabase::default();

        let scheme = type_check_file(
            &db,
            "main",
            r#"
main = (|x| x)({})
"#,
        );

        assert_matches!((&db).kind(&scheme.ty), TypeKind::IntTy);
        //assert_matches_unit_ty!(&db, &scheme.ty);
        assert_matches!(scheme.eff, Row::Closed(closed) => {
            assert!(closed.fields(&&db).is_empty());
            assert!(closed.values(&&db).is_empty());
        })
    }
}
