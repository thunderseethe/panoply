use aiahr_lower_reducir::ReducIrItem;
use aiahr_reducir::ty::{ReducIrTy, ReducIrTyApp};
use rustc_hash::FxHashMap;
use std::convert::Infallible;

use aiahr_core::id::{ReducIrVarId, TermName};
use aiahr_reducir::{ReducIr, ReducIrFold, ReducIrKind, ReducIrVar, P};

#[salsa::jar(db = Db)]
pub struct Jar(ReducIrOptimizedItem, simple_reducir_item);

pub trait Db: salsa::DbWithJar<Jar> + aiahr_lower_reducir::Db {
    fn as_opt_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn simple_reducir_item(&self, item: ReducIrItem) -> ReducIrOptimizedItem {
        simple_reducir_item(self.as_opt_reducir_db(), item)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_lower_reducir::Db {}

#[salsa::tracked]
pub struct ReducIrOptimizedItem {
    #[id]
    name: TermName,
    #[return_ref]
    item: ReducIr<Infallible>,
}

#[salsa::tracked]
fn simple_reducir_item(db: &dyn crate::Db, item: ReducIrItem) -> ReducIrOptimizedItem {
    let ir = simplify(db, item);
    ReducIrOptimizedItem::new(db, item.name(db.as_lower_reducir_db()), ir)
}

struct Subst {
    env: FxHashMap<ReducIrVarId, ReducIr<Infallible>>,
}
impl ReducIrFold for Subst {
    type InExt = Infallible;

    type OutExt = Infallible;

    fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
    }

    fn fold_ir(&mut self, ir: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        match ir {
            ReducIrKind::Var(v) => match self.env.get(&v.var) {
                Some(val) => val.clone(),
                None => ReducIr::new(ir),
            },
            _ => ReducIr::new(ir),
        }
    }
}

struct SubstTy<'a> {
    db: &'a dyn crate::Db,
    needle: ReducIrTyApp,
}

impl SubstTy<'_> {
    fn subst(&self, ty: ReducIrTy) -> ReducIrTy {
        self.needle.clone().subst_into(self.db.as_ir_db(), ty)
    }
}

impl ReducIrFold for SubstTy<'_> {
    type InExt = Infallible;

    type OutExt = Infallible;

    fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
    }

    fn fold_ir(&mut self, kind: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        match kind {
            ReducIrKind::TyAbs(kind, body) => ReducIr::new(ReducIrKind::TyAbs(
                kind,
                P::new(body.fold(&mut SubstTy {
                    db: self.db,
                    needle: self.needle.clone().shift(self.db.as_ir_db(), 1),
                })),
            )),
            ReducIrKind::Var(var) => ReducIr::var(ReducIrVar {
                var: var.var,
                ty: self.subst(var.ty),
            }),
            ReducIrKind::Item(item, item_ty) => {
                ReducIr::new(ReducIrKind::Item(item, self.subst(item_ty)))
            }
            ReducIrKind::Abs(vars, body) => ReducIr::new(ReducIrKind::Abs(
                vars.iter()
                    .map(|v| ReducIrVar {
                        var: v.var,
                        ty: self.subst(v.ty),
                    })
                    .collect(),
                body,
            )),
            ReducIrKind::Tag(ty, tag, value) => {
                ReducIr::new(ReducIrKind::Tag(self.subst(ty), tag, value))
            }
            ReducIrKind::Case(ty, discr, branches) => {
                ReducIr::new(ReducIrKind::Case(self.subst(ty), discr, branches))
            }

            kind => ReducIr::new(kind),
        }
    }
}

struct Simplify<'a> {
    db: &'a dyn crate::Db,
    row_evs: FxHashMap<TermName, &'a ReducIr<Infallible>>,
}
impl ReducIrFold for Simplify<'_> {
    type InExt = Infallible;

    type OutExt = Infallible;

    fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
    }

    fn fold_ir(&mut self, ir: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        use aiahr_reducir::ReducIrKind::*;
        match ir {
            TyApp(body, ty_app) => match body.kind() {
                TyAbs(_, body) => self.traverse_ir(&body.fold(&mut SubstTy {
                    db: self.db,
                    needle: ty_app,
                })),
                _ => ReducIr::new(TyApp(body, ty_app)),
            },
            FieldProj(indx, ref base) => match base.kind() {
                Struct(elems) => elems[indx].clone(),
                _ => ReducIr::new(ir),
            },
            App(head, spine) => match head.kind() {
                Abs(ref args, body) => {
                    let mut remaining_args = vec![];
                    let mut remaining_apps = vec![];

                    let mut args_iter = args.iter().copied();
                    let mut spine_iter = spine.into_iter();
                    let mut env = FxHashMap::default();

                    loop {
                        match (args_iter.next(), spine_iter.next()) {
                            (Some(var), Some(arg)) => {
                                if is_value(&arg) {
                                    env.insert(var.var, arg);
                                } else {
                                    remaining_args.push(var);
                                    remaining_apps.push(arg);
                                }
                            }
                            (None, Some(arg)) => {
                                remaining_apps.push(arg);
                                break;
                            }
                            (Some(var), None) => {
                                remaining_args.push(var);
                                break;
                            }
                            (None, None) => break,
                        }
                    }
                    remaining_args.extend(args_iter);
                    remaining_apps.extend(spine_iter);
                    ReducIr::app(
                        ReducIr::abss(
                            remaining_args,
                            self.traverse_ir(&body.fold(&mut Subst { env })),
                        ),
                        remaining_apps,
                    )
                }
                _ => ReducIr::new(App(head, spine)),
            },
            Case(ty, discr, branches) => match discr.kind() {
                Tag(_, tag, val) => self.traverse_ir(&ReducIr::app(
                    branches[*tag].clone(),
                    [val.clone().into_inner()],
                )),
                _ => ReducIr::new(Case(ty, discr, branches)),
            },
            // Always inline row evidence
            Item(name, _) => match self.row_evs.get(&name) {
                Some(ir) => self.traverse_ir(*ir),
                None => ReducIr::new(ir),
            },
            ir => ReducIr::new(ir),
        }
    }
}

/// True if an ir term is a value (contains no computations), false if the term does require
/// computation that can be done
fn is_value(ir: &ReducIr<Infallible>) -> bool {
    match ir.kind() {
        ReducIrKind::Int(_)
        | ReducIrKind::Var(_)
        | ReducIrKind::Item(_, _)
        | ReducIrKind::Abs(_, _)
        | ReducIrKind::TyAbs(_, _)
        | ReducIrKind::X(_) => true,
        ReducIrKind::App(_, _) | ReducIrKind::Case(_, _, _) | ReducIrKind::TyApp(_, _) => false,
        ReducIrKind::Struct(elems) => elems.iter().all(is_value),
        ReducIrKind::FieldProj(_, base) => is_value(base),
        ReducIrKind::Tag(_, _, base) => is_value(base),
    }
}

#[allow(dead_code)]
fn simplify(db: &dyn crate::Db, item: ReducIrItem) -> ReducIr<Infallible> {
    let lower_db = db.as_lower_reducir_db();
    let row_evs = item.row_evs(lower_db);
    let ir = item.mon_item(lower_db);
    let inline_row_ev = row_evs
        .iter()
        .flat_map(|row_ev| {
            let simple_item = row_ev.simple(db.as_lower_reducir_db());
            let scoped_item = row_ev.scoped(db.as_lower_reducir_db());

            let simple = (simple_item.name(lower_db), simple_item.mon_item(lower_db));
            let scoped = (scoped_item.name(lower_db), scoped_item.mon_item(lower_db));
            [simple, scoped]
        })
        .collect::<FxHashMap<_, _>>();

    ir.fold(&mut Simplify {
        db,
        row_evs: inline_row_ev,
    })
}

#[cfg(test)]
mod tests {

    use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
    use aiahr_core::pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx};
    use aiahr_core::Db as CoreDb;
    use aiahr_lower_reducir::{Db as LowerDb, ReducIrItem};
    use aiahr_parser::Db as ParseDb;
    use aiahr_reducir::TypeCheck;
    use expect_test::expect;

    use crate::simplify;

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_lower_reducir::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_nameres::Jar,
        aiahr_parser::Jar,
        aiahr_reducir::Jar,
        aiahr_tc::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn lower_function(db: &TestDatabase, input: &str, fn_name: &str) -> ReducIrItem {
        let path = std::path::PathBuf::from("test.aiahr");
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

        match db.lower_item_for_file_name(path, db.ident_str(fn_name)) {
            Some(term) => term,
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }

    fn lower_mon_snippet(db: &TestDatabase, input: &str) -> ReducIrItem {
        let main = format!("f = {}", input);
        lower_function(db, &main, "f")
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
        let simple_ir = simplify(&db, ir);

        let pretty_ir = simple_ir.pretty_with(&db).pprint().pretty(80).to_string();

        let expect = expect![[r#"
            (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                ((((__mon_bind @ {1}) @ {} -> {{}, {}}) @ {{}, {}})
                  (let
                    (V6 (V4[0]
                      (V5[0]
                        (fun [V7, V8, V9] (V8 V9 V9))
                        (fun [V10, V11, V12] (V11 {} V10)))
                      (fun [V13, V14] (V2[0] V14 V13))))
                    (((_mon_freshm @ (Marker {} -> {{}, {}})) @ {1} -> (Control {1} {} ->
                    {{}, {}}))
                      (fun [V18]
                        ((((__mon_prompt @ {1}) @ {0}) @ {} -> {{}, {}})
                          V18
                          (fun [V0] (V1[0] V0 {V18, {V6[0], V6[1]}}))
                          ((((__mon_bind @ {0}) @ {}) @ {} -> {{}, {}})
                            (let (V16 (V1[3][0] V0))
                              (fun [V0]
                                <1: {V16[0], (fun [V17] (V16[1][1] {} V17)), (fun [V0] V0)
                                  }>))
                            (fun [V21] (let (V22 (V6[2] V21)) (fun [V0] <0: V22>))))))))
                  (fun [V23] (let (V24 (V23 {})) (fun [V0] <0: V24>))))))"#]];
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
                } -> {1} -> {1} -> (Control {1} {{}, {}})"#]];
        let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
        expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
    }
}
