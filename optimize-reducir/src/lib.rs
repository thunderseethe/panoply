use aiahr_lower_reducir::ReducIrItem;
use rustc_hash::FxHashMap;
use std::convert::Infallible;

use aiahr_reducir::{ReducIr, ReducIrFold, ReducIrKind, ReducIrTermName};

use crate::subst::{Subst, SubstTy};

#[salsa::jar(db = Db)]
pub struct Jar(ReducIrOptimizedItem, simple_reducir_item);

pub trait Db: salsa::DbWithJar<Jar> + aiahr_lower_reducir::Db {
    fn as_opt_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn simple_reducir_item(&self, item: ReducIrItem) -> Vec<ReducIrOptimizedItem> {
        simple_reducir_item(self.as_opt_reducir_db(), item)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_lower_reducir::Db {}

#[salsa::tracked]
pub struct ReducIrOptimizedItem {
    #[id]
    name: ReducIrTermName,
    #[return_ref]
    item: ReducIr,
}

#[salsa::tracked]
fn simple_reducir_item(db: &dyn crate::Db, item: ReducIrItem) -> Vec<ReducIrOptimizedItem> {
    let ir = simplify(db, item);

    let core_db = db.as_core_db();
    let term_name = item.name(db.as_lower_reducir_db());
    let (lam_ir, lifts) = lift::lambda_lift(db, term_name.module(core_db), term_name, ir);
    std::iter::once(ReducIrOptimizedItem::new(
        db,
        ReducIrTermName::Term(item.name(db.as_lower_reducir_db())),
        lam_ir,
    ))
    .chain(
        lifts
            .into_iter()
            .map(|(name, ir)| ReducIrOptimizedItem::new(db, name, ir)),
    )
    .collect()
}

mod subst {
    use std::convert::Infallible;

    use aiahr_reducir::ty::{ReducIrTy, ReducIrTyApp};
    use aiahr_reducir::{ReducIr, ReducIrEndoFold, ReducIrKind, ReducIrLocal, ReducIrVar, P};
    use rustc_hash::FxHashMap;

    pub(crate) struct Subst<'a> {
        pub(crate) env: &'a FxHashMap<ReducIrLocal, ReducIr>,
    }
    impl ReducIrEndoFold for Subst<'_> {
        type Ext = Infallible;

        fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
            match kind {
                ReducIrKind::Var(v) => match self.env.get(&v.var) {
                    Some(val) => val.clone(),
                    None => ReducIr::new(kind),
                },
                _ => ReducIr::new(kind),
            }
        }
    }

    pub(crate) struct SubstTy<'a> {
        pub(crate) db: &'a dyn crate::Db,
        pub(crate) needle: ReducIrTyApp,
    }

    impl SubstTy<'_> {
        fn subst(&self, ty: ReducIrTy) -> ReducIrTy {
            self.needle.clone().subst_into(self.db.as_ir_db(), ty)
        }
    }

    impl ReducIrEndoFold for SubstTy<'_> {
        type Ext = Infallible;

        fn endofold_ir(&mut self, kind: ReducIrKind<Self::Ext>) -> ReducIr<Self::Ext> {
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
}

mod lift {
    use std::convert::Infallible;

    use aiahr_core::id::TermName;
    use aiahr_core::modules::Module;
    use aiahr_reducir::{ReducIr, ReducIrInPlaceFold, ReducIrKind, ReducIrTermName, TypeCheck};

    struct EtaExpand;

    impl ReducIrInPlaceFold for EtaExpand {
        type Ext = Infallible;

        fn fold_ir_in_place(&mut self, kind: &mut ReducIrKind<Self::Ext>) {
            if let ReducIrKind::Abs(_, _) = kind {
                let lam = ReducIr::new(std::mem::replace(kind, ReducIrKind::Int(0)));
                let free_vars = lam.free_var_set();
                *kind = ReducIr::app(
                    ReducIr::abss(free_vars.iter().copied(), lam),
                    free_vars.iter().map(|v| ReducIr::var(*v)),
                )
                .kind;
            }
        }
    }

    struct LambdaLift<'a> {
        db: &'a dyn crate::Db,
        module: Module,
        term_name: TermName,
        lifts: Vec<(ReducIrTermName, ReducIr)>,
    }
    impl ReducIrInPlaceFold for LambdaLift<'_> {
        type Ext = Infallible;

        fn fold_ir_in_place(&mut self, kind: &mut ReducIrKind<Self::Ext>) {
            if let ReducIrKind::Abs(_, _) = kind {
                let core_db = self.db.as_core_db();
                // Make up a name for our lambda's item
                let i = self.lifts.len();
                let name = ReducIrTermName::gen(
                    self.db.as_ir_db(),
                    format!("{}_lam_{}", self.term_name.name(core_db).text(core_db), i),
                    self.module,
                );
                // Replace our kind in place by our freshly generated item
                let lam = std::mem::replace(kind, ReducIrKind::Int(0));
                let lam_ir = ReducIr { kind: lam };
                let ty = lam_ir.type_check(self.db.as_ir_db()).unwrap();
                *kind = ReducIrKind::Item(name, ty);
                // Record our lifted lambda's name and body
                self.lifts.push((name, lam_ir));
            }
        }
    }

    pub(crate) fn lambda_lift(
        db: &dyn crate::Db,
        module: Module,
        term_name: TermName,
        mut ir: ReducIr,
    ) -> (ReducIr, Vec<(ReducIrTermName, ReducIr)>) {
        let mut lam_lift = LambdaLift {
            db,
            module,
            term_name,
            lifts: vec![],
        };
        ir.fold_in_place_inside_item(&mut EtaExpand);
        ir.fold_in_place_inside_item(&mut lam_lift);
        (ir, lam_lift.lifts)
    }
}

/// True if an ir term is a value (contains no computations), false if the term does require
/// computation that can be done
fn is_value(ir: &ReducIr) -> bool {
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

struct Simplify<'a> {
    db: &'a dyn crate::Db,
    row_evs: FxHashMap<ReducIrTermName, &'a ReducIr>,
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
                Struct(elems) => self.traverse_ir(&elems[indx]),
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
                                let arg = arg.fold(&mut Subst { env: &env }).fold(self);
                                if is_value(&arg) {
                                    // Later args may refer to earlier args we're about to
                                    // substitute
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
                            self.traverse_ir(&body.fold(&mut Subst { env: &env })),
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

fn simplify(db: &dyn crate::Db, item: ReducIrItem) -> ReducIr {
    let lower_db = db.as_lower_reducir_db();
    let row_evs = item.row_evs(lower_db);
    let ir = item.mon_item(lower_db);
    let inline_row_ev = row_evs
        .iter()
        .flat_map(|row_ev| {
            let simple_item = row_ev.simple(db.as_lower_reducir_db());
            let scoped_item = row_ev.scoped(db.as_lower_reducir_db());

            let simple = (
                ReducIrTermName::Gen(simple_item.name(lower_db)),
                simple_item.mon_item(lower_db),
            );
            let scoped = (
                ReducIrTermName::Gen(scoped_item.name(lower_db)),
                scoped_item.mon_item(lower_db),
            );
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

    use crate::{simplify, Db};

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
    fn simplify_state_get() {
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
                  (((_mon_freshm @ (Marker {} -> {{}, {}})) @ {1} -> (Control {1} {} -> { {}
                                                                                        , {}
                                                                                        }))
                    (fun [V18]
                      ((((__mon_prompt @ {1}) @ {0}) @ {} -> {{}, {}})
                        V18
                        (fun [V0]
                          (V1[0]
                            V0
                            {V18, {(fun [V10, V11, V12] (V11 {} V10)), (fun [V7, V8, V9]
                              (V8 V9 V9))}}))
                        ((((__mon_bind @ {0}) @ {}) @ {} -> {{}, {}})
                          (let (V16 (V1[3][0] V0))
                            (fun [V0]
                              <1: {V16[0], (fun [V17] (V16[1][1] {} V17)), (fun [V0] V0)}>))
                          (fun [V21] (fun [V0] <0: (fun [V14] {V14, V21})>))))))
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

    #[test]
    fn lambda_lift_state_get() {
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
        let ir_and_lifts = db.simple_reducir_item(ir);

        let expects = vec![
            // f ir
            expect![[r#"
                (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
                    ((((__mon_bind @ {1}) @ {} -> {{}, {}}) @ {{}, {}})
                      (((_mon_freshm @ (Marker {} -> {{}, {}})) @ {1} -> (Control {1} {} -> { {}
                                                                                            , {}
                                                                                            }))
                        (f_lam_9 V1 V0))
                      f_lam_11)))"#]],
            // f_lam_0
            expect!["(fun [V10, V11, V12] (V11 {} V10))"],
            // f_lam_1
            expect!["(fun [V7, V8, V9] (V8 V9 V9))"],
            // f_lam_2
            expect!["(fun [V1, V18, V0] (V1[0] V0 {V18, {f_lam_0, f_lam_1}}))"],
            // f_lam_3
            expect!["(fun [V16, V17] (V16[1][1] {} V17))"],
            // f_lam_4
            expect!["(fun [V0] V0)"],
            // f_lam_5
            expect!["(fun [V16, V0] <1: {V16[0], (f_lam_3 V16), f_lam_4}>)"],
            // f_lam_6
            expect!["(fun [V21, V14] {V14, V21})"],
            // f_lam_7
            expect!["(fun [V21, V0] <0: (f_lam_6 V21)>)"],
            // f_lam_8
            expect!["(fun [V21] (f_lam_7 V21))"],
            // f_lam_9
            expect![[r#"
                (fun [V1, V0, V18]
                  ((((__mon_prompt @ {1}) @ {0}) @ {} -> {{}, {}})
                    V18
                    (f_lam_2 V1 V18)
                    ((((__mon_bind @ {0}) @ {}) @ {} -> {{}, {}})
                      (f_lam_5 (V1[3][0] V0))
                      f_lam_8)))"#]],
            // f_lam_10
            expect!["(fun [V24, V0] <0: V24>)"],
            // f_lam_11
            expect!["(fun [V23] (f_lam_10 (V23 {})))"],
        ];

        assert_eq!(ir_and_lifts.len(), expects.len());
        for (ir, expect) in ir_and_lifts.into_iter().zip(expects.into_iter()) {
            let pretty_ir = ir
                .item(&db)
                .pretty_with(&db)
                .pprint()
                .pretty(80)
                .to_string();
            expect.assert_eq(&pretty_ir)
        }
    }
}
