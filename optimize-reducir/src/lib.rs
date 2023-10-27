use aiahr_core::id::{ReducIrTyVarId, TermName};
use aiahr_core::modules::Module;
use aiahr_reducir::mon::{MonReducIrItem, MonReducIrModule};
use aiahr_reducir::optimized::{OptimizedReducIrItem, OptimizedReducIrModule};
use aiahr_reducir::ty::{Kind, MkReducIrTy, ReducIrTyApp, ReducIrTyKind, ReducIrVarTy};
use aiahr_reducir::zip_non_consuming::ZipNonConsuming;
use rustc_hash::FxHashMap;
use std::convert::Infallible;

use aiahr_reducir::{
    Lets, ReducIr, ReducIrFold, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar, P,
};

use crate::subst::{Subst, SubstTy};

#[salsa::jar(db = Db)]
pub struct Jar(simple_reducir_item, simple_reducir_module);

pub trait Db: salsa::DbWithJar<Jar> + aiahr_lower_reducir::Db {
    fn as_opt_reducir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn simplify_reducir_for_name(&self, name: TermName) -> OptimizedReducIrItem {
        let reducir_item = self.lower_reducir_mon_item_of(name);
        self.simple_reducir_item(reducir_item)
    }

    fn simple_reducir_item(&self, item: MonReducIrItem) -> OptimizedReducIrItem {
        simple_reducir_item(self.as_opt_reducir_db(), item)
    }

    fn simple_reducir_module(&self, module: Module) -> OptimizedReducIrModule {
        let ir_module = self.lower_reducir_mon_module_of(module);
        simple_reducir_module(self.as_opt_reducir_db(), ir_module)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_lower_reducir::Db {}

#[salsa::tracked]
fn simple_reducir_item(db: &dyn crate::Db, item: MonReducIrItem) -> OptimizedReducIrItem {
    let ir = simplify(db, item);

    let term_name = item.name(db.as_reducir_db());
    OptimizedReducIrItem::new(db.as_reducir_db(), ReducIrTermName::Term(term_name), ir)
}

#[salsa::tracked]
fn simple_reducir_module(
    db: &dyn crate::Db,
    ir_module: MonReducIrModule,
) -> OptimizedReducIrModule {
    let reducir_db = db.as_reducir_db();
    OptimizedReducIrModule::new(
        db.as_reducir_db(),
        ir_module.module(reducir_db),
        ir_module
            .items(reducir_db)
            .iter()
            .map(|item| db.simple_reducir_item(*item))
            .collect(),
    )
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
            self.needle.clone().subst_into(self.db.as_reducir_db(), ty)
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
                        needle: self.needle.clone().shift(self.db.as_reducir_db(), 1),
                    })),
                )),
                ReducIrKind::TyApp(body, ty_app) => {
                    let ty_app = match ty_app {
                        ReducIrTyApp::Ty(ty) => ReducIrTyApp::Ty(
                            self.needle.clone().subst_into(self.db.as_reducir_db(), ty),
                        ),
                        _ => todo!(),
                    };
                    ReducIr::new(ReducIrKind::TyApp(body, ty_app))
                }
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

struct InsertLet;
impl ReducIrFold for InsertLet {
    type InExt = Infallible;

    type OutExt = Lets;

    fn fold_ext(&mut self, _: &Self::InExt) -> Self::OutExt {
        unreachable!()
    }

    fn fold_ir(&mut self, kind: ReducIrKind<Self::OutExt>) -> ReducIr<Self::OutExt> {
        match kind {
            ReducIrKind::App(head, spine) => match head.into_inner().kind {
                ReducIrKind::Abs(vars, body) => {
                    let mut vars_iter = vars.iter().copied().peekable();
                    let mut args_iter = spine.into_iter().peekable();
                    let binds = vars_iter
                        .zip_non_consuming(&mut args_iter)
                        .collect::<Vec<_>>();
                    ReducIr::app(
                        ReducIr::abss(
                            vars_iter,
                            // We want these lets inside the abs so they're not considered free.
                            ReducIr::new(ReducIrKind::X(Lets { binds, body })),
                        ),
                        args_iter,
                    )
                }
                head => ReducIr::app(ReducIr::new(head), spine),
            },
            kind => ReducIr::new(kind),
        }
    }
}

struct Simplify<'a> {
    db: &'a dyn crate::Db,
    builtin_evs: FxHashMap<ReducIrTermName, &'a ReducIr>,
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
            Item(name, _) => match self.builtin_evs.get(&name) {
                Some(ir) => self.traverse_ir(*ir),
                None => ReducIr::new(ir),
            },
            ir => ReducIr::new(ir),
        }
    }
}

fn simplify(db: &dyn crate::Db, item: MonReducIrItem) -> ReducIr<Lets> {
    let reducir_db = db.as_reducir_db();
    let row_evs = item.row_evs(reducir_db);
    let ir = item.item(reducir_db);
    let mut builtin_evs = row_evs
        .iter()
        .flat_map(|row_ev| {
            let simple_item = row_ev.simple(reducir_db);
            let scoped_item = row_ev.scoped(reducir_db);

            let simple = (
                ReducIrTermName::Gen(simple_item.name(reducir_db)),
                simple_item.item(reducir_db),
            );
            let scoped = (
                ReducIrTermName::Gen(scoped_item.name(reducir_db)),
                scoped_item.item(reducir_db),
            );
            [simple, scoped]
        })
        .collect::<FxHashMap<_, _>>();

    let module = item.name(reducir_db).module(db.as_core_db());
    let name = ReducIrTermName::gen(db, "__mon_freshm", module);
    let freshm_term = freshm_term(db, module, name);
    builtin_evs.insert(name, &freshm_term);

    ir.fold(&mut Simplify { db, builtin_evs })
        .fold(&mut InsertLet)
}

fn freshm_term(db: &dyn crate::Db, module: Module, top_level: ReducIrTermName) -> ReducIr {
    let a = ReducIrVarTy {
        var: ReducIrTyVarId(0),
        kind: Kind::Type,
    };
    let b = ReducIrVarTy {
        var: ReducIrTyVarId(1),
        kind: Kind::Type,
    };
    let var_ty0 = db.mk_reducir_ty(ReducIrTyKind::VarTy(0));
    let var_ty1 = db.mk_reducir_ty(ReducIrTyKind::VarTy(1));
    let f = ReducIrVar {
        var: ReducIrLocal {
            top_level,
            id: aiahr_core::id::ReducIrVarId(0),
        },
        ty: db.mk_fun_ty(
            [db.mk_reducir_ty(ReducIrTyKind::MarkerTy(var_ty1))],
            var_ty0,
        ),
    };

    let marker = ReducIr::new(ReducIrKind::Item(
        ReducIrTermName::gen(db, "__mon_generate_marker", module),
        db.mk_forall_ty(
            [Kind::Type],
            db.mk_fun_ty(
                [db.mk_reducir_ty(ReducIrTyKind::ProductTy(vec![]))],
                db.mk_reducir_ty(ReducIrTyKind::MarkerTy(var_ty0)),
            ),
        ),
    ));

    ReducIr::new(ReducIrKind::TyAbs(
        a,
        P::new(ReducIr::new(ReducIrKind::TyAbs(
            b,
            P::new(ReducIr::abss(
                [f],
                ReducIr::app(
                    ReducIr::var(f),
                    [ReducIr::app(
                        ReducIr::ty_app(marker, [ReducIrTyApp::Ty(var_ty1)]),
                        [ReducIr::new(ReducIrKind::Struct(vec![]))],
                    )],
                ),
            )),
        ))),
    ))
}

/*/// Prompt handles "installing" our prompt into the stack and running an action under an
/// updated effect row
fn prompt_term(db: &dyn crate::Db, module: Module) -> ReducIr {
    use ReducIrTyKind::*;
    let m_ty = db.mk_reducir_ty(VarTy(2));
    let upd_m = db.mk_reducir_ty(VarTy(1));
    let a = db.mk_reducir_ty(VarTy(0));

    let mark_ty = db.mk_reducir_ty(MarkerTy(a));
    let upd_fun_ty = db.mk_fun_ty([m_ty], upd_m);
    let body_fun_ty = db.mk_fun_ty([upd_m], db.mk_reducir_ty(ControlTy(upd_m, a)));
    let ret_ty = db.mk_reducir_ty(ControlTy(m_ty, a));

    let prompt_type = db.mk_forall_ty(
        [Kind::Type, Kind::Type, Kind::Type],
        db.mk_fun_ty(
            [mark_ty, upd_fun_ty, body_fun_ty],
            db.mk_fun_ty([m_ty], ret_ty),
        ),
    );
    let top_level = ReducIrTermName::gen(db, "__mon_prompt", module);
    let mut var_gen = IdGen::new();
    let mut gen_local = || ReducIrLocal {
        top_level,
        id: var_gen.generate(),
    };
    let mark = ReducIrVar {
        var: gen_local(),
        ty: mark_ty,
    };
    let upd = ReducIrVar {
        var: gen_local(),
        ty: upd_fun_ty,
    };
    let body = ReducIrVar {
        var: gen_local(),
        ty: body_fun_ty,
    };
    let evv = ReducIrVar {
        var: gen_local(),
        ty: m_ty,
    };
    let x = ReducIrVar {
        var: gen_local(),
        ty: a,
    };
    let y = ReducIrVar {
        var: gen_local(),
        ty: db.mk_reducir_ty(ReducIrTyKind::ProductTy(vec![mark_ty])),
    };

    let unit = db.mk_reducir_ty(ProductTy(vec![]));
    let meq = ReducIr::new(ReducIrKind::Item(
        ReducIrTermName::gen(db, "__mon_eqm", module),
        db.mk_fun_ty(
            [m_ty, m_ty],
            db.mk_reducir_ty(CoproductTy(vec![unit, unit])),
        ),
    ));

    /*
      prompt : ∀𝜇 𝜇'. ∀𝛼 h. (𝜇 -> {Marker 𝜇 𝛼, h} -> Mon 𝜇' 𝛼) -> Marker 𝜇 𝛼 -> h -> Mon 𝜇' 𝛼 -> Mon 𝜇 𝛼
      prompt upd m h e = 𝜆w. case e (upd w {m, h}) of
        Pure x -> Pure x
        Yield m' f k ->
            case m == m' of
                False -> Yield m' f (\x . prompt upd m h (k x))
                True -> f (\x . prompt upd m h (k x)) w
    */
    let m_ = ReducIr::field_proj(0, ReducIr::var(y));
    let f = ReducIr::field_proj(1, ReducIr::var(y));
    let x = ReducIrVar { var: gen_local(), ty: db.mk_reducir_ty(ReducIrTyKind::IntTy) };
    ReducIr::abss(
        [mark, upd, body, evv],
        ReducIr::case(
            ret_ty,
            ReducIr::app(
                ReducIr::var(body),
                [ReducIr::app(ReducIr::var(upd), [ReducIr::var(evv)])],
            ),
            [
                ReducIr::abss(
                    [x],
                    ReducIr::new(ReducIrKind::Tag(ret_ty, 0, P::new(ReducIr::var(x)))),
                ),
                ReducIr::abss(
                    [y],
                    ReducIr::case(
                        ret_ty,
                        ReducIr::app(meq, [ReducIr::var(mark), m_]),
                        [ReducIr::abss(
                            [ReducIrVar {
                                var: gen_local(),
                                ty: unit,
                            }],
                            ReducIr::new(ReducIrKind::Tag(
                                ret_ty,
                                1,
                                ReducIr::new(ReducIrKind::Struct(vec![
                                    m_,
                                    f,
                                    ReducIr::abss(
                                        [todo!()],
                                        ReducIr::app(f, []),
                                    ),
                                ])),
                            )),
                        )],
                    ),
                ),
            ],
        ),
    )
}

/// TODO: Return an item representing the bind implementation of our delimited continuation
/// monad
fn bind_item(&mut self) -> ReducIr {
    /*
    (e: Mon m a) |> (g : a -> Mon m b) : Mon m b =
       𝜆w. case e w of
           Pure x → g x w (monadic bind)
           Yield m f k → Yield m f (𝜆x. g x |> k)
    */
    let m = self.mk_reducir_ty(VarTy(2));
    let a = self.mk_reducir_ty(VarTy(1));
    let b = self.mk_reducir_ty(VarTy(0));
    let mon_m_b = self.mk_mon_ty(m, b);

    let bind_type = self.mk_forall_ty(
        [Kind::Type, Kind::Type, Kind::Type],
        self.mk_fun_ty(
            [self.mk_mon_ty(m, a), self.mk_fun_ty([a], mon_m_b)],
            mon_m_b,
        ),
    );
    ReducIr::new(Item(
        ReducIrTermName::gen(self.db, "__mon_bind", self.current.module(self.db)),
        bind_type,
    ))
}*/

#[cfg(test)]
mod tests {

    use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
    use aiahr_core::pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx};
    use aiahr_core::Db as CoreDb;
    use aiahr_lower_reducir::Db as LowerDb;
    use aiahr_parser::Db as ParseDb;
    use aiahr_reducir::{mon::MonReducIrItem, TypeCheck};
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

    fn lower_function(db: &TestDatabase, input: &str, fn_name: &str) -> MonReducIrItem {
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

        match db.lower_reducir_mon_item_for_file_name(path, db.ident_str(fn_name)) {
            Some(term) => term,
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }

    fn lower_mon_snippet(db: &TestDatabase, input: &str) -> MonReducIrItem {
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
                ((((__mon_bind @ Ty({1})) @ Ty({} -> {{}, {}})) @ Ty({{}, {}}))
                  (let (V18 ((__mon_generate_marker @ Ty({} -> {{}, {}})) {}))
                    ((((__mon_prompt @ Ty({1})) @ Ty({0})) @ Ty({} -> {{}, {}}))
                      V18
                      (fun [V0]
                        (V1[0]
                          V0
                          {V18, {(fun [V10, V11, V12] (V11 {} V10)), (fun [V7, V8, V9]
                            (V8 V9 V9))}}))
                      ((((__mon_bind @ Ty({0})) @ Ty({})) @ Ty({} -> {{}, {}}))
                        (fun [V0]
                          (let (V16 (V1[3][0] V0))
                            <1: {V16[0], (fun [V17] (V16[1][1] {} V17)), (fun [V0] V0)}>))
                        (fun [V21] (fun [V0] <0: (fun [V14] {V14, V21})>)))))
                  (fun [V23] (fun [V0] (let (V24 (V23 {})) <0: V24>)))
                  V0)))"#]];
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
        let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
        expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
    }
}
