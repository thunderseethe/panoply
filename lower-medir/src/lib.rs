use aiahr_core::id::{IdSupply, TermName};
use aiahr_core::id_converter::IdConverter;
use aiahr_core::ident::Ident;
use aiahr_core::modules::Module;
use aiahr_medir::{self as medir};
use aiahr_optimize_reducir::{ReducIrOptimizedItem, ReducIrOptimizedModule};
use medir::{MedIrItem, MedIrModule};

#[salsa::jar(db = Db)]
pub struct Jar(lower_item, lower_module);
pub trait Db: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db + medir::Db {
    fn as_lower_medir_db(&self) -> &dyn crate::Db {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
    }

    fn lower_medir_item_for_file_name(
        &self,
        path: std::path::PathBuf,
        item: Ident,
    ) -> Option<Vec<MedIrItem>> {
        let module = self.root_module_for_path(path);
        let term_name = self.id_for_name(module, item)?;
        // First index of lowering is always the item itself, the rest of the vector is our closure
        // conversions for that item
        Some(self.lower_medir_item(term_name))
    }

    fn lower_medir_item(&self, name: TermName) -> Vec<MedIrItem> {
        let opt_item = self.simplify_reducir_for_name(name);
        lower_item(self.as_lower_medir_db(), opt_item)
    }

    fn lower_medir_module(&self, module: Module) -> MedIrModule {
        let opt_module = self.simple_reducir_module(module);
        lower_module(self.as_lower_medir_db(), opt_module)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db + medir::Db {}

#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term: ReducIrOptimizedItem) -> Vec<MedIrItem> {
    let opt_db = db.as_opt_reducir_db();
    let mut converter = IdConverter::new();
    let mut ctx = lower::LowerCtx::new(db, term.name(opt_db), &mut converter);
    let defn = ctx.lower_item(term.item(opt_db));
    // This is to make lifetimes work how we need them
    let lifts = ctx.lifts;
    let supply: IdSupply<_> = converter.into();
    let mut defns = lifts
        .into_iter()
        .map(|defn| {
            MedIrItem::new(
                db.as_medir_db(),
                defn.name,
                defn,
                IdSupply::start_from(&supply),
            )
        })
        .collect::<Vec<_>>();
    defns.insert(0, MedIrItem::new(db.as_medir_db(), defn.name, defn, supply));
    defns
}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: ReducIrOptimizedModule) -> MedIrModule {
    let opt_db = db.as_opt_reducir_db();
    let items = module
        .items(opt_db)
        .iter()
        .flat_map(|opt_item| lower_item(db, *opt_item))
        .collect();
    let module = module.module(opt_db);
    MedIrModule::new(db.as_medir_db(), module, items)
}

mod lower {
    use aiahr_core::id::MedIrVarId;
    use aiahr_core::id_converter::IdConverter;
    use aiahr_medir as medir;
    use aiahr_medir::MedIrItemName;
    use aiahr_reducir::{Lets, ReducIr, ReducIrKind, ReducIrLocal, ReducIrTermName, ReducIrVar};
    use medir::{Atom, Locals, MedIr, MedIrKind, MedIrVar};

    pub(crate) struct LowerCtx<'a> {
        db: &'a dyn crate::Db,
        current: ReducIrTermName,
        var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
        pub(crate) lifts: Vec<medir::Defn>,
    }

    impl<'a> LowerCtx<'a> {
        pub(crate) fn new(
            db: &'a dyn crate::Db,
            current: ReducIrTermName,
            var_converter: &'a mut IdConverter<ReducIrLocal, MedIrVarId>,
        ) -> Self {
            Self {
                db,
                current,
                var_converter,
                lifts: vec![],
            }
        }

        pub(crate) fn lower_item(&mut self, reducir: &ReducIr<Lets>) -> medir::Defn {
            let mut binds = vec![];
            let (params, body) = match reducir.try_top_level_def() {
                Ok(top_level) => {
                    let params = top_level
                        .vars
                        .iter()
                        .map(|var| self.var_converter.convert(var.var))
                        .map(MedIrVar::new)
                        .collect();
                    (params, self.lower_binds(top_level.body, &mut binds))
                }
                Err(body) => (vec![], self.lower_binds(body, &mut binds)),
            };
            medir::Defn {
                name: MedIrItemName::new(self.current),
                params,
                body: medir::Locals { binds, body },
            }
        }

        fn mk_atom(&mut self, body: MedIr, binds: &mut Vec<(MedIrVar, MedIr)>) -> Atom {
            match body.kind {
                MedIrKind::Atom(atom) => atom,
                _ => {
                    let v = MedIrVar::new(self.var_converter.generate());
                    binds.push((v, body));
                    Atom::Var(v)
                }
            }
        }

        fn lower_binds(
            &mut self,
            body: &ReducIr<Lets>,
            binds: &mut Vec<(MedIrVar, MedIr)>,
        ) -> MedIr {
            match body.kind() {
                ReducIrKind::Int(i) => MedIr::int(*i),
                ReducIrKind::Var(v) => {
                    let var = self.var_converter.convert(v.var);
                    MedIr::var(MedIrVar::new(var))
                }
                ReducIrKind::Item(name, _) => {
                    MedIr::new(MedIrKind::Closure(MedIrItemName::new(*name), vec![]))
                }
                ReducIrKind::Abs(vars, body) => {
                    let (name, free_vars) = self.closure_convert(vars, body);
                    MedIr::new(MedIrKind::Closure(name, free_vars))
                }
                ReducIrKind::App(head, spine) => {
                    let medir_spine = spine
                        .iter()
                        .map(|arg| {
                            let medir = self.lower_binds(arg, binds);
                            match &medir.kind {
                                MedIrKind::Atom(atom) => *atom,
                                _ => {
                                    let var = MedIrVar::new(self.var_converter.generate());
                                    binds.push((var, medir));
                                    medir::Atom::Var(var)
                                }
                            }
                        })
                        .collect();
                    let medir = self.lower_binds(head, binds);
                    MedIr::new(match medir.kind {
                        MedIrKind::Atom(medir::Atom::Var(v)) => {
                            MedIrKind::Call(medir::Call::Unknown(v), medir_spine)
                        }
                        MedIrKind::Closure(item, args) => {
                            let args = args
                                .into_iter()
                                .map(medir::Atom::Var)
                                .chain(medir_spine)
                                .collect();
                            MedIrKind::Call(medir::Call::Known(item), args)
                        }
                        kind => {
                            let v = MedIrVar::new(self.var_converter.generate());
                            binds.push((v, MedIr::new(kind)));
                            MedIrKind::Call(medir::Call::Unknown(v), medir_spine)
                        }
                    })
                }
                // We get rid of types in this IR
                ReducIrKind::TyAbs(_, body) | ReducIrKind::TyApp(body, _) => {
                    self.lower_binds(body, binds)
                }
                ReducIrKind::Struct(elems) => {
                    // TODO: Flatten?
                    MedIr::new(MedIrKind::Blocks(
                        elems
                            .iter()
                            .map(|elem| {
                                let e = self.lower_binds(elem, binds);
                                self.mk_atom(e, binds)
                            })
                            .collect(),
                    ))
                }
                ReducIrKind::FieldProj(indx, body) => {
                    let base = self.lower_binds(body, binds);
                    let base_var = match self.mk_atom(base, binds) {
                        Atom::Var(v) => v,
                        _ => unreachable!(),
                    };
                    MedIr::new(MedIrKind::BlockAccess(base_var, *indx))
                }
                ReducIrKind::Tag(_, tag, body) => {
                    // TODO: Flatten?
                    let value = self.lower_binds(body, binds);
                    let value_atom = self.mk_atom(value, binds);
                    MedIr::new(MedIrKind::Blocks(vec![Atom::Int(*tag), value_atom]))
                }
                ReducIrKind::Case(_, scrutinee, branches) => {
                    let medir_scrutinee = self.lower_binds(scrutinee, binds);
                    let scrutinee_var = match self.mk_atom(medir_scrutinee, binds) {
                        Atom::Var(v) => v,
                        Atom::Int(_) => unreachable!(),
                    };
                    let discr_var = MedIrVar::new(self.var_converter.generate());
                    binds.push((
                        discr_var,
                        MedIr::new(MedIrKind::BlockAccess(scrutinee_var, 0)),
                    ));
                    let cases = branches
                        .iter()
                        .map(|branch| {
                            match branch.kind() {
                                ReducIrKind::Abs(vars, body) => {
                                    let branch_var =
                                        MedIrVar::new(self.var_converter.convert(vars[0].var));
                                    let scrutinee_value =
                                        MedIr::new(MedIrKind::BlockAccess(scrutinee_var, 1));
                                    //TODO: Handle flattening?
                                    let mut binds = vec![(branch_var, scrutinee_value)];
                                    let body = self.lower_binds(body, &mut binds);
                                    medir::Locals { binds, body }
                                }
                                _ => unreachable!(),
                            }
                        })
                        .collect();
                    MedIr::new(MedIrKind::Switch(Atom::Var(discr_var), cases))
                }
                ReducIrKind::X(Lets {
                    binds: reducir_binds,
                    body,
                }) => {
                    for (var, body) in reducir_binds.iter() {
                        let defn = self.lower_binds(body, binds);
                        binds.push((MedIrVar::new(self.var_converter.convert(var.var)), defn));
                    }
                    self.lower_binds(body, binds)
                }
            }
        }

        fn closure_convert(
            &mut self,
            vars: &[ReducIrVar],
            body: &ReducIr<Lets>,
        ) -> (MedIrItemName, Vec<MedIrVar>) {
            let mut free_vars = body
                .free_var_set()
                .into_iter()
                .filter(|var| !vars.contains(var))
                .map(|var| self.var_converter.convert(var.var))
                .map(MedIrVar::new)
                .collect::<Vec<_>>();
            free_vars.sort();

            let params = free_vars
                .iter()
                .copied()
                .chain(
                    vars.iter()
                        .copied()
                        .map(|var| self.var_converter.convert(var.var))
                        .map(MedIrVar::new),
                )
                .collect();
            let mut binds = vec![];
            let body = self.lower_binds(body, &mut binds);

            let num = self.lifts.len();
            let current_name = self.current.name(self.db).text(self.db.as_core_db());
            let module = self.current.module(self.db);
            let lift_name =
                ReducIrTermName::gen(self.db, format!("{}_lam_{}", current_name, num), module);
            let name = MedIrItemName::new(lift_name);
            self.lifts.push(medir::Defn {
                name,
                params,
                body: Locals { binds, body },
            });
            (name, free_vars)
        }
    }
}

#[cfg(test)]
mod tests {
    use aiahr_core::file::{FileId, SourceFile, SourceFileSet};
    use aiahr_core::pretty::{PrettyPrint, PrettyWithCtx};
    use aiahr_core::Db as CoreDb;
    use aiahr_parser::Db as ParseDb;
    use expect_test::expect;

    use crate::{Db, MedIrItem};

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        aiahr_ast::Jar,
        aiahr_core::Jar,
        aiahr_desugar::Jar,
        aiahr_lower_reducir::Jar,
        aiahr_medir::Jar,
        aiahr_nameres::Jar,
        aiahr_optimize_reducir::Jar,
        aiahr_parser::Jar,
        aiahr_reducir::Jar,
        aiahr_tc::Jar,
        aiahr_ty::Jar
    )]
    struct TestDatabase {
        storage: salsa::Storage<Self>,
    }
    impl salsa::Database for TestDatabase {}

    fn lower_function(db: &TestDatabase, input: &str, fn_name: &str) -> Vec<MedIrItem> {
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

        match db.lower_medir_item_for_file_name(path, db.ident_str(fn_name)) {
            Some(term) => term,
            None => {
                dbg!(db.all_parse_errors());
                panic!("Errors occurred")
            }
        }
    }
    fn lower_snippet(db: &TestDatabase, input: &str) -> Vec<MedIrItem> {
        let main = format!("f = {}", input);
        lower_function(db, &main, "f")
    }

    #[test]
    fn lower_medir_state_get() {
        let db = TestDatabase::default();
        let items = lower_snippet(
            &db,
            r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))({})"#,
        );

        let expects = vec![
            expect![[r#"
                defn f(V0, V1) {
                  let V2 = [];
                  let V3 = __mon_generate_marker(V2);
                  let V16 = make_closure(f_lam_2,[V0, V3]);
                  let V28 = make_closure(f_lam_5,[V0]);
                  let V32 = make_closure(f_lam_8,[]);
                  let V33 = __mon_bind(V28, V32);
                  let V34 = __mon_prompt(V3, V16, V33);
                  let V38 = make_closure(f_lam_10,[]);
                  __mon_bind(V34, V38)
                }"#]],
            expect![[r#"
                defn f_lam_0(V4, V5, V6) {
                  let V7 = [];
                  apply_closure(V5)(V7, V4)
                }"#]],
            expect![[r#"
                defn f_lam_1(V9, V10, V11) {
                  apply_closure(V10)(V11, V11)
                }"#]],
            expect![[r#"
                defn f_lam_2(V0, V3, V1) {
                  let V8 = make_closure(f_lam_0,[]);
                  let V12 = make_closure(f_lam_1,[]);
                  let V13 = [V8, V12];
                  let V14 = [V3, V13];
                  let V15 = V0[0];
                  apply_closure(V15)(V1, V14)
                }"#]],
            expect![[r#"
                defn f_lam_3(V19, V21) {
                  let V22 = [];
                  let V23 = V19[1];
                  let V24 = V23[1];
                  apply_closure(V24)(V22, V21)
                }"#]],
            expect![[r#"
                defn f_lam_4(V1) {
                  V1
                }"#]],
            expect![[r#"
                defn f_lam_5(V0, V1) {
                  let V17 = V0[3];
                  let V18 = V17[0];
                  let V19 = apply_closure(V18)(V1);
                  let V20 = V19[0];
                  let V25 = make_closure(f_lam_3,[V19]);
                  let V26 = make_closure(f_lam_4,[]);
                  let V27 = [V20, V25, V26];
                  [1, V27]
                }"#]],
            expect![[r#"
                defn f_lam_6(V29, V30) {
                  [V30, V29]
                }"#]],
            expect![[r#"
                defn f_lam_7(V29, V1) {
                  let V31 = make_closure(f_lam_6,[V29]);
                  [0, V31]
                }"#]],
            expect![[r#"
                defn f_lam_8(V29) {
                  make_closure(f_lam_7,[V29])
                }"#]],
            expect![[r#"
                defn f_lam_9(V35, V1) {
                  let V36 = [];
                  let V37 = apply_closure(V35)(V36);
                  [0, V37]
                }"#]],
            expect![[r#"
                defn f_lam_10(V35) {
                  make_closure(f_lam_9,[V35])
                }"#]],
        ];

        assert_eq!(items.len(), expects.len());
        for (item, expect) in items.into_iter().zip(expects.into_iter()) {
            let str = item
                .item(&db)
                .pretty_with(&db)
                .pprint()
                .pretty(80)
                .to_string();
            expect.assert_eq(&str)
        }
    }
}
