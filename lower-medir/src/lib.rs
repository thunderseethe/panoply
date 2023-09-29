use aiahr_core::id::{IdSupply, TermName};
use aiahr_core::id_converter::IdConverter;
use aiahr_core::ident::Ident;
use aiahr_core::modules::Module;
use aiahr_medir::{self as medir};
use aiahr_reducir::optimized::{OptimizedReducIrItem, OptimizedReducIrModule};
use medir::{MedIrItem, MedIrModule};

mod lower;

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
        Some(self.lower_medir_item_of(term_name))
    }

    fn lower_medir_item_of(&self, name: TermName) -> Vec<MedIrItem> {
        let opt_item = self.simplify_reducir_for_name(name);
        lower_item(self.as_lower_medir_db(), opt_item)
    }

    fn lower_medir_module_of(&self, module: Module) -> MedIrModule {
        let opt_module = self.simple_reducir_module(module);
        lower_module(self.as_lower_medir_db(), opt_module)
    }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + aiahr_optimize_reducir::Db + medir::Db {}

#[salsa::tracked]
fn lower_item(db: &dyn crate::Db, term: OptimizedReducIrItem) -> Vec<MedIrItem> {
    let reducir_db = db.as_reducir_db();
    let medir_db = db.as_medir_db();
    let mut converter = IdConverter::new();
    let mut ctx = lower::LowerCtx::new(db, term.name(reducir_db), &mut converter);
    let defn = ctx.lower_item(term.item(reducir_db));
    // This is to make lifetimes work how we need them
    let lifts = ctx.lifts;
    let supply: IdSupply<_> = converter.into();
    let mut defns = lifts
        .into_iter()
        .map(|defn| MedIrItem::new(medir_db, defn.name, defn, IdSupply::start_from(&supply)))
        .collect::<Vec<_>>();
    defns.insert(0, MedIrItem::new(medir_db, defn.name, defn, supply));
    defns
}

#[salsa::tracked]
fn lower_module(db: &dyn crate::Db, module: OptimizedReducIrModule) -> MedIrModule {
    let reducir_db = db.as_reducir_db();
    let items = module
        .items(reducir_db)
        .iter()
        .flat_map(|opt_item| lower_item(db, *opt_item))
        .collect();
    let module = module.module(reducir_db);
    MedIrModule::new(db.as_medir_db(), module, items)
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
