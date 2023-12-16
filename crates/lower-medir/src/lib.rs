use base::{
    id::{IdSupply, TermName},
    id_converter::IdConverter,
    ident::Ident,
    modules::Module,
};
use medir::{MedIrItem, MedIrModule};
use reducir::optimized::{OptimizedReducIrItem, OptimizedReducIrModule};

mod lower;

#[salsa::jar(db = Db)]
pub struct Jar(lower_item, lower_module);
pub trait Db: salsa::DbWithJar<Jar> + optimize_reducir::Db + medir::Db {
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
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + optimize_reducir::Db + medir::Db {}

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
    use base::{
        file::{FileId, SourceFile, SourceFileSet},
        pretty::{PrettyPrint, PrettyWithCtx},
        Db as BaseDb,
    };
    use expect_test::expect;
    use parser::Db as ParseDb;

    use crate::{Db, MedIrItem};

    #[derive(Default)]
    #[salsa::db(
        crate::Jar,
        ast::Jar,
        base::Jar,
        desugar::Jar,
        lower_reducir::Jar,
        medir::Jar,
        nameres::Jar,
        optimize_reducir::Jar,
        parser::Jar,
        reducir::Jar,
        tc::Jar,
        ty::Jar
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
                  let V69 = make_closure(f_lam_16,[V0, V3]);
                  let V70 = V1;
                  let V71 = apply_closure(V69)(V70);
                  let V72 = V71[0];
                  switch V72 <
                    branch 0 {
                      let V73 = V71[1];
                      let V74 = [];
                      let V75 = apply_closure(V73)(V74);
                      typecast<[Int,Int,Int,Int]>([0, V75])
                    }
                    branch 1 {
                      let V76 = V71[1];
                      let V77 = V76[0];
                      let V78 = V76[1];
                      let V82 = make_closure(f_lam_18,[V76]);
                      let V83 = [V77, V78, V82];
                      typecast<[Int,Int,Int,Int]>([1, V83])
                    }
                  >
                }"#]],
            expect![[r#"
                defn f_lam_0(V7, V9) {
                  let V10 = [];
                  let V11 = V7[1];
                  let V12 = V11[1];
                  apply_closure(V12)(V10, V9)
                }"#]],
            expect![[r#"
                defn f_lam_1(V1) {
                  V1
                }"#]],
            expect![[r#"
                defn f_lam_2(V0, V1) {
                  let V5 = V0[3];
                  let V6 = V5[0];
                  let V7 = apply_closure(V6)(V1);
                  let V8 = V7[0];
                  let V13 = make_closure(f_lam_0,[V7]);
                  let V14 = make_closure(f_lam_1,[]);
                  let V15 = [V8, V13, V14];
                  typecast<[Int,Int,Int,Int]>([1, V15])
                }"#]],
            expect![[r#"
                defn f_lam_3(V17, V18) {
                  [V18, V17]
                }"#]],
            expect![[r#"
                defn f_lam_4(V17, V1) {
                  let V19 = make_closure(f_lam_3,[V17]);
                  typecast<[Int,Int,Int,Int]>([0, V19])
                }"#]],
            expect![[r#"
                defn f_lam_5(V17) {
                  make_closure(f_lam_4,[V17])
                }"#]],
            expect![[r#"
                defn f_lam_6(V22, V23, V24) {
                  let V25 = [];
                  apply_closure(V23)(V25, V22)
                }"#]],
            expect![[r#"
                defn f_lam_7(V27, V28, V29) {
                  apply_closure(V28)(V29, V29)
                }"#]],
            expect![[r#"
                defn f_lam_8(V22, V23, V24) {
                  let V46 = [];
                  apply_closure(V23)(V46, V22)
                }"#]],
            expect![[r#"
                defn f_lam_9(V27, V28, V29) {
                  apply_closure(V28)(V29, V29)
                }"#]],
            expect![[r#"
                defn f_lam_10(V0, V3, V1) {
                  let V47 = make_closure(f_lam_8,[]);
                  let V48 = make_closure(f_lam_9,[]);
                  let V49 = [V47, V48];
                  let V50 = [V3, V49];
                  let V51 = V0[0];
                  apply_closure(V51)(V1, V50)
                }"#]],
            expect![[r#"
                defn f_lam_11(V0, V3, V38, V45) {
                  let V52 = make_closure(f_lam_10,[V0, V3]);
                  let V53 = V38[2];
                  let V54 = apply_closure(V53)(V45);
                  make_closure(__mon_prompt,[V3, V52, V54])
                }"#]],
            expect![[r#"
                defn f_lam_12(V22, V23, V24) {
                  let V58 = [];
                  apply_closure(V23)(V58, V22)
                }"#]],
            expect![[r#"
                defn f_lam_13(V27, V28, V29) {
                  apply_closure(V28)(V29, V29)
                }"#]],
            expect![[r#"
                defn f_lam_14(V0, V3, V1) {
                  let V59 = make_closure(f_lam_12,[]);
                  let V60 = make_closure(f_lam_13,[]);
                  let V61 = [V59, V60];
                  let V62 = [V3, V61];
                  let V63 = V0[0];
                  apply_closure(V63)(V1, V62)
                }"#]],
            expect![[r#"
                defn f_lam_15(V0, V3, V38, V57) {
                  let V64 = make_closure(f_lam_14,[V0, V3]);
                  let V65 = V38[2];
                  let V66 = apply_closure(V65)(V57);
                  make_closure(__mon_prompt,[V3, V64, V66])
                }"#]],
            expect![[r#"
                defn f_lam_16(V0, V3, V4) {
                  let V16 = make_closure(f_lam_2,[V0]);
                  let V20 = make_closure(f_lam_5,[]);
                  let V21 = make_closure(__mon_bind,[V16, V20]);
                  let V26 = make_closure(f_lam_6,[]);
                  let V30 = make_closure(f_lam_7,[]);
                  let V31 = [V26, V30];
                  let V32 = [V3, V31];
                  let V33 = V0[0];
                  let V34 = apply_closure(V33)(V4, V32);
                  let V35 = apply_closure(V21)(V34);
                  let V36 = V35[0];
                  switch V36 <
                    branch 0 {
                      let V37 = V35[1];
                      typecast<[Int,Int,Int,Int]>([0, V37])
                    }
                    branch 1 {
                      let V38 = V35[1];
                      let V39 = V38[0];
                      let V40 = __mon_eqm(V3, V39);
                      let V41 = V40[0];
                      switch V41 <
                        branch 0 {
                          let V42 = V40[1];
                          let V43 = V38[0];
                          let V44 = V38[1];
                          let V55 = make_closure(f_lam_11,[V0, V3, V38]);
                          let V56 = [V43, V44, V55];
                          typecast<[Int,Int,Int,Int]>([1, V56])
                        }
                        branch 1 {
                          let V42 = V40[1];
                          let V67 = make_closure(f_lam_15,[V0, V3, V38]);
                          let V68 = V38[1];
                          apply_closure(V68)(V67, V4)
                        }
                      >
                    }
                  >
                }"#]],
            expect![[r#"
                defn f_lam_17(V73, V1) {
                  let V79 = [];
                  let V75 = apply_closure(V73)(V79);
                  typecast<[Int,Int,Int,Int]>([0, V75])
                }"#]],
            expect![[r#"
                defn f_lam_18(V76, V73) {
                  let V80 = make_closure(f_lam_17,[V73]);
                  let V81 = V76[2];
                  make_closure(__mon_bind,[V80, V81])
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
