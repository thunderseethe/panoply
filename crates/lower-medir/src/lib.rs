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
        let path = std::path::PathBuf::from("test");
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
                  let V68 = make_closure(f_lam_16,[V0]);
                  let V69 = V1;
                  let V70 = apply_closure(V68)(V69);
                  let V71 = V70[0];
                  switch V71 <
                    branch 0 {
                      let V72 = V70[1];
                      let V73 = [];
                      let V74 = apply_closure(V72)(V73);
                      typecast<[Int,Int,Int,Int]>([0, V74])
                    }
                    branch 1 {
                      let V75 = V70[1];
                      let V76 = V75;
                      let V77 = V76[0];
                      let V78 = V76[1];
                      let V82 = make_closure(f_lam_18,[V76]);
                      let V83 = [V77, V78, V82];
                      typecast<[Int,Int,Int,Int]>([1, V83])
                    }
                  >
                }"#]],
            expect![[r#"
                defn f_lam_0(V5, V6, V7) {
                  let V8 = [];
                  apply_closure(V6)(V8, V5)
                }"#]],
            expect![[r#"
                defn f_lam_1(V10, V11, V12) {
                  apply_closure(V11)(V12, V12)
                }"#]],
            expect![[r#"
                defn f_lam_2(V19, V21) {
                  let V22 = [];
                  let V23 = V19[1];
                  let V24 = V23[1];
                  apply_closure(V24)(V22, V21)
                }"#]],
            expect![[r#"
                defn f_lam_3(V26, V1) {
                  typecast<[Int,Int,Int,Int]>([0, V26])
                }"#]],
            expect![[r#"
                defn f_lam_4(V0, V1) {
                  let V17 = V0[3];
                  let V18 = V17[0];
                  let V19 = apply_closure(V18)(V1);
                  let V20 = V19[0];
                  let V25 = make_closure(f_lam_2,[V19]);
                  let V27 = make_closure(f_lam_3,[]);
                  let V28 = [V20, V25, V27];
                  typecast<[Int,Int,Int,Int]>([1, V28])
                }"#]],
            expect![[r#"
                defn f_lam_5(V30, V31) {
                  [V31, V30]
                }"#]],
            expect![[r#"
                defn f_lam_6(V30, V1) {
                  let V32 = make_closure(f_lam_5,[V30]);
                  typecast<[Int,Int,Int,Int]>([0, V32])
                }"#]],
            expect![[r#"
                defn f_lam_7(V30) {
                  make_closure(f_lam_6,[V30])
                }"#]],
            expect![[r#"
                defn f_lam_8(V5, V6, V7) {
                  let V45 = [];
                  apply_closure(V6)(V45, V5)
                }"#]],
            expect![[r#"
                defn f_lam_9(V10, V11, V12) {
                  apply_closure(V11)(V12, V12)
                }"#]],
            expect![[r#"
                defn f_lam_10(V0, V4, V1) {
                  let V46 = make_closure(f_lam_8,[]);
                  let V47 = make_closure(f_lam_9,[]);
                  let V48 = [V46, V47];
                  let V49 = [V4, V48];
                  let V50 = V0[0];
                  apply_closure(V50)(V1, V49)
                }"#]],
            expect![[r#"
                defn f_lam_11(V0, V4, V37, V44) {
                  let V51 = make_closure(f_lam_10,[V0, V4]);
                  let V52 = V37[2];
                  let V53 = apply_closure(V52)(V44);
                  make_closure(__mon_prompt,[V4, V51, V53])
                }"#]],
            expect![[r#"
                defn f_lam_12(V5, V6, V7) {
                  let V57 = [];
                  apply_closure(V6)(V57, V5)
                }"#]],
            expect![[r#"
                defn f_lam_13(V10, V11, V12) {
                  apply_closure(V11)(V12, V12)
                }"#]],
            expect![[r#"
                defn f_lam_14(V0, V4, V1) {
                  let V58 = make_closure(f_lam_12,[]);
                  let V59 = make_closure(f_lam_13,[]);
                  let V60 = [V58, V59];
                  let V61 = [V4, V60];
                  let V62 = V0[0];
                  apply_closure(V62)(V1, V61)
                }"#]],
            expect![[r#"
                defn f_lam_15(V0, V4, V37, V56) {
                  let V63 = make_closure(f_lam_14,[V0, V4]);
                  let V64 = V37[2];
                  let V65 = apply_closure(V64)(V56);
                  make_closure(__mon_prompt,[V4, V63, V65])
                }"#]],
            expect![[r#"
                defn f_lam_16(V0, V2) {
                  let V3 = [];
                  let V4 = __mon_generate_marker(V3);
                  let V9 = make_closure(f_lam_0,[]);
                  let V13 = make_closure(f_lam_1,[]);
                  let V14 = [V9, V13];
                  let V15 = [V4, V14];
                  let V16 = V0[0];
                  let V1 = apply_closure(V16)(V2, V15);
                  let V29 = make_closure(f_lam_4,[V0]);
                  let V33 = make_closure(f_lam_7,[]);
                  let V34 = __mon_bind(V29, V33, V1);
                  let V35 = V34[0];
                  switch V35 <
                    branch 0 {
                      let V36 = V34[1];
                      typecast<[Int,Int,Int,Int]>([0, V36])
                    }
                    branch 1 {
                      let V37 = V34[1];
                      let V38 = V37[0];
                      let V39 = __mon_eqm(V4, V38);
                      let V40 = V39[0];
                      switch V40 <
                        branch 0 {
                          let V41 = V39[1];
                          let V42 = V37[0];
                          let V43 = V37[1];
                          let V54 = make_closure(f_lam_11,[V0, V4, V37]);
                          let V55 = [V42, V43, V54];
                          typecast<[Int,Int,Int,Int]>([1, V55])
                        }
                        branch 1 {
                          let V41 = V39[1];
                          let V66 = make_closure(f_lam_15,[V0, V4, V37]);
                          let V67 = V37[1];
                          apply_closure(V67)(V66, V2)
                        }
                      >
                    }
                  >
                }"#]],
            expect![[r#"
                defn f_lam_17(V72, V1) {
                  let V79 = [];
                  let V74 = apply_closure(V72)(V79);
                  typecast<[Int,Int,Int,Int]>([0, V74])
                }"#]],
            expect![[r#"
                defn f_lam_18(V76, V72) {
                  let V80 = make_closure(f_lam_17,[V72]);
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
