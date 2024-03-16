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
    let opt_item = self.simple_reducir_item_of(name);
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
    put : Int -> {},
    get : {} -> Int
}

effect Reader {
    ask : {} -> Int
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
    let main = format!("main = {}", input);
    lower_function(db, &main, "main")
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
} do State.get({}))(3429).value"#,
    );

    let expects = vec![
      expect![[r#"
          defn main() {
            let V0 = [];
            let V1 = __mon_generate_marker(V0);
            let V76 = make_closure(main_lam_14,[V1]);
            let V85 = make_closure(main_lam_16,[]);
            let V88 = make_closure(main_lam_17,[]);
            let V89 = [];
            let V90 = __mon_prompt(V1, V76, V85, V88, V89);
            let V91 = V90[0];
            let V104 = switch V91 <
              branch 0 {
                let V55 = V90[1];
                let V92 = [];
                apply_closure(V55: (Int,[]) -> [Int,Int,Int,Int])(3429, V92)
              }
              branch 1 {
                let V57 = V90[1];
                let V58 = V57;
                let V93 = V58[0];
                let V94 = V58[1];
                let V102 = make_closure(main_lam_19,[V58]);
                let V103 = [V93, V94, V102];
                typecast<[Int,Int,Int,Int]>([1, V103])
              }
            >;
            let V105 = V104[0];
            let V124 = switch V105 <
              branch 0 {
                let V106 = V104[1];
                let V107 = V106[1];
                typecast<[Int,Int,Int,Int]>([0, V107])
              }
              branch 1 {
                let V108 = V104[1];
                let V109 = V108;
                let V110 = V109[0];
                let V111 = V109[1];
                let V122 = make_closure(main_lam_21,[V109]);
                let V123 = [V110, V111, V122];
                typecast<[Int,Int,Int,Int]>([1, V123])
              }
            >;
            let V125 = V124[0];
            switch V125 <
              branch 0 {
                let V126 = V124[1];
                V126
              }
              branch 1 {
                let V126 = V124[1];
                5467
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_0(V12, V13) {
            let V8 = V12[0];
            let V14 = [];
            apply_closure(V8: ([],[]) -> [Int,Int,Int,Int])(V14, V13)
          }"#]],
      expect![[r#"
          defn main_lam_1(V30, V31, V32) {
            let V5 = V30[0];
            apply_closure(V31: (Int,[]) -> [Int,Int,Int,Int])(V5, V32)
          }"#]],
      expect![[r#"
          defn main_lam_2(V25, V26, V27) {
            let V5 = V25[0];
            let V22 = V25[1];
            let V28 = V22[2];
            let V29 = apply_closure(V28: (Int,[]) -> [Int,Int,Int,Int])(V26);
            let V33 = make_closure(main_lam_1,[V5]);
            __mon_bind(V29, V33, V27)
          }"#]],
      expect![[r#"
          defn main_lam_3(V16, V17) {
            let V5 = V16[0];
            let V15 = V16[1];
            let V18 = apply_closure(V15: ([]) -> [Int,Int,Int,Int])(V17);
            let V19 = V18[0];
            switch V19 <
              branch 0 {
                let V20 = V18[1];
                apply_closure(V20: (Int,[]) -> [Int,Int,Int,Int])(V5, V17)
              }
              branch 1 {
                let V21 = V18[1];
                let V22 = V21;
                let V23 = V22[0];
                let V24 = V22[1];
                let V34 = make_closure(main_lam_2,[V5, V22]);
                let V35 = [V23, V24, V34];
                typecast<[Int,Int,Int,Int]>([1, V35])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_4(V10, V11) {
            let V5 = V10[0];
            let V8 = V10[1];
            let V15 = make_closure(main_lam_0,[V8]);
            make_closure(main_lam_3,[V5, V15])
          }"#]],
      expect![[r#"
          defn main_lam_5(V7, V8, V9) {
            let V5 = V7[0];
            let V36 = make_closure(main_lam_4,[V5, V8]);
            typecast<[Int,Int,Int,Int]>([0, V36])
          }"#]],
      expect![[r#"
          defn main_lam_6(V4, V5, V6) {
            let V37 = make_closure(main_lam_5,[V5]);
            typecast<[Int,Int,Int,Int]>([0, V37])
          }"#]],
      expect![[r#"
          defn main_lam_7(V47, V48) {
            let V43 = V47[0];
            apply_closure(V43: (Int,[]) -> [Int,Int,Int,Int])(3429, V48)
          }"#]],
      expect![[r#"
          defn main_lam_8(V66, V67, V68) {
            apply_closure(V67: (Int,[]) -> [Int,Int,Int,Int])(3429, V68)
          }"#]],
      expect![[r#"
          defn main_lam_9(V61, V62, V63) {
            let V58 = V61[0];
            let V64 = V58[2];
            let V65 = apply_closure(V64: (Int,[]) -> [Int,Int,Int,Int])(V62);
            let V69 = make_closure(main_lam_8,[]);
            __mon_bind(V65, V69, V63)
          }"#]],
      expect![[r#"
          defn main_lam_10(V50, V51) {
            let V49 = V50[0];
            let V52 = [];
            let V53 = apply_closure(V49: ([]) -> [Int,Int,Int,Int])(V52);
            let V54 = V53[0];
            switch V54 <
              branch 0 {
                let V55 = V53[1];
                let V56 = [];
                apply_closure(V55: (Int,[]) -> [Int,Int,Int,Int])(3429, V56)
              }
              branch 1 {
                let V57 = V53[1];
                let V58 = V57;
                let V59 = V58[0];
                let V60 = V58[1];
                let V70 = make_closure(main_lam_9,[V58]);
                let V71 = [V59, V60, V70];
                typecast<[Int,Int,Int,Int]>([1, V71])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_11(V45, V46) {
            let V43 = V45[0];
            let V49 = make_closure(main_lam_7,[V43]);
            make_closure(main_lam_10,[V49])
          }"#]],
      expect![[r#"
          defn main_lam_12(V42, V43, V44) {
            let V72 = make_closure(main_lam_11,[V43]);
            typecast<[Int,Int,Int,Int]>([0, V72])
          }"#]],
      expect![[r#"
          defn main_lam_13(V39, V40, V41) {
            let V73 = make_closure(main_lam_12,[]);
            typecast<[Int,Int,Int,Int]>([0, V73])
          }"#]],
      expect![[r#"
          defn main_lam_14(V2, V3) {
            let V1 = V2[0];
            let V38 = make_closure(main_lam_6,[]);
            let V74 = make_closure(main_lam_13,[]);
            let V75 = [V38, V74];
            [V1, V75]
          }"#]],
      expect![[r#"
          defn main_lam_15(V80, V81, V82) {
            let V78 = V80[0];
            let V83 = [V81, V78];
            typecast<[Int,Int,Int,Int]>([0, V83])
          }"#]],
      expect![[r#"
          defn main_lam_16(V77, V78, V79) {
            let V84 = make_closure(main_lam_15,[V78]);
            typecast<[Int,Int,Int,Int]>([0, V84])
          }"#]],
      expect![[r#"
          defn main_lam_17(V86, V87) {
            typecast<[Int,Int,Int,Int]>([0, 3429])
          }"#]],
      expect![[r#"
          defn main_lam_18(V99, V67, V100) {
            apply_closure(V67: (Int,[]) -> [Int,Int,Int,Int])(3429, V100)
          }"#]],
      expect![[r#"
          defn main_lam_19(V95, V62, V96) {
            let V58 = V95[0];
            let V97 = V58[2];
            let V98 = apply_closure(V97: (Int,[]) -> [Int,Int,Int,Int])(V62);
            let V101 = make_closure(main_lam_18,[]);
            __mon_bind(V98, V101, V96)
          }"#]],
      expect![[r#"
            defn main_lam_20(V117, V118, V119) {
              let V120 = V118[1];
              typecast<[Int,Int,Int,Int]>([0, V120])
            }"#]],
      expect![[r#"
            defn main_lam_21(V112, V113, V114) {
              let V109 = V112[0];
              let V115 = V109[2];
              let V116 = apply_closure(V115: (Int,[]) -> [Int,Int,Int,Int])(V113);
              let V121 = make_closure(main_lam_20,[]);
              __mon_bind(V116, V121, V114)
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

  #[test]
  fn lower_medir_concat() {
    let db = TestDatabase::default();
    let items = lower_snippet(&db, "({ x = 3429 } ,, { y = 12 }).x");

    let expects = vec![expect![[r#"
            defn main() {
              3429
            }"#]]];

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
