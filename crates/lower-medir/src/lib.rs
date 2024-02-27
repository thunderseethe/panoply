use base::{
  id::{IdSupply, TermName},
  id_converter::IdConverter,
  ident::Ident,
  modules::Module,
  pretty::{PrettyPrint, PrettyWithCtx},
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
  for defn in defns.iter() {
    println!(
      "{}",
      defn
        .item(medir_db)
        .pretty_with(medir_db)
        .pprint()
        .pretty(80)
    );
  }
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
            let V77 = make_closure(main_lam_19,[V1]);
            let V94 = make_closure(main_lam_22,[V77]);
            let V95 = [];
            let V96 = apply_closure(V94: ([]) -> [Int,Int,Int,Int])(V95);
            let V97 = V96[0];
            let V114 = switch V97 <
              branch 0 {
                let V98 = V96[1];
                let V99 = V98[1];
                typecast<[Int,Int,Int,Int]>([0, V99])
              }
              branch 1 {
                let V100 = V96[1];
                let V101 = V100;
                let V102 = V101[0];
                let V103 = V101[1];
                let V112 = make_closure(main_lam_24,[V101]);
                let V113 = [V102, V103, V112];
                typecast<[Int,Int,Int,Int]>([1, V113])
              }
            >;
            let V115 = V114[0];
            switch V115 <
              branch 0 {
                let V116 = V114[1];
                V116
              }
              branch 1 {
                let V116 = V114[1];
                5467
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_0(V6, V25) {
            apply_closure(V25: (Int,[]) -> [Int,Int,Int,Int])(V6)
          }"#]],
      expect![[r#"
          defn main_lam_1(V6, V18, V21, V22) {
            let V23 = V18[2];
            let V24 = apply_closure(V23: (Int,[]) -> [Int,Int,Int,Int])(V21);
            let V26 = make_closure(main_lam_0,[V6]);
            __mon_bind(V24, V26, V22)
          }"#]],
      expect![[r#"
          defn main_lam_2(V6, V12, V13) {
            let V14 = apply_closure(V12: ([]) -> [Int,Int,Int,Int])(V13);
            let V15 = V14[0];
            switch V15 <
              branch 0 {
                let V16 = V14[1];
                apply_closure(V16: (Int,[]) -> [Int,Int,Int,Int])(V6, V13)
              }
              branch 1 {
                let V17 = V14[1];
                let V18 = V17;
                let V19 = V18[0];
                let V20 = V18[1];
                let V27 = make_closure(main_lam_1,[V6, V18]);
                let V28 = [V19, V20, V27];
                typecast<[Int,Int,Int,Int]>([1, V28])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_3(V6, V8, V10) {
            let V11 = [];
            let V12 = apply_closure(V8: ([],[]) -> [Int,Int,Int,Int])(V11);
            make_closure(main_lam_2,[V6, V12])
          }"#]],
      expect![[r#"
          defn main_lam_4(V6, V8, V9) {
            let V29 = make_closure(main_lam_3,[V6, V8]);
            typecast<[Int,Int,Int,Int]>([0, V29])
          }"#]],
      expect![[r#"
          defn main_lam_5(V6, V7) {
            let V30 = make_closure(main_lam_4,[V6]);
            typecast<[Int,Int,Int,Int]>([0, V30])
          }"#]],
      expect![[r#"
          defn main_lam_6(V3, V48) {
            apply_closure(V48: (Int,[]) -> [Int,Int,Int,Int])(V3)
          }"#]],
      expect![[r#"
          defn main_lam_7(V3, V41, V44, V45) {
            let V46 = V41[2];
            let V47 = apply_closure(V46: (Int,[]) -> [Int,Int,Int,Int])(V44);
            let V49 = make_closure(main_lam_6,[V3]);
            __mon_bind(V47, V49, V45)
          }"#]],
      expect![[r#"
          defn main_lam_8(V3, V36, V4) {
            let V37 = apply_closure(V36: ([]) -> [Int,Int,Int,Int])(V4);
            let V38 = V37[0];
            switch V38 <
              branch 0 {
                let V39 = V37[1];
                apply_closure(V39: (Int,[]) -> [Int,Int,Int,Int])(V3, V4)
              }
              branch 1 {
                let V40 = V37[1];
                let V41 = V40;
                let V42 = V41[0];
                let V43 = V41[1];
                let V50 = make_closure(main_lam_7,[V3, V41]);
                let V51 = [V42, V43, V50];
                typecast<[Int,Int,Int,Int]>([1, V51])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_9(V34, V3) {
            let V36 = apply_closure(V34: (Int,[]) -> [Int,Int,Int,Int])(V3);
            make_closure(main_lam_8,[V3, V36])
          }"#]],
      expect![[r#"
          defn main_lam_10(V34, V35) {
            let V52 = make_closure(main_lam_9,[V34]);
            typecast<[Int,Int,Int,Int]>([0, V52])
          }"#]],
      expect![[r#"
          defn main_lam_11(V32, V33) {
            let V53 = make_closure(main_lam_10,[]);
            typecast<[Int,Int,Int,Int]>([0, V53])
          }"#]],
      expect![[r#"
          defn main_lam_12(V1, V5) {
            let V31 = make_closure(main_lam_5,[]);
            let V54 = make_closure(main_lam_11,[]);
            let V55 = [V31, V54];
            [V1, V55]
          }"#]],
      expect![[r#"
          defn main_lam_13(V57, V59, V60) {
            let V61 = [V59, V57];
            typecast<[Int,Int,Int,Int]>([0, V61])
          }"#]],
      expect![[r#"
          defn main_lam_14(V57, V58) {
            let V62 = make_closure(main_lam_13,[V57]);
            typecast<[Int,Int,Int,Int]>([0, V62])
          }"#]],
      expect![[r#"
          defn main_lam_15(V3, V64) {
            typecast<[Int,Int,Int,Int]>([0, V3])
          }"#]],
      expect![[r#"
          defn main_lam_16(V3, V48) {
            apply_closure(V48: (Int,[]) -> [Int,Int,Int,Int])(V3)
          }"#]],
      expect![[r#"
          defn main_lam_17(V3, V41, V44, V70) {
            let V71 = V41[2];
            let V72 = apply_closure(V71: (Int,[]) -> [Int,Int,Int,Int])(V44);
            let V73 = make_closure(main_lam_16,[V3]);
            __mon_bind(V72, V73, V70)
          }"#]],
      expect![[r#"
          defn main_lam_18(V1, V3, V4) {
            let V56 = make_closure(main_lam_12,[V1]);
            let V63 = make_closure(main_lam_14,[]);
            let V65 = make_closure(main_lam_15,[V3]);
            let V66 = __mon_prompt(V1, V56, V63, V65, V4);
            let V67 = V66[0];
            switch V67 <
              branch 0 {
                let V39 = V66[1];
                apply_closure(V39: (Int,[]) -> [Int,Int,Int,Int])(V3, V4)
              }
              branch 1 {
                let V40 = V66[1];
                let V41 = V40;
                let V68 = V41[0];
                let V69 = V41[1];
                let V74 = make_closure(main_lam_17,[V3, V41]);
                let V75 = [V68, V69, V74];
                typecast<[Int,Int,Int,Int]>([1, V75])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_19(V1, V2) {
            let V76 = make_closure(main_lam_18,[V1]);
            typecast<[Int,Int,Int,Int]>([0, V76])
          }"#]],
      expect![[r#"
          defn main_lam_20(V90) {
            apply_closure(V90: (Int,[]) -> [Int,Int,Int,Int])(3429)
          }"#]],
      expect![[r#"
          defn main_lam_21(V83, V86, V87) {
            let V88 = V83[2];
            let V89 = apply_closure(V88: (Int,[]) -> [Int,Int,Int,Int])(V86);
            let V91 = make_closure(main_lam_20,[]);
            __mon_bind(V89, V91, V87)
          }"#]],
      expect![[r#"
          defn main_lam_22(V77, V78) {
            let V79 = apply_closure(V77: ([]) -> [Int,Int,Int,Int])(V78);
            let V80 = V79[0];
            switch V80 <
              branch 0 {
                let V81 = V79[1];
                apply_closure(V81: (Int,[]) -> [Int,Int,Int,Int])(3429, V78)
              }
              branch 1 {
                let V82 = V79[1];
                let V83 = V82;
                let V84 = V83[0];
                let V85 = V83[1];
                let V92 = make_closure(main_lam_21,[V83]);
                let V93 = [V84, V85, V92];
                typecast<[Int,Int,Int,Int]>([1, V93])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_23(V108, V109) {
            let V110 = V108[1];
            typecast<[Int,Int,Int,Int]>([0, V110])
          }"#]],
      expect![[r#"
          defn main_lam_24(V101, V104, V105) {
            let V106 = V101[2];
            let V107 = apply_closure(V106: (Int,[]) -> [Int,Int,Int,Int])(V104);
            let V111 = make_closure(main_lam_23,[]);
            __mon_bind(V107, V111, V105)
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
