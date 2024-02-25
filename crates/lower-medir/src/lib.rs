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
            let V10 = make_closure(main_lam_19,[V1]);
            let V10 = make_closure(main_lam_22,[V10]);
            let V85 = [];
            let V86 = apply_closure(V10: ([]) -> [Int,Int,Int,Int])(V85);
            let V87 = V86[0];
            let V103 = switch V87 <
              branch 0 {
                let V88 = V86[1];
                let V89 = V88[1];
                typecast<[Int,Int,Int,Int]>([0, V89])
              }
              branch 1 {
                let V90 = V86[1];
                let V91 = V90;
                let V92 = V91[0];
                let V93 = V91[1];
                let V101 = make_closure(main_lam_24,[V91]);
                let V102 = [V92, V93, V101];
                typecast<[Int,Int,Int,Int]>([1, V102])
              }
            >;
            let V104 = V103[0];
            switch V104 <
              branch 0 {
                let V5 = V103[1];
                V5
              }
              branch 1 {
                let V5 = V103[1];
                5467
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_0(V6, V23) {
            apply_closure(V23: (Int,[]) -> [Int,Int,Int,Int])(V6)
          }"#]],
      expect![[r#"
          defn main_lam_1(V6, V16, V19, V20) {
            let V21 = V16[2];
            let V22 = apply_closure(V21: (Int,[]) -> [Int,Int,Int,Int])(V19);
            let V24 = make_closure(main_lam_0,[V6]);
            __mon_bind(V22, V24, V20)
          }"#]],
      expect![[r#"
          defn main_lam_2(V6, V10, V11) {
            let V12 = apply_closure(V10: ([]) -> [Int,Int,Int,Int])(V11);
            let V13 = V12[0];
            switch V13 <
              branch 0 {
                let V14 = V12[1];
                apply_closure(V14: (Int,[]) -> [Int,Int,Int,Int])(V6, V11)
              }
              branch 1 {
                let V15 = V12[1];
                let V16 = V15;
                let V17 = V16[0];
                let V18 = V16[1];
                let V25 = make_closure(main_lam_1,[V6, V16]);
                let V26 = [V17, V18, V25];
                typecast<[Int,Int,Int,Int]>([1, V26])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_3(V6, V7, V8) {
            let V9 = [];
            let V10 = apply_closure(V7: ([],[]) -> [Int,Int,Int,Int])(V9);
            make_closure(main_lam_2,[V6, V10])
          }"#]],
      expect![[r#"
          defn main_lam_4(V6, V7, V5) {
            let V27 = make_closure(main_lam_3,[V6, V7]);
            typecast<[Int,Int,Int,Int]>([0, V27])
          }"#]],
      expect![[r#"
          defn main_lam_5(V6, V5) {
            let V28 = make_closure(main_lam_4,[V6]);
            typecast<[Int,Int,Int,Int]>([0, V28])
          }"#]],
      expect![[r#"
          defn main_lam_6(V3, V43) {
            apply_closure(V43: (Int,[]) -> [Int,Int,Int,Int])(V3)
          }"#]],
      expect![[r#"
          defn main_lam_7(V3, V36, V39, V40) {
            let V41 = V36[2];
            let V42 = apply_closure(V41: (Int,[]) -> [Int,Int,Int,Int])(V39);
            let V44 = make_closure(main_lam_6,[V3]);
            __mon_bind(V42, V44, V40)
          }"#]],
      expect![[r#"
          defn main_lam_8(V3, V10, V4) {
            let V32 = apply_closure(V10: ([]) -> [Int,Int,Int,Int])(V4);
            let V33 = V32[0];
            switch V33 <
              branch 0 {
                let V34 = V32[1];
                apply_closure(V34: (Int,[]) -> [Int,Int,Int,Int])(V3, V4)
              }
              branch 1 {
                let V35 = V32[1];
                let V36 = V35;
                let V37 = V36[0];
                let V38 = V36[1];
                let V45 = make_closure(main_lam_7,[V3, V36]);
                let V46 = [V37, V38, V45];
                typecast<[Int,Int,Int,Int]>([1, V46])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_9(V31, V3) {
            let V10 = apply_closure(V31: (Int,[]) -> [Int,Int,Int,Int])(V3);
            make_closure(main_lam_8,[V3, V10])
          }"#]],
      expect![[r#"
          defn main_lam_10(V31, V5) {
            let V47 = make_closure(main_lam_9,[V31]);
            typecast<[Int,Int,Int,Int]>([0, V47])
          }"#]],
      expect![[r#"
          defn main_lam_11(V30, V5) {
            let V48 = make_closure(main_lam_10,[]);
            typecast<[Int,Int,Int,Int]>([0, V48])
          }"#]],
      expect![[r#"
          defn main_lam_12(V1, V5) {
            let V29 = make_closure(main_lam_5,[]);
            let V49 = make_closure(main_lam_11,[]);
            let V50 = [V29, V49];
            [V1, V50]
          }"#]],
      expect![[r#"
          defn main_lam_13(V52, V53, V5) {
            let V54 = [V53, V52];
            typecast<[Int,Int,Int,Int]>([0, V54])
          }"#]],
      expect![[r#"
          defn main_lam_14(V52, V5) {
            let V55 = make_closure(main_lam_13,[V52]);
            typecast<[Int,Int,Int,Int]>([0, V55])
          }"#]],
      expect![[r#"
          defn main_lam_15(V3, V5) {
            typecast<[Int,Int,Int,Int]>([0, V3])
          }"#]],
      expect![[r#"
          defn main_lam_16(V3, V43) {
            apply_closure(V43: (Int,[]) -> [Int,Int,Int,Int])(V3)
          }"#]],
      expect![[r#"
          defn main_lam_17(V3, V36, V39, V62) {
            let V63 = V36[2];
            let V64 = apply_closure(V63: (Int,[]) -> [Int,Int,Int,Int])(V39);
            let V65 = make_closure(main_lam_16,[V3]);
            __mon_bind(V64, V65, V62)
          }"#]],
      expect![[r#"
          defn main_lam_18(V1, V3, V4) {
            let V51 = make_closure(main_lam_12,[V1]);
            let V56 = make_closure(main_lam_14,[]);
            let V57 = make_closure(main_lam_15,[V3]);
            let V58 = __mon_prompt(V1, V51, V56, V57, V4);
            let V59 = V58[0];
            switch V59 <
              branch 0 {
                let V34 = V58[1];
                apply_closure(V34: (Int,[]) -> [Int,Int,Int,Int])(V3, V4)
              }
              branch 1 {
                let V35 = V58[1];
                let V36 = V35;
                let V60 = V36[0];
                let V61 = V36[1];
                let V66 = make_closure(main_lam_17,[V3, V36]);
                let V67 = [V60, V61, V66];
                typecast<[Int,Int,Int,Int]>([1, V67])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_19(V1, V2) {
            let V68 = make_closure(main_lam_18,[V1]);
            typecast<[Int,Int,Int,Int]>([0, V68])
          }"#]],
      expect![[r#"
          defn main_lam_20(V81) {
            apply_closure(V81: (Int,[]) -> [Int,Int,Int,Int])(3429)
          }"#]],
      expect![[r#"
          defn main_lam_21(V74, V77, V78) {
            let V79 = V74[2];
            let V80 = apply_closure(V79: (Int,[]) -> [Int,Int,Int,Int])(V77);
            let V82 = make_closure(main_lam_20,[]);
            __mon_bind(V80, V82, V78)
          }"#]],
      expect![[r#"
          defn main_lam_22(V10, V69) {
            let V70 = apply_closure(V10: ([]) -> [Int,Int,Int,Int])(V69);
            let V71 = V70[0];
            switch V71 <
              branch 0 {
                let V72 = V70[1];
                apply_closure(V72: (Int,[]) -> [Int,Int,Int,Int])(3429, V69)
              }
              branch 1 {
                let V73 = V70[1];
                let V74 = V73;
                let V75 = V74[0];
                let V76 = V74[1];
                let V83 = make_closure(main_lam_21,[V74]);
                let V84 = [V75, V76, V83];
                typecast<[Int,Int,Int,Int]>([1, V84])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_23(V98, V5) {
            let V99 = V98[1];
            typecast<[Int,Int,Int,Int]>([0, V99])
          }"#]],
      expect![[r#"
          defn main_lam_24(V91, V94, V95) {
            let V96 = V91[2];
            let V97 = apply_closure(V96: (Int,[]) -> [Int,Int,Int,Int])(V94);
            let V100 = make_closure(main_lam_23,[]);
            __mon_bind(V97, V100, V95)
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
