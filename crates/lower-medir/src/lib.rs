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
            let V53 = make_closure(main_lam_12,[V1]);
            let V58 = make_closure(main_lam_14,[]);
            let V59 = make_closure(main_lam_15,[]);
            let V60 = [];
            let V61 = __mon_prompt(V1, V53, V58, V59, V60);
            let V62 = V61[0];
            let V72 = switch V62 <
              branch 0 {
                let V35 = V61[1];
                let V63 = [];
                apply_closure(V35)(3429, V63)
              }
              branch 1 {
                let V37 = V61[1];
                let V38 = V37;
                let V64 = V38[0];
                let V65 = V38[1];
                let V70 = make_closure(main_lam_17,[V38]);
                let V71 = [V64, V65, V70];
                typecast<[Int,Int,Int,Int]>([1, V71])
              }
            >;
            let V73 = V72[0];
            let V89 = switch V73 <
              branch 0 {
                let V74 = V72[1];
                let V75 = V74[1];
                typecast<[Int,Int,Int,Int]>([0, V75])
              }
              branch 1 {
                let V76 = V72[1];
                let V77 = V76;
                let V78 = V77[0];
                let V79 = V77[1];
                let V87 = make_closure(main_lam_19,[V77]);
                let V88 = [V78, V79, V87];
                typecast<[Int,Int,Int,Int]>([1, V88])
              }
            >;
            let V90 = V89[0];
            switch V90 <
              branch 0 {
                let V2 = V89[1];
                V2
              }
              branch 1 {
                let V2 = V89[1];
                5467
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_0(V3, V20) {
            apply_closure(V20)(V3)
          }"#]],
      expect![[r#"
          defn main_lam_1(V3, V13, V16, V17) {
            let V18 = V13[2];
            let V19 = apply_closure(V18)(V16);
            let V21 = make_closure(main_lam_0,[V3]);
            __mon_bind(V19, V21, V17)
          }"#]],
      expect![[r#"
          defn main_lam_2(V3, V7, V8) {
            let V9 = apply_closure(V7)(V8);
            let V10 = V9[0];
            switch V10 <
              branch 0 {
                let V11 = V9[1];
                apply_closure(V11)(V3, V8)
              }
              branch 1 {
                let V12 = V9[1];
                let V13 = V12;
                let V14 = V13[0];
                let V15 = V13[1];
                let V22 = make_closure(main_lam_1,[V3, V13]);
                let V23 = [V14, V15, V22];
                typecast<[Int,Int,Int,Int]>([1, V23])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_3(V3, V4, V5) {
            let V6 = [];
            let V7 = apply_closure(V4)(V6);
            make_closure(main_lam_2,[V3, V7])
          }"#]],
      expect![[r#"
          defn main_lam_4(V3, V4, V2) {
            let V24 = make_closure(main_lam_3,[V3, V4]);
            typecast<[Int,Int,Int,Int]>([0, V24])
          }"#]],
      expect![[r#"
          defn main_lam_5(V3, V2) {
            let V25 = make_closure(main_lam_4,[V3]);
            typecast<[Int,Int,Int,Int]>([0, V25])
          }"#]],
      expect![[r#"
          defn main_lam_6(V45) {
            apply_closure(V45)(3429)
          }"#]],
      expect![[r#"
          defn main_lam_7(V38, V41, V42) {
            let V43 = V38[2];
            let V44 = apply_closure(V43)(V41);
            let V46 = make_closure(main_lam_6,[]);
            __mon_bind(V44, V46, V42)
          }"#]],
      expect![[r#"
          defn main_lam_8(V30, V31) {
            let V32 = [];
            let V33 = apply_closure(V30)(V32);
            let V34 = V33[0];
            switch V34 <
              branch 0 {
                let V35 = V33[1];
                let V36 = [];
                apply_closure(V35)(3429, V36)
              }
              branch 1 {
                let V37 = V33[1];
                let V38 = V37;
                let V39 = V38[0];
                let V40 = V38[1];
                let V47 = make_closure(main_lam_7,[V38]);
                let V48 = [V39, V40, V47];
                typecast<[Int,Int,Int,Int]>([1, V48])
              }
            >
          }"#]],
      expect![[r#"
          defn main_lam_9(V28, V29) {
            let V30 = apply_closure(V28)(3429);
            make_closure(main_lam_8,[V30])
          }"#]],
      expect![[r#"
          defn main_lam_10(V28, V2) {
            let V49 = make_closure(main_lam_9,[V28]);
            typecast<[Int,Int,Int,Int]>([0, V49])
          }"#]],
      expect![[r#"
          defn main_lam_11(V27, V2) {
            let V50 = make_closure(main_lam_10,[]);
            typecast<[Int,Int,Int,Int]>([0, V50])
          }"#]],
      expect![[r#"
          defn main_lam_12(V1, V2) {
            let V26 = make_closure(main_lam_5,[]);
            let V51 = make_closure(main_lam_11,[]);
            let V52 = [V26, V51];
            [V1, V52]
          }"#]],
      expect![[r#"
          defn main_lam_13(V54, V55, V2) {
            let V56 = [V55, V54];
            typecast<[Int,Int,Int,Int]>([0, V56])
          }"#]],
      expect![[r#"
          defn main_lam_14(V54, V2) {
            let V57 = make_closure(main_lam_13,[V54]);
            typecast<[Int,Int,Int,Int]>([0, V57])
          }"#]],
      expect![[r#"
          defn main_lam_15(V2) {
            typecast<[Int,Int,Int,Int]>([0, 3429])
          }"#]],
      expect![[r#"
          defn main_lam_16(V45) {
            apply_closure(V45)(3429)
          }"#]],
      expect![[r#"
          defn main_lam_17(V38, V41, V66) {
            let V67 = V38[2];
            let V68 = apply_closure(V67)(V41);
            let V69 = make_closure(main_lam_16,[]);
            __mon_bind(V68, V69, V66)
          }"#]],
      expect![[r#"
          defn main_lam_18(V84, V2) {
            let V85 = V84[1];
            typecast<[Int,Int,Int,Int]>([0, V85])
          }"#]],
      expect![[r#"
          defn main_lam_19(V77, V80, V81) {
            let V82 = V77[2];
            let V83 = apply_closure(V82)(V80);
            let V86 = make_closure(main_lam_18,[]);
            __mon_bind(V83, V86, V81)
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
