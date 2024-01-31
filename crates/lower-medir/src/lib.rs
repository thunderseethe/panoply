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
            let V8 = make_closure(f_lam_0,[]);
            let V12 = make_closure(f_lam_1,[]);
            let V13 = [V8, V12];
            let V14 = [V3, V13];
            let V15 = V0[0];
            let V1 = apply_closure(V15)(V1, V14);
            let V16 = V0[3];
            let V17 = V16[0];
            let V18 = apply_closure(V17)(V1);
            let V19 = V18[0];
            let V20 = __mon_eqm(V3, V19);
            let V21 = V20[0];
            let V51 = switch V21 <
              branch 0 {
                let V22 = V20[1];
                let V23 = V18[0];
                let V28 = make_closure(f_lam_2,[V18]);
                let V36 = make_closure(f_lam_6,[]);
                let V37 = [V23, V28, V36];
                typecast<[Int,Int,Int,Int]>([1, V37])
              }
              branch 1 {
                let V22 = V20[1];
                let V38 = V18[0];
                let V42 = make_closure(f_lam_7,[V18]);
                let V47 = make_closure(f_lam_11,[]);
                let V48 = typecast<[Int,((Int,Int) -> [Int,Int,Int,Int],Int) -> [Int,Int,Int,Int],(Int,Int) -> [Int,Int,Int,Int]]>([V38, V42, V47]);
                let V49 = V48[2];
                let V50 = V48[1];
                apply_closure(V50)(V49, V1)
              }
            >;
            let V52 = V51[0];
            switch V52 <
              branch 0 {
                let V53 = V51[1];
                let V54 = [];
                let V55 = apply_closure(V53)(V54);
                typecast<[Int,Int,Int,Int]>([0, V55])
              }
              branch 1 {
                let V56 = V51[1];
                let V57 = V56;
                let V58 = V57[0];
                let V59 = V57[1];
                let V67 = make_closure(f_lam_14,[V57]);
                let V68 = [V58, V59, V67];
                typecast<[Int,Int,Int,Int]>([1, V68])
              }
            >
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
          defn f_lam_2(V18, V24) {
            let V25 = [];
            let V26 = V18[1];
            let V27 = V26[1];
            apply_closure(V27)(V25, V24)
          }"#]],
      expect![[r#"
          defn f_lam_3(V29, V1) {
            typecast<[Int,Int,Int,Int]>([0, V29])
          }"#]],
      expect![[r#"
          defn f_lam_4(V32, V33) {
            [V33, V32]
          }"#]],
      expect![[r#"
          defn f_lam_5(V32, V1) {
            let V34 = make_closure(f_lam_4,[V32]);
            typecast<[Int,Int,Int,Int]>([0, V34])
          }"#]],
      expect![[r#"
          defn f_lam_6(V29, V30) {
            let V31 = make_closure(f_lam_3,[V29]);
            let V35 = make_closure(f_lam_5,[]);
            __mon_bind(V31, V35, V30)
          }"#]],
      expect![[r#"
          defn f_lam_7(V18, V24) {
            let V39 = [];
            let V40 = V18[1];
            let V41 = V40[1];
            apply_closure(V41)(V39, V24)
          }"#]],
      expect![[r#"
          defn f_lam_8(V29, V1) {
            typecast<[Int,Int,Int,Int]>([0, V29])
          }"#]],
      expect![[r#"
          defn f_lam_9(V32, V33) {
            [V33, V32]
          }"#]],
      expect![[r#"
          defn f_lam_10(V32, V1) {
            let V45 = make_closure(f_lam_9,[V32]);
            typecast<[Int,Int,Int,Int]>([0, V45])
          }"#]],
      expect![[r#"
          defn f_lam_11(V29, V43) {
            let V44 = make_closure(f_lam_8,[V29]);
            let V46 = make_closure(f_lam_10,[]);
            __mon_bind(V44, V46, V43)
          }"#]],
      expect![[r#"
          defn f_lam_12(V65, V1) {
            typecast<[Int,Int,Int,Int]>([0, V65])
          }"#]],
      expect![[r#"
          defn f_lam_13(V63) {
            let V64 = [];
            let V65 = apply_closure(V63)(V64);
            make_closure(f_lam_12,[V65])
          }"#]],
      expect![[r#"
          defn f_lam_14(V57, V29, V60) {
            let V61 = V57[2];
            let V62 = apply_closure(V61)(V29);
            let V66 = make_closure(f_lam_13,[]);
            __mon_bind(V62, V66, V60)
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
