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

  fn lower_medir_module_for_file_name(
    &self,
    path: std::path::PathBuf,
  ) -> MedIrModule {
    let module = self.root_module_for_path(path);
    self.lower_medir_module_of(module)
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
  use medir::{MedIr, MedIrModule};
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

  fn lower_function(db: &TestDatabase, input: &str, fn_name: &str) -> MedIrModule {
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

    db.lower_medir_module_for_file_name(path)
  }

  fn lower_snippet(db: &TestDatabase, input: &str) -> MedIrModule {
    let main = format!("main = {}", input);
    lower_function(db, &main, "main")
  }

  #[test]
  fn lower_medir_state_get() {
    let db = TestDatabase::default();
    let module = lower_snippet(
      &db,
      r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(3429).value"#,
    );

    let expect = expect![[r#"
        defn main() {
          let V0 = [];
          let V1 = __mon_generate_marker(V0);
          let V86 = make_closure(main_lam_20,[V1]);
          let V91 = make_closure(main_lam_21,[]);
          let V92 = [];
          let V93 = __mon_bind(V86, V91, V92);
          let V94 = V93[0];
          switch V94 <
            branch 0 {
              let V95 = V93[1];
              V95
            }
            branch 1 {
              let V95 = V93[1];
              5467
            }
          >
        }
        defn main_lam_0(V17, V18) {
          let V12 = V17[0];
          let V19 = [];
          apply_closure(V12: ([],[]) -> [Int,Int,Int,Int])(V19, V18)
        }
        defn main_lam_1(V21, V22, V23) {
          let V9 = V21[0];
          apply_closure(V22: (Int,[]) -> [Int,Int,Int,Int])(V9, V23)
        }
        defn main_lam_2(V14, V15, V16) {
          let V9 = V14[0];
          let V12 = V14[1];
          let V20 = make_closure(main_lam_0,[V12]);
          let V24 = make_closure(main_lam_1,[V9]);
          __mon_bind(V20, V24, V16)
        }
        defn main_lam_3(V11, V12, V13) {
          let V9 = V11[0];
          let V25 = make_closure(main_lam_2,[V9, V12]);
          typecast<[Int,Int,Int,Int]>([0, V25])
        }
        defn main_lam_4(V8, V9, V10) {
          let V26 = make_closure(main_lam_3,[V9]);
          typecast<[Int,Int,Int,Int]>([0, V26])
        }
        defn main_lam_5(V37, V38) {
          let V32 = V37[0];
          let V35 = V37[1];
          apply_closure(V32: (Int,[]) -> [Int,Int,Int,Int])(V35, V38)
        }
        defn main_lam_6(V40, V41, V42) {
          let V35 = V40[0];
          apply_closure(V41: (Int,[]) -> [Int,Int,Int,Int])(V35, V42)
        }
        defn main_lam_7(V34, V35, V36) {
          let V32 = V34[0];
          let V39 = make_closure(main_lam_5,[V32, V35]);
          let V43 = make_closure(main_lam_6,[V35]);
          __mon_bind(V39, V43, V36)
        }
        defn main_lam_8(V31, V32, V33) {
          let V44 = make_closure(main_lam_7,[V32]);
          typecast<[Int,Int,Int,Int]>([0, V44])
        }
        defn main_lam_9(V28, V29, V30) {
          let V45 = make_closure(main_lam_8,[]);
          typecast<[Int,Int,Int,Int]>([0, V45])
        }
        defn main_lam_10(V6, V7) {
          let V1 = V6[0];
          let V27 = make_closure(main_lam_4,[]);
          let V46 = make_closure(main_lam_9,[]);
          let V47 = [V27, V46];
          [V1, V47]
        }
        defn main_lam_11(V52, V53, V54) {
          let V50 = V52[0];
          let V55 = [V53, V50];
          typecast<[Int,Int,Int,Int]>([0, V55])
        }
        defn main_lam_12(V49, V50, V51) {
          let V56 = make_closure(main_lam_11,[V50]);
          typecast<[Int,Int,Int,Int]>([0, V56])
        }
        defn main_lam_13(V64, V65) {
          let V59 = V64[0];
          let V66 = [];
          let V67 = V59[1];
          let V68 = V67[1];
          apply_closure(V68: ([],[]) -> [Int,Int,Int,Int])(V66, V65)
        }
        defn main_lam_14(V70, V71, V72) {
          let V62 = V70[0];
          apply_closure(V71: ((Int,[]) -> [Int,Int,Int,Int],[]) -> [Int,Int,Int,Int])(V62, V72)
        }
        defn main_lam_15(V61, V62, V63) {
          let V59 = V61[0];
          let V69 = make_closure(main_lam_13,[V59]);
          let V73 = make_closure(main_lam_14,[V62]);
          __mon_bind(V69, V73, V63)
        }
        defn main_lam_16(V75, V76, V77) {
          typecast<[Int,Int,Int,Int]>([0, V76])
        }
        defn main_lam_17(V58, V59) {
          let V60 = V59[0];
          let V74 = make_closure(main_lam_15,[V59]);
          let V78 = make_closure(main_lam_16,[]);
          let V79 = [V60, V74, V78];
          typecast<[Int,Int,Int,Int]>([1, V79])
        }
        defn main_lam_18(V4, V5) {
          let V1 = V4[0];
          let V48 = make_closure(main_lam_10,[V1]);
          let V57 = make_closure(main_lam_12,[]);
          let V80 = make_closure(main_lam_17,[]);
          __mon_prompt(V1, V48, V57, V80, V5)
        }
        defn main_lam_19(V82, V83, V84) {
          apply_closure(V83: (Int,[]) -> [Int,Int,Int,Int])(3429, V84)
        }
        defn main_lam_20(V2, V3) {
          let V1 = V2[0];
          let V81 = make_closure(main_lam_18,[V1]);
          let V85 = make_closure(main_lam_19,[]);
          __mon_bind(V81, V85, V3)
        }
        defn main_lam_21(V87, V88, V89) {
          let V90 = V88[1];
          typecast<[Int,Int,Int,Int]>([0, V90])
        }
        defn __mon_bind(V0, V1, V2) {
          let V3 = apply_closure(V0: (Int) -> [Int,Int,Int,Int])(V2);
          let V4 = V3[0];
          switch V4 <
            branch 0 {
              let V5 = V3[1];
              apply_closure(V1: (Int,Int) -> [Int,Int,Int,Int])(V5, V2)
            }
            branch 1 {
              let V6 = V3[1];
              let V7 = V6;
              let V8 = V7[0];
              let V9 = V7[1];
              let V17 = make_closure(__mon_bind_lam_1,[V1, V7]);
              let V18 = [V8, V9, V17];
              typecast<[Int,Int,Int,Int]>([1, V18])
            }
          >
        }
        defn __mon_bind_lam_0(V13, V14) {
          let V7 = V13[0];
          let V11 = V13[1];
          let V15 = V7[2];
          apply_closure(V15: (Int,Int) -> [Int,Int,Int,Int])(V11, V14)
        }
        defn __mon_bind_lam_1(V10, V11, V12) {
          let V1 = V10[0];
          let V7 = V10[1];
          let V16 = make_closure(__mon_bind_lam_0,[V7, V11]);
          __mon_bind(V16, V1, V12)
        }
        defn __mon_prompt(V0, V1, V2, V3, V4) {
          let V5 = apply_closure(V1: (Int) -> Int)(V4);
          let V6 = apply_closure(V3: (Int) -> [Int,Int,Int,Int])(V5);
          let V7 = V6[0];
          switch V7 <
            branch 0 {
              let V8 = V6[1];
              apply_closure(V2: (Int,Int) -> [Int,Int,Int,Int])(V8, V4)
            }
            branch 1 {
              let V9 = V6[1];
              let V10 = V9[0];
              let V11 = __mon_eqm(V0, V10);
              let V12 = V11[0];
              switch V12 <
                branch 0 {
                  let V13 = V11[1];
                  let V14 = V9;
                  let V15 = V14[0];
                  let V16 = V14[1];
                  let V24 = make_closure(__mon_prompt_lam_1,[V0, V1, V2, V14]);
                  let V25 = [V15, V16, V24];
                  typecast<[Int,Int,Int,Int]>([1, V25])
                }
                branch 1 {
                  let V13 = V11[1];
                  let V33 = make_closure(__mon_prompt_lam_3,[V0, V1, V2, V9]);
                  let V34 = V9[1];
                  apply_closure(V34: ((Int,Int) -> [Int,Int,Int,Int],Int) -> [Int,Int,Int,Int])(V33, V4)
                }
              >
            }
          >
        }
        defn __mon_prompt_lam_0(V20, V21) {
          let V14 = V20[0];
          let V18 = V20[1];
          let V22 = V14[2];
          apply_closure(V22: (Int,Int) -> [Int,Int,Int,Int])(V18, V21)
        }
        defn __mon_prompt_lam_1(V17, V18, V19) {
          let V0 = V17[0];
          let V1 = V17[1];
          let V2 = V17[2];
          let V14 = V17[3];
          let V23 = make_closure(__mon_prompt_lam_0,[V14, V18]);
          __mon_prompt(V0, V1, V2, V23, V19)
        }
        defn __mon_prompt_lam_2(V29, V30) {
          let V9 = V29[0];
          let V27 = V29[1];
          let V31 = V9[2];
          apply_closure(V31: (Int,Int) -> [Int,Int,Int,Int])(V27, V30)
        }
        defn __mon_prompt_lam_3(V26, V27, V28) {
          let V0 = V26[0];
          let V1 = V26[1];
          let V2 = V26[2];
          let V9 = V26[3];
          let V32 = make_closure(__mon_prompt_lam_2,[V9, V27]);
          __mon_prompt(V0, V1, V2, V32, V28)
        }"#]];

    expect.assert_eq(module.pretty_string(&db, 80).as_str());
  }

  #[test]
  fn lower_medir_concat() {
    let db = TestDatabase::default();
    let module = lower_snippet(&db, "({ x = 3429 } ,, { y = 12 }).x");

    let expect = expect![[r#"
        defn main() {
          3429
        }
        defn __mon_bind(V0, V1, V2) {
          let V3 = apply_closure(V0: (Int) -> [Int,Int,Int,Int])(V2);
          let V4 = V3[0];
          switch V4 <
            branch 0 {
              let V5 = V3[1];
              apply_closure(V1: (Int,Int) -> [Int,Int,Int,Int])(V5, V2)
            }
            branch 1 {
              let V6 = V3[1];
              let V7 = V6;
              let V8 = V7[0];
              let V9 = V7[1];
              let V17 = make_closure(__mon_bind_lam_1,[V1, V7]);
              let V18 = [V8, V9, V17];
              typecast<[Int,Int,Int,Int]>([1, V18])
            }
          >
        }
        defn __mon_bind_lam_0(V13, V14) {
          let V7 = V13[0];
          let V11 = V13[1];
          let V15 = V7[2];
          apply_closure(V15: (Int,Int) -> [Int,Int,Int,Int])(V11, V14)
        }
        defn __mon_bind_lam_1(V10, V11, V12) {
          let V1 = V10[0];
          let V7 = V10[1];
          let V16 = make_closure(__mon_bind_lam_0,[V7, V11]);
          __mon_bind(V16, V1, V12)
        }
        defn __mon_prompt(V0, V1, V2, V3, V4) {
          let V5 = apply_closure(V1: (Int) -> Int)(V4);
          let V6 = apply_closure(V3: (Int) -> [Int,Int,Int,Int])(V5);
          let V7 = V6[0];
          switch V7 <
            branch 0 {
              let V8 = V6[1];
              apply_closure(V2: (Int,Int) -> [Int,Int,Int,Int])(V8, V4)
            }
            branch 1 {
              let V9 = V6[1];
              let V10 = V9[0];
              let V11 = __mon_eqm(V0, V10);
              let V12 = V11[0];
              switch V12 <
                branch 0 {
                  let V13 = V11[1];
                  let V14 = V9;
                  let V15 = V14[0];
                  let V16 = V14[1];
                  let V24 = make_closure(__mon_prompt_lam_1,[V0, V1, V2, V14]);
                  let V25 = [V15, V16, V24];
                  typecast<[Int,Int,Int,Int]>([1, V25])
                }
                branch 1 {
                  let V13 = V11[1];
                  let V33 = make_closure(__mon_prompt_lam_3,[V0, V1, V2, V9]);
                  let V34 = V9[1];
                  apply_closure(V34: ((Int,Int) -> [Int,Int,Int,Int],Int) -> [Int,Int,Int,Int])(V33, V4)
                }
              >
            }
          >
        }
        defn __mon_prompt_lam_0(V20, V21) {
          let V14 = V20[0];
          let V18 = V20[1];
          let V22 = V14[2];
          apply_closure(V22: (Int,Int) -> [Int,Int,Int,Int])(V18, V21)
        }
        defn __mon_prompt_lam_1(V17, V18, V19) {
          let V0 = V17[0];
          let V1 = V17[1];
          let V2 = V17[2];
          let V14 = V17[3];
          let V23 = make_closure(__mon_prompt_lam_0,[V14, V18]);
          __mon_prompt(V0, V1, V2, V23, V19)
        }
        defn __mon_prompt_lam_2(V29, V30) {
          let V9 = V29[0];
          let V27 = V29[1];
          let V31 = V9[2];
          apply_closure(V31: (Int,Int) -> [Int,Int,Int,Int])(V27, V30)
        }
        defn __mon_prompt_lam_3(V26, V27, V28) {
          let V0 = V26[0];
          let V1 = V26[1];
          let V2 = V26[2];
          let V9 = V26[3];
          let V32 = make_closure(__mon_prompt_lam_2,[V9, V27]);
          __mon_prompt(V0, V1, V2, V32, V28)
        }"#]];

    expect.assert_eq(module.pretty_string(&db, 80).as_str());
  }
}
