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

  fn lower_medir_module_for_file_name(&self, path: std::path::PathBuf) -> MedIrModule {
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
    pretty::PrettyWithCtx,
  };
  use expect_test::expect;
  use medir::MedIrModule;
  use parser::Db as ParserDb;

  use crate::Db as LowerMedirDb;

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

  fn lower_function(db: &TestDatabase, input: &str) -> MedIrModule {
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

    let module = db.lower_medir_module_for_file_name(path);
    let errors = db.all_parse_errors();
    dbg!(errors);
    module
  }

  fn lower_snippet(db: &TestDatabase, input: &str) -> MedIrModule {
    let main = format!("defn main = {}", input);
    lower_function(db, &main)
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
} do get({}))(3429).value"#,
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

  #[test]
  fn lower_medir_effect_item_across_item_boundary() {
    let db = TestDatabase::default();
    let medir_module = lower_function(
      &db,
      r#"
defn foo = |env| with {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { value = x, state = s },
} do (with {
  ask = |x| |k| k(env),
  return = |x| x,
} do let w = put(ask({})); get({}))

defn main = foo(16777215)(14).state"#,
    );

    let expect = expect![[r#"
        defn foo(V0, V1, V2, V3) {
          let V172 = make_closure(foo_lam_37,[V0, V1, V2, V4]);
          typecast<[Int,Int,Int,Int]>([0, V172])
        }
        defn foo_lam_0(V22, V23) {
          let V17 = V22[0];
          let V24 = [];
          apply_closure(V17: ([],Int) -> [Int,Int,Int,Int])(V24, V23)
        }
        defn foo_lam_1(V26, V27, V28) {
          let V14 = V26[0];
          apply_closure(V27: (Int,Int) -> [Int,Int,Int,Int])(V14, V28)
        }
        defn foo_lam_2(V19, V20, V21) {
          let V14 = V19[0];
          let V17 = V19[1];
          let V25 = make_closure(foo_lam_0,[V17]);
          let V29 = make_closure(foo_lam_1,[V14]);
          __mon_bind(V25, V29, V21)
        }
        defn foo_lam_3(V16, V17, V18) {
          let V14 = V16[0];
          let V30 = make_closure(foo_lam_2,[V14, V17]);
          typecast<[Int,Int,Int,Int]>([0, V30])
        }
        defn foo_lam_4(V13, V14, V15) {
          let V31 = make_closure(foo_lam_3,[V14]);
          typecast<[Int,Int,Int,Int]>([0, V31])
        }
        defn foo_lam_5(V42, V43) {
          let V37 = V42[0];
          let V40 = V42[1];
          apply_closure(V37: (Int,Int) -> [Int,Int,Int,Int])(V40, V43)
        }
        defn foo_lam_6(V45, V46, V47) {
          let V40 = V45[0];
          apply_closure(V46: (Int,Int) -> [Int,Int,Int,Int])(V40, V47)
        }
        defn foo_lam_7(V39, V40, V41) {
          let V37 = V39[0];
          let V44 = make_closure(foo_lam_5,[V37, V40]);
          let V48 = make_closure(foo_lam_6,[V40]);
          __mon_bind(V44, V48, V41)
        }
        defn foo_lam_8(V36, V37, V38) {
          let V49 = make_closure(foo_lam_7,[V37]);
          typecast<[Int,Int,Int,Int]>([0, V49])
        }
        defn foo_lam_9(V33, V34, V35) {
          let V50 = make_closure(foo_lam_8,[]);
          typecast<[Int,Int,Int,Int]>([0, V50])
        }
        defn foo_lam_10(V11, V12) {
          let V0 = V11[0];
          let V8 = V11[1];
          let V32 = make_closure(foo_lam_4,[]);
          let V51 = make_closure(foo_lam_9,[]);
          let V52 = [V32, V51];
          let V53 = [V8, V52];
          let V54 = V0[0];
          apply_closure(V54: (Int,[Int,[(Int,Int) -> [Int,Int,Int,Int],([],Int) -> [Int,Int,Int,Int]]]) -> Int)(V12, V53)
        }
        defn foo_lam_11(V59, V60, V61) {
          let V57 = V59[0];
          let V62 = [V57, V60];
          typecast<[Int,Int,Int,Int]>([0, V62])
        }
        defn foo_lam_12(V56, V57, V58) {
          let V63 = make_closure(foo_lam_11,[V57]);
          typecast<[Int,Int,Int,Int]>([0, V63])
        }
        defn foo_lam_13(V74, V75, V76) {
          let V6 = V74[0];
          apply_closure(V75: (Int,Int) -> [Int,Int,Int,Int])(V6, V76)
        }
        defn foo_lam_14(V71, V72, V73) {
          let V6 = V71[0];
          let V77 = make_closure(foo_lam_13,[V6]);
          typecast<[Int,Int,Int,Int]>([0, V77])
        }
        defn foo_lam_15(V69, V70) {
          let V1 = V69[0];
          let V6 = V69[1];
          let V68 = V69[2];
          let V78 = make_closure(foo_lam_14,[V6]);
          let V79 = [V68, V78];
          let V80 = V1[0];
          apply_closure(V80: (Int,[Int,([],Int) -> [Int,Int,Int,Int]]) -> Int)(V70, V79)
        }
        defn foo_lam_16(V82, V83, V84) {
          typecast<[Int,Int,Int,Int]>([0, V83])
        }
        defn foo_lam_17(V102, V103) {
          let V93 = V102[0];
          let V104 = [];
          let V105 = V93[1];
          apply_closure(V105: ([],Int) -> [Int,Int,Int,Int])(V104, V103)
        }
        defn foo_lam_18(V107, V108, V109) {
          let V100 = V107[0];
          apply_closure(V108: ((Int,Int) -> [Int,Int,Int,Int],Int) -> [Int,Int,Int,Int])(V100, V109)
        }
        defn foo_lam_19(V99, V100, V101) {
          let V93 = V99[0];
          let V106 = make_closure(foo_lam_17,[V93]);
          let V110 = make_closure(foo_lam_18,[V100]);
          __mon_bind(V106, V110, V101)
        }
        defn foo_lam_20(V112, V113, V114) {
          typecast<[Int,Int,Int,Int]>([0, V113])
        }
        defn foo_lam_21(V96, V97) {
          let V93 = V96[0];
          let V98 = V93[0];
          let V111 = make_closure(foo_lam_19,[V93]);
          let V115 = make_closure(foo_lam_20,[]);
          let V116 = [V98, V111, V115];
          typecast<[Int,Int,Int,Int]>([1, V116])
        }
        defn foo_lam_22(V125, V126) {
          let V4 = V125[0];
          let V90 = V125[1];
          let V127 = V90[1];
          let V128 = V127[0];
          apply_closure(V128: (Int,Int) -> [Int,Int,Int,Int])(V4, V126)
        }
        defn foo_lam_23(V130, V131, V132) {
          let V123 = V130[0];
          apply_closure(V131: (([],Int) -> [Int,Int,Int,Int],Int) -> [Int,Int,Int,Int])(V123, V132)
        }
        defn foo_lam_24(V122, V123, V124) {
          let V4 = V122[0];
          let V90 = V122[1];
          let V129 = make_closure(foo_lam_22,[V4, V90]);
          let V133 = make_closure(foo_lam_23,[V123]);
          __mon_bind(V129, V133, V124)
        }
        defn foo_lam_25(V135, V136, V137) {
          typecast<[Int,Int,Int,Int]>([0, V136])
        }
        defn foo_lam_26(V118, V119, V120) {
          let V4 = V118[0];
          let V90 = V118[1];
          let V121 = V90[0];
          let V134 = make_closure(foo_lam_24,[V4, V90]);
          let V138 = make_closure(foo_lam_25,[]);
          let V139 = [V121, V134, V138];
          typecast<[Int,Int,Int,Int]>([1, V139])
        }
        defn foo_lam_27(V94, V95) {
          let V4 = V94[0];
          let V90 = V94[1];
          let V93 = V94[2];
          let V117 = make_closure(foo_lam_21,[V93]);
          let V140 = make_closure(foo_lam_26,[V4, V90]);
          __mon_bind(V117, V140, V95)
        }
        defn foo_lam_28(V153, V154) {
          let V146 = V153[0];
          let V155 = [];
          let V156 = V146[1];
          let V157 = V156[1];
          apply_closure(V157: ([],Int) -> [Int,Int,Int,Int])(V155, V154)
        }
        defn foo_lam_29(V159, V160, V161) {
          let V151 = V159[0];
          apply_closure(V160: ((Int,Int) -> [Int,Int,Int,Int],Int) -> [Int,Int,Int,Int])(V151, V161)
        }
        defn foo_lam_30(V150, V151, V152) {
          let V146 = V150[0];
          let V158 = make_closure(foo_lam_28,[V146]);
          let V162 = make_closure(foo_lam_29,[V151]);
          __mon_bind(V158, V162, V152)
        }
        defn foo_lam_31(V164, V165, V166) {
          typecast<[Int,Int,Int,Int]>([0, V165])
        }
        defn foo_lam_32(V147, V148) {
          let V146 = V147[0];
          let V149 = V146[0];
          let V163 = make_closure(foo_lam_30,[V146]);
          let V167 = make_closure(foo_lam_31,[]);
          let V168 = [V149, V163, V167];
          typecast<[Int,Int,Int,Int]>([1, V168])
        }
        defn foo_lam_33(V142, V143) {
          let V2 = V142[0];
          let V87 = V142[1];
          let V144 = V2[3];
          let V145 = V144[0];
          let V146 = apply_closure(V145: (Int) -> [Int,[(Int,Int) -> [Int,Int,Int,Int],([],Int) -> [Int,Int,Int,Int]]])(V87);
          make_closure(foo_lam_32,[V146])
        }
        defn foo_lam_34(V86, V87) {
          let V1 = V86[0];
          let V2 = V86[1];
          let V4 = V86[2];
          let V88 = V2[3];
          let V89 = V88[0];
          let V90 = apply_closure(V89: (Int) -> [Int,[(Int,Int) -> [Int,Int,Int,Int],([],Int) -> [Int,Int,Int,Int]]])(V87);
          let V91 = V1[3];
          let V92 = V91[0];
          let V93 = apply_closure(V92: (Int) -> [Int,([],Int) -> [Int,Int,Int,Int]])(V87);
          let V141 = make_closure(foo_lam_27,[V4, V90, V93]);
          let V169 = make_closure(foo_lam_33,[V2, V87]);
          __mon_bind(V141, V169, V87)
        }
        defn foo_lam_35(V65, V66) {
          let V1 = V65[0];
          let V2 = V65[1];
          let V4 = V65[2];
          let V6 = V65[3];
          let V67 = [];
          let V68 = __mon_generate_marker(V67);
          let V81 = make_closure(foo_lam_15,[V1, V6, V68]);
          let V85 = make_closure(foo_lam_16,[]);
          let V170 = make_closure(foo_lam_34,[V1, V2, V4]);
          __mon_prompt(V68, V81, V85, V170, V66)
        }
        defn foo_lam_36(V9, V10) {
          let V0 = V9[0];
          let V1 = V9[1];
          let V2 = V9[2];
          let V4 = V9[3];
          let V6 = V9[4];
          let V8 = V9[5];
          let V55 = make_closure(foo_lam_10,[V0, V8]);
          let V64 = make_closure(foo_lam_12,[]);
          let V171 = make_closure(foo_lam_35,[V1, V2, V4, V6]);
          __mon_prompt(V8, V55, V64, V171, V10)
        }
        defn foo_lam_37(V5, V6) {
          let V0 = V5[0];
          let V1 = V5[1];
          let V2 = V5[2];
          let V4 = V5[3];
          let V7 = [];
          let V8 = __mon_generate_marker(V7);
          make_closure(foo_lam_36,[V0, V1, V2, V4, V6, V8])
        }
        defn main() {
          let V95 = make_closure(main_lam_22,[]);
          let V100 = make_closure(main_lam_23,[]);
          let V101 = [];
          let V102 = __mon_bind(V95, V100, V101);
          let V103 = V102[0];
          switch V103 <
            branch 0 {
              let V104 = V102[1];
              V104
            }
            branch 1 {
              let V104 = V102[1];
              5467
            }
          >
        }
        defn main_lam_0(V6, V7, V8) {
          V8
        }
        defn main_lam_1(V10, V11, V12, V13) {
          apply_closure(V12: ([Int,[(Int,[]) -> [Int,Int,Int,Int],([],[]) -> [Int,Int,Int,Int]]]) -> Int)(V13)
        }
        defn main_lam_2(V15, V16) {
          []
        }
        defn main_lam_3(V18, V19) {
          let V20 = V19[0];
          switch V20 <>
        }
        defn main_lam_4(V23, V24) {
          V24
        }
        defn main_lam_5(V26, V27) {
          V27
        }
        defn main_lam_6(V31, V32, V33) {
          [V32, V33]
        }
        defn main_lam_7(V35, V36, V37, V38) {
          let V39 = V38[0];
          switch V39 <
            branch 0 {
              let V40 = V38[1];
              apply_closure(V36: ([Int,[(Int,[]) -> [Int,Int,Int,Int],([],[]) -> [Int,Int,Int,Int]]]) -> Int)(V40)
            }
            branch 1 {
              let V41 = V38[1];
              apply_closure(V37: ([Int,([],[Int,[(Int,[]) -> [Int,Int,Int,Int],([],[]) -> [Int,Int,Int,Int]]]) -> [Int,Int,Int,Int]]) -> Int)(V41)
            }
          >
        }
        defn main_lam_8(V43, V44) {
          V44[0]
        }
        defn main_lam_9(V46, V47) {
          typecast<[Int,Int,Int]>([0, V47])
        }
        defn main_lam_10(V50, V51) {
          V51[1]
        }
        defn main_lam_11(V53, V54) {
          typecast<[Int,Int,Int]>([1, V54])
        }
        defn main_lam_12(V58, V59, V60) {
          [V60, V59]
        }
        defn main_lam_13(V62, V63, V64, V65) {
          let V66 = V65[0];
          switch V66 <
            branch 0 {
              let V67 = V65[1];
              apply_closure(V64: ([Int,[(Int,[]) -> [Int,Int,Int,Int],([],[]) -> [Int,Int,Int,Int]]]) -> Int)(V67)
            }
            branch 1 {
              let V68 = V65[1];
              apply_closure(V63: ([Int,([],[Int,[(Int,[]) -> [Int,Int,Int,Int],([],[]) -> [Int,Int,Int,Int]]]) -> [Int,Int,Int,Int]]) -> Int)(V68)
            }
          >
        }
        defn main_lam_14(V70, V71) {
          V71[1]
        }
        defn main_lam_15(V73, V74) {
          typecast<[Int,Int,Int]>([1, V74])
        }
        defn main_lam_16(V77, V78) {
          V78[0]
        }
        defn main_lam_17(V80, V81) {
          typecast<[Int,Int,Int]>([0, V81])
        }
        defn main_lam_18(V4, V5) {
          let V9 = make_closure(main_lam_0,[]);
          let V14 = make_closure(main_lam_1,[]);
          let V17 = make_closure(main_lam_2,[]);
          let V21 = make_closure(main_lam_3,[]);
          let V22 = [V17, V21];
          let V25 = make_closure(main_lam_4,[]);
          let V28 = make_closure(main_lam_5,[]);
          let V29 = [V25, V28];
          let V30 = [V9, V14, V22, V29];
          let V34 = make_closure(main_lam_6,[]);
          let V42 = make_closure(main_lam_7,[]);
          let V45 = make_closure(main_lam_8,[]);
          let V48 = make_closure(main_lam_9,[]);
          let V49 = [V45, V48];
          let V52 = make_closure(main_lam_10,[]);
          let V55 = make_closure(main_lam_11,[]);
          let V56 = [V52, V55];
          let V57 = [V34, V42, V49, V56];
          let V61 = make_closure(main_lam_12,[]);
          let V69 = make_closure(main_lam_13,[]);
          let V72 = make_closure(main_lam_14,[]);
          let V75 = make_closure(main_lam_15,[]);
          let V76 = [V72, V75];
          let V79 = make_closure(main_lam_16,[]);
          let V82 = make_closure(main_lam_17,[]);
          let V83 = [V79, V82];
          let V84 = [V61, V69, V76, V83];
          foo(V30, V57, V84, V5)
        }
        defn main_lam_19(V86, V87, V88) {
          apply_closure(V87: (Int,[]) -> [Int,Int,Int,Int])(16777215, V88)
        }
        defn main_lam_20(V2, V3) {
          let V85 = make_closure(main_lam_18,[]);
          let V89 = make_closure(main_lam_19,[]);
          __mon_bind(V85, V89, V3)
        }
        defn main_lam_21(V91, V92, V93) {
          apply_closure(V92: (Int,[]) -> [Int,Int,Int,Int])(14, V93)
        }
        defn main_lam_22(V0, V1) {
          let V90 = make_closure(main_lam_20,[]);
          let V94 = make_closure(main_lam_21,[]);
          __mon_bind(V90, V94, V1)
        }
        defn main_lam_23(V96, V97, V98) {
          let V99 = V97[1];
          typecast<[Int,Int,Int,Int]>([0, V99])
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
    expect.assert_eq(medir_module.pretty_string(&db, 80).as_str());
  }
}
