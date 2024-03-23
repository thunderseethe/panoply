use std::default;

use base::{
  id::{IdSupply, TermName},
  ident::Ident,
  modules::Module,
  pretty::{PrettyErrorWithDb, PrettyWithCtx},
};
use reducir::{
  mon::{MonReducIrItem, MonReducIrModule},
  optimized::{OptimizedReducIrItem, OptimizedReducIrModule},
  ReducIrTermName, TypeCheck,
};

mod occurrence;
mod simplify;
mod subst;
use simplify::{bind_term, prompt_term};

use crate::simplify::EtaExpand;

#[salsa::jar(db = Db)]
pub struct Jar(simple_reducir_item, simple_reducir_module);

pub trait Db: salsa::DbWithJar<Jar> + lower_reducir::Db {
  fn as_opt_reducir_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }

  fn simple_reducir_item_of(&self, name: TermName) -> OptimizedReducIrItem {
    let reducir_item = self.lower_reducir_mon_item_of(name);
    self.simple_reducir_item(reducir_item)
  }

  fn simple_reducir_item(&self, item: MonReducIrItem) -> OptimizedReducIrItem {
    simple_reducir_item(self.as_opt_reducir_db(), item)
  }

  fn simple_reducir_module(&self, module: Module) -> OptimizedReducIrModule {
    let ir_module = self.lower_reducir_mon_module_of(module);
    simple_reducir_module(self.as_opt_reducir_db(), ir_module)
  }

  fn simple_reducir_item_for_file_name(
    &self,
    path: std::path::PathBuf,
    item: Ident,
  ) -> Option<OptimizedReducIrItem> {
    let module = self.root_module_for_path(path);
    let term_name = self.id_for_name(module, item)?;
    Some(self.simple_reducir_item_of(term_name))
  }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + lower_reducir::Db {}

#[salsa::tracked]
fn simple_reducir_item(db: &dyn crate::Db, item: MonReducIrItem) -> OptimizedReducIrItem {
  let ir_db = db.as_reducir_db();
  let name = item.name(ir_db);
  let row_evs = item.row_evs(ir_db);
  let var_supply = item.var_supply(ir_db);

  let mut var_supply = IdSupply::start_from(var_supply);

  let ir = item.item(ir_db);

  let ir = simplify::simplify(db, name, row_evs, item.item(ir_db), &mut var_supply);

  let term_name = item.name(db.as_reducir_db());
  OptimizedReducIrItem::new(
    db.as_reducir_db(),
    ReducIrTermName::Term(term_name),
    ir.clone(),
  )
}

#[salsa::tracked]
fn simple_reducir_module(
  db: &dyn crate::Db,
  ir_module: MonReducIrModule,
) -> OptimizedReducIrModule {
  let reducir_db = db.as_reducir_db();
  let module = ir_module.module(reducir_db);

  // TODO: Instead of incluing these in the module we should create a separate method
  // that produces a module with just these functions inside it.
  // Then lower that module down to wasm and link it in automatically (instead of hardcoding the
  // wasm version of these functions.)
  let bind_name = ReducIrTermName::gen(db, "__mon_bind", module);
  let (bind, mut bind_supply) = bind_term(db, bind_name);
  let bind = bind.fold(&mut EtaExpand {
    db,
    supply: &mut bind_supply,
    name: bind_name,
  });
  debug_assert!(bind.type_check(db).map_err_pretty_with(db).is_ok());

  let prompt_name = ReducIrTermName::gen(db, "__mon_prompt", module);
  let (prompt, mut prompt_supply) = prompt_term(db, module, prompt_name);
  let prompt = prompt.fold(&mut EtaExpand {
    db,
    supply: &mut prompt_supply,
    name: prompt_name,
  });
  debug_assert!(prompt.type_check(db).map_err_pretty_with(db).is_ok());

  OptimizedReducIrModule::new(
    db.as_reducir_db(),
    module,
    ir_module
      .items(reducir_db)
      .iter()
      .map(|item| db.simple_reducir_item(*item))
      .chain([
        OptimizedReducIrItem::new(reducir_db, bind_name, bind),
        OptimizedReducIrItem::new(reducir_db, prompt_name, prompt),
      ])
      .collect::<Vec<_>>(),
  )
}

#[cfg(test)]
mod tests {
  use base::{
    file::{FileId, SourceFile, SourceFileSet},
    pretty::{PrettyErrorWithDb, PrettyPrint, PrettyWithCtx},
    Db as BaseDb,
  };
  use expect_test::expect;
  use parser::Db as ParseDb;
  use reducir::{optimized::OptimizedReducIrItem, TypeCheck};

  use crate::Db;

  #[derive(Default)]
  #[salsa::db(
    crate::Jar,
    ast::Jar,
    base::Jar,
    desugar::Jar,
    lower_reducir::Jar,
    nameres::Jar,
    parser::Jar,
    reducir::Jar,
    tc::Jar,
    ty::Jar
  )]
  struct TestDatabase {
    storage: salsa::Storage<Self>,
  }
  impl salsa::Database for TestDatabase {}

  fn simple_function(db: &TestDatabase, fn_name: &str, input: &str) -> OptimizedReducIrItem {
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

    match db.simple_reducir_item_for_file_name(path, db.ident_str(fn_name)) {
      Some(term) => term,
      None => {
        dbg!(db.all_parse_errors());
        panic!("Errors occurred")
      }
    }
  }

  fn simple_snippet(db: &TestDatabase, input: &str) -> OptimizedReducIrItem {
    let main = format!("f = {}", input);
    simple_function(db, "f", &main)
  }

  #[test]
  fn simplify_state_get() {
    let db = TestDatabase::default();

    let ir = simple_snippet(
      &db,
      r#"
(with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(825)"#,
    );
    let simple_ir = ir.item(&db);

    let pretty_ir = simple_ir.pretty_with(&db).pprint().pretty(80).to_string();
    let expect = expect![[r#"
        (forall [(1: ScopedRow) (0: ScopedRow)] (fun [V1, V0]
            (let (V20 ((__mon_generate_marker @ [..]) {}))
              ((__mon_bind @ [..])
                (fun [V236]
                  ((__mon_prompt @ [..])
                    V20
                    (fun [V21]
                      (V1[0]
                        V21
                        {V20, {(fun [V11, V27]
                          <0: (fun [V12, V26]
                              <0: (fun [V13, V229]
                                  ((__mon_bind @ [..])
                                    (fun [V227] (V12 {} V227))
                                    (fun [V25, V228] (V25 V11 V228))
                                    V229))>)>), (fun [V8, V24]
                          <0: (fun [V9, V23]
                              <0: (fun [V10, V232]
                                  ((__mon_bind @ [..])
                                    (fun [V230] (V9 V10 V230))
                                    (fun [V22, V231] (V22 V10 V231))
                                    V232))>)>)}}))
                    (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                    (fun [V16]
                      (let (V18 (V1[3][0] V16))
                        <1: (forall [(0: Type) (1: Type) (2: Type)] {V18[0], (fun
                              [V19
                              ,V235]
                              ((__mon_bind @ [..])
                                (fun [V233] (V18[1][1] {} V233))
                                (fun [V32, V234] (V32 V19 V234))
                                V235)), (fun [V31, V33] <0: V31>)})>))
                    V236))
                (fun [V34, V237] (V34 825 V237))
                V0))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall ScopedRow .
          forall ScopedRow .
            { {1} -> { (Marker Int -> (Mon {1} {Int, Int}))
                     , { Int -> (Mon {1} ({} -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       , {} -> (Mon {1} (Int -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                       }
                     } -> {0}
            , forall Type .
              (<2> -> T0) -> ({ (Marker Int -> (Mon {2} {Int, Int}))
                              , { Int -> (Mon {2} ({} -> (Mon {2} Int -> (Mon {2} { Int
                                                                                  , Int
                                                                                  })))
                                -> (Mon {2} Int -> (Mon {2} {Int, Int})))
                                , {} -> (Mon {2} (Int -> (Mon {2} Int -> (Mon {2} { Int
                                                                                  , Int
                                                                                  })))
                                -> (Mon {2} Int -> (Mon {2} {Int, Int})))
                                }
                              } -> T0) -> <1> -> T0
            , {{0} -> {1}, <1> -> <0>}
            , { {0} -> { (Marker Int -> (Mon {1} {Int, Int}))
                       , { Int -> (Mon {1} ({} -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         , {} -> (Mon {1} (Int -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         -> (Mon {1} Int -> (Mon {1} {Int, Int})))
                         }
                       }
              , { (Marker Int -> (Mon {1} {Int, Int}))
                , { Int -> (Mon {1} ({} -> (Mon {1} Int -> (Mon {1} {Int, Int}))) ->
                  (Mon {1} Int -> (Mon {1} {Int, Int})))
                  , {} -> (Mon {1} (Int -> (Mon {1} Int -> (Mon {1} {Int, Int}))) ->
                  (Mon {1} Int -> (Mon {1} {Int, Int})))
                  }
                } -> <0>
              }
            } -> (Mon {1} {Int, Int})"#]];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }

  #[test]
  #[ignore = "Fix inlining variables so they typecheck"]
  fn simplify_state_get_in_main() {
    let db = TestDatabase::default();

    let ir = simple_function(
      &db,
      "main",
      r#"
main = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(825).value"#,
    );
    let simple_ir = ir.item(&db);

    let pretty_ir = simple_ir.pretty_with(&db).pprint().pretty(80).to_string();
    let expect =
      expect!["(let (V20 ((__mon_generate_marker @ [Ty(Int -> {} {Int, Int})]) {})) 825)"];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }

  #[test]
  //#[ignore = "Fix inlining variables so they typecheck"]
  fn simplify_multi_effect_in_main() {
    let db = TestDatabase::default();

    let ir = simple_function(
      &db,
      "main",
      r#"
main = (with {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do (with {
  ask = |x| |k| k(16777215),
  return = |x| x,
} do State.get({})))(14).value
      "#,
    );

    let simple_ir = ir.item(&db);

    let pretty_ir = simple_ir.pretty_string(&db, 80);
    let expect = expect![[r#"
        (let (V31 ((__mon_generate_marker @ [..]) {}))
          (case ((__mon_bind @ [..])
              (fun [V422]
                ((__mon_bind @ [..])
                  (fun [V420]
                    ((__mon_prompt @ [..])
                      V31
                      (fun [V32]
                        {V31, {(fun [V15, V38]
                          <0: (fun [V16, V37]
                              <0: (fun [V17, V412]
                                  ((__mon_bind @ [..])
                                    (fun [V410] (V16 {} V410))
                                    (fun [V36, V411] (V36 V15 V411))
                                    V412))>)>), (fun [V12, V35]
                          <0: (fun [V13, V34]
                              <0: (fun [V14, V415]
                                  ((__mon_bind @ [..])
                                    (fun [V413] (V13 V14 V413))
                                    (fun [V33, V414] (V33 V14 V414))
                                    V415))>)>)}})
                      (fun [V18, V40] <0: (fun [V19, V39] <0: {V19, V18}>)>)
                      (fun [V20]
                        (let (V29 ((__mon_generate_marker @ [..]) {}))
                          ((__mon_prompt @ [..])
                            V29
                            (fun [V30]
                              {V30, {V29, (fun [V22, V41]
                                <0: (fun [V23, V416] (V23 16777215 V416))>)}})
                            (fun [V24, V42] <0: V24>)
                            (fun [V25]
                              <1: (forall [(0: Type) (1: Type) (2: Type)] {
                                  V25[0][0], (fun [V28, V419]
                                    ((__mon_bind @ [..])
                                      (fun [V417] (V25[0][1][1] {} V417))
                                      (fun [V45, V418] (V45 V28 V418))
                                      V419)), (fun [V44, V46] <0: V44>)})>)
                            V20)))
                      V420))
                  (fun [V47, V421] (V47 14 V421))
                  V422))
              (fun [V48, V49] <0: V48[1]>)
              {})
            (fun [V0] V0)
            (fun [V0] 5467)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_string(&db, 80));
  }
}
