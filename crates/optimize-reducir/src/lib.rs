use base::{id::TermName, ident::Ident, modules::Module, pretty::PrettyErrorWithDb};
use reducir::{
  mon::{MonReducIrItem, MonReducIrModule},
  optimized::{OptimizedReducIrItem, OptimizedReducIrModule},
  ReducIrTermName, TypeCheck,
};

mod occurrence;
mod simplify;
mod subst;
use simplify::{bind_term, prompt_term};

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
  let ir = simplify::simplify(db, name, row_evs, item.item(ir_db));

  let term_name = item.name(db.as_reducir_db());
  OptimizedReducIrItem::new(db.as_reducir_db(), ReducIrTermName::Term(term_name), ir)
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
  let bind = bind_term(db, bind_name);
  debug_assert!(bind.type_check(db).map_err_pretty_with(db).is_ok());

  let prompt_name = ReducIrTermName::gen(db, "__mon_prompt", module);
  let prompt = prompt_term(db, module, prompt_name);
  prompt
    .type_check(db)
    .map_err_pretty_with(db)
    .expect("Prompt should type check");

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

  fn simple_function(db: &TestDatabase, input: &str, fn_name: &str) -> OptimizedReducIrItem {
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
    simple_function(db, &main, "f")
  }

  #[test]
  #[ignore = "Fixing monadic lowering of ops"]
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
        (forall [(T1: ScopedRow) (T0: ScopedRow)] (fun [V1, V0]
            (let
              [ (V19 ((__mon_generate_marker @ [Ty(Int -> {Int, Int})]) {}))
              , (V0 (V1[0]
                V0
                {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun [V8, V9, V10]
                  (V9 V10 V10))}}))
              , (V17 (V1[3][0] V0))
              ]
              (case (case (__mon_eqm V19 V17[0])
                  (fun [V5]
                    <1: (forall [(T3: Type) (T4: Type) (T5: Type)] {V17[0], (fun [V18]
                          (V17[1][1] {} V18)), (fun [V8]
                          ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int -> {Int, Int})])
                            V19
                            (fun [V0]
                              (V1[0]
                                V0
                                {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun
                                  [V8
                                  ,V9
                                  ,V10] (V9 V10 V10))}}))
                            ((__mon_bind @ [Ty({3}), Ty(Int), Ty(Int -> {Int, Int})])
                              (fun [V0] <0: V8>)
                              (fun [V21] (fun [V0] <0: (fun [V15] {V15, V21})>)))))})>)
                  (fun [V5]
                    (V17[1][1]
                      {}
                      (fun [V9]
                        ((__mon_prompt @ [Ty({1}), Ty({0}), Ty(Int -> {Int, Int})])
                          V19
                          (fun [V0]
                            (V1[0]
                              V0
                              {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun
                                [V8
                                ,V9
                                ,V10] (V9 V10 V10))}}))
                          ((__mon_bind @ [Ty({0}), Ty(Int), Ty(Int -> {Int, Int})])
                            (fun [V0] <0: V9>)
                            (fun [V21] (fun [V0] <0: (fun [V15] {V15, V21})>)))))
                      V0)))
                (fun [V3] <0: (V3 825)>)
                (fun [V4]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V5 (V4 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V5[0], V5[1], (fun [V6]
                          ((__mon_bind @ [Ty({4}), Ty(Int -> {Int, Int}), Ty({ Int
                                                                             , Int
                                                                             })])
                            (V5[2] V6)
                            (fun [V23] (let (V24 (V23 825)) (fun [V0] <0: V24>)))))
                        }))>)))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall ScopedRow .
          forall ScopedRow .
            { {1} -> { (Marker Int -> {Int, Int})
                     , { Int -> ({} -> Int -> {Int, Int}) -> Int -> {Int, Int}
                       , {} -> (Int -> Int -> {Int, Int}) -> Int -> {Int, Int}
                       }
                     } -> {0}
            , forall Type .
              (<2> -> T0) -> ({ (Marker Int -> {Int, Int})
                              , { Int -> ({} -> Int -> {Int, Int}) -> Int -> {Int, Int}
                                , {} -> (Int -> Int -> {Int, Int}) -> Int -> {Int, Int}
                                }
                              } -> T0) -> <1> -> T0
            , {{0} -> {1}, <1> -> <0>}
            , { {0} -> { (Marker Int -> {Int, Int})
                       , { Int -> ({} -> Int -> {Int, Int}) -> Int -> {Int, Int}
                         , {} -> (Int -> Int -> {Int, Int}) -> Int -> {Int, Int}
                         }
                       }
              , { (Marker Int -> {Int, Int})
                , { Int -> ({} -> Int -> {Int, Int}) -> Int -> {Int, Int}
                  , {} -> (Int -> Int -> {Int, Int}) -> Int -> {Int, Int}
                  }
                } -> <0>
              }
            } -> {1} -> (Control {1} {Int, Int})"#]];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }

  #[test]
  fn simplify_state_get_in_main() {
    let db = TestDatabase::default();

    let ir = simple_function(
      &db,
      r#"
main = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(825).value"#,
      "main",
    );
    let simple_ir = ir.item(&db);

    let pretty_ir = simple_ir.pretty_with(&db).pprint().pretty(80).to_string();
    let expect = expect![[r#"
        (let (V19 ((__mon_generate_marker @ [Ty(Int -> {Int, Int})]) {}))
          (case (case (case ((__mon_prompt @ [Ty({}), Ty({ (Marker Int -> {Int, Int})
                                                         , { Int -> ({} -> Int -> { Int
                                                                                  , Int
                                                                                  }) ->
                                                           Int -> {Int, Int}
                                                           , {} -> (Int -> Int -> { Int
                                                                                  , Int
                                                                                  }) ->
                                                           Int -> {Int, Int}
                                                           }
                                                         }), Ty(Int -> {Int, Int})])
                  V19
                  (fun [V0]
                    {V19, {(fun [V11, V12, V13] (V12 {} V11)), (fun [V8, V9, V10]
                      (V9 V10 V10))}})
                  ((__mon_bind @ [Ty({ (Marker Int -> {Int, Int})
                                     , { Int -> ({} -> Int -> {Int, Int}) -> Int ->
                                       {Int, Int}
                                       , {} -> (Int -> Int -> {Int, Int}) -> Int ->
                                       {Int, Int}
                                       }
                                     }), Ty(Int), Ty(Int -> {Int, Int})])
                    (fun [V0] <0: {}>)
                    (fun [V21] (fun [V0] <0: (fun [V15] {V15, V21})>)))
                  {})
                (fun [V3] <0: (V3 825)>)
                (fun [V4]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V5 (V4 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V5[0], V5[1], (fun [V6]
                          ((__mon_bind @ [Ty({}), Ty(Int -> {Int, Int}), Ty({ Int
                                                                            , Int
                                                                            })])
                            (V5[2] V6)
                            (fun [V23] (let (V24 (V23 825)) (fun [V0] <0: V24>)))))
                        }))>))
              (fun [V3] <0: V3[1]>)
              (fun [V4]
                <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                      (V5 (V4 @ [Ty(T2), Ty(T1), Ty(T0)]))
                      {V5[0], V5[1], (fun [V6]
                        ((__mon_bind @ [Ty({}), Ty({Int, Int}), Ty(Int)])
                          (V5[2] V6)
                          (fun [V25] (fun [V0] <0: V25[1]>))))}))>))
            (fun [V0] V0)
            (fun [V0] 5467)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }
}
