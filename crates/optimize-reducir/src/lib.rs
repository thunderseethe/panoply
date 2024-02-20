use base::{
  id::{IdSupply, TermName},
  ident::Ident,
  modules::Module,
  pretty::PrettyErrorWithDb,
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

  let ir = simplify::simplify(db, name, row_evs, item.item(ir_db), &mut var_supply);

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
              [ (V19 ((__mon_generate_marker @ [Ty(Int -> {1} {Int, Int})]) {}))
              , (V0 (V1[0]
                V0
                {V19, {(fun [V11, V0]
                  <0: (fun [V12, V0]
                      <0: (fun [V13]
                          (let (V262 (V12 {}))
                            (fun [V264]
                              (case (V262 V264)
                                (fun [V265] (V265 V11 V264))
                                (fun [V269]
                                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                                        (V271 (V269 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                        {V271[0], V271[1], (fun [V272, V375]
                                          ((__mon_bind @ [Ty({4}), Ty(Int -> {4} ->
                                          (Control {4} {Int, Int})), Ty({Int, Int})])
                                            (V271[2] V272)
                                            (fun [V21] (V21 V11))
                                            V375))}))>)))))>)>), (fun [V8, V0]
                  <0: (fun [V9, V0]
                      <0: (fun [V10]
                          (let (V243 (V9 V10))
                            (fun [V245]
                              (case (V243 V245)
                                (fun [V246] (V246 V10 V245))
                                (fun [V250]
                                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                                        (V252 (V250 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                        {V252[0], V252[1], (fun [V253, V376]
                                          ((__mon_bind @ [Ty({4}), Ty(Int -> {4} ->
                                          (Control {4} {Int, Int})), Ty({Int, Int})])
                                            (V252[2] V253)
                                            (fun [V20] (V20 V10))
                                            V376))}))>)))))>)>)}}))
              , (V17 (V1[3][0] V0))
              ]
              (case (case (__mon_eqm V19 V17[0])
                  (fun [V336]
                    <1: (forall [(T4: Type) (T5: Type) (T6: Type)] {V17[0], (fun [V18]
                          (let (V355 (V17[1][1] {}))
                            (fun [V357]
                              (case (V355 V357)
                                (fun [V358] (V358 V18 V357))
                                (fun [V362]
                                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                                        (V364 (V362 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                        {V364[0], V364[1], (fun [V365, V366]
                                          ((__mon_bind @ [Ty({7}), Ty((Int -> {7} ->
                                          (Control {7} Int -> {7} -> (Control {7} { Int
                                                                                  , Int
                                                                                  })))
                                          -> {7} -> (Control {7} Int -> {7} ->
                                          (Control {7} {Int, Int}))), Ty(Int -> {7} ->
                                          (Control {7} {Int, Int}))])
                                            (V364[2] V365)
                                            (fun [V23] (V23 V18))
                                            V366))}))>))))), (fun [V340, V341]
                          ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int -> {4} ->
                          (Control {4} {Int, Int}))])
                            V19
                            (fun [V0]
                              (V1[0]
                                V0
                                {V19, {(fun [V11, V0]
                                  <0: (fun [V12, V0]
                                      <0: (fun [V13]
                                          (let (V262 (V12 {}))
                                            (fun [V264]
                                              (case (V262 V264)
                                                (fun [V265] (V265 V11 V264))
                                                (fun [V269]
                                                  <1: (forall
                                                      [(T3: Type) (T4: Type) (T5: Type)]
                                                      (let
                                                        (V271 (V269 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                        {V271[0], V271[1], (fun
                                                          [V272
                                                          ,V367]
                                                          ((__mon_bind @ [Ty({7}), Ty(Int
                                                          -> {7} -> (Control {7} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                                                            (V271[2] V272)
                                                            (fun [V21] (V21 V11))
                                                            V367))}))>)))))>)>), (fun
                                  [V8
                                  ,V0]
                                  <0: (fun [V9, V0]
                                      <0: (fun [V10]
                                          (let (V243 (V9 V10))
                                            (fun [V245]
                                              (case (V243 V245)
                                                (fun [V246] (V246 V10 V245))
                                                (fun [V250]
                                                  <1: (forall
                                                      [(T3: Type) (T4: Type) (T5: Type)]
                                                      (let
                                                        (V252 (V250 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                        {V252[0], V252[1], (fun
                                                          [V253
                                                          ,V368]
                                                          ((__mon_bind @ [Ty({7}), Ty(Int
                                                          -> {7} -> (Control {7} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                                                            (V252[2] V253)
                                                            (fun [V20] (V20 V10))
                                                            V368))}))>)))))>)>)}}))
                            (fun [V14, V0] <0: (fun [V15, V0] <0: {V15, V14}>)>)
                            (fun [V0] <0: V340>)
                            V341))})>)
                  (fun [V344]
                    (case (V17[1][1] {} V0)
                      (fun [V358]
                        (V358
                          (fun [V345, V346]
                            ((__mon_prompt @ [Ty({1}), Ty({0}), Ty(Int), Ty(Int -> {1}
                            -> (Control {1} {Int, Int}))])
                              V19
                              (fun [V0]
                                (V1[0]
                                  V0
                                  {V19, {(fun [V11, V0]
                                    <0: (fun [V12, V0]
                                        <0: (fun [V13]
                                            (let (V262 (V12 {}))
                                              (fun [V264]
                                                (case (V262 V264)
                                                  (fun [V265] (V265 V11 V264))
                                                  (fun [V269]
                                                    <1: (forall
                                                        [(T3: Type) (T4: Type) (T5: Type)]
                                                        (let
                                                          (V271 (V269 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                          {V271[0], V271[1], (fun
                                                            [V272
                                                            ,V369]
                                                            ((__mon_bind @ [Ty({4}), Ty(Int
                                                            -> {4} -> (Control {4} { Int
                                                                                   , Int
                                                                                   })), Ty({ Int
                                                                                           , Int
                                                                                           })])
                                                              (V271[2] V272)
                                                              (fun [V21] (V21 V11))
                                                              V369))}))>)))))>)>), (fun
                                    [V8
                                    ,V0]
                                    <0: (fun [V9, V0]
                                        <0: (fun [V10]
                                            (let (V243 (V9 V10))
                                              (fun [V245]
                                                (case (V243 V245)
                                                  (fun [V246] (V246 V10 V245))
                                                  (fun [V250]
                                                    <1: (forall
                                                        [(T3: Type) (T4: Type) (T5: Type)]
                                                        (let
                                                          (V252 (V250 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                          {V252[0], V252[1], (fun
                                                            [V253
                                                            ,V370]
                                                            ((__mon_bind @ [Ty({4}), Ty(Int
                                                            -> {4} -> (Control {4} { Int
                                                                                   , Int
                                                                                   })), Ty({ Int
                                                                                           , Int
                                                                                           })])
                                                              (V252[2] V253)
                                                              (fun [V20] (V20 V10))
                                                              V370))}))>)))))>)>)}}))
                              (fun [V14, V0] <0: (fun [V15, V0] <0: {V15, V14}>)>)
                              (fun [V0] <0: V345>)
                              V346))
                          V0))
                      (fun [V362]
                        <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                              (V364 (V362 @ [Ty(T2), Ty(T1), Ty(T0)]))
                              {V364[0], V364[1], (fun [V365, V373]
                                ((__mon_bind @ [Ty({4}), Ty((Int -> {4} ->
                                (Control {4} Int -> {4} -> (Control {4} {Int, Int}))) ->
                                {4} -> (Control {4} Int -> {4} -> (Control {4} { Int
                                                                               , Int
                                                                               }))), Ty(Int
                                -> {4} -> (Control {4} {Int, Int}))])
                                  (V364[2] V365)
                                  (fun [V23]
                                    (V23
                                      (fun [V345, V346]
                                        ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int
                                        -> {4} -> (Control {4} {Int, Int}))])
                                          V19
                                          (fun [V0]
                                            (V1[0]
                                              V0
                                              {V19, {(fun [V11, V0]
                                                <0: (fun [V12, V0]
                                                    <0: (fun [V13]
                                                        (let (V262 (V12 {}))
                                                          (fun [V264]
                                                            (case (V262 V264)
                                                              (fun [V265]
                                                                (V265 V11 V264))
                                                              (fun [V269]
                                                                <1: (forall
                                                                    [(T3: Type) (T4: Type) (T5: Type)]
                                                                    (let
                                                                      (V271 (V269 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                      {
                                                                      V271[0], V271[1], (fun
                                                                        [V272
                                                                        ,V371]
                                                                        ((__mon_bind @ [Ty({7}), Ty(Int
                                                                        -> {7} ->
                                                                        (Control {7} { Int
                                                                                     , Int
                                                                                     })), Ty({ Int
                                                                                             , Int
                                                                                             })])
                                                                          (V271[2] V272)
                                                                          (fun [V21]
                                                                            (V21 V11))
                                                                          V371))
                                                                      }))>)))))>)>), (fun
                                                [V8
                                                ,V0]
                                                <0: (fun [V9, V0]
                                                    <0: (fun [V10]
                                                        (let (V243 (V9 V10))
                                                          (fun [V245]
                                                            (case (V243 V245)
                                                              (fun [V246]
                                                                (V246 V10 V245))
                                                              (fun [V250]
                                                                <1: (forall
                                                                    [(T3: Type) (T4: Type) (T5: Type)]
                                                                    (let
                                                                      (V252 (V250 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                      {
                                                                      V252[0], V252[1], (fun
                                                                        [V253
                                                                        ,V372]
                                                                        ((__mon_bind @ [Ty({7}), Ty(Int
                                                                        -> {7} ->
                                                                        (Control {7} { Int
                                                                                     , Int
                                                                                     })), Ty({ Int
                                                                                             , Int
                                                                                             })])
                                                                          (V252[2] V253)
                                                                          (fun [V20]
                                                                            (V20 V10))
                                                                          V372))
                                                                      }))>)))))>)>)}}))
                                          (fun [V14, V0]
                                            <0: (fun [V15, V0] <0: {V15, V14}>)>)
                                          (fun [V0] <0: V345>)
                                          V346))))
                                  V373))}))>))))
                (fun [V227] (V227 825 V0))
                (fun [V231]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V233 (V231 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V233[0], V233[1], (fun [V234, V374]
                          ((__mon_bind @ [Ty({4}), Ty(Int -> {4} -> (Control {4} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                            (V233[2] V234)
                            (fun [V24] (V24 825))
                            V374))}))>)))))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect![[r#"
        forall ScopedRow .
          forall ScopedRow .
            { {1} -> { (Marker Int -> {1} -> (Control {1} {Int, Int}))
                     , { Int -> {1} -> (Control {1} ({} -> {1} -> (Control {1} Int ->
                       {1} -> (Control {1} {Int, Int}))) -> {1} -> (Control {1} Int ->
                       {1} -> (Control {1} {Int, Int})))
                       , {} -> {1} -> (Control {1} (Int -> {1} -> (Control {1} Int ->
                       {1} -> (Control {1} {Int, Int}))) -> {1} -> (Control {1} Int ->
                       {1} -> (Control {1} {Int, Int})))
                       }
                     } -> {0}
            , forall Type .
              (<2> -> T0) -> ({ (Marker Int -> {2} -> (Control {2} {Int, Int}))
                              , { Int -> {2} -> (Control {2} ({} -> {2} ->
                                (Control {2} Int -> {2} -> (Control {2} {Int, Int}))) ->
                                {2} -> (Control {2} Int -> {2} -> (Control {2} { Int
                                                                               , Int
                                                                               })))
                                , {} -> {2} -> (Control {2} (Int -> {2} ->
                                (Control {2} Int -> {2} -> (Control {2} {Int, Int}))) ->
                                {2} -> (Control {2} Int -> {2} -> (Control {2} { Int
                                                                               , Int
                                                                               })))
                                }
                              } -> T0) -> <1> -> T0
            , {{0} -> {1}, <1> -> <0>}
            , { {0} -> { (Marker Int -> {1} -> (Control {1} {Int, Int}))
                       , { Int -> {1} -> (Control {1} ({} -> {1} -> (Control {1} Int ->
                         {1} -> (Control {1} {Int, Int}))) -> {1} -> (Control {1} Int ->
                         {1} -> (Control {1} {Int, Int})))
                         , {} -> {1} -> (Control {1} (Int -> {1} -> (Control {1} Int ->
                         {1} -> (Control {1} {Int, Int}))) -> {1} -> (Control {1} Int ->
                         {1} -> (Control {1} {Int, Int})))
                         }
                       }
              , { (Marker Int -> {1} -> (Control {1} {Int, Int}))
                , { Int -> {1} -> (Control {1} ({} -> {1} -> (Control {1} Int -> {1} ->
                  (Control {1} {Int, Int}))) -> {1} -> (Control {1} Int -> {1} ->
                  (Control {1} {Int, Int})))
                  , {} -> {1} -> (Control {1} (Int -> {1} -> (Control {1} Int -> {1} ->
                  (Control {1} {Int, Int}))) -> {1} -> (Control {1} Int -> {1} ->
                  (Control {1} {Int, Int})))
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
        (let (V19 ((__mon_generate_marker @ [Ty(Int -> {} {Int, Int})]) {}))
          (case (case (case ((__mon_prompt @ [Ty({}), Ty({ (Marker Int -> {} ->
                                                         (Control {} {Int, Int}))
                                                         , { Int -> {} ->
                                                           (Control {} ({} -> {} ->
                                                           (Control {} Int -> {} ->
                                                           (Control {} {Int, Int}))) ->
                                                           {} -> (Control {} Int -> {}
                                                           -> (Control {} {Int, Int})))
                                                           , {} -> {} ->
                                                           (Control {} (Int -> {} ->
                                                           (Control {} Int -> {} ->
                                                           (Control {} {Int, Int}))) ->
                                                           {} -> (Control {} Int -> {}
                                                           -> (Control {} {Int, Int})))
                                                           }
                                                         }), Ty(Int), Ty(Int -> {} ->
                (Control {} {Int, Int}))])
                  V19
                  (fun [V0]
                    {V19, {(fun [V11, V0]
                      <0: (fun [V12, V0]
                          <0: (fun [V13]
                              (let (V309 (V12 {}))
                                (fun [V311]
                                  (case (V309 V311)
                                    (fun [V312] (V312 V11 V311))
                                    (fun [V316]
                                      <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                          (let (V318 (V316 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                            {V318[0], V318[1], (fun [V319, V413]
                                              ((__mon_bind @ [Ty({}), Ty(Int -> {} ->
                                              (Control {} {Int, Int})), Ty({Int, Int})])
                                                (V318[2] V319)
                                                (fun [V21] (V21 V11))
                                                V413))}))>)))))>)>), (fun [V8, V0]
                      <0: (fun [V9, V0]
                          <0: (fun [V10]
                              (let (V290 (V9 825))
                                (fun [V292]
                                  (case (V290 {})
                                    (fun [V293] (V293 825 {}))
                                    (fun [V297]
                                      <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                          (let (V299 (V297 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                            {V299[0], V299[1], (fun [V300, V414]
                                              ((__mon_bind @ [Ty({}), Ty(Int -> {} ->
                                              (Control {} {Int, Int})), Ty({Int, Int})])
                                                (V299[2] V300)
                                                (fun [V20] (V20 825))
                                                V414))}))>)))))>)>)}})
                  (fun [V14, V0] <0: (fun [V15, V0] <0: {V15, V14}>)>)
                  (fun [V0] <0: 825>)
                  {})
                (fun [V293] (V293 825 {}))
                (fun [V297]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V299 (V297 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V299[0], V299[1], (fun [V300, V415]
                          ((__mon_bind @ [Ty({}), Ty(Int -> {} -> (Control {} { Int
                                                                              , Int
                                                                              })), Ty({ Int
                                                                                      , Int
                                                                                      })])
                            (V299[2] V300)
                            (fun [V20] (V20 825))
                            V415))}))>))
              (fun [V255] <0: V255[1]>)
              (fun [V259]
                <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                      (V261 (V259 @ [Ty(T2), Ty(T1), Ty(T0)]))
                      {V261[0], V261[1], (fun [V262, V416]
                        ((__mon_bind @ [Ty({}), Ty({Int, Int}), Ty(Int)])
                          (V261[2] V262)
                          (fun [V25, V0] <0: V25[1]>)
                          V416))}))>))
            (fun [V0] V0)
            (fun [V0] 5467)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }
}
