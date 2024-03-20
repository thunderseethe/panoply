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
  println!("{}", ir.pretty_string(db, 80));
  ir.type_check(ir_db).unwrap();

  let ir = simplify::simplify(db, name, row_evs, item.item(ir_db), &mut var_supply);

  println!("{}", ir.pretty_string(db, 80));

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
  #[ignore = "Fix inlining variables so they typecheck"]
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
            (let
              [ (V20 ((__mon_generate_marker @ [Ty(Int -> {1} {Int, Int})]) {}))
              , (V18 (V1[3][0]
                (V1[0]
                  V0
                  {V20, {(fun [V11, V27]
                    <0: (fun [V12, V26]
                        <0: (fun [V13]
                            (let (V174 (fun [V402] (V12 {} V402)))
                              (fun [V176]
                                (case (V174 V176)
                                  (fun [V177] (V177 V11 V176))
                                  (fun [V181]
                                    <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                                          (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                          {V183[0], V183[1], (fun [V184, V401]
                                            ((__mon_bind @ [Ty({4}), Ty(Int ->
                                            (Mon {4} {Int, Int})), Ty({Int, Int})])
                                              (V183[2] V184)
                                              (fun [V25, V400] (V25 V11 V400))
                                              V401))}))>)))))>)>), (fun [V8, V24]
                    <0: (fun [V9, V23]
                        <0: (fun [V10]
                            (let (V155 (fun [V405] (V9 V10 V405)))
                              (fun [V157]
                                (case (V155 V157)
                                  (fun [V158] (V158 V10 V157))
                                  (fun [V162]
                                    <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                                          (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                          {V164[0], V164[1], (fun [V165, V404]
                                            ((__mon_bind @ [Ty({4}), Ty(Int ->
                                            (Mon {4} {Int, Int})), Ty({Int, Int})])
                                              (V164[2] V165)
                                              (fun [V22, V403] (V22 V10 V403))
                                              V404))}))>)))))>)>)}})))
              ]
              (case (case (__mon_eqm V20 V18[0])
                  (fun [V117]
                    <1: (forall [(4: Type) (5: Type) (6: Type)] {V18[0], (fun [V19]
                          (let (V136 (V18[1][1] {}))
                            (fun [V138]
                              (case (V136 V138)
                                (fun [V139] (V139 V19 V138))
                                (fun [V143]
                                  <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                                        (V145 (V143 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                        {V145[0], V145[1], (fun [V146, V377]
                                          ((__mon_bind @ [Ty({7}), Ty((Int ->
                                          (Mon {7} Int -> (Mon {7} {Int, Int}))) ->
                                          (Mon {7} Int -> (Mon {7} {Int, Int}))), Ty(Int
                                          -> (Mon {7} {Int, Int}))])
                                            (V145[2] V146)
                                            (fun [V32, V376] (V32 V19 V376))
                                            V377))}))>))))), (fun [V121, V122]
                          ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int ->
                          (Mon {4} {Int, Int}))])
                            V20
                            (fun [V21]
                              (V1[0]
                                V21
                                {V20, {(fun [V11, V27]
                                  <0: (fun [V12, V26]
                                      <0: (fun [V13]
                                          (let (V174 (fun [V380] (V12 {} V380)))
                                            (fun [V176]
                                              (case (V174 V176)
                                                (fun [V177] (V177 V11 V176))
                                                (fun [V181]
                                                  <1: (forall
                                                      [(3: Type) (4: Type) (5: Type)]
                                                      (let
                                                        (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                        {V183[0], V183[1], (fun
                                                          [V184
                                                          ,V379]
                                                          ((__mon_bind @ [Ty({7}), Ty(Int
                                                          -> (Mon {7} { Int
                                                                      , Int
                                                                      })), Ty({ Int
                                                                              , Int
                                                                              })])
                                                            (V183[2] V184)
                                                            (fun [V25, V378]
                                                              (V25 V11 V378))
                                                            V379))}))>)))))>)>), (fun
                                  [V8
                                  ,V24]
                                  <0: (fun [V9, V23]
                                      <0: (fun [V10]
                                          (let (V155 (fun [V383] (V9 V10 V383)))
                                            (fun [V157]
                                              (case (V155 V157)
                                                (fun [V158] (V158 V10 V157))
                                                (fun [V162]
                                                  <1: (forall
                                                      [(3: Type) (4: Type) (5: Type)]
                                                      (let
                                                        (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                        {V164[0], V164[1], (fun
                                                          [V165
                                                          ,V382]
                                                          ((__mon_bind @ [Ty({7}), Ty(Int
                                                          -> (Mon {7} { Int
                                                                      , Int
                                                                      })), Ty({ Int
                                                                              , Int
                                                                              })])
                                                            (V164[2] V165)
                                                            (fun [V22, V381]
                                                              (V22 V10 V381))
                                                            V382))}))>)))))>)>)}}))
                            (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                            (fun [V33] <0: V121>)
                            V122))})>)
                  (fun [V125]
                    (case (V18[1][1] {} V0)
                      (fun [V139]
                        (V139
                          (fun [V126, V127]
                            ((__mon_prompt @ [Ty({1}), Ty({0}), Ty(Int), Ty(Int ->
                            (Mon {1} {Int, Int}))])
                              V20
                              (fun [V21]
                                (V1[0]
                                  V21
                                  {V20, {(fun [V11, V27]
                                    <0: (fun [V12, V26]
                                        <0: (fun [V13]
                                            (let (V174 (fun [V386] (V12 {} V386)))
                                              (fun [V176]
                                                (case (V174 V176)
                                                  (fun [V177] (V177 V11 V176))
                                                  (fun [V181]
                                                    <1: (forall
                                                        [(3: Type) (4: Type) (5: Type)]
                                                        (let
                                                          (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                          {V183[0], V183[1], (fun
                                                            [V184
                                                            ,V385]
                                                            ((__mon_bind @ [Ty({4}), Ty(Int
                                                            -> (Mon {4} { Int
                                                                        , Int
                                                                        })), Ty({ Int
                                                                                , Int
                                                                                })])
                                                              (V183[2] V184)
                                                              (fun [V25, V384]
                                                                (V25 V11 V384))
                                                              V385))}))>)))))>)>), (fun
                                    [V8
                                    ,V24]
                                    <0: (fun [V9, V23]
                                        <0: (fun [V10]
                                            (let (V155 (fun [V389] (V9 V10 V389)))
                                              (fun [V157]
                                                (case (V155 V157)
                                                  (fun [V158] (V158 V10 V157))
                                                  (fun [V162]
                                                    <1: (forall
                                                        [(3: Type) (4: Type) (5: Type)]
                                                        (let
                                                          (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                          {V164[0], V164[1], (fun
                                                            [V165
                                                            ,V388]
                                                            ((__mon_bind @ [Ty({4}), Ty(Int
                                                            -> (Mon {4} { Int
                                                                        , Int
                                                                        })), Ty({ Int
                                                                                , Int
                                                                                })])
                                                              (V164[2] V165)
                                                              (fun [V22, V387]
                                                                (V22 V10 V387))
                                                              V388))}))>)))))>)>)}}))
                              (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                              (fun [V33] <0: V126>)
                              V127))
                          V0))
                      (fun [V143]
                        <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                              (V145 (V143 @ [Ty(T2), Ty(T1), Ty(T0)]))
                              {V145[0], V145[1], (fun [V146, V397]
                                ((__mon_bind @ [Ty({4}), Ty((Int -> (Mon {4} Int ->
                                (Mon {4} {Int, Int}))) -> (Mon {4} Int -> (Mon {4} { Int
                                                                                   , Int
                                                                                   }))), Ty(Int
                                -> (Mon {4} {Int, Int}))])
                                  (V145[2] V146)
                                  (fun [V32, V396]
                                    (V32
                                      (fun [V126, V127]
                                        ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int
                                        -> (Mon {4} {Int, Int}))])
                                          V20
                                          (fun [V21]
                                            (V1[0]
                                              V21
                                              {V20, {(fun [V11, V27]
                                                <0: (fun [V12, V26]
                                                    <0: (fun [V13]
                                                        (let
                                                          (V174 (fun [V392]
                                                            (V12 {} V392)))
                                                          (fun [V176]
                                                            (case (V174 V176)
                                                              (fun [V177]
                                                                (V177 V11 V176))
                                                              (fun [V181]
                                                                <1: (forall
                                                                    [(3: Type) (4: Type) (5: Type)]
                                                                    (let
                                                                      (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                      {
                                                                      V183[0], V183[1], (fun
                                                                        [V184
                                                                        ,V391]
                                                                        ((__mon_bind @ [Ty({7}), Ty(Int
                                                                        ->
                                                                        (Mon {7} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                                                                          (V183[2] V184)
                                                                          (fun
                                                                            [V25
                                                                            ,V390]
                                                                            (V25
                                                                              V11
                                                                              V390))
                                                                          V391))
                                                                      }))>)))))>)>), (fun
                                                [V8
                                                ,V24]
                                                <0: (fun [V9, V23]
                                                    <0: (fun [V10]
                                                        (let
                                                          (V155 (fun [V395]
                                                            (V9 V10 V395)))
                                                          (fun [V157]
                                                            (case (V155 V157)
                                                              (fun [V158]
                                                                (V158 V10 V157))
                                                              (fun [V162]
                                                                <1: (forall
                                                                    [(3: Type) (4: Type) (5: Type)]
                                                                    (let
                                                                      (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                      {
                                                                      V164[0], V164[1], (fun
                                                                        [V165
                                                                        ,V394]
                                                                        ((__mon_bind @ [Ty({7}), Ty(Int
                                                                        ->
                                                                        (Mon {7} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                                                                          (V164[2] V165)
                                                                          (fun
                                                                            [V22
                                                                            ,V393]
                                                                            (V22
                                                                              V10
                                                                              V393))
                                                                          V394))
                                                                      }))>)))))>)>)}}))
                                          (fun [V14, V29]
                                            <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                          (fun [V33] <0: V126>)
                                          V127))
                                      V396))
                                  V397))}))>))))
                (fun [V46] (V46 825 V0))
                (fun [V50]
                  <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                        (V52 (V50 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V52[0], V52[1], (fun [V53, V399]
                          ((__mon_bind @ [Ty({4}), Ty(Int -> (Mon {4} { Int
                                                                      , Int
                                                                      })), Ty({ Int
                                                                              , Int
                                                                              })])
                            (V52[2] V53)
                            (fun [V34, V398] (V34 825 V398))
                            V399))}))>)))))"#]];
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
  #[ignore = "Fix inlining variables so they typecheck"]
  fn simplify_multi_effect_in_main() {
    let db = TestDatabase::default();

    let ir = simple_function(
      &db,
      "main",
      r#"
main = (with  {
  get = |x| |k| |s| k(s)(s),
  put = |x| |k| |s| k({})(x),
  return = |x| |s| { state = s, value = x },
} do (with {
  ask = |x| |k| k(16777215),
  return = |x| x,
} do w = Reader.ask({}); State.put(w)))(14).state
      "#,
    );

    let simple_ir = ir.item(&db);

    let pretty_ir = simple_ir.pretty_string(&db, 80);
    let expect = expect![[r#"
        (let
          [ (V36 ((__mon_generate_marker @ [Ty(Int -> {} {Int, {}})]) {}))
          , (V34 ((__mon_generate_marker @ [Ty({})]) {}))
          ]
          (case (case (case (case (case (__mon_eqm V34 V36)
                    (fun [V788]
                      <1: (forall [(4: Type) (5: Type) (6: Type)] {V36, (fun [V33, V294]
                            <0: (fun [V18]
                                (let (V330 (fun [V820] (V33 {} V820)))
                                  (fun [V332]
                                    (case (V330 V332)
                                      (fun [V333] (V333 16777215 V332))
                                      (fun [V337]
                                        <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                                              (V339 (V337 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                              {V339[0], V339[1], (fun [V340, V819]
                                                ((__mon_bind @ [Ty({}), Ty(Int ->
                                                (Mon {} {Int, {}})), Ty({Int, {}})])
                                                  (V339[2] V340)
                                                  (fun [V41, V818] (V41 16777215 V818))
                                                  V819))}))>)))))>), (fun [V792, V793]
                            ((__mon_prompt @ [Ty({ (Marker Int -> (Mon {} {Int, {}}))
                                                 , { Int -> (Mon {} ({} -> (Mon {} Int
                                                   -> (Mon {} {Int, {}}))) ->
                                                   (Mon {} Int -> (Mon {} {Int, {}})))
                                                   , {} -> (Mon {} (Int -> (Mon {} Int
                                                   -> (Mon {} {Int, {}}))) ->
                                                   (Mon {} Int -> (Mon {} {Int, {}})))
                                                   }
                                                 }), Ty({ { (Marker Int -> (Mon {} { Int
                                                                                   , {}
                                                                                   }))
                                                          , { Int -> (Mon {} ({} ->
                                                            (Mon {} Int -> (Mon {} { Int
                                                                                   , {}
                                                                                   })))
                                                            -> (Mon {} Int ->
                                                            (Mon {} {Int, {}})))
                                                            , {} -> (Mon {} (Int ->
                                                            (Mon {} Int -> (Mon {} { Int
                                                                                   , {}
                                                                                   })))
                                                            -> (Mon {} Int ->
                                                            (Mon {} {Int, {}})))
                                                            }
                                                          }
                                                        , { (Marker {})
                                                          , {} -> (Mon { (Marker Int ->
                                                                       (Mon {} { Int
                                                                               , {}
                                                                               }))
                                                                       , { Int ->
                                                                         (Mon {} ({} ->
                                                                         (Mon {} Int ->
                                                                         (Mon {} { Int
                                                                                 , {}
                                                                                 }))) ->
                                                                         (Mon {} Int ->
                                                                         (Mon {} { Int
                                                                                 , {}
                                                                                 })))
                                                                         , {} ->
                                                                         (Mon {} (Int ->
                                                                         (Mon {} Int ->
                                                                         (Mon {} { Int
                                                                                 , {}
                                                                                 }))) ->
                                                                         (Mon {} Int ->
                                                                         (Mon {} { Int
                                                                                 , {}
                                                                                 })))
                                                                         }
                                                                       } (Int ->
                                                          (Mon { (Marker Int ->
                                                               (Mon {} {Int, {}}))
                                                               , { Int -> (Mon {} ({} ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}}))) ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}})))
                                                                 , {} -> (Mon {} (Int ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}}))) ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}})))
                                                                 }
                                                               } {})) ->
                                                          (Mon { (Marker Int ->
                                                               (Mon {} {Int, {}}))
                                                               , { Int -> (Mon {} ({} ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}}))) ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}})))
                                                                 , {} -> (Mon {} (Int ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}}))) ->
                                                                 (Mon {} Int ->
                                                                 (Mon {} {Int, {}})))
                                                                 }
                                                               } {}))
                                                          }
                                                        }), Ty({}), Ty({})])
                              V34
                              (fun [V35]
                                {V35, {V34, (fun [V23, V46]
                                  <0: (fun [V24, V821] (V24 16777215 V821))>)}})
                              (fun [V25, V47] <0: V25>)
                              (fun [V51] <0: V792>)
                              V793))})>)
                    (fun [V796]
                      <0: (fun [V18, V332]
                          (case ((__mon_prompt @ [Ty({ (Marker Int -> (Mon {} { Int
                                                                              , {}
                                                                              }))
                                                     , { Int -> (Mon {} ({} ->
                                                       (Mon {} Int -> (Mon {} { Int
                                                                              , {}
                                                                              }))) ->
                                                       (Mon {} Int -> (Mon {} { Int
                                                                              , {}
                                                                              })))
                                                       , {} -> (Mon {} (Int ->
                                                       (Mon {} Int -> (Mon {} { Int
                                                                              , {}
                                                                              }))) ->
                                                       (Mon {} Int -> (Mon {} { Int
                                                                              , {}
                                                                              })))
                                                       }
                                                     }), Ty({ { (Marker Int ->
                                                              (Mon {} {Int, {}}))
                                                              , { Int -> (Mon {} ({} ->
                                                                (Mon {} Int ->
                                                                (Mon {} {Int, {}}))) ->
                                                                (Mon {} Int ->
                                                                (Mon {} {Int, {}})))
                                                                , {} -> (Mon {} (Int ->
                                                                (Mon {} Int ->
                                                                (Mon {} {Int, {}}))) ->
                                                                (Mon {} Int ->
                                                                (Mon {} {Int, {}})))
                                                                }
                                                              }
                                                            , { (Marker {})
                                                              , {} -> (Mon { (Marker Int
                                                                           ->
                                                                           (Mon {} { Int
                                                                                   , {}
                                                                                   }))
                                                                           , { Int ->
                                                                             (Mon {} ({}
                                                                             ->
                                                                             (Mon {} Int
                                                                             ->
                                                                             (Mon {} { Int
                                                                                     , {}
                                                                                     })))
                                                                             ->
                                                                             (Mon {} Int
                                                                             ->
                                                                             (Mon {} { Int
                                                                                     , {}
                                                                                     })))
                                                                             , {} ->
                                                                             (Mon {} (Int
                                                                             ->
                                                                             (Mon {} Int
                                                                             ->
                                                                             (Mon {} { Int
                                                                                     , {}
                                                                                     })))
                                                                             ->
                                                                             (Mon {} Int
                                                                             ->
                                                                             (Mon {} { Int
                                                                                     , {}
                                                                                     })))
                                                                             }
                                                                           } (Int ->
                                                              (Mon { (Marker Int ->
                                                                   (Mon {} {Int, {}}))
                                                                   , { Int ->
                                                                     (Mon {} ({} ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             }))) ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             })))
                                                                     , {} ->
                                                                     (Mon {} (Int ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             }))) ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             })))
                                                                     }
                                                                   } {})) ->
                                                              (Mon { (Marker Int ->
                                                                   (Mon {} {Int, {}}))
                                                                   , { Int ->
                                                                     (Mon {} ({} ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             }))) ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             })))
                                                                     , {} ->
                                                                     (Mon {} (Int ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             }))) ->
                                                                     (Mon {} Int ->
                                                                     (Mon {} { Int
                                                                             , {}
                                                                             })))
                                                                     }
                                                                   } {}))
                                                              }
                                                            }), Ty({}), Ty({})])
                              V34
                              (fun [V35]
                                {V35, {V34, (fun [V23, V46]
                                  <0: (fun [V24, V822] (V24 16777215 V822))>)}})
                              (fun [V25, V47] <0: V25>)
                              (fun [V51] <0: {}>)
                              V332)
                            (fun [V333] (V333 16777215 V332))
                            (fun [V337]
                              <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                                    (V339 (V337 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                    {V339[0], V339[1], (fun [V340, V824]
                                      ((__mon_bind @ [Ty({}), Ty(Int -> (Mon {} { Int
                                                                                , {}
                                                                                })), Ty({ Int
                                                                                        , {}
                                                                                        })])
                                        (V339[2] V340)
                                        (fun [V41, V823] (V41 16777215 V823))
                                        V824))}))>)))>))
                  (fun [V138] <0: (fun [V20, V44] <0: {V20, V138}>)>)
                  (fun [V155]
                    (case (__mon_eqm
                        V36
                        (V155 @ [Ty({}), Ty({}), Ty(Int -> (Mon {} {Int, {}}))])[0])
                      (fun [V161]
                        <1: (forall [(4: Type) (5: Type) (6: Type)] (let
                              (V164 (V155 @ [Ty(T2), Ty(T1), Ty(T0)]))
                              {V164[0], V164[1], (fun [V165, V166]
                                ((__mon_prompt @ [Ty({}), Ty({ (Marker Int ->
                                                             (Mon {} {Int, {}}))
                                                             , { Int -> (Mon {} ({} ->
                                                               (Mon {} Int ->
                                                               (Mon {} {Int, {}}))) ->
                                                               (Mon {} Int ->
                                                               (Mon {} {Int, {}})))
                                                               , {} -> (Mon {} (Int ->
                                                               (Mon {} Int ->
                                                               (Mon {} {Int, {}}))) ->
                                                               (Mon {} Int ->
                                                               (Mon {} {Int, {}})))
                                                               }
                                                             }), Ty({}), Ty(Int ->
                                (Mon {} {Int, {}}))])
                                  V36
                                  (fun [V37]
                                    {V36, {(fun [V16, V43]
                                      <0: (fun [V17, V42]
                                          <0: (fun [V18]
                                              (let (V330 (fun [V827] (V17 {} V827)))
                                                (fun [V332]
                                                  (case (V330 V332)
                                                    (fun [V333] (V333 V16 V332))
                                                    (fun [V337]
                                                      <1: (forall
                                                          [(3: Type) (4: Type) (5: Type)]
                                                          (let
                                                            (V339 (V337 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                            {V339[0], V339[1], (fun
                                                              [V340
                                                              ,V826]
                                                              ((__mon_bind @ [Ty({}), Ty(Int
                                                              -> (Mon {} { Int
                                                                         , {}
                                                                         })), Ty({ Int
                                                                                 , {}
                                                                                 })])
                                                                (V339[2] V340)
                                                                (fun [V41, V825]
                                                                  (V41 V16 V825))
                                                                V826))
                                                            }))>)))))>)>), (fun
                                      [V13
                                      ,V40]
                                      <0: (fun [V14, V39]
                                          <0: (fun [V15]
                                              (let (V311 (fun [V830] (V14 V15 V830)))
                                                (fun [V313]
                                                  (case (V311 V313)
                                                    (fun [V314] (V314 V15 V313))
                                                    (fun [V318]
                                                      <1: (forall
                                                          [(3: Type) (4: Type) (5: Type)]
                                                          (let
                                                            (V320 (V318 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                            {V320[0], V320[1], (fun
                                                              [V321
                                                              ,V829]
                                                              ((__mon_bind @ [Ty({}), Ty(Int
                                                              -> (Mon {} { Int
                                                                         , {}
                                                                         })), Ty({ Int
                                                                                 , {}
                                                                                 })])
                                                                (V320[2] V321)
                                                                (fun [V38, V828]
                                                                  (V38 V15 V828))
                                                                V829))}))>)))))>)>)}})
                                  (fun [V19, V45] <0: (fun [V20, V44] <0: {V20, V19}>)>)
                                  (V164[2] V165)
                                  V166))}))>)
                      (fun [V169]
                        ((V155 @ [Ty({}), Ty({}), Ty(Int -> (Mon {} {Int, {}}))])[1]
                          (fun [V170, V171]
                            ((__mon_prompt @ [Ty({}), Ty({ (Marker Int -> (Mon {} { Int
                                                                                  , {}
                                                                                  }))
                                                         , { Int -> (Mon {} ({} ->
                                                           (Mon {} Int -> (Mon {} { Int
                                                                                  , {}
                                                                                  })))
                                                           -> (Mon {} Int ->
                                                           (Mon {} {Int, {}})))
                                                           , {} -> (Mon {} (Int ->
                                                           (Mon {} Int -> (Mon {} { Int
                                                                                  , {}
                                                                                  })))
                                                           -> (Mon {} Int ->
                                                           (Mon {} {Int, {}})))
                                                           }
                                                         }), Ty({}), Ty(Int ->
                            (Mon {} {Int, {}}))])
                              V36
                              (fun [V37]
                                {V36, {(fun [V16, V43]
                                  <0: (fun [V17, V42]
                                      <0: (fun [V18]
                                          (let (V330 (fun [V833] (V17 {} V833)))
                                            (fun [V332]
                                              (case (V330 V332)
                                                (fun [V333] (V333 V16 V332))
                                                (fun [V337]
                                                  <1: (forall
                                                      [(3: Type) (4: Type) (5: Type)]
                                                      (let
                                                        (V339 (V337 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                        {V339[0], V339[1], (fun
                                                          [V340
                                                          ,V832]
                                                          ((__mon_bind @ [Ty({}), Ty(Int
                                                          -> (Mon {} { Int
                                                                     , {}
                                                                     })), Ty({ Int
                                                                             , {}
                                                                             })])
                                                            (V339[2] V340)
                                                            (fun [V41, V831]
                                                              (V41 V16 V831))
                                                            V832))}))>)))))>)>), (fun
                                  [V13
                                  ,V40]
                                  <0: (fun [V14, V39]
                                      <0: (fun [V15]
                                          (let (V311 (fun [V836] (V14 V15 V836)))
                                            (fun [V313]
                                              (case (V311 V313)
                                                (fun [V314] (V314 V15 V313))
                                                (fun [V318]
                                                  <1: (forall
                                                      [(3: Type) (4: Type) (5: Type)]
                                                      (let
                                                        (V320 (V318 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                        {V320[0], V320[1], (fun
                                                          [V321
                                                          ,V835]
                                                          ((__mon_bind @ [Ty({}), Ty(Int
                                                          -> (Mon {} { Int
                                                                     , {}
                                                                     })), Ty({ Int
                                                                             , {}
                                                                             })])
                                                            (V320[2] V321)
                                                            (fun [V38, V834]
                                                              (V38 V15 V834))
                                                            V835))}))>)))))>)>)}})
                              (fun [V19, V45] <0: (fun [V20, V44] <0: {V20, V19}>)>)
                              ((V155 @ [Ty({}), Ty({}), Ty(Int -> (Mon {} { Int
                                                                          , {}
                                                                          }))])[2] V170)
                              V171))
                          {})))))
                (fun [V90] (V90 14 {}))
                (fun [V94]
                  <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                        (V96 (V94 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V96[0], V96[1], (fun [V97, V838]
                          ((__mon_bind @ [Ty({}), Ty(Int -> (Mon {} { Int
                                                                    , {}
                                                                    })), Ty({Int, {}})])
                            (V96[2] V97)
                            (fun [V57, V837] (V57 14 V837))
                            V838))}))>))
              (fun [V71] <0: V71[1]>)
              (fun [V75]
                <1: (forall [(3: Type) (4: Type) (5: Type)] (let
                      (V77 (V75 @ [Ty(T2), Ty(T1), Ty(T0)]))
                      {V77[0], V77[1], (fun [V78, V839]
                        ((__mon_bind @ [Ty({}), Ty({Int, {}}), Ty({})])
                          (V77[2] V78)
                          (fun [V58, V59] <0: V58[1]>)
                          V839))}))>))
            (fun [V0] V0)
            (fun [V0] 5467)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_string(&db, 80));
  }
}
