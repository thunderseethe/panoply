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
              (V43 (let (V20 ((__mon_generate_marker @ [Ty(Int -> {1} {Int, Int})]) {}))
                (fun [V93]
                  (let
                    (V18 (V1[3][0]
                      (V1[0]
                        V93
                        {V20, {(fun [V11, V27]
                          <0: (fun [V12, V26]
                              <0: (fun [V13]
                                  (let (V174 (V12 {}))
                                    (fun [V176]
                                      (case (V174 V176)
                                        (fun [V177] (V177 V11 V176))
                                        (fun [V181]
                                          <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                              (let
                                                (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                {V183[0], V183[1], (fun [V184, V385]
                                                  ((__mon_bind @ [Ty({4}), Ty(Int -> {4}
                                                  -> (Control {4} {Int, Int})), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                    (V183[2] V184)
                                                    (fun [V25] (V25 V11))
                                                    V385))}))>)))))>)>), (fun [V8, V24]
                          <0: (fun [V9, V23]
                              <0: (fun [V10]
                                  (let (V155 (V9 V10))
                                    (fun [V157]
                                      (case (V155 V157)
                                        (fun [V158] (V158 V10 V157))
                                        (fun [V162]
                                          <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                              (let
                                                (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                {V164[0], V164[1], (fun [V165, V386]
                                                  ((__mon_bind @ [Ty({4}), Ty(Int -> {4}
                                                  -> (Control {4} {Int, Int})), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                    (V164[2] V165)
                                                    (fun [V22] (V22 V10))
                                                    V386))}))>)))))>)>)}})))
                    (case (__mon_eqm V20 V18[0])
                      (fun [V117]
                        <1: (forall [(T4: Type) (T5: Type) (T6: Type)] {V18[0], (fun
                              [V19]
                              (let (V136 (V18[1][1] {}))
                                (fun [V138]
                                  (case (V136 V138)
                                    (fun [V139] (V139 V19 V138))
                                    (fun [V143]
                                      <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                          (let (V145 (V143 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                            {V145[0], V145[1], (fun [V146, V377]
                                              ((__mon_bind @ [Ty({7}), Ty((Int -> {7} ->
                                              (Control {7} Int -> {7} ->
                                              (Control {7} {Int, Int}))) -> {7} ->
                                              (Control {7} Int -> {7} ->
                                              (Control {7} {Int, Int}))), Ty(Int -> {7}
                                              -> (Control {7} {Int, Int}))])
                                                (V145[2] V146)
                                                (fun [V32] (V32 V19))
                                                V377))}))>))))), (fun [V121, V122]
                              ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int -> {4}
                              -> (Control {4} {Int, Int}))])
                                V20
                                (fun [V21]
                                  (V1[0]
                                    V21
                                    {V20, {(fun [V11, V27]
                                      <0: (fun [V12, V26]
                                          <0: (fun [V13]
                                              (let (V174 (V12 {}))
                                                (fun [V176]
                                                  (case (V174 V176)
                                                    (fun [V177] (V177 V11 V176))
                                                    (fun [V181]
                                                      <1: (forall
                                                          [(T3: Type) (T4: Type) (T5: Type)]
                                                          (let
                                                            (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                            {V183[0], V183[1], (fun
                                                              [V184
                                                              ,V378]
                                                              ((__mon_bind @ [Ty({7}), Ty(Int
                                                              -> {7} ->
                                                              (Control {7} { Int
                                                                           , Int
                                                                           })), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                                (V183[2] V184)
                                                                (fun [V25] (V25 V11))
                                                                V378))
                                                            }))>)))))>)>), (fun
                                      [V8
                                      ,V24]
                                      <0: (fun [V9, V23]
                                          <0: (fun [V10]
                                              (let (V155 (V9 V10))
                                                (fun [V157]
                                                  (case (V155 V157)
                                                    (fun [V158] (V158 V10 V157))
                                                    (fun [V162]
                                                      <1: (forall
                                                          [(T3: Type) (T4: Type) (T5: Type)]
                                                          (let
                                                            (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                            {V164[0], V164[1], (fun
                                                              [V165
                                                              ,V379]
                                                              ((__mon_bind @ [Ty({7}), Ty(Int
                                                              -> {7} ->
                                                              (Control {7} { Int
                                                                           , Int
                                                                           })), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                                (V164[2] V165)
                                                                (fun [V22] (V22 V10))
                                                                V379))}))>)))))>)>)}}))
                                (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                (fun [V33] <0: V121>)
                                V122))})>)
                      (fun [V125]
                        (case (V18[1][1] {} V93)
                          (fun [V139]
                            (V139
                              (fun [V126, V127]
                                ((__mon_prompt @ [Ty({1}), Ty({0}), Ty(Int), Ty(Int ->
                                {1} -> (Control {1} {Int, Int}))])
                                  V20
                                  (fun [V21]
                                    (V1[0]
                                      V21
                                      {V20, {(fun [V11, V27]
                                        <0: (fun [V12, V26]
                                            <0: (fun [V13]
                                                (let (V174 (V12 {}))
                                                  (fun [V176]
                                                    (case (V174 V176)
                                                      (fun [V177] (V177 V11 V176))
                                                      (fun [V181]
                                                        <1: (forall
                                                            [(T3: Type) (T4: Type) (T5: Type)]
                                                            (let
                                                              (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                              {V183[0], V183[1], (fun
                                                                [V184
                                                                ,V380]
                                                                ((__mon_bind @ [Ty({4}), Ty(Int
                                                                -> {4} ->
                                                                (Control {4} { Int
                                                                             , Int
                                                                             })), Ty({ Int
                                                                                     , Int
                                                                                     })])
                                                                  (V183[2] V184)
                                                                  (fun [V25] (V25 V11))
                                                                  V380))
                                                              }))>)))))>)>), (fun
                                        [V8
                                        ,V24]
                                        <0: (fun [V9, V23]
                                            <0: (fun [V10]
                                                (let (V155 (V9 V10))
                                                  (fun [V157]
                                                    (case (V155 V157)
                                                      (fun [V158] (V158 V10 V157))
                                                      (fun [V162]
                                                        <1: (forall
                                                            [(T3: Type) (T4: Type) (T5: Type)]
                                                            (let
                                                              (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                              {V164[0], V164[1], (fun
                                                                [V165
                                                                ,V381]
                                                                ((__mon_bind @ [Ty({4}), Ty(Int
                                                                -> {4} ->
                                                                (Control {4} { Int
                                                                             , Int
                                                                             })), Ty({ Int
                                                                                     , Int
                                                                                     })])
                                                                  (V164[2] V165)
                                                                  (fun [V22] (V22 V10))
                                                                  V381))}))>)))))>)>)}
                                      }))
                                  (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                  (fun [V33] <0: V126>)
                                  V127))
                              V93))
                          (fun [V143]
                            <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                                  (V145 (V143 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                  {V145[0], V145[1], (fun [V146, V384]
                                    ((__mon_bind @ [Ty({4}), Ty((Int -> {4} ->
                                    (Control {4} Int -> {4} -> (Control {4} { Int
                                                                            , Int
                                                                            }))) -> {4}
                                    -> (Control {4} Int -> {4} -> (Control {4} { Int
                                                                               , Int
                                                                               }))), Ty(Int
                                    -> {4} -> (Control {4} {Int, Int}))])
                                      (V145[2] V146)
                                      (fun [V32]
                                        (V32
                                          (fun [V126, V127]
                                            ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int
                                            -> {4} -> (Control {4} {Int, Int}))])
                                              V20
                                              (fun [V21]
                                                (V1[0]
                                                  V21
                                                  {V20, {(fun [V11, V27]
                                                    <0: (fun [V12, V26]
                                                        <0: (fun [V13]
                                                            (let (V174 (V12 {}))
                                                              (fun [V176]
                                                                (case (V174 V176)
                                                                  (fun [V177]
                                                                    (V177 V11 V176))
                                                                  (fun [V181]
                                                                    <1: (forall
                                                                        [(T3: Type) (T4: Type) (T5: Type)]
                                                                        (let
                                                                          (V183 (V181 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                          {
                                                                          V183[0], V183[1], (fun
                                                                            [V184
                                                                            ,V382]
                                                                            ((__mon_bind @ [Ty({7}), Ty(Int
                                                                            -> {7} ->
                                                                            (Control {7} { Int
                                                                                         , Int
                                                                                         })), Ty({ Int
                                                                                                 , Int
                                                                                                 })])
                                                                              (V183[2]
                                                                                V184)
                                                                              (fun [V25]
                                                                                (V25
                                                                                  V11))
                                                                              V382))
                                                                          }))>)))))>)>), (fun
                                                    [V8
                                                    ,V24]
                                                    <0: (fun [V9, V23]
                                                        <0: (fun [V10]
                                                            (let (V155 (V9 V10))
                                                              (fun [V157]
                                                                (case (V155 V157)
                                                                  (fun [V158]
                                                                    (V158 V10 V157))
                                                                  (fun [V162]
                                                                    <1: (forall
                                                                        [(T3: Type) (T4: Type) (T5: Type)]
                                                                        (let
                                                                          (V164 (V162 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                          {
                                                                          V164[0], V164[1], (fun
                                                                            [V165
                                                                            ,V383]
                                                                            ((__mon_bind @ [Ty({7}), Ty(Int
                                                                            -> {7} ->
                                                                            (Control {7} { Int
                                                                                         , Int
                                                                                         })), Ty({ Int
                                                                                                 , Int
                                                                                                 })])
                                                                              (V164[2]
                                                                                V165)
                                                                              (fun [V22]
                                                                                (V22
                                                                                  V10))
                                                                              V383))
                                                                          }))>)))))>)>)}
                                                  }))
                                              (fun [V14, V29]
                                                <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                              (fun [V33] <0: V126>)
                                              V127))))
                                      V384))}))>))))))))
              (case (V43 V0)
                (fun [V46] (V46 825 V0))
                (fun [V50]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V52 (V50 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V52[0], V52[1], (fun [V53, V376]
                          ((__mon_bind @ [Ty({4}), Ty(Int -> {4} -> (Control {4} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                            (V52[2] V53)
                            (fun [V34] (V34 825))
                            V376))}))>)))))"#]];
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
        (let
          (V45 (let
            (V64 (let (V20 ((__mon_generate_marker @ [Ty(Int -> {} {Int, Int})]) {}))
              (fun [V114]
                <0: (fun [V10, V178]
                    (case ((__mon_prompt @ [Ty({}), Ty({ (Marker Int -> {} ->
                                                       (Control {} {Int, Int}))
                                                       , { Int -> {} -> (Control {} ({}
                                                         -> {} -> (Control {} Int -> {}
                                                         -> (Control {} {Int, Int}))) ->
                                                         {} -> (Control {} Int -> {} ->
                                                         (Control {} {Int, Int})))
                                                         , {} -> {} -> (Control {} (Int
                                                         -> {} -> (Control {} Int -> {}
                                                         -> (Control {} {Int, Int}))) ->
                                                         {} -> (Control {} Int -> {} ->
                                                         (Control {} {Int, Int})))
                                                         }
                                                       }), Ty(Int), Ty(Int -> {} ->
                      (Control {} {Int, Int}))])
                        V20
                        (fun [V21]
                          {V20, {(fun [V11, V27]
                            <0: (fun [V12, V26]
                                <0: (fun [V13]
                                    (let (V195 (V12 {}))
                                      (fun [V197]
                                        (case (V195 V197)
                                          (fun [V198] (V198 V11 V197))
                                          (fun [V202]
                                            <1: (forall
                                                [(T3: Type) (T4: Type) (T5: Type)] (let
                                                  (V204 (V202 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                  {V204[0], V204[1], (fun [V205, V426]
                                                    ((__mon_bind @ [Ty({}), Ty(Int -> {}
                                                    -> (Control {} { Int
                                                                   , Int
                                                                   })), Ty({Int, Int})])
                                                      (V204[2] V205)
                                                      (fun [V25] (V25 V11))
                                                      V426))}))>)))))>)>), (fun
                            [V8
                            ,V24]
                            <0: (fun [V9, V23]
                                <0: (fun [V10]
                                    (let (V176 (V9 V10))
                                      (fun [V178]
                                        (case (V176 V178)
                                          (fun [V179] (V179 V10 V178))
                                          (fun [V183]
                                            <1: (forall
                                                [(T3: Type) (T4: Type) (T5: Type)] (let
                                                  (V185 (V183 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                  {V185[0], V185[1], (fun [V186, V427]
                                                    ((__mon_bind @ [Ty({}), Ty(Int -> {}
                                                    -> (Control {} { Int
                                                                   , Int
                                                                   })), Ty({Int, Int})])
                                                      (V185[2] V186)
                                                      (fun [V22] (V22 V10))
                                                      V427))}))>)))))>)>)}})
                        (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                        (fun [V33] <0: V10>)
                        V178)
                      (fun [V179] (V179 V10 V178))
                      (fun [V183]
                        <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                              (V185 (V183 @ [Ty(T2), Ty(T1), Ty(T0)]))
                              {V185[0], V185[1], (fun [V186, V428]
                                ((__mon_bind @ [Ty({}), Ty(Int -> {} ->
                                (Control {} {Int, Int})), Ty({Int, Int})])
                                  (V185[2] V186)
                                  (fun [V22] (V22 V10))
                                  V428))}))>)))>)))
            (fun [V66]
              (case (V64 V66)
                (fun [V67] (V67 825 V66))
                (fun [V71]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V73 (V71 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V73[0], V73[1], (fun [V74, V425]
                          ((__mon_bind @ [Ty({}), Ty(Int -> {} -> (Control {} { Int
                                                                              , Int
                                                                              })), Ty({ Int
                                                                                      , Int
                                                                                      })])
                            (V73[2] V74)
                            (fun [V34] (V34 825))
                            V425))}))>)))))
          (case (case (V45 {})
              (fun [V48] <0: V48[1]>)
              (fun [V52]
                <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                      (V54 (V52 @ [Ty(T2), Ty(T1), Ty(T0)]))
                      {V54[0], V54[1], (fun [V55, V424]
                        ((__mon_bind @ [Ty({}), Ty({Int, Int}), Ty(Int)])
                          (V54[2] V55)
                          (fun [V35, V36] <0: V35[1]>)
                          V424))}))>))
            (fun [V0] V0)
            (fun [V0] 5467)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }
}
