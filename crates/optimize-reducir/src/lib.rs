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
              (V0 (let (V20 ((__mon_generate_marker @ [Ty(Int -> {1} {Int, Int})]) {}))
                (fun [V0]
                  (let
                    (V18 (V1[3][0]
                      (V1[0]
                        V0
                        {V20, {(fun [V11, V27]
                          <0: (fun [V12, V26]
                              <0: (fun [V13]
                                  (let (V0 (V12 {}))
                                    (fun [V170]
                                      (case (V0 V170)
                                        (fun [V171] (V171 V11 V170))
                                        (fun [V175]
                                          <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                              (let
                                                (V177 (V175 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                {V177[0], V177[1], (fun [V178, V344]
                                                  ((__mon_bind @ [Ty({4}), Ty(Int -> {4}
                                                  -> (Control {4} {Int, Int})), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                    (V177[2] V178)
                                                    (fun [V25] (V25 V11))
                                                    V344))}))>)))))>)>), (fun [V8, V24]
                          <0: (fun [V9, V23]
                              <0: (fun [V10]
                                  (let (V0 (V9 V10))
                                    (fun [V152]
                                      (case (V0 V152)
                                        (fun [V153] (V153 V10 V152))
                                        (fun [V157]
                                          <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                              (let
                                                (V159 (V157 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                {V159[0], V159[1], (fun [V160, V345]
                                                  ((__mon_bind @ [Ty({4}), Ty(Int -> {4}
                                                  -> (Control {4} {Int, Int})), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                    (V159[2] V160)
                                                    (fun [V22] (V22 V10))
                                                    V345))}))>)))))>)>)}})))
                    (case (__mon_eqm V20 V18[0])
                      (fun [V114]
                        <1: (forall [(T4: Type) (T5: Type) (T6: Type)] {V18[0], (fun
                              [V19]
                              (let (V0 (V18[1][1] {}))
                                (fun [V134]
                                  (case (V0 V134)
                                    (fun [V135] (V135 V19 V134))
                                    (fun [V139]
                                      <1: (forall [(T3: Type) (T4: Type) (T5: Type)]
                                          (let (V141 (V139 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                            {V141[0], V141[1], (fun [V142, V336]
                                              ((__mon_bind @ [Ty({7}), Ty((Int -> {7} ->
                                              (Control {7} Int -> {7} ->
                                              (Control {7} {Int, Int}))) -> {7} ->
                                              (Control {7} Int -> {7} ->
                                              (Control {7} {Int, Int}))), Ty(Int -> {7}
                                              -> (Control {7} {Int, Int}))])
                                                (V141[2] V142)
                                                (fun [V32] (V32 V19))
                                                V336))}))>))))), (fun [V118, V119]
                              ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int -> {4}
                              -> (Control {4} {Int, Int}))])
                                V20
                                (fun [V21]
                                  (V1[0]
                                    V21
                                    {V20, {(fun [V11, V27]
                                      <0: (fun [V12, V26]
                                          <0: (fun [V13]
                                              (let (V0 (V12 {}))
                                                (fun [V170]
                                                  (case (V0 V170)
                                                    (fun [V171] (V171 V11 V170))
                                                    (fun [V175]
                                                      <1: (forall
                                                          [(T3: Type) (T4: Type) (T5: Type)]
                                                          (let
                                                            (V177 (V175 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                            {V177[0], V177[1], (fun
                                                              [V178
                                                              ,V337]
                                                              ((__mon_bind @ [Ty({7}), Ty(Int
                                                              -> {7} ->
                                                              (Control {7} { Int
                                                                           , Int
                                                                           })), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                                (V177[2] V178)
                                                                (fun [V25] (V25 V11))
                                                                V337))
                                                            }))>)))))>)>), (fun
                                      [V8
                                      ,V24]
                                      <0: (fun [V9, V23]
                                          <0: (fun [V10]
                                              (let (V0 (V9 V10))
                                                (fun [V152]
                                                  (case (V0 V152)
                                                    (fun [V153] (V153 V10 V152))
                                                    (fun [V157]
                                                      <1: (forall
                                                          [(T3: Type) (T4: Type) (T5: Type)]
                                                          (let
                                                            (V159 (V157 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                            {V159[0], V159[1], (fun
                                                              [V160
                                                              ,V338]
                                                              ((__mon_bind @ [Ty({7}), Ty(Int
                                                              -> {7} ->
                                                              (Control {7} { Int
                                                                           , Int
                                                                           })), Ty({ Int
                                                                                   , Int
                                                                                   })])
                                                                (V159[2] V160)
                                                                (fun [V22] (V22 V10))
                                                                V338))}))>)))))>)>)}}))
                                (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                (fun [V33] <0: V118>)
                                V119))})>)
                      (fun [V122]
                        (case (V18[1][1] {} V0)
                          (fun [V135]
                            (V135
                              (fun [V123, V124]
                                ((__mon_prompt @ [Ty({1}), Ty({0}), Ty(Int), Ty(Int ->
                                {1} -> (Control {1} {Int, Int}))])
                                  V20
                                  (fun [V21]
                                    (V1[0]
                                      V21
                                      {V20, {(fun [V11, V27]
                                        <0: (fun [V12, V26]
                                            <0: (fun [V13]
                                                (let (V0 (V12 {}))
                                                  (fun [V170]
                                                    (case (V0 V170)
                                                      (fun [V171] (V171 V11 V170))
                                                      (fun [V175]
                                                        <1: (forall
                                                            [(T3: Type) (T4: Type) (T5: Type)]
                                                            (let
                                                              (V177 (V175 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                              {V177[0], V177[1], (fun
                                                                [V178
                                                                ,V339]
                                                                ((__mon_bind @ [Ty({4}), Ty(Int
                                                                -> {4} ->
                                                                (Control {4} { Int
                                                                             , Int
                                                                             })), Ty({ Int
                                                                                     , Int
                                                                                     })])
                                                                  (V177[2] V178)
                                                                  (fun [V25] (V25 V11))
                                                                  V339))
                                                              }))>)))))>)>), (fun
                                        [V8
                                        ,V24]
                                        <0: (fun [V9, V23]
                                            <0: (fun [V10]
                                                (let (V0 (V9 V10))
                                                  (fun [V152]
                                                    (case (V0 V152)
                                                      (fun [V153] (V153 V10 V152))
                                                      (fun [V157]
                                                        <1: (forall
                                                            [(T3: Type) (T4: Type) (T5: Type)]
                                                            (let
                                                              (V159 (V157 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                              {V159[0], V159[1], (fun
                                                                [V160
                                                                ,V340]
                                                                ((__mon_bind @ [Ty({4}), Ty(Int
                                                                -> {4} ->
                                                                (Control {4} { Int
                                                                             , Int
                                                                             })), Ty({ Int
                                                                                     , Int
                                                                                     })])
                                                                  (V159[2] V160)
                                                                  (fun [V22] (V22 V10))
                                                                  V340))}))>)))))>)>)}
                                      }))
                                  (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                  (fun [V33] <0: V123>)
                                  V124))
                              V0))
                          (fun [V139]
                            <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                                  (V141 (V139 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                  {V141[0], V141[1], (fun [V142, V343]
                                    ((__mon_bind @ [Ty({4}), Ty((Int -> {4} ->
                                    (Control {4} Int -> {4} -> (Control {4} { Int
                                                                            , Int
                                                                            }))) -> {4}
                                    -> (Control {4} Int -> {4} -> (Control {4} { Int
                                                                               , Int
                                                                               }))), Ty(Int
                                    -> {4} -> (Control {4} {Int, Int}))])
                                      (V141[2] V142)
                                      (fun [V32]
                                        (V32
                                          (fun [V123, V124]
                                            ((__mon_prompt @ [Ty({4}), Ty({3}), Ty(Int), Ty(Int
                                            -> {4} -> (Control {4} {Int, Int}))])
                                              V20
                                              (fun [V21]
                                                (V1[0]
                                                  V21
                                                  {V20, {(fun [V11, V27]
                                                    <0: (fun [V12, V26]
                                                        <0: (fun [V13]
                                                            (let (V0 (V12 {}))
                                                              (fun [V170]
                                                                (case (V0 V170)
                                                                  (fun [V171]
                                                                    (V171 V11 V170))
                                                                  (fun [V175]
                                                                    <1: (forall
                                                                        [(T3: Type) (T4: Type) (T5: Type)]
                                                                        (let
                                                                          (V177 (V175 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                          {
                                                                          V177[0], V177[1], (fun
                                                                            [V178
                                                                            ,V341]
                                                                            ((__mon_bind @ [Ty({7}), Ty(Int
                                                                            -> {7} ->
                                                                            (Control {7} { Int
                                                                                         , Int
                                                                                         })), Ty({ Int
                                                                                                 , Int
                                                                                                 })])
                                                                              (V177[2]
                                                                                V178)
                                                                              (fun [V25]
                                                                                (V25
                                                                                  V11))
                                                                              V341))
                                                                          }))>)))))>)>), (fun
                                                    [V8
                                                    ,V24]
                                                    <0: (fun [V9, V23]
                                                        <0: (fun [V10]
                                                            (let (V0 (V9 V10))
                                                              (fun [V152]
                                                                (case (V0 V152)
                                                                  (fun [V153]
                                                                    (V153 V10 V152))
                                                                  (fun [V157]
                                                                    <1: (forall
                                                                        [(T3: Type) (T4: Type) (T5: Type)]
                                                                        (let
                                                                          (V159 (V157 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                                          {
                                                                          V159[0], V159[1], (fun
                                                                            [V160
                                                                            ,V342]
                                                                            ((__mon_bind @ [Ty({7}), Ty(Int
                                                                            -> {7} ->
                                                                            (Control {7} { Int
                                                                                         , Int
                                                                                         })), Ty({ Int
                                                                                                 , Int
                                                                                                 })])
                                                                              (V159[2]
                                                                                V160)
                                                                              (fun [V22]
                                                                                (V22
                                                                                  V10))
                                                                              V342))
                                                                          }))>)))))>)>)}
                                                  }))
                                              (fun [V14, V29]
                                                <0: (fun [V15, V28] <0: {V15, V14}>)>)
                                              (fun [V33] <0: V123>)
                                              V124))))
                                      V343))}))>))))))))
              (case (V0 V0)
                (fun [V45] (V45 825 V0))
                (fun [V49]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V51 (V49 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V51[0], V51[1], (fun [V52, V335]
                          ((__mon_bind @ [Ty({4}), Ty(Int -> {4} -> (Control {4} { Int
                                                                                 , Int
                                                                                 })), Ty({ Int
                                                                                         , Int
                                                                                         })])
                            (V51[2] V52)
                            (fun [V34] (V34 825))
                            V335))}))>)))))"#]];
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
          (V0 (let
            (V0 (let (V20 ((__mon_generate_marker @ [Ty(Int -> {} {Int, Int})]) {}))
              (fun [V0]
                <0: (fun [V10, V172]
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
                                    (let (V0 (V12 {}))
                                      (fun [V190]
                                        (case (V0 V190)
                                          (fun [V191] (V191 V11 V190))
                                          (fun [V195]
                                            <1: (forall
                                                [(T3: Type) (T4: Type) (T5: Type)] (let
                                                  (V197 (V195 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                  {V197[0], V197[1], (fun [V198, V377]
                                                    ((__mon_bind @ [Ty({}), Ty(Int -> {}
                                                    -> (Control {} { Int
                                                                   , Int
                                                                   })), Ty({Int, Int})])
                                                      (V197[2] V198)
                                                      (fun [V25] (V25 V11))
                                                      V377))}))>)))))>)>), (fun
                            [V8
                            ,V24]
                            <0: (fun [V9, V23]
                                <0: (fun [V10]
                                    (let (V0 (V9 V10))
                                      (fun [V172]
                                        (case (V0 V172)
                                          (fun [V173] (V173 V10 V172))
                                          (fun [V177]
                                            <1: (forall
                                                [(T3: Type) (T4: Type) (T5: Type)] (let
                                                  (V179 (V177 @ [Ty(T2), Ty(T1), Ty(T0)]))
                                                  {V179[0], V179[1], (fun [V180, V378]
                                                    ((__mon_bind @ [Ty({}), Ty(Int -> {}
                                                    -> (Control {} { Int
                                                                   , Int
                                                                   })), Ty({Int, Int})])
                                                      (V179[2] V180)
                                                      (fun [V22] (V22 V10))
                                                      V378))}))>)))))>)>)}})
                        (fun [V14, V29] <0: (fun [V15, V28] <0: {V15, V14}>)>)
                        (fun [V33] <0: V10>)
                        V172)
                      (fun [V173] (V173 V10 V172))
                      (fun [V177]
                        <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                              (V179 (V177 @ [Ty(T2), Ty(T1), Ty(T0)]))
                              {V179[0], V179[1], (fun [V180, V379]
                                ((__mon_bind @ [Ty({}), Ty(Int -> {} ->
                                (Control {} {Int, Int})), Ty({Int, Int})])
                                  (V179[2] V180)
                                  (fun [V22] (V22 V10))
                                  V379))}))>)))>)))
            (fun [V64]
              (case (V0 V64)
                (fun [V65] (V65 825 V64))
                (fun [V69]
                  <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                        (V71 (V69 @ [Ty(T2), Ty(T1), Ty(T0)]))
                        {V71[0], V71[1], (fun [V72, V376]
                          ((__mon_bind @ [Ty({}), Ty(Int -> {} -> (Control {} { Int
                                                                              , Int
                                                                              })), Ty({ Int
                                                                                      , Int
                                                                                      })])
                            (V71[2] V72)
                            (fun [V34] (V34 825))
                            V376))}))>)))))
          (case (case (V0 {})
              (fun [V47] <0: V47[1]>)
              (fun [V51]
                <1: (forall [(T3: Type) (T4: Type) (T5: Type)] (let
                      (V53 (V51 @ [Ty(T2), Ty(T1), Ty(T0)]))
                      {V53[0], V53[1], (fun [V54, V375]
                        ((__mon_bind @ [Ty({}), Ty({Int, Int}), Ty(Int)])
                          (V53[2] V54)
                          (fun [V35, V36] <0: V35[1]>)
                          V375))}))>))
            (fun [V0] V0)
            (fun [V0] 5467)))"#]];
    expect.assert_eq(&pretty_ir);

    let expect_ty = expect!["Int"];
    let simple_ty = simple_ir.type_check(&db).map_err_pretty_with(&db).unwrap();
    expect_ty.assert_eq(&simple_ty.pretty_with(&db).pprint().pretty(80).to_string());
  }
}
