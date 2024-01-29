use base::modules::Module;

mod emitter;
use emitter::emit_wasm_module;

#[salsa::jar(db = Db)]
pub struct Jar();

pub trait Db: salsa::DbWithJar<Jar> + medir::Db + lower_medir::Db {
  fn as_emit_wasm_db(&self) -> &dyn crate::Db {
    <Self as salsa::DbWithJar<Jar>>::as_jar_db(self)
  }

  fn emit_module(&self, module: Module) -> wasm_encoder::Module {
    let medir_module = self.lower_medir_module_of(module);
    emit_wasm_module(self.as_emit_wasm_db(), medir_module)
  }

  fn emit_module_for_path(&self, path: std::path::PathBuf) -> wasm_encoder::Module {
    let module = self.root_module_for_path(path);
    self.emit_module(module)
  }
}
impl<DB> Db for DB where DB: salsa::DbWithJar<Jar> + medir::Db + lower_medir::Db {}

#[cfg(test)]
mod tests {
  use base::file::{FileId, SourceFile, SourceFileSet};
  use expect_test::expect;
  use wasmparser::Validator;

  use crate::Db;

  #[derive(Default)]
  #[salsa::db(
    crate::Jar,
    ast::Jar,
    base::Jar,
    desugar::Jar,
    lower_medir::Jar,
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

  fn emit_module(db: &TestDatabase, input: &str) -> wasm_encoder::Module {
    let path = std::path::PathBuf::from("test");
    let mut contents = r#"
effect State {
    put : Int -> {},
    get : {} -> Int
}

effect Reader {
    ask : {} -> {}
}

"#
    .to_string();
    contents.push_str(input);
    let file = SourceFile::new(db, FileId::new(db, path.clone()), contents);
    SourceFileSet::new(db, vec![file]);

    db.emit_module_for_path(path)
  }

  #[test]
  fn test_prj() {
    let db = TestDatabase::default();

    let wasm_module = emit_module(&db, "f = { x = 5678, y = 1234 }.x");

    let bytes = wasm_module.finish();
    let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
      function_references: true,
      ..Default::default()
    });
    let validate_res = validator.validate_all(&bytes);
    let string = wasmprinter::print_bytes(bytes).unwrap();
    let expect = expect![[r#"
        (module $test
          (type $fun_0_1 (;0;) (func (result i32)))
          (type $fun_1_1 (;1;) (func (param i32) (result i32)))
          (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
          (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;4;) (func (param i32 i32 i32 i32) (result i32)))
          (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
            (local i32)
            global.get 0
            global.get 0
            i32.const 1
            i32.add
            global.set 0
            return
          )
          (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            global.get 0
            global.get 0
            local.get 0
            i32.add
            global.set 0
            return
          )
          (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 4
            call $alloc
            local.tee 2
            i32.const 1
            i32.const 0
            local.get 0
            local.get 1
            i32.eq
            select
            i32.store
            local.get 2
            return
          )
          (func $__apply_3_2 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $f (;4;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            i32.const 5678
            i32.store offset=4
            local.get 1
            return
          )
          (func $__mon_bind (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 3
            i32.load
            local.set 4
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 4
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 3
                  i32.load offset=4
                  local.set 5
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 5
                  local.get 2
                  local.get 1
                  i32.load
                  call_indirect (type $fun_3_1)
                  br 2 (;@1;)
                end
                local.get 3
                i32.load offset=4
                local.set 6
                local.get 6
                local.set 7
                local.get 7
                i32.load
                local.set 8
                local.get 7
                i32.load offset=4
                local.set 9
                i32.const 16
                call $alloc
                local.set 10
                local.get 10
                i32.const 3
                i32.store
                local.get 10
                i32.const 6
                i32.store offset=4
                local.get 10
                local.get 1
                i32.store offset=8
                local.get 10
                local.get 7
                i32.store offset=12
                local.get 10
                local.set 10
                i32.const 12
                call $alloc
                local.set 11
                local.get 11
                local.get 8
                i32.store
                local.get 11
                local.get 9
                i32.store offset=4
                local.get 11
                local.get 10
                i32.store offset=8
                local.get 11
                local.set 11
                i32.const 8
                call $alloc
                local.set 12
                local.get 12
                i32.const 1
                i32.store
                local.get 12
                local.get 11
                i32.store offset=4
                local.get 12
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_bind_lam_0 (;6;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 1
            i32.load offset=8
            local.set 3
            local.get 3
            i32.const 4
            i32.add
            local.get 2
            local.get 3
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 3
            i32.store
            local.get 5
            i32.const 5
            i32.store offset=4
            local.get 5
            local.get 4
            i32.store offset=8
            local.get 5
            local.get 0
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_prompt (;7;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            local.get 2
            i32.const 4
            i32.add
            local.get 4
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            local.get 5
            i32.load
            local.set 6
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 6
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 5
                  i32.load offset=4
                  local.set 7
                  i32.const 8
                  call $alloc
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.store
                  local.get 8
                  local.get 7
                  i32.store offset=4
                  local.get 8
                  br 2 (;@1;)
                end
                local.get 5
                i32.load offset=4
                local.set 8
                local.get 8
                i32.load
                local.set 9
                local.get 0
                local.get 9
                call $__mon_eqm
                local.set 10
                local.get 10
                i32.load
                local.set 11
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 11
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 10
                      i32.load offset=4
                      local.set 12
                      local.get 8
                      local.set 13
                      local.get 13
                      i32.load
                      local.set 14
                      local.get 13
                      i32.load offset=4
                      local.set 15
                      local.get 13
                      i32.load offset=8
                      local.set 16
                      i32.const 12
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 14
                      i32.store
                      local.get 17
                      local.get 15
                      i32.store offset=4
                      local.get 17
                      local.get 16
                      i32.store offset=8
                      local.get 17
                      local.set 17
                      i32.const 8
                      call $alloc
                      local.set 18
                      local.get 18
                      i32.const 1
                      i32.store
                      local.get 18
                      local.get 17
                      i32.store offset=4
                      local.get 18
                      br 2 (;@3;)
                    end
                    local.get 10
                    i32.load offset=4
                    local.set 12
                    local.get 8
                    local.set 18
                    local.get 18
                    i32.load offset=8
                    local.set 19
                    local.get 18
                    i32.load offset=4
                    local.set 20
                    local.get 20
                    i32.const 4
                    i32.add
                    local.get 19
                    local.get 3
                    local.get 20
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (table (;0;) 8 8 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (global (;1;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_3_2 $f $__mon_bind $__mon_bind_lam_0 $__mon_prompt)
        )"#]];
    expect.assert_eq(&string);

    validate_res.expect("Validation Failed");
  }

  #[test]
  fn test_wand() {
    let db = TestDatabase::default();

    let wasm_module = emit_module(
      &db,
      r#"f = |m||n| (m ,, n).x

g = f({ x = {} })({ y = {} })
"#,
    );

    let bytes = wasm_module.finish();
    let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
      function_references: true,
      ..Default::default()
    });
    let validate_res = validator.validate_all(&bytes);
    let string = wasmprinter::print_bytes(bytes).unwrap();
    let expect = expect![[r#"
        (module $test
          (type $fun_0_1 (;0;) (func (result i32)))
          (type $fun_1_1 (;1;) (func (param i32) (result i32)))
          (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
          (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;4;) (func (param i32 i32 i32 i32) (result i32)))
          (type $fun_5_1 (;5;) (func (param i32 i32 i32 i32 i32) (result i32)))
          (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
            (local i32)
            global.get 0
            global.get 0
            i32.const 1
            i32.add
            global.set 0
            return
          )
          (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            global.get 0
            global.get 0
            local.get 0
            i32.add
            global.set 0
            return
          )
          (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 4
            call $alloc
            local.tee 2
            i32.const 1
            i32.const 0
            local.get 0
            local.get 1
            i32.eq
            select
            i32.store
            local.get 2
            return
          )
          (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_1_1)
          )
          (func $__apply_2_0 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_3_0 (;5;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 3
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_3_2 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $f (;7;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.load
            local.set 5
            local.get 5
            i32.const 4
            i32.add
            local.get 2
            local.get 3
            local.get 5
            i32.load
            call_indirect (type $fun_3_1)
            local.set 6
            local.get 1
            i32.load offset=12
            local.set 7
            local.get 7
            i32.load
            local.set 8
            local.get 8
            i32.const 4
            i32.add
            local.get 6
            local.get 8
            i32.load
            call_indirect (type $fun_2_1)
            local.set 9
            i32.const 8
            call $alloc
            local.set 10
            local.get 10
            i32.const 0
            i32.store
            local.get 10
            local.get 9
            i32.store offset=4
            local.get 10
            return
          )
          (func $g (;8;) (type $fun_1_1) (param i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 4
            i32.store
            local.get 1
            i32.const 9
            i32.store offset=4
            local.get 1
            local.set 1
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 5
            i32.store
            local.get 2
            i32.const 10
            i32.store offset=4
            local.get 2
            local.set 2
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 3
            i32.store
            local.get 3
            i32.const 11
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 3
            i32.store
            local.get 4
            i32.const 12
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            local.get 3
            i32.store
            local.get 5
            local.get 4
            i32.store offset=4
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            i32.const 3
            i32.store
            local.get 6
            i32.const 13
            i32.store offset=4
            local.get 6
            local.set 6
            i32.const 8
            call $alloc
            local.set 7
            local.get 7
            i32.const 3
            i32.store
            local.get 7
            i32.const 14
            i32.store offset=4
            local.get 7
            local.set 7
            i32.const 8
            call $alloc
            local.set 8
            local.get 8
            local.get 6
            i32.store
            local.get 8
            local.get 7
            i32.store offset=4
            local.get 8
            local.set 8
            i32.const 16
            call $alloc
            local.set 9
            local.get 9
            local.get 1
            i32.store
            local.get 9
            local.get 2
            i32.store offset=4
            local.get 9
            local.get 5
            i32.store offset=8
            local.get 9
            local.get 8
            i32.store offset=12
            local.get 9
            local.set 9
            i32.const 8
            call $alloc
            local.set 10
            local.get 10
            i32.const 4
            i32.store
            local.get 10
            i32.const 15
            i32.store offset=4
            local.get 10
            local.set 10
            i32.const 8
            call $alloc
            local.set 11
            local.get 11
            i32.const 5
            i32.store
            local.get 11
            i32.const 16
            i32.store offset=4
            local.get 11
            local.set 11
            i32.const 8
            call $alloc
            local.set 12
            local.get 12
            i32.const 3
            i32.store
            local.get 12
            i32.const 17
            i32.store offset=4
            local.get 12
            local.set 12
            i32.const 8
            call $alloc
            local.set 13
            local.get 13
            i32.const 3
            i32.store
            local.get 13
            i32.const 18
            i32.store offset=4
            local.get 13
            local.set 13
            i32.const 8
            call $alloc
            local.set 14
            local.get 14
            local.get 12
            i32.store
            local.get 14
            local.get 13
            i32.store offset=4
            local.get 14
            local.set 14
            i32.const 8
            call $alloc
            local.set 15
            local.get 15
            i32.const 3
            i32.store
            local.get 15
            i32.const 19
            i32.store offset=4
            local.get 15
            local.set 15
            i32.const 8
            call $alloc
            local.set 16
            local.get 16
            i32.const 3
            i32.store
            local.get 16
            i32.const 20
            i32.store offset=4
            local.get 16
            local.set 16
            i32.const 8
            call $alloc
            local.set 17
            local.get 17
            local.get 15
            i32.store
            local.get 17
            local.get 16
            i32.store offset=4
            local.get 17
            local.set 17
            i32.const 16
            call $alloc
            local.set 18
            local.get 18
            local.get 10
            i32.store
            local.get 18
            local.get 11
            i32.store offset=4
            local.get 18
            local.get 14
            i32.store offset=8
            local.get 18
            local.get 17
            i32.store offset=12
            local.get 18
            local.set 18
            i32.const 0
            call $alloc
            local.set 19
            local.get 19
            local.set 19
            i32.const 0
            call $alloc
            local.set 20
            local.get 20
            local.set 20
            local.get 9
            local.get 18
            local.get 19
            local.get 20
            local.get 0
            call $f
            return
          )
          (func $g_lam_0 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 0
            i32.store
            local.get 2
            local.get 1
            i32.store offset=4
            local.get 2
            return
          )
          (func $g_lam_1 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load
            local.set 3
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 3
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 2
                  i32.load offset=4
                  local.set 4
                  local.get 0
                  i32.const 4
                  i32.add
                  local.get 4
                  local.get 0
                  i32.load
                  call_indirect (type $fun_2_1)
                  br 2 (;@1;)
                end
                local.get 2
                i32.load offset=4
                local.set 4
                local.get 1
                i32.const 4
                i32.add
                local.get 4
                local.get 1
                i32.load
                call_indirect (type $fun_2_1)
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $g_lam_2 (;11;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load
            return
          )
          (func $g_lam_3 (;12;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $g_lam_4 (;13;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load offset=4
            return
          )
          (func $g_lam_5 (;14;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 1
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $g_lam_6 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 1
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $g_lam_7 (;16;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load
            local.set 3
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 3
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 2
                  i32.load offset=4
                  local.set 4
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 4
                  local.get 1
                  i32.load
                  call_indirect (type $fun_2_1)
                  br 2 (;@1;)
                end
                local.get 2
                i32.load offset=4
                local.set 4
                local.get 0
                i32.const 4
                i32.add
                local.get 4
                local.get 0
                i32.load
                call_indirect (type $fun_2_1)
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $g_lam_8 (;17;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load offset=4
            return
          )
          (func $g_lam_9 (;18;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 1
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $g_lam_10 (;19;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load
            return
          )
          (func $g_lam_11 (;20;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 0
            i32.store
            local.get 1
            local.get 0
            i32.store offset=4
            local.get 1
            return
          )
          (func $__mon_bind (;21;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 3
            i32.load
            local.set 4
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 4
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 3
                  i32.load offset=4
                  local.set 5
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 5
                  local.get 2
                  local.get 1
                  i32.load
                  call_indirect (type $fun_3_1)
                  br 2 (;@1;)
                end
                local.get 3
                i32.load offset=4
                local.set 6
                local.get 6
                local.set 7
                local.get 7
                i32.load
                local.set 8
                local.get 7
                i32.load offset=4
                local.set 9
                i32.const 16
                call $alloc
                local.set 10
                local.get 10
                i32.const 6
                i32.store
                local.get 10
                i32.const 22
                i32.store offset=4
                local.get 10
                local.get 1
                i32.store offset=8
                local.get 10
                local.get 7
                i32.store offset=12
                local.get 10
                local.set 10
                i32.const 12
                call $alloc
                local.set 11
                local.get 11
                local.get 8
                i32.store
                local.get 11
                local.get 9
                i32.store offset=4
                local.get 11
                local.get 10
                i32.store offset=8
                local.get 11
                local.set 11
                i32.const 8
                call $alloc
                local.set 12
                local.get 12
                i32.const 1
                i32.store
                local.get 12
                local.get 11
                i32.store offset=4
                local.get 12
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_bind_lam_0 (;22;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 1
            i32.load offset=8
            local.set 3
            local.get 3
            i32.const 4
            i32.add
            local.get 2
            local.get 3
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 6
            i32.store
            local.get 5
            i32.const 21
            i32.store offset=4
            local.get 5
            local.get 4
            i32.store offset=8
            local.get 5
            local.get 0
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_prompt (;23;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            local.get 2
            i32.const 4
            i32.add
            local.get 4
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            local.get 5
            i32.load
            local.set 6
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 6
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 5
                  i32.load offset=4
                  local.set 7
                  i32.const 8
                  call $alloc
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.store
                  local.get 8
                  local.get 7
                  i32.store offset=4
                  local.get 8
                  br 2 (;@1;)
                end
                local.get 5
                i32.load offset=4
                local.set 8
                local.get 8
                i32.load
                local.set 9
                local.get 0
                local.get 9
                call $__mon_eqm
                local.set 10
                local.get 10
                i32.load
                local.set 11
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 11
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 10
                      i32.load offset=4
                      local.set 12
                      local.get 8
                      local.set 13
                      local.get 13
                      i32.load
                      local.set 14
                      local.get 13
                      i32.load offset=4
                      local.set 15
                      local.get 13
                      i32.load offset=8
                      local.set 16
                      i32.const 12
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 14
                      i32.store
                      local.get 17
                      local.get 15
                      i32.store offset=4
                      local.get 17
                      local.get 16
                      i32.store offset=8
                      local.get 17
                      local.set 17
                      i32.const 8
                      call $alloc
                      local.set 18
                      local.get 18
                      i32.const 1
                      i32.store
                      local.get 18
                      local.get 17
                      i32.store offset=4
                      local.get 18
                      br 2 (;@3;)
                    end
                    local.get 10
                    i32.load offset=4
                    local.set 12
                    local.get 8
                    local.set 18
                    local.get 18
                    i32.load offset=8
                    local.set 19
                    local.get 18
                    i32.load offset=4
                    local.set 20
                    local.get 20
                    i32.const 4
                    i32.add
                    local.get 19
                    local.get 3
                    local.get 20
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (table (;0;) 24 24 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (global (;1;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_3_0 $__apply_3_2 $f $g $g_lam_0 $g_lam_1 $g_lam_2 $g_lam_3 $g_lam_4 $g_lam_5 $g_lam_6 $g_lam_7 $g_lam_8 $g_lam_9 $g_lam_10 $g_lam_11 $__mon_bind $__mon_bind_lam_0 $__mon_prompt)
        )"#]];
    expect.assert_eq(&string);

    validate_res.expect("Validation Failed");
  }

  #[test]
  fn test_simple_get() {
    let db = TestDatabase::default();

    let wasm_module = emit_module(
      &db,
      r#"
f = (with {
    get = |x| |k| |s| k(s)(s),
    put = |x| |k| |s| k({})(x),
    return = |x| |s| {state = s, value = x},
} do State.get({}))(825)"#,
    );

    let bytes = wasm_module.finish();
    /*let mut validator = Validator::new_with_features(wasmparser::WasmFeatures {
        function_references: true,
        ..Default::default()
    });
    let validate_res = validator.validate_all(&bytes);*/
    let string = wasmprinter::print_bytes(bytes).unwrap();
    let expect = expect![[r#"
        (module $test
          (type $fun_0_1 (;0;) (func (result i32)))
          (type $fun_1_1 (;1;) (func (param i32) (result i32)))
          (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
          (type $fun_4_1 (;3;) (func (param i32 i32 i32 i32) (result i32)))
          (type $fun_3_1 (;4;) (func (param i32 i32 i32) (result i32)))
          (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
            (local i32)
            global.get 0
            global.get 0
            i32.const 1
            i32.add
            global.set 0
            return
          )
          (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            global.get 0
            global.get 0
            local.get 0
            i32.add
            global.set 0
            return
          )
          (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 4
            call $alloc
            local.tee 2
            i32.const 1
            i32.const 0
            local.get 0
            local.get 1
            i32.eq
            select
            i32.store
            local.get 2
            return
          )
          (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_1_1)
          )
          (func $__apply_2_1 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_3_0 (;5;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 3
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_3_2 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $f (;7;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            i32.const 0
            call $alloc
            local.set 2
            local.get 2
            local.set 2
            local.get 2
            call $__mon_generate_marker
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 5
            i32.store
            local.get 4
            i32.const 8
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            i32.const 5
            i32.store
            local.get 5
            i32.const 9
            i32.store offset=4
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            local.get 4
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            local.set 6
            i32.const 8
            call $alloc
            local.set 7
            local.get 7
            local.get 3
            i32.store
            local.get 7
            local.get 6
            i32.store offset=4
            local.get 7
            local.set 7
            local.get 0
            i32.load
            local.set 8
            local.get 8
            i32.const 4
            i32.add
            local.get 1
            local.get 7
            local.get 8
            i32.load
            call_indirect (type $fun_3_1)
            local.set 1
            local.get 0
            i32.load offset=12
            local.set 9
            local.get 9
            i32.load
            local.set 10
            local.get 10
            i32.const 4
            i32.add
            local.get 1
            local.get 10
            i32.load
            call_indirect (type $fun_2_1)
            local.set 11
            local.get 11
            i32.load
            local.set 12
            local.get 3
            local.get 12
            call $__mon_eqm
            local.set 13
            local.get 13
            i32.load
            local.set 14
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 14
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 13
                  i32.load offset=4
                  local.set 15
                  local.get 11
                  i32.load
                  local.set 16
                  i32.const 12
                  call $alloc
                  local.set 17
                  local.get 17
                  i32.const 4
                  i32.store
                  local.get 17
                  i32.const 10
                  i32.store offset=4
                  local.get 17
                  local.get 11
                  i32.store offset=8
                  local.get 17
                  local.set 17
                  i32.const 8
                  call $alloc
                  local.set 18
                  local.get 18
                  i32.const 3
                  i32.store
                  local.get 18
                  i32.const 15
                  i32.store offset=4
                  local.get 18
                  local.set 18
                  i32.const 12
                  call $alloc
                  local.set 19
                  local.get 19
                  local.get 16
                  i32.store
                  local.get 19
                  local.get 17
                  i32.store offset=4
                  local.get 19
                  local.get 18
                  i32.store offset=8
                  local.get 19
                  local.set 19
                  i32.const 8
                  call $alloc
                  local.set 20
                  local.get 20
                  i32.const 1
                  i32.store
                  local.get 20
                  local.get 19
                  i32.store offset=4
                  local.get 20
                  br 2 (;@1;)
                end
                local.get 13
                i32.load offset=4
                local.set 15
                local.get 11
                i32.load
                local.set 20
                i32.const 12
                call $alloc
                local.set 21
                local.get 21
                i32.const 4
                i32.store
                local.get 21
                i32.const 16
                i32.store offset=4
                local.get 21
                local.get 11
                i32.store offset=8
                local.get 21
                local.set 21
                i32.const 8
                call $alloc
                local.set 22
                local.get 22
                i32.const 3
                i32.store
                local.get 22
                i32.const 21
                i32.store offset=4
                local.get 22
                local.set 22
                i32.const 12
                call $alloc
                local.set 23
                local.get 23
                local.get 20
                i32.store
                local.get 23
                local.get 21
                i32.store offset=4
                local.get 23
                local.get 22
                i32.store offset=8
                local.get 23
                local.set 23
                local.get 23
                i32.load offset=8
                local.set 24
                local.get 23
                i32.load offset=4
                local.set 25
                local.get 25
                i32.const 4
                i32.add
                local.get 24
                local.get 1
                local.get 25
                i32.load
                call_indirect (type $fun_3_1)
                br 1 (;@1;)
              end
              unreachable
            end
            local.set 26
            local.get 26
            i32.load
            local.set 27
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 27
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 26
                  i32.load offset=4
                  local.set 28
                  local.get 28
                  i32.const 4
                  i32.add
                  i32.const 825
                  local.get 28
                  i32.load
                  call_indirect (type $fun_2_1)
                  local.set 29
                  i32.const 8
                  call $alloc
                  local.set 30
                  local.get 30
                  i32.const 0
                  i32.store
                  local.get 30
                  local.get 29
                  i32.store offset=4
                  local.get 30
                  br 2 (;@1;)
                end
                local.get 26
                i32.load offset=4
                local.set 30
                local.get 30
                local.set 31
                local.get 31
                i32.load
                local.set 32
                local.get 31
                i32.load offset=4
                local.set 33
                i32.const 12
                call $alloc
                local.set 34
                local.get 34
                i32.const 4
                i32.store
                local.get 34
                i32.const 24
                i32.store offset=4
                local.get 34
                local.get 31
                i32.store offset=8
                local.get 34
                local.set 34
                i32.const 12
                call $alloc
                local.set 35
                local.get 35
                local.get 32
                i32.store
                local.get 35
                local.get 33
                i32.store offset=4
                local.get 35
                local.get 34
                i32.store offset=8
                local.get 35
                local.set 35
                i32.const 8
                call $alloc
                local.set 36
                local.get 36
                i32.const 1
                i32.store
                local.get 36
                local.get 35
                i32.store offset=4
                local.get 36
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $f_lam_0 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32)
            i32.const 0
            call $alloc
            local.set 3
            local.get 3
            local.set 3
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 0
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_1 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 2
            local.get 2
            local.get 1
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_2 (;10;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 0
            call $alloc
            local.set 2
            local.get 2
            local.set 2
            local.get 0
            i32.load offset=4
            local.set 3
            local.get 3
            i32.load offset=4
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 2
            local.get 1
            local.get 4
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_3 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 0
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_4 (;12;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 1
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_5 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32)
            i32.const 12
            call $alloc
            local.set 2
            local.get 2
            i32.const 4
            i32.store
            local.get 2
            i32.const 12
            i32.store offset=4
            local.get 2
            local.get 0
            i32.store offset=8
            local.get 2
            local.set 2
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 0
            i32.store
            local.get 3
            local.get 2
            i32.store offset=4
            local.get 3
            return
          )
          (func $f_lam_6 (;14;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 12
            call $alloc
            local.set 1
            local.get 1
            i32.const 4
            i32.store
            local.get 1
            i32.const 13
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            local.get 1
            return
          )
          (func $f_lam_7 (;15;) (type $fun_1_1) (param i32) (result i32)
            (local i32 i32 i32)
            i32.const 12
            call $alloc
            local.set 1
            local.get 1
            i32.const 4
            i32.store
            local.get 1
            i32.const 11
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            local.get 1
            local.set 1
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 3
            i32.store
            local.get 2
            i32.const 14
            i32.store offset=4
            local.get 2
            local.set 2
            i32.const 16
            call $alloc
            local.set 3
            local.get 3
            i32.const 6
            i32.store
            local.get 3
            i32.const 25
            i32.store offset=4
            local.get 3
            local.get 1
            i32.store offset=8
            local.get 3
            local.get 2
            i32.store offset=12
            local.get 3
            return
          )
          (func $f_lam_8 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 0
            call $alloc
            local.set 2
            local.get 2
            local.set 2
            local.get 0
            i32.load offset=4
            local.set 3
            local.get 3
            i32.load offset=4
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 2
            local.get 1
            local.get 4
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_9 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 0
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_10 (;18;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            local.get 1
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_11 (;19;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32)
            i32.const 12
            call $alloc
            local.set 2
            local.get 2
            i32.const 4
            i32.store
            local.get 2
            i32.const 18
            i32.store offset=4
            local.get 2
            local.get 0
            i32.store offset=8
            local.get 2
            local.set 2
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 0
            i32.store
            local.get 3
            local.get 2
            i32.store offset=4
            local.get 3
            return
          )
          (func $f_lam_12 (;20;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 12
            call $alloc
            local.set 1
            local.get 1
            i32.const 4
            i32.store
            local.get 1
            i32.const 19
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            local.get 1
            return
          )
          (func $f_lam_13 (;21;) (type $fun_1_1) (param i32) (result i32)
            (local i32 i32 i32)
            i32.const 12
            call $alloc
            local.set 1
            local.get 1
            i32.const 4
            i32.store
            local.get 1
            i32.const 17
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            local.get 1
            local.set 1
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 3
            i32.store
            local.get 2
            i32.const 20
            i32.store offset=4
            local.get 2
            local.set 2
            i32.const 16
            call $alloc
            local.set 3
            local.get 3
            i32.const 6
            i32.store
            local.get 3
            i32.const 25
            i32.store offset=4
            local.get 3
            local.get 1
            i32.store offset=8
            local.get 3
            local.get 2
            i32.store offset=12
            local.get 3
            return
          )
          (func $f_lam_14 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32)
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 0
            i32.store
            local.get 2
            local.get 0
            i32.store offset=4
            local.get 2
            return
          )
          (func $f_lam_15 (;23;) (type $fun_1_1) (param i32) (result i32)
            (local i32 i32)
            local.get 0
            i32.const 4
            i32.add
            i32.const 825
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 1
            i32.const 12
            call $alloc
            local.set 2
            local.get 2
            i32.const 4
            i32.store
            local.get 2
            i32.const 22
            i32.store offset=4
            local.get 2
            local.get 1
            i32.store offset=8
            local.get 2
            return
          )
          (func $f_lam_16 (;24;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32)
            local.get 0
            i32.load offset=8
            local.set 2
            local.get 2
            i32.const 4
            i32.add
            local.get 1
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 3
            i32.store
            local.get 4
            i32.const 23
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 6
            i32.store
            local.get 5
            i32.const 25
            i32.store offset=4
            local.get 5
            local.get 3
            i32.store offset=8
            local.get 5
            local.get 4
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_bind (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.set 3
            local.get 3
            i32.load
            local.set 4
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 4
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 3
                  i32.load offset=4
                  local.set 5
                  local.get 1
                  i32.const 4
                  i32.add
                  local.get 5
                  local.get 2
                  local.get 1
                  i32.load
                  call_indirect (type $fun_3_1)
                  br 2 (;@1;)
                end
                local.get 3
                i32.load offset=4
                local.set 6
                local.get 6
                local.set 7
                local.get 7
                i32.load
                local.set 8
                local.get 7
                i32.load offset=4
                local.set 9
                i32.const 16
                call $alloc
                local.set 10
                local.get 10
                i32.const 6
                i32.store
                local.get 10
                i32.const 26
                i32.store offset=4
                local.get 10
                local.get 1
                i32.store offset=8
                local.get 10
                local.get 7
                i32.store offset=12
                local.get 10
                local.set 10
                i32.const 12
                call $alloc
                local.set 11
                local.get 11
                local.get 8
                i32.store
                local.get 11
                local.get 9
                i32.store offset=4
                local.get 11
                local.get 10
                i32.store offset=8
                local.get 11
                local.set 11
                i32.const 8
                call $alloc
                local.set 12
                local.get 12
                i32.const 1
                i32.store
                local.get 12
                local.get 11
                i32.store offset=4
                local.get 12
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $__mon_bind_lam_0 (;26;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 1
            i32.load offset=8
            local.set 3
            local.get 3
            i32.const 4
            i32.add
            local.get 2
            local.get 3
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 6
            i32.store
            local.get 5
            i32.const 25
            i32.store offset=4
            local.get 5
            local.get 4
            i32.store offset=8
            local.get 5
            local.get 0
            i32.store offset=12
            local.get 5
            return
          )
          (func $__mon_prompt (;27;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.set 4
            local.get 2
            i32.const 4
            i32.add
            local.get 4
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            local.get 5
            i32.load
            local.set 6
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 6
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 5
                  i32.load offset=4
                  local.set 7
                  i32.const 8
                  call $alloc
                  local.set 8
                  local.get 8
                  i32.const 0
                  i32.store
                  local.get 8
                  local.get 7
                  i32.store offset=4
                  local.get 8
                  br 2 (;@1;)
                end
                local.get 5
                i32.load offset=4
                local.set 8
                local.get 8
                i32.load
                local.set 9
                local.get 0
                local.get 9
                call $__mon_eqm
                local.set 10
                local.get 10
                i32.load
                local.set 11
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 11
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 10
                      i32.load offset=4
                      local.set 12
                      local.get 8
                      local.set 13
                      local.get 13
                      i32.load
                      local.set 14
                      local.get 13
                      i32.load offset=4
                      local.set 15
                      local.get 13
                      i32.load offset=8
                      local.set 16
                      i32.const 12
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 14
                      i32.store
                      local.get 17
                      local.get 15
                      i32.store offset=4
                      local.get 17
                      local.get 16
                      i32.store offset=8
                      local.get 17
                      local.set 17
                      i32.const 8
                      call $alloc
                      local.set 18
                      local.get 18
                      i32.const 1
                      i32.store
                      local.get 18
                      local.get 17
                      i32.store offset=4
                      local.get 18
                      br 2 (;@3;)
                    end
                    local.get 10
                    i32.load offset=4
                    local.set 12
                    local.get 8
                    local.set 18
                    local.get 18
                    i32.load offset=8
                    local.set 19
                    local.get 18
                    i32.load offset=4
                    local.set 20
                    local.get 20
                    i32.const 4
                    i32.add
                    local.get 19
                    local.get 3
                    local.get 20
                    i32.load
                    call_indirect (type $fun_3_1)
                    br 1 (;@3;)
                  end
                  unreachable
                end
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (table (;0;) 28 28 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (global (;1;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $f $f_lam_0 $f_lam_1 $f_lam_2 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_6 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_10 $f_lam_11 $f_lam_12 $f_lam_13 $f_lam_14 $f_lam_15 $f_lam_16 $__mon_bind $__mon_bind_lam_0 $__mon_prompt)
        )"#]];
    expect.assert_eq(&string);

    /*match validate_res {
        Ok(_) => {},
        Err(bin_reader_err) => {
            panic!("{}", bin_reader_err.message());
        },
    }*/
  }
}
