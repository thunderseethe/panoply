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
          (type (;0;) (func (result i32)))
          (type (;1;) (func (param i32) (result i32)))
          (type (;2;) (func (param i32) (result i32)))
          (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
          (type $fun_3_1 (;4;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;5;) (func (param i32 i32 i32 i32) (result i32)))
          (type $fun_1_1 (;6;) (func (param i32) (result i32)))
          (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
          (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
          (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
          (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $__apply_3_2 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_4_3 (;5;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 0
            i32.load offset=12
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_4_1)
          )
          (func $f (;6;) (type $fun_1_1) (param i32) (result i32)
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
          (func $__mon_bind (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
                i32.const 4
                i32.store
                local.get 10
                i32.const 8
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
          (func $__mon_bind_lam_0 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
            i32.const 4
            i32.store
            local.get 5
            i32.const 7
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
          (func $__mon_prompt (;9;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
                      i32.load
                      local.set 13
                      local.get 8
                      i32.load offset=4
                      local.set 14
                      i32.const 20
                      call $alloc
                      local.set 15
                      local.get 15
                      i32.const 5
                      i32.store
                      local.get 15
                      i32.const 10
                      i32.store offset=4
                      local.get 15
                      local.get 0
                      i32.store offset=8
                      local.get 15
                      local.get 1
                      i32.store offset=12
                      local.get 15
                      local.get 8
                      i32.store offset=16
                      local.get 15
                      local.set 15
                      local.get 8
                      i32.load offset=8
                      local.set 16
                      i32.const 16
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 13
                      i32.store
                      local.get 17
                      local.get 14
                      i32.store offset=4
                      local.get 17
                      local.get 15
                      i32.store offset=8
                      local.get 17
                      local.get 16
                      i32.store offset=12
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
                    i32.const 20
                    call $alloc
                    local.set 18
                    local.get 18
                    i32.const 5
                    i32.store
                    local.get 18
                    i32.const 11
                    i32.store offset=4
                    local.get 18
                    local.get 0
                    i32.store offset=8
                    local.get 18
                    local.get 1
                    i32.store offset=12
                    local.get 18
                    local.get 8
                    i32.store offset=16
                    local.get 18
                    local.set 18
                    local.get 8
                    i32.load offset=4
                    local.set 19
                    local.get 19
                    i32.const 4
                    i32.add
                    local.get 18
                    local.get 3
                    local.get 19
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
          (func $__mon_prompt_lam_0 (;10;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 5
            i32.store
            local.get 6
            i32.const 9
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (func $__mon_prompt_lam_1 (;11;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 5
            i32.store
            local.get 6
            i32.const 9
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (table (;0;) 12 12 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_3_2 $__apply_4_3 $f $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
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
          (type (;0;) (func (result i32)))
          (type (;1;) (func (param i32) (result i32)))
          (type (;2;) (func (param i32) (result i32)))
          (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
          (type $fun_1_1 (;4;) (func (param i32) (result i32)))
          (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
          (type $fun_5_1 (;7;) (func (param i32 i32 i32 i32 i32) (result i32)))
          (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
          (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
          (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
          (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_1_1)
          )
          (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_3_0 (;6;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 3
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_3_2 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_4_3 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 0
            i32.load offset=12
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_4_1)
          )
          (func $f (;9;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
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
          (func $g (;10;) (type $fun_1_1) (param i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 1
            local.get 1
            i32.const 5
            i32.store
            local.get 1
            i32.const 11
            i32.store offset=4
            local.get 1
            local.set 1
            i32.const 8
            call $alloc
            local.set 2
            local.get 2
            i32.const 6
            i32.store
            local.get 2
            i32.const 12
            i32.store offset=4
            local.get 2
            local.set 2
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 4
            i32.store
            local.get 3
            i32.const 13
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 4
            i32.store
            local.get 4
            i32.const 14
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
            i32.const 4
            i32.store
            local.get 6
            i32.const 15
            i32.store offset=4
            local.get 6
            local.set 6
            i32.const 8
            call $alloc
            local.set 7
            local.get 7
            i32.const 4
            i32.store
            local.get 7
            i32.const 16
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
            i32.const 5
            i32.store
            local.get 10
            i32.const 17
            i32.store offset=4
            local.get 10
            local.set 10
            i32.const 8
            call $alloc
            local.set 11
            local.get 11
            i32.const 6
            i32.store
            local.get 11
            i32.const 18
            i32.store offset=4
            local.get 11
            local.set 11
            i32.const 8
            call $alloc
            local.set 12
            local.get 12
            i32.const 4
            i32.store
            local.get 12
            i32.const 19
            i32.store offset=4
            local.get 12
            local.set 12
            i32.const 8
            call $alloc
            local.set 13
            local.get 13
            i32.const 4
            i32.store
            local.get 13
            i32.const 20
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
            i32.const 4
            i32.store
            local.get 15
            i32.const 21
            i32.store offset=4
            local.get 15
            local.set 15
            i32.const 8
            call $alloc
            local.set 16
            local.get 16
            i32.const 4
            i32.store
            local.get 16
            i32.const 22
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
          (func $g_lam_0 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $g_lam_1 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $g_lam_2 (;13;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load
            return
          )
          (func $g_lam_3 (;14;) (type $fun_1_1) (param i32) (result i32)
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
          (func $g_lam_4 (;15;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load offset=4
            return
          )
          (func $g_lam_5 (;16;) (type $fun_1_1) (param i32) (result i32)
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
          (func $g_lam_6 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $g_lam_7 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $g_lam_8 (;19;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load offset=4
            return
          )
          (func $g_lam_9 (;20;) (type $fun_1_1) (param i32) (result i32)
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
          (func $g_lam_10 (;21;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            local.get 0
            i32.load
            return
          )
          (func $g_lam_11 (;22;) (type $fun_1_1) (param i32) (result i32)
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
          (func $__mon_bind (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
                i32.const 7
                i32.store
                local.get 10
                i32.const 24
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
          (func $__mon_bind_lam_0 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
            i32.const 7
            i32.store
            local.get 5
            i32.const 23
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
          (func $__mon_prompt (;25;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
                      i32.load
                      local.set 13
                      local.get 8
                      i32.load offset=4
                      local.set 14
                      i32.const 20
                      call $alloc
                      local.set 15
                      local.get 15
                      i32.const 8
                      i32.store
                      local.get 15
                      i32.const 26
                      i32.store offset=4
                      local.get 15
                      local.get 0
                      i32.store offset=8
                      local.get 15
                      local.get 1
                      i32.store offset=12
                      local.get 15
                      local.get 8
                      i32.store offset=16
                      local.get 15
                      local.set 15
                      local.get 8
                      i32.load offset=8
                      local.set 16
                      i32.const 16
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 13
                      i32.store
                      local.get 17
                      local.get 14
                      i32.store offset=4
                      local.get 17
                      local.get 15
                      i32.store offset=8
                      local.get 17
                      local.get 16
                      i32.store offset=12
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
                    i32.const 20
                    call $alloc
                    local.set 18
                    local.get 18
                    i32.const 8
                    i32.store
                    local.get 18
                    i32.const 27
                    i32.store offset=4
                    local.get 18
                    local.get 0
                    i32.store offset=8
                    local.get 18
                    local.get 1
                    i32.store offset=12
                    local.get 18
                    local.get 8
                    i32.store offset=16
                    local.get 18
                    local.set 18
                    local.get 8
                    i32.load offset=4
                    local.set 19
                    local.get 19
                    i32.const 4
                    i32.add
                    local.get 18
                    local.get 3
                    local.get 19
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
          (func $__mon_prompt_lam_0 (;26;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 8
            i32.store
            local.get 6
            i32.const 25
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (func $__mon_prompt_lam_1 (;27;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 8
            i32.store
            local.get 6
            i32.const 25
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (table (;0;) 28 28 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_3_0 $__apply_3_2 $__apply_4_3 $f $g $g_lam_0 $g_lam_1 $g_lam_2 $g_lam_3 $g_lam_4 $g_lam_5 $g_lam_6 $g_lam_7 $g_lam_8 $g_lam_9 $g_lam_10 $g_lam_11 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
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
          (type (;0;) (func (result i32)))
          (type (;1;) (func (param i32) (result i32)))
          (type (;2;) (func (param i32) (result i32)))
          (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
          (type $fun_1_1 (;4;) (func (param i32) (result i32)))
          (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
          (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
          (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
          (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
          (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
          (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_1_1)
          )
          (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_2_1 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
          )
          (func $__apply_3_0 (;7;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            local.get 1
            local.get 2
            local.get 3
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_3_2 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
          )
          (func $__apply_4_3 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
            local.get 0
            i32.load offset=4
            local.get 0
            i32.load offset=8
            local.get 0
            i32.load offset=12
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_4_1)
          )
          (func $f (;10;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
            i32.const 7
            i32.store
            local.get 4
            i32.const 11
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 8
            call $alloc
            local.set 5
            local.get 5
            i32.const 7
            i32.store
            local.get 5
            i32.const 12
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
            i32.const 12
            call $alloc
            local.set 12
            local.get 12
            i32.const 6
            i32.store
            local.get 12
            i32.const 15
            i32.store offset=4
            local.get 12
            local.get 11
            i32.store offset=8
            local.get 12
            local.set 12
            i32.const 8
            call $alloc
            local.set 13
            local.get 13
            i32.const 4
            i32.store
            local.get 13
            i32.const 18
            i32.store offset=4
            local.get 13
            local.set 13
            local.get 12
            local.get 13
            local.get 1
            call $__mon_bind
            local.set 14
            local.get 14
            i32.load
            local.set 15
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 15
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 14
                  i32.load offset=4
                  local.set 16
                  i32.const 8
                  call $alloc
                  local.set 17
                  local.get 17
                  i32.const 0
                  i32.store
                  local.get 17
                  local.get 16
                  i32.store offset=4
                  local.get 17
                  br 2 (;@1;)
                end
                local.get 14
                i32.load offset=4
                local.set 17
                local.get 17
                i32.load
                local.set 18
                local.get 3
                local.get 18
                call $__mon_eqm
                local.set 19
                local.get 19
                i32.load
                local.set 20
                block (result i32) ;; label = @3
                  block ;; label = @4
                    block ;; label = @5
                      block ;; label = @6
                        local.get 20
                        br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
                      end
                      local.get 19
                      i32.load offset=4
                      local.set 21
                      local.get 17
                      i32.load
                      local.set 22
                      local.get 17
                      i32.load offset=4
                      local.set 23
                      i32.const 20
                      call $alloc
                      local.set 24
                      local.get 24
                      i32.const 9
                      i32.store
                      local.get 24
                      i32.const 22
                      i32.store offset=4
                      local.get 24
                      local.get 0
                      i32.store offset=8
                      local.get 24
                      local.get 3
                      i32.store offset=12
                      local.get 24
                      local.get 17
                      i32.store offset=16
                      local.get 24
                      local.set 24
                      local.get 17
                      i32.load offset=8
                      local.set 25
                      i32.const 16
                      call $alloc
                      local.set 26
                      local.get 26
                      local.get 22
                      i32.store
                      local.get 26
                      local.get 23
                      i32.store offset=4
                      local.get 26
                      local.get 24
                      i32.store offset=8
                      local.get 26
                      local.get 25
                      i32.store offset=12
                      local.get 26
                      local.set 26
                      i32.const 8
                      call $alloc
                      local.set 27
                      local.get 27
                      i32.const 1
                      i32.store
                      local.get 27
                      local.get 26
                      i32.store offset=4
                      local.get 27
                      br 2 (;@3;)
                    end
                    local.get 19
                    i32.load offset=4
                    local.set 21
                    i32.const 20
                    call $alloc
                    local.set 27
                    local.get 27
                    i32.const 9
                    i32.store
                    local.get 27
                    i32.const 26
                    i32.store offset=4
                    local.get 27
                    local.get 0
                    i32.store offset=8
                    local.get 27
                    local.get 3
                    i32.store offset=12
                    local.get 27
                    local.get 17
                    i32.store offset=16
                    local.get 27
                    local.set 27
                    local.get 17
                    i32.load offset=4
                    local.set 28
                    local.get 28
                    i32.const 4
                    i32.add
                    local.get 27
                    local.get 1
                    local.get 28
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
            local.set 29
            local.get 29
            i32.load
            local.set 30
            block (result i32) ;; label = @1
              block ;; label = @2
                block ;; label = @3
                  block ;; label = @4
                    local.get 30
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
                  end
                  local.get 29
                  i32.load offset=4
                  local.set 31
                  local.get 31
                  i32.const 4
                  i32.add
                  i32.const 825
                  local.get 31
                  i32.load
                  call_indirect (type $fun_2_1)
                  local.set 32
                  i32.const 8
                  call $alloc
                  local.set 33
                  local.get 33
                  i32.const 0
                  i32.store
                  local.get 33
                  local.get 32
                  i32.store offset=4
                  local.get 33
                  br 2 (;@1;)
                end
                local.get 29
                i32.load offset=4
                local.set 33
                local.get 33
                local.set 34
                local.get 34
                i32.load
                local.set 35
                local.get 34
                i32.load offset=4
                local.set 36
                i32.const 12
                call $alloc
                local.set 37
                local.get 37
                i32.const 6
                i32.store
                local.get 37
                i32.const 29
                i32.store offset=4
                local.get 37
                local.get 34
                i32.store offset=8
                local.get 37
                local.set 37
                i32.const 12
                call $alloc
                local.set 38
                local.get 38
                local.get 35
                i32.store
                local.get 38
                local.get 36
                i32.store offset=4
                local.get 38
                local.get 37
                i32.store offset=8
                local.get 38
                local.set 38
                i32.const 8
                call $alloc
                local.set 39
                local.get 39
                i32.const 1
                i32.store
                local.get 39
                local.get 38
                i32.store offset=4
                local.get 39
                br 1 (;@1;)
              end
              unreachable
            end
            return
          )
          (func $f_lam_0 (;11;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $f_lam_1 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $f_lam_2 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $f_lam_3 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $f_lam_4 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32 i32 i32 i32)
            local.get 0
            i32.load
            local.set 2
            i32.const 12
            call $alloc
            local.set 3
            local.get 3
            i32.const 6
            i32.store
            local.get 3
            i32.const 13
            i32.store offset=4
            local.get 3
            local.get 0
            i32.store offset=8
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 5
            i32.store
            local.get 4
            i32.const 14
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 12
            call $alloc
            local.set 5
            local.get 5
            local.get 2
            i32.store
            local.get 5
            local.get 3
            i32.store offset=4
            local.get 5
            local.get 4
            i32.store offset=8
            local.get 5
            local.set 5
            i32.const 8
            call $alloc
            local.set 6
            local.get 6
            i32.const 1
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            return
          )
          (func $f_lam_5 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $f_lam_6 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
            (local i32 i32)
            i32.const 12
            call $alloc
            local.set 2
            local.get 2
            i32.const 6
            i32.store
            local.get 2
            i32.const 16
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
          (func $f_lam_7 (;18;) (type $fun_1_1) (param i32) (result i32)
            (local i32)
            i32.const 12
            call $alloc
            local.set 1
            local.get 1
            i32.const 6
            i32.store
            local.get 1
            i32.const 17
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            local.get 1
            return
          )
          (func $f_lam_8 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $f_lam_9 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $f_lam_10 (;21;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 7
            i32.store
            local.get 3
            i32.const 19
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 7
            i32.store
            local.get 4
            i32.const 20
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
            local.get 1
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            local.set 6
            local.get 0
            i32.load
            local.set 7
            local.get 7
            i32.const 4
            i32.add
            local.get 2
            local.get 6
            local.get 7
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_11 (;22;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 16
            call $alloc
            local.set 4
            local.get 4
            i32.const 8
            i32.store
            local.get 4
            i32.const 21
            i32.store offset=4
            local.get 4
            local.get 0
            i32.store offset=8
            local.get 4
            local.get 1
            i32.store offset=12
            local.get 4
            local.set 4
            local.get 2
            i32.load offset=8
            local.set 5
            local.get 5
            i32.const 4
            i32.add
            local.get 3
            local.get 5
            i32.load
            call_indirect (type $fun_2_1)
            local.set 6
            i32.const 20
            call $alloc
            local.set 7
            local.get 7
            i32.const 9
            i32.store
            local.get 7
            i32.const 32
            i32.store offset=4
            local.get 7
            local.get 1
            i32.store offset=8
            local.get 7
            local.get 4
            i32.store offset=12
            local.get 7
            local.get 6
            i32.store offset=16
            local.get 7
            return
          )
          (func $f_lam_12 (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $f_lam_13 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
          (func $f_lam_14 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32)
            i32.const 8
            call $alloc
            local.set 3
            local.get 3
            i32.const 7
            i32.store
            local.get 3
            i32.const 23
            i32.store offset=4
            local.get 3
            local.set 3
            i32.const 8
            call $alloc
            local.set 4
            local.get 4
            i32.const 7
            i32.store
            local.get 4
            i32.const 24
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
            local.get 1
            i32.store
            local.get 6
            local.get 5
            i32.store offset=4
            local.get 6
            local.set 6
            local.get 0
            i32.load
            local.set 7
            local.get 7
            i32.const 4
            i32.add
            local.get 2
            local.get 6
            local.get 7
            i32.load
            call_indirect (type $fun_3_1)
            return
          )
          (func $f_lam_15 (;26;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32)
            i32.const 16
            call $alloc
            local.set 4
            local.get 4
            i32.const 8
            i32.store
            local.get 4
            i32.const 25
            i32.store offset=4
            local.get 4
            local.get 0
            i32.store offset=8
            local.get 4
            local.get 1
            i32.store offset=12
            local.get 4
            local.set 4
            local.get 2
            i32.load offset=8
            local.set 5
            local.get 5
            i32.const 4
            i32.add
            local.get 3
            local.get 5
            i32.load
            call_indirect (type $fun_2_1)
            local.set 6
            i32.const 20
            call $alloc
            local.set 7
            local.get 7
            i32.const 9
            i32.store
            local.get 7
            i32.const 32
            i32.store offset=4
            local.get 7
            local.get 1
            i32.store offset=8
            local.get 7
            local.get 4
            i32.store offset=12
            local.get 7
            local.get 6
            i32.store offset=16
            local.get 7
            return
          )
          (func $f_lam_16 (;27;) (type $fun_2_1) (param i32 i32) (result i32)
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
          (func $f_lam_17 (;28;) (type $fun_1_1) (param i32) (result i32)
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
            i32.const 6
            i32.store
            local.get 2
            i32.const 27
            i32.store offset=4
            local.get 2
            local.get 1
            i32.store offset=8
            local.get 2
            return
          )
          (func $f_lam_18 (;29;) (type $fun_2_1) (param i32 i32) (result i32)
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
            i32.const 4
            i32.store
            local.get 4
            i32.const 28
            i32.store offset=4
            local.get 4
            local.set 4
            i32.const 16
            call $alloc
            local.set 5
            local.get 5
            i32.const 8
            i32.store
            local.get 5
            i32.const 30
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
          (func $__mon_bind (;30;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
                i32.const 8
                i32.store
                local.get 10
                i32.const 31
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
          (func $__mon_bind_lam_0 (;31;) (type $fun_3_1) (param i32 i32 i32) (result i32)
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
            i32.const 8
            i32.store
            local.get 5
            i32.const 30
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
          (func $__mon_prompt (;32;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
                      i32.load
                      local.set 13
                      local.get 8
                      i32.load offset=4
                      local.set 14
                      i32.const 20
                      call $alloc
                      local.set 15
                      local.get 15
                      i32.const 9
                      i32.store
                      local.get 15
                      i32.const 33
                      i32.store offset=4
                      local.get 15
                      local.get 0
                      i32.store offset=8
                      local.get 15
                      local.get 1
                      i32.store offset=12
                      local.get 15
                      local.get 8
                      i32.store offset=16
                      local.get 15
                      local.set 15
                      local.get 8
                      i32.load offset=8
                      local.set 16
                      i32.const 16
                      call $alloc
                      local.set 17
                      local.get 17
                      local.get 13
                      i32.store
                      local.get 17
                      local.get 14
                      i32.store offset=4
                      local.get 17
                      local.get 15
                      i32.store offset=8
                      local.get 17
                      local.get 16
                      i32.store offset=12
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
                    i32.const 20
                    call $alloc
                    local.set 18
                    local.get 18
                    i32.const 9
                    i32.store
                    local.get 18
                    i32.const 34
                    i32.store offset=4
                    local.get 18
                    local.get 0
                    i32.store offset=8
                    local.get 18
                    local.get 1
                    i32.store offset=12
                    local.get 18
                    local.get 8
                    i32.store offset=16
                    local.get 18
                    local.set 18
                    local.get 8
                    i32.load offset=4
                    local.set 19
                    local.get 19
                    i32.const 4
                    i32.add
                    local.get 18
                    local.get 3
                    local.get 19
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
          (func $__mon_prompt_lam_0 (;33;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 9
            i32.store
            local.get 6
            i32.const 32
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (func $__mon_prompt_lam_1 (;34;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
            (local i32 i32 i32)
            local.get 2
            i32.load offset=8
            local.set 4
            local.get 4
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            i32.load
            call_indirect (type $fun_2_1)
            local.set 5
            i32.const 20
            call $alloc
            local.set 6
            local.get 6
            i32.const 9
            i32.store
            local.get 6
            i32.const 32
            i32.store offset=4
            local.get 6
            local.get 0
            i32.store offset=8
            local.get 6
            local.get 1
            i32.store offset=12
            local.get 6
            local.get 5
            i32.store offset=16
            local.get 6
            return
          )
          (table (;0;) 35 35 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i32) i32.const 0)
          (export "mem" (memory 0))
          (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $__apply_4_3 $f $f_lam_0 $f_lam_1 $f_lam_2 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_6 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_10 $f_lam_11 $f_lam_12 $f_lam_13 $f_lam_14 $f_lam_15 $f_lam_16 $f_lam_17 $f_lam_18 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
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
