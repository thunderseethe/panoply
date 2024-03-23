(module $example
  (type (;0;) (func (result i32)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32 i32) (result i32)))
  (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
  (type $fun_1_1 (;4;) (func (param i32) (result i32)))
  (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
  (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
  (type $fun_5_1 (;7;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type $fun_0_1 (;8;) (func (result i32)))
  (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
  (import "intrinsic" "__mon_prompt" (func (;1;) (type 2)))
  (import "intrinsic" "__mon_bind" (func (;2;) (type 1)))
  (import "intrinsic" "__mon_eqm" (func (;3;) (type 1)))
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
  (func $wand (;7;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
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
    global.get 0
    i32.const 0
    i32.store
    global.get 0
    local.get 9
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (func $main (;8;) (type $fun_0_1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 5
    i32.store
    global.get 0
    i32.const 9
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 0
    global.get 0
    i32.const 6
    i32.store
    global.get 0
    i32.const 10
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 1
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 11
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 2
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 12
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 3
    global.get 0
    local.get 2
    i32.store
    global.get 0
    local.get 3
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 4
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 13
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 5
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 14
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 6
    global.get 0
    local.get 5
    i32.store
    global.get 0
    local.get 6
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 7
    global.get 0
    local.get 0
    i32.store
    global.get 0
    local.get 1
    i32.store offset=4
    global.get 0
    local.get 4
    i32.store offset=8
    global.get 0
    local.get 7
    i32.store offset=12
    global.get 0
    global.get 0
    i32.const 16
    i32.add
    global.set 0
    local.set 8
    global.get 0
    i32.const 5
    i32.store
    global.get 0
    i32.const 15
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 9
    global.get 0
    i32.const 6
    i32.store
    global.get 0
    i32.const 16
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 10
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 17
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 11
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 18
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 12
    global.get 0
    local.get 11
    i32.store
    global.get 0
    local.get 12
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 13
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 19
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 14
    global.get 0
    i32.const 4
    i32.store
    global.get 0
    i32.const 20
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 15
    global.get 0
    local.get 14
    i32.store
    global.get 0
    local.get 15
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
    local.set 16
    global.get 0
    local.get 9
    i32.store
    global.get 0
    local.get 10
    i32.store offset=4
    global.get 0
    local.get 13
    i32.store offset=8
    global.get 0
    local.get 16
    i32.store offset=12
    global.get 0
    global.get 0
    i32.const 16
    i32.add
    global.set 0
    local.set 17
    global.get 0
    local.set 18
    local.get 8
    local.get 17
    i32.const 734
    i32.const 568
    local.get 18
    call $wand
    local.set 19
    local.get 19
    i32.load
    local.set 20
    block (result i32) ;; label = @1
      block (result i32) ;; label = @2
        local.get 20
        local.get 20
        br_table 0 (;@2;) 1 (;@1;)
      end
      local.get 19
      i32.load offset=4
      local.set 21
      local.get 21
      br 0 (;@1;)
    end
  )
  (func $main_lam_0 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    local.get 1
    i32.store
    global.get 0
    local.get 0
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (func $main_lam_1 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 2
    i32.load
    local.set 3
    block (result i32) ;; label = @1
      block (result i32) ;; label = @2
        block (result i32) ;; label = @3
          local.get 3
          local.get 3
          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
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
      br 0 (;@1;)
    end
  )
  (func $main_lam_2 (;11;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.load offset=4
  )
  (func $main_lam_3 (;12;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 1
    i32.store
    global.get 0
    local.get 0
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (func $main_lam_4 (;13;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.load
  )
  (func $main_lam_5 (;14;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 0
    i32.store
    global.get 0
    local.get 0
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (func $main_lam_6 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    local.get 1
    i32.store
    global.get 0
    local.get 0
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (func $main_lam_7 (;16;) (type $fun_3_1) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    local.get 2
    i32.load
    local.set 3
    block (result i32) ;; label = @1
      block (result i32) ;; label = @2
        block (result i32) ;; label = @3
          local.get 3
          local.get 3
          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
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
      br 0 (;@1;)
    end
  )
  (func $main_lam_8 (;17;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.load offset=4
  )
  (func $main_lam_9 (;18;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 1
    i32.store
    global.get 0
    local.get 0
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (func $main_lam_10 (;19;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.load
  )
  (func $main_lam_11 (;20;) (type $fun_1_1) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 0
    i32.store
    global.get 0
    local.get 0
    i32.store offset=4
    global.get 0
    global.get 0
    i32.const 8
    i32.add
    global.set 0
  )
  (table (;0;) 21 21 funcref)
  (memory (;0;) 1)
  (global (;0;) (mut i32) i32.const 0)
  (export "main" (func $main))
  (export "mem" (memory 0))
  (elem (;0;) (i32.const 0) func 0 1 2 3 $__apply_1_0 $__apply_2_0 $__apply_3_0 $wand $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11)
)

