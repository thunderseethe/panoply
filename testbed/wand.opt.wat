(module $reader
  (type $fun_2_1 (func (param i32 i32) (result i32)))
  (type $fun_3_1 (func (param i32 i32 i32) (result i32)))
  (type $fun_4_1 (func (param i32 i32 i32 i32) (result i32)))
  (type $fun_1_1 (func (param i32) (result i32)))
  (type $fun_5_1 (func (param i32 i32 i32 i32 i32) (result i32)))
  (type $fun_6_1 (func (param i32 i32 i32 i32 i32 i32) (result i32)))
  (type $fun_0_1 (func (result i32)))
  (func $__mon_generate_marker (type $fun_0_1) (result i32)
    (local i32)
    global.get 1
    local.tee 0
    i32.const 1
    i32.add
    global.set 1
    local.get 0)
  (func $alloc (type $fun_1_1) (param i32) (result i32)
    local.get 0
    global.get 0
    local.tee 0
    i32.add
    global.set 0
    local.get 0)
  (func $__mon_eqm (type $fun_2_1) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 2
    i32.const 4
    i32.add
    global.set 0
    local.get 2
    local.get 0
    local.get 1
    i32.eq
    i32.store
    local.get 2)
  (func $__apply_1_0 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_1_1))
  (func $__apply_2_0 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 1
    local.get 2
    local.get 0
    i32.load
    call_indirect (type $fun_2_1))
  (func $__apply_2_1 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_2_1))
  (func $__apply_3_0 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    local.get 1
    local.get 2
    local.get 3
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $__apply_3_1 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 1
    local.get 2
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $__apply_3_2 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 0
    i32.load offset=8
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $__apply_4_2 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 0
    i32.load offset=8
    local.get 1
    local.get 2
    local.get 0
    i32.load
    call_indirect (type $fun_4_1))
  (func $__apply_4_3 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 0
    i32.load offset=8
    local.get 0
    i32.load offset=12
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_4_1))
  (func $__apply_5_3 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 0
    i32.load offset=8
    local.get 0
    i32.load offset=12
    local.get 1
    local.get 2
    local.get 0
    i32.load
    call_indirect (type $fun_5_1))
  (func $__apply_5_4 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 0
    i32.load offset=8
    local.get 0
    i32.load offset=12
    local.get 0
    i32.load offset=16
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_5_1))
  (func $__apply_6_4 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.get 0
    i32.load offset=8
    local.get 0
    i32.load offset=12
    local.get 0
    i32.load offset=16
    local.get 1
    local.get 2
    local.get 0
    i32.load
    call_indirect (type $fun_6_1))
  (func $f (type $fun_2_1) (param i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 16
    i32.add
    global.set 0
    local.get 1
    i32.const 8
    i32.store
    local.get 1
    i32.const 42
    i32.store offset=4
    local.get 1
    local.get 0
    i32.store offset=8
    local.get 1
    local.get 0
    i32.store offset=12
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0)
  (func $f_lam_0 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 1
    i32.const 4
    i32.add
    local.get 0
    local.get 1
    i32.load
    call_indirect (type $fun_2_1))
  (func $f_lam_1 (type $fun_3_1) (param i32 i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 12
    i32.add
    global.set 0
    local.get 1
    i32.const 5
    i32.store
    local.get 1
    i32.const 15
    i32.store offset=4
    local.get 1
    local.get 0
    i32.store offset=8
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0)
  (func $f_lam_3 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    local.get 1
    i32.load offset=8
    local.tee 1
    i32.const 4
    i32.add
    local.get 2
    local.get 1
    i32.load
    call_indirect (type $fun_2_1)
    global.get 0
    local.tee 1
    i32.const 12
    i32.add
    global.set 0
    local.get 1
    i32.const 5
    i32.store
    local.get 1
    i32.const 17
    i32.store offset=4
    local.get 1
    local.get 0
    i32.store offset=8
    local.get 1
    local.get 3
    call $__mon_bind)
  (func $f_lam_4 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.const 4
            i32.add
            local.get 3
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.tee 0
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 0
          i32.load offset=4
          local.tee 2
          i32.const 4
          i32.add
          local.get 1
          local.get 0
          local.get 2
          i32.load
          call_indirect (type $fun_3_1)
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=4
        local.tee 2
        i32.load
        local.set 3
        local.get 2
        i32.load offset=4
        local.set 4
        global.get 0
        local.tee 0
        i32.const 16
        i32.add
        global.set 0
        local.get 0
        i32.const 9
        i32.store
        local.get 0
        i32.const 18
        i32.store offset=4
        local.get 0
        local.get 1
        i32.store offset=8
        local.get 0
        local.get 2
        i32.store offset=12
        global.get 0
        local.tee 1
        i32.const 12
        i32.add
        global.set 0
        local.get 1
        local.get 3
        i32.store
        local.get 1
        local.get 4
        i32.store offset=4
        local.get 1
        local.get 0
        i32.store offset=8
        global.get 0
        local.tee 0
        i32.const 8
        i32.add
        global.set 0
        local.get 0
        i32.const 1
        i32.store
        local.get 0
        local.get 1
        i32.store offset=4
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0)
  (func $f_lam_5 (type $fun_2_1) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 2
    global.set 0
    local.get 0
    i32.load offset=4
    local.tee 0
    i32.const 4
    i32.add
    local.get 2
    local.get 0
    i32.load
    call_indirect (type $fun_2_1)
    local.set 2
    global.get 0
    local.tee 0
    i32.const 20
    i32.add
    global.set 0
    local.get 0
    i32.const 10
    i32.store
    local.get 0
    i32.const 19
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store offset=8
    local.get 0
    local.get 1
    i32.store offset=12
    local.get 0
    local.get 2
    i32.store offset=16
    local.get 0)
  (func $f_lam_7 (type $fun_3_1) (param i32 i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 12
    i32.add
    global.set 0
    local.get 1
    i32.const 5
    i32.store
    local.get 1
    i32.const 21
    i32.store offset=4
    local.get 1
    local.get 0
    i32.store offset=8
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0)
  (func $f_lam_8 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 4
    i32.const 12
    i32.add
    global.set 0
    local.get 4
    i32.const 7
    i32.store
    local.get 4
    i32.const 22
    i32.store offset=4
    local.get 4
    local.get 1
    i32.store offset=8
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    local.get 2
    i32.store
    local.get 1
    local.get 4
    i32.store offset=4
    local.get 0
    i32.load
    local.tee 0
    i32.const 4
    i32.add
    local.get 3
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $f_lam_9 (type $fun_2_1) (param i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 0
    i32.store
    local.get 1
    local.get 0
    i32.store offset=4
    local.get 1)
  (func $f_lam_11 (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 5
    i32.const 20
    i32.add
    global.set 0
    local.get 5
    i32.const 10
    i32.store
    local.get 5
    i32.const 23
    i32.store offset=4
    local.get 5
    local.get 0
    i32.store offset=8
    local.get 5
    local.get 1
    i32.store offset=12
    local.get 5
    local.get 2
    i32.store offset=16
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 4
    i32.store
    local.get 1
    i32.const 24
    i32.store offset=4
    global.get 0
    local.tee 0
    i32.const 12
    i32.add
    global.set 0
    local.get 0
    i32.const 5
    i32.store
    local.get 0
    i32.const 25
    i32.store offset=4
    local.get 0
    local.get 3
    i32.store offset=8
    local.get 2
    local.get 5
    local.get 1
    local.get 0
    local.get 4
    call $__mon_prompt)
  (func $f_lam_13 (type $fun_3_1) (param i32 i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 12
    i32.add
    global.set 0
    local.get 1
    i32.const 5
    i32.store
    local.get 1
    i32.const 27
    i32.store offset=4
    local.get 1
    local.get 0
    i32.store offset=8
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0)
  (func $f_lam_14 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 4
    i32.const 12
    i32.add
    global.set 0
    local.get 4
    i32.const 7
    i32.store
    local.get 4
    i32.const 28
    i32.store offset=4
    local.get 4
    local.get 1
    i32.store offset=8
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    local.get 2
    i32.store
    local.get 1
    local.get 4
    i32.store offset=4
    local.get 0
    i32.load
    local.tee 0
    i32.const 4
    i32.add
    local.get 3
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $f_lam_17 (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 5
    i32.const 20
    i32.add
    global.set 0
    local.get 5
    i32.const 10
    i32.store
    local.get 5
    i32.const 29
    i32.store offset=4
    local.get 5
    local.get 0
    i32.store offset=8
    local.get 5
    local.get 1
    i32.store offset=12
    local.get 5
    local.get 2
    i32.store offset=16
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 4
    i32.store
    local.get 1
    i32.const 30
    i32.store offset=4
    global.get 0
    local.tee 0
    i32.const 12
    i32.add
    global.set 0
    local.get 0
    i32.const 5
    i32.store
    local.get 0
    i32.const 31
    i32.store offset=4
    local.get 0
    local.get 3
    i32.store offset=8
    local.get 2
    local.get 5
    local.get 1
    local.get 0
    local.get 4
    call $__mon_prompt)
  (func $f_lam_19 (type $fun_3_1) (param i32 i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 12
    i32.add
    global.set 0
    local.get 1
    i32.const 5
    i32.store
    local.get 1
    i32.const 33
    i32.store offset=4
    local.get 1
    local.get 0
    i32.store offset=8
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0)
  (func $f_lam_20 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 4
    i32.const 12
    i32.add
    global.set 0
    local.get 4
    i32.const 7
    i32.store
    local.get 4
    i32.const 34
    i32.store offset=4
    local.get 4
    local.get 1
    i32.store offset=8
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    local.get 2
    i32.store
    local.get 1
    local.get 4
    i32.store offset=4
    local.get 0
    i32.load
    local.tee 0
    i32.const 4
    i32.add
    local.get 3
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $f_lam_23 (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 5
    i32.const 20
    i32.add
    global.set 0
    local.get 5
    i32.const 10
    i32.store
    local.get 5
    i32.const 35
    i32.store offset=4
    local.get 5
    local.get 0
    i32.store offset=8
    local.get 5
    local.get 1
    i32.store offset=12
    local.get 5
    local.get 2
    i32.store offset=16
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 4
    i32.store
    local.get 1
    i32.const 36
    i32.store offset=4
    global.get 0
    local.tee 0
    i32.const 12
    i32.add
    global.set 0
    local.get 0
    i32.const 5
    i32.store
    local.get 0
    i32.const 37
    i32.store offset=4
    local.get 0
    local.get 3
    i32.store offset=8
    local.get 2
    local.get 5
    local.get 1
    local.get 0
    local.get 4
    call $__mon_prompt)
  (func $f_lam_24 (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 4
    i32.const 20
    i32.add
    global.set 0
    local.get 4
    i32.const 11
    i32.store
    local.get 4
    i32.const 38
    i32.store offset=4
    local.get 4
    local.get 0
    i32.store offset=8
    local.get 4
    local.get 1
    i32.store offset=12
    local.get 4
    local.get 2
    i32.store offset=16
    local.get 3
    i32.const 4
    i32.add
    local.get 4
    local.get 3
    i32.load
    call_indirect (type $fun_2_1))
  (func $f_lam_25 (type $fun_6_1) (param i32 i32 i32 i32 i32 i32) (result i32)
    local.get 3
    i32.load offset=8
    local.tee 3
    i32.const 4
    i32.add
    local.get 4
    local.get 3
    i32.load
    call_indirect (type $fun_2_1)
    global.get 0
    local.tee 3
    i32.const 20
    i32.add
    global.set 0
    local.get 3
    i32.const 10
    i32.store
    local.get 3
    i32.const 39
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store offset=8
    local.get 3
    local.get 1
    i32.store offset=12
    local.get 3
    local.get 2
    i32.store offset=16
    local.get 3
    local.get 5
    call $__mon_bind)
  (func $f_lam_26 (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    local.tee 4
    i32.const 12
    i32.add
    global.set 0
    local.get 4
    i32.const 7
    i32.store
    local.get 4
    i32.const 16
    i32.store offset=4
    local.get 4
    local.get 2
    i32.store offset=8
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    local.get 3
    i32.store
    local.get 0
    local.get 4
    i32.store offset=4
    local.get 1
    i32.load
    local.tee 5
    i32.const 4
    i32.add
    local.get 4
    local.get 0
    local.get 5
    i32.load
    call_indirect (type $fun_3_1)
    local.set 0
    local.get 1
    i32.load offset=12
    i32.load
    local.tee 5
    i32.const 4
    i32.add
    local.get 0
    local.get 5
    i32.load
    call_indirect (type $fun_2_1)
    local.tee 5
    i32.load
    local.set 6
    global.get 0
    local.tee 0
    i32.const 4
    i32.add
    global.set 0
    local.get 0
    local.get 3
    local.get 6
    i32.eq
    i32.store
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 0
          i32.load offset=4
          drop
          local.get 5
          i32.load
          local.set 6
          global.get 0
          local.tee 4
          i32.const 12
          i32.add
          global.set 0
          local.get 4
          i32.const 5
          i32.store
          local.get 4
          i32.const 20
          i32.store offset=4
          local.get 4
          local.get 5
          i32.store offset=8
          global.get 0
          local.tee 0
          i32.const 20
          i32.add
          global.set 0
          local.get 0
          i32.const 11
          i32.store
          local.get 0
          i32.const 26
          i32.store offset=4
          local.get 0
          local.get 1
          i32.store offset=8
          local.get 0
          local.get 2
          i32.store offset=12
          local.get 0
          local.get 3
          i32.store offset=16
          global.get 0
          local.tee 1
          i32.const 12
          i32.add
          global.set 0
          local.get 1
          local.get 6
          i32.store
          local.get 1
          local.get 4
          i32.store offset=4
          local.get 1
          local.get 0
          i32.store offset=8
          global.get 0
          local.tee 0
          i32.const 8
          i32.add
          global.set 0
          local.get 0
          i32.const 1
          i32.store
          local.get 0
          local.get 1
          i32.store offset=4
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=4
        drop
        global.get 0
        local.tee 0
        global.set 0
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 5
                i32.load offset=4
                local.tee 5
                i32.const 4
                i32.add
                local.get 0
                local.get 4
                local.get 5
                i32.load
                call_indirect (type $fun_3_1)
                local.tee 0
                i32.load
                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
              end
              local.get 0
              i32.load offset=4
              local.set 5
              global.get 0
              local.tee 0
              i32.const 20
              i32.add
              global.set 0
              local.get 0
              i32.const 11
              i32.store
              local.get 0
              i32.const 32
              i32.store offset=4
              local.get 0
              local.get 1
              i32.store offset=8
              local.get 0
              local.get 2
              i32.store offset=12
              local.get 0
              local.get 3
              i32.store offset=16
              local.get 5
              i32.const 4
              i32.add
              local.get 0
              local.get 4
              local.get 5
              i32.load
              call_indirect (type $fun_3_1)
              local.set 0
              br 2 (;@3;)
            end
            local.get 0
            i32.load offset=4
            local.tee 4
            i32.load
            local.set 5
            local.get 4
            i32.load offset=4
            local.set 6
            global.get 0
            local.tee 0
            i32.const 24
            i32.add
            global.set 0
            local.get 0
            i32.const 13
            i32.store
            local.get 0
            i32.const 40
            i32.store offset=4
            local.get 0
            local.get 1
            i32.store offset=8
            local.get 0
            local.get 2
            i32.store offset=12
            local.get 0
            local.get 3
            i32.store offset=16
            local.get 0
            local.get 4
            i32.store offset=20
            global.get 0
            local.tee 1
            i32.const 12
            i32.add
            global.set 0
            local.get 1
            local.get 5
            i32.store
            local.get 1
            local.get 6
            i32.store offset=4
            local.get 1
            local.get 0
            i32.store offset=8
            global.get 0
            local.tee 0
            i32.const 8
            i32.add
            global.set 0
            local.get 0
            i32.const 1
            i32.store
            local.get 0
            local.get 1
            i32.store offset=4
            br 1 (;@3;)
          end
          unreachable
        end
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0)
  (func $f_lam_27 (type $fun_3_1) (param i32 i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 2
    global.set 0
    global.get 1
    local.tee 3
    i32.const 1
    i32.add
    global.set 1
    global.get 0
    local.tee 0
    i32.const 24
    i32.add
    global.set 0
    local.get 0
    i32.const 12
    i32.store
    local.get 0
    i32.const 41
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store offset=8
    local.get 0
    local.get 1
    i32.store offset=12
    local.get 0
    local.get 2
    i32.store offset=16
    local.get 0
    local.get 3
    i32.store offset=20
    local.get 0)
  (func $main (type $fun_0_1) (result i32)
    (local i32 i32 i32 i32)
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 3
    i32.store
    local.get 0
    i32.const 50
    i32.store offset=4
    global.get 0
    local.tee 1
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.const 4
            i32.add
            local.get 1
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.tee 0
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 0
          i32.load offset=4
          local.set 0
          global.get 0
          local.tee 1
          global.set 0
          local.get 0
          i32.const 4
          i32.add
          i32.const 16777215
          local.get 1
          local.get 0
          i32.load
          call_indirect (type $fun_3_1)
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=4
        local.tee 1
        i32.load
        local.set 2
        local.get 1
        i32.load offset=4
        local.set 3
        global.get 0
        local.tee 0
        i32.const 12
        i32.add
        global.set 0
        local.get 0
        i32.const 7
        i32.store
        local.get 0
        i32.const 52
        i32.store offset=4
        local.get 0
        local.get 1
        i32.store offset=8
        global.get 0
        local.tee 1
        i32.const 12
        i32.add
        global.set 0
        local.get 1
        local.get 2
        i32.store
        local.get 1
        local.get 3
        i32.store offset=4
        local.get 1
        local.get 0
        i32.store offset=8
        global.get 0
        local.tee 0
        i32.const 8
        i32.add
        global.set 0
        local.get 0
        i32.const 1
        i32.store
        local.get 0
        local.get 1
        i32.store offset=4
        br 1 (;@1;)
      end
      unreachable
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 0
          i32.load offset=4
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=4
        drop
        i32.const 5467
        local.set 0
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0)
  (func $main_lam_0 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 1)
  (func $main_lam_1 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 1
    i32.const 4
    i32.add
    local.get 2
    local.get 1
    i32.load
    call_indirect (type $fun_2_1))
  (func $main_lam_2 (type $fun_1_1) (param i32) (result i32)
    global.get 0
    local.tee 0
    global.set 0
    local.get 0)
  (func $main_lam_3 (type $fun_1_1) (param i32) (result i32)
    local.get 0
    i32.load
    drop
    unreachable)
  (func $main_lam_4 (type $fun_1_1) (param i32) (result i32)
    local.get 0)
  (func $main_lam_6 (type $fun_1_1) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    global.get 0
    local.tee 3
    i32.const 8
    i32.add
    global.set 0
    local.get 3
    i32.const 4
    i32.store
    local.get 3
    i32.const 44
    i32.store offset=4
    global.get 0
    local.tee 4
    i32.const 8
    i32.add
    global.set 0
    local.get 4
    i32.const 6
    i32.store
    local.get 4
    i32.const 45
    i32.store offset=4
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 3
    i32.store
    local.get 1
    i32.const 46
    i32.store offset=4
    global.get 0
    local.tee 2
    i32.const 8
    i32.add
    global.set 0
    local.get 2
    i32.const 3
    i32.store
    local.get 2
    i32.const 47
    i32.store offset=4
    global.get 0
    local.tee 5
    i32.const 8
    i32.add
    global.set 0
    local.get 5
    local.get 1
    i32.store
    local.get 5
    local.get 2
    i32.store offset=4
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 3
    i32.store
    local.get 1
    i32.const 48
    i32.store offset=4
    global.get 0
    local.tee 2
    i32.const 8
    i32.add
    global.set 0
    local.get 2
    i32.const 3
    i32.store
    local.get 2
    i32.const 49
    i32.store offset=4
    global.get 0
    local.tee 6
    i32.const 8
    i32.add
    global.set 0
    local.get 6
    local.get 1
    i32.store
    local.get 6
    local.get 2
    i32.store offset=4
    global.get 0
    local.tee 1
    i32.const 16
    i32.add
    global.set 0
    local.get 1
    local.get 3
    i32.store
    local.get 1
    local.get 4
    i32.store offset=4
    local.get 1
    local.get 5
    i32.store offset=8
    local.get 1
    local.get 6
    i32.store offset=12
    local.get 1
    local.get 0
    call $f)
  (func $main_lam_7 (type $fun_1_1) (param i32) (result i32)
    local.get 0
    i32.const 4
    i32.add
    i32.const 16777215
    local.get 0
    i32.load
    call_indirect (type $fun_2_1))
  (func $main_lam_8 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load offset=8
    local.tee 0
    i32.const 4
    i32.add
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_2_1)
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 3
    i32.store
    local.get 0
    i32.const 51
    i32.store offset=4
    local.get 0
    local.get 2
    call $__mon_bind)
  (func $__mon_bind (type $fun_3_1) (param i32 i32 i32) (result i32)
    (local i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.const 4
            i32.add
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_2_1)
            local.tee 0
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 1
          i32.const 4
          i32.add
          local.get 0
          i32.load offset=4
          local.get 2
          local.get 1
          i32.load
          call_indirect (type $fun_3_1)
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=4
        local.tee 2
        i32.load
        local.set 3
        local.get 2
        i32.load offset=4
        local.set 4
        global.get 0
        local.tee 0
        i32.const 16
        i32.add
        global.set 0
        local.get 0
        i32.const 8
        i32.store
        local.get 0
        i32.const 54
        i32.store offset=4
        local.get 0
        local.get 1
        i32.store offset=8
        local.get 0
        local.get 2
        i32.store offset=12
        global.get 0
        local.tee 1
        i32.const 12
        i32.add
        global.set 0
        local.get 1
        local.get 3
        i32.store
        local.get 1
        local.get 4
        i32.store offset=4
        local.get 1
        local.get 0
        i32.store offset=8
        global.get 0
        local.tee 0
        i32.const 8
        i32.add
        global.set 0
        local.get 0
        i32.const 1
        i32.store
        local.get 0
        local.get 1
        i32.store offset=4
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0)
  (func $__mon_bind_lam_0 (type $fun_3_1) (param i32 i32 i32) (result i32)
    local.get 1
    i32.load offset=8
    local.tee 1
    i32.const 4
    i32.add
    local.get 2
    local.get 1
    i32.load
    call_indirect (type $fun_2_1)
    local.set 2
    global.get 0
    local.tee 1
    i32.const 16
    i32.add
    global.set 0
    local.get 1
    i32.const 8
    i32.store
    local.get 1
    i32.const 53
    i32.store offset=4
    local.get 1
    local.get 2
    i32.store offset=8
    local.get 1
    local.get 0
    i32.store offset=12
    local.get 1)
  (func $__mon_prompt (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.const 4
            i32.add
            local.get 1
            i32.const 4
            i32.add
            local.get 4
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.get 3
            i32.load
            call_indirect (type $fun_2_1)
            local.tee 3
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 2
          i32.const 4
          i32.add
          local.get 3
          i32.load offset=4
          local.get 4
          local.get 2
          i32.load
          call_indirect (type $fun_3_1)
          local.set 0
          br 2 (;@1;)
        end
        local.get 3
        i32.load offset=4
        local.tee 5
        i32.load
        local.set 6
        global.get 0
        local.tee 3
        i32.const 4
        i32.add
        global.set 0
        local.get 3
        local.get 0
        local.get 6
        i32.eq
        i32.store
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 3
                i32.load
                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
              end
              local.get 3
              i32.load offset=4
              drop
              local.get 5
              i32.load
              local.set 4
              local.get 5
              i32.load offset=4
              local.set 6
              global.get 0
              local.tee 3
              i32.const 24
              i32.add
              global.set 0
              local.get 3
              i32.const 13
              i32.store
              local.get 3
              i32.const 56
              i32.store offset=4
              local.get 3
              local.get 0
              i32.store offset=8
              local.get 3
              local.get 1
              i32.store offset=12
              local.get 3
              local.get 2
              i32.store offset=16
              local.get 3
              local.get 5
              i32.store offset=20
              global.get 0
              local.tee 1
              i32.const 12
              i32.add
              global.set 0
              local.get 1
              local.get 4
              i32.store
              local.get 1
              local.get 6
              i32.store offset=4
              local.get 1
              local.get 3
              i32.store offset=8
              global.get 0
              local.tee 0
              i32.const 8
              i32.add
              global.set 0
              local.get 0
              i32.const 1
              i32.store
              local.get 0
              local.get 1
              i32.store offset=4
              br 2 (;@3;)
            end
            local.get 3
            i32.load offset=4
            drop
            global.get 0
            local.tee 3
            i32.const 24
            i32.add
            global.set 0
            local.get 3
            i32.const 13
            i32.store
            local.get 3
            i32.const 57
            i32.store offset=4
            local.get 3
            local.get 0
            i32.store offset=8
            local.get 3
            local.get 1
            i32.store offset=12
            local.get 3
            local.get 2
            i32.store offset=16
            local.get 3
            local.get 5
            i32.store offset=20
            local.get 5
            i32.load offset=4
            local.tee 0
            i32.const 4
            i32.add
            local.get 3
            local.get 4
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
            local.set 0
            br 1 (;@3;)
          end
          unreachable
        end
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0)
  (func $__mon_prompt_lam_0 (type $fun_6_1) (param i32 i32 i32 i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    i32.load offset=8
    local.tee 0
    i32.const 4
    i32.add
    local.get 4
    local.get 0
    i32.load
    call_indirect (type $fun_2_1)
    local.get 5
    call $__mon_prompt)
  (table (;0;) 58 58 funcref)
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 0))
  (global (;1;) (mut i32) (i32.const 0))
  (export "main" (func $main))
  (export "mem" (memory 0))
  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_1 $__apply_3_2 $__apply_4_2 $__apply_4_3 $__apply_5_3 $__apply_5_4 $__apply_6_4 $f $f_lam_0 $f_lam_1 $f_lam_0 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_0 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_9 $f_lam_11 $f_lam_0 $f_lam_13 $f_lam_14 $f_lam_9 $f_lam_9 $f_lam_17 $f_lam_0 $f_lam_19 $f_lam_20 $f_lam_9 $f_lam_9 $f_lam_23 $f_lam_24 $f_lam_25 $f_lam_26 $f_lam_27 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_4 $main_lam_6 $main_lam_7 $main_lam_8 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_0))
