(module $state
  (type $fun_2_1 (func (param i32 i32) (result i32)))
  (type $fun_3_1 (func (param i32 i32 i32) (result i32)))
  (type $fun_1_1 (func (param i32) (result i32)))
  (type $fun_0_1 (func (result i32)))
  (type $fun_4_1 (func (param i32 i32 i32 i32) (result i32)))
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
  (func $main (type $fun_0_1) (result i32)
    (local i32 i32 i32 i32)
    global.get 1
    local.tee 3
    i32.const 1
    i32.add
    global.set 1
    global.get 0
    local.tee 1
    i32.const 8
    i32.add
    global.set 0
    local.get 1
    i32.const 4
    i32.store
    local.get 1
    i32.const 9
    i32.store offset=4
    global.get 0
    local.tee 2
    i32.const 8
    i32.add
    global.set 0
    local.get 2
    i32.const 4
    i32.store
    local.get 2
    i32.const 13
    i32.store offset=4
    global.get 0
    local.tee 0
    i32.const 12
    i32.add
    global.set 0
    local.get 0
    local.get 3
    i32.store
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0
    local.get 2
    i32.store offset=8
    local.get 0
    i32.load offset=8
    local.set 1
    global.get 0
    local.tee 2
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.load offset=4
            local.tee 0
            i32.const 4
            i32.add
            local.get 1
            local.get 2
            local.get 0
            i32.load
            call_indirect (type $fun_3_1)
            local.tee 0
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 0
          i32.load offset=4
          local.tee 0
          i32.const 4
          i32.add
          i32.const 16777215
          local.get 0
          i32.load
          call_indirect (type $fun_2_1)
          local.set 1
          global.get 0
          local.tee 0
          i32.const 8
          i32.add
          global.set 0
          local.get 0
          i32.const 0
          i32.store
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
        i32.const 6
        i32.store
        local.get 0
        i32.const 16
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
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0
    local.get 1
    i32.store offset=4
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
          i32.load offset=4
          local.set 1
          global.get 0
          local.tee 0
          i32.const 8
          i32.add
          global.set 0
          local.get 0
          i32.const 0
          i32.store
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
        i32.const 6
        i32.store
        local.get 0
        i32.const 18
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
        br 1 (;@1;)
      end
      unreachable
    end
    local.get 0
    local.get 1
    i32.store offset=4
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
    local.get 0
    i32.const 4
    i32.add
    local.get 1
    local.get 1
    local.get 0
    i32.load
    call_indirect (type $fun_3_1))
  (func $main_lam_1 (type $fun_2_1) (param i32 i32) (result i32)
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
  (func $main_lam_2 (type $fun_2_1) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 2
    i32.const 8
    i32.add
    global.set 0
    local.get 2
    local.get 1
    i32.store
    local.get 2
    local.get 0
    i32.store offset=4
    local.get 2)
  (func $main_lam_3 (type $fun_2_1) (param i32 i32) (result i32)
    global.get 0
    local.tee 1
    i32.const 12
    i32.add
    global.set 0
    local.get 1
    i32.const 5
    i32.store
    local.get 1
    i32.const 11
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
  (func $main_lam_4 (type $fun_2_1) (param i32 i32) (result i32)
    (local i32)
    global.get 0
    local.tee 2
    i32.const 12
    i32.add
    global.set 0
    local.get 2
    i32.const 5
    i32.store
    local.get 2
    i32.const 10
    i32.store offset=4
    local.get 2
    local.get 0
    i32.store offset=8
    global.get 0
    local.tee 0
    i32.const 8
    i32.add
    global.set 0
    local.get 0
    i32.const 4
    i32.store
    local.get 0
    i32.const 12
    i32.store offset=4
    local.get 2
    local.get 0
    local.get 1
    call $__mon_bind)
  (func $main_lam_6 (type $fun_1_1) (param i32) (result i32)
    (local i32)
    local.get 0
    i32.const 4
    i32.add
    i32.const 16777215
    local.get 0
    i32.load
    call_indirect (type $fun_2_1)
    local.set 1
    global.get 0
    local.tee 0
    i32.const 12
    i32.add
    global.set 0
    local.get 0
    i32.const 5
    i32.store
    local.get 0
    i32.const 14
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store offset=8
    local.get 0)
  (func $main_lam_7 (type $fun_3_1) (param i32 i32 i32) (result i32)
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
    i32.const 15
    i32.store offset=4
    local.get 0
    local.get 2
    call $__mon_bind)
  (func $main_lam_8 (type $fun_2_1) (param i32 i32) (result i32)
    local.get 0
    i32.load offset=4
    local.set 1
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
  (func $main_lam_9 (type $fun_3_1) (param i32 i32 i32) (result i32)
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
    i32.const 4
    i32.store
    local.get 0
    i32.const 17
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
        i32.const 7
        i32.store
        local.get 0
        i32.const 20
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
    i32.const 7
    i32.store
    local.get 1
    i32.const 19
    i32.store offset=4
    local.get 1
    local.get 2
    i32.store offset=8
    local.get 1
    local.get 0
    i32.store offset=12
    local.get 1)
  (func $__mon_prompt (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.const 4
            i32.add
            local.get 1
            i32.const 4
            i32.add
            local.get 3
            local.get 1
            i32.load
            call_indirect (type $fun_2_1)
            local.get 2
            i32.load
            call_indirect (type $fun_2_1)
            local.tee 1
            i32.load
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          local.get 1
          i32.load offset=4
          local.set 1
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
          br 2 (;@1;)
        end
        local.get 1
        i32.load offset=4
        local.tee 2
        i32.load
        local.set 4
        global.get 0
        local.tee 1
        i32.const 4
        i32.add
        global.set 0
        local.get 1
        local.get 0
        local.get 4
        i32.eq
        i32.store
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 1
                i32.load
                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
              end
              local.get 1
              i32.load offset=4
              drop
              local.get 2
              i32.load
              local.set 0
              local.get 2
              i32.load offset=4
              local.set 3
              local.get 2
              i32.load offset=8
              local.set 2
              global.get 0
              local.tee 1
              i32.const 12
              i32.add
              global.set 0
              local.get 1
              local.get 0
              i32.store
              local.get 1
              local.get 3
              i32.store offset=4
              local.get 1
              local.get 2
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
            local.get 1
            i32.load offset=4
            drop
            local.get 2
            i32.load offset=4
            local.tee 0
            i32.const 4
            i32.add
            local.get 2
            i32.load offset=8
            local.get 3
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
  (table (;0;) 22 22 funcref)
  (memory (;0;) 1)
  (global (;0;) (mut i32) (i32.const 0))
  (global (;1;) (mut i32) (i32.const 0))
  (export "main" (func $main))
  (export "mem" (memory 0))
  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_1 $__apply_3_2 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_1 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $__mon_bind $__mon_bind_lam_0 $__mon_prompt))
