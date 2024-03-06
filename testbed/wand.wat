(module $multi
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_5_1 (;4;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@75    ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@76    ;)    (local i32)
(;@78    ;)    global.get 1
(;@7a    ;)    global.get 1
(;@7c    ;)    i32.const 1
(;@7e    ;)    i32.add
(;@7f    ;)    global.set 1
(;@81    ;)    return
             )
(;@84    ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@85    ;)    (local i32)
(;@87    ;)    global.get 0
(;@89    ;)    global.get 0
(;@8b    ;)    local.get 0
(;@8d    ;)    i32.add
(;@8e    ;)    global.set 0
(;@90    ;)    return
             )
(;@93    ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@94    ;)    (local i32)
(;@96    ;)    i32.const 4
(;@98    ;)    call $alloc
(;@9a    ;)    local.tee 2
(;@9c    ;)    i32.const 1
(;@9e    ;)    i32.const 0
(;@a0    ;)    local.get 0
(;@a2    ;)    local.get 1
(;@a4    ;)    i32.eq
(;@a5    ;)    select
(;@a6    ;)    i32.store
(;@a9    ;)    local.get 2
(;@ab    ;)    return
             )
(;@ae    ;)  (func $__call_1 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@af    ;)    (local i32)
(;@b1    ;)    local.get 0
(;@b3    ;)    i32.const 12
(;@b5    ;)    i32.add
(;@b6    ;)    local.set 2
(;@b8    ;)    block ;; label = @1
(;@ba    ;)      block ;; label = @2
(;@bc    ;)        block ;; label = @3
(;@be    ;)          local.get 0
(;@c0    ;)          i32.load
(;@c3    ;)          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
(;@c8    ;)        end
(;@c9    ;)        local.get 2
(;@cb    ;)        local.get 0
(;@cd    ;)        i32.load offset=8
(;@d0    ;)        call_indirect (type $fun_1_1)
(;@d3    ;)        local.get 1
(;@d5    ;)        call $__call_1
(;@d7    ;)        return
(;@d8    ;)      end
(;@d9    ;)      local.get 2
(;@db    ;)      local.get 1
(;@dd    ;)      local.get 0
(;@df    ;)      i32.load offset=8
(;@e2    ;)      call_indirect (type $fun_2_1)
(;@e5    ;)      return
(;@e6    ;)    end
(;@e7    ;)    unreachable
             )
(;@ea    ;)  (func $__call_2 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@eb    ;)    (local i32)
(;@ed    ;)    local.get 0
(;@ef    ;)    i32.const 12
(;@f1    ;)    i32.add
(;@f2    ;)    local.set 3
(;@f4    ;)    block ;; label = @1
(;@f6    ;)      block ;; label = @2
(;@f8    ;)        block ;; label = @3
(;@fa    ;)          block ;; label = @4
(;@fc    ;)            local.get 0
(;@fe    ;)            i32.load
(;@101   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 3 (;@1;)
(;@107   ;)          end
(;@108   ;)          local.get 3
(;@10a   ;)          local.get 0
(;@10c   ;)          i32.load offset=8
(;@10f   ;)          call_indirect (type $fun_1_1)
(;@112   ;)          local.get 1
(;@114   ;)          local.get 2
(;@116   ;)          call $__call_2
(;@118   ;)          return
(;@119   ;)        end
(;@11a   ;)        local.get 3
(;@11c   ;)        local.get 1
(;@11e   ;)        local.get 0
(;@120   ;)        i32.load offset=8
(;@123   ;)        call_indirect (type $fun_2_1)
(;@126   ;)        local.get 2
(;@128   ;)        call $__call_1
(;@12a   ;)        return
(;@12b   ;)      end
(;@12c   ;)      local.get 3
(;@12e   ;)      local.get 1
(;@130   ;)      local.get 2
(;@132   ;)      local.get 0
(;@134   ;)      i32.load offset=8
(;@137   ;)      call_indirect (type $fun_3_1)
(;@13a   ;)      return
(;@13b   ;)    end
(;@13c   ;)    unreachable
             )
(;@140   ;)  (func $__mon_bind (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@141   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@143   ;)    local.get 0
(;@145   ;)    local.get 2
(;@147   ;)    call $__call_1
(;@149   ;)    local.set 3
(;@14b   ;)    local.get 3
(;@14d   ;)    i32.load
(;@150   ;)    local.set 4
(;@152   ;)    block (result i32) ;; label = @1
(;@154   ;)      block ;; label = @2
(;@156   ;)        block ;; label = @3
(;@158   ;)          block ;; label = @4
(;@15a   ;)            local.get 4
(;@15c   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@161   ;)          end
(;@162   ;)          local.get 3
(;@164   ;)          i32.load offset=4
(;@167   ;)          local.set 5
(;@169   ;)          local.get 1
(;@16b   ;)          local.get 5
(;@16d   ;)          local.get 2
(;@16f   ;)          call $__call_2
(;@171   ;)          br 2 (;@1;)
(;@173   ;)        end
(;@174   ;)        local.get 3
(;@176   ;)        i32.load offset=4
(;@179   ;)        local.set 6
(;@17b   ;)        local.get 6
(;@17d   ;)        local.set 7
(;@17f   ;)        local.get 7
(;@181   ;)        i32.load
(;@184   ;)        local.set 8
(;@186   ;)        local.get 7
(;@188   ;)        i32.load offset=4
(;@18b   ;)        local.set 9
(;@18d   ;)        i32.const 20
(;@18f   ;)        call $alloc
(;@191   ;)        local.set 10
(;@193   ;)        local.get 10
(;@195   ;)        i32.const 1
(;@197   ;)        i32.store
(;@19a   ;)        local.get 10
(;@19c   ;)        i32.const 2
(;@19e   ;)        i32.store offset=4
(;@1a1   ;)        local.get 10
(;@1a3   ;)        i32.const 6
(;@1a5   ;)        i32.store offset=8
(;@1a8   ;)        local.get 10
(;@1aa   ;)        local.get 1
(;@1ac   ;)        i32.store offset=12
(;@1af   ;)        local.get 10
(;@1b1   ;)        local.get 7
(;@1b3   ;)        i32.store offset=16
(;@1b6   ;)        local.get 10
(;@1b8   ;)        local.set 10
(;@1ba   ;)        i32.const 12
(;@1bc   ;)        call $alloc
(;@1be   ;)        local.set 11
(;@1c0   ;)        local.get 11
(;@1c2   ;)        local.get 8
(;@1c4   ;)        i32.store
(;@1c7   ;)        local.get 11
(;@1c9   ;)        local.get 9
(;@1cb   ;)        i32.store offset=4
(;@1ce   ;)        local.get 11
(;@1d0   ;)        local.get 10
(;@1d2   ;)        i32.store offset=8
(;@1d5   ;)        local.get 11
(;@1d7   ;)        local.set 11
(;@1d9   ;)        i32.const 8
(;@1db   ;)        call $alloc
(;@1dd   ;)        local.set 12
(;@1df   ;)        local.get 12
(;@1e1   ;)        i32.const 1
(;@1e3   ;)        i32.store
(;@1e6   ;)        local.get 12
(;@1e8   ;)        local.get 11
(;@1ea   ;)        i32.store offset=4
(;@1ed   ;)        local.get 12
(;@1ef   ;)        br 1 (;@1;)
(;@1f1   ;)      end
(;@1f2   ;)      unreachable
(;@1f3   ;)    end
(;@1f4   ;)    return
             )
(;@1f7   ;)  (func $__mon_bind_lam_0 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1f8   ;)    (local i32 i32 i32 i32 i32)
(;@1fa   ;)    local.get 0
(;@1fc   ;)    i32.load
(;@1ff   ;)    local.set 2
(;@201   ;)    local.get 0
(;@203   ;)    i32.load offset=4
(;@206   ;)    local.set 3
(;@208   ;)    local.get 3
(;@20a   ;)    i32.load offset=8
(;@20d   ;)    local.set 4
(;@20f   ;)    local.get 4
(;@211   ;)    local.get 1
(;@213   ;)    call $__call_1
(;@215   ;)    local.set 5
(;@217   ;)    i32.const 12
(;@219   ;)    call $alloc
(;@21b   ;)    local.set 6
(;@21d   ;)    local.get 6
(;@21f   ;)    i32.const 2
(;@221   ;)    i32.store
(;@224   ;)    local.get 6
(;@226   ;)    i32.const 2
(;@228   ;)    i32.store offset=4
(;@22b   ;)    local.get 6
(;@22d   ;)    i32.const 5
(;@22f   ;)    i32.store offset=8
(;@232   ;)    local.get 6
(;@234   ;)    local.get 5
(;@236   ;)    i32.store offset=12
(;@239   ;)    local.get 6
(;@23b   ;)    local.get 2
(;@23d   ;)    i32.store offset=16
(;@240   ;)    local.get 6
(;@242   ;)    return
             )
(;@246   ;)  (func $__mon_prompt (;7;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@247   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@249   ;)    local.get 1
(;@24b   ;)    local.get 4
(;@24d   ;)    call $__call_1
(;@24f   ;)    local.set 5
(;@251   ;)    local.get 3
(;@253   ;)    local.get 5
(;@255   ;)    call $__call_1
(;@257   ;)    local.set 6
(;@259   ;)    local.get 6
(;@25b   ;)    i32.load
(;@25e   ;)    local.set 7
(;@260   ;)    block (result i32) ;; label = @1
(;@262   ;)      block ;; label = @2
(;@264   ;)        block ;; label = @3
(;@266   ;)          block ;; label = @4
(;@268   ;)            local.get 7
(;@26a   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@26f   ;)          end
(;@270   ;)          local.get 6
(;@272   ;)          i32.load offset=4
(;@275   ;)          local.set 8
(;@277   ;)          local.get 2
(;@279   ;)          local.get 8
(;@27b   ;)          local.get 4
(;@27d   ;)          call $__call_2
(;@27f   ;)          br 2 (;@1;)
(;@281   ;)        end
(;@282   ;)        local.get 6
(;@284   ;)        i32.load offset=4
(;@287   ;)        local.set 9
(;@289   ;)        local.get 9
(;@28b   ;)        i32.load
(;@28e   ;)        local.set 10
(;@290   ;)        local.get 0
(;@292   ;)        local.get 10
(;@294   ;)        call $__mon_eqm
(;@296   ;)        local.set 11
(;@298   ;)        local.get 11
(;@29a   ;)        i32.load
(;@29d   ;)        local.set 12
(;@29f   ;)        block (result i32) ;; label = @3
(;@2a1   ;)          block ;; label = @4
(;@2a3   ;)            block ;; label = @5
(;@2a5   ;)              block ;; label = @6
(;@2a7   ;)                local.get 12
(;@2a9   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@2ae   ;)              end
(;@2af   ;)              local.get 11
(;@2b1   ;)              i32.load offset=4
(;@2b4   ;)              local.set 13
(;@2b6   ;)              local.get 9
(;@2b8   ;)              local.set 14
(;@2ba   ;)              local.get 14
(;@2bc   ;)              i32.load
(;@2bf   ;)              local.set 15
(;@2c1   ;)              local.get 14
(;@2c3   ;)              i32.load offset=4
(;@2c6   ;)              local.set 16
(;@2c8   ;)              i32.const 28
(;@2ca   ;)              call $alloc
(;@2cc   ;)              local.set 17
(;@2ce   ;)              local.get 17
(;@2d0   ;)              i32.const 2
(;@2d2   ;)              i32.store
(;@2d5   ;)              local.get 17
(;@2d7   ;)              i32.const 4
(;@2d9   ;)              i32.store offset=4
(;@2dc   ;)              local.get 17
(;@2de   ;)              i32.const 8
(;@2e0   ;)              i32.store offset=8
(;@2e3   ;)              local.get 17
(;@2e5   ;)              local.get 0
(;@2e7   ;)              i32.store offset=12
(;@2ea   ;)              local.get 17
(;@2ec   ;)              local.get 1
(;@2ee   ;)              i32.store offset=16
(;@2f1   ;)              local.get 17
(;@2f3   ;)              local.get 2
(;@2f5   ;)              i32.store offset=20
(;@2f8   ;)              local.get 17
(;@2fa   ;)              local.get 14
(;@2fc   ;)              i32.store offset=24
(;@2ff   ;)              local.get 17
(;@301   ;)              local.set 17
(;@303   ;)              i32.const 12
(;@305   ;)              call $alloc
(;@307   ;)              local.set 18
(;@309   ;)              local.get 18
(;@30b   ;)              local.get 15
(;@30d   ;)              i32.store
(;@310   ;)              local.get 18
(;@312   ;)              local.get 16
(;@314   ;)              i32.store offset=4
(;@317   ;)              local.get 18
(;@319   ;)              local.get 17
(;@31b   ;)              i32.store offset=8
(;@31e   ;)              local.get 18
(;@320   ;)              local.set 18
(;@322   ;)              i32.const 8
(;@324   ;)              call $alloc
(;@326   ;)              local.set 19
(;@328   ;)              local.get 19
(;@32a   ;)              i32.const 1
(;@32c   ;)              i32.store
(;@32f   ;)              local.get 19
(;@331   ;)              local.get 18
(;@333   ;)              i32.store offset=4
(;@336   ;)              local.get 19
(;@338   ;)              br 2 (;@3;)
(;@33a   ;)            end
(;@33b   ;)            local.get 11
(;@33d   ;)            i32.load offset=4
(;@340   ;)            local.set 13
(;@342   ;)            i32.const 28
(;@344   ;)            call $alloc
(;@346   ;)            local.set 19
(;@348   ;)            local.get 19
(;@34a   ;)            i32.const 2
(;@34c   ;)            i32.store
(;@34f   ;)            local.get 19
(;@351   ;)            i32.const 4
(;@353   ;)            i32.store offset=4
(;@356   ;)            local.get 19
(;@358   ;)            i32.const 9
(;@35a   ;)            i32.store offset=8
(;@35d   ;)            local.get 19
(;@35f   ;)            local.get 0
(;@361   ;)            i32.store offset=12
(;@364   ;)            local.get 19
(;@366   ;)            local.get 1
(;@368   ;)            i32.store offset=16
(;@36b   ;)            local.get 19
(;@36d   ;)            local.get 2
(;@36f   ;)            i32.store offset=20
(;@372   ;)            local.get 19
(;@374   ;)            local.get 9
(;@376   ;)            i32.store offset=24
(;@379   ;)            local.get 19
(;@37b   ;)            local.set 19
(;@37d   ;)            local.get 9
(;@37f   ;)            i32.load offset=4
(;@382   ;)            local.set 20
(;@384   ;)            local.get 20
(;@386   ;)            local.get 19
(;@388   ;)            local.get 4
(;@38a   ;)            call $__call_2
(;@38c   ;)            br 1 (;@3;)
(;@38e   ;)          end
(;@38f   ;)          unreachable
(;@390   ;)        end
(;@391   ;)        br 1 (;@1;)
(;@393   ;)      end
(;@394   ;)      unreachable
(;@395   ;)    end
(;@396   ;)    return
             )
(;@399   ;)  (func $__mon_prompt_lam_0 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@39a   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@39c   ;)    local.get 0
(;@39e   ;)    i32.load
(;@3a1   ;)    local.set 3
(;@3a3   ;)    local.get 0
(;@3a5   ;)    i32.load offset=4
(;@3a8   ;)    local.set 4
(;@3aa   ;)    local.get 0
(;@3ac   ;)    i32.load offset=8
(;@3af   ;)    local.set 5
(;@3b1   ;)    local.get 0
(;@3b3   ;)    i32.load offset=12
(;@3b6   ;)    local.set 6
(;@3b8   ;)    local.get 6
(;@3ba   ;)    i32.load offset=8
(;@3bd   ;)    local.set 7
(;@3bf   ;)    local.get 7
(;@3c1   ;)    local.get 1
(;@3c3   ;)    call $__call_1
(;@3c5   ;)    local.set 8
(;@3c7   ;)    local.get 3
(;@3c9   ;)    local.get 4
(;@3cb   ;)    local.get 5
(;@3cd   ;)    local.get 8
(;@3cf   ;)    local.get 2
(;@3d1   ;)    call $__mon_prompt
(;@3d3   ;)    return
             )
(;@3d6   ;)  (func $__mon_prompt_lam_1 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@3d7   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@3d9   ;)    local.get 0
(;@3db   ;)    i32.load
(;@3de   ;)    local.set 3
(;@3e0   ;)    local.get 0
(;@3e2   ;)    i32.load offset=4
(;@3e5   ;)    local.set 4
(;@3e7   ;)    local.get 0
(;@3e9   ;)    i32.load offset=8
(;@3ec   ;)    local.set 5
(;@3ee   ;)    local.get 0
(;@3f0   ;)    i32.load offset=12
(;@3f3   ;)    local.set 6
(;@3f5   ;)    local.get 6
(;@3f7   ;)    i32.load offset=8
(;@3fa   ;)    local.set 7
(;@3fc   ;)    local.get 7
(;@3fe   ;)    local.get 1
(;@400   ;)    call $__call_1
(;@402   ;)    local.set 8
(;@404   ;)    local.get 3
(;@406   ;)    local.get 4
(;@408   ;)    local.get 5
(;@40a   ;)    local.get 8
(;@40c   ;)    local.get 2
(;@40e   ;)    call $__mon_prompt
(;@410   ;)    return
             )
(;@3d    ;)  (table (;0;) 10 10 funcref)
(;@44    ;)  (memory (;0;) 1)
(;@49    ;)  (global (;0;) (mut i32) i32.const 0)
(;@4e    ;)  (global (;1;) (mut i32) i32.const 0)
(;@56    ;)  (export "mem" (memory 0))
(;@5f    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__call_1 $__call_2 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
