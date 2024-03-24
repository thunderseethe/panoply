(module $inner_outer
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_5_1 (;4;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@7b    ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@7c    ;)    (local i32)
(;@7e    ;)    global.get 1
(;@80    ;)    global.get 1
(;@82    ;)    i32.const 1
(;@84    ;)    i32.add
(;@85    ;)    global.set 1
(;@87    ;)    return
             )
(;@8a    ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@8b    ;)    (local i32)
(;@8d    ;)    global.get 0
(;@8f    ;)    global.get 0
(;@91    ;)    local.get 0
(;@93    ;)    i32.add
(;@94    ;)    global.set 0
(;@96    ;)    return
             )
(;@99    ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@9a    ;)    (local i32)
(;@9c    ;)    i32.const 4
(;@9e    ;)    call $alloc
(;@a0    ;)    local.tee 2
(;@a2    ;)    i32.const 1
(;@a4    ;)    i32.const 0
(;@a6    ;)    local.get 0
(;@a8    ;)    local.get 1
(;@aa    ;)    i32.eq
(;@ab    ;)    select
(;@ac    ;)    i32.store
(;@af    ;)    local.get 2
(;@b1    ;)    return
             )
(;@b4    ;)  (func $__call_1 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@b5    ;)    (local i32)
(;@b7    ;)    local.get 0
(;@b9    ;)    i32.const 12
(;@bb    ;)    i32.add
(;@bc    ;)    local.set 2
(;@be    ;)    block ;; label = @1
(;@c0    ;)      block ;; label = @2
(;@c2    ;)        block ;; label = @3
(;@c4    ;)          local.get 0
(;@c6    ;)          i32.load
(;@c9    ;)          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
(;@ce    ;)        end
(;@cf    ;)        local.get 2
(;@d1    ;)        local.get 0
(;@d3    ;)        i32.load offset=8
(;@d6    ;)        call_indirect (type $fun_1_1)
(;@d9    ;)        local.get 1
(;@db    ;)        call $__call_1
(;@dd    ;)        return
(;@de    ;)      end
(;@df    ;)      local.get 2
(;@e1    ;)      local.get 1
(;@e3    ;)      local.get 0
(;@e5    ;)      i32.load offset=8
(;@e8    ;)      call_indirect (type $fun_2_1)
(;@eb    ;)      return
(;@ec    ;)    end
(;@ed    ;)    unreachable
             )
(;@f0    ;)  (func $__call_2 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@f1    ;)    (local i32)
(;@f3    ;)    local.get 0
(;@f5    ;)    i32.const 12
(;@f7    ;)    i32.add
(;@f8    ;)    local.set 3
(;@fa    ;)    block ;; label = @1
(;@fc    ;)      block ;; label = @2
(;@fe    ;)        block ;; label = @3
(;@100   ;)          block ;; label = @4
(;@102   ;)            local.get 0
(;@104   ;)            i32.load
(;@107   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 3 (;@1;)
(;@10d   ;)          end
(;@10e   ;)          local.get 3
(;@110   ;)          local.get 0
(;@112   ;)          i32.load offset=8
(;@115   ;)          call_indirect (type $fun_1_1)
(;@118   ;)          local.get 1
(;@11a   ;)          local.get 2
(;@11c   ;)          call $__call_2
(;@11e   ;)          return
(;@11f   ;)        end
(;@120   ;)        local.get 3
(;@122   ;)        local.get 1
(;@124   ;)        local.get 0
(;@126   ;)        i32.load offset=8
(;@129   ;)        call_indirect (type $fun_2_1)
(;@12c   ;)        local.get 2
(;@12e   ;)        call $__call_1
(;@130   ;)        return
(;@131   ;)      end
(;@132   ;)      local.get 3
(;@134   ;)      local.get 1
(;@136   ;)      local.get 2
(;@138   ;)      local.get 0
(;@13a   ;)      i32.load offset=8
(;@13d   ;)      call_indirect (type $fun_3_1)
(;@140   ;)      return
(;@141   ;)    end
(;@142   ;)    unreachable
             )
(;@146   ;)  (func $__mon_bind (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@147   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@149   ;)    local.get 0
(;@14b   ;)    local.get 2
(;@14d   ;)    call $__call_1
(;@14f   ;)    local.set 3
(;@151   ;)    local.get 3
(;@153   ;)    i32.load
(;@156   ;)    local.set 4
(;@158   ;)    block (result i32) ;; label = @1
(;@15a   ;)      block ;; label = @2
(;@15c   ;)        block ;; label = @3
(;@15e   ;)          block ;; label = @4
(;@160   ;)            local.get 4
(;@162   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@167   ;)          end
(;@168   ;)          local.get 3
(;@16a   ;)          i32.load offset=4
(;@16d   ;)          local.set 5
(;@16f   ;)          local.get 1
(;@171   ;)          local.get 5
(;@173   ;)          local.get 2
(;@175   ;)          call $__call_2
(;@177   ;)          br 2 (;@1;)
(;@179   ;)        end
(;@17a   ;)        local.get 3
(;@17c   ;)        i32.load offset=4
(;@17f   ;)        local.set 6
(;@181   ;)        local.get 6
(;@183   ;)        local.set 7
(;@185   ;)        local.get 7
(;@187   ;)        i32.load
(;@18a   ;)        local.set 8
(;@18c   ;)        local.get 7
(;@18e   ;)        i32.load offset=4
(;@191   ;)        local.set 9
(;@193   ;)        i32.const 20
(;@195   ;)        call $alloc
(;@197   ;)        local.set 10
(;@199   ;)        local.get 10
(;@19b   ;)        i32.const 2
(;@19d   ;)        i32.store
(;@1a0   ;)        local.get 10
(;@1a2   ;)        i32.const 2
(;@1a4   ;)        i32.store offset=4
(;@1a7   ;)        local.get 10
(;@1a9   ;)        i32.const 7
(;@1ab   ;)        i32.store offset=8
(;@1ae   ;)        local.get 10
(;@1b0   ;)        local.get 1
(;@1b2   ;)        i32.store offset=12
(;@1b5   ;)        local.get 10
(;@1b7   ;)        local.get 7
(;@1b9   ;)        i32.store offset=16
(;@1bc   ;)        local.get 10
(;@1be   ;)        local.set 10
(;@1c0   ;)        i32.const 12
(;@1c2   ;)        call $alloc
(;@1c4   ;)        local.set 11
(;@1c6   ;)        local.get 11
(;@1c8   ;)        local.get 8
(;@1ca   ;)        i32.store
(;@1cd   ;)        local.get 11
(;@1cf   ;)        local.get 9
(;@1d1   ;)        i32.store offset=4
(;@1d4   ;)        local.get 11
(;@1d6   ;)        local.get 10
(;@1d8   ;)        i32.store offset=8
(;@1db   ;)        local.get 11
(;@1dd   ;)        local.set 11
(;@1df   ;)        i32.const 8
(;@1e1   ;)        call $alloc
(;@1e3   ;)        local.set 12
(;@1e5   ;)        local.get 12
(;@1e7   ;)        i32.const 1
(;@1e9   ;)        i32.store
(;@1ec   ;)        local.get 12
(;@1ee   ;)        local.get 11
(;@1f0   ;)        i32.store offset=4
(;@1f3   ;)        local.get 12
(;@1f5   ;)        br 1 (;@1;)
(;@1f7   ;)      end
(;@1f8   ;)      unreachable
(;@1f9   ;)    end
(;@1fa   ;)    return
             )
(;@1fd   ;)  (func $__mon_bind_lam_0 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1fe   ;)    (local i32 i32 i32 i32)
(;@200   ;)    local.get 0
(;@202   ;)    i32.load
(;@205   ;)    local.set 2
(;@207   ;)    local.get 0
(;@209   ;)    i32.load offset=4
(;@20c   ;)    local.set 3
(;@20e   ;)    local.get 2
(;@210   ;)    i32.load offset=8
(;@213   ;)    local.set 4
(;@215   ;)    local.get 4
(;@217   ;)    local.get 3
(;@219   ;)    local.get 1
(;@21b   ;)    call $__call_2
(;@21d   ;)    return
             )
(;@220   ;)  (func $__mon_bind_lam_1 (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@221   ;)    (local i32 i32 i32 i32)
(;@223   ;)    local.get 0
(;@225   ;)    i32.load
(;@228   ;)    local.set 3
(;@22a   ;)    local.get 0
(;@22c   ;)    i32.load offset=4
(;@22f   ;)    local.set 4
(;@231   ;)    i32.const 20
(;@233   ;)    call $alloc
(;@235   ;)    local.set 5
(;@237   ;)    local.get 5
(;@239   ;)    i32.const 1
(;@23b   ;)    i32.store
(;@23e   ;)    local.get 5
(;@240   ;)    i32.const 2
(;@242   ;)    i32.store offset=4
(;@245   ;)    local.get 5
(;@247   ;)    i32.const 6
(;@249   ;)    i32.store offset=8
(;@24c   ;)    local.get 5
(;@24e   ;)    local.get 4
(;@250   ;)    i32.store offset=12
(;@253   ;)    local.get 5
(;@255   ;)    local.get 1
(;@257   ;)    i32.store offset=16
(;@25a   ;)    local.get 5
(;@25c   ;)    local.set 5
(;@25e   ;)    local.get 5
(;@260   ;)    local.get 3
(;@262   ;)    local.get 2
(;@264   ;)    call $__mon_bind
(;@266   ;)    return
             )
(;@26a   ;)  (func $__mon_prompt (;8;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@26b   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@26d   ;)    local.get 1
(;@26f   ;)    local.get 4
(;@271   ;)    call $__call_1
(;@273   ;)    local.set 5
(;@275   ;)    local.get 3
(;@277   ;)    local.get 5
(;@279   ;)    call $__call_1
(;@27b   ;)    local.set 6
(;@27d   ;)    local.get 6
(;@27f   ;)    i32.load
(;@282   ;)    local.set 7
(;@284   ;)    block (result i32) ;; label = @1
(;@286   ;)      block ;; label = @2
(;@288   ;)        block ;; label = @3
(;@28a   ;)          block ;; label = @4
(;@28c   ;)            local.get 7
(;@28e   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@293   ;)          end
(;@294   ;)          local.get 6
(;@296   ;)          i32.load offset=4
(;@299   ;)          local.set 8
(;@29b   ;)          local.get 2
(;@29d   ;)          local.get 8
(;@29f   ;)          local.get 4
(;@2a1   ;)          call $__call_2
(;@2a3   ;)          br 2 (;@1;)
(;@2a5   ;)        end
(;@2a6   ;)        local.get 6
(;@2a8   ;)        i32.load offset=4
(;@2ab   ;)        local.set 9
(;@2ad   ;)        local.get 9
(;@2af   ;)        i32.load
(;@2b2   ;)        local.set 10
(;@2b4   ;)        local.get 0
(;@2b6   ;)        local.get 10
(;@2b8   ;)        call $__mon_eqm
(;@2ba   ;)        local.set 11
(;@2bc   ;)        local.get 11
(;@2be   ;)        i32.load
(;@2c1   ;)        local.set 12
(;@2c3   ;)        block (result i32) ;; label = @3
(;@2c5   ;)          block ;; label = @4
(;@2c7   ;)            block ;; label = @5
(;@2c9   ;)              block ;; label = @6
(;@2cb   ;)                local.get 12
(;@2cd   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@2d2   ;)              end
(;@2d3   ;)              local.get 11
(;@2d5   ;)              i32.load offset=4
(;@2d8   ;)              local.set 13
(;@2da   ;)              local.get 9
(;@2dc   ;)              local.set 14
(;@2de   ;)              local.get 14
(;@2e0   ;)              i32.load
(;@2e3   ;)              local.set 15
(;@2e5   ;)              local.get 14
(;@2e7   ;)              i32.load offset=4
(;@2ea   ;)              local.set 16
(;@2ec   ;)              i32.const 28
(;@2ee   ;)              call $alloc
(;@2f0   ;)              local.set 17
(;@2f2   ;)              local.get 17
(;@2f4   ;)              i32.const 2
(;@2f6   ;)              i32.store
(;@2f9   ;)              local.get 17
(;@2fb   ;)              i32.const 4
(;@2fd   ;)              i32.store offset=4
(;@300   ;)              local.get 17
(;@302   ;)              i32.const 10
(;@304   ;)              i32.store offset=8
(;@307   ;)              local.get 17
(;@309   ;)              local.get 0
(;@30b   ;)              i32.store offset=12
(;@30e   ;)              local.get 17
(;@310   ;)              local.get 1
(;@312   ;)              i32.store offset=16
(;@315   ;)              local.get 17
(;@317   ;)              local.get 2
(;@319   ;)              i32.store offset=20
(;@31c   ;)              local.get 17
(;@31e   ;)              local.get 14
(;@320   ;)              i32.store offset=24
(;@323   ;)              local.get 17
(;@325   ;)              local.set 17
(;@327   ;)              i32.const 12
(;@329   ;)              call $alloc
(;@32b   ;)              local.set 18
(;@32d   ;)              local.get 18
(;@32f   ;)              local.get 15
(;@331   ;)              i32.store
(;@334   ;)              local.get 18
(;@336   ;)              local.get 16
(;@338   ;)              i32.store offset=4
(;@33b   ;)              local.get 18
(;@33d   ;)              local.get 17
(;@33f   ;)              i32.store offset=8
(;@342   ;)              local.get 18
(;@344   ;)              local.set 18
(;@346   ;)              i32.const 8
(;@348   ;)              call $alloc
(;@34a   ;)              local.set 19
(;@34c   ;)              local.get 19
(;@34e   ;)              i32.const 1
(;@350   ;)              i32.store
(;@353   ;)              local.get 19
(;@355   ;)              local.get 18
(;@357   ;)              i32.store offset=4
(;@35a   ;)              local.get 19
(;@35c   ;)              br 2 (;@3;)
(;@35e   ;)            end
(;@35f   ;)            local.get 11
(;@361   ;)            i32.load offset=4
(;@364   ;)            local.set 13
(;@366   ;)            i32.const 28
(;@368   ;)            call $alloc
(;@36a   ;)            local.set 19
(;@36c   ;)            local.get 19
(;@36e   ;)            i32.const 2
(;@370   ;)            i32.store
(;@373   ;)            local.get 19
(;@375   ;)            i32.const 4
(;@377   ;)            i32.store offset=4
(;@37a   ;)            local.get 19
(;@37c   ;)            i32.const 12
(;@37e   ;)            i32.store offset=8
(;@381   ;)            local.get 19
(;@383   ;)            local.get 0
(;@385   ;)            i32.store offset=12
(;@388   ;)            local.get 19
(;@38a   ;)            local.get 1
(;@38c   ;)            i32.store offset=16
(;@38f   ;)            local.get 19
(;@391   ;)            local.get 2
(;@393   ;)            i32.store offset=20
(;@396   ;)            local.get 19
(;@398   ;)            local.get 9
(;@39a   ;)            i32.store offset=24
(;@39d   ;)            local.get 19
(;@39f   ;)            local.set 19
(;@3a1   ;)            local.get 9
(;@3a3   ;)            i32.load offset=4
(;@3a6   ;)            local.set 20
(;@3a8   ;)            local.get 20
(;@3aa   ;)            local.get 19
(;@3ac   ;)            local.get 4
(;@3ae   ;)            call $__call_2
(;@3b0   ;)            br 1 (;@3;)
(;@3b2   ;)          end
(;@3b3   ;)          unreachable
(;@3b4   ;)        end
(;@3b5   ;)        br 1 (;@1;)
(;@3b7   ;)      end
(;@3b8   ;)      unreachable
(;@3b9   ;)    end
(;@3ba   ;)    return
             )
(;@3bd   ;)  (func $__mon_prompt_lam_0 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3be   ;)    (local i32 i32 i32 i32)
(;@3c0   ;)    local.get 0
(;@3c2   ;)    i32.load
(;@3c5   ;)    local.set 2
(;@3c7   ;)    local.get 0
(;@3c9   ;)    i32.load offset=4
(;@3cc   ;)    local.set 3
(;@3ce   ;)    local.get 2
(;@3d0   ;)    i32.load offset=8
(;@3d3   ;)    local.set 4
(;@3d5   ;)    local.get 4
(;@3d7   ;)    local.get 3
(;@3d9   ;)    local.get 1
(;@3db   ;)    call $__call_2
(;@3dd   ;)    return
             )
(;@3e0   ;)  (func $__mon_prompt_lam_1 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@3e1   ;)    (local i32 i32 i32 i32 i32 i32)
(;@3e3   ;)    local.get 0
(;@3e5   ;)    i32.load
(;@3e8   ;)    local.set 3
(;@3ea   ;)    local.get 0
(;@3ec   ;)    i32.load offset=4
(;@3ef   ;)    local.set 4
(;@3f1   ;)    local.get 0
(;@3f3   ;)    i32.load offset=8
(;@3f6   ;)    local.set 5
(;@3f8   ;)    local.get 0
(;@3fa   ;)    i32.load offset=12
(;@3fd   ;)    local.set 6
(;@3ff   ;)    i32.const 20
(;@401   ;)    call $alloc
(;@403   ;)    local.set 7
(;@405   ;)    local.get 7
(;@407   ;)    i32.const 1
(;@409   ;)    i32.store
(;@40c   ;)    local.get 7
(;@40e   ;)    i32.const 2
(;@410   ;)    i32.store offset=4
(;@413   ;)    local.get 7
(;@415   ;)    i32.const 9
(;@417   ;)    i32.store offset=8
(;@41a   ;)    local.get 7
(;@41c   ;)    local.get 6
(;@41e   ;)    i32.store offset=12
(;@421   ;)    local.get 7
(;@423   ;)    local.get 1
(;@425   ;)    i32.store offset=16
(;@428   ;)    local.get 7
(;@42a   ;)    local.set 7
(;@42c   ;)    local.get 3
(;@42e   ;)    local.get 4
(;@430   ;)    local.get 5
(;@432   ;)    local.get 7
(;@434   ;)    local.get 2
(;@436   ;)    call $__mon_prompt
(;@438   ;)    return
             )
(;@43b   ;)  (func $__mon_prompt_lam_2 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@43c   ;)    (local i32 i32 i32 i32)
(;@43e   ;)    local.get 0
(;@440   ;)    i32.load
(;@443   ;)    local.set 2
(;@445   ;)    local.get 0
(;@447   ;)    i32.load offset=4
(;@44a   ;)    local.set 3
(;@44c   ;)    local.get 2
(;@44e   ;)    i32.load offset=8
(;@451   ;)    local.set 4
(;@453   ;)    local.get 4
(;@455   ;)    local.get 3
(;@457   ;)    local.get 1
(;@459   ;)    call $__call_2
(;@45b   ;)    return
             )
(;@45e   ;)  (func $__mon_prompt_lam_3 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@45f   ;)    (local i32 i32 i32 i32 i32 i32)
(;@461   ;)    local.get 0
(;@463   ;)    i32.load
(;@466   ;)    local.set 3
(;@468   ;)    local.get 0
(;@46a   ;)    i32.load offset=4
(;@46d   ;)    local.set 4
(;@46f   ;)    local.get 0
(;@471   ;)    i32.load offset=8
(;@474   ;)    local.set 5
(;@476   ;)    local.get 0
(;@478   ;)    i32.load offset=12
(;@47b   ;)    local.set 6
(;@47d   ;)    i32.const 20
(;@47f   ;)    call $alloc
(;@481   ;)    local.set 7
(;@483   ;)    local.get 7
(;@485   ;)    i32.const 1
(;@487   ;)    i32.store
(;@48a   ;)    local.get 7
(;@48c   ;)    i32.const 2
(;@48e   ;)    i32.store offset=4
(;@491   ;)    local.get 7
(;@493   ;)    i32.const 11
(;@495   ;)    i32.store offset=8
(;@498   ;)    local.get 7
(;@49a   ;)    local.get 6
(;@49c   ;)    i32.store offset=12
(;@49f   ;)    local.get 7
(;@4a1   ;)    local.get 1
(;@4a3   ;)    i32.store offset=16
(;@4a6   ;)    local.get 7
(;@4a8   ;)    local.set 7
(;@4aa   ;)    local.get 3
(;@4ac   ;)    local.get 4
(;@4ae   ;)    local.get 5
(;@4b0   ;)    local.get 7
(;@4b2   ;)    local.get 2
(;@4b4   ;)    call $__mon_prompt
(;@4b6   ;)    return
             )
(;@40    ;)  (table (;0;) 13 13 funcref)
(;@47    ;)  (memory (;0;) 1)
(;@4c    ;)  (global (;0;) (mut i32) i32.const 0)
(;@51    ;)  (global (;1;) (mut i32) i32.const 0)
(;@59    ;)  (export "mem" (memory 0))
(;@62    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__call_1 $__call_2 $__mon_bind $__mon_bind_lam_0 $__mon_bind_lam_1 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1 $__mon_prompt_lam_2 $__mon_prompt_lam_3)
           )
