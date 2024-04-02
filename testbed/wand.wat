(module $eff_item
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_4_1 (;4;) (func (param i32 i32 i32 i32) (result i32)))
(;@29    ;)  (type $fun_5_1 (;5;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@ee    ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@ef    ;)    (local i32)
(;@f1    ;)    global.get 1
(;@f3    ;)    global.get 1
(;@f5    ;)    i32.const 1
(;@f7    ;)    i32.add
(;@f8    ;)    global.set 1
(;@fa    ;)    return
             )
(;@fd    ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@fe    ;)    (local i32)
(;@100   ;)    global.get 0
(;@102   ;)    global.get 0
(;@104   ;)    local.get 0
(;@106   ;)    i32.add
(;@107   ;)    global.set 0
(;@109   ;)    return
             )
(;@10c   ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@10d   ;)    (local i32)
(;@10f   ;)    i32.const 4
(;@111   ;)    call $alloc
(;@113   ;)    local.tee 2
(;@115   ;)    i32.const 1
(;@117   ;)    i32.const 0
(;@119   ;)    local.get 0
(;@11b   ;)    local.get 1
(;@11d   ;)    i32.eq
(;@11e   ;)    select
(;@11f   ;)    i32.store
(;@122   ;)    local.get 2
(;@124   ;)    return
             )
(;@127   ;)  (func $__call_1 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@128   ;)    (local i32)
(;@12a   ;)    local.get 0
(;@12c   ;)    i32.const 12
(;@12e   ;)    i32.add
(;@12f   ;)    local.set 2
(;@131   ;)    block ;; label = @1
(;@133   ;)      block ;; label = @2
(;@135   ;)        block ;; label = @3
(;@137   ;)          local.get 0
(;@139   ;)          i32.load
(;@13c   ;)          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
(;@141   ;)        end
(;@142   ;)        local.get 2
(;@144   ;)        local.get 0
(;@146   ;)        i32.load offset=8
(;@149   ;)        call_indirect (type $fun_1_1)
(;@14c   ;)        local.get 1
(;@14e   ;)        call $__call_1
(;@150   ;)        return
(;@151   ;)      end
(;@152   ;)      local.get 2
(;@154   ;)      local.get 1
(;@156   ;)      local.get 0
(;@158   ;)      i32.load offset=8
(;@15b   ;)      call_indirect (type $fun_2_1)
(;@15e   ;)      return
(;@15f   ;)    end
(;@160   ;)    unreachable
             )
(;@163   ;)  (func $__call_2 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@164   ;)    (local i32)
(;@166   ;)    local.get 0
(;@168   ;)    i32.const 12
(;@16a   ;)    i32.add
(;@16b   ;)    local.set 3
(;@16d   ;)    block ;; label = @1
(;@16f   ;)      block ;; label = @2
(;@171   ;)        block ;; label = @3
(;@173   ;)          block ;; label = @4
(;@175   ;)            local.get 0
(;@177   ;)            i32.load
(;@17a   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 3 (;@1;)
(;@180   ;)          end
(;@181   ;)          local.get 3
(;@183   ;)          local.get 0
(;@185   ;)          i32.load offset=8
(;@188   ;)          call_indirect (type $fun_1_1)
(;@18b   ;)          local.get 1
(;@18d   ;)          local.get 2
(;@18f   ;)          call $__call_2
(;@191   ;)          return
(;@192   ;)        end
(;@193   ;)        local.get 3
(;@195   ;)        local.get 1
(;@197   ;)        local.get 0
(;@199   ;)        i32.load offset=8
(;@19c   ;)        call_indirect (type $fun_2_1)
(;@19f   ;)        local.get 2
(;@1a1   ;)        call $__call_1
(;@1a3   ;)        return
(;@1a4   ;)      end
(;@1a5   ;)      local.get 3
(;@1a7   ;)      local.get 1
(;@1a9   ;)      local.get 2
(;@1ab   ;)      local.get 0
(;@1ad   ;)      i32.load offset=8
(;@1b0   ;)      call_indirect (type $fun_3_1)
(;@1b3   ;)      return
(;@1b4   ;)    end
(;@1b5   ;)    unreachable
             )
(;@1b8   ;)  (func $foo (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1b9   ;)    (local i32 i32)
(;@1bb   ;)    i32.const 20
(;@1bd   ;)    call $alloc
(;@1bf   ;)    local.set 3
(;@1c1   ;)    local.get 3
(;@1c3   ;)    i32.const 1
(;@1c5   ;)    i32.store
(;@1c8   ;)    local.get 3
(;@1ca   ;)    i32.const 2
(;@1cc   ;)    i32.store offset=4
(;@1cf   ;)    local.get 3
(;@1d1   ;)    i32.const 35
(;@1d3   ;)    i32.store offset=8
(;@1d6   ;)    local.get 3
(;@1d8   ;)    local.get 0
(;@1da   ;)    i32.store offset=12
(;@1dd   ;)    local.get 3
(;@1df   ;)    local.get 1
(;@1e1   ;)    i32.store offset=16
(;@1e4   ;)    local.get 3
(;@1e6   ;)    local.set 3
(;@1e8   ;)    i32.const 8
(;@1ea   ;)    call $alloc
(;@1ec   ;)    local.set 4
(;@1ee   ;)    local.get 4
(;@1f0   ;)    i32.const 0
(;@1f2   ;)    i32.store
(;@1f5   ;)    local.get 4
(;@1f7   ;)    local.get 3
(;@1f9   ;)    i32.store offset=4
(;@1fc   ;)    local.get 4
(;@1fe   ;)    return
             )
(;@201   ;)  (func $foo_lam_0 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@202   ;)    (local i32 i32 i32)
(;@204   ;)    local.get 0
(;@206   ;)    i32.load
(;@209   ;)    local.set 2
(;@20b   ;)    local.get 0
(;@20d   ;)    i32.load offset=4
(;@210   ;)    local.set 3
(;@212   ;)    local.get 2
(;@214   ;)    local.get 3
(;@216   ;)    local.get 1
(;@218   ;)    call $__call_2
(;@21a   ;)    return
             )
(;@21d   ;)  (func $foo_lam_1 (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@21e   ;)    (local i32 i32)
(;@220   ;)    local.get 0
(;@222   ;)    i32.load
(;@225   ;)    local.set 3
(;@227   ;)    local.get 1
(;@229   ;)    local.get 3
(;@22b   ;)    local.get 2
(;@22d   ;)    call $__call_2
(;@22f   ;)    return
             )
(;@232   ;)  (func $foo_lam_2 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@233   ;)    (local i32 i32 i32 i32)
(;@235   ;)    local.get 0
(;@237   ;)    i32.load
(;@23a   ;)    local.set 3
(;@23c   ;)    i32.const 20
(;@23e   ;)    call $alloc
(;@240   ;)    local.set 4
(;@242   ;)    local.get 4
(;@244   ;)    i32.const 1
(;@246   ;)    i32.store
(;@249   ;)    local.get 4
(;@24b   ;)    i32.const 2
(;@24d   ;)    i32.store offset=4
(;@250   ;)    local.get 4
(;@252   ;)    i32.const 6
(;@254   ;)    i32.store offset=8
(;@257   ;)    local.get 4
(;@259   ;)    local.get 3
(;@25b   ;)    i32.store offset=12
(;@25e   ;)    local.get 4
(;@260   ;)    local.get 1
(;@262   ;)    i32.store offset=16
(;@265   ;)    local.get 4
(;@267   ;)    local.set 4
(;@269   ;)    i32.const 16
(;@26b   ;)    call $alloc
(;@26d   ;)    local.set 5
(;@26f   ;)    local.get 5
(;@271   ;)    i32.const 2
(;@273   ;)    i32.store
(;@276   ;)    local.get 5
(;@278   ;)    i32.const 1
(;@27a   ;)    i32.store offset=4
(;@27d   ;)    local.get 5
(;@27f   ;)    i32.const 7
(;@281   ;)    i32.store offset=8
(;@284   ;)    local.get 5
(;@286   ;)    local.get 1
(;@288   ;)    i32.store offset=12
(;@28b   ;)    local.get 5
(;@28d   ;)    local.set 5
(;@28f   ;)    local.get 4
(;@291   ;)    local.get 5
(;@293   ;)    local.get 2
(;@295   ;)    call $__mon_bind
(;@297   ;)    return
             )
(;@29a   ;)  (func $foo_lam_3 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@29b   ;)    (local i32 i32)
(;@29d   ;)    i32.const 16
(;@29f   ;)    call $alloc
(;@2a1   ;)    local.set 3
(;@2a3   ;)    local.get 3
(;@2a5   ;)    i32.const 2
(;@2a7   ;)    i32.store
(;@2aa   ;)    local.get 3
(;@2ac   ;)    i32.const 1
(;@2ae   ;)    i32.store offset=4
(;@2b1   ;)    local.get 3
(;@2b3   ;)    i32.const 8
(;@2b5   ;)    i32.store offset=8
(;@2b8   ;)    local.get 3
(;@2ba   ;)    local.get 1
(;@2bc   ;)    i32.store offset=12
(;@2bf   ;)    local.get 3
(;@2c1   ;)    local.set 3
(;@2c3   ;)    i32.const 8
(;@2c5   ;)    call $alloc
(;@2c7   ;)    local.set 4
(;@2c9   ;)    local.get 4
(;@2cb   ;)    i32.const 0
(;@2cd   ;)    i32.store
(;@2d0   ;)    local.get 4
(;@2d2   ;)    local.get 3
(;@2d4   ;)    i32.store offset=4
(;@2d7   ;)    local.get 4
(;@2d9   ;)    return
             )
(;@2dc   ;)  (func $foo_lam_4 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@2dd   ;)    (local i32 i32)
(;@2df   ;)    i32.const 12
(;@2e1   ;)    call $alloc
(;@2e3   ;)    local.set 3
(;@2e5   ;)    local.get 3
(;@2e7   ;)    i32.const 2
(;@2e9   ;)    i32.store
(;@2ec   ;)    local.get 3
(;@2ee   ;)    i32.const 0
(;@2f0   ;)    i32.store offset=4
(;@2f3   ;)    local.get 3
(;@2f5   ;)    i32.const 9
(;@2f7   ;)    i32.store offset=8
(;@2fa   ;)    local.get 3
(;@2fc   ;)    local.set 3
(;@2fe   ;)    i32.const 8
(;@300   ;)    call $alloc
(;@302   ;)    local.set 4
(;@304   ;)    local.get 4
(;@306   ;)    i32.const 0
(;@308   ;)    i32.store
(;@30b   ;)    local.get 4
(;@30d   ;)    local.get 3
(;@30f   ;)    i32.store offset=4
(;@312   ;)    local.get 4
(;@314   ;)    return
             )
(;@317   ;)  (func $foo_lam_5 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@318   ;)    (local i32 i32 i32)
(;@31a   ;)    local.get 0
(;@31c   ;)    i32.load
(;@31f   ;)    local.set 2
(;@321   ;)    i32.const 0
(;@323   ;)    call $alloc
(;@325   ;)    local.set 3
(;@327   ;)    local.get 3
(;@329   ;)    local.set 3
(;@32b   ;)    local.get 2
(;@32d   ;)    local.get 3
(;@32f   ;)    local.get 1
(;@331   ;)    call $__call_2
(;@333   ;)    return
             )
(;@336   ;)  (func $foo_lam_6 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@337   ;)    (local i32 i32)
(;@339   ;)    local.get 0
(;@33b   ;)    i32.load
(;@33e   ;)    local.set 3
(;@340   ;)    local.get 1
(;@342   ;)    local.get 3
(;@344   ;)    local.get 2
(;@346   ;)    call $__call_2
(;@348   ;)    return
             )
(;@34b   ;)  (func $foo_lam_7 (;13;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@34c   ;)    (local i32 i32 i32 i32 i32)
(;@34e   ;)    local.get 0
(;@350   ;)    i32.load
(;@353   ;)    local.set 3
(;@355   ;)    local.get 0
(;@357   ;)    i32.load offset=4
(;@35a   ;)    local.set 4
(;@35c   ;)    i32.const 16
(;@35e   ;)    call $alloc
(;@360   ;)    local.set 5
(;@362   ;)    local.get 5
(;@364   ;)    i32.const 1
(;@366   ;)    i32.store
(;@369   ;)    local.get 5
(;@36b   ;)    i32.const 1
(;@36d   ;)    i32.store offset=4
(;@370   ;)    local.get 5
(;@372   ;)    i32.const 11
(;@374   ;)    i32.store offset=8
(;@377   ;)    local.get 5
(;@379   ;)    local.get 4
(;@37b   ;)    i32.store offset=12
(;@37e   ;)    local.get 5
(;@380   ;)    local.set 5
(;@382   ;)    i32.const 16
(;@384   ;)    call $alloc
(;@386   ;)    local.set 6
(;@388   ;)    local.get 6
(;@38a   ;)    i32.const 2
(;@38c   ;)    i32.store
(;@38f   ;)    local.get 6
(;@391   ;)    i32.const 1
(;@393   ;)    i32.store offset=4
(;@396   ;)    local.get 6
(;@398   ;)    i32.const 12
(;@39a   ;)    i32.store offset=8
(;@39d   ;)    local.get 6
(;@39f   ;)    local.get 3
(;@3a1   ;)    i32.store offset=12
(;@3a4   ;)    local.get 6
(;@3a6   ;)    local.set 6
(;@3a8   ;)    local.get 5
(;@3aa   ;)    local.get 6
(;@3ac   ;)    local.get 2
(;@3ae   ;)    call $__mon_bind
(;@3b0   ;)    return
             )
(;@3b3   ;)  (func $foo_lam_8 (;14;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@3b4   ;)    (local i32 i32 i32)
(;@3b6   ;)    local.get 0
(;@3b8   ;)    i32.load
(;@3bb   ;)    local.set 3
(;@3bd   ;)    i32.const 20
(;@3bf   ;)    call $alloc
(;@3c1   ;)    local.set 4
(;@3c3   ;)    local.get 4
(;@3c5   ;)    i32.const 2
(;@3c7   ;)    i32.store
(;@3ca   ;)    local.get 4
(;@3cc   ;)    i32.const 2
(;@3ce   ;)    i32.store offset=4
(;@3d1   ;)    local.get 4
(;@3d3   ;)    i32.const 13
(;@3d5   ;)    i32.store offset=8
(;@3d8   ;)    local.get 4
(;@3da   ;)    local.get 3
(;@3dc   ;)    i32.store offset=12
(;@3df   ;)    local.get 4
(;@3e1   ;)    local.get 1
(;@3e3   ;)    i32.store offset=16
(;@3e6   ;)    local.get 4
(;@3e8   ;)    local.set 4
(;@3ea   ;)    i32.const 8
(;@3ec   ;)    call $alloc
(;@3ee   ;)    local.set 5
(;@3f0   ;)    local.get 5
(;@3f2   ;)    i32.const 0
(;@3f4   ;)    i32.store
(;@3f7   ;)    local.get 5
(;@3f9   ;)    local.get 4
(;@3fb   ;)    i32.store offset=4
(;@3fe   ;)    local.get 5
(;@400   ;)    return
             )
(;@403   ;)  (func $foo_lam_9 (;15;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@404   ;)    (local i32 i32)
(;@406   ;)    i32.const 16
(;@408   ;)    call $alloc
(;@40a   ;)    local.set 3
(;@40c   ;)    local.get 3
(;@40e   ;)    i32.const 2
(;@410   ;)    i32.store
(;@413   ;)    local.get 3
(;@415   ;)    i32.const 1
(;@417   ;)    i32.store offset=4
(;@41a   ;)    local.get 3
(;@41c   ;)    i32.const 14
(;@41e   ;)    i32.store offset=8
(;@421   ;)    local.get 3
(;@423   ;)    local.get 1
(;@425   ;)    i32.store offset=12
(;@428   ;)    local.get 3
(;@42a   ;)    local.set 3
(;@42c   ;)    i32.const 8
(;@42e   ;)    call $alloc
(;@430   ;)    local.set 4
(;@432   ;)    local.get 4
(;@434   ;)    i32.const 0
(;@436   ;)    i32.store
(;@439   ;)    local.get 4
(;@43b   ;)    local.get 3
(;@43d   ;)    i32.store offset=4
(;@440   ;)    local.get 4
(;@442   ;)    return
             )
(;@446   ;)  (func $foo_lam_10 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@447   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32)
(;@449   ;)    local.get 0
(;@44b   ;)    i32.load
(;@44e   ;)    local.set 2
(;@450   ;)    local.get 0
(;@452   ;)    i32.load offset=4
(;@455   ;)    local.set 3
(;@457   ;)    i32.const 12
(;@459   ;)    call $alloc
(;@45b   ;)    local.set 4
(;@45d   ;)    local.get 4
(;@45f   ;)    i32.const 2
(;@461   ;)    i32.store
(;@464   ;)    local.get 4
(;@466   ;)    i32.const 0
(;@468   ;)    i32.store offset=4
(;@46b   ;)    local.get 4
(;@46d   ;)    i32.const 10
(;@46f   ;)    i32.store offset=8
(;@472   ;)    local.get 4
(;@474   ;)    local.set 4
(;@476   ;)    i32.const 12
(;@478   ;)    call $alloc
(;@47a   ;)    local.set 5
(;@47c   ;)    local.get 5
(;@47e   ;)    i32.const 2
(;@480   ;)    i32.store
(;@483   ;)    local.get 5
(;@485   ;)    i32.const 0
(;@487   ;)    i32.store offset=4
(;@48a   ;)    local.get 5
(;@48c   ;)    i32.const 15
(;@48e   ;)    i32.store offset=8
(;@491   ;)    local.get 5
(;@493   ;)    local.set 5
(;@495   ;)    i32.const 8
(;@497   ;)    call $alloc
(;@499   ;)    local.set 6
(;@49b   ;)    local.get 6
(;@49d   ;)    local.get 4
(;@49f   ;)    i32.store
(;@4a2   ;)    local.get 6
(;@4a4   ;)    local.get 5
(;@4a6   ;)    i32.store offset=4
(;@4a9   ;)    local.get 6
(;@4ab   ;)    local.set 6
(;@4ad   ;)    i32.const 8
(;@4af   ;)    call $alloc
(;@4b1   ;)    local.set 7
(;@4b3   ;)    local.get 7
(;@4b5   ;)    local.get 3
(;@4b7   ;)    i32.store
(;@4ba   ;)    local.get 7
(;@4bc   ;)    local.get 6
(;@4be   ;)    i32.store offset=4
(;@4c1   ;)    local.get 7
(;@4c3   ;)    local.set 7
(;@4c5   ;)    local.get 2
(;@4c7   ;)    i32.load
(;@4ca   ;)    local.set 8
(;@4cc   ;)    local.get 8
(;@4ce   ;)    local.get 1
(;@4d0   ;)    local.get 7
(;@4d2   ;)    call $__call_2
(;@4d4   ;)    return
             )
(;@4d7   ;)  (func $foo_lam_11 (;17;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@4d8   ;)    (local i32 i32 i32)
(;@4da   ;)    local.get 0
(;@4dc   ;)    i32.load
(;@4df   ;)    local.set 3
(;@4e1   ;)    i32.const 8
(;@4e3   ;)    call $alloc
(;@4e5   ;)    local.set 4
(;@4e7   ;)    local.get 4
(;@4e9   ;)    local.get 1
(;@4eb   ;)    i32.store
(;@4ee   ;)    local.get 4
(;@4f0   ;)    local.get 3
(;@4f2   ;)    i32.store offset=4
(;@4f5   ;)    local.get 4
(;@4f7   ;)    local.set 4
(;@4f9   ;)    i32.const 8
(;@4fb   ;)    call $alloc
(;@4fd   ;)    local.set 5
(;@4ff   ;)    local.get 5
(;@501   ;)    i32.const 0
(;@503   ;)    i32.store
(;@506   ;)    local.get 5
(;@508   ;)    local.get 4
(;@50a   ;)    i32.store offset=4
(;@50d   ;)    local.get 5
(;@50f   ;)    return
             )
(;@512   ;)  (func $foo_lam_12 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@513   ;)    (local i32 i32)
(;@515   ;)    i32.const 16
(;@517   ;)    call $alloc
(;@519   ;)    local.set 3
(;@51b   ;)    local.get 3
(;@51d   ;)    i32.const 2
(;@51f   ;)    i32.store
(;@522   ;)    local.get 3
(;@524   ;)    i32.const 1
(;@526   ;)    i32.store offset=4
(;@529   ;)    local.get 3
(;@52b   ;)    i32.const 17
(;@52d   ;)    i32.store offset=8
(;@530   ;)    local.get 3
(;@532   ;)    local.get 1
(;@534   ;)    i32.store offset=12
(;@537   ;)    local.get 3
(;@539   ;)    local.set 3
(;@53b   ;)    i32.const 8
(;@53d   ;)    call $alloc
(;@53f   ;)    local.set 4
(;@541   ;)    local.get 4
(;@543   ;)    i32.const 0
(;@545   ;)    i32.store
(;@548   ;)    local.get 4
(;@54a   ;)    local.get 3
(;@54c   ;)    i32.store offset=4
(;@54f   ;)    local.get 4
(;@551   ;)    return
             )
(;@554   ;)  (func $foo_lam_13 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@555   ;)    (local i32 i32)
(;@557   ;)    local.get 0
(;@559   ;)    i32.load
(;@55c   ;)    local.set 3
(;@55e   ;)    local.get 1
(;@560   ;)    local.get 3
(;@562   ;)    local.get 2
(;@564   ;)    call $__call_2
(;@566   ;)    return
             )
(;@569   ;)  (func $foo_lam_14 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@56a   ;)    (local i32 i32 i32)
(;@56c   ;)    local.get 0
(;@56e   ;)    i32.load
(;@571   ;)    local.set 3
(;@573   ;)    i32.const 16
(;@575   ;)    call $alloc
(;@577   ;)    local.set 4
(;@579   ;)    local.get 4
(;@57b   ;)    i32.const 2
(;@57d   ;)    i32.store
(;@580   ;)    local.get 4
(;@582   ;)    i32.const 1
(;@584   ;)    i32.store offset=4
(;@587   ;)    local.get 4
(;@589   ;)    i32.const 19
(;@58b   ;)    i32.store offset=8
(;@58e   ;)    local.get 4
(;@590   ;)    local.get 3
(;@592   ;)    i32.store offset=12
(;@595   ;)    local.get 4
(;@597   ;)    local.set 4
(;@599   ;)    i32.const 8
(;@59b   ;)    call $alloc
(;@59d   ;)    local.set 5
(;@59f   ;)    local.get 5
(;@5a1   ;)    i32.const 0
(;@5a3   ;)    i32.store
(;@5a6   ;)    local.get 5
(;@5a8   ;)    local.get 4
(;@5aa   ;)    i32.store offset=4
(;@5ad   ;)    local.get 5
(;@5af   ;)    return
             )
(;@5b2   ;)  (func $foo_lam_15 (;21;) (type $fun_2_1) (param i32 i32) (result i32)
(;@5b3   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@5b5   ;)    local.get 0
(;@5b7   ;)    i32.load
(;@5ba   ;)    local.set 2
(;@5bc   ;)    local.get 0
(;@5be   ;)    i32.load offset=4
(;@5c1   ;)    local.set 3
(;@5c3   ;)    local.get 0
(;@5c5   ;)    i32.load offset=8
(;@5c8   ;)    local.set 4
(;@5ca   ;)    i32.const 16
(;@5cc   ;)    call $alloc
(;@5ce   ;)    local.set 5
(;@5d0   ;)    local.get 5
(;@5d2   ;)    i32.const 2
(;@5d4   ;)    i32.store
(;@5d7   ;)    local.get 5
(;@5d9   ;)    i32.const 1
(;@5db   ;)    i32.store offset=4
(;@5de   ;)    local.get 5
(;@5e0   ;)    i32.const 20
(;@5e2   ;)    i32.store offset=8
(;@5e5   ;)    local.get 5
(;@5e7   ;)    local.get 3
(;@5e9   ;)    i32.store offset=12
(;@5ec   ;)    local.get 5
(;@5ee   ;)    local.set 5
(;@5f0   ;)    i32.const 8
(;@5f2   ;)    call $alloc
(;@5f4   ;)    local.set 6
(;@5f6   ;)    local.get 6
(;@5f8   ;)    local.get 4
(;@5fa   ;)    i32.store
(;@5fd   ;)    local.get 6
(;@5ff   ;)    local.get 5
(;@601   ;)    i32.store offset=4
(;@604   ;)    local.get 6
(;@606   ;)    local.set 6
(;@608   ;)    local.get 2
(;@60a   ;)    i32.load
(;@60d   ;)    local.set 7
(;@60f   ;)    local.get 7
(;@611   ;)    local.get 1
(;@613   ;)    local.get 6
(;@615   ;)    call $__call_2
(;@617   ;)    return
             )
(;@61a   ;)  (func $foo_lam_16 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
(;@61b   ;)    (local i32 i32 i32 i32 i32)
(;@61d   ;)    local.get 0
(;@61f   ;)    i32.load
(;@622   ;)    local.set 2
(;@624   ;)    local.get 0
(;@626   ;)    i32.load offset=4
(;@629   ;)    local.set 3
(;@62b   ;)    local.get 3
(;@62d   ;)    i32.load offset=4
(;@630   ;)    local.set 4
(;@632   ;)    local.get 4
(;@634   ;)    i32.load offset=4
(;@637   ;)    local.set 5
(;@639   ;)    local.get 5
(;@63b   ;)    local.get 2
(;@63d   ;)    local.get 1
(;@63f   ;)    call $__call_2
(;@641   ;)    return
             )
(;@644   ;)  (func $foo_lam_17 (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@645   ;)    (local i32 i32)
(;@647   ;)    local.get 0
(;@649   ;)    i32.load
(;@64c   ;)    local.set 3
(;@64e   ;)    local.get 1
(;@650   ;)    local.get 3
(;@652   ;)    local.get 2
(;@654   ;)    call $__call_2
(;@656   ;)    return
             )
(;@659   ;)  (func $foo_lam_18 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@65a   ;)    (local i32 i32 i32 i32 i32)
(;@65c   ;)    local.get 0
(;@65e   ;)    i32.load
(;@661   ;)    local.set 3
(;@663   ;)    local.get 0
(;@665   ;)    i32.load offset=4
(;@668   ;)    local.set 4
(;@66a   ;)    i32.const 20
(;@66c   ;)    call $alloc
(;@66e   ;)    local.set 5
(;@670   ;)    local.get 5
(;@672   ;)    i32.const 1
(;@674   ;)    i32.store
(;@677   ;)    local.get 5
(;@679   ;)    i32.const 2
(;@67b   ;)    i32.store offset=4
(;@67e   ;)    local.get 5
(;@680   ;)    i32.const 22
(;@682   ;)    i32.store offset=8
(;@685   ;)    local.get 5
(;@687   ;)    local.get 3
(;@689   ;)    i32.store offset=12
(;@68c   ;)    local.get 5
(;@68e   ;)    local.get 4
(;@690   ;)    i32.store offset=16
(;@693   ;)    local.get 5
(;@695   ;)    local.set 5
(;@697   ;)    i32.const 16
(;@699   ;)    call $alloc
(;@69b   ;)    local.set 6
(;@69d   ;)    local.get 6
(;@69f   ;)    i32.const 2
(;@6a1   ;)    i32.store
(;@6a4   ;)    local.get 6
(;@6a6   ;)    i32.const 1
(;@6a8   ;)    i32.store offset=4
(;@6ab   ;)    local.get 6
(;@6ad   ;)    i32.const 23
(;@6af   ;)    i32.store offset=8
(;@6b2   ;)    local.get 6
(;@6b4   ;)    local.get 1
(;@6b6   ;)    i32.store offset=12
(;@6b9   ;)    local.get 6
(;@6bb   ;)    local.set 6
(;@6bd   ;)    local.get 5
(;@6bf   ;)    local.get 6
(;@6c1   ;)    local.get 2
(;@6c3   ;)    call $__mon_bind
(;@6c5   ;)    return
             )
(;@6c8   ;)  (func $foo_lam_19 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@6c9   ;)    (local i32)
(;@6cb   ;)    i32.const 8
(;@6cd   ;)    call $alloc
(;@6cf   ;)    local.set 3
(;@6d1   ;)    local.get 3
(;@6d3   ;)    i32.const 0
(;@6d5   ;)    i32.store
(;@6d8   ;)    local.get 3
(;@6da   ;)    local.get 1
(;@6dc   ;)    i32.store offset=4
(;@6df   ;)    local.get 3
(;@6e1   ;)    return
             )
(;@6e5   ;)  (func $foo_lam_20 (;26;) (type $fun_2_1) (param i32 i32) (result i32)
(;@6e6   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@6e8   ;)    local.get 0
(;@6ea   ;)    i32.load
(;@6ed   ;)    local.set 2
(;@6ef   ;)    local.get 0
(;@6f1   ;)    i32.load offset=4
(;@6f4   ;)    local.set 3
(;@6f6   ;)    local.get 3
(;@6f8   ;)    i32.load
(;@6fb   ;)    local.set 4
(;@6fd   ;)    i32.const 20
(;@6ff   ;)    call $alloc
(;@701   ;)    local.set 5
(;@703   ;)    local.get 5
(;@705   ;)    i32.const 2
(;@707   ;)    i32.store
(;@70a   ;)    local.get 5
(;@70c   ;)    i32.const 2
(;@70e   ;)    i32.store offset=4
(;@711   ;)    local.get 5
(;@713   ;)    i32.const 24
(;@715   ;)    i32.store offset=8
(;@718   ;)    local.get 5
(;@71a   ;)    local.get 2
(;@71c   ;)    i32.store offset=12
(;@71f   ;)    local.get 5
(;@721   ;)    local.get 3
(;@723   ;)    i32.store offset=16
(;@726   ;)    local.get 5
(;@728   ;)    local.set 5
(;@72a   ;)    i32.const 12
(;@72c   ;)    call $alloc
(;@72e   ;)    local.set 6
(;@730   ;)    local.get 6
(;@732   ;)    i32.const 2
(;@734   ;)    i32.store
(;@737   ;)    local.get 6
(;@739   ;)    i32.const 0
(;@73b   ;)    i32.store offset=4
(;@73e   ;)    local.get 6
(;@740   ;)    i32.const 25
(;@742   ;)    i32.store offset=8
(;@745   ;)    local.get 6
(;@747   ;)    local.set 6
(;@749   ;)    i32.const 12
(;@74b   ;)    call $alloc
(;@74d   ;)    local.set 7
(;@74f   ;)    local.get 7
(;@751   ;)    local.get 4
(;@753   ;)    i32.store
(;@756   ;)    local.get 7
(;@758   ;)    local.get 5
(;@75a   ;)    i32.store offset=4
(;@75d   ;)    local.get 7
(;@75f   ;)    local.get 6
(;@761   ;)    i32.store offset=8
(;@764   ;)    local.get 7
(;@766   ;)    local.set 7
(;@768   ;)    i32.const 8
(;@76a   ;)    call $alloc
(;@76c   ;)    local.set 8
(;@76e   ;)    local.get 8
(;@770   ;)    i32.const 1
(;@772   ;)    i32.store
(;@775   ;)    local.get 8
(;@777   ;)    local.get 7
(;@779   ;)    i32.store offset=4
(;@77c   ;)    local.get 8
(;@77e   ;)    return
             )
(;@781   ;)  (func $foo_lam_21 (;27;) (type $fun_2_1) (param i32 i32) (result i32)
(;@782   ;)    (local i32 i32 i32 i32 i32 i32)
(;@784   ;)    local.get 0
(;@786   ;)    i32.load
(;@789   ;)    local.set 2
(;@78b   ;)    local.get 0
(;@78d   ;)    i32.load offset=4
(;@790   ;)    local.set 3
(;@792   ;)    local.get 2
(;@794   ;)    i32.load offset=12
(;@797   ;)    local.set 4
(;@799   ;)    local.get 4
(;@79b   ;)    i32.load
(;@79e   ;)    local.set 5
(;@7a0   ;)    local.get 5
(;@7a2   ;)    local.get 3
(;@7a4   ;)    call $__call_1
(;@7a6   ;)    local.set 6
(;@7a8   ;)    i32.const 20
(;@7aa   ;)    call $alloc
(;@7ac   ;)    local.set 7
(;@7ae   ;)    local.get 7
(;@7b0   ;)    i32.const 1
(;@7b2   ;)    i32.store
(;@7b5   ;)    local.get 7
(;@7b7   ;)    i32.const 2
(;@7b9   ;)    i32.store offset=4
(;@7bc   ;)    local.get 7
(;@7be   ;)    i32.const 26
(;@7c0   ;)    i32.store offset=8
(;@7c3   ;)    local.get 7
(;@7c5   ;)    local.get 1
(;@7c7   ;)    i32.store offset=12
(;@7ca   ;)    local.get 7
(;@7cc   ;)    local.get 6
(;@7ce   ;)    i32.store offset=16
(;@7d1   ;)    local.get 7
(;@7d3   ;)    return
             )
(;@7d6   ;)  (func $foo_lam_22 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
(;@7d7   ;)    (local i32 i32 i32 i32)
(;@7d9   ;)    local.get 0
(;@7db   ;)    i32.load
(;@7de   ;)    local.set 2
(;@7e0   ;)    i32.const 0
(;@7e2   ;)    call $alloc
(;@7e4   ;)    local.set 3
(;@7e6   ;)    local.get 3
(;@7e8   ;)    local.set 3
(;@7ea   ;)    local.get 2
(;@7ec   ;)    i32.load offset=4
(;@7ef   ;)    local.set 4
(;@7f1   ;)    local.get 4
(;@7f3   ;)    local.get 3
(;@7f5   ;)    local.get 1
(;@7f7   ;)    call $__call_2
(;@7f9   ;)    return
             )
(;@7fc   ;)  (func $foo_lam_23 (;29;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@7fd   ;)    (local i32 i32)
(;@7ff   ;)    local.get 0
(;@801   ;)    i32.load
(;@804   ;)    local.set 3
(;@806   ;)    local.get 1
(;@808   ;)    local.get 3
(;@80a   ;)    local.get 2
(;@80c   ;)    call $__call_2
(;@80e   ;)    return
             )
(;@811   ;)  (func $foo_lam_24 (;30;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@812   ;)    (local i32 i32 i32 i32)
(;@814   ;)    local.get 0
(;@816   ;)    i32.load
(;@819   ;)    local.set 3
(;@81b   ;)    i32.const 16
(;@81d   ;)    call $alloc
(;@81f   ;)    local.set 4
(;@821   ;)    local.get 4
(;@823   ;)    i32.const 1
(;@825   ;)    i32.store
(;@828   ;)    local.get 4
(;@82a   ;)    i32.const 1
(;@82c   ;)    i32.store offset=4
(;@82f   ;)    local.get 4
(;@831   ;)    i32.const 28
(;@833   ;)    i32.store offset=8
(;@836   ;)    local.get 4
(;@838   ;)    local.get 3
(;@83a   ;)    i32.store offset=12
(;@83d   ;)    local.get 4
(;@83f   ;)    local.set 4
(;@841   ;)    i32.const 16
(;@843   ;)    call $alloc
(;@845   ;)    local.set 5
(;@847   ;)    local.get 5
(;@849   ;)    i32.const 2
(;@84b   ;)    i32.store
(;@84e   ;)    local.get 5
(;@850   ;)    i32.const 1
(;@852   ;)    i32.store offset=4
(;@855   ;)    local.get 5
(;@857   ;)    i32.const 29
(;@859   ;)    i32.store offset=8
(;@85c   ;)    local.get 5
(;@85e   ;)    local.get 1
(;@860   ;)    i32.store offset=12
(;@863   ;)    local.get 5
(;@865   ;)    local.set 5
(;@867   ;)    local.get 4
(;@869   ;)    local.get 5
(;@86b   ;)    local.get 2
(;@86d   ;)    call $__mon_bind
(;@86f   ;)    return
             )
(;@872   ;)  (func $foo_lam_25 (;31;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@873   ;)    (local i32)
(;@875   ;)    i32.const 8
(;@877   ;)    call $alloc
(;@879   ;)    local.set 3
(;@87b   ;)    local.get 3
(;@87d   ;)    i32.const 0
(;@87f   ;)    i32.store
(;@882   ;)    local.get 3
(;@884   ;)    local.get 1
(;@886   ;)    i32.store offset=4
(;@889   ;)    local.get 3
(;@88b   ;)    return
             )
(;@88f   ;)  (func $foo_lam_26 (;32;) (type $fun_2_1) (param i32 i32) (result i32)
(;@890   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@892   ;)    local.get 0
(;@894   ;)    i32.load
(;@897   ;)    local.set 2
(;@899   ;)    local.get 2
(;@89b   ;)    i32.load offset=12
(;@89e   ;)    local.set 3
(;@8a0   ;)    local.get 3
(;@8a2   ;)    i32.load
(;@8a5   ;)    local.set 4
(;@8a7   ;)    local.get 4
(;@8a9   ;)    local.get 1
(;@8ab   ;)    call $__call_1
(;@8ad   ;)    local.set 5
(;@8af   ;)    local.get 5
(;@8b1   ;)    i32.load
(;@8b4   ;)    local.set 6
(;@8b6   ;)    i32.const 16
(;@8b8   ;)    call $alloc
(;@8ba   ;)    local.set 7
(;@8bc   ;)    local.get 7
(;@8be   ;)    i32.const 2
(;@8c0   ;)    i32.store
(;@8c3   ;)    local.get 7
(;@8c5   ;)    i32.const 1
(;@8c7   ;)    i32.store offset=4
(;@8ca   ;)    local.get 7
(;@8cc   ;)    i32.const 30
(;@8ce   ;)    i32.store offset=8
(;@8d1   ;)    local.get 7
(;@8d3   ;)    local.get 5
(;@8d5   ;)    i32.store offset=12
(;@8d8   ;)    local.get 7
(;@8da   ;)    local.set 7
(;@8dc   ;)    i32.const 12
(;@8de   ;)    call $alloc
(;@8e0   ;)    local.set 8
(;@8e2   ;)    local.get 8
(;@8e4   ;)    i32.const 2
(;@8e6   ;)    i32.store
(;@8e9   ;)    local.get 8
(;@8eb   ;)    i32.const 0
(;@8ed   ;)    i32.store offset=4
(;@8f0   ;)    local.get 8
(;@8f2   ;)    i32.const 31
(;@8f4   ;)    i32.store offset=8
(;@8f7   ;)    local.get 8
(;@8f9   ;)    local.set 8
(;@8fb   ;)    i32.const 12
(;@8fd   ;)    call $alloc
(;@8ff   ;)    local.set 9
(;@901   ;)    local.get 9
(;@903   ;)    local.get 6
(;@905   ;)    i32.store
(;@908   ;)    local.get 9
(;@90a   ;)    local.get 7
(;@90c   ;)    i32.store offset=4
(;@90f   ;)    local.get 9
(;@911   ;)    local.get 8
(;@913   ;)    i32.store offset=8
(;@916   ;)    local.get 9
(;@918   ;)    local.set 9
(;@91a   ;)    i32.const 8
(;@91c   ;)    call $alloc
(;@91e   ;)    local.set 10
(;@920   ;)    local.get 10
(;@922   ;)    i32.const 1
(;@924   ;)    i32.store
(;@927   ;)    local.get 10
(;@929   ;)    local.get 9
(;@92b   ;)    i32.store offset=4
(;@92e   ;)    local.get 10
(;@930   ;)    return
             )
(;@934   ;)  (func $foo_lam_27 (;33;) (type $fun_2_1) (param i32 i32) (result i32)
(;@935   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@937   ;)    local.get 0
(;@939   ;)    i32.load
(;@93c   ;)    local.set 2
(;@93e   ;)    local.get 0
(;@940   ;)    i32.load offset=4
(;@943   ;)    local.set 3
(;@945   ;)    local.get 0
(;@947   ;)    i32.load offset=8
(;@94a   ;)    local.set 4
(;@94c   ;)    i32.const 0
(;@94e   ;)    call $alloc
(;@950   ;)    local.set 5
(;@952   ;)    local.get 5
(;@954   ;)    local.set 5
(;@956   ;)    local.get 5
(;@958   ;)    call $__mon_generate_marker
(;@95a   ;)    local.set 6
(;@95c   ;)    i32.const 24
(;@95e   ;)    call $alloc
(;@960   ;)    local.set 7
(;@962   ;)    local.get 7
(;@964   ;)    i32.const 1
(;@966   ;)    i32.store
(;@969   ;)    local.get 7
(;@96b   ;)    i32.const 3
(;@96d   ;)    i32.store offset=4
(;@970   ;)    local.get 7
(;@972   ;)    i32.const 21
(;@974   ;)    i32.store offset=8
(;@977   ;)    local.get 7
(;@979   ;)    local.get 3
(;@97b   ;)    i32.store offset=12
(;@97e   ;)    local.get 7
(;@980   ;)    local.get 4
(;@982   ;)    i32.store offset=16
(;@985   ;)    local.get 7
(;@987   ;)    local.get 6
(;@989   ;)    i32.store offset=20
(;@98c   ;)    local.get 7
(;@98e   ;)    local.set 7
(;@990   ;)    i32.const 20
(;@992   ;)    call $alloc
(;@994   ;)    local.set 8
(;@996   ;)    local.get 8
(;@998   ;)    i32.const 1
(;@99a   ;)    i32.store
(;@99d   ;)    local.get 8
(;@99f   ;)    i32.const 2
(;@9a1   ;)    i32.store offset=4
(;@9a4   ;)    local.get 8
(;@9a6   ;)    i32.const 27
(;@9a8   ;)    i32.store offset=8
(;@9ab   ;)    local.get 8
(;@9ad   ;)    local.get 2
(;@9af   ;)    i32.store offset=12
(;@9b2   ;)    local.get 8
(;@9b4   ;)    local.get 1
(;@9b6   ;)    i32.store offset=16
(;@9b9   ;)    local.get 8
(;@9bb   ;)    local.set 8
(;@9bd   ;)    i32.const 16
(;@9bf   ;)    call $alloc
(;@9c1   ;)    local.set 9
(;@9c3   ;)    local.get 9
(;@9c5   ;)    i32.const 1
(;@9c7   ;)    i32.store
(;@9ca   ;)    local.get 9
(;@9cc   ;)    i32.const 1
(;@9ce   ;)    i32.store offset=4
(;@9d1   ;)    local.get 9
(;@9d3   ;)    i32.const 32
(;@9d5   ;)    i32.store offset=8
(;@9d8   ;)    local.get 9
(;@9da   ;)    local.get 3
(;@9dc   ;)    i32.store offset=12
(;@9df   ;)    local.get 9
(;@9e1   ;)    local.set 9
(;@9e3   ;)    local.get 6
(;@9e5   ;)    local.get 7
(;@9e7   ;)    local.get 8
(;@9e9   ;)    local.get 9
(;@9eb   ;)    local.get 1
(;@9ed   ;)    call $__mon_prompt
(;@9ef   ;)    return
             )
(;@9f3   ;)  (func $foo_lam_28 (;34;) (type $fun_2_1) (param i32 i32) (result i32)
(;@9f4   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32)
(;@9f6   ;)    local.get 0
(;@9f8   ;)    i32.load
(;@9fb   ;)    local.set 2
(;@9fd   ;)    local.get 0
(;@9ff   ;)    i32.load offset=4
(;@a02   ;)    local.set 3
(;@a04   ;)    local.get 0
(;@a06   ;)    i32.load offset=8
(;@a09   ;)    local.set 4
(;@a0b   ;)    local.get 0
(;@a0d   ;)    i32.load offset=12
(;@a10   ;)    local.set 5
(;@a12   ;)    i32.const 20
(;@a14   ;)    call $alloc
(;@a16   ;)    local.set 6
(;@a18   ;)    local.get 6
(;@a1a   ;)    i32.const 1
(;@a1c   ;)    i32.store
(;@a1f   ;)    local.get 6
(;@a21   ;)    i32.const 2
(;@a23   ;)    i32.store offset=4
(;@a26   ;)    local.get 6
(;@a28   ;)    i32.const 16
(;@a2a   ;)    i32.store offset=8
(;@a2d   ;)    local.get 6
(;@a2f   ;)    local.get 2
(;@a31   ;)    i32.store offset=12
(;@a34   ;)    local.get 6
(;@a36   ;)    local.get 5
(;@a38   ;)    i32.store offset=16
(;@a3b   ;)    local.get 6
(;@a3d   ;)    local.set 6
(;@a3f   ;)    i32.const 12
(;@a41   ;)    call $alloc
(;@a43   ;)    local.set 7
(;@a45   ;)    local.get 7
(;@a47   ;)    i32.const 2
(;@a49   ;)    i32.store
(;@a4c   ;)    local.get 7
(;@a4e   ;)    i32.const 0
(;@a50   ;)    i32.store offset=4
(;@a53   ;)    local.get 7
(;@a55   ;)    i32.const 18
(;@a57   ;)    i32.store offset=8
(;@a5a   ;)    local.get 7
(;@a5c   ;)    local.set 7
(;@a5e   ;)    i32.const 24
(;@a60   ;)    call $alloc
(;@a62   ;)    local.set 8
(;@a64   ;)    local.get 8
(;@a66   ;)    i32.const 1
(;@a68   ;)    i32.store
(;@a6b   ;)    local.get 8
(;@a6d   ;)    i32.const 3
(;@a6f   ;)    i32.store offset=4
(;@a72   ;)    local.get 8
(;@a74   ;)    i32.const 33
(;@a76   ;)    i32.store offset=8
(;@a79   ;)    local.get 8
(;@a7b   ;)    local.get 2
(;@a7d   ;)    i32.store offset=12
(;@a80   ;)    local.get 8
(;@a82   ;)    local.get 3
(;@a84   ;)    i32.store offset=16
(;@a87   ;)    local.get 8
(;@a89   ;)    local.get 4
(;@a8b   ;)    i32.store offset=20
(;@a8e   ;)    local.get 8
(;@a90   ;)    local.set 8
(;@a92   ;)    local.get 5
(;@a94   ;)    local.get 6
(;@a96   ;)    local.get 7
(;@a98   ;)    local.get 8
(;@a9a   ;)    local.get 1
(;@a9c   ;)    call $__mon_prompt
(;@a9e   ;)    return
             )
(;@aa1   ;)  (func $foo_lam_29 (;35;) (type $fun_2_1) (param i32 i32) (result i32)
(;@aa2   ;)    (local i32 i32 i32 i32 i32)
(;@aa4   ;)    local.get 0
(;@aa6   ;)    i32.load
(;@aa9   ;)    local.set 2
(;@aab   ;)    local.get 0
(;@aad   ;)    i32.load offset=4
(;@ab0   ;)    local.set 3
(;@ab2   ;)    i32.const 0
(;@ab4   ;)    call $alloc
(;@ab6   ;)    local.set 4
(;@ab8   ;)    local.get 4
(;@aba   ;)    local.set 4
(;@abc   ;)    local.get 4
(;@abe   ;)    call $__mon_generate_marker
(;@ac0   ;)    local.set 5
(;@ac2   ;)    i32.const 28
(;@ac4   ;)    call $alloc
(;@ac6   ;)    local.set 6
(;@ac8   ;)    local.get 6
(;@aca   ;)    i32.const 1
(;@acc   ;)    i32.store
(;@acf   ;)    local.get 6
(;@ad1   ;)    i32.const 4
(;@ad3   ;)    i32.store offset=4
(;@ad6   ;)    local.get 6
(;@ad8   ;)    i32.const 34
(;@ada   ;)    i32.store offset=8
(;@add   ;)    local.get 6
(;@adf   ;)    local.get 2
(;@ae1   ;)    i32.store offset=12
(;@ae4   ;)    local.get 6
(;@ae6   ;)    local.get 3
(;@ae8   ;)    i32.store offset=16
(;@aeb   ;)    local.get 6
(;@aed   ;)    local.get 1
(;@aef   ;)    i32.store offset=20
(;@af2   ;)    local.get 6
(;@af4   ;)    local.get 5
(;@af6   ;)    i32.store offset=24
(;@af9   ;)    local.get 6
(;@afb   ;)    return
             )
(;@aff   ;)  (func $main (;36;) (type $fun_0_1) (result i32)
(;@b00   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@b02   ;)    i32.const 12
(;@b04   ;)    call $alloc
(;@b06   ;)    local.set 0
(;@b08   ;)    local.get 0
(;@b0a   ;)    i32.const 1
(;@b0c   ;)    i32.store
(;@b0f   ;)    local.get 0
(;@b11   ;)    i32.const 0
(;@b13   ;)    i32.store offset=4
(;@b16   ;)    local.get 0
(;@b18   ;)    i32.const 53
(;@b1a   ;)    i32.store offset=8
(;@b1d   ;)    local.get 0
(;@b1f   ;)    local.set 0
(;@b21   ;)    i32.const 12
(;@b23   ;)    call $alloc
(;@b25   ;)    local.set 1
(;@b27   ;)    local.get 1
(;@b29   ;)    i32.const 2
(;@b2b   ;)    i32.store
(;@b2e   ;)    local.get 1
(;@b30   ;)    i32.const 0
(;@b32   ;)    i32.store offset=4
(;@b35   ;)    local.get 1
(;@b37   ;)    i32.const 54
(;@b39   ;)    i32.store offset=8
(;@b3c   ;)    local.get 1
(;@b3e   ;)    local.set 1
(;@b40   ;)    i32.const 0
(;@b42   ;)    call $alloc
(;@b44   ;)    local.set 2
(;@b46   ;)    local.get 2
(;@b48   ;)    local.set 2
(;@b4a   ;)    local.get 0
(;@b4c   ;)    local.get 1
(;@b4e   ;)    local.get 2
(;@b50   ;)    call $__mon_bind
(;@b52   ;)    local.set 3
(;@b54   ;)    local.get 3
(;@b56   ;)    i32.load
(;@b59   ;)    local.set 4
(;@b5b   ;)    block (result i32) ;; label = @1
(;@b5d   ;)      block ;; label = @2
(;@b5f   ;)        block ;; label = @3
(;@b61   ;)          block ;; label = @4
(;@b63   ;)            local.get 4
(;@b65   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@b6a   ;)          end
(;@b6b   ;)          local.get 3
(;@b6d   ;)          i32.load offset=4
(;@b70   ;)          local.set 5
(;@b72   ;)          local.get 5
(;@b74   ;)          br 2 (;@1;)
(;@b76   ;)        end
(;@b77   ;)        local.get 3
(;@b79   ;)        i32.load offset=4
(;@b7c   ;)        local.set 5
(;@b7e   ;)        i32.const 5467
(;@b81   ;)        br 1 (;@1;)
(;@b83   ;)      end
(;@b84   ;)      unreachable
(;@b85   ;)    end
(;@b86   ;)    return
             )
(;@b89   ;)  (func $main_lam_0 (;37;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@b8a   ;)    (local i32)
(;@b8c   ;)    local.get 2
(;@b8e   ;)    return
             )
(;@b91   ;)  (func $main_lam_1 (;38;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@b92   ;)    (local i32)
(;@b94   ;)    local.get 2
(;@b96   ;)    local.get 3
(;@b98   ;)    call $__call_1
(;@b9a   ;)    return
             )
(;@b9d   ;)  (func $main_lam_2 (;39;) (type $fun_2_1) (param i32 i32) (result i32)
(;@b9e   ;)    (local i32)
(;@ba0   ;)    i32.const 0
(;@ba2   ;)    call $alloc
(;@ba4   ;)    local.set 2
(;@ba6   ;)    local.get 2
(;@ba8   ;)    return
             )
(;@bab   ;)  (func $main_lam_3 (;40;) (type $fun_2_1) (param i32 i32) (result i32)
(;@bac   ;)    (local i32 i32)
(;@bae   ;)    local.get 1
(;@bb0   ;)    i32.load
(;@bb3   ;)    local.set 2
(;@bb5   ;)    block (result i32) ;; label = @1
(;@bb7   ;)      block ;; label = @2
(;@bb9   ;)        local.get 2
(;@bbb   ;)        br_table 0 (;@2;)
(;@bbe   ;)      end
(;@bbf   ;)      unreachable
(;@bc0   ;)    end
(;@bc1   ;)    return
             )
(;@bc4   ;)  (func $main_lam_4 (;41;) (type $fun_2_1) (param i32 i32) (result i32)
(;@bc5   ;)    (local i32)
(;@bc7   ;)    local.get 1
(;@bc9   ;)    return
             )
(;@bcc   ;)  (func $main_lam_5 (;42;) (type $fun_2_1) (param i32 i32) (result i32)
(;@bcd   ;)    (local i32)
(;@bcf   ;)    local.get 1
(;@bd1   ;)    return
             )
(;@bd4   ;)  (func $main_lam_6 (;43;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@bd5   ;)    (local i32)
(;@bd7   ;)    i32.const 8
(;@bd9   ;)    call $alloc
(;@bdb   ;)    local.set 3
(;@bdd   ;)    local.get 3
(;@bdf   ;)    local.get 1
(;@be1   ;)    i32.store
(;@be4   ;)    local.get 3
(;@be6   ;)    local.get 2
(;@be8   ;)    i32.store offset=4
(;@beb   ;)    local.get 3
(;@bed   ;)    return
             )
(;@bf0   ;)  (func $main_lam_7 (;44;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@bf1   ;)    (local i32 i32 i32 i32)
(;@bf3   ;)    local.get 3
(;@bf5   ;)    i32.load
(;@bf8   ;)    local.set 4
(;@bfa   ;)    block (result i32) ;; label = @1
(;@bfc   ;)      block ;; label = @2
(;@bfe   ;)        block ;; label = @3
(;@c00   ;)          block ;; label = @4
(;@c02   ;)            local.get 4
(;@c04   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@c09   ;)          end
(;@c0a   ;)          local.get 3
(;@c0c   ;)          i32.load offset=4
(;@c0f   ;)          local.set 5
(;@c11   ;)          local.get 1
(;@c13   ;)          local.get 5
(;@c15   ;)          call $__call_1
(;@c17   ;)          br 2 (;@1;)
(;@c19   ;)        end
(;@c1a   ;)        local.get 3
(;@c1c   ;)        i32.load offset=4
(;@c1f   ;)        local.set 6
(;@c21   ;)        local.get 2
(;@c23   ;)        local.get 6
(;@c25   ;)        call $__call_1
(;@c27   ;)        br 1 (;@1;)
(;@c29   ;)      end
(;@c2a   ;)      unreachable
(;@c2b   ;)    end
(;@c2c   ;)    return
             )
(;@c2f   ;)  (func $main_lam_8 (;45;) (type $fun_2_1) (param i32 i32) (result i32)
(;@c30   ;)    (local i32)
(;@c32   ;)    local.get 1
(;@c34   ;)    i32.load
(;@c37   ;)    return
             )
(;@c3a   ;)  (func $main_lam_9 (;46;) (type $fun_2_1) (param i32 i32) (result i32)
(;@c3b   ;)    (local i32)
(;@c3d   ;)    i32.const 8
(;@c3f   ;)    call $alloc
(;@c41   ;)    local.set 2
(;@c43   ;)    local.get 2
(;@c45   ;)    i32.const 0
(;@c47   ;)    i32.store
(;@c4a   ;)    local.get 2
(;@c4c   ;)    local.get 1
(;@c4e   ;)    i32.store offset=4
(;@c51   ;)    local.get 2
(;@c53   ;)    return
             )
(;@c56   ;)  (func $main_lam_10 (;47;) (type $fun_2_1) (param i32 i32) (result i32)
(;@c57   ;)    (local i32)
(;@c59   ;)    local.get 1
(;@c5b   ;)    i32.load offset=4
(;@c5e   ;)    return
             )
(;@c61   ;)  (func $main_lam_11 (;48;) (type $fun_2_1) (param i32 i32) (result i32)
(;@c62   ;)    (local i32)
(;@c64   ;)    i32.const 8
(;@c66   ;)    call $alloc
(;@c68   ;)    local.set 2
(;@c6a   ;)    local.get 2
(;@c6c   ;)    i32.const 1
(;@c6e   ;)    i32.store
(;@c71   ;)    local.get 2
(;@c73   ;)    local.get 1
(;@c75   ;)    i32.store offset=4
(;@c78   ;)    local.get 2
(;@c7a   ;)    return
             )
(;@c7e   ;)  (func $main_lam_12 (;49;) (type $fun_2_1) (param i32 i32) (result i32)
(;@c7f   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@c81   ;)    i32.const 12
(;@c83   ;)    call $alloc
(;@c85   ;)    local.set 2
(;@c87   ;)    local.get 2
(;@c89   ;)    i32.const 2
(;@c8b   ;)    i32.store
(;@c8e   ;)    local.get 2
(;@c90   ;)    i32.const 0
(;@c92   ;)    i32.store offset=4
(;@c95   ;)    local.get 2
(;@c97   ;)    i32.const 37
(;@c99   ;)    i32.store offset=8
(;@c9c   ;)    local.get 2
(;@c9e   ;)    local.set 2
(;@ca0   ;)    i32.const 12
(;@ca2   ;)    call $alloc
(;@ca4   ;)    local.set 3
(;@ca6   ;)    local.get 3
(;@ca8   ;)    i32.const 3
(;@caa   ;)    i32.store
(;@cad   ;)    local.get 3
(;@caf   ;)    i32.const 0
(;@cb1   ;)    i32.store offset=4
(;@cb4   ;)    local.get 3
(;@cb6   ;)    i32.const 38
(;@cb8   ;)    i32.store offset=8
(;@cbb   ;)    local.get 3
(;@cbd   ;)    local.set 3
(;@cbf   ;)    i32.const 12
(;@cc1   ;)    call $alloc
(;@cc3   ;)    local.set 4
(;@cc5   ;)    local.get 4
(;@cc7   ;)    i32.const 1
(;@cc9   ;)    i32.store
(;@ccc   ;)    local.get 4
(;@cce   ;)    i32.const 0
(;@cd0   ;)    i32.store offset=4
(;@cd3   ;)    local.get 4
(;@cd5   ;)    i32.const 39
(;@cd7   ;)    i32.store offset=8
(;@cda   ;)    local.get 4
(;@cdc   ;)    local.set 4
(;@cde   ;)    i32.const 12
(;@ce0   ;)    call $alloc
(;@ce2   ;)    local.set 5
(;@ce4   ;)    local.get 5
(;@ce6   ;)    i32.const 1
(;@ce8   ;)    i32.store
(;@ceb   ;)    local.get 5
(;@ced   ;)    i32.const 0
(;@cef   ;)    i32.store offset=4
(;@cf2   ;)    local.get 5
(;@cf4   ;)    i32.const 40
(;@cf6   ;)    i32.store offset=8
(;@cf9   ;)    local.get 5
(;@cfb   ;)    local.set 5
(;@cfd   ;)    i32.const 8
(;@cff   ;)    call $alloc
(;@d01   ;)    local.set 6
(;@d03   ;)    local.get 6
(;@d05   ;)    local.get 4
(;@d07   ;)    i32.store
(;@d0a   ;)    local.get 6
(;@d0c   ;)    local.get 5
(;@d0e   ;)    i32.store offset=4
(;@d11   ;)    local.get 6
(;@d13   ;)    local.set 6
(;@d15   ;)    i32.const 12
(;@d17   ;)    call $alloc
(;@d19   ;)    local.set 7
(;@d1b   ;)    local.get 7
(;@d1d   ;)    i32.const 1
(;@d1f   ;)    i32.store
(;@d22   ;)    local.get 7
(;@d24   ;)    i32.const 0
(;@d26   ;)    i32.store offset=4
(;@d29   ;)    local.get 7
(;@d2b   ;)    i32.const 41
(;@d2d   ;)    i32.store offset=8
(;@d30   ;)    local.get 7
(;@d32   ;)    local.set 7
(;@d34   ;)    i32.const 12
(;@d36   ;)    call $alloc
(;@d38   ;)    local.set 8
(;@d3a   ;)    local.get 8
(;@d3c   ;)    i32.const 1
(;@d3e   ;)    i32.store
(;@d41   ;)    local.get 8
(;@d43   ;)    i32.const 0
(;@d45   ;)    i32.store offset=4
(;@d48   ;)    local.get 8
(;@d4a   ;)    i32.const 42
(;@d4c   ;)    i32.store offset=8
(;@d4f   ;)    local.get 8
(;@d51   ;)    local.set 8
(;@d53   ;)    i32.const 8
(;@d55   ;)    call $alloc
(;@d57   ;)    local.set 9
(;@d59   ;)    local.get 9
(;@d5b   ;)    local.get 7
(;@d5d   ;)    i32.store
(;@d60   ;)    local.get 9
(;@d62   ;)    local.get 8
(;@d64   ;)    i32.store offset=4
(;@d67   ;)    local.get 9
(;@d69   ;)    local.set 9
(;@d6b   ;)    i32.const 16
(;@d6d   ;)    call $alloc
(;@d6f   ;)    local.set 10
(;@d71   ;)    local.get 10
(;@d73   ;)    local.get 2
(;@d75   ;)    i32.store
(;@d78   ;)    local.get 10
(;@d7a   ;)    local.get 3
(;@d7c   ;)    i32.store offset=4
(;@d7f   ;)    local.get 10
(;@d81   ;)    local.get 6
(;@d83   ;)    i32.store offset=8
(;@d86   ;)    local.get 10
(;@d88   ;)    local.get 9
(;@d8a   ;)    i32.store offset=12
(;@d8d   ;)    local.get 10
(;@d8f   ;)    local.set 10
(;@d91   ;)    i32.const 12
(;@d93   ;)    call $alloc
(;@d95   ;)    local.set 11
(;@d97   ;)    local.get 11
(;@d99   ;)    i32.const 2
(;@d9b   ;)    i32.store
(;@d9e   ;)    local.get 11
(;@da0   ;)    i32.const 0
(;@da2   ;)    i32.store offset=4
(;@da5   ;)    local.get 11
(;@da7   ;)    i32.const 43
(;@da9   ;)    i32.store offset=8
(;@dac   ;)    local.get 11
(;@dae   ;)    local.set 11
(;@db0   ;)    i32.const 12
(;@db2   ;)    call $alloc
(;@db4   ;)    local.set 12
(;@db6   ;)    local.get 12
(;@db8   ;)    i32.const 3
(;@dba   ;)    i32.store
(;@dbd   ;)    local.get 12
(;@dbf   ;)    i32.const 0
(;@dc1   ;)    i32.store offset=4
(;@dc4   ;)    local.get 12
(;@dc6   ;)    i32.const 44
(;@dc8   ;)    i32.store offset=8
(;@dcb   ;)    local.get 12
(;@dcd   ;)    local.set 12
(;@dcf   ;)    i32.const 12
(;@dd1   ;)    call $alloc
(;@dd3   ;)    local.set 13
(;@dd5   ;)    local.get 13
(;@dd7   ;)    i32.const 1
(;@dd9   ;)    i32.store
(;@ddc   ;)    local.get 13
(;@dde   ;)    i32.const 0
(;@de0   ;)    i32.store offset=4
(;@de3   ;)    local.get 13
(;@de5   ;)    i32.const 45
(;@de7   ;)    i32.store offset=8
(;@dea   ;)    local.get 13
(;@dec   ;)    local.set 13
(;@dee   ;)    i32.const 12
(;@df0   ;)    call $alloc
(;@df2   ;)    local.set 14
(;@df4   ;)    local.get 14
(;@df6   ;)    i32.const 1
(;@df8   ;)    i32.store
(;@dfb   ;)    local.get 14
(;@dfd   ;)    i32.const 0
(;@dff   ;)    i32.store offset=4
(;@e02   ;)    local.get 14
(;@e04   ;)    i32.const 46
(;@e06   ;)    i32.store offset=8
(;@e09   ;)    local.get 14
(;@e0b   ;)    local.set 14
(;@e0d   ;)    i32.const 8
(;@e0f   ;)    call $alloc
(;@e11   ;)    local.set 15
(;@e13   ;)    local.get 15
(;@e15   ;)    local.get 13
(;@e17   ;)    i32.store
(;@e1a   ;)    local.get 15
(;@e1c   ;)    local.get 14
(;@e1e   ;)    i32.store offset=4
(;@e21   ;)    local.get 15
(;@e23   ;)    local.set 15
(;@e25   ;)    i32.const 12
(;@e27   ;)    call $alloc
(;@e29   ;)    local.set 16
(;@e2b   ;)    local.get 16
(;@e2d   ;)    i32.const 1
(;@e2f   ;)    i32.store
(;@e32   ;)    local.get 16
(;@e34   ;)    i32.const 0
(;@e36   ;)    i32.store offset=4
(;@e39   ;)    local.get 16
(;@e3b   ;)    i32.const 47
(;@e3d   ;)    i32.store offset=8
(;@e40   ;)    local.get 16
(;@e42   ;)    local.set 16
(;@e44   ;)    i32.const 12
(;@e46   ;)    call $alloc
(;@e48   ;)    local.set 17
(;@e4a   ;)    local.get 17
(;@e4c   ;)    i32.const 1
(;@e4e   ;)    i32.store
(;@e51   ;)    local.get 17
(;@e53   ;)    i32.const 0
(;@e55   ;)    i32.store offset=4
(;@e58   ;)    local.get 17
(;@e5a   ;)    i32.const 48
(;@e5c   ;)    i32.store offset=8
(;@e5f   ;)    local.get 17
(;@e61   ;)    local.set 17
(;@e63   ;)    i32.const 8
(;@e65   ;)    call $alloc
(;@e67   ;)    local.set 18
(;@e69   ;)    local.get 18
(;@e6b   ;)    local.get 16
(;@e6d   ;)    i32.store
(;@e70   ;)    local.get 18
(;@e72   ;)    local.get 17
(;@e74   ;)    i32.store offset=4
(;@e77   ;)    local.get 18
(;@e79   ;)    local.set 18
(;@e7b   ;)    i32.const 16
(;@e7d   ;)    call $alloc
(;@e7f   ;)    local.set 19
(;@e81   ;)    local.get 19
(;@e83   ;)    local.get 11
(;@e85   ;)    i32.store
(;@e88   ;)    local.get 19
(;@e8a   ;)    local.get 12
(;@e8c   ;)    i32.store offset=4
(;@e8f   ;)    local.get 19
(;@e91   ;)    local.get 15
(;@e93   ;)    i32.store offset=8
(;@e96   ;)    local.get 19
(;@e98   ;)    local.get 18
(;@e9a   ;)    i32.store offset=12
(;@e9d   ;)    local.get 19
(;@e9f   ;)    local.set 19
(;@ea1   ;)    local.get 10
(;@ea3   ;)    local.get 19
(;@ea5   ;)    local.get 1
(;@ea7   ;)    call $foo
(;@ea9   ;)    return
             )
(;@eac   ;)  (func $main_lam_13 (;50;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@ead   ;)    (local i32)
(;@eaf   ;)    local.get 1
(;@eb1   ;)    i32.const 78543
(;@eb5   ;)    local.get 2
(;@eb7   ;)    call $__call_2
(;@eb9   ;)    return
             )
(;@ebc   ;)  (func $main_lam_14 (;51;) (type $fun_2_1) (param i32 i32) (result i32)
(;@ebd   ;)    (local i32 i32 i32)
(;@ebf   ;)    i32.const 12
(;@ec1   ;)    call $alloc
(;@ec3   ;)    local.set 2
(;@ec5   ;)    local.get 2
(;@ec7   ;)    i32.const 1
(;@ec9   ;)    i32.store
(;@ecc   ;)    local.get 2
(;@ece   ;)    i32.const 0
(;@ed0   ;)    i32.store offset=4
(;@ed3   ;)    local.get 2
(;@ed5   ;)    i32.const 49
(;@ed7   ;)    i32.store offset=8
(;@eda   ;)    local.get 2
(;@edc   ;)    local.set 2
(;@ede   ;)    i32.const 12
(;@ee0   ;)    call $alloc
(;@ee2   ;)    local.set 3
(;@ee4   ;)    local.get 3
(;@ee6   ;)    i32.const 2
(;@ee8   ;)    i32.store
(;@eeb   ;)    local.get 3
(;@eed   ;)    i32.const 0
(;@eef   ;)    i32.store offset=4
(;@ef2   ;)    local.get 3
(;@ef4   ;)    i32.const 50
(;@ef6   ;)    i32.store offset=8
(;@ef9   ;)    local.get 3
(;@efb   ;)    local.set 3
(;@efd   ;)    local.get 2
(;@eff   ;)    local.get 3
(;@f01   ;)    local.get 1
(;@f03   ;)    call $__mon_bind
(;@f05   ;)    return
             )
(;@f08   ;)  (func $main_lam_15 (;52;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@f09   ;)    (local i32)
(;@f0b   ;)    local.get 1
(;@f0d   ;)    i32.const 16763
(;@f11   ;)    local.get 2
(;@f13   ;)    call $__call_2
(;@f15   ;)    return
             )
(;@f18   ;)  (func $main_lam_16 (;53;) (type $fun_2_1) (param i32 i32) (result i32)
(;@f19   ;)    (local i32 i32 i32)
(;@f1b   ;)    i32.const 12
(;@f1d   ;)    call $alloc
(;@f1f   ;)    local.set 2
(;@f21   ;)    local.get 2
(;@f23   ;)    i32.const 1
(;@f25   ;)    i32.store
(;@f28   ;)    local.get 2
(;@f2a   ;)    i32.const 0
(;@f2c   ;)    i32.store offset=4
(;@f2f   ;)    local.get 2
(;@f31   ;)    i32.const 51
(;@f33   ;)    i32.store offset=8
(;@f36   ;)    local.get 2
(;@f38   ;)    local.set 2
(;@f3a   ;)    i32.const 12
(;@f3c   ;)    call $alloc
(;@f3e   ;)    local.set 3
(;@f40   ;)    local.get 3
(;@f42   ;)    i32.const 2
(;@f44   ;)    i32.store
(;@f47   ;)    local.get 3
(;@f49   ;)    i32.const 0
(;@f4b   ;)    i32.store offset=4
(;@f4e   ;)    local.get 3
(;@f50   ;)    i32.const 52
(;@f52   ;)    i32.store offset=8
(;@f55   ;)    local.get 3
(;@f57   ;)    local.set 3
(;@f59   ;)    local.get 2
(;@f5b   ;)    local.get 3
(;@f5d   ;)    local.get 1
(;@f5f   ;)    call $__mon_bind
(;@f61   ;)    return
             )
(;@f64   ;)  (func $main_lam_17 (;54;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@f65   ;)    (local i32 i32)
(;@f67   ;)    local.get 1
(;@f69   ;)    i32.load
(;@f6c   ;)    local.set 3
(;@f6e   ;)    i32.const 8
(;@f70   ;)    call $alloc
(;@f72   ;)    local.set 4
(;@f74   ;)    local.get 4
(;@f76   ;)    i32.const 0
(;@f78   ;)    i32.store
(;@f7b   ;)    local.get 4
(;@f7d   ;)    local.get 3
(;@f7f   ;)    i32.store offset=4
(;@f82   ;)    local.get 4
(;@f84   ;)    return
             )
(;@f88   ;)  (func $__mon_bind (;55;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@f89   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@f8b   ;)    local.get 0
(;@f8d   ;)    local.get 2
(;@f8f   ;)    call $__call_1
(;@f91   ;)    local.set 3
(;@f93   ;)    local.get 3
(;@f95   ;)    i32.load
(;@f98   ;)    local.set 4
(;@f9a   ;)    block (result i32) ;; label = @1
(;@f9c   ;)      block ;; label = @2
(;@f9e   ;)        block ;; label = @3
(;@fa0   ;)          block ;; label = @4
(;@fa2   ;)            local.get 4
(;@fa4   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@fa9   ;)          end
(;@faa   ;)          local.get 3
(;@fac   ;)          i32.load offset=4
(;@faf   ;)          local.set 5
(;@fb1   ;)          local.get 1
(;@fb3   ;)          local.get 5
(;@fb5   ;)          local.get 2
(;@fb7   ;)          call $__call_2
(;@fb9   ;)          br 2 (;@1;)
(;@fbb   ;)        end
(;@fbc   ;)        local.get 3
(;@fbe   ;)        i32.load offset=4
(;@fc1   ;)        local.set 6
(;@fc3   ;)        local.get 6
(;@fc5   ;)        local.set 7
(;@fc7   ;)        local.get 7
(;@fc9   ;)        i32.load
(;@fcc   ;)        local.set 8
(;@fce   ;)        local.get 7
(;@fd0   ;)        i32.load offset=4
(;@fd3   ;)        local.set 9
(;@fd5   ;)        i32.const 20
(;@fd7   ;)        call $alloc
(;@fd9   ;)        local.set 10
(;@fdb   ;)        local.get 10
(;@fdd   ;)        i32.const 2
(;@fdf   ;)        i32.store
(;@fe2   ;)        local.get 10
(;@fe4   ;)        i32.const 2
(;@fe6   ;)        i32.store offset=4
(;@fe9   ;)        local.get 10
(;@feb   ;)        i32.const 57
(;@fed   ;)        i32.store offset=8
(;@ff0   ;)        local.get 10
(;@ff2   ;)        local.get 1
(;@ff4   ;)        i32.store offset=12
(;@ff7   ;)        local.get 10
(;@ff9   ;)        local.get 7
(;@ffb   ;)        i32.store offset=16
(;@ffe   ;)        local.get 10
(;@1000  ;)        local.set 10
(;@1002  ;)        i32.const 12
(;@1004  ;)        call $alloc
(;@1006  ;)        local.set 11
(;@1008  ;)        local.get 11
(;@100a  ;)        local.get 8
(;@100c  ;)        i32.store
(;@100f  ;)        local.get 11
(;@1011  ;)        local.get 9
(;@1013  ;)        i32.store offset=4
(;@1016  ;)        local.get 11
(;@1018  ;)        local.get 10
(;@101a  ;)        i32.store offset=8
(;@101d  ;)        local.get 11
(;@101f  ;)        local.set 11
(;@1021  ;)        i32.const 8
(;@1023  ;)        call $alloc
(;@1025  ;)        local.set 12
(;@1027  ;)        local.get 12
(;@1029  ;)        i32.const 1
(;@102b  ;)        i32.store
(;@102e  ;)        local.get 12
(;@1030  ;)        local.get 11
(;@1032  ;)        i32.store offset=4
(;@1035  ;)        local.get 12
(;@1037  ;)        br 1 (;@1;)
(;@1039  ;)      end
(;@103a  ;)      unreachable
(;@103b  ;)    end
(;@103c  ;)    return
             )
(;@103f  ;)  (func $__mon_bind_lam_0 (;56;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1040  ;)    (local i32 i32 i32 i32)
(;@1042  ;)    local.get 0
(;@1044  ;)    i32.load
(;@1047  ;)    local.set 2
(;@1049  ;)    local.get 0
(;@104b  ;)    i32.load offset=4
(;@104e  ;)    local.set 3
(;@1050  ;)    local.get 2
(;@1052  ;)    i32.load offset=8
(;@1055  ;)    local.set 4
(;@1057  ;)    local.get 4
(;@1059  ;)    local.get 3
(;@105b  ;)    local.get 1
(;@105d  ;)    call $__call_2
(;@105f  ;)    return
             )
(;@1062  ;)  (func $__mon_bind_lam_1 (;57;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1063  ;)    (local i32 i32 i32 i32)
(;@1065  ;)    local.get 0
(;@1067  ;)    i32.load
(;@106a  ;)    local.set 3
(;@106c  ;)    local.get 0
(;@106e  ;)    i32.load offset=4
(;@1071  ;)    local.set 4
(;@1073  ;)    i32.const 20
(;@1075  ;)    call $alloc
(;@1077  ;)    local.set 5
(;@1079  ;)    local.get 5
(;@107b  ;)    i32.const 1
(;@107d  ;)    i32.store
(;@1080  ;)    local.get 5
(;@1082  ;)    i32.const 2
(;@1084  ;)    i32.store offset=4
(;@1087  ;)    local.get 5
(;@1089  ;)    i32.const 56
(;@108b  ;)    i32.store offset=8
(;@108e  ;)    local.get 5
(;@1090  ;)    local.get 4
(;@1092  ;)    i32.store offset=12
(;@1095  ;)    local.get 5
(;@1097  ;)    local.get 1
(;@1099  ;)    i32.store offset=16
(;@109c  ;)    local.get 5
(;@109e  ;)    local.set 5
(;@10a0  ;)    local.get 5
(;@10a2  ;)    local.get 3
(;@10a4  ;)    local.get 2
(;@10a6  ;)    call $__mon_bind
(;@10a8  ;)    return
             )
(;@10ac  ;)  (func $__mon_prompt (;58;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@10ad  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@10af  ;)    local.get 1
(;@10b1  ;)    local.get 4
(;@10b3  ;)    call $__call_1
(;@10b5  ;)    local.set 5
(;@10b7  ;)    local.get 3
(;@10b9  ;)    local.get 5
(;@10bb  ;)    call $__call_1
(;@10bd  ;)    local.set 6
(;@10bf  ;)    local.get 6
(;@10c1  ;)    i32.load
(;@10c4  ;)    local.set 7
(;@10c6  ;)    block (result i32) ;; label = @1
(;@10c8  ;)      block ;; label = @2
(;@10ca  ;)        block ;; label = @3
(;@10cc  ;)          block ;; label = @4
(;@10ce  ;)            local.get 7
(;@10d0  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@10d5  ;)          end
(;@10d6  ;)          local.get 6
(;@10d8  ;)          i32.load offset=4
(;@10db  ;)          local.set 8
(;@10dd  ;)          local.get 2
(;@10df  ;)          local.get 8
(;@10e1  ;)          local.get 4
(;@10e3  ;)          call $__call_2
(;@10e5  ;)          br 2 (;@1;)
(;@10e7  ;)        end
(;@10e8  ;)        local.get 6
(;@10ea  ;)        i32.load offset=4
(;@10ed  ;)        local.set 9
(;@10ef  ;)        local.get 9
(;@10f1  ;)        i32.load
(;@10f4  ;)        local.set 10
(;@10f6  ;)        local.get 0
(;@10f8  ;)        local.get 10
(;@10fa  ;)        call $__mon_eqm
(;@10fc  ;)        local.set 11
(;@10fe  ;)        local.get 11
(;@1100  ;)        i32.load
(;@1103  ;)        local.set 12
(;@1105  ;)        block (result i32) ;; label = @3
(;@1107  ;)          block ;; label = @4
(;@1109  ;)            block ;; label = @5
(;@110b  ;)              block ;; label = @6
(;@110d  ;)                local.get 12
(;@110f  ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@1114  ;)              end
(;@1115  ;)              local.get 11
(;@1117  ;)              i32.load offset=4
(;@111a  ;)              local.set 13
(;@111c  ;)              local.get 9
(;@111e  ;)              local.set 14
(;@1120  ;)              local.get 14
(;@1122  ;)              i32.load
(;@1125  ;)              local.set 15
(;@1127  ;)              local.get 14
(;@1129  ;)              i32.load offset=4
(;@112c  ;)              local.set 16
(;@112e  ;)              i32.const 28
(;@1130  ;)              call $alloc
(;@1132  ;)              local.set 17
(;@1134  ;)              local.get 17
(;@1136  ;)              i32.const 2
(;@1138  ;)              i32.store
(;@113b  ;)              local.get 17
(;@113d  ;)              i32.const 4
(;@113f  ;)              i32.store offset=4
(;@1142  ;)              local.get 17
(;@1144  ;)              i32.const 60
(;@1146  ;)              i32.store offset=8
(;@1149  ;)              local.get 17
(;@114b  ;)              local.get 0
(;@114d  ;)              i32.store offset=12
(;@1150  ;)              local.get 17
(;@1152  ;)              local.get 1
(;@1154  ;)              i32.store offset=16
(;@1157  ;)              local.get 17
(;@1159  ;)              local.get 2
(;@115b  ;)              i32.store offset=20
(;@115e  ;)              local.get 17
(;@1160  ;)              local.get 14
(;@1162  ;)              i32.store offset=24
(;@1165  ;)              local.get 17
(;@1167  ;)              local.set 17
(;@1169  ;)              i32.const 12
(;@116b  ;)              call $alloc
(;@116d  ;)              local.set 18
(;@116f  ;)              local.get 18
(;@1171  ;)              local.get 15
(;@1173  ;)              i32.store
(;@1176  ;)              local.get 18
(;@1178  ;)              local.get 16
(;@117a  ;)              i32.store offset=4
(;@117d  ;)              local.get 18
(;@117f  ;)              local.get 17
(;@1181  ;)              i32.store offset=8
(;@1184  ;)              local.get 18
(;@1186  ;)              local.set 18
(;@1188  ;)              i32.const 8
(;@118a  ;)              call $alloc
(;@118c  ;)              local.set 19
(;@118e  ;)              local.get 19
(;@1190  ;)              i32.const 1
(;@1192  ;)              i32.store
(;@1195  ;)              local.get 19
(;@1197  ;)              local.get 18
(;@1199  ;)              i32.store offset=4
(;@119c  ;)              local.get 19
(;@119e  ;)              br 2 (;@3;)
(;@11a0  ;)            end
(;@11a1  ;)            local.get 11
(;@11a3  ;)            i32.load offset=4
(;@11a6  ;)            local.set 13
(;@11a8  ;)            i32.const 28
(;@11aa  ;)            call $alloc
(;@11ac  ;)            local.set 19
(;@11ae  ;)            local.get 19
(;@11b0  ;)            i32.const 2
(;@11b2  ;)            i32.store
(;@11b5  ;)            local.get 19
(;@11b7  ;)            i32.const 4
(;@11b9  ;)            i32.store offset=4
(;@11bc  ;)            local.get 19
(;@11be  ;)            i32.const 62
(;@11c0  ;)            i32.store offset=8
(;@11c3  ;)            local.get 19
(;@11c5  ;)            local.get 0
(;@11c7  ;)            i32.store offset=12
(;@11ca  ;)            local.get 19
(;@11cc  ;)            local.get 1
(;@11ce  ;)            i32.store offset=16
(;@11d1  ;)            local.get 19
(;@11d3  ;)            local.get 2
(;@11d5  ;)            i32.store offset=20
(;@11d8  ;)            local.get 19
(;@11da  ;)            local.get 9
(;@11dc  ;)            i32.store offset=24
(;@11df  ;)            local.get 19
(;@11e1  ;)            local.set 19
(;@11e3  ;)            local.get 9
(;@11e5  ;)            i32.load offset=4
(;@11e8  ;)            local.set 20
(;@11ea  ;)            local.get 20
(;@11ec  ;)            local.get 19
(;@11ee  ;)            local.get 4
(;@11f0  ;)            call $__call_2
(;@11f2  ;)            br 1 (;@3;)
(;@11f4  ;)          end
(;@11f5  ;)          unreachable
(;@11f6  ;)        end
(;@11f7  ;)        br 1 (;@1;)
(;@11f9  ;)      end
(;@11fa  ;)      unreachable
(;@11fb  ;)    end
(;@11fc  ;)    return
             )
(;@11ff  ;)  (func $__mon_prompt_lam_0 (;59;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1200  ;)    (local i32 i32 i32 i32)
(;@1202  ;)    local.get 0
(;@1204  ;)    i32.load
(;@1207  ;)    local.set 2
(;@1209  ;)    local.get 0
(;@120b  ;)    i32.load offset=4
(;@120e  ;)    local.set 3
(;@1210  ;)    local.get 2
(;@1212  ;)    i32.load offset=8
(;@1215  ;)    local.set 4
(;@1217  ;)    local.get 4
(;@1219  ;)    local.get 3
(;@121b  ;)    local.get 1
(;@121d  ;)    call $__call_2
(;@121f  ;)    return
             )
(;@1222  ;)  (func $__mon_prompt_lam_1 (;60;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1223  ;)    (local i32 i32 i32 i32 i32 i32)
(;@1225  ;)    local.get 0
(;@1227  ;)    i32.load
(;@122a  ;)    local.set 3
(;@122c  ;)    local.get 0
(;@122e  ;)    i32.load offset=4
(;@1231  ;)    local.set 4
(;@1233  ;)    local.get 0
(;@1235  ;)    i32.load offset=8
(;@1238  ;)    local.set 5
(;@123a  ;)    local.get 0
(;@123c  ;)    i32.load offset=12
(;@123f  ;)    local.set 6
(;@1241  ;)    i32.const 20
(;@1243  ;)    call $alloc
(;@1245  ;)    local.set 7
(;@1247  ;)    local.get 7
(;@1249  ;)    i32.const 1
(;@124b  ;)    i32.store
(;@124e  ;)    local.get 7
(;@1250  ;)    i32.const 2
(;@1252  ;)    i32.store offset=4
(;@1255  ;)    local.get 7
(;@1257  ;)    i32.const 59
(;@1259  ;)    i32.store offset=8
(;@125c  ;)    local.get 7
(;@125e  ;)    local.get 6
(;@1260  ;)    i32.store offset=12
(;@1263  ;)    local.get 7
(;@1265  ;)    local.get 1
(;@1267  ;)    i32.store offset=16
(;@126a  ;)    local.get 7
(;@126c  ;)    local.set 7
(;@126e  ;)    local.get 3
(;@1270  ;)    local.get 4
(;@1272  ;)    local.get 5
(;@1274  ;)    local.get 7
(;@1276  ;)    local.get 2
(;@1278  ;)    call $__mon_prompt
(;@127a  ;)    return
             )
(;@127d  ;)  (func $__mon_prompt_lam_2 (;61;) (type $fun_2_1) (param i32 i32) (result i32)
(;@127e  ;)    (local i32 i32 i32 i32)
(;@1280  ;)    local.get 0
(;@1282  ;)    i32.load
(;@1285  ;)    local.set 2
(;@1287  ;)    local.get 0
(;@1289  ;)    i32.load offset=4
(;@128c  ;)    local.set 3
(;@128e  ;)    local.get 2
(;@1290  ;)    i32.load offset=8
(;@1293  ;)    local.set 4
(;@1295  ;)    local.get 4
(;@1297  ;)    local.get 3
(;@1299  ;)    local.get 1
(;@129b  ;)    call $__call_2
(;@129d  ;)    return
             )
(;@12a0  ;)  (func $__mon_prompt_lam_3 (;62;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@12a1  ;)    (local i32 i32 i32 i32 i32 i32)
(;@12a3  ;)    local.get 0
(;@12a5  ;)    i32.load
(;@12a8  ;)    local.set 3
(;@12aa  ;)    local.get 0
(;@12ac  ;)    i32.load offset=4
(;@12af  ;)    local.set 4
(;@12b1  ;)    local.get 0
(;@12b3  ;)    i32.load offset=8
(;@12b6  ;)    local.set 5
(;@12b8  ;)    local.get 0
(;@12ba  ;)    i32.load offset=12
(;@12bd  ;)    local.set 6
(;@12bf  ;)    i32.const 20
(;@12c1  ;)    call $alloc
(;@12c3  ;)    local.set 7
(;@12c5  ;)    local.get 7
(;@12c7  ;)    i32.const 1
(;@12c9  ;)    i32.store
(;@12cc  ;)    local.get 7
(;@12ce  ;)    i32.const 2
(;@12d0  ;)    i32.store offset=4
(;@12d3  ;)    local.get 7
(;@12d5  ;)    i32.const 61
(;@12d7  ;)    i32.store offset=8
(;@12da  ;)    local.get 7
(;@12dc  ;)    local.get 6
(;@12de  ;)    i32.store offset=12
(;@12e1  ;)    local.get 7
(;@12e3  ;)    local.get 1
(;@12e5  ;)    i32.store offset=16
(;@12e8  ;)    local.get 7
(;@12ea  ;)    local.set 7
(;@12ec  ;)    local.get 3
(;@12ee  ;)    local.get 4
(;@12f0  ;)    local.get 5
(;@12f2  ;)    local.get 7
(;@12f4  ;)    local.get 2
(;@12f6  ;)    call $__mon_prompt
(;@12f8  ;)    return
             )
(;@7a    ;)  (table (;0;) 63 63 funcref)
(;@81    ;)  (memory (;0;) 1)
(;@86    ;)  (global (;0;) (mut i32) i32.const 0)
(;@8b    ;)  (global (;1;) (mut i32) i32.const 0)
(;@93    ;)  (export "main" (func $main))
(;@9a    ;)  (export "mem" (memory 0))
(;@a3    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__call_1 $__call_2 $foo $foo_lam_0 $foo_lam_1 $foo_lam_2 $foo_lam_3 $foo_lam_4 $foo_lam_5 $foo_lam_6 $foo_lam_7 $foo_lam_8 $foo_lam_9 $foo_lam_10 $foo_lam_11 $foo_lam_12 $foo_lam_13 $foo_lam_14 $foo_lam_15 $foo_lam_16 $foo_lam_17 $foo_lam_18 $foo_lam_19 $foo_lam_20 $foo_lam_21 $foo_lam_22 $foo_lam_23 $foo_lam_24 $foo_lam_25 $foo_lam_26 $foo_lam_27 $foo_lam_28 $foo_lam_29 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11 $main_lam_12 $main_lam_13 $main_lam_14 $main_lam_15 $main_lam_16 $main_lam_17 $__mon_bind $__mon_bind_lam_0 $__mon_bind_lam_1 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1 $__mon_prompt_lam_2 $__mon_prompt_lam_3)
           )
