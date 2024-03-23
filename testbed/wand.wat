(module $multi
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_5_1 (;4;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@c6    ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@c7    ;)    (local i32)
(;@c9    ;)    global.get 1
(;@cb    ;)    global.get 1
(;@cd    ;)    i32.const 1
(;@cf    ;)    i32.add
(;@d0    ;)    global.set 1
(;@d2    ;)    return
             )
(;@d5    ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@d6    ;)    (local i32)
(;@d8    ;)    global.get 0
(;@da    ;)    global.get 0
(;@dc    ;)    local.get 0
(;@de    ;)    i32.add
(;@df    ;)    global.set 0
(;@e1    ;)    return
             )
(;@e4    ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@e5    ;)    (local i32)
(;@e7    ;)    i32.const 4
(;@e9    ;)    call $alloc
(;@eb    ;)    local.tee 2
(;@ed    ;)    i32.const 1
(;@ef    ;)    i32.const 0
(;@f1    ;)    local.get 0
(;@f3    ;)    local.get 1
(;@f5    ;)    i32.eq
(;@f6    ;)    select
(;@f7    ;)    i32.store
(;@fa    ;)    local.get 2
(;@fc    ;)    return
             )
(;@ff    ;)  (func $__call_1 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@100   ;)    (local i32)
(;@102   ;)    local.get 0
(;@104   ;)    i32.const 12
(;@106   ;)    i32.add
(;@107   ;)    local.set 2
(;@109   ;)    block ;; label = @1
(;@10b   ;)      block ;; label = @2
(;@10d   ;)        block ;; label = @3
(;@10f   ;)          local.get 0
(;@111   ;)          i32.load
(;@114   ;)          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
(;@119   ;)        end
(;@11a   ;)        local.get 2
(;@11c   ;)        local.get 0
(;@11e   ;)        i32.load offset=8
(;@121   ;)        call_indirect (type $fun_1_1)
(;@124   ;)        local.get 1
(;@126   ;)        call $__call_1
(;@128   ;)        return
(;@129   ;)      end
(;@12a   ;)      local.get 2
(;@12c   ;)      local.get 1
(;@12e   ;)      local.get 0
(;@130   ;)      i32.load offset=8
(;@133   ;)      call_indirect (type $fun_2_1)
(;@136   ;)      return
(;@137   ;)    end
(;@138   ;)    unreachable
             )
(;@13b   ;)  (func $__call_2 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@13c   ;)    (local i32)
(;@13e   ;)    local.get 0
(;@140   ;)    i32.const 12
(;@142   ;)    i32.add
(;@143   ;)    local.set 3
(;@145   ;)    block ;; label = @1
(;@147   ;)      block ;; label = @2
(;@149   ;)        block ;; label = @3
(;@14b   ;)          block ;; label = @4
(;@14d   ;)            local.get 0
(;@14f   ;)            i32.load
(;@152   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 3 (;@1;)
(;@158   ;)          end
(;@159   ;)          local.get 3
(;@15b   ;)          local.get 0
(;@15d   ;)          i32.load offset=8
(;@160   ;)          call_indirect (type $fun_1_1)
(;@163   ;)          local.get 1
(;@165   ;)          local.get 2
(;@167   ;)          call $__call_2
(;@169   ;)          return
(;@16a   ;)        end
(;@16b   ;)        local.get 3
(;@16d   ;)        local.get 1
(;@16f   ;)        local.get 0
(;@171   ;)        i32.load offset=8
(;@174   ;)        call_indirect (type $fun_2_1)
(;@177   ;)        local.get 2
(;@179   ;)        call $__call_1
(;@17b   ;)        return
(;@17c   ;)      end
(;@17d   ;)      local.get 3
(;@17f   ;)      local.get 1
(;@181   ;)      local.get 2
(;@183   ;)      local.get 0
(;@185   ;)      i32.load offset=8
(;@188   ;)      call_indirect (type $fun_3_1)
(;@18b   ;)      return
(;@18c   ;)    end
(;@18d   ;)    unreachable
             )
(;@191   ;)  (func $main (;5;) (type $fun_0_1) (result i32)
(;@192   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@194   ;)    i32.const 0
(;@196   ;)    call $alloc
(;@198   ;)    local.set 0
(;@19a   ;)    local.get 0
(;@19c   ;)    local.set 0
(;@19e   ;)    local.get 0
(;@1a0   ;)    call $__mon_generate_marker
(;@1a2   ;)    local.set 1
(;@1a4   ;)    i32.const 16
(;@1a6   ;)    call $alloc
(;@1a8   ;)    local.set 2
(;@1aa   ;)    local.get 2
(;@1ac   ;)    i32.const 1
(;@1ae   ;)    i32.store
(;@1b1   ;)    local.get 2
(;@1b3   ;)    i32.const 1
(;@1b5   ;)    i32.store offset=4
(;@1b8   ;)    local.get 2
(;@1ba   ;)    i32.const 37
(;@1bc   ;)    i32.store offset=8
(;@1bf   ;)    local.get 2
(;@1c1   ;)    local.get 1
(;@1c3   ;)    i32.store offset=12
(;@1c6   ;)    local.get 2
(;@1c8   ;)    local.set 2
(;@1ca   ;)    i32.const 12
(;@1cc   ;)    call $alloc
(;@1ce   ;)    local.set 3
(;@1d0   ;)    local.get 3
(;@1d2   ;)    i32.const 2
(;@1d4   ;)    i32.store
(;@1d7   ;)    local.get 3
(;@1d9   ;)    i32.const 0
(;@1db   ;)    i32.store offset=4
(;@1de   ;)    local.get 3
(;@1e0   ;)    i32.const 38
(;@1e2   ;)    i32.store offset=8
(;@1e5   ;)    local.get 3
(;@1e7   ;)    local.set 3
(;@1e9   ;)    i32.const 0
(;@1eb   ;)    call $alloc
(;@1ed   ;)    local.set 4
(;@1ef   ;)    local.get 4
(;@1f1   ;)    local.set 4
(;@1f3   ;)    local.get 2
(;@1f5   ;)    local.get 3
(;@1f7   ;)    local.get 4
(;@1f9   ;)    call $__mon_bind
(;@1fb   ;)    local.set 5
(;@1fd   ;)    local.get 5
(;@1ff   ;)    i32.load
(;@202   ;)    local.set 6
(;@204   ;)    block (result i32) ;; label = @1
(;@206   ;)      block ;; label = @2
(;@208   ;)        block ;; label = @3
(;@20a   ;)          block ;; label = @4
(;@20c   ;)            local.get 6
(;@20e   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@213   ;)          end
(;@214   ;)          local.get 5
(;@216   ;)          i32.load offset=4
(;@219   ;)          local.set 7
(;@21b   ;)          local.get 7
(;@21d   ;)          br 2 (;@1;)
(;@21f   ;)        end
(;@220   ;)        local.get 5
(;@222   ;)        i32.load offset=4
(;@225   ;)        local.set 7
(;@227   ;)        i32.const 5467
(;@22a   ;)        br 1 (;@1;)
(;@22c   ;)      end
(;@22d   ;)      unreachable
(;@22e   ;)    end
(;@22f   ;)    return
             )
(;@232   ;)  (func $main_lam_0 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@233   ;)    (local i32 i32 i32)
(;@235   ;)    local.get 0
(;@237   ;)    i32.load
(;@23a   ;)    local.set 2
(;@23c   ;)    local.get 0
(;@23e   ;)    i32.load offset=4
(;@241   ;)    local.set 3
(;@243   ;)    local.get 2
(;@245   ;)    local.get 3
(;@247   ;)    local.get 1
(;@249   ;)    call $__call_2
(;@24b   ;)    return
             )
(;@24e   ;)  (func $main_lam_1 (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@24f   ;)    (local i32 i32)
(;@251   ;)    local.get 0
(;@253   ;)    i32.load
(;@256   ;)    local.set 3
(;@258   ;)    local.get 1
(;@25a   ;)    local.get 3
(;@25c   ;)    local.get 2
(;@25e   ;)    call $__call_2
(;@260   ;)    return
             )
(;@263   ;)  (func $main_lam_2 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@264   ;)    (local i32 i32 i32 i32)
(;@266   ;)    local.get 0
(;@268   ;)    i32.load
(;@26b   ;)    local.set 3
(;@26d   ;)    i32.const 20
(;@26f   ;)    call $alloc
(;@271   ;)    local.set 4
(;@273   ;)    local.get 4
(;@275   ;)    i32.const 1
(;@277   ;)    i32.store
(;@27a   ;)    local.get 4
(;@27c   ;)    i32.const 2
(;@27e   ;)    i32.store offset=4
(;@281   ;)    local.get 4
(;@283   ;)    i32.const 6
(;@285   ;)    i32.store offset=8
(;@288   ;)    local.get 4
(;@28a   ;)    local.get 3
(;@28c   ;)    i32.store offset=12
(;@28f   ;)    local.get 4
(;@291   ;)    local.get 1
(;@293   ;)    i32.store offset=16
(;@296   ;)    local.get 4
(;@298   ;)    local.set 4
(;@29a   ;)    i32.const 16
(;@29c   ;)    call $alloc
(;@29e   ;)    local.set 5
(;@2a0   ;)    local.get 5
(;@2a2   ;)    i32.const 2
(;@2a4   ;)    i32.store
(;@2a7   ;)    local.get 5
(;@2a9   ;)    i32.const 1
(;@2ab   ;)    i32.store offset=4
(;@2ae   ;)    local.get 5
(;@2b0   ;)    i32.const 7
(;@2b2   ;)    i32.store offset=8
(;@2b5   ;)    local.get 5
(;@2b7   ;)    local.get 1
(;@2b9   ;)    i32.store offset=12
(;@2bc   ;)    local.get 5
(;@2be   ;)    local.set 5
(;@2c0   ;)    local.get 4
(;@2c2   ;)    local.get 5
(;@2c4   ;)    local.get 2
(;@2c6   ;)    call $__mon_bind
(;@2c8   ;)    return
             )
(;@2cb   ;)  (func $main_lam_3 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@2cc   ;)    (local i32 i32)
(;@2ce   ;)    i32.const 16
(;@2d0   ;)    call $alloc
(;@2d2   ;)    local.set 3
(;@2d4   ;)    local.get 3
(;@2d6   ;)    i32.const 2
(;@2d8   ;)    i32.store
(;@2db   ;)    local.get 3
(;@2dd   ;)    i32.const 1
(;@2df   ;)    i32.store offset=4
(;@2e2   ;)    local.get 3
(;@2e4   ;)    i32.const 8
(;@2e6   ;)    i32.store offset=8
(;@2e9   ;)    local.get 3
(;@2eb   ;)    local.get 1
(;@2ed   ;)    i32.store offset=12
(;@2f0   ;)    local.get 3
(;@2f2   ;)    local.set 3
(;@2f4   ;)    i32.const 8
(;@2f6   ;)    call $alloc
(;@2f8   ;)    local.set 4
(;@2fa   ;)    local.get 4
(;@2fc   ;)    i32.const 0
(;@2fe   ;)    i32.store
(;@301   ;)    local.get 4
(;@303   ;)    local.get 3
(;@305   ;)    i32.store offset=4
(;@308   ;)    local.get 4
(;@30a   ;)    return
             )
(;@30d   ;)  (func $main_lam_4 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@30e   ;)    (local i32 i32)
(;@310   ;)    i32.const 12
(;@312   ;)    call $alloc
(;@314   ;)    local.set 3
(;@316   ;)    local.get 3
(;@318   ;)    i32.const 2
(;@31a   ;)    i32.store
(;@31d   ;)    local.get 3
(;@31f   ;)    i32.const 0
(;@321   ;)    i32.store offset=4
(;@324   ;)    local.get 3
(;@326   ;)    i32.const 9
(;@328   ;)    i32.store offset=8
(;@32b   ;)    local.get 3
(;@32d   ;)    local.set 3
(;@32f   ;)    i32.const 8
(;@331   ;)    call $alloc
(;@333   ;)    local.set 4
(;@335   ;)    local.get 4
(;@337   ;)    i32.const 0
(;@339   ;)    i32.store
(;@33c   ;)    local.get 4
(;@33e   ;)    local.get 3
(;@340   ;)    i32.store offset=4
(;@343   ;)    local.get 4
(;@345   ;)    return
             )
(;@348   ;)  (func $main_lam_5 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@349   ;)    (local i32 i32 i32)
(;@34b   ;)    local.get 0
(;@34d   ;)    i32.load
(;@350   ;)    local.set 2
(;@352   ;)    i32.const 0
(;@354   ;)    call $alloc
(;@356   ;)    local.set 3
(;@358   ;)    local.get 3
(;@35a   ;)    local.set 3
(;@35c   ;)    local.get 2
(;@35e   ;)    local.get 3
(;@360   ;)    local.get 1
(;@362   ;)    call $__call_2
(;@364   ;)    return
             )
(;@367   ;)  (func $main_lam_6 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@368   ;)    (local i32 i32)
(;@36a   ;)    local.get 0
(;@36c   ;)    i32.load
(;@36f   ;)    local.set 3
(;@371   ;)    local.get 1
(;@373   ;)    local.get 3
(;@375   ;)    local.get 2
(;@377   ;)    call $__call_2
(;@379   ;)    return
             )
(;@37c   ;)  (func $main_lam_7 (;13;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@37d   ;)    (local i32 i32 i32 i32 i32)
(;@37f   ;)    local.get 0
(;@381   ;)    i32.load
(;@384   ;)    local.set 3
(;@386   ;)    local.get 0
(;@388   ;)    i32.load offset=4
(;@38b   ;)    local.set 4
(;@38d   ;)    i32.const 16
(;@38f   ;)    call $alloc
(;@391   ;)    local.set 5
(;@393   ;)    local.get 5
(;@395   ;)    i32.const 1
(;@397   ;)    i32.store
(;@39a   ;)    local.get 5
(;@39c   ;)    i32.const 1
(;@39e   ;)    i32.store offset=4
(;@3a1   ;)    local.get 5
(;@3a3   ;)    i32.const 11
(;@3a5   ;)    i32.store offset=8
(;@3a8   ;)    local.get 5
(;@3aa   ;)    local.get 4
(;@3ac   ;)    i32.store offset=12
(;@3af   ;)    local.get 5
(;@3b1   ;)    local.set 5
(;@3b3   ;)    i32.const 16
(;@3b5   ;)    call $alloc
(;@3b7   ;)    local.set 6
(;@3b9   ;)    local.get 6
(;@3bb   ;)    i32.const 2
(;@3bd   ;)    i32.store
(;@3c0   ;)    local.get 6
(;@3c2   ;)    i32.const 1
(;@3c4   ;)    i32.store offset=4
(;@3c7   ;)    local.get 6
(;@3c9   ;)    i32.const 12
(;@3cb   ;)    i32.store offset=8
(;@3ce   ;)    local.get 6
(;@3d0   ;)    local.get 3
(;@3d2   ;)    i32.store offset=12
(;@3d5   ;)    local.get 6
(;@3d7   ;)    local.set 6
(;@3d9   ;)    local.get 5
(;@3db   ;)    local.get 6
(;@3dd   ;)    local.get 2
(;@3df   ;)    call $__mon_bind
(;@3e1   ;)    return
             )
(;@3e4   ;)  (func $main_lam_8 (;14;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@3e5   ;)    (local i32 i32 i32)
(;@3e7   ;)    local.get 0
(;@3e9   ;)    i32.load
(;@3ec   ;)    local.set 3
(;@3ee   ;)    i32.const 20
(;@3f0   ;)    call $alloc
(;@3f2   ;)    local.set 4
(;@3f4   ;)    local.get 4
(;@3f6   ;)    i32.const 2
(;@3f8   ;)    i32.store
(;@3fb   ;)    local.get 4
(;@3fd   ;)    i32.const 2
(;@3ff   ;)    i32.store offset=4
(;@402   ;)    local.get 4
(;@404   ;)    i32.const 13
(;@406   ;)    i32.store offset=8
(;@409   ;)    local.get 4
(;@40b   ;)    local.get 3
(;@40d   ;)    i32.store offset=12
(;@410   ;)    local.get 4
(;@412   ;)    local.get 1
(;@414   ;)    i32.store offset=16
(;@417   ;)    local.get 4
(;@419   ;)    local.set 4
(;@41b   ;)    i32.const 8
(;@41d   ;)    call $alloc
(;@41f   ;)    local.set 5
(;@421   ;)    local.get 5
(;@423   ;)    i32.const 0
(;@425   ;)    i32.store
(;@428   ;)    local.get 5
(;@42a   ;)    local.get 4
(;@42c   ;)    i32.store offset=4
(;@42f   ;)    local.get 5
(;@431   ;)    return
             )
(;@434   ;)  (func $main_lam_9 (;15;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@435   ;)    (local i32 i32)
(;@437   ;)    i32.const 16
(;@439   ;)    call $alloc
(;@43b   ;)    local.set 3
(;@43d   ;)    local.get 3
(;@43f   ;)    i32.const 2
(;@441   ;)    i32.store
(;@444   ;)    local.get 3
(;@446   ;)    i32.const 1
(;@448   ;)    i32.store offset=4
(;@44b   ;)    local.get 3
(;@44d   ;)    i32.const 14
(;@44f   ;)    i32.store offset=8
(;@452   ;)    local.get 3
(;@454   ;)    local.get 1
(;@456   ;)    i32.store offset=12
(;@459   ;)    local.get 3
(;@45b   ;)    local.set 3
(;@45d   ;)    i32.const 8
(;@45f   ;)    call $alloc
(;@461   ;)    local.set 4
(;@463   ;)    local.get 4
(;@465   ;)    i32.const 0
(;@467   ;)    i32.store
(;@46a   ;)    local.get 4
(;@46c   ;)    local.get 3
(;@46e   ;)    i32.store offset=4
(;@471   ;)    local.get 4
(;@473   ;)    return
             )
(;@476   ;)  (func $main_lam_10 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@477   ;)    (local i32 i32 i32 i32 i32)
(;@479   ;)    local.get 0
(;@47b   ;)    i32.load
(;@47e   ;)    local.set 2
(;@480   ;)    i32.const 12
(;@482   ;)    call $alloc
(;@484   ;)    local.set 3
(;@486   ;)    local.get 3
(;@488   ;)    i32.const 2
(;@48a   ;)    i32.store
(;@48d   ;)    local.get 3
(;@48f   ;)    i32.const 0
(;@491   ;)    i32.store offset=4
(;@494   ;)    local.get 3
(;@496   ;)    i32.const 10
(;@498   ;)    i32.store offset=8
(;@49b   ;)    local.get 3
(;@49d   ;)    local.set 3
(;@49f   ;)    i32.const 12
(;@4a1   ;)    call $alloc
(;@4a3   ;)    local.set 4
(;@4a5   ;)    local.get 4
(;@4a7   ;)    i32.const 2
(;@4a9   ;)    i32.store
(;@4ac   ;)    local.get 4
(;@4ae   ;)    i32.const 0
(;@4b0   ;)    i32.store offset=4
(;@4b3   ;)    local.get 4
(;@4b5   ;)    i32.const 15
(;@4b7   ;)    i32.store offset=8
(;@4ba   ;)    local.get 4
(;@4bc   ;)    local.set 4
(;@4be   ;)    i32.const 8
(;@4c0   ;)    call $alloc
(;@4c2   ;)    local.set 5
(;@4c4   ;)    local.get 5
(;@4c6   ;)    local.get 3
(;@4c8   ;)    i32.store
(;@4cb   ;)    local.get 5
(;@4cd   ;)    local.get 4
(;@4cf   ;)    i32.store offset=4
(;@4d2   ;)    local.get 5
(;@4d4   ;)    local.set 5
(;@4d6   ;)    i32.const 8
(;@4d8   ;)    call $alloc
(;@4da   ;)    local.set 6
(;@4dc   ;)    local.get 6
(;@4de   ;)    local.get 2
(;@4e0   ;)    i32.store
(;@4e3   ;)    local.get 6
(;@4e5   ;)    local.get 5
(;@4e7   ;)    i32.store offset=4
(;@4ea   ;)    local.get 6
(;@4ec   ;)    return
             )
(;@4ef   ;)  (func $main_lam_11 (;17;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@4f0   ;)    (local i32 i32 i32)
(;@4f2   ;)    local.get 0
(;@4f4   ;)    i32.load
(;@4f7   ;)    local.set 3
(;@4f9   ;)    i32.const 8
(;@4fb   ;)    call $alloc
(;@4fd   ;)    local.set 4
(;@4ff   ;)    local.get 4
(;@501   ;)    local.get 1
(;@503   ;)    i32.store
(;@506   ;)    local.get 4
(;@508   ;)    local.get 3
(;@50a   ;)    i32.store offset=4
(;@50d   ;)    local.get 4
(;@50f   ;)    local.set 4
(;@511   ;)    i32.const 8
(;@513   ;)    call $alloc
(;@515   ;)    local.set 5
(;@517   ;)    local.get 5
(;@519   ;)    i32.const 0
(;@51b   ;)    i32.store
(;@51e   ;)    local.get 5
(;@520   ;)    local.get 4
(;@522   ;)    i32.store offset=4
(;@525   ;)    local.get 5
(;@527   ;)    return
             )
(;@52a   ;)  (func $main_lam_12 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@52b   ;)    (local i32 i32)
(;@52d   ;)    i32.const 16
(;@52f   ;)    call $alloc
(;@531   ;)    local.set 3
(;@533   ;)    local.get 3
(;@535   ;)    i32.const 2
(;@537   ;)    i32.store
(;@53a   ;)    local.get 3
(;@53c   ;)    i32.const 1
(;@53e   ;)    i32.store offset=4
(;@541   ;)    local.get 3
(;@543   ;)    i32.const 17
(;@545   ;)    i32.store offset=8
(;@548   ;)    local.get 3
(;@54a   ;)    local.get 1
(;@54c   ;)    i32.store offset=12
(;@54f   ;)    local.get 3
(;@551   ;)    local.set 3
(;@553   ;)    i32.const 8
(;@555   ;)    call $alloc
(;@557   ;)    local.set 4
(;@559   ;)    local.get 4
(;@55b   ;)    i32.const 0
(;@55d   ;)    i32.store
(;@560   ;)    local.get 4
(;@562   ;)    local.get 3
(;@564   ;)    i32.store offset=4
(;@567   ;)    local.get 4
(;@569   ;)    return
             )
(;@56c   ;)  (func $main_lam_13 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@56d   ;)    (local i32)
(;@56f   ;)    local.get 1
(;@571   ;)    i32.const 78543
(;@575   ;)    local.get 2
(;@577   ;)    call $__call_2
(;@579   ;)    return
             )
(;@57c   ;)  (func $main_lam_14 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@57d   ;)    (local i32 i32)
(;@57f   ;)    i32.const 12
(;@581   ;)    call $alloc
(;@583   ;)    local.set 3
(;@585   ;)    local.get 3
(;@587   ;)    i32.const 2
(;@589   ;)    i32.store
(;@58c   ;)    local.get 3
(;@58e   ;)    i32.const 0
(;@590   ;)    i32.store offset=4
(;@593   ;)    local.get 3
(;@595   ;)    i32.const 19
(;@597   ;)    i32.store offset=8
(;@59a   ;)    local.get 3
(;@59c   ;)    local.set 3
(;@59e   ;)    i32.const 8
(;@5a0   ;)    call $alloc
(;@5a2   ;)    local.set 4
(;@5a4   ;)    local.get 4
(;@5a6   ;)    i32.const 0
(;@5a8   ;)    i32.store
(;@5ab   ;)    local.get 4
(;@5ad   ;)    local.get 3
(;@5af   ;)    i32.store offset=4
(;@5b2   ;)    local.get 4
(;@5b4   ;)    return
             )
(;@5b7   ;)  (func $main_lam_15 (;21;) (type $fun_2_1) (param i32 i32) (result i32)
(;@5b8   ;)    (local i32 i32 i32 i32)
(;@5ba   ;)    local.get 0
(;@5bc   ;)    i32.load
(;@5bf   ;)    local.set 2
(;@5c1   ;)    i32.const 12
(;@5c3   ;)    call $alloc
(;@5c5   ;)    local.set 3
(;@5c7   ;)    local.get 3
(;@5c9   ;)    i32.const 2
(;@5cb   ;)    i32.store
(;@5ce   ;)    local.get 3
(;@5d0   ;)    i32.const 0
(;@5d2   ;)    i32.store offset=4
(;@5d5   ;)    local.get 3
(;@5d7   ;)    i32.const 20
(;@5d9   ;)    i32.store offset=8
(;@5dc   ;)    local.get 3
(;@5de   ;)    local.set 3
(;@5e0   ;)    i32.const 8
(;@5e2   ;)    call $alloc
(;@5e4   ;)    local.set 4
(;@5e6   ;)    local.get 4
(;@5e8   ;)    local.get 2
(;@5ea   ;)    i32.store
(;@5ed   ;)    local.get 4
(;@5ef   ;)    local.get 3
(;@5f1   ;)    i32.store offset=4
(;@5f4   ;)    local.get 4
(;@5f6   ;)    local.set 4
(;@5f8   ;)    i32.const 8
(;@5fa   ;)    call $alloc
(;@5fc   ;)    local.set 5
(;@5fe   ;)    local.get 5
(;@600   ;)    local.get 1
(;@602   ;)    i32.store
(;@605   ;)    local.get 5
(;@607   ;)    local.get 4
(;@609   ;)    i32.store offset=4
(;@60c   ;)    local.get 5
(;@60e   ;)    return
             )
(;@611   ;)  (func $main_lam_16 (;22;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@612   ;)    (local i32)
(;@614   ;)    i32.const 8
(;@616   ;)    call $alloc
(;@618   ;)    local.set 3
(;@61a   ;)    local.get 3
(;@61c   ;)    i32.const 0
(;@61e   ;)    i32.store
(;@621   ;)    local.get 3
(;@623   ;)    local.get 1
(;@625   ;)    i32.store offset=4
(;@628   ;)    local.get 3
(;@62a   ;)    return
             )
(;@62d   ;)  (func $main_lam_17 (;23;) (type $fun_2_1) (param i32 i32) (result i32)
(;@62e   ;)    (local i32 i32 i32 i32 i32)
(;@630   ;)    local.get 0
(;@632   ;)    i32.load
(;@635   ;)    local.set 2
(;@637   ;)    i32.const 0
(;@639   ;)    call $alloc
(;@63b   ;)    local.set 3
(;@63d   ;)    local.get 3
(;@63f   ;)    local.set 3
(;@641   ;)    local.get 2
(;@643   ;)    i32.load offset=4
(;@646   ;)    local.set 4
(;@648   ;)    local.get 4
(;@64a   ;)    i32.load offset=4
(;@64d   ;)    local.set 5
(;@64f   ;)    local.get 5
(;@651   ;)    local.get 3
(;@653   ;)    local.get 1
(;@655   ;)    call $__call_2
(;@657   ;)    return
             )
(;@65a   ;)  (func $main_lam_18 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@65b   ;)    (local i32 i32)
(;@65d   ;)    local.get 0
(;@65f   ;)    i32.load
(;@662   ;)    local.set 3
(;@664   ;)    local.get 1
(;@666   ;)    local.get 3
(;@668   ;)    local.get 2
(;@66a   ;)    call $__call_2
(;@66c   ;)    return
             )
(;@66f   ;)  (func $main_lam_19 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@670   ;)    (local i32 i32 i32 i32)
(;@672   ;)    local.get 0
(;@674   ;)    i32.load
(;@677   ;)    local.set 3
(;@679   ;)    i32.const 16
(;@67b   ;)    call $alloc
(;@67d   ;)    local.set 4
(;@67f   ;)    local.get 4
(;@681   ;)    i32.const 1
(;@683   ;)    i32.store
(;@686   ;)    local.get 4
(;@688   ;)    i32.const 1
(;@68a   ;)    i32.store offset=4
(;@68d   ;)    local.get 4
(;@68f   ;)    i32.const 23
(;@691   ;)    i32.store offset=8
(;@694   ;)    local.get 4
(;@696   ;)    local.get 3
(;@698   ;)    i32.store offset=12
(;@69b   ;)    local.get 4
(;@69d   ;)    local.set 4
(;@69f   ;)    i32.const 16
(;@6a1   ;)    call $alloc
(;@6a3   ;)    local.set 5
(;@6a5   ;)    local.get 5
(;@6a7   ;)    i32.const 2
(;@6a9   ;)    i32.store
(;@6ac   ;)    local.get 5
(;@6ae   ;)    i32.const 1
(;@6b0   ;)    i32.store offset=4
(;@6b3   ;)    local.get 5
(;@6b5   ;)    i32.const 24
(;@6b7   ;)    i32.store offset=8
(;@6ba   ;)    local.get 5
(;@6bc   ;)    local.get 1
(;@6be   ;)    i32.store offset=12
(;@6c1   ;)    local.get 5
(;@6c3   ;)    local.set 5
(;@6c5   ;)    local.get 4
(;@6c7   ;)    local.get 5
(;@6c9   ;)    local.get 2
(;@6cb   ;)    call $__mon_bind
(;@6cd   ;)    return
             )
(;@6d0   ;)  (func $main_lam_20 (;26;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@6d1   ;)    (local i32)
(;@6d3   ;)    i32.const 8
(;@6d5   ;)    call $alloc
(;@6d7   ;)    local.set 3
(;@6d9   ;)    local.get 3
(;@6db   ;)    i32.const 0
(;@6dd   ;)    i32.store
(;@6e0   ;)    local.get 3
(;@6e2   ;)    local.get 1
(;@6e4   ;)    i32.store offset=4
(;@6e7   ;)    local.get 3
(;@6e9   ;)    return
             )
(;@6ed   ;)  (func $main_lam_21 (;27;) (type $fun_2_1) (param i32 i32) (result i32)
(;@6ee   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@6f0   ;)    local.get 0
(;@6f2   ;)    i32.load
(;@6f5   ;)    local.set 2
(;@6f7   ;)    local.get 2
(;@6f9   ;)    i32.load offset=4
(;@6fc   ;)    local.set 3
(;@6fe   ;)    local.get 3
(;@700   ;)    i32.load
(;@703   ;)    local.set 4
(;@705   ;)    i32.const 16
(;@707   ;)    call $alloc
(;@709   ;)    local.set 5
(;@70b   ;)    local.get 5
(;@70d   ;)    i32.const 2
(;@70f   ;)    i32.store
(;@712   ;)    local.get 5
(;@714   ;)    i32.const 1
(;@716   ;)    i32.store offset=4
(;@719   ;)    local.get 5
(;@71b   ;)    i32.const 25
(;@71d   ;)    i32.store offset=8
(;@720   ;)    local.get 5
(;@722   ;)    local.get 2
(;@724   ;)    i32.store offset=12
(;@727   ;)    local.get 5
(;@729   ;)    local.set 5
(;@72b   ;)    i32.const 12
(;@72d   ;)    call $alloc
(;@72f   ;)    local.set 6
(;@731   ;)    local.get 6
(;@733   ;)    i32.const 2
(;@735   ;)    i32.store
(;@738   ;)    local.get 6
(;@73a   ;)    i32.const 0
(;@73c   ;)    i32.store offset=4
(;@73f   ;)    local.get 6
(;@741   ;)    i32.const 26
(;@743   ;)    i32.store offset=8
(;@746   ;)    local.get 6
(;@748   ;)    local.set 6
(;@74a   ;)    i32.const 12
(;@74c   ;)    call $alloc
(;@74e   ;)    local.set 7
(;@750   ;)    local.get 7
(;@752   ;)    local.get 4
(;@754   ;)    i32.store
(;@757   ;)    local.get 7
(;@759   ;)    local.get 5
(;@75b   ;)    i32.store offset=4
(;@75e   ;)    local.get 7
(;@760   ;)    local.get 6
(;@762   ;)    i32.store offset=8
(;@765   ;)    local.get 7
(;@767   ;)    local.set 7
(;@769   ;)    i32.const 8
(;@76b   ;)    call $alloc
(;@76d   ;)    local.set 8
(;@76f   ;)    local.get 8
(;@771   ;)    i32.const 1
(;@773   ;)    i32.store
(;@776   ;)    local.get 8
(;@778   ;)    local.get 7
(;@77a   ;)    i32.store offset=4
(;@77d   ;)    local.get 8
(;@77f   ;)    return
             )
(;@782   ;)  (func $main_lam_22 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
(;@783   ;)    (local i32 i32 i32 i32 i32 i32)
(;@785   ;)    local.get 0
(;@787   ;)    i32.load
(;@78a   ;)    local.set 2
(;@78c   ;)    local.get 0
(;@78e   ;)    i32.load offset=4
(;@791   ;)    local.set 3
(;@793   ;)    local.get 2
(;@795   ;)    i32.load
(;@798   ;)    local.set 4
(;@79a   ;)    local.get 4
(;@79c   ;)    i32.load offset=4
(;@79f   ;)    local.set 5
(;@7a1   ;)    local.get 5
(;@7a3   ;)    i32.load offset=4
(;@7a6   ;)    local.set 6
(;@7a8   ;)    local.get 6
(;@7aa   ;)    local.get 3
(;@7ac   ;)    local.get 1
(;@7ae   ;)    call $__call_2
(;@7b0   ;)    return
             )
(;@7b3   ;)  (func $main_lam_23 (;29;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@7b4   ;)    (local i32 i32)
(;@7b6   ;)    local.get 0
(;@7b8   ;)    i32.load
(;@7bb   ;)    local.set 3
(;@7bd   ;)    local.get 1
(;@7bf   ;)    local.get 3
(;@7c1   ;)    local.get 2
(;@7c3   ;)    call $__call_2
(;@7c5   ;)    return
             )
(;@7c8   ;)  (func $main_lam_24 (;30;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@7c9   ;)    (local i32 i32 i32 i32 i32)
(;@7cb   ;)    local.get 0
(;@7cd   ;)    i32.load
(;@7d0   ;)    local.set 3
(;@7d2   ;)    local.get 0
(;@7d4   ;)    i32.load offset=4
(;@7d7   ;)    local.set 4
(;@7d9   ;)    i32.const 20
(;@7db   ;)    call $alloc
(;@7dd   ;)    local.set 5
(;@7df   ;)    local.get 5
(;@7e1   ;)    i32.const 1
(;@7e3   ;)    i32.store
(;@7e6   ;)    local.get 5
(;@7e8   ;)    i32.const 2
(;@7ea   ;)    i32.store offset=4
(;@7ed   ;)    local.get 5
(;@7ef   ;)    i32.const 28
(;@7f1   ;)    i32.store offset=8
(;@7f4   ;)    local.get 5
(;@7f6   ;)    local.get 3
(;@7f8   ;)    i32.store offset=12
(;@7fb   ;)    local.get 5
(;@7fd   ;)    local.get 4
(;@7ff   ;)    i32.store offset=16
(;@802   ;)    local.get 5
(;@804   ;)    local.set 5
(;@806   ;)    i32.const 16
(;@808   ;)    call $alloc
(;@80a   ;)    local.set 6
(;@80c   ;)    local.get 6
(;@80e   ;)    i32.const 2
(;@810   ;)    i32.store
(;@813   ;)    local.get 6
(;@815   ;)    i32.const 1
(;@817   ;)    i32.store offset=4
(;@81a   ;)    local.get 6
(;@81c   ;)    i32.const 29
(;@81e   ;)    i32.store offset=8
(;@821   ;)    local.get 6
(;@823   ;)    local.get 1
(;@825   ;)    i32.store offset=12
(;@828   ;)    local.get 6
(;@82a   ;)    local.set 6
(;@82c   ;)    local.get 5
(;@82e   ;)    local.get 6
(;@830   ;)    local.get 2
(;@832   ;)    call $__mon_bind
(;@834   ;)    return
             )
(;@837   ;)  (func $main_lam_25 (;31;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@838   ;)    (local i32)
(;@83a   ;)    i32.const 8
(;@83c   ;)    call $alloc
(;@83e   ;)    local.set 3
(;@840   ;)    local.get 3
(;@842   ;)    i32.const 0
(;@844   ;)    i32.store
(;@847   ;)    local.get 3
(;@849   ;)    local.get 1
(;@84b   ;)    i32.store offset=4
(;@84e   ;)    local.get 3
(;@850   ;)    return
             )
(;@854   ;)  (func $main_lam_26 (;32;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@855   ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@857   ;)    local.get 0
(;@859   ;)    i32.load
(;@85c   ;)    local.set 3
(;@85e   ;)    local.get 3
(;@860   ;)    i32.load
(;@863   ;)    local.set 4
(;@865   ;)    local.get 4
(;@867   ;)    i32.load
(;@86a   ;)    local.set 5
(;@86c   ;)    i32.const 20
(;@86e   ;)    call $alloc
(;@870   ;)    local.set 6
(;@872   ;)    local.get 6
(;@874   ;)    i32.const 2
(;@876   ;)    i32.store
(;@879   ;)    local.get 6
(;@87b   ;)    i32.const 2
(;@87d   ;)    i32.store offset=4
(;@880   ;)    local.get 6
(;@882   ;)    i32.const 30
(;@884   ;)    i32.store offset=8
(;@887   ;)    local.get 6
(;@889   ;)    local.get 3
(;@88b   ;)    i32.store offset=12
(;@88e   ;)    local.get 6
(;@890   ;)    local.get 1
(;@892   ;)    i32.store offset=16
(;@895   ;)    local.get 6
(;@897   ;)    local.set 6
(;@899   ;)    i32.const 12
(;@89b   ;)    call $alloc
(;@89d   ;)    local.set 7
(;@89f   ;)    local.get 7
(;@8a1   ;)    i32.const 2
(;@8a3   ;)    i32.store
(;@8a6   ;)    local.get 7
(;@8a8   ;)    i32.const 0
(;@8aa   ;)    i32.store offset=4
(;@8ad   ;)    local.get 7
(;@8af   ;)    i32.const 31
(;@8b1   ;)    i32.store offset=8
(;@8b4   ;)    local.get 7
(;@8b6   ;)    local.set 7
(;@8b8   ;)    i32.const 12
(;@8ba   ;)    call $alloc
(;@8bc   ;)    local.set 8
(;@8be   ;)    local.get 8
(;@8c0   ;)    local.get 5
(;@8c2   ;)    i32.store
(;@8c5   ;)    local.get 8
(;@8c7   ;)    local.get 6
(;@8c9   ;)    i32.store offset=4
(;@8cc   ;)    local.get 8
(;@8ce   ;)    local.get 7
(;@8d0   ;)    i32.store offset=8
(;@8d3   ;)    local.get 8
(;@8d5   ;)    local.set 8
(;@8d7   ;)    i32.const 8
(;@8d9   ;)    call $alloc
(;@8db   ;)    local.set 9
(;@8dd   ;)    local.get 9
(;@8df   ;)    i32.const 1
(;@8e1   ;)    i32.store
(;@8e4   ;)    local.get 9
(;@8e6   ;)    local.get 8
(;@8e8   ;)    i32.store offset=4
(;@8eb   ;)    local.get 9
(;@8ed   ;)    return
             )
(;@8f0   ;)  (func $main_lam_27 (;33;) (type $fun_2_1) (param i32 i32) (result i32)
(;@8f1   ;)    (local i32 i32 i32)
(;@8f3   ;)    i32.const 16
(;@8f5   ;)    call $alloc
(;@8f7   ;)    local.set 2
(;@8f9   ;)    local.get 2
(;@8fb   ;)    i32.const 1
(;@8fd   ;)    i32.store
(;@900   ;)    local.get 2
(;@902   ;)    i32.const 1
(;@904   ;)    i32.store offset=4
(;@907   ;)    local.get 2
(;@909   ;)    i32.const 27
(;@90b   ;)    i32.store offset=8
(;@90e   ;)    local.get 2
(;@910   ;)    local.get 1
(;@912   ;)    i32.store offset=12
(;@915   ;)    local.get 2
(;@917   ;)    local.set 2
(;@919   ;)    i32.const 16
(;@91b   ;)    call $alloc
(;@91d   ;)    local.set 3
(;@91f   ;)    local.get 3
(;@921   ;)    i32.const 2
(;@923   ;)    i32.store
(;@926   ;)    local.get 3
(;@928   ;)    i32.const 1
(;@92a   ;)    i32.store offset=4
(;@92d   ;)    local.get 3
(;@92f   ;)    i32.const 32
(;@931   ;)    i32.store offset=8
(;@934   ;)    local.get 3
(;@936   ;)    local.get 1
(;@938   ;)    i32.store offset=12
(;@93b   ;)    local.get 3
(;@93d   ;)    local.set 3
(;@93f   ;)    local.get 2
(;@941   ;)    local.get 3
(;@943   ;)    local.get 1
(;@945   ;)    call $__mon_bind
(;@947   ;)    return
             )
(;@94b   ;)  (func $main_lam_28 (;34;) (type $fun_2_1) (param i32 i32) (result i32)
(;@94c   ;)    (local i32 i32 i32 i32 i32 i32)
(;@94e   ;)    i32.const 0
(;@950   ;)    call $alloc
(;@952   ;)    local.set 2
(;@954   ;)    local.get 2
(;@956   ;)    local.set 2
(;@958   ;)    local.get 2
(;@95a   ;)    call $__mon_generate_marker
(;@95c   ;)    local.set 3
(;@95e   ;)    i32.const 16
(;@960   ;)    call $alloc
(;@962   ;)    local.set 4
(;@964   ;)    local.get 4
(;@966   ;)    i32.const 1
(;@968   ;)    i32.store
(;@96b   ;)    local.get 4
(;@96d   ;)    i32.const 1
(;@96f   ;)    i32.store offset=4
(;@972   ;)    local.get 4
(;@974   ;)    i32.const 21
(;@976   ;)    i32.store offset=8
(;@979   ;)    local.get 4
(;@97b   ;)    local.get 3
(;@97d   ;)    i32.store offset=12
(;@980   ;)    local.get 4
(;@982   ;)    local.set 4
(;@984   ;)    i32.const 12
(;@986   ;)    call $alloc
(;@988   ;)    local.set 5
(;@98a   ;)    local.get 5
(;@98c   ;)    i32.const 2
(;@98e   ;)    i32.store
(;@991   ;)    local.get 5
(;@993   ;)    i32.const 0
(;@995   ;)    i32.store offset=4
(;@998   ;)    local.get 5
(;@99a   ;)    i32.const 22
(;@99c   ;)    i32.store offset=8
(;@99f   ;)    local.get 5
(;@9a1   ;)    local.set 5
(;@9a3   ;)    i32.const 12
(;@9a5   ;)    call $alloc
(;@9a7   ;)    local.set 6
(;@9a9   ;)    local.get 6
(;@9ab   ;)    i32.const 1
(;@9ad   ;)    i32.store
(;@9b0   ;)    local.get 6
(;@9b2   ;)    i32.const 0
(;@9b4   ;)    i32.store offset=4
(;@9b7   ;)    local.get 6
(;@9b9   ;)    i32.const 33
(;@9bb   ;)    i32.store offset=8
(;@9be   ;)    local.get 6
(;@9c0   ;)    local.set 6
(;@9c2   ;)    local.get 3
(;@9c4   ;)    local.get 4
(;@9c6   ;)    local.get 5
(;@9c8   ;)    local.get 6
(;@9ca   ;)    local.get 1
(;@9cc   ;)    call $__mon_prompt
(;@9ce   ;)    return
             )
(;@9d1   ;)  (func $main_lam_29 (;35;) (type $fun_2_1) (param i32 i32) (result i32)
(;@9d2   ;)    (local i32 i32 i32 i32 i32)
(;@9d4   ;)    local.get 0
(;@9d6   ;)    i32.load
(;@9d9   ;)    local.set 2
(;@9db   ;)    i32.const 16
(;@9dd   ;)    call $alloc
(;@9df   ;)    local.set 3
(;@9e1   ;)    local.get 3
(;@9e3   ;)    i32.const 1
(;@9e5   ;)    i32.store
(;@9e8   ;)    local.get 3
(;@9ea   ;)    i32.const 1
(;@9ec   ;)    i32.store offset=4
(;@9ef   ;)    local.get 3
(;@9f1   ;)    i32.const 16
(;@9f3   ;)    i32.store offset=8
(;@9f6   ;)    local.get 3
(;@9f8   ;)    local.get 2
(;@9fa   ;)    i32.store offset=12
(;@9fd   ;)    local.get 3
(;@9ff   ;)    local.set 3
(;@a01   ;)    i32.const 12
(;@a03   ;)    call $alloc
(;@a05   ;)    local.set 4
(;@a07   ;)    local.get 4
(;@a09   ;)    i32.const 2
(;@a0b   ;)    i32.store
(;@a0e   ;)    local.get 4
(;@a10   ;)    i32.const 0
(;@a12   ;)    i32.store offset=4
(;@a15   ;)    local.get 4
(;@a17   ;)    i32.const 18
(;@a19   ;)    i32.store offset=8
(;@a1c   ;)    local.get 4
(;@a1e   ;)    local.set 4
(;@a20   ;)    i32.const 12
(;@a22   ;)    call $alloc
(;@a24   ;)    local.set 5
(;@a26   ;)    local.get 5
(;@a28   ;)    i32.const 1
(;@a2a   ;)    i32.store
(;@a2d   ;)    local.get 5
(;@a2f   ;)    i32.const 0
(;@a31   ;)    i32.store offset=4
(;@a34   ;)    local.get 5
(;@a36   ;)    i32.const 34
(;@a38   ;)    i32.store offset=8
(;@a3b   ;)    local.get 5
(;@a3d   ;)    local.set 5
(;@a3f   ;)    local.get 2
(;@a41   ;)    local.get 3
(;@a43   ;)    local.get 4
(;@a45   ;)    local.get 5
(;@a47   ;)    local.get 1
(;@a49   ;)    call $__mon_prompt
(;@a4b   ;)    return
             )
(;@a4e   ;)  (func $main_lam_30 (;36;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@a4f   ;)    (local i32)
(;@a51   ;)    local.get 1
(;@a53   ;)    i32.const 16763
(;@a57   ;)    local.get 2
(;@a59   ;)    call $__call_2
(;@a5b   ;)    return
             )
(;@a5e   ;)  (func $main_lam_31 (;37;) (type $fun_2_1) (param i32 i32) (result i32)
(;@a5f   ;)    (local i32 i32 i32 i32)
(;@a61   ;)    local.get 0
(;@a63   ;)    i32.load
(;@a66   ;)    local.set 2
(;@a68   ;)    i32.const 16
(;@a6a   ;)    call $alloc
(;@a6c   ;)    local.set 3
(;@a6e   ;)    local.get 3
(;@a70   ;)    i32.const 1
(;@a72   ;)    i32.store
(;@a75   ;)    local.get 3
(;@a77   ;)    i32.const 1
(;@a79   ;)    i32.store offset=4
(;@a7c   ;)    local.get 3
(;@a7e   ;)    i32.const 35
(;@a80   ;)    i32.store offset=8
(;@a83   ;)    local.get 3
(;@a85   ;)    local.get 2
(;@a87   ;)    i32.store offset=12
(;@a8a   ;)    local.get 3
(;@a8c   ;)    local.set 3
(;@a8e   ;)    i32.const 12
(;@a90   ;)    call $alloc
(;@a92   ;)    local.set 4
(;@a94   ;)    local.get 4
(;@a96   ;)    i32.const 2
(;@a98   ;)    i32.store
(;@a9b   ;)    local.get 4
(;@a9d   ;)    i32.const 0
(;@a9f   ;)    i32.store offset=4
(;@aa2   ;)    local.get 4
(;@aa4   ;)    i32.const 36
(;@aa6   ;)    i32.store offset=8
(;@aa9   ;)    local.get 4
(;@aab   ;)    local.set 4
(;@aad   ;)    local.get 3
(;@aaf   ;)    local.get 4
(;@ab1   ;)    local.get 1
(;@ab3   ;)    call $__mon_bind
(;@ab5   ;)    return
             )
(;@ab8   ;)  (func $main_lam_32 (;38;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@ab9   ;)    (local i32 i32)
(;@abb   ;)    local.get 1
(;@abd   ;)    i32.load
(;@ac0   ;)    local.set 3
(;@ac2   ;)    i32.const 8
(;@ac4   ;)    call $alloc
(;@ac6   ;)    local.set 4
(;@ac8   ;)    local.get 4
(;@aca   ;)    i32.const 0
(;@acc   ;)    i32.store
(;@acf   ;)    local.get 4
(;@ad1   ;)    local.get 3
(;@ad3   ;)    i32.store offset=4
(;@ad6   ;)    local.get 4
(;@ad8   ;)    return
             )
(;@adc   ;)  (func $__mon_bind (;39;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@add   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@adf   ;)    local.get 0
(;@ae1   ;)    local.get 2
(;@ae3   ;)    call $__call_1
(;@ae5   ;)    local.set 3
(;@ae7   ;)    local.get 3
(;@ae9   ;)    i32.load
(;@aec   ;)    local.set 4
(;@aee   ;)    block (result i32) ;; label = @1
(;@af0   ;)      block ;; label = @2
(;@af2   ;)        block ;; label = @3
(;@af4   ;)          block ;; label = @4
(;@af6   ;)            local.get 4
(;@af8   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@afd   ;)          end
(;@afe   ;)          local.get 3
(;@b00   ;)          i32.load offset=4
(;@b03   ;)          local.set 5
(;@b05   ;)          local.get 1
(;@b07   ;)          local.get 5
(;@b09   ;)          local.get 2
(;@b0b   ;)          call $__call_2
(;@b0d   ;)          br 2 (;@1;)
(;@b0f   ;)        end
(;@b10   ;)        local.get 3
(;@b12   ;)        i32.load offset=4
(;@b15   ;)        local.set 6
(;@b17   ;)        local.get 6
(;@b19   ;)        local.set 7
(;@b1b   ;)        local.get 7
(;@b1d   ;)        i32.load
(;@b20   ;)        local.set 8
(;@b22   ;)        local.get 7
(;@b24   ;)        i32.load offset=4
(;@b27   ;)        local.set 9
(;@b29   ;)        i32.const 20
(;@b2b   ;)        call $alloc
(;@b2d   ;)        local.set 10
(;@b2f   ;)        local.get 10
(;@b31   ;)        i32.const 2
(;@b33   ;)        i32.store
(;@b36   ;)        local.get 10
(;@b38   ;)        i32.const 2
(;@b3a   ;)        i32.store offset=4
(;@b3d   ;)        local.get 10
(;@b3f   ;)        i32.const 41
(;@b41   ;)        i32.store offset=8
(;@b44   ;)        local.get 10
(;@b46   ;)        local.get 1
(;@b48   ;)        i32.store offset=12
(;@b4b   ;)        local.get 10
(;@b4d   ;)        local.get 7
(;@b4f   ;)        i32.store offset=16
(;@b52   ;)        local.get 10
(;@b54   ;)        local.set 10
(;@b56   ;)        i32.const 12
(;@b58   ;)        call $alloc
(;@b5a   ;)        local.set 11
(;@b5c   ;)        local.get 11
(;@b5e   ;)        local.get 8
(;@b60   ;)        i32.store
(;@b63   ;)        local.get 11
(;@b65   ;)        local.get 9
(;@b67   ;)        i32.store offset=4
(;@b6a   ;)        local.get 11
(;@b6c   ;)        local.get 10
(;@b6e   ;)        i32.store offset=8
(;@b71   ;)        local.get 11
(;@b73   ;)        local.set 11
(;@b75   ;)        i32.const 8
(;@b77   ;)        call $alloc
(;@b79   ;)        local.set 12
(;@b7b   ;)        local.get 12
(;@b7d   ;)        i32.const 1
(;@b7f   ;)        i32.store
(;@b82   ;)        local.get 12
(;@b84   ;)        local.get 11
(;@b86   ;)        i32.store offset=4
(;@b89   ;)        local.get 12
(;@b8b   ;)        br 1 (;@1;)
(;@b8d   ;)      end
(;@b8e   ;)      unreachable
(;@b8f   ;)    end
(;@b90   ;)    return
             )
(;@b93   ;)  (func $__mon_bind_lam_0 (;40;) (type $fun_2_1) (param i32 i32) (result i32)
(;@b94   ;)    (local i32 i32 i32 i32)
(;@b96   ;)    local.get 0
(;@b98   ;)    i32.load
(;@b9b   ;)    local.set 2
(;@b9d   ;)    local.get 0
(;@b9f   ;)    i32.load offset=4
(;@ba2   ;)    local.set 3
(;@ba4   ;)    local.get 2
(;@ba6   ;)    i32.load offset=8
(;@ba9   ;)    local.set 4
(;@bab   ;)    local.get 4
(;@bad   ;)    local.get 3
(;@baf   ;)    local.get 1
(;@bb1   ;)    call $__call_2
(;@bb3   ;)    return
             )
(;@bb6   ;)  (func $__mon_bind_lam_1 (;41;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@bb7   ;)    (local i32 i32 i32 i32)
(;@bb9   ;)    local.get 0
(;@bbb   ;)    i32.load
(;@bbe   ;)    local.set 3
(;@bc0   ;)    local.get 0
(;@bc2   ;)    i32.load offset=4
(;@bc5   ;)    local.set 4
(;@bc7   ;)    i32.const 20
(;@bc9   ;)    call $alloc
(;@bcb   ;)    local.set 5
(;@bcd   ;)    local.get 5
(;@bcf   ;)    i32.const 1
(;@bd1   ;)    i32.store
(;@bd4   ;)    local.get 5
(;@bd6   ;)    i32.const 2
(;@bd8   ;)    i32.store offset=4
(;@bdb   ;)    local.get 5
(;@bdd   ;)    i32.const 40
(;@bdf   ;)    i32.store offset=8
(;@be2   ;)    local.get 5
(;@be4   ;)    local.get 4
(;@be6   ;)    i32.store offset=12
(;@be9   ;)    local.get 5
(;@beb   ;)    local.get 1
(;@bed   ;)    i32.store offset=16
(;@bf0   ;)    local.get 5
(;@bf2   ;)    local.set 5
(;@bf4   ;)    local.get 5
(;@bf6   ;)    local.get 3
(;@bf8   ;)    local.get 2
(;@bfa   ;)    call $__mon_bind
(;@bfc   ;)    return
             )
(;@c00   ;)  (func $__mon_prompt (;42;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@c01   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@c03   ;)    local.get 1
(;@c05   ;)    local.get 4
(;@c07   ;)    call $__call_1
(;@c09   ;)    local.set 5
(;@c0b   ;)    local.get 3
(;@c0d   ;)    local.get 5
(;@c0f   ;)    call $__call_1
(;@c11   ;)    local.set 6
(;@c13   ;)    local.get 6
(;@c15   ;)    i32.load
(;@c18   ;)    local.set 7
(;@c1a   ;)    block (result i32) ;; label = @1
(;@c1c   ;)      block ;; label = @2
(;@c1e   ;)        block ;; label = @3
(;@c20   ;)          block ;; label = @4
(;@c22   ;)            local.get 7
(;@c24   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@c29   ;)          end
(;@c2a   ;)          local.get 6
(;@c2c   ;)          i32.load offset=4
(;@c2f   ;)          local.set 8
(;@c31   ;)          local.get 2
(;@c33   ;)          local.get 8
(;@c35   ;)          local.get 4
(;@c37   ;)          call $__call_2
(;@c39   ;)          br 2 (;@1;)
(;@c3b   ;)        end
(;@c3c   ;)        local.get 6
(;@c3e   ;)        i32.load offset=4
(;@c41   ;)        local.set 9
(;@c43   ;)        local.get 9
(;@c45   ;)        i32.load
(;@c48   ;)        local.set 10
(;@c4a   ;)        local.get 0
(;@c4c   ;)        local.get 10
(;@c4e   ;)        call $__mon_eqm
(;@c50   ;)        local.set 11
(;@c52   ;)        local.get 11
(;@c54   ;)        i32.load
(;@c57   ;)        local.set 12
(;@c59   ;)        block (result i32) ;; label = @3
(;@c5b   ;)          block ;; label = @4
(;@c5d   ;)            block ;; label = @5
(;@c5f   ;)              block ;; label = @6
(;@c61   ;)                local.get 12
(;@c63   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@c68   ;)              end
(;@c69   ;)              local.get 11
(;@c6b   ;)              i32.load offset=4
(;@c6e   ;)              local.set 13
(;@c70   ;)              local.get 9
(;@c72   ;)              local.set 14
(;@c74   ;)              local.get 14
(;@c76   ;)              i32.load
(;@c79   ;)              local.set 15
(;@c7b   ;)              local.get 14
(;@c7d   ;)              i32.load offset=4
(;@c80   ;)              local.set 16
(;@c82   ;)              i32.const 28
(;@c84   ;)              call $alloc
(;@c86   ;)              local.set 17
(;@c88   ;)              local.get 17
(;@c8a   ;)              i32.const 2
(;@c8c   ;)              i32.store
(;@c8f   ;)              local.get 17
(;@c91   ;)              i32.const 4
(;@c93   ;)              i32.store offset=4
(;@c96   ;)              local.get 17
(;@c98   ;)              i32.const 44
(;@c9a   ;)              i32.store offset=8
(;@c9d   ;)              local.get 17
(;@c9f   ;)              local.get 0
(;@ca1   ;)              i32.store offset=12
(;@ca4   ;)              local.get 17
(;@ca6   ;)              local.get 1
(;@ca8   ;)              i32.store offset=16
(;@cab   ;)              local.get 17
(;@cad   ;)              local.get 2
(;@caf   ;)              i32.store offset=20
(;@cb2   ;)              local.get 17
(;@cb4   ;)              local.get 14
(;@cb6   ;)              i32.store offset=24
(;@cb9   ;)              local.get 17
(;@cbb   ;)              local.set 17
(;@cbd   ;)              i32.const 12
(;@cbf   ;)              call $alloc
(;@cc1   ;)              local.set 18
(;@cc3   ;)              local.get 18
(;@cc5   ;)              local.get 15
(;@cc7   ;)              i32.store
(;@cca   ;)              local.get 18
(;@ccc   ;)              local.get 16
(;@cce   ;)              i32.store offset=4
(;@cd1   ;)              local.get 18
(;@cd3   ;)              local.get 17
(;@cd5   ;)              i32.store offset=8
(;@cd8   ;)              local.get 18
(;@cda   ;)              local.set 18
(;@cdc   ;)              i32.const 8
(;@cde   ;)              call $alloc
(;@ce0   ;)              local.set 19
(;@ce2   ;)              local.get 19
(;@ce4   ;)              i32.const 1
(;@ce6   ;)              i32.store
(;@ce9   ;)              local.get 19
(;@ceb   ;)              local.get 18
(;@ced   ;)              i32.store offset=4
(;@cf0   ;)              local.get 19
(;@cf2   ;)              br 2 (;@3;)
(;@cf4   ;)            end
(;@cf5   ;)            local.get 11
(;@cf7   ;)            i32.load offset=4
(;@cfa   ;)            local.set 13
(;@cfc   ;)            i32.const 28
(;@cfe   ;)            call $alloc
(;@d00   ;)            local.set 19
(;@d02   ;)            local.get 19
(;@d04   ;)            i32.const 2
(;@d06   ;)            i32.store
(;@d09   ;)            local.get 19
(;@d0b   ;)            i32.const 4
(;@d0d   ;)            i32.store offset=4
(;@d10   ;)            local.get 19
(;@d12   ;)            i32.const 46
(;@d14   ;)            i32.store offset=8
(;@d17   ;)            local.get 19
(;@d19   ;)            local.get 0
(;@d1b   ;)            i32.store offset=12
(;@d1e   ;)            local.get 19
(;@d20   ;)            local.get 1
(;@d22   ;)            i32.store offset=16
(;@d25   ;)            local.get 19
(;@d27   ;)            local.get 2
(;@d29   ;)            i32.store offset=20
(;@d2c   ;)            local.get 19
(;@d2e   ;)            local.get 9
(;@d30   ;)            i32.store offset=24
(;@d33   ;)            local.get 19
(;@d35   ;)            local.set 19
(;@d37   ;)            local.get 9
(;@d39   ;)            i32.load offset=4
(;@d3c   ;)            local.set 20
(;@d3e   ;)            local.get 20
(;@d40   ;)            local.get 19
(;@d42   ;)            local.get 4
(;@d44   ;)            call $__call_2
(;@d46   ;)            br 1 (;@3;)
(;@d48   ;)          end
(;@d49   ;)          unreachable
(;@d4a   ;)        end
(;@d4b   ;)        br 1 (;@1;)
(;@d4d   ;)      end
(;@d4e   ;)      unreachable
(;@d4f   ;)    end
(;@d50   ;)    return
             )
(;@d53   ;)  (func $__mon_prompt_lam_0 (;43;) (type $fun_2_1) (param i32 i32) (result i32)
(;@d54   ;)    (local i32 i32 i32 i32)
(;@d56   ;)    local.get 0
(;@d58   ;)    i32.load
(;@d5b   ;)    local.set 2
(;@d5d   ;)    local.get 0
(;@d5f   ;)    i32.load offset=4
(;@d62   ;)    local.set 3
(;@d64   ;)    local.get 2
(;@d66   ;)    i32.load offset=8
(;@d69   ;)    local.set 4
(;@d6b   ;)    local.get 4
(;@d6d   ;)    local.get 3
(;@d6f   ;)    local.get 1
(;@d71   ;)    call $__call_2
(;@d73   ;)    return
             )
(;@d76   ;)  (func $__mon_prompt_lam_1 (;44;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@d77   ;)    (local i32 i32 i32 i32 i32 i32)
(;@d79   ;)    local.get 0
(;@d7b   ;)    i32.load
(;@d7e   ;)    local.set 3
(;@d80   ;)    local.get 0
(;@d82   ;)    i32.load offset=4
(;@d85   ;)    local.set 4
(;@d87   ;)    local.get 0
(;@d89   ;)    i32.load offset=8
(;@d8c   ;)    local.set 5
(;@d8e   ;)    local.get 0
(;@d90   ;)    i32.load offset=12
(;@d93   ;)    local.set 6
(;@d95   ;)    i32.const 20
(;@d97   ;)    call $alloc
(;@d99   ;)    local.set 7
(;@d9b   ;)    local.get 7
(;@d9d   ;)    i32.const 1
(;@d9f   ;)    i32.store
(;@da2   ;)    local.get 7
(;@da4   ;)    i32.const 2
(;@da6   ;)    i32.store offset=4
(;@da9   ;)    local.get 7
(;@dab   ;)    i32.const 43
(;@dad   ;)    i32.store offset=8
(;@db0   ;)    local.get 7
(;@db2   ;)    local.get 6
(;@db4   ;)    i32.store offset=12
(;@db7   ;)    local.get 7
(;@db9   ;)    local.get 1
(;@dbb   ;)    i32.store offset=16
(;@dbe   ;)    local.get 7
(;@dc0   ;)    local.set 7
(;@dc2   ;)    local.get 3
(;@dc4   ;)    local.get 4
(;@dc6   ;)    local.get 5
(;@dc8   ;)    local.get 7
(;@dca   ;)    local.get 2
(;@dcc   ;)    call $__mon_prompt
(;@dce   ;)    return
             )
(;@dd1   ;)  (func $__mon_prompt_lam_2 (;45;) (type $fun_2_1) (param i32 i32) (result i32)
(;@dd2   ;)    (local i32 i32 i32 i32)
(;@dd4   ;)    local.get 0
(;@dd6   ;)    i32.load
(;@dd9   ;)    local.set 2
(;@ddb   ;)    local.get 0
(;@ddd   ;)    i32.load offset=4
(;@de0   ;)    local.set 3
(;@de2   ;)    local.get 2
(;@de4   ;)    i32.load offset=8
(;@de7   ;)    local.set 4
(;@de9   ;)    local.get 4
(;@deb   ;)    local.get 3
(;@ded   ;)    local.get 1
(;@def   ;)    call $__call_2
(;@df1   ;)    return
             )
(;@df4   ;)  (func $__mon_prompt_lam_3 (;46;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@df5   ;)    (local i32 i32 i32 i32 i32 i32)
(;@df7   ;)    local.get 0
(;@df9   ;)    i32.load
(;@dfc   ;)    local.set 3
(;@dfe   ;)    local.get 0
(;@e00   ;)    i32.load offset=4
(;@e03   ;)    local.set 4
(;@e05   ;)    local.get 0
(;@e07   ;)    i32.load offset=8
(;@e0a   ;)    local.set 5
(;@e0c   ;)    local.get 0
(;@e0e   ;)    i32.load offset=12
(;@e11   ;)    local.set 6
(;@e13   ;)    i32.const 20
(;@e15   ;)    call $alloc
(;@e17   ;)    local.set 7
(;@e19   ;)    local.get 7
(;@e1b   ;)    i32.const 1
(;@e1d   ;)    i32.store
(;@e20   ;)    local.get 7
(;@e22   ;)    i32.const 2
(;@e24   ;)    i32.store offset=4
(;@e27   ;)    local.get 7
(;@e29   ;)    i32.const 45
(;@e2b   ;)    i32.store offset=8
(;@e2e   ;)    local.get 7
(;@e30   ;)    local.get 6
(;@e32   ;)    i32.store offset=12
(;@e35   ;)    local.get 7
(;@e37   ;)    local.get 1
(;@e39   ;)    i32.store offset=16
(;@e3c   ;)    local.get 7
(;@e3e   ;)    local.set 7
(;@e40   ;)    local.get 3
(;@e42   ;)    local.get 4
(;@e44   ;)    local.get 5
(;@e46   ;)    local.get 7
(;@e48   ;)    local.get 2
(;@e4a   ;)    call $__mon_prompt
(;@e4c   ;)    return
             )
(;@62    ;)  (table (;0;) 47 47 funcref)
(;@69    ;)  (memory (;0;) 1)
(;@6e    ;)  (global (;0;) (mut i32) i32.const 0)
(;@73    ;)  (global (;1;) (mut i32) i32.const 0)
(;@7b    ;)  (export "main" (func $main))
(;@82    ;)  (export "mem" (memory 0))
(;@8b    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__call_1 $__call_2 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11 $main_lam_12 $main_lam_13 $main_lam_14 $main_lam_15 $main_lam_16 $main_lam_17 $main_lam_18 $main_lam_19 $main_lam_20 $main_lam_21 $main_lam_22 $main_lam_23 $main_lam_24 $main_lam_25 $main_lam_26 $main_lam_27 $main_lam_28 $main_lam_29 $main_lam_30 $main_lam_31 $main_lam_32 $__mon_bind $__mon_bind_lam_0 $__mon_bind_lam_1 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1 $__mon_prompt_lam_2 $__mon_prompt_lam_3)
           )
