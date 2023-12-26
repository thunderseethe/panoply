(module $wand
(;@b     ;)  (type (;0;) (func (result i32)))
(;@f     ;)  (type (;1;) (func (param i32 i32) (result i32)))
(;@15    ;)  (type (;2;) (func (param i32 i32 i32) (result i32)))
(;@1c    ;)  (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
(;@22    ;)  (type $fun_1_1 (;4;) (func (param i32) (result i32)))
(;@27    ;)  (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
(;@2e    ;)  (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
(;@36    ;)  (type $fun_5_1 (;7;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@3f    ;)  (type $fun_0_1 (;8;) (func (result i32)))
(;@46    ;)  (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
(;@68    ;)  (import "intrinsic" "__mon_prompt" (func (;1;) (type 2)))
(;@81    ;)  (import "intrinsic" "__mon_bind" (func (;2;) (type 1)))
(;@98    ;)  (import "intrinsic" "__mon_eqm" (func (;3;) (type 1)))
(;@10a   ;)  (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
(;@10b   ;)    local.get 1
(;@10d   ;)    local.get 0
(;@10f   ;)    i32.load
(;@112   ;)    call_indirect (type $fun_1_1)
             )
(;@117   ;)  (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@118   ;)    local.get 1
(;@11a   ;)    local.get 2
(;@11c   ;)    local.get 0
(;@11e   ;)    i32.load
(;@121   ;)    call_indirect (type $fun_2_1)
             )
(;@126   ;)  (func $__apply_3_0 (;6;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@127   ;)    local.get 1
(;@129   ;)    local.get 2
(;@12b   ;)    local.get 3
(;@12d   ;)    local.get 0
(;@12f   ;)    i32.load
(;@132   ;)    call_indirect (type $fun_3_1)
             )
(;@137   ;)  (func $f (;7;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@138   ;)    (local i32 i32 i32 i32 i32 i32)
(;@13a   ;)    local.get 0
(;@13c   ;)    i32.load
(;@13f   ;)    local.set 5
(;@141   ;)    local.get 5
(;@143   ;)    i32.const 4
(;@145   ;)    i32.add
(;@146   ;)    local.get 2
(;@148   ;)    local.get 3
(;@14a   ;)    local.get 5
(;@14c   ;)    i32.load
(;@14f   ;)    call_indirect (type $fun_3_1)
(;@152   ;)    local.set 6
(;@154   ;)    local.get 1
(;@156   ;)    i32.load offset=12
(;@159   ;)    local.set 7
(;@15b   ;)    local.get 7
(;@15d   ;)    i32.load
(;@160   ;)    local.set 8
(;@162   ;)    local.get 8
(;@164   ;)    i32.const 4
(;@166   ;)    i32.add
(;@167   ;)    local.get 6
(;@169   ;)    local.get 8
(;@16b   ;)    i32.load
(;@16e   ;)    call_indirect (type $fun_2_1)
(;@171   ;)    local.set 9
(;@173   ;)    global.get 0
(;@175   ;)    i32.const 0
(;@177   ;)    i32.store
(;@17a   ;)    global.get 0
(;@17c   ;)    local.get 9
(;@17e   ;)    i32.store offset=4
(;@181   ;)    global.get 0
(;@183   ;)    global.get 0
(;@185   ;)    i32.const 8
(;@187   ;)    i32.add
(;@188   ;)    global.set 0
             )
(;@18d   ;)  (func $main (;8;) (type $fun_0_1) (result i32)
(;@18e   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@190   ;)    global.get 0
(;@192   ;)    i32.const 5
(;@194   ;)    i32.store
(;@197   ;)    global.get 0
(;@199   ;)    i32.const 9
(;@19b   ;)    i32.store offset=4
(;@19e   ;)    global.get 0
(;@1a0   ;)    global.get 0
(;@1a2   ;)    i32.const 8
(;@1a4   ;)    i32.add
(;@1a5   ;)    global.set 0
(;@1a7   ;)    local.set 0
(;@1a9   ;)    global.get 0
(;@1ab   ;)    i32.const 6
(;@1ad   ;)    i32.store
(;@1b0   ;)    global.get 0
(;@1b2   ;)    i32.const 10
(;@1b4   ;)    i32.store offset=4
(;@1b7   ;)    global.get 0
(;@1b9   ;)    global.get 0
(;@1bb   ;)    i32.const 8
(;@1bd   ;)    i32.add
(;@1be   ;)    global.set 0
(;@1c0   ;)    local.set 1
(;@1c2   ;)    global.get 0
(;@1c4   ;)    i32.const 4
(;@1c6   ;)    i32.store
(;@1c9   ;)    global.get 0
(;@1cb   ;)    i32.const 11
(;@1cd   ;)    i32.store offset=4
(;@1d0   ;)    global.get 0
(;@1d2   ;)    global.get 0
(;@1d4   ;)    i32.const 8
(;@1d6   ;)    i32.add
(;@1d7   ;)    global.set 0
(;@1d9   ;)    local.set 2
(;@1db   ;)    global.get 0
(;@1dd   ;)    i32.const 4
(;@1df   ;)    i32.store
(;@1e2   ;)    global.get 0
(;@1e4   ;)    i32.const 12
(;@1e6   ;)    i32.store offset=4
(;@1e9   ;)    global.get 0
(;@1eb   ;)    global.get 0
(;@1ed   ;)    i32.const 8
(;@1ef   ;)    i32.add
(;@1f0   ;)    global.set 0
(;@1f2   ;)    local.set 3
(;@1f4   ;)    global.get 0
(;@1f6   ;)    local.get 2
(;@1f8   ;)    i32.store
(;@1fb   ;)    global.get 0
(;@1fd   ;)    local.get 3
(;@1ff   ;)    i32.store offset=4
(;@202   ;)    global.get 0
(;@204   ;)    global.get 0
(;@206   ;)    i32.const 8
(;@208   ;)    i32.add
(;@209   ;)    global.set 0
(;@20b   ;)    local.set 4
(;@20d   ;)    global.get 0
(;@20f   ;)    i32.const 4
(;@211   ;)    i32.store
(;@214   ;)    global.get 0
(;@216   ;)    i32.const 13
(;@218   ;)    i32.store offset=4
(;@21b   ;)    global.get 0
(;@21d   ;)    global.get 0
(;@21f   ;)    i32.const 8
(;@221   ;)    i32.add
(;@222   ;)    global.set 0
(;@224   ;)    local.set 5
(;@226   ;)    global.get 0
(;@228   ;)    i32.const 4
(;@22a   ;)    i32.store
(;@22d   ;)    global.get 0
(;@22f   ;)    i32.const 14
(;@231   ;)    i32.store offset=4
(;@234   ;)    global.get 0
(;@236   ;)    global.get 0
(;@238   ;)    i32.const 8
(;@23a   ;)    i32.add
(;@23b   ;)    global.set 0
(;@23d   ;)    local.set 6
(;@23f   ;)    global.get 0
(;@241   ;)    local.get 5
(;@243   ;)    i32.store
(;@246   ;)    global.get 0
(;@248   ;)    local.get 6
(;@24a   ;)    i32.store offset=4
(;@24d   ;)    global.get 0
(;@24f   ;)    global.get 0
(;@251   ;)    i32.const 8
(;@253   ;)    i32.add
(;@254   ;)    global.set 0
(;@256   ;)    local.set 7
(;@258   ;)    global.get 0
(;@25a   ;)    local.get 0
(;@25c   ;)    i32.store
(;@25f   ;)    global.get 0
(;@261   ;)    local.get 1
(;@263   ;)    i32.store offset=4
(;@266   ;)    global.get 0
(;@268   ;)    local.get 4
(;@26a   ;)    i32.store offset=8
(;@26d   ;)    global.get 0
(;@26f   ;)    local.get 7
(;@271   ;)    i32.store offset=12
(;@274   ;)    global.get 0
(;@276   ;)    global.get 0
(;@278   ;)    i32.const 16
(;@27a   ;)    i32.add
(;@27b   ;)    global.set 0
(;@27d   ;)    local.set 8
(;@27f   ;)    global.get 0
(;@281   ;)    i32.const 5
(;@283   ;)    i32.store
(;@286   ;)    global.get 0
(;@288   ;)    i32.const 15
(;@28a   ;)    i32.store offset=4
(;@28d   ;)    global.get 0
(;@28f   ;)    global.get 0
(;@291   ;)    i32.const 8
(;@293   ;)    i32.add
(;@294   ;)    global.set 0
(;@296   ;)    local.set 9
(;@298   ;)    global.get 0
(;@29a   ;)    i32.const 6
(;@29c   ;)    i32.store
(;@29f   ;)    global.get 0
(;@2a1   ;)    i32.const 16
(;@2a3   ;)    i32.store offset=4
(;@2a6   ;)    global.get 0
(;@2a8   ;)    global.get 0
(;@2aa   ;)    i32.const 8
(;@2ac   ;)    i32.add
(;@2ad   ;)    global.set 0
(;@2af   ;)    local.set 10
(;@2b1   ;)    global.get 0
(;@2b3   ;)    i32.const 4
(;@2b5   ;)    i32.store
(;@2b8   ;)    global.get 0
(;@2ba   ;)    i32.const 17
(;@2bc   ;)    i32.store offset=4
(;@2bf   ;)    global.get 0
(;@2c1   ;)    global.get 0
(;@2c3   ;)    i32.const 8
(;@2c5   ;)    i32.add
(;@2c6   ;)    global.set 0
(;@2c8   ;)    local.set 11
(;@2ca   ;)    global.get 0
(;@2cc   ;)    i32.const 4
(;@2ce   ;)    i32.store
(;@2d1   ;)    global.get 0
(;@2d3   ;)    i32.const 18
(;@2d5   ;)    i32.store offset=4
(;@2d8   ;)    global.get 0
(;@2da   ;)    global.get 0
(;@2dc   ;)    i32.const 8
(;@2de   ;)    i32.add
(;@2df   ;)    global.set 0
(;@2e1   ;)    local.set 12
(;@2e3   ;)    global.get 0
(;@2e5   ;)    local.get 11
(;@2e7   ;)    i32.store
(;@2ea   ;)    global.get 0
(;@2ec   ;)    local.get 12
(;@2ee   ;)    i32.store offset=4
(;@2f1   ;)    global.get 0
(;@2f3   ;)    global.get 0
(;@2f5   ;)    i32.const 8
(;@2f7   ;)    i32.add
(;@2f8   ;)    global.set 0
(;@2fa   ;)    local.set 13
(;@2fc   ;)    global.get 0
(;@2fe   ;)    i32.const 4
(;@300   ;)    i32.store
(;@303   ;)    global.get 0
(;@305   ;)    i32.const 19
(;@307   ;)    i32.store offset=4
(;@30a   ;)    global.get 0
(;@30c   ;)    global.get 0
(;@30e   ;)    i32.const 8
(;@310   ;)    i32.add
(;@311   ;)    global.set 0
(;@313   ;)    local.set 14
(;@315   ;)    global.get 0
(;@317   ;)    i32.const 4
(;@319   ;)    i32.store
(;@31c   ;)    global.get 0
(;@31e   ;)    i32.const 20
(;@320   ;)    i32.store offset=4
(;@323   ;)    global.get 0
(;@325   ;)    global.get 0
(;@327   ;)    i32.const 8
(;@329   ;)    i32.add
(;@32a   ;)    global.set 0
(;@32c   ;)    local.set 15
(;@32e   ;)    global.get 0
(;@330   ;)    local.get 14
(;@332   ;)    i32.store
(;@335   ;)    global.get 0
(;@337   ;)    local.get 15
(;@339   ;)    i32.store offset=4
(;@33c   ;)    global.get 0
(;@33e   ;)    global.get 0
(;@340   ;)    i32.const 8
(;@342   ;)    i32.add
(;@343   ;)    global.set 0
(;@345   ;)    local.set 16
(;@347   ;)    global.get 0
(;@349   ;)    local.get 9
(;@34b   ;)    i32.store
(;@34e   ;)    global.get 0
(;@350   ;)    local.get 10
(;@352   ;)    i32.store offset=4
(;@355   ;)    global.get 0
(;@357   ;)    local.get 13
(;@359   ;)    i32.store offset=8
(;@35c   ;)    global.get 0
(;@35e   ;)    local.get 16
(;@360   ;)    i32.store offset=12
(;@363   ;)    global.get 0
(;@365   ;)    global.get 0
(;@367   ;)    i32.const 16
(;@369   ;)    i32.add
(;@36a   ;)    global.set 0
(;@36c   ;)    local.set 17
(;@36e   ;)    global.get 0
(;@370   ;)    local.set 18
(;@372   ;)    global.get 0
(;@374   ;)    local.set 19
(;@376   ;)    local.get 8
(;@378   ;)    local.get 17
(;@37a   ;)    i32.const 12
(;@37c   ;)    local.get 18
(;@37e   ;)    local.get 19
(;@380   ;)    call $f
(;@382   ;)    local.set 20
(;@384   ;)    local.get 20
(;@386   ;)    i32.load
(;@389   ;)    local.set 21
(;@38b   ;)    block (result i32) ;; label = @1
(;@38d   ;)      block (result i32) ;; label = @2
(;@38f   ;)        block (result i32) ;; label = @3
(;@391   ;)          local.get 21
(;@393   ;)          local.get 21
(;@395   ;)          br_table 0 (;@3;) 1 (;@2;)
(;@399   ;)        end
(;@39a   ;)        local.get 20
(;@39c   ;)        i32.load offset=4
(;@39f   ;)        local.set 22
(;@3a1   ;)        local.get 22
(;@3a3   ;)        br 1 (;@1;)
(;@3a5   ;)      end
(;@3a6   ;)      unreachable
(;@3a7   ;)    end
             )
(;@3aa   ;)  (func $main_lam_0 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3ab   ;)    (local i32)
(;@3ad   ;)    global.get 0
(;@3af   ;)    local.get 0
(;@3b1   ;)    i32.store
(;@3b4   ;)    global.get 0
(;@3b6   ;)    local.get 1
(;@3b8   ;)    i32.store offset=4
(;@3bb   ;)    global.get 0
(;@3bd   ;)    global.get 0
(;@3bf   ;)    i32.const 8
(;@3c1   ;)    i32.add
(;@3c2   ;)    global.set 0
             )
(;@3c6   ;)  (func $main_lam_1 (;10;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@3c7   ;)    (local i32 i32 i32)
(;@3c9   ;)    local.get 2
(;@3cb   ;)    i32.load
(;@3ce   ;)    local.set 3
(;@3d0   ;)    block (result i32) ;; label = @1
(;@3d2   ;)      block (result i32) ;; label = @2
(;@3d4   ;)        block (result i32) ;; label = @3
(;@3d6   ;)          block (result i32) ;; label = @4
(;@3d8   ;)            local.get 3
(;@3da   ;)            local.get 3
(;@3dc   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@3e1   ;)          end
(;@3e2   ;)          local.get 2
(;@3e4   ;)          i32.load offset=4
(;@3e7   ;)          local.set 4
(;@3e9   ;)          local.get 0
(;@3eb   ;)          i32.const 4
(;@3ed   ;)          i32.add
(;@3ee   ;)          local.get 4
(;@3f0   ;)          local.get 0
(;@3f2   ;)          i32.load
(;@3f5   ;)          call_indirect (type $fun_2_1)
(;@3f8   ;)          br 2 (;@1;)
(;@3fa   ;)        end
(;@3fb   ;)        local.get 2
(;@3fd   ;)        i32.load offset=4
(;@400   ;)        local.set 4
(;@402   ;)        local.get 1
(;@404   ;)        i32.const 4
(;@406   ;)        i32.add
(;@407   ;)        local.get 4
(;@409   ;)        local.get 1
(;@40b   ;)        i32.load
(;@40e   ;)        call_indirect (type $fun_2_1)
(;@411   ;)        br 1 (;@1;)
(;@413   ;)      end
(;@414   ;)      unreachable
(;@415   ;)    end
             )
(;@418   ;)  (func $main_lam_2 (;11;) (type $fun_1_1) (param i32) (result i32)
(;@419   ;)    (local i32)
(;@41b   ;)    local.get 0
(;@41d   ;)    i32.load
             )
(;@422   ;)  (func $main_lam_3 (;12;) (type $fun_1_1) (param i32) (result i32)
(;@423   ;)    (local i32)
(;@425   ;)    global.get 0
(;@427   ;)    i32.const 0
(;@429   ;)    i32.store
(;@42c   ;)    global.get 0
(;@42e   ;)    local.get 0
(;@430   ;)    i32.store offset=4
(;@433   ;)    global.get 0
(;@435   ;)    global.get 0
(;@437   ;)    i32.const 8
(;@439   ;)    i32.add
(;@43a   ;)    global.set 0
             )
(;@43e   ;)  (func $main_lam_4 (;13;) (type $fun_1_1) (param i32) (result i32)
(;@43f   ;)    (local i32)
(;@441   ;)    local.get 0
(;@443   ;)    i32.load offset=4
             )
(;@448   ;)  (func $main_lam_5 (;14;) (type $fun_1_1) (param i32) (result i32)
(;@449   ;)    (local i32)
(;@44b   ;)    global.get 0
(;@44d   ;)    i32.const 1
(;@44f   ;)    i32.store
(;@452   ;)    global.get 0
(;@454   ;)    local.get 0
(;@456   ;)    i32.store offset=4
(;@459   ;)    global.get 0
(;@45b   ;)    global.get 0
(;@45d   ;)    i32.const 8
(;@45f   ;)    i32.add
(;@460   ;)    global.set 0
             )
(;@464   ;)  (func $main_lam_6 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@465   ;)    (local i32)
(;@467   ;)    global.get 0
(;@469   ;)    local.get 1
(;@46b   ;)    i32.store
(;@46e   ;)    global.get 0
(;@470   ;)    local.get 0
(;@472   ;)    i32.store offset=4
(;@475   ;)    global.get 0
(;@477   ;)    global.get 0
(;@479   ;)    i32.const 8
(;@47b   ;)    i32.add
(;@47c   ;)    global.set 0
             )
(;@480   ;)  (func $main_lam_7 (;16;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@481   ;)    (local i32 i32 i32)
(;@483   ;)    local.get 2
(;@485   ;)    i32.load
(;@488   ;)    local.set 3
(;@48a   ;)    block (result i32) ;; label = @1
(;@48c   ;)      block (result i32) ;; label = @2
(;@48e   ;)        block (result i32) ;; label = @3
(;@490   ;)          block (result i32) ;; label = @4
(;@492   ;)            local.get 3
(;@494   ;)            local.get 3
(;@496   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@49b   ;)          end
(;@49c   ;)          local.get 2
(;@49e   ;)          i32.load offset=4
(;@4a1   ;)          local.set 4
(;@4a3   ;)          local.get 1
(;@4a5   ;)          i32.const 4
(;@4a7   ;)          i32.add
(;@4a8   ;)          local.get 4
(;@4aa   ;)          local.get 1
(;@4ac   ;)          i32.load
(;@4af   ;)          call_indirect (type $fun_2_1)
(;@4b2   ;)          br 2 (;@1;)
(;@4b4   ;)        end
(;@4b5   ;)        local.get 2
(;@4b7   ;)        i32.load offset=4
(;@4ba   ;)        local.set 4
(;@4bc   ;)        local.get 0
(;@4be   ;)        i32.const 4
(;@4c0   ;)        i32.add
(;@4c1   ;)        local.get 4
(;@4c3   ;)        local.get 0
(;@4c5   ;)        i32.load
(;@4c8   ;)        call_indirect (type $fun_2_1)
(;@4cb   ;)        br 1 (;@1;)
(;@4cd   ;)      end
(;@4ce   ;)      unreachable
(;@4cf   ;)    end
             )
(;@4d2   ;)  (func $main_lam_8 (;17;) (type $fun_1_1) (param i32) (result i32)
(;@4d3   ;)    (local i32)
(;@4d5   ;)    local.get 0
(;@4d7   ;)    i32.load offset=4
             )
(;@4dc   ;)  (func $main_lam_9 (;18;) (type $fun_1_1) (param i32) (result i32)
(;@4dd   ;)    (local i32)
(;@4df   ;)    global.get 0
(;@4e1   ;)    i32.const 1
(;@4e3   ;)    i32.store
(;@4e6   ;)    global.get 0
(;@4e8   ;)    local.get 0
(;@4ea   ;)    i32.store offset=4
(;@4ed   ;)    global.get 0
(;@4ef   ;)    global.get 0
(;@4f1   ;)    i32.const 8
(;@4f3   ;)    i32.add
(;@4f4   ;)    global.set 0
             )
(;@4f8   ;)  (func $main_lam_10 (;19;) (type $fun_1_1) (param i32) (result i32)
(;@4f9   ;)    (local i32)
(;@4fb   ;)    local.get 0
(;@4fd   ;)    i32.load
             )
(;@502   ;)  (func $main_lam_11 (;20;) (type $fun_1_1) (param i32) (result i32)
(;@503   ;)    (local i32)
(;@505   ;)    global.get 0
(;@507   ;)    i32.const 0
(;@509   ;)    i32.store
(;@50c   ;)    global.get 0
(;@50e   ;)    local.get 0
(;@510   ;)    i32.store offset=4
(;@513   ;)    global.get 0
(;@515   ;)    global.get 0
(;@517   ;)    i32.const 8
(;@519   ;)    i32.add
(;@51a   ;)    global.set 0
             )
(;@c5    ;)  (table (;0;) 21 21 funcref)
(;@cc    ;)  (memory (;0;) 1)
(;@d1    ;)  (global (;0;) (mut i32) i32.const 0)
(;@d9    ;)  (export "main" (func $main))
(;@e0    ;)  (export "mem" (memory 0))
(;@e9    ;)  (elem (;0;) (i32.const 0) func 0 1 2 3 $__apply_1_0 $__apply_2_0 $__apply_3_0 $f $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11)
           )
     local.get 1
(;@3fc   ;)      i32.const 4
(;@3fe   ;)      i32.add
(;@3ff   ;)      local.get 4
(;@401   ;)      local.get 1
(;@403   ;)      i32.load
(;@406   ;)      call_indirect (type $fun_2_1)
(;@409   ;)    end
             )
(;@40c   ;)  (func $main_lam_2 (;11;) (type $fun_1_1) (param i32) (result i32)
(;@40d   ;)    (local i32)
(;@40f   ;)    local.get 0
(;@411   ;)    i32.load
             )
(;@416   ;)  (func $main_lam_3 (;12;) (type $fun_1_1) (param i32) (result i32)
(;@417   ;)    (local i32)
(;@419   ;)    global.get 0
(;@41b   ;)    i32.const 0
(;@41d   ;)    i32.store
(;@420   ;)    global.get 0
(;@422   ;)    local.get 0
(;@424   ;)    i32.store offset=4
(;@427   ;)    global.get 0
(;@429   ;)    global.get 0
(;@42b   ;)    i32.const 8
(;@42d   ;)    i32.add
(;@42e   ;)    global.set 0
             )
(;@432   ;)  (func $main_lam_4 (;13;) (type $fun_1_1) (param i32) (result i32)
(;@433   ;)    (local i32)
(;@435   ;)    local.get 0
(;@437   ;)    i32.load offset=4
             )
(;@43c   ;)  (func $main_lam_5 (;14;) (type $fun_1_1) (param i32) (result i32)
(;@43d   ;)    (local i32)
(;@43f   ;)    global.get 0
(;@441   ;)    i32.const 1
(;@443   ;)    i32.store
(;@446   ;)    global.get 0
(;@448   ;)    local.get 0
(;@44a   ;)    i32.store offset=4
(;@44d   ;)    global.get 0
(;@44f   ;)    global.get 0
(;@451   ;)    i32.const 8
(;@453   ;)    i32.add
(;@454   ;)    global.set 0
             )
(;@458   ;)  (func $main_lam_6 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@459   ;)    (local i32)
(;@45b   ;)    global.get 0
(;@45d   ;)    local.get 1
(;@45f   ;)    i32.store
(;@462   ;)    global.get 0
(;@464   ;)    local.get 0
(;@466   ;)    i32.store offset=4
(;@469   ;)    global.get 0
(;@46b   ;)    global.get 0
(;@46d   ;)    i32.const 8
(;@46f   ;)    i32.add
(;@470   ;)    global.set 0
             )
(;@474   ;)  (func $main_lam_7 (;16;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@475   ;)    (local i32 i32 i32)
(;@477   ;)    local.get 2
(;@479   ;)    i32.load
(;@47c   ;)    local.set 3
(;@47e   ;)    block (result i32) ;; label = @1
(;@480   ;)      block (result i32) ;; label = @2
(;@482   ;)        block (result i32) ;; label = @3
(;@484   ;)          local.get 3
(;@486   ;)          local.get 3
(;@488   ;)          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
(;@48d   ;)        end
(;@48e   ;)        local.get 2
(;@490   ;)        i32.load offset=4
(;@493   ;)        local.set 4
(;@495   ;)        local.get 1
(;@497   ;)        i32.const 4
(;@499   ;)        i32.add
(;@49a   ;)        local.get 4
(;@49c   ;)        local.get 1
(;@49e   ;)        i32.load
(;@4a1   ;)        call_indirect (type $fun_2_1)
(;@4a4   ;)        br 1 (;@1;)
(;@4a6   ;)      end
(;@4a7   ;)      local.get 2
(;@4a9   ;)      i32.load offset=4
(;@4ac   ;)      local.set 4
(;@4ae   ;)      local.get 0
(;@4b0   ;)      i32.const 4
(;@4b2   ;)      i32.add
(;@4b3   ;)      local.get 4
(;@4b5   ;)      local.get 0
(;@4b7   ;)      i32.load
(;@4ba   ;)      call_indirect (type $fun_2_1)
(;@4bd   ;)    end
             )
(;@4c0   ;)  (func $main_lam_8 (;17;) (type $fun_1_1) (param i32) (result i32)
(;@4c1   ;)    (local i32)
(;@4c3   ;)    local.get 0
(;@4c5   ;)    i32.load offset=4
             )
(;@4ca   ;)  (func $main_lam_9 (;18;) (type $fun_1_1) (param i32) (result i32)
(;@4cb   ;)    (local i32)
(;@4cd   ;)    global.get 0
(;@4cf   ;)    i32.const 1
(;@4d1   ;)    i32.store
(;@4d4   ;)    global.get 0
(;@4d6   ;)    local.get 0
(;@4d8   ;)    i32.store offset=4
(;@4db   ;)    global.get 0
(;@4dd   ;)    global.get 0
(;@4df   ;)    i32.const 8
(;@4e1   ;)    i32.add
(;@4e2   ;)    global.set 0
             )
(;@4e6   ;)  (func $main_lam_10 (;19;) (type $fun_1_1) (param i32) (result i32)
(;@4e7   ;)    (local i32)
(;@4e9   ;)    local.get 0
(;@4eb   ;)    i32.load
             )
(;@4f0   ;)  (func $main_lam_11 (;20;) (type $fun_1_1) (param i32) (result i32)
(;@4f1   ;)    (local i32)
(;@4f3   ;)    global.get 0
(;@4f5   ;)    i32.const 0
(;@4f7   ;)    i32.store
(;@4fa   ;)    global.get 0
(;@4fc   ;)    local.get 0
(;@4fe   ;)    i32.store offset=4
(;@501   ;)    global.get 0
(;@503   ;)    global.get 0
(;@505   ;)    i32.const 8
(;@507   ;)    i32.add
(;@508   ;)    global.set 0
             )
(;@c5    ;)  (table (;0;) 21 21 funcref)
(;@cc    ;)  (memory (;0;) 1)
(;@d1    ;)  (global (;0;) (mut i32) i32.const 0)
(;@d9    ;)  (export "main" (func $main))
(;@e0    ;)  (export "mem" (memory 0))
(;@e9    ;)  (elem (;0;) (i32.const 0) func 0 1 2 3 $__apply_1_0 $__apply_2_0 $__apply_3_0 $f $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11)
           )
