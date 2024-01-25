(module $state
(;@b     ;)  (type (;0;) (func (result i32)))
(;@f     ;)  (type (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type (;2;) (func (param i32) (result i32)))
(;@19    ;)  (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
(;@1f    ;)  (type $fun_1_1 (;4;) (func (param i32) (result i32)))
(;@24    ;)  (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
(;@2b    ;)  (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
(;@33    ;)  (type $fun_0_1 (;7;) (func (result i32)))
(;@3a    ;)  (import "intrinsic" "__mon_generate_marker" (func $__mon_generate_marker (;0;) (type 0)))
(;@5c    ;)  (import "intrinsic" "alloc" (func $alloc (;1;) (type 1)))
(;@6e    ;)  (import "intrinsic" "trace" (func $trace (;2;) (type 2)))
(;@101   ;)  (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@102   ;)    (local i32)
(;@104   ;)    i32.const 4
(;@106   ;)    call $alloc
(;@108   ;)    local.tee 2
(;@10a   ;)    i32.const 1
(;@10c   ;)    i32.const 0
(;@10e   ;)    local.get 0
(;@110   ;)    local.get 1
(;@112   ;)    i32.eq
(;@113   ;)    select
(;@114   ;)    i32.store
(;@117   ;)    local.get 2
(;@119   ;)    return
             )
(;@11c   ;)  (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
(;@11d   ;)    local.get 1
(;@11f   ;)    local.get 0
(;@121   ;)    i32.load
(;@124   ;)    call_indirect (type $fun_1_1)
             )
(;@129   ;)  (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@12a   ;)    local.get 1
(;@12c   ;)    local.get 2
(;@12e   ;)    local.get 0
(;@130   ;)    i32.load
(;@133   ;)    call_indirect (type $fun_2_1)
             )
(;@138   ;)  (func $__apply_2_1 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@139   ;)    local.get 0
(;@13b   ;)    i32.load offset=4
(;@13e   ;)    local.get 1
(;@140   ;)    local.get 0
(;@142   ;)    i32.load
(;@145   ;)    call_indirect (type $fun_2_1)
             )
(;@14a   ;)  (func $__apply_3_0 (;7;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@14b   ;)    local.get 1
(;@14d   ;)    local.get 2
(;@14f   ;)    local.get 3
(;@151   ;)    local.get 0
(;@153   ;)    i32.load
(;@156   ;)    call_indirect (type $fun_3_1)
             )
(;@15b   ;)  (func $__apply_3_2 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
(;@15c   ;)    local.get 0
(;@15e   ;)    i32.load offset=4
(;@161   ;)    local.get 0
(;@163   ;)    i32.load offset=8
(;@166   ;)    local.get 1
(;@168   ;)    local.get 0
(;@16a   ;)    i32.load
(;@16d   ;)    call_indirect (type $fun_3_1)
             )
(;@172   ;)  (func $__apply_4_3 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@173   ;)    local.get 0
(;@175   ;)    i32.load offset=4
(;@178   ;)    local.get 0
(;@17a   ;)    i32.load offset=8
(;@17d   ;)    local.get 0
(;@17f   ;)    i32.load offset=12
(;@182   ;)    local.get 1
(;@184   ;)    local.get 0
(;@186   ;)    i32.load
(;@189   ;)    call_indirect (type $fun_4_1)
             )
(;@18f   ;)  (func $main (;10;) (type $fun_0_1) (result i32)
(;@190   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@192   ;)    i32.const 0
(;@194   ;)    call $alloc
(;@196   ;)    local.set 0
(;@198   ;)    local.get 0
(;@19a   ;)    local.set 0
(;@19c   ;)    local.get 0
(;@19e   ;)    call $__mon_generate_marker
(;@1a0   ;)    local.set 1
(;@1a2   ;)    i32.const 12
(;@1a4   ;)    call $alloc
(;@1a6   ;)    local.set 2
(;@1a8   ;)    local.get 2
(;@1aa   ;)    i32.const 6
(;@1ac   ;)    i32.store
(;@1af   ;)    local.get 2
(;@1b1   ;)    i32.const 28
(;@1b3   ;)    i32.store offset=4
(;@1b6   ;)    local.get 2
(;@1b8   ;)    local.get 1
(;@1ba   ;)    i32.store offset=8
(;@1bd   ;)    local.get 2
(;@1bf   ;)    local.set 2
(;@1c1   ;)    i32.const 8
(;@1c3   ;)    call $alloc
(;@1c5   ;)    local.set 3
(;@1c7   ;)    local.get 3
(;@1c9   ;)    i32.const 4
(;@1cb   ;)    i32.store
(;@1ce   ;)    local.get 3
(;@1d0   ;)    i32.const 30
(;@1d2   ;)    i32.store offset=4
(;@1d5   ;)    local.get 3
(;@1d7   ;)    local.set 3
(;@1d9   ;)    i32.const 16
(;@1db   ;)    call $alloc
(;@1dd   ;)    local.set 4
(;@1df   ;)    local.get 4
(;@1e1   ;)    i32.const 8
(;@1e3   ;)    i32.store
(;@1e6   ;)    local.get 4
(;@1e8   ;)    i32.const 34
(;@1ea   ;)    i32.store offset=4
(;@1ed   ;)    local.get 4
(;@1ef   ;)    local.get 2
(;@1f1   ;)    i32.store offset=8
(;@1f4   ;)    local.get 4
(;@1f6   ;)    local.get 3
(;@1f8   ;)    i32.store offset=12
(;@1fb   ;)    local.get 4
(;@1fd   ;)    local.set 4
(;@1ff   ;)    i32.const 0
(;@201   ;)    call $alloc
(;@203   ;)    local.set 5
(;@205   ;)    local.get 5
(;@207   ;)    local.set 5
(;@209   ;)    local.get 4
(;@20b   ;)    i32.const 4
(;@20d   ;)    i32.add
(;@20e   ;)    local.get 5
(;@210   ;)    local.get 4
(;@212   ;)    i32.load
(;@215   ;)    call_indirect (type $fun_2_1)
(;@218   ;)    local.set 6
(;@21a   ;)    local.get 6
(;@21c   ;)    i32.load
(;@21f   ;)    local.set 7
(;@221   ;)    block (result i32) ;; label = @1
(;@223   ;)      block ;; label = @2
(;@225   ;)        block ;; label = @3
(;@227   ;)          block ;; label = @4
(;@229   ;)            local.get 7
(;@22b   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@230   ;)          end
(;@231   ;)          local.get 6
(;@233   ;)          i32.load offset=4
(;@236   ;)          local.set 8
(;@238   ;)          local.get 8
(;@23a   ;)          i32.load offset=4
(;@23d   ;)          local.set 9
(;@23f   ;)          i32.const 8
(;@241   ;)          call $alloc
(;@243   ;)          local.set 10
(;@245   ;)          local.get 10
(;@247   ;)          i32.const 0
(;@249   ;)          i32.store
(;@24c   ;)          local.get 10
(;@24e   ;)          local.get 9
(;@250   ;)          i32.store offset=4
(;@253   ;)          local.get 10
(;@255   ;)          br 2 (;@1;)
(;@257   ;)        end
(;@258   ;)        local.get 6
(;@25a   ;)        i32.load offset=4
(;@25d   ;)        local.set 10
(;@25f   ;)        local.get 10
(;@261   ;)        local.set 11
(;@263   ;)        local.get 11
(;@265   ;)        i32.load
(;@268   ;)        local.set 12
(;@26a   ;)        local.get 11
(;@26c   ;)        i32.load offset=4
(;@26f   ;)        local.set 13
(;@271   ;)        i32.const 12
(;@273   ;)        call $alloc
(;@275   ;)        local.set 14
(;@277   ;)        local.get 14
(;@279   ;)        i32.const 6
(;@27b   ;)        i32.store
(;@27e   ;)        local.get 14
(;@280   ;)        i32.const 33
(;@282   ;)        i32.store offset=4
(;@285   ;)        local.get 14
(;@287   ;)        local.get 11
(;@289   ;)        i32.store offset=8
(;@28c   ;)        local.get 14
(;@28e   ;)        local.set 14
(;@290   ;)        i32.const 12
(;@292   ;)        call $alloc
(;@294   ;)        local.set 15
(;@296   ;)        local.get 15
(;@298   ;)        local.get 12
(;@29a   ;)        i32.store
(;@29d   ;)        local.get 15
(;@29f   ;)        local.get 13
(;@2a1   ;)        i32.store offset=4
(;@2a4   ;)        local.get 15
(;@2a6   ;)        local.get 14
(;@2a8   ;)        i32.store offset=8
(;@2ab   ;)        local.get 15
(;@2ad   ;)        local.set 15
(;@2af   ;)        i32.const 8
(;@2b1   ;)        call $alloc
(;@2b3   ;)        local.set 16
(;@2b5   ;)        local.get 16
(;@2b7   ;)        i32.const 1
(;@2b9   ;)        i32.store
(;@2bc   ;)        local.get 16
(;@2be   ;)        local.get 15
(;@2c0   ;)        i32.store offset=4
(;@2c3   ;)        local.get 16
(;@2c5   ;)        br 1 (;@1;)
(;@2c7   ;)      end
(;@2c8   ;)      unreachable
(;@2c9   ;)    end
(;@2ca   ;)    local.set 16
(;@2cc   ;)    local.get 16
(;@2ce   ;)    i32.load
(;@2d1   ;)    local.set 17
(;@2d3   ;)    block (result i32) ;; label = @1
(;@2d5   ;)      block ;; label = @2
(;@2d7   ;)        block ;; label = @3
(;@2d9   ;)          block ;; label = @4
(;@2db   ;)            local.get 17
(;@2dd   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2e2   ;)          end
(;@2e3   ;)          local.get 16
(;@2e5   ;)          i32.load offset=4
(;@2e8   ;)          local.set 18
(;@2ea   ;)          local.get 18
(;@2ec   ;)          br 2 (;@1;)
(;@2ee   ;)        end
(;@2ef   ;)        local.get 16
(;@2f1   ;)        i32.load offset=4
(;@2f4   ;)        local.set 18
(;@2f6   ;)        i32.const 5467
(;@2f9   ;)        br 1 (;@1;)
(;@2fb   ;)      end
(;@2fc   ;)      unreachable
(;@2fd   ;)    end
(;@2fe   ;)    return
             )
(;@301   ;)  (func $main_lam_0 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@302   ;)    (local i32)
(;@304   ;)    local.get 0
(;@306   ;)    i32.const 4
(;@308   ;)    i32.add
(;@309   ;)    local.get 1
(;@30b   ;)    local.get 1
(;@30d   ;)    local.get 0
(;@30f   ;)    i32.load
(;@312   ;)    call_indirect (type $fun_3_1)
(;@315   ;)    return
             )
(;@318   ;)  (func $main_lam_1 (;12;) (type $fun_1_1) (param i32) (result i32)
(;@319   ;)    (local i32)
(;@31b   ;)    i32.const 12
(;@31d   ;)    call $alloc
(;@31f   ;)    local.set 1
(;@321   ;)    local.get 1
(;@323   ;)    i32.const 6
(;@325   ;)    i32.store
(;@328   ;)    local.get 1
(;@32a   ;)    i32.const 11
(;@32c   ;)    i32.store offset=4
(;@32f   ;)    local.get 1
(;@331   ;)    local.get 0
(;@333   ;)    i32.store offset=8
(;@336   ;)    local.get 1
(;@338   ;)    return
             )
(;@33b   ;)  (func $main_lam_2 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@33c   ;)    (local i32)
(;@33e   ;)    i32.const 8
(;@340   ;)    call $alloc
(;@342   ;)    local.set 2
(;@344   ;)    local.get 2
(;@346   ;)    i32.const 0
(;@348   ;)    i32.store
(;@34b   ;)    local.get 2
(;@34d   ;)    local.get 0
(;@34f   ;)    i32.store offset=4
(;@352   ;)    local.get 2
(;@354   ;)    return
             )
(;@357   ;)  (func $main_lam_3 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@358   ;)    (local i32 i32 i32 i32)
(;@35a   ;)    i32.const 8
(;@35c   ;)    call $alloc
(;@35e   ;)    local.set 2
(;@360   ;)    local.get 2
(;@362   ;)    i32.const 4
(;@364   ;)    i32.store
(;@367   ;)    local.get 2
(;@369   ;)    i32.const 12
(;@36b   ;)    i32.store offset=4
(;@36e   ;)    local.get 2
(;@370   ;)    local.set 2
(;@372   ;)    i32.const 8
(;@374   ;)    call $alloc
(;@376   ;)    local.set 3
(;@378   ;)    local.get 3
(;@37a   ;)    i32.const 5
(;@37c   ;)    i32.store
(;@37f   ;)    local.get 3
(;@381   ;)    i32.const 13
(;@383   ;)    i32.store offset=4
(;@386   ;)    local.get 3
(;@388   ;)    local.set 3
(;@38a   ;)    i32.const 12
(;@38c   ;)    call $alloc
(;@38e   ;)    local.set 4
(;@390   ;)    local.get 4
(;@392   ;)    local.get 0
(;@394   ;)    i32.store
(;@397   ;)    local.get 4
(;@399   ;)    local.get 2
(;@39b   ;)    i32.store offset=4
(;@39e   ;)    local.get 4
(;@3a0   ;)    local.get 3
(;@3a2   ;)    i32.store offset=8
(;@3a5   ;)    local.get 4
(;@3a7   ;)    local.set 4
(;@3a9   ;)    i32.const 8
(;@3ab   ;)    call $alloc
(;@3ad   ;)    local.set 5
(;@3af   ;)    local.get 5
(;@3b1   ;)    i32.const 1
(;@3b3   ;)    i32.store
(;@3b6   ;)    local.get 5
(;@3b8   ;)    local.get 4
(;@3ba   ;)    i32.store offset=4
(;@3bd   ;)    local.get 5
(;@3bf   ;)    return
             )
(;@3c2   ;)  (func $main_lam_4 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3c3   ;)    (local i32)
(;@3c5   ;)    i32.const 8
(;@3c7   ;)    call $alloc
(;@3c9   ;)    local.set 2
(;@3cb   ;)    local.get 2
(;@3cd   ;)    local.get 1
(;@3cf   ;)    i32.store
(;@3d2   ;)    local.get 2
(;@3d4   ;)    local.get 0
(;@3d6   ;)    i32.store offset=4
(;@3d9   ;)    local.get 2
(;@3db   ;)    return
             )
(;@3de   ;)  (func $main_lam_5 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3df   ;)    (local i32 i32)
(;@3e1   ;)    i32.const 12
(;@3e3   ;)    call $alloc
(;@3e5   ;)    local.set 2
(;@3e7   ;)    local.get 2
(;@3e9   ;)    i32.const 6
(;@3eb   ;)    i32.store
(;@3ee   ;)    local.get 2
(;@3f0   ;)    i32.const 15
(;@3f2   ;)    i32.store offset=4
(;@3f5   ;)    local.get 2
(;@3f7   ;)    local.get 0
(;@3f9   ;)    i32.store offset=8
(;@3fc   ;)    local.get 2
(;@3fe   ;)    local.set 2
(;@400   ;)    i32.const 8
(;@402   ;)    call $alloc
(;@404   ;)    local.set 3
(;@406   ;)    local.get 3
(;@408   ;)    i32.const 0
(;@40a   ;)    i32.store
(;@40d   ;)    local.get 3
(;@40f   ;)    local.get 2
(;@411   ;)    i32.store offset=4
(;@414   ;)    local.get 3
(;@416   ;)    return
             )
(;@419   ;)  (func $main_lam_6 (;17;) (type $fun_1_1) (param i32) (result i32)
(;@41a   ;)    (local i32)
(;@41c   ;)    i32.const 12
(;@41e   ;)    call $alloc
(;@420   ;)    local.set 1
(;@422   ;)    local.get 1
(;@424   ;)    i32.const 6
(;@426   ;)    i32.store
(;@429   ;)    local.get 1
(;@42b   ;)    i32.const 16
(;@42d   ;)    i32.store offset=4
(;@430   ;)    local.get 1
(;@432   ;)    local.get 0
(;@434   ;)    i32.store offset=8
(;@437   ;)    local.get 1
(;@439   ;)    return
             )
(;@43c   ;)  (func $main_lam_7 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@43d   ;)    (local i32)
(;@43f   ;)    local.get 1
(;@441   ;)    i32.const 4
(;@443   ;)    i32.add
(;@444   ;)    local.get 2
(;@446   ;)    local.get 2
(;@448   ;)    local.get 1
(;@44a   ;)    i32.load
(;@44d   ;)    call_indirect (type $fun_3_1)
(;@450   ;)    return
             )
(;@453   ;)  (func $main_lam_8 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@454   ;)    (local i32 i32)
(;@456   ;)    i32.const 0
(;@458   ;)    call $alloc
(;@45a   ;)    local.set 3
(;@45c   ;)    local.get 3
(;@45e   ;)    local.set 3
(;@460   ;)    local.get 1
(;@462   ;)    i32.const 4
(;@464   ;)    i32.add
(;@465   ;)    local.get 3
(;@467   ;)    local.get 0
(;@469   ;)    local.get 1
(;@46b   ;)    i32.load
(;@46e   ;)    call_indirect (type $fun_3_1)
(;@471   ;)    return
             )
(;@474   ;)  (func $main_lam_9 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@475   ;)    (local i32)
(;@477   ;)    local.get 1
(;@479   ;)    i32.const 4
(;@47b   ;)    i32.add
(;@47c   ;)    local.get 2
(;@47e   ;)    local.get 2
(;@480   ;)    local.get 1
(;@482   ;)    i32.load
(;@485   ;)    call_indirect (type $fun_3_1)
(;@488   ;)    return
             )
(;@48b   ;)  (func $main_lam_10 (;21;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@48c   ;)    (local i32 i32)
(;@48e   ;)    i32.const 0
(;@490   ;)    call $alloc
(;@492   ;)    local.set 3
(;@494   ;)    local.get 3
(;@496   ;)    local.set 3
(;@498   ;)    local.get 1
(;@49a   ;)    i32.const 4
(;@49c   ;)    i32.add
(;@49d   ;)    local.get 3
(;@49f   ;)    local.get 0
(;@4a1   ;)    local.get 1
(;@4a3   ;)    i32.load
(;@4a6   ;)    call_indirect (type $fun_3_1)
(;@4a9   ;)    return
             )
(;@4ac   ;)  (func $main_lam_11 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4ad   ;)    (local i32 i32 i32 i32)
(;@4af   ;)    i32.const 8
(;@4b1   ;)    call $alloc
(;@4b3   ;)    local.set 2
(;@4b5   ;)    local.get 2
(;@4b7   ;)    i32.const 7
(;@4b9   ;)    i32.store
(;@4bc   ;)    local.get 2
(;@4be   ;)    i32.const 20
(;@4c0   ;)    i32.store offset=4
(;@4c3   ;)    local.get 2
(;@4c5   ;)    local.set 2
(;@4c7   ;)    i32.const 8
(;@4c9   ;)    call $alloc
(;@4cb   ;)    local.set 3
(;@4cd   ;)    local.get 3
(;@4cf   ;)    i32.const 7
(;@4d1   ;)    i32.store
(;@4d4   ;)    local.get 3
(;@4d6   ;)    i32.const 21
(;@4d8   ;)    i32.store offset=4
(;@4db   ;)    local.get 3
(;@4dd   ;)    local.set 3
(;@4df   ;)    i32.const 8
(;@4e1   ;)    call $alloc
(;@4e3   ;)    local.set 4
(;@4e5   ;)    local.get 4
(;@4e7   ;)    local.get 2
(;@4e9   ;)    i32.store
(;@4ec   ;)    local.get 4
(;@4ee   ;)    local.get 3
(;@4f0   ;)    i32.store offset=4
(;@4f3   ;)    local.get 4
(;@4f5   ;)    local.set 4
(;@4f7   ;)    i32.const 8
(;@4f9   ;)    call $alloc
(;@4fb   ;)    local.set 5
(;@4fd   ;)    local.get 5
(;@4ff   ;)    local.get 0
(;@501   ;)    i32.store
(;@504   ;)    local.get 5
(;@506   ;)    local.get 4
(;@508   ;)    i32.store offset=4
(;@50b   ;)    local.get 5
(;@50d   ;)    return
             )
(;@510   ;)  (func $main_lam_12 (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@511   ;)    (local i32 i32 i32 i32)
(;@513   ;)    i32.const 12
(;@515   ;)    call $alloc
(;@517   ;)    local.set 3
(;@519   ;)    local.get 3
(;@51b   ;)    i32.const 6
(;@51d   ;)    i32.store
(;@520   ;)    local.get 3
(;@522   ;)    i32.const 22
(;@524   ;)    i32.store offset=4
(;@527   ;)    local.get 3
(;@529   ;)    local.get 0
(;@52b   ;)    i32.store offset=8
(;@52e   ;)    local.get 3
(;@530   ;)    local.set 3
(;@532   ;)    local.get 1
(;@534   ;)    i32.load offset=8
(;@537   ;)    local.set 4
(;@539   ;)    local.get 4
(;@53b   ;)    i32.const 4
(;@53d   ;)    i32.add
(;@53e   ;)    local.get 2
(;@540   ;)    local.get 4
(;@542   ;)    i32.load
(;@545   ;)    call_indirect (type $fun_2_1)
(;@548   ;)    local.set 5
(;@54a   ;)    i32.const 20
(;@54c   ;)    call $alloc
(;@54e   ;)    local.set 6
(;@550   ;)    local.get 6
(;@552   ;)    i32.const 9
(;@554   ;)    i32.store
(;@557   ;)    local.get 6
(;@559   ;)    i32.const 36
(;@55b   ;)    i32.store offset=4
(;@55e   ;)    local.get 6
(;@560   ;)    local.get 0
(;@562   ;)    i32.store offset=8
(;@565   ;)    local.get 6
(;@567   ;)    local.get 3
(;@569   ;)    i32.store offset=12
(;@56c   ;)    local.get 6
(;@56e   ;)    local.get 5
(;@570   ;)    i32.store offset=16
(;@573   ;)    local.get 6
(;@575   ;)    return
             )
(;@578   ;)  (func $main_lam_13 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@579   ;)    (local i32)
(;@57b   ;)    local.get 1
(;@57d   ;)    i32.const 4
(;@57f   ;)    i32.add
(;@580   ;)    local.get 2
(;@582   ;)    local.get 2
(;@584   ;)    local.get 1
(;@586   ;)    i32.load
(;@589   ;)    call_indirect (type $fun_3_1)
(;@58c   ;)    return
             )
(;@58f   ;)  (func $main_lam_14 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@590   ;)    (local i32 i32)
(;@592   ;)    i32.const 0
(;@594   ;)    call $alloc
(;@596   ;)    local.set 3
(;@598   ;)    local.get 3
(;@59a   ;)    local.set 3
(;@59c   ;)    local.get 1
(;@59e   ;)    i32.const 4
(;@5a0   ;)    i32.add
(;@5a1   ;)    local.get 3
(;@5a3   ;)    local.get 0
(;@5a5   ;)    local.get 1
(;@5a7   ;)    i32.load
(;@5aa   ;)    call_indirect (type $fun_3_1)
(;@5ad   ;)    return
             )
(;@5b0   ;)  (func $main_lam_15 (;26;) (type $fun_2_1) (param i32 i32) (result i32)
(;@5b1   ;)    (local i32 i32 i32 i32)
(;@5b3   ;)    i32.const 8
(;@5b5   ;)    call $alloc
(;@5b7   ;)    local.set 2
(;@5b9   ;)    local.get 2
(;@5bb   ;)    i32.const 7
(;@5bd   ;)    i32.store
(;@5c0   ;)    local.get 2
(;@5c2   ;)    i32.const 24
(;@5c4   ;)    i32.store offset=4
(;@5c7   ;)    local.get 2
(;@5c9   ;)    local.set 2
(;@5cb   ;)    i32.const 8
(;@5cd   ;)    call $alloc
(;@5cf   ;)    local.set 3
(;@5d1   ;)    local.get 3
(;@5d3   ;)    i32.const 7
(;@5d5   ;)    i32.store
(;@5d8   ;)    local.get 3
(;@5da   ;)    i32.const 25
(;@5dc   ;)    i32.store offset=4
(;@5df   ;)    local.get 3
(;@5e1   ;)    local.set 3
(;@5e3   ;)    i32.const 8
(;@5e5   ;)    call $alloc
(;@5e7   ;)    local.set 4
(;@5e9   ;)    local.get 4
(;@5eb   ;)    local.get 2
(;@5ed   ;)    i32.store
(;@5f0   ;)    local.get 4
(;@5f2   ;)    local.get 3
(;@5f4   ;)    i32.store offset=4
(;@5f7   ;)    local.get 4
(;@5f9   ;)    local.set 4
(;@5fb   ;)    i32.const 8
(;@5fd   ;)    call $alloc
(;@5ff   ;)    local.set 5
(;@601   ;)    local.get 5
(;@603   ;)    local.get 0
(;@605   ;)    i32.store
(;@608   ;)    local.get 5
(;@60a   ;)    local.get 4
(;@60c   ;)    i32.store offset=4
(;@60f   ;)    local.get 5
(;@611   ;)    return
             )
(;@614   ;)  (func $main_lam_16 (;27;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@615   ;)    (local i32 i32 i32 i32)
(;@617   ;)    i32.const 12
(;@619   ;)    call $alloc
(;@61b   ;)    local.set 3
(;@61d   ;)    local.get 3
(;@61f   ;)    i32.const 6
(;@621   ;)    i32.store
(;@624   ;)    local.get 3
(;@626   ;)    i32.const 26
(;@628   ;)    i32.store offset=4
(;@62b   ;)    local.get 3
(;@62d   ;)    local.get 0
(;@62f   ;)    i32.store offset=8
(;@632   ;)    local.get 3
(;@634   ;)    local.set 3
(;@636   ;)    local.get 1
(;@638   ;)    i32.load offset=8
(;@63b   ;)    local.set 4
(;@63d   ;)    local.get 4
(;@63f   ;)    i32.const 4
(;@641   ;)    i32.add
(;@642   ;)    local.get 2
(;@644   ;)    local.get 4
(;@646   ;)    i32.load
(;@649   ;)    call_indirect (type $fun_2_1)
(;@64c   ;)    local.set 5
(;@64e   ;)    i32.const 20
(;@650   ;)    call $alloc
(;@652   ;)    local.set 6
(;@654   ;)    local.get 6
(;@656   ;)    i32.const 9
(;@658   ;)    i32.store
(;@65b   ;)    local.get 6
(;@65d   ;)    i32.const 36
(;@65f   ;)    i32.store offset=4
(;@662   ;)    local.get 6
(;@664   ;)    local.get 0
(;@666   ;)    i32.store offset=8
(;@669   ;)    local.get 6
(;@66b   ;)    local.get 3
(;@66d   ;)    i32.store offset=12
(;@670   ;)    local.get 6
(;@672   ;)    local.get 5
(;@674   ;)    i32.store offset=16
(;@677   ;)    local.get 6
(;@679   ;)    return
             )
(;@67d   ;)  (func $main_lam_17 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
(;@67e   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@680   ;)    i32.const 12
(;@682   ;)    call $alloc
(;@684   ;)    local.set 2
(;@686   ;)    local.get 2
(;@688   ;)    i32.const 6
(;@68a   ;)    i32.store
(;@68d   ;)    local.get 2
(;@68f   ;)    i32.const 14
(;@691   ;)    i32.store offset=4
(;@694   ;)    local.get 2
(;@696   ;)    local.get 0
(;@698   ;)    i32.store offset=8
(;@69b   ;)    local.get 2
(;@69d   ;)    local.set 2
(;@69f   ;)    i32.const 8
(;@6a1   ;)    call $alloc
(;@6a3   ;)    local.set 3
(;@6a5   ;)    local.get 3
(;@6a7   ;)    i32.const 4
(;@6a9   ;)    i32.store
(;@6ac   ;)    local.get 3
(;@6ae   ;)    i32.const 17
(;@6b0   ;)    i32.store offset=4
(;@6b3   ;)    local.get 3
(;@6b5   ;)    local.set 3
(;@6b7   ;)    i32.const 8
(;@6b9   ;)    call $alloc
(;@6bb   ;)    local.set 4
(;@6bd   ;)    local.get 4
(;@6bf   ;)    i32.const 7
(;@6c1   ;)    i32.store
(;@6c4   ;)    local.get 4
(;@6c6   ;)    i32.const 18
(;@6c8   ;)    i32.store offset=4
(;@6cb   ;)    local.get 4
(;@6cd   ;)    local.set 4
(;@6cf   ;)    i32.const 8
(;@6d1   ;)    call $alloc
(;@6d3   ;)    local.set 5
(;@6d5   ;)    local.get 5
(;@6d7   ;)    i32.const 7
(;@6d9   ;)    i32.store
(;@6dc   ;)    local.get 5
(;@6de   ;)    i32.const 19
(;@6e0   ;)    i32.store offset=4
(;@6e3   ;)    local.get 5
(;@6e5   ;)    local.set 5
(;@6e7   ;)    i32.const 8
(;@6e9   ;)    call $alloc
(;@6eb   ;)    local.set 6
(;@6ed   ;)    local.get 6
(;@6ef   ;)    local.get 4
(;@6f1   ;)    i32.store
(;@6f4   ;)    local.get 6
(;@6f6   ;)    local.get 5
(;@6f8   ;)    i32.store offset=4
(;@6fb   ;)    local.get 6
(;@6fd   ;)    local.set 6
(;@6ff   ;)    i32.const 8
(;@701   ;)    call $alloc
(;@703   ;)    local.set 7
(;@705   ;)    local.get 7
(;@707   ;)    local.get 0
(;@709   ;)    i32.store
(;@70c   ;)    local.get 7
(;@70e   ;)    local.get 6
(;@710   ;)    i32.store offset=4
(;@713   ;)    local.get 7
(;@715   ;)    local.set 7
(;@717   ;)    local.get 2
(;@719   ;)    local.get 3
(;@71b   ;)    local.get 7
(;@71d   ;)    call $__mon_bind
(;@71f   ;)    local.set 8
(;@721   ;)    local.get 8
(;@723   ;)    i32.load
(;@726   ;)    local.set 9
(;@728   ;)    block (result i32) ;; label = @1
(;@72a   ;)      block ;; label = @2
(;@72c   ;)        block ;; label = @3
(;@72e   ;)          block ;; label = @4
(;@730   ;)            local.get 9
(;@732   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@737   ;)          end
(;@738   ;)          local.get 8
(;@73a   ;)          i32.load offset=4
(;@73d   ;)          local.set 10
(;@73f   ;)          i32.const 8
(;@741   ;)          call $alloc
(;@743   ;)          local.set 11
(;@745   ;)          local.get 11
(;@747   ;)          i32.const 0
(;@749   ;)          i32.store
(;@74c   ;)          local.get 11
(;@74e   ;)          local.get 10
(;@750   ;)          i32.store offset=4
(;@753   ;)          local.get 11
(;@755   ;)          br 2 (;@1;)
(;@757   ;)        end
(;@758   ;)        local.get 8
(;@75a   ;)        i32.load offset=4
(;@75d   ;)        local.set 11
(;@75f   ;)        local.get 11
(;@761   ;)        i32.load
(;@764   ;)        local.set 12
(;@766   ;)        local.get 0
(;@768   ;)        local.get 12
(;@76a   ;)        call $__mon_eqm
(;@76c   ;)        local.set 13
(;@76e   ;)        local.get 13
(;@770   ;)        i32.load
(;@773   ;)        local.set 14
(;@775   ;)        block (result i32) ;; label = @3
(;@777   ;)          block ;; label = @4
(;@779   ;)            block ;; label = @5
(;@77b   ;)              block ;; label = @6
(;@77d   ;)                local.get 14
(;@77f   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@784   ;)              end
(;@785   ;)              local.get 13
(;@787   ;)              i32.load offset=4
(;@78a   ;)              local.set 15
(;@78c   ;)              local.get 11
(;@78e   ;)              i32.load
(;@791   ;)              local.set 16
(;@793   ;)              local.get 11
(;@795   ;)              i32.load offset=4
(;@798   ;)              local.set 17
(;@79a   ;)              i32.const 16
(;@79c   ;)              call $alloc
(;@79e   ;)              local.set 18
(;@7a0   ;)              local.get 18
(;@7a2   ;)              i32.const 8
(;@7a4   ;)              i32.store
(;@7a7   ;)              local.get 18
(;@7a9   ;)              i32.const 23
(;@7ab   ;)              i32.store offset=4
(;@7ae   ;)              local.get 18
(;@7b0   ;)              local.get 0
(;@7b2   ;)              i32.store offset=8
(;@7b5   ;)              local.get 18
(;@7b7   ;)              local.get 11
(;@7b9   ;)              i32.store offset=12
(;@7bc   ;)              local.get 18
(;@7be   ;)              local.set 18
(;@7c0   ;)              local.get 11
(;@7c2   ;)              i32.load offset=8
(;@7c5   ;)              local.set 19
(;@7c7   ;)              i32.const 16
(;@7c9   ;)              call $alloc
(;@7cb   ;)              local.set 20
(;@7cd   ;)              local.get 20
(;@7cf   ;)              local.get 16
(;@7d1   ;)              i32.store
(;@7d4   ;)              local.get 20
(;@7d6   ;)              local.get 17
(;@7d8   ;)              i32.store offset=4
(;@7db   ;)              local.get 20
(;@7dd   ;)              local.get 18
(;@7df   ;)              i32.store offset=8
(;@7e2   ;)              local.get 20
(;@7e4   ;)              local.get 19
(;@7e6   ;)              i32.store offset=12
(;@7e9   ;)              local.get 20
(;@7eb   ;)              local.set 20
(;@7ed   ;)              i32.const 8
(;@7ef   ;)              call $alloc
(;@7f1   ;)              local.set 21
(;@7f3   ;)              local.get 21
(;@7f5   ;)              i32.const 1
(;@7f7   ;)              i32.store
(;@7fa   ;)              local.get 21
(;@7fc   ;)              local.get 20
(;@7fe   ;)              i32.store offset=4
(;@801   ;)              local.get 21
(;@803   ;)              br 2 (;@3;)
(;@805   ;)            end
(;@806   ;)            local.get 13
(;@808   ;)            i32.load offset=4
(;@80b   ;)            local.set 15
(;@80d   ;)            i32.const 16
(;@80f   ;)            call $alloc
(;@811   ;)            local.set 21
(;@813   ;)            local.get 21
(;@815   ;)            i32.const 8
(;@817   ;)            i32.store
(;@81a   ;)            local.get 21
(;@81c   ;)            i32.const 27
(;@81e   ;)            i32.store offset=4
(;@821   ;)            local.get 21
(;@823   ;)            local.get 0
(;@825   ;)            i32.store offset=8
(;@828   ;)            local.get 21
(;@82a   ;)            local.get 11
(;@82c   ;)            i32.store offset=12
(;@82f   ;)            local.get 21
(;@831   ;)            local.set 21
(;@833   ;)            local.get 11
(;@835   ;)            i32.load offset=4
(;@838   ;)            local.set 22
(;@83a   ;)            local.get 22
(;@83c   ;)            i32.const 4
(;@83e   ;)            i32.add
(;@83f   ;)            local.get 21
(;@841   ;)            local.get 1
(;@843   ;)            local.get 22
(;@845   ;)            i32.load
(;@848   ;)            call_indirect (type $fun_3_1)
(;@84b   ;)            br 1 (;@3;)
(;@84d   ;)          end
(;@84e   ;)          unreachable
(;@84f   ;)        end
(;@850   ;)        br 1 (;@1;)
(;@852   ;)      end
(;@853   ;)      unreachable
(;@854   ;)    end
(;@855   ;)    return
             )
(;@858   ;)  (func $main_lam_18 (;29;) (type $fun_2_1) (param i32 i32) (result i32)
(;@859   ;)    (local i32)
(;@85b   ;)    i32.const 8
(;@85d   ;)    call $alloc
(;@85f   ;)    local.set 2
(;@861   ;)    local.get 2
(;@863   ;)    i32.const 0
(;@865   ;)    i32.store
(;@868   ;)    local.get 2
(;@86a   ;)    local.get 0
(;@86c   ;)    i32.store offset=4
(;@86f   ;)    local.get 2
(;@871   ;)    return
             )
(;@874   ;)  (func $main_lam_19 (;30;) (type $fun_1_1) (param i32) (result i32)
(;@875   ;)    (local i32 i32)
(;@877   ;)    local.get 0
(;@879   ;)    i32.const 4
(;@87b   ;)    i32.add
(;@87c   ;)    i32.const 825
(;@87f   ;)    local.get 0
(;@881   ;)    i32.load
(;@884   ;)    call_indirect (type $fun_2_1)
(;@887   ;)    local.set 1
(;@889   ;)    i32.const 12
(;@88b   ;)    call $alloc
(;@88d   ;)    local.set 2
(;@88f   ;)    local.get 2
(;@891   ;)    i32.const 6
(;@893   ;)    i32.store
(;@896   ;)    local.get 2
(;@898   ;)    i32.const 29
(;@89a   ;)    i32.store offset=4
(;@89d   ;)    local.get 2
(;@89f   ;)    local.get 1
(;@8a1   ;)    i32.store offset=8
(;@8a4   ;)    local.get 2
(;@8a6   ;)    return
             )
(;@8a9   ;)  (func $main_lam_20 (;31;) (type $fun_2_1) (param i32 i32) (result i32)
(;@8aa   ;)    (local i32 i32)
(;@8ac   ;)    local.get 0
(;@8ae   ;)    i32.load offset=4
(;@8b1   ;)    local.set 2
(;@8b3   ;)    i32.const 8
(;@8b5   ;)    call $alloc
(;@8b7   ;)    local.set 3
(;@8b9   ;)    local.get 3
(;@8bb   ;)    i32.const 0
(;@8bd   ;)    i32.store
(;@8c0   ;)    local.get 3
(;@8c2   ;)    local.get 2
(;@8c4   ;)    i32.store offset=4
(;@8c7   ;)    local.get 3
(;@8c9   ;)    return
             )
(;@8cc   ;)  (func $main_lam_21 (;32;) (type $fun_1_1) (param i32) (result i32)
(;@8cd   ;)    (local i32)
(;@8cf   ;)    i32.const 12
(;@8d1   ;)    call $alloc
(;@8d3   ;)    local.set 1
(;@8d5   ;)    local.get 1
(;@8d7   ;)    i32.const 6
(;@8d9   ;)    i32.store
(;@8dc   ;)    local.get 1
(;@8de   ;)    i32.const 31
(;@8e0   ;)    i32.store offset=4
(;@8e3   ;)    local.get 1
(;@8e5   ;)    local.get 0
(;@8e7   ;)    i32.store offset=8
(;@8ea   ;)    local.get 1
(;@8ec   ;)    return
             )
(;@8ef   ;)  (func $main_lam_22 (;33;) (type $fun_2_1) (param i32 i32) (result i32)
(;@8f0   ;)    (local i32 i32 i32 i32)
(;@8f2   ;)    local.get 0
(;@8f4   ;)    i32.load offset=8
(;@8f7   ;)    local.set 2
(;@8f9   ;)    local.get 2
(;@8fb   ;)    i32.const 4
(;@8fd   ;)    i32.add
(;@8fe   ;)    local.get 1
(;@900   ;)    local.get 2
(;@902   ;)    i32.load
(;@905   ;)    call_indirect (type $fun_2_1)
(;@908   ;)    local.set 3
(;@90a   ;)    i32.const 8
(;@90c   ;)    call $alloc
(;@90e   ;)    local.set 4
(;@910   ;)    local.get 4
(;@912   ;)    i32.const 4
(;@914   ;)    i32.store
(;@917   ;)    local.get 4
(;@919   ;)    i32.const 32
(;@91b   ;)    i32.store offset=4
(;@91e   ;)    local.get 4
(;@920   ;)    local.set 4
(;@922   ;)    i32.const 16
(;@924   ;)    call $alloc
(;@926   ;)    local.set 5
(;@928   ;)    local.get 5
(;@92a   ;)    i32.const 8
(;@92c   ;)    i32.store
(;@92f   ;)    local.get 5
(;@931   ;)    i32.const 34
(;@933   ;)    i32.store offset=4
(;@936   ;)    local.get 5
(;@938   ;)    local.get 3
(;@93a   ;)    i32.store offset=8
(;@93d   ;)    local.get 5
(;@93f   ;)    local.get 4
(;@941   ;)    i32.store offset=12
(;@944   ;)    local.get 5
(;@946   ;)    return
             )
(;@94a   ;)  (func $__mon_bind (;34;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@94b   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@94d   ;)    local.get 0
(;@94f   ;)    i32.const 4
(;@951   ;)    i32.add
(;@952   ;)    local.get 2
(;@954   ;)    local.get 0
(;@956   ;)    i32.load
(;@959   ;)    call_indirect (type $fun_2_1)
(;@95c   ;)    local.set 3
(;@95e   ;)    local.get 3
(;@960   ;)    i32.load
(;@963   ;)    local.set 4
(;@965   ;)    block (result i32) ;; label = @1
(;@967   ;)      block ;; label = @2
(;@969   ;)        block ;; label = @3
(;@96b   ;)          block ;; label = @4
(;@96d   ;)            local.get 4
(;@96f   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@974   ;)          end
(;@975   ;)          local.get 3
(;@977   ;)          i32.load offset=4
(;@97a   ;)          local.set 5
(;@97c   ;)          local.get 1
(;@97e   ;)          i32.const 4
(;@980   ;)          i32.add
(;@981   ;)          local.get 5
(;@983   ;)          local.get 2
(;@985   ;)          local.get 1
(;@987   ;)          i32.load
(;@98a   ;)          call_indirect (type $fun_3_1)
(;@98d   ;)          br 2 (;@1;)
(;@98f   ;)        end
(;@990   ;)        local.get 3
(;@992   ;)        i32.load offset=4
(;@995   ;)        local.set 6
(;@997   ;)        local.get 6
(;@999   ;)        local.set 7
(;@99b   ;)        local.get 7
(;@99d   ;)        i32.load
(;@9a0   ;)        local.set 8
(;@9a2   ;)        local.get 7
(;@9a4   ;)        i32.load offset=4
(;@9a7   ;)        local.set 9
(;@9a9   ;)        i32.const 16
(;@9ab   ;)        call $alloc
(;@9ad   ;)        local.set 10
(;@9af   ;)        local.get 10
(;@9b1   ;)        i32.const 8
(;@9b3   ;)        i32.store
(;@9b6   ;)        local.get 10
(;@9b8   ;)        i32.const 35
(;@9ba   ;)        i32.store offset=4
(;@9bd   ;)        local.get 10
(;@9bf   ;)        local.get 1
(;@9c1   ;)        i32.store offset=8
(;@9c4   ;)        local.get 10
(;@9c6   ;)        local.get 7
(;@9c8   ;)        i32.store offset=12
(;@9cb   ;)        local.get 10
(;@9cd   ;)        local.set 10
(;@9cf   ;)        i32.const 12
(;@9d1   ;)        call $alloc
(;@9d3   ;)        local.set 11
(;@9d5   ;)        local.get 11
(;@9d7   ;)        local.get 8
(;@9d9   ;)        i32.store
(;@9dc   ;)        local.get 11
(;@9de   ;)        local.get 9
(;@9e0   ;)        i32.store offset=4
(;@9e3   ;)        local.get 11
(;@9e5   ;)        local.get 10
(;@9e7   ;)        i32.store offset=8
(;@9ea   ;)        local.get 11
(;@9ec   ;)        local.set 11
(;@9ee   ;)        i32.const 8
(;@9f0   ;)        call $alloc
(;@9f2   ;)        local.set 12
(;@9f4   ;)        local.get 12
(;@9f6   ;)        i32.const 1
(;@9f8   ;)        i32.store
(;@9fb   ;)        local.get 12
(;@9fd   ;)        local.get 11
(;@9ff   ;)        i32.store offset=4
(;@a02   ;)        local.get 12
(;@a04   ;)        br 1 (;@1;)
(;@a06   ;)      end
(;@a07   ;)      unreachable
(;@a08   ;)    end
(;@a09   ;)    return
             )
(;@a0c   ;)  (func $__mon_bind_lam_0 (;35;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@a0d   ;)    (local i32 i32 i32)
(;@a0f   ;)    local.get 1
(;@a11   ;)    i32.load offset=8
(;@a14   ;)    local.set 3
(;@a16   ;)    local.get 3
(;@a18   ;)    i32.const 4
(;@a1a   ;)    i32.add
(;@a1b   ;)    local.get 2
(;@a1d   ;)    local.get 3
(;@a1f   ;)    i32.load
(;@a22   ;)    call_indirect (type $fun_2_1)
(;@a25   ;)    local.set 4
(;@a27   ;)    i32.const 16
(;@a29   ;)    call $alloc
(;@a2b   ;)    local.set 5
(;@a2d   ;)    local.get 5
(;@a2f   ;)    i32.const 8
(;@a31   ;)    i32.store
(;@a34   ;)    local.get 5
(;@a36   ;)    i32.const 34
(;@a38   ;)    i32.store offset=4
(;@a3b   ;)    local.get 5
(;@a3d   ;)    local.get 4
(;@a3f   ;)    i32.store offset=8
(;@a42   ;)    local.get 5
(;@a44   ;)    local.get 0
(;@a46   ;)    i32.store offset=12
(;@a49   ;)    local.get 5
(;@a4b   ;)    return
             )
(;@a4f   ;)  (func $__mon_prompt (;36;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@a50   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@a52   ;)    local.get 1
(;@a54   ;)    i32.const 4
(;@a56   ;)    i32.add
(;@a57   ;)    local.get 3
(;@a59   ;)    local.get 1
(;@a5b   ;)    i32.load
(;@a5e   ;)    call_indirect (type $fun_2_1)
(;@a61   ;)    local.set 4
(;@a63   ;)    local.get 2
(;@a65   ;)    i32.const 4
(;@a67   ;)    i32.add
(;@a68   ;)    local.get 4
(;@a6a   ;)    local.get 2
(;@a6c   ;)    i32.load
(;@a6f   ;)    call_indirect (type $fun_2_1)
(;@a72   ;)    local.set 5
(;@a74   ;)    local.get 5
(;@a76   ;)    i32.load
(;@a79   ;)    local.set 6
(;@a7b   ;)    block (result i32) ;; label = @1
(;@a7d   ;)      block ;; label = @2
(;@a7f   ;)        block ;; label = @3
(;@a81   ;)          block ;; label = @4
(;@a83   ;)            local.get 6
(;@a85   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@a8a   ;)          end
(;@a8b   ;)          local.get 5
(;@a8d   ;)          i32.load offset=4
(;@a90   ;)          local.set 7
(;@a92   ;)          i32.const 8
(;@a94   ;)          call $alloc
(;@a96   ;)          local.set 8
(;@a98   ;)          local.get 8
(;@a9a   ;)          i32.const 0
(;@a9c   ;)          i32.store
(;@a9f   ;)          local.get 8
(;@aa1   ;)          local.get 7
(;@aa3   ;)          i32.store offset=4
(;@aa6   ;)          local.get 8
(;@aa8   ;)          br 2 (;@1;)
(;@aaa   ;)        end
(;@aab   ;)        local.get 5
(;@aad   ;)        i32.load offset=4
(;@ab0   ;)        local.set 8
(;@ab2   ;)        local.get 8
(;@ab4   ;)        i32.load
(;@ab7   ;)        local.set 9
(;@ab9   ;)        local.get 0
(;@abb   ;)        local.get 9
(;@abd   ;)        call $__mon_eqm
(;@abf   ;)        local.set 10
(;@ac1   ;)        local.get 10
(;@ac3   ;)        i32.load
(;@ac6   ;)        local.set 11
(;@ac8   ;)        block (result i32) ;; label = @3
(;@aca   ;)          block ;; label = @4
(;@acc   ;)            block ;; label = @5
(;@ace   ;)              block ;; label = @6
(;@ad0   ;)                local.get 11
(;@ad2   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@ad7   ;)              end
(;@ad8   ;)              local.get 10
(;@ada   ;)              i32.load offset=4
(;@add   ;)              local.set 12
(;@adf   ;)              local.get 8
(;@ae1   ;)              i32.load
(;@ae4   ;)              local.set 13
(;@ae6   ;)              local.get 8
(;@ae8   ;)              i32.load offset=4
(;@aeb   ;)              local.set 14
(;@aed   ;)              i32.const 20
(;@aef   ;)              call $alloc
(;@af1   ;)              local.set 15
(;@af3   ;)              local.get 15
(;@af5   ;)              i32.const 9
(;@af7   ;)              i32.store
(;@afa   ;)              local.get 15
(;@afc   ;)              i32.const 37
(;@afe   ;)              i32.store offset=4
(;@b01   ;)              local.get 15
(;@b03   ;)              local.get 0
(;@b05   ;)              i32.store offset=8
(;@b08   ;)              local.get 15
(;@b0a   ;)              local.get 1
(;@b0c   ;)              i32.store offset=12
(;@b0f   ;)              local.get 15
(;@b11   ;)              local.get 8
(;@b13   ;)              i32.store offset=16
(;@b16   ;)              local.get 15
(;@b18   ;)              local.set 15
(;@b1a   ;)              local.get 8
(;@b1c   ;)              i32.load offset=8
(;@b1f   ;)              local.set 16
(;@b21   ;)              i32.const 16
(;@b23   ;)              call $alloc
(;@b25   ;)              local.set 17
(;@b27   ;)              local.get 17
(;@b29   ;)              local.get 13
(;@b2b   ;)              i32.store
(;@b2e   ;)              local.get 17
(;@b30   ;)              local.get 14
(;@b32   ;)              i32.store offset=4
(;@b35   ;)              local.get 17
(;@b37   ;)              local.get 15
(;@b39   ;)              i32.store offset=8
(;@b3c   ;)              local.get 17
(;@b3e   ;)              local.get 16
(;@b40   ;)              i32.store offset=12
(;@b43   ;)              local.get 17
(;@b45   ;)              local.set 17
(;@b47   ;)              i32.const 8
(;@b49   ;)              call $alloc
(;@b4b   ;)              local.set 18
(;@b4d   ;)              local.get 18
(;@b4f   ;)              i32.const 1
(;@b51   ;)              i32.store
(;@b54   ;)              local.get 18
(;@b56   ;)              local.get 17
(;@b58   ;)              i32.store offset=4
(;@b5b   ;)              local.get 18
(;@b5d   ;)              br 2 (;@3;)
(;@b5f   ;)            end
(;@b60   ;)            local.get 10
(;@b62   ;)            i32.load offset=4
(;@b65   ;)            local.set 12
(;@b67   ;)            i32.const 20
(;@b69   ;)            call $alloc
(;@b6b   ;)            local.set 18
(;@b6d   ;)            local.get 18
(;@b6f   ;)            i32.const 9
(;@b71   ;)            i32.store
(;@b74   ;)            local.get 18
(;@b76   ;)            i32.const 38
(;@b78   ;)            i32.store offset=4
(;@b7b   ;)            local.get 18
(;@b7d   ;)            local.get 0
(;@b7f   ;)            i32.store offset=8
(;@b82   ;)            local.get 18
(;@b84   ;)            local.get 1
(;@b86   ;)            i32.store offset=12
(;@b89   ;)            local.get 18
(;@b8b   ;)            local.get 8
(;@b8d   ;)            i32.store offset=16
(;@b90   ;)            local.get 18
(;@b92   ;)            local.set 18
(;@b94   ;)            local.get 8
(;@b96   ;)            i32.load offset=4
(;@b99   ;)            local.set 19
(;@b9b   ;)            local.get 19
(;@b9d   ;)            i32.const 4
(;@b9f   ;)            i32.add
(;@ba0   ;)            local.get 18
(;@ba2   ;)            local.get 3
(;@ba4   ;)            local.get 19
(;@ba6   ;)            i32.load
(;@ba9   ;)            call_indirect (type $fun_3_1)
(;@bac   ;)            br 1 (;@3;)
(;@bae   ;)          end
(;@baf   ;)          unreachable
(;@bb0   ;)        end
(;@bb1   ;)        br 1 (;@1;)
(;@bb3   ;)      end
(;@bb4   ;)      unreachable
(;@bb5   ;)    end
(;@bb6   ;)    return
             )
(;@bb9   ;)  (func $__mon_prompt_lam_0 (;37;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@bba   ;)    (local i32 i32 i32)
(;@bbc   ;)    local.get 2
(;@bbe   ;)    i32.load offset=8
(;@bc1   ;)    local.set 4
(;@bc3   ;)    local.get 4
(;@bc5   ;)    i32.const 4
(;@bc7   ;)    i32.add
(;@bc8   ;)    local.get 3
(;@bca   ;)    local.get 4
(;@bcc   ;)    i32.load
(;@bcf   ;)    call_indirect (type $fun_2_1)
(;@bd2   ;)    local.set 5
(;@bd4   ;)    i32.const 20
(;@bd6   ;)    call $alloc
(;@bd8   ;)    local.set 6
(;@bda   ;)    local.get 6
(;@bdc   ;)    i32.const 9
(;@bde   ;)    i32.store
(;@be1   ;)    local.get 6
(;@be3   ;)    i32.const 36
(;@be5   ;)    i32.store offset=4
(;@be8   ;)    local.get 6
(;@bea   ;)    local.get 0
(;@bec   ;)    i32.store offset=8
(;@bef   ;)    local.get 6
(;@bf1   ;)    local.get 1
(;@bf3   ;)    i32.store offset=12
(;@bf6   ;)    local.get 6
(;@bf8   ;)    local.get 5
(;@bfa   ;)    i32.store offset=16
(;@bfd   ;)    local.get 6
(;@bff   ;)    return
             )
(;@c02   ;)  (func $__mon_prompt_lam_1 (;38;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@c03   ;)    (local i32 i32 i32)
(;@c05   ;)    local.get 2
(;@c07   ;)    i32.load offset=8
(;@c0a   ;)    local.set 4
(;@c0c   ;)    local.get 4
(;@c0e   ;)    i32.const 4
(;@c10   ;)    i32.add
(;@c11   ;)    local.get 3
(;@c13   ;)    local.get 4
(;@c15   ;)    i32.load
(;@c18   ;)    call_indirect (type $fun_2_1)
(;@c1b   ;)    local.set 5
(;@c1d   ;)    i32.const 20
(;@c1f   ;)    call $alloc
(;@c21   ;)    local.set 6
(;@c23   ;)    local.get 6
(;@c25   ;)    i32.const 9
(;@c27   ;)    i32.store
(;@c2a   ;)    local.get 6
(;@c2c   ;)    i32.const 36
(;@c2e   ;)    i32.store offset=4
(;@c31   ;)    local.get 6
(;@c33   ;)    local.get 0
(;@c35   ;)    i32.store offset=8
(;@c38   ;)    local.get 6
(;@c3a   ;)    local.get 1
(;@c3c   ;)    i32.store offset=12
(;@c3f   ;)    local.get 6
(;@c41   ;)    local.get 5
(;@c43   ;)    i32.store offset=16
(;@c46   ;)    local.get 6
(;@c48   ;)    return
             )
(;@aa    ;)  (table (;0;) 39 39 funcref)
(;@b1    ;)  (memory (;0;) 1)
(;@b6    ;)  (global (;0;) (mut i32) i32.const 0)
(;@be    ;)  (export "main" (func $main))
(;@c5    ;)  (export "mem" (memory 0))
(;@ce    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $__apply_4_3 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11 $main_lam_12 $main_lam_13 $main_lam_14 $main_lam_15 $main_lam_16 $main_lam_17 $main_lam_18 $main_lam_19 $main_lam_20 $main_lam_21 $main_lam_22 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
