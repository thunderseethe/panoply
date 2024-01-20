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
(;@ff    ;)  (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@100   ;)    (local i32)
(;@102   ;)    i32.const 4
(;@104   ;)    call $alloc
(;@106   ;)    local.tee 2
(;@108   ;)    i32.const 1
(;@10a   ;)    i32.const 0
(;@10c   ;)    local.get 0
(;@10e   ;)    local.get 1
(;@110   ;)    i32.eq
(;@111   ;)    select
(;@112   ;)    i32.store
(;@115   ;)    local.get 2
(;@117   ;)    return
             )
(;@11a   ;)  (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
(;@11b   ;)    local.get 1
(;@11d   ;)    local.get 0
(;@11f   ;)    i32.load
(;@122   ;)    call_indirect (type $fun_1_1)
             )
(;@127   ;)  (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@128   ;)    local.get 1
(;@12a   ;)    local.get 2
(;@12c   ;)    local.get 0
(;@12e   ;)    i32.load
(;@131   ;)    call_indirect (type $fun_2_1)
             )
(;@136   ;)  (func $__apply_2_1 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@137   ;)    local.get 0
(;@139   ;)    i32.load offset=4
(;@13c   ;)    local.get 1
(;@13e   ;)    local.get 0
(;@140   ;)    i32.load
(;@143   ;)    call_indirect (type $fun_2_1)
             )
(;@148   ;)  (func $__apply_3_0 (;7;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@149   ;)    local.get 1
(;@14b   ;)    local.get 2
(;@14d   ;)    local.get 3
(;@14f   ;)    local.get 0
(;@151   ;)    i32.load
(;@154   ;)    call_indirect (type $fun_3_1)
             )
(;@159   ;)  (func $__apply_3_2 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
(;@15a   ;)    local.get 0
(;@15c   ;)    i32.load offset=4
(;@15f   ;)    local.get 0
(;@161   ;)    i32.load offset=8
(;@164   ;)    local.get 1
(;@166   ;)    local.get 0
(;@168   ;)    i32.load
(;@16b   ;)    call_indirect (type $fun_3_1)
             )
(;@170   ;)  (func $__apply_4_3 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@171   ;)    local.get 0
(;@173   ;)    i32.load offset=4
(;@176   ;)    local.get 0
(;@178   ;)    i32.load offset=8
(;@17b   ;)    local.get 0
(;@17d   ;)    i32.load offset=12
(;@180   ;)    local.get 1
(;@182   ;)    local.get 0
(;@184   ;)    i32.load
(;@187   ;)    call_indirect (type $fun_4_1)
             )
(;@18d   ;)  (func $main (;10;) (type $fun_0_1) (result i32)
(;@18e   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@190   ;)    i32.const 0
(;@192   ;)    call $alloc
(;@194   ;)    local.set 0
(;@196   ;)    local.get 0
(;@198   ;)    local.set 0
(;@19a   ;)    local.get 0
(;@19c   ;)    call $__mon_generate_marker
(;@19e   ;)    local.set 1
(;@1a0   ;)    i32.const 12
(;@1a2   ;)    call $alloc
(;@1a4   ;)    local.set 2
(;@1a6   ;)    local.get 2
(;@1a8   ;)    i32.const 6
(;@1aa   ;)    i32.store
(;@1ad   ;)    local.get 2
(;@1af   ;)    i32.const 28
(;@1b1   ;)    i32.store offset=4
(;@1b4   ;)    local.get 2
(;@1b6   ;)    local.get 1
(;@1b8   ;)    i32.store offset=8
(;@1bb   ;)    local.get 2
(;@1bd   ;)    local.set 2
(;@1bf   ;)    i32.const 8
(;@1c1   ;)    call $alloc
(;@1c3   ;)    local.set 3
(;@1c5   ;)    local.get 3
(;@1c7   ;)    i32.const 4
(;@1c9   ;)    i32.store
(;@1cc   ;)    local.get 3
(;@1ce   ;)    i32.const 30
(;@1d0   ;)    i32.store offset=4
(;@1d3   ;)    local.get 3
(;@1d5   ;)    local.set 3
(;@1d7   ;)    i32.const 16
(;@1d9   ;)    call $alloc
(;@1db   ;)    local.set 4
(;@1dd   ;)    local.get 4
(;@1df   ;)    i32.const 8
(;@1e1   ;)    i32.store
(;@1e4   ;)    local.get 4
(;@1e6   ;)    i32.const 33
(;@1e8   ;)    i32.store offset=4
(;@1eb   ;)    local.get 4
(;@1ed   ;)    local.get 2
(;@1ef   ;)    i32.store offset=8
(;@1f2   ;)    local.get 4
(;@1f4   ;)    local.get 3
(;@1f6   ;)    i32.store offset=12
(;@1f9   ;)    local.get 4
(;@1fb   ;)    local.set 4
(;@1fd   ;)    i32.const 0
(;@1ff   ;)    call $alloc
(;@201   ;)    local.set 5
(;@203   ;)    local.get 5
(;@205   ;)    local.set 5
(;@207   ;)    local.get 4
(;@209   ;)    i32.const 4
(;@20b   ;)    i32.add
(;@20c   ;)    local.get 5
(;@20e   ;)    local.get 4
(;@210   ;)    i32.load
(;@213   ;)    call_indirect (type $fun_2_1)
(;@216   ;)    local.set 6
(;@218   ;)    local.get 6
(;@21a   ;)    i32.load
(;@21d   ;)    local.set 7
(;@21f   ;)    block (result i32) ;; label = @1
(;@221   ;)      block ;; label = @2
(;@223   ;)        block ;; label = @3
(;@225   ;)          block ;; label = @4
(;@227   ;)            local.get 7
(;@229   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@22e   ;)          end
(;@22f   ;)          local.get 6
(;@231   ;)          i32.load offset=4
(;@234   ;)          local.set 8
(;@236   ;)          local.get 8
(;@238   ;)          i32.load offset=4
(;@23b   ;)          local.set 9
(;@23d   ;)          i32.const 8
(;@23f   ;)          call $alloc
(;@241   ;)          local.set 10
(;@243   ;)          local.get 10
(;@245   ;)          i32.const 0
(;@247   ;)          i32.store
(;@24a   ;)          local.get 10
(;@24c   ;)          local.get 9
(;@24e   ;)          i32.store offset=4
(;@251   ;)          local.get 10
(;@253   ;)          br 2 (;@1;)
(;@255   ;)        end
(;@256   ;)        local.get 6
(;@258   ;)        i32.load offset=4
(;@25b   ;)        local.set 10
(;@25d   ;)        local.get 10
(;@25f   ;)        local.set 11
(;@261   ;)        local.get 11
(;@263   ;)        i32.load
(;@266   ;)        local.set 12
(;@268   ;)        local.get 11
(;@26a   ;)        i32.load offset=4
(;@26d   ;)        local.set 13
(;@26f   ;)        i32.const 12
(;@271   ;)        call $alloc
(;@273   ;)        local.set 14
(;@275   ;)        local.get 14
(;@277   ;)        i32.const 6
(;@279   ;)        i32.store
(;@27c   ;)        local.get 14
(;@27e   ;)        i32.const 32
(;@280   ;)        i32.store offset=4
(;@283   ;)        local.get 14
(;@285   ;)        local.get 11
(;@287   ;)        i32.store offset=8
(;@28a   ;)        local.get 14
(;@28c   ;)        local.set 14
(;@28e   ;)        i32.const 12
(;@290   ;)        call $alloc
(;@292   ;)        local.set 15
(;@294   ;)        local.get 15
(;@296   ;)        local.get 12
(;@298   ;)        i32.store
(;@29b   ;)        local.get 15
(;@29d   ;)        local.get 13
(;@29f   ;)        i32.store offset=4
(;@2a2   ;)        local.get 15
(;@2a4   ;)        local.get 14
(;@2a6   ;)        i32.store offset=8
(;@2a9   ;)        local.get 15
(;@2ab   ;)        local.set 15
(;@2ad   ;)        i32.const 8
(;@2af   ;)        call $alloc
(;@2b1   ;)        local.set 16
(;@2b3   ;)        local.get 16
(;@2b5   ;)        i32.const 1
(;@2b7   ;)        i32.store
(;@2ba   ;)        local.get 16
(;@2bc   ;)        local.get 15
(;@2be   ;)        i32.store offset=4
(;@2c1   ;)        local.get 16
(;@2c3   ;)        br 1 (;@1;)
(;@2c5   ;)      end
(;@2c6   ;)      unreachable
(;@2c7   ;)    end
(;@2c8   ;)    local.set 16
(;@2ca   ;)    local.get 16
(;@2cc   ;)    i32.load
(;@2cf   ;)    local.set 17
(;@2d1   ;)    block (result i32) ;; label = @1
(;@2d3   ;)      block ;; label = @2
(;@2d5   ;)        block ;; label = @3
(;@2d7   ;)          block ;; label = @4
(;@2d9   ;)            local.get 17
(;@2db   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2e0   ;)          end
(;@2e1   ;)          local.get 16
(;@2e3   ;)          i32.load offset=4
(;@2e6   ;)          local.set 18
(;@2e8   ;)          local.get 18
(;@2ea   ;)          br 2 (;@1;)
(;@2ec   ;)        end
(;@2ed   ;)        local.get 16
(;@2ef   ;)        i32.load offset=4
(;@2f2   ;)        local.set 18
(;@2f4   ;)        i32.const 5467
(;@2f7   ;)        br 1 (;@1;)
(;@2f9   ;)      end
(;@2fa   ;)      unreachable
(;@2fb   ;)    end
(;@2fc   ;)    return
             )
(;@2ff   ;)  (func $main_lam_0 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@300   ;)    (local i32)
(;@302   ;)    local.get 0
(;@304   ;)    i32.const 4
(;@306   ;)    i32.add
(;@307   ;)    local.get 1
(;@309   ;)    local.get 1
(;@30b   ;)    local.get 0
(;@30d   ;)    i32.load
(;@310   ;)    call_indirect (type $fun_3_1)
(;@313   ;)    return
             )
(;@316   ;)  (func $main_lam_1 (;12;) (type $fun_1_1) (param i32) (result i32)
(;@317   ;)    (local i32)
(;@319   ;)    i32.const 12
(;@31b   ;)    call $alloc
(;@31d   ;)    local.set 1
(;@31f   ;)    local.get 1
(;@321   ;)    i32.const 6
(;@323   ;)    i32.store
(;@326   ;)    local.get 1
(;@328   ;)    i32.const 11
(;@32a   ;)    i32.store offset=4
(;@32d   ;)    local.get 1
(;@32f   ;)    local.get 0
(;@331   ;)    i32.store offset=8
(;@334   ;)    local.get 1
(;@336   ;)    return
             )
(;@339   ;)  (func $main_lam_2 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@33a   ;)    (local i32)
(;@33c   ;)    i32.const 8
(;@33e   ;)    call $alloc
(;@340   ;)    local.set 2
(;@342   ;)    local.get 2
(;@344   ;)    i32.const 0
(;@346   ;)    i32.store
(;@349   ;)    local.get 2
(;@34b   ;)    local.get 0
(;@34d   ;)    i32.store offset=4
(;@350   ;)    local.get 2
(;@352   ;)    return
             )
(;@355   ;)  (func $main_lam_3 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@356   ;)    (local i32 i32 i32 i32)
(;@358   ;)    i32.const 8
(;@35a   ;)    call $alloc
(;@35c   ;)    local.set 2
(;@35e   ;)    local.get 2
(;@360   ;)    i32.const 4
(;@362   ;)    i32.store
(;@365   ;)    local.get 2
(;@367   ;)    i32.const 12
(;@369   ;)    i32.store offset=4
(;@36c   ;)    local.get 2
(;@36e   ;)    local.set 2
(;@370   ;)    i32.const 8
(;@372   ;)    call $alloc
(;@374   ;)    local.set 3
(;@376   ;)    local.get 3
(;@378   ;)    i32.const 5
(;@37a   ;)    i32.store
(;@37d   ;)    local.get 3
(;@37f   ;)    i32.const 13
(;@381   ;)    i32.store offset=4
(;@384   ;)    local.get 3
(;@386   ;)    local.set 3
(;@388   ;)    i32.const 12
(;@38a   ;)    call $alloc
(;@38c   ;)    local.set 4
(;@38e   ;)    local.get 4
(;@390   ;)    local.get 0
(;@392   ;)    i32.store
(;@395   ;)    local.get 4
(;@397   ;)    local.get 2
(;@399   ;)    i32.store offset=4
(;@39c   ;)    local.get 4
(;@39e   ;)    local.get 3
(;@3a0   ;)    i32.store offset=8
(;@3a3   ;)    local.get 4
(;@3a5   ;)    local.set 4
(;@3a7   ;)    i32.const 8
(;@3a9   ;)    call $alloc
(;@3ab   ;)    local.set 5
(;@3ad   ;)    local.get 5
(;@3af   ;)    i32.const 1
(;@3b1   ;)    i32.store
(;@3b4   ;)    local.get 5
(;@3b6   ;)    local.get 4
(;@3b8   ;)    i32.store offset=4
(;@3bb   ;)    local.get 5
(;@3bd   ;)    return
             )
(;@3c0   ;)  (func $main_lam_4 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3c1   ;)    (local i32)
(;@3c3   ;)    i32.const 8
(;@3c5   ;)    call $alloc
(;@3c7   ;)    local.set 2
(;@3c9   ;)    local.get 2
(;@3cb   ;)    local.get 1
(;@3cd   ;)    i32.store
(;@3d0   ;)    local.get 2
(;@3d2   ;)    local.get 0
(;@3d4   ;)    i32.store offset=4
(;@3d7   ;)    local.get 2
(;@3d9   ;)    return
             )
(;@3dc   ;)  (func $main_lam_5 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3dd   ;)    (local i32 i32)
(;@3df   ;)    i32.const 12
(;@3e1   ;)    call $alloc
(;@3e3   ;)    local.set 2
(;@3e5   ;)    local.get 2
(;@3e7   ;)    i32.const 6
(;@3e9   ;)    i32.store
(;@3ec   ;)    local.get 2
(;@3ee   ;)    i32.const 15
(;@3f0   ;)    i32.store offset=4
(;@3f3   ;)    local.get 2
(;@3f5   ;)    local.get 0
(;@3f7   ;)    i32.store offset=8
(;@3fa   ;)    local.get 2
(;@3fc   ;)    local.set 2
(;@3fe   ;)    i32.const 8
(;@400   ;)    call $alloc
(;@402   ;)    local.set 3
(;@404   ;)    local.get 3
(;@406   ;)    i32.const 0
(;@408   ;)    i32.store
(;@40b   ;)    local.get 3
(;@40d   ;)    local.get 2
(;@40f   ;)    i32.store offset=4
(;@412   ;)    local.get 3
(;@414   ;)    return
             )
(;@417   ;)  (func $main_lam_6 (;17;) (type $fun_1_1) (param i32) (result i32)
(;@418   ;)    (local i32)
(;@41a   ;)    i32.const 12
(;@41c   ;)    call $alloc
(;@41e   ;)    local.set 1
(;@420   ;)    local.get 1
(;@422   ;)    i32.const 6
(;@424   ;)    i32.store
(;@427   ;)    local.get 1
(;@429   ;)    i32.const 16
(;@42b   ;)    i32.store offset=4
(;@42e   ;)    local.get 1
(;@430   ;)    local.get 0
(;@432   ;)    i32.store offset=8
(;@435   ;)    local.get 1
(;@437   ;)    return
             )
(;@43a   ;)  (func $main_lam_7 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@43b   ;)    (local i32)
(;@43d   ;)    local.get 1
(;@43f   ;)    i32.const 4
(;@441   ;)    i32.add
(;@442   ;)    local.get 2
(;@444   ;)    local.get 2
(;@446   ;)    local.get 1
(;@448   ;)    i32.load
(;@44b   ;)    call_indirect (type $fun_3_1)
(;@44e   ;)    return
             )
(;@451   ;)  (func $main_lam_8 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@452   ;)    (local i32 i32)
(;@454   ;)    i32.const 0
(;@456   ;)    call $alloc
(;@458   ;)    local.set 3
(;@45a   ;)    local.get 3
(;@45c   ;)    local.set 3
(;@45e   ;)    local.get 1
(;@460   ;)    i32.const 4
(;@462   ;)    i32.add
(;@463   ;)    local.get 3
(;@465   ;)    local.get 0
(;@467   ;)    local.get 1
(;@469   ;)    i32.load
(;@46c   ;)    call_indirect (type $fun_3_1)
(;@46f   ;)    return
             )
(;@472   ;)  (func $main_lam_9 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@473   ;)    (local i32)
(;@475   ;)    local.get 1
(;@477   ;)    i32.const 4
(;@479   ;)    i32.add
(;@47a   ;)    local.get 2
(;@47c   ;)    local.get 2
(;@47e   ;)    local.get 1
(;@480   ;)    i32.load
(;@483   ;)    call_indirect (type $fun_3_1)
(;@486   ;)    return
             )
(;@489   ;)  (func $main_lam_10 (;21;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@48a   ;)    (local i32 i32)
(;@48c   ;)    i32.const 0
(;@48e   ;)    call $alloc
(;@490   ;)    local.set 3
(;@492   ;)    local.get 3
(;@494   ;)    local.set 3
(;@496   ;)    local.get 1
(;@498   ;)    i32.const 4
(;@49a   ;)    i32.add
(;@49b   ;)    local.get 3
(;@49d   ;)    local.get 0
(;@49f   ;)    local.get 1
(;@4a1   ;)    i32.load
(;@4a4   ;)    call_indirect (type $fun_3_1)
(;@4a7   ;)    return
             )
(;@4aa   ;)  (func $main_lam_11 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4ab   ;)    (local i32 i32 i32 i32)
(;@4ad   ;)    i32.const 8
(;@4af   ;)    call $alloc
(;@4b1   ;)    local.set 2
(;@4b3   ;)    local.get 2
(;@4b5   ;)    i32.const 7
(;@4b7   ;)    i32.store
(;@4ba   ;)    local.get 2
(;@4bc   ;)    i32.const 20
(;@4be   ;)    i32.store offset=4
(;@4c1   ;)    local.get 2
(;@4c3   ;)    local.set 2
(;@4c5   ;)    i32.const 8
(;@4c7   ;)    call $alloc
(;@4c9   ;)    local.set 3
(;@4cb   ;)    local.get 3
(;@4cd   ;)    i32.const 7
(;@4cf   ;)    i32.store
(;@4d2   ;)    local.get 3
(;@4d4   ;)    i32.const 21
(;@4d6   ;)    i32.store offset=4
(;@4d9   ;)    local.get 3
(;@4db   ;)    local.set 3
(;@4dd   ;)    i32.const 8
(;@4df   ;)    call $alloc
(;@4e1   ;)    local.set 4
(;@4e3   ;)    local.get 4
(;@4e5   ;)    local.get 2
(;@4e7   ;)    i32.store
(;@4ea   ;)    local.get 4
(;@4ec   ;)    local.get 3
(;@4ee   ;)    i32.store offset=4
(;@4f1   ;)    local.get 4
(;@4f3   ;)    local.set 4
(;@4f5   ;)    i32.const 8
(;@4f7   ;)    call $alloc
(;@4f9   ;)    local.set 5
(;@4fb   ;)    local.get 5
(;@4fd   ;)    local.get 0
(;@4ff   ;)    i32.store
(;@502   ;)    local.get 5
(;@504   ;)    local.get 4
(;@506   ;)    i32.store offset=4
(;@509   ;)    local.get 5
(;@50b   ;)    return
             )
(;@50e   ;)  (func $main_lam_12 (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@50f   ;)    (local i32 i32 i32 i32)
(;@511   ;)    i32.const 12
(;@513   ;)    call $alloc
(;@515   ;)    local.set 3
(;@517   ;)    local.get 3
(;@519   ;)    i32.const 6
(;@51b   ;)    i32.store
(;@51e   ;)    local.get 3
(;@520   ;)    i32.const 22
(;@522   ;)    i32.store offset=4
(;@525   ;)    local.get 3
(;@527   ;)    local.get 0
(;@529   ;)    i32.store offset=8
(;@52c   ;)    local.get 3
(;@52e   ;)    local.set 3
(;@530   ;)    local.get 1
(;@532   ;)    i32.load offset=8
(;@535   ;)    local.set 4
(;@537   ;)    local.get 4
(;@539   ;)    i32.const 4
(;@53b   ;)    i32.add
(;@53c   ;)    local.get 2
(;@53e   ;)    local.get 4
(;@540   ;)    i32.load
(;@543   ;)    call_indirect (type $fun_2_1)
(;@546   ;)    local.set 5
(;@548   ;)    i32.const 20
(;@54a   ;)    call $alloc
(;@54c   ;)    local.set 6
(;@54e   ;)    local.get 6
(;@550   ;)    i32.const 9
(;@552   ;)    i32.store
(;@555   ;)    local.get 6
(;@557   ;)    i32.const 35
(;@559   ;)    i32.store offset=4
(;@55c   ;)    local.get 6
(;@55e   ;)    local.get 0
(;@560   ;)    i32.store offset=8
(;@563   ;)    local.get 6
(;@565   ;)    local.get 3
(;@567   ;)    i32.store offset=12
(;@56a   ;)    local.get 6
(;@56c   ;)    local.get 5
(;@56e   ;)    i32.store offset=16
(;@571   ;)    local.get 6
(;@573   ;)    return
             )
(;@576   ;)  (func $main_lam_13 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@577   ;)    (local i32)
(;@579   ;)    local.get 1
(;@57b   ;)    i32.const 4
(;@57d   ;)    i32.add
(;@57e   ;)    local.get 2
(;@580   ;)    local.get 2
(;@582   ;)    local.get 1
(;@584   ;)    i32.load
(;@587   ;)    call_indirect (type $fun_3_1)
(;@58a   ;)    return
             )
(;@58d   ;)  (func $main_lam_14 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@58e   ;)    (local i32 i32)
(;@590   ;)    i32.const 0
(;@592   ;)    call $alloc
(;@594   ;)    local.set 3
(;@596   ;)    local.get 3
(;@598   ;)    local.set 3
(;@59a   ;)    local.get 1
(;@59c   ;)    i32.const 4
(;@59e   ;)    i32.add
(;@59f   ;)    local.get 3
(;@5a1   ;)    local.get 0
(;@5a3   ;)    local.get 1
(;@5a5   ;)    i32.load
(;@5a8   ;)    call_indirect (type $fun_3_1)
(;@5ab   ;)    return
             )
(;@5ae   ;)  (func $main_lam_15 (;26;) (type $fun_2_1) (param i32 i32) (result i32)
(;@5af   ;)    (local i32 i32 i32 i32)
(;@5b1   ;)    i32.const 8
(;@5b3   ;)    call $alloc
(;@5b5   ;)    local.set 2
(;@5b7   ;)    local.get 2
(;@5b9   ;)    i32.const 7
(;@5bb   ;)    i32.store
(;@5be   ;)    local.get 2
(;@5c0   ;)    i32.const 24
(;@5c2   ;)    i32.store offset=4
(;@5c5   ;)    local.get 2
(;@5c7   ;)    local.set 2
(;@5c9   ;)    i32.const 8
(;@5cb   ;)    call $alloc
(;@5cd   ;)    local.set 3
(;@5cf   ;)    local.get 3
(;@5d1   ;)    i32.const 7
(;@5d3   ;)    i32.store
(;@5d6   ;)    local.get 3
(;@5d8   ;)    i32.const 25
(;@5da   ;)    i32.store offset=4
(;@5dd   ;)    local.get 3
(;@5df   ;)    local.set 3
(;@5e1   ;)    i32.const 8
(;@5e3   ;)    call $alloc
(;@5e5   ;)    local.set 4
(;@5e7   ;)    local.get 4
(;@5e9   ;)    local.get 2
(;@5eb   ;)    i32.store
(;@5ee   ;)    local.get 4
(;@5f0   ;)    local.get 3
(;@5f2   ;)    i32.store offset=4
(;@5f5   ;)    local.get 4
(;@5f7   ;)    local.set 4
(;@5f9   ;)    i32.const 8
(;@5fb   ;)    call $alloc
(;@5fd   ;)    local.set 5
(;@5ff   ;)    local.get 5
(;@601   ;)    local.get 0
(;@603   ;)    i32.store
(;@606   ;)    local.get 5
(;@608   ;)    local.get 4
(;@60a   ;)    i32.store offset=4
(;@60d   ;)    local.get 5
(;@60f   ;)    return
             )
(;@612   ;)  (func $main_lam_16 (;27;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@613   ;)    (local i32 i32 i32 i32)
(;@615   ;)    i32.const 12
(;@617   ;)    call $alloc
(;@619   ;)    local.set 3
(;@61b   ;)    local.get 3
(;@61d   ;)    i32.const 6
(;@61f   ;)    i32.store
(;@622   ;)    local.get 3
(;@624   ;)    i32.const 26
(;@626   ;)    i32.store offset=4
(;@629   ;)    local.get 3
(;@62b   ;)    local.get 0
(;@62d   ;)    i32.store offset=8
(;@630   ;)    local.get 3
(;@632   ;)    local.set 3
(;@634   ;)    local.get 1
(;@636   ;)    i32.load offset=8
(;@639   ;)    local.set 4
(;@63b   ;)    local.get 4
(;@63d   ;)    i32.const 4
(;@63f   ;)    i32.add
(;@640   ;)    local.get 2
(;@642   ;)    local.get 4
(;@644   ;)    i32.load
(;@647   ;)    call_indirect (type $fun_2_1)
(;@64a   ;)    local.set 5
(;@64c   ;)    i32.const 20
(;@64e   ;)    call $alloc
(;@650   ;)    local.set 6
(;@652   ;)    local.get 6
(;@654   ;)    i32.const 9
(;@656   ;)    i32.store
(;@659   ;)    local.get 6
(;@65b   ;)    i32.const 35
(;@65d   ;)    i32.store offset=4
(;@660   ;)    local.get 6
(;@662   ;)    local.get 0
(;@664   ;)    i32.store offset=8
(;@667   ;)    local.get 6
(;@669   ;)    local.get 3
(;@66b   ;)    i32.store offset=12
(;@66e   ;)    local.get 6
(;@670   ;)    local.get 5
(;@672   ;)    i32.store offset=16
(;@675   ;)    local.get 6
(;@677   ;)    return
             )
(;@67b   ;)  (func $main_lam_17 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
(;@67c   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@67e   ;)    i32.const 12
(;@680   ;)    call $alloc
(;@682   ;)    local.set 2
(;@684   ;)    local.get 2
(;@686   ;)    i32.const 6
(;@688   ;)    i32.store
(;@68b   ;)    local.get 2
(;@68d   ;)    i32.const 14
(;@68f   ;)    i32.store offset=4
(;@692   ;)    local.get 2
(;@694   ;)    local.get 0
(;@696   ;)    i32.store offset=8
(;@699   ;)    local.get 2
(;@69b   ;)    local.set 2
(;@69d   ;)    i32.const 8
(;@69f   ;)    call $alloc
(;@6a1   ;)    local.set 3
(;@6a3   ;)    local.get 3
(;@6a5   ;)    i32.const 4
(;@6a7   ;)    i32.store
(;@6aa   ;)    local.get 3
(;@6ac   ;)    i32.const 17
(;@6ae   ;)    i32.store offset=4
(;@6b1   ;)    local.get 3
(;@6b3   ;)    local.set 3
(;@6b5   ;)    i32.const 8
(;@6b7   ;)    call $alloc
(;@6b9   ;)    local.set 4
(;@6bb   ;)    local.get 4
(;@6bd   ;)    i32.const 7
(;@6bf   ;)    i32.store
(;@6c2   ;)    local.get 4
(;@6c4   ;)    i32.const 18
(;@6c6   ;)    i32.store offset=4
(;@6c9   ;)    local.get 4
(;@6cb   ;)    local.set 4
(;@6cd   ;)    i32.const 8
(;@6cf   ;)    call $alloc
(;@6d1   ;)    local.set 5
(;@6d3   ;)    local.get 5
(;@6d5   ;)    i32.const 7
(;@6d7   ;)    i32.store
(;@6da   ;)    local.get 5
(;@6dc   ;)    i32.const 19
(;@6de   ;)    i32.store offset=4
(;@6e1   ;)    local.get 5
(;@6e3   ;)    local.set 5
(;@6e5   ;)    i32.const 8
(;@6e7   ;)    call $alloc
(;@6e9   ;)    local.set 6
(;@6eb   ;)    local.get 6
(;@6ed   ;)    local.get 4
(;@6ef   ;)    i32.store
(;@6f2   ;)    local.get 6
(;@6f4   ;)    local.get 5
(;@6f6   ;)    i32.store offset=4
(;@6f9   ;)    local.get 6
(;@6fb   ;)    local.set 6
(;@6fd   ;)    i32.const 8
(;@6ff   ;)    call $alloc
(;@701   ;)    local.set 7
(;@703   ;)    local.get 7
(;@705   ;)    local.get 0
(;@707   ;)    i32.store
(;@70a   ;)    local.get 7
(;@70c   ;)    local.get 6
(;@70e   ;)    i32.store offset=4
(;@711   ;)    local.get 7
(;@713   ;)    local.set 7
(;@715   ;)    local.get 2
(;@717   ;)    local.get 3
(;@719   ;)    local.get 7
(;@71b   ;)    call $__mon_bind
(;@71d   ;)    local.set 8
(;@71f   ;)    local.get 8
(;@721   ;)    i32.load
(;@724   ;)    local.set 9
(;@726   ;)    block (result i32) ;; label = @1
(;@728   ;)      block ;; label = @2
(;@72a   ;)        block ;; label = @3
(;@72c   ;)          block ;; label = @4
(;@72e   ;)            local.get 9
(;@730   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@735   ;)          end
(;@736   ;)          local.get 8
(;@738   ;)          i32.load offset=4
(;@73b   ;)          local.set 10
(;@73d   ;)          i32.const 8
(;@73f   ;)          call $alloc
(;@741   ;)          local.set 11
(;@743   ;)          local.get 11
(;@745   ;)          i32.const 0
(;@747   ;)          i32.store
(;@74a   ;)          local.get 11
(;@74c   ;)          local.get 10
(;@74e   ;)          i32.store offset=4
(;@751   ;)          local.get 11
(;@753   ;)          br 2 (;@1;)
(;@755   ;)        end
(;@756   ;)        local.get 8
(;@758   ;)        i32.load offset=4
(;@75b   ;)        local.set 11
(;@75d   ;)        local.get 11
(;@75f   ;)        i32.load
(;@762   ;)        local.set 12
(;@764   ;)        local.get 0
(;@766   ;)        local.get 12
(;@768   ;)        call $__mon_eqm
(;@76a   ;)        local.set 13
(;@76c   ;)        local.get 13
(;@76e   ;)        i32.load
(;@771   ;)        local.set 14
(;@773   ;)        block (result i32) ;; label = @3
(;@775   ;)          block ;; label = @4
(;@777   ;)            block ;; label = @5
(;@779   ;)              block ;; label = @6
(;@77b   ;)                local.get 14
(;@77d   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@782   ;)              end
(;@783   ;)              local.get 13
(;@785   ;)              i32.load offset=4
(;@788   ;)              local.set 15
(;@78a   ;)              local.get 11
(;@78c   ;)              i32.load
(;@78f   ;)              local.set 16
(;@791   ;)              local.get 11
(;@793   ;)              i32.load offset=4
(;@796   ;)              local.set 17
(;@798   ;)              i32.const 16
(;@79a   ;)              call $alloc
(;@79c   ;)              local.set 18
(;@79e   ;)              local.get 18
(;@7a0   ;)              i32.const 8
(;@7a2   ;)              i32.store
(;@7a5   ;)              local.get 18
(;@7a7   ;)              i32.const 23
(;@7a9   ;)              i32.store offset=4
(;@7ac   ;)              local.get 18
(;@7ae   ;)              local.get 0
(;@7b0   ;)              i32.store offset=8
(;@7b3   ;)              local.get 18
(;@7b5   ;)              local.get 11
(;@7b7   ;)              i32.store offset=12
(;@7ba   ;)              local.get 18
(;@7bc   ;)              local.set 18
(;@7be   ;)              local.get 11
(;@7c0   ;)              i32.load offset=8
(;@7c3   ;)              local.set 19
(;@7c5   ;)              i32.const 16
(;@7c7   ;)              call $alloc
(;@7c9   ;)              local.set 20
(;@7cb   ;)              local.get 20
(;@7cd   ;)              local.get 16
(;@7cf   ;)              i32.store
(;@7d2   ;)              local.get 20
(;@7d4   ;)              local.get 17
(;@7d6   ;)              i32.store offset=4
(;@7d9   ;)              local.get 20
(;@7db   ;)              local.get 18
(;@7dd   ;)              i32.store offset=8
(;@7e0   ;)              local.get 20
(;@7e2   ;)              local.get 19
(;@7e4   ;)              i32.store offset=12
(;@7e7   ;)              local.get 20
(;@7e9   ;)              local.set 20
(;@7eb   ;)              i32.const 8
(;@7ed   ;)              call $alloc
(;@7ef   ;)              local.set 21
(;@7f1   ;)              local.get 21
(;@7f3   ;)              i32.const 1
(;@7f5   ;)              i32.store
(;@7f8   ;)              local.get 21
(;@7fa   ;)              local.get 20
(;@7fc   ;)              i32.store offset=4
(;@7ff   ;)              local.get 21
(;@801   ;)              br 2 (;@3;)
(;@803   ;)            end
(;@804   ;)            local.get 13
(;@806   ;)            i32.load offset=4
(;@809   ;)            local.set 15
(;@80b   ;)            i32.const 16
(;@80d   ;)            call $alloc
(;@80f   ;)            local.set 21
(;@811   ;)            local.get 21
(;@813   ;)            i32.const 8
(;@815   ;)            i32.store
(;@818   ;)            local.get 21
(;@81a   ;)            i32.const 27
(;@81c   ;)            i32.store offset=4
(;@81f   ;)            local.get 21
(;@821   ;)            local.get 0
(;@823   ;)            i32.store offset=8
(;@826   ;)            local.get 21
(;@828   ;)            local.get 11
(;@82a   ;)            i32.store offset=12
(;@82d   ;)            local.get 21
(;@82f   ;)            local.set 21
(;@831   ;)            local.get 11
(;@833   ;)            i32.load offset=4
(;@836   ;)            local.set 22
(;@838   ;)            local.get 22
(;@83a   ;)            i32.const 4
(;@83c   ;)            i32.add
(;@83d   ;)            local.get 21
(;@83f   ;)            local.get 1
(;@841   ;)            local.get 22
(;@843   ;)            i32.load
(;@846   ;)            call_indirect (type $fun_3_1)
(;@849   ;)            br 1 (;@3;)
(;@84b   ;)          end
(;@84c   ;)          unreachable
(;@84d   ;)        end
(;@84e   ;)        br 1 (;@1;)
(;@850   ;)      end
(;@851   ;)      unreachable
(;@852   ;)    end
(;@853   ;)    return
             )
(;@856   ;)  (func $main_lam_18 (;29;) (type $fun_2_1) (param i32 i32) (result i32)
(;@857   ;)    (local i32)
(;@859   ;)    i32.const 8
(;@85b   ;)    call $alloc
(;@85d   ;)    local.set 2
(;@85f   ;)    local.get 2
(;@861   ;)    i32.const 0
(;@863   ;)    i32.store
(;@866   ;)    local.get 2
(;@868   ;)    local.get 0
(;@86a   ;)    i32.store offset=4
(;@86d   ;)    local.get 2
(;@86f   ;)    return
             )
(;@872   ;)  (func $main_lam_19 (;30;) (type $fun_1_1) (param i32) (result i32)
(;@873   ;)    (local i32 i32)
(;@875   ;)    local.get 0
(;@877   ;)    i32.const 4
(;@879   ;)    i32.add
(;@87a   ;)    i32.const 825
(;@87d   ;)    local.get 0
(;@87f   ;)    i32.load
(;@882   ;)    call_indirect (type $fun_2_1)
(;@885   ;)    local.set 1
(;@887   ;)    i32.const 12
(;@889   ;)    call $alloc
(;@88b   ;)    local.set 2
(;@88d   ;)    local.get 2
(;@88f   ;)    i32.const 6
(;@891   ;)    i32.store
(;@894   ;)    local.get 2
(;@896   ;)    i32.const 29
(;@898   ;)    i32.store offset=4
(;@89b   ;)    local.get 2
(;@89d   ;)    local.get 1
(;@89f   ;)    i32.store offset=8
(;@8a2   ;)    local.get 2
(;@8a4   ;)    return
             )
(;@8a7   ;)  (func $main_lam_20 (;31;) (type $fun_2_1) (param i32 i32) (result i32)
(;@8a8   ;)    (local i32 i32)
(;@8aa   ;)    local.get 0
(;@8ac   ;)    i32.load offset=4
(;@8af   ;)    local.set 2
(;@8b1   ;)    i32.const 8
(;@8b3   ;)    call $alloc
(;@8b5   ;)    local.set 3
(;@8b7   ;)    local.get 3
(;@8b9   ;)    i32.const 0
(;@8bb   ;)    i32.store
(;@8be   ;)    local.get 3
(;@8c0   ;)    local.get 2
(;@8c2   ;)    i32.store offset=4
(;@8c5   ;)    local.get 3
(;@8c7   ;)    return
             )
(;@8ca   ;)  (func $main_lam_21 (;32;) (type $fun_2_1) (param i32 i32) (result i32)
(;@8cb   ;)    (local i32 i32 i32)
(;@8cd   ;)    i32.const 12
(;@8cf   ;)    call $alloc
(;@8d1   ;)    local.set 2
(;@8d3   ;)    local.get 2
(;@8d5   ;)    i32.const 6
(;@8d7   ;)    i32.store
(;@8da   ;)    local.get 2
(;@8dc   ;)    i32.const 31
(;@8de   ;)    i32.store offset=4
(;@8e1   ;)    local.get 2
(;@8e3   ;)    local.get 1
(;@8e5   ;)    i32.store offset=8
(;@8e8   ;)    local.get 2
(;@8ea   ;)    local.set 2
(;@8ec   ;)    local.get 0
(;@8ee   ;)    i32.load offset=8
(;@8f1   ;)    local.set 3
(;@8f3   ;)    i32.const 16
(;@8f5   ;)    call $alloc
(;@8f7   ;)    local.set 4
(;@8f9   ;)    local.get 4
(;@8fb   ;)    i32.const 8
(;@8fd   ;)    i32.store
(;@900   ;)    local.get 4
(;@902   ;)    i32.const 33
(;@904   ;)    i32.store offset=4
(;@907   ;)    local.get 4
(;@909   ;)    local.get 2
(;@90b   ;)    i32.store offset=8
(;@90e   ;)    local.get 4
(;@910   ;)    local.get 3
(;@912   ;)    i32.store offset=12
(;@915   ;)    local.get 4
(;@917   ;)    return
             )
(;@91b   ;)  (func $__mon_bind (;33;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@91c   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@91e   ;)    local.get 0
(;@920   ;)    i32.const 4
(;@922   ;)    i32.add
(;@923   ;)    local.get 2
(;@925   ;)    local.get 0
(;@927   ;)    i32.load
(;@92a   ;)    call_indirect (type $fun_2_1)
(;@92d   ;)    local.set 3
(;@92f   ;)    local.get 3
(;@931   ;)    i32.load
(;@934   ;)    local.set 4
(;@936   ;)    block (result i32) ;; label = @1
(;@938   ;)      block ;; label = @2
(;@93a   ;)        block ;; label = @3
(;@93c   ;)          block ;; label = @4
(;@93e   ;)            local.get 4
(;@940   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@945   ;)          end
(;@946   ;)          local.get 3
(;@948   ;)          i32.load offset=4
(;@94b   ;)          local.set 5
(;@94d   ;)          local.get 1
(;@94f   ;)          i32.const 4
(;@951   ;)          i32.add
(;@952   ;)          local.get 5
(;@954   ;)          local.get 2
(;@956   ;)          local.get 1
(;@958   ;)          i32.load
(;@95b   ;)          call_indirect (type $fun_3_1)
(;@95e   ;)          br 2 (;@1;)
(;@960   ;)        end
(;@961   ;)        local.get 3
(;@963   ;)        i32.load offset=4
(;@966   ;)        local.set 6
(;@968   ;)        local.get 6
(;@96a   ;)        local.set 7
(;@96c   ;)        local.get 7
(;@96e   ;)        i32.load
(;@971   ;)        local.set 8
(;@973   ;)        local.get 7
(;@975   ;)        i32.load offset=4
(;@978   ;)        local.set 9
(;@97a   ;)        i32.const 16
(;@97c   ;)        call $alloc
(;@97e   ;)        local.set 10
(;@980   ;)        local.get 10
(;@982   ;)        i32.const 8
(;@984   ;)        i32.store
(;@987   ;)        local.get 10
(;@989   ;)        i32.const 34
(;@98b   ;)        i32.store offset=4
(;@98e   ;)        local.get 10
(;@990   ;)        local.get 1
(;@992   ;)        i32.store offset=8
(;@995   ;)        local.get 10
(;@997   ;)        local.get 7
(;@999   ;)        i32.store offset=12
(;@99c   ;)        local.get 10
(;@99e   ;)        local.set 10
(;@9a0   ;)        i32.const 12
(;@9a2   ;)        call $alloc
(;@9a4   ;)        local.set 11
(;@9a6   ;)        local.get 11
(;@9a8   ;)        local.get 8
(;@9aa   ;)        i32.store
(;@9ad   ;)        local.get 11
(;@9af   ;)        local.get 9
(;@9b1   ;)        i32.store offset=4
(;@9b4   ;)        local.get 11
(;@9b6   ;)        local.get 10
(;@9b8   ;)        i32.store offset=8
(;@9bb   ;)        local.get 11
(;@9bd   ;)        local.set 11
(;@9bf   ;)        i32.const 8
(;@9c1   ;)        call $alloc
(;@9c3   ;)        local.set 12
(;@9c5   ;)        local.get 12
(;@9c7   ;)        i32.const 1
(;@9c9   ;)        i32.store
(;@9cc   ;)        local.get 12
(;@9ce   ;)        local.get 11
(;@9d0   ;)        i32.store offset=4
(;@9d3   ;)        local.get 12
(;@9d5   ;)        br 1 (;@1;)
(;@9d7   ;)      end
(;@9d8   ;)      unreachable
(;@9d9   ;)    end
(;@9da   ;)    return
             )
(;@9dd   ;)  (func $__mon_bind_lam_0 (;34;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@9de   ;)    (local i32 i32 i32)
(;@9e0   ;)    local.get 0
(;@9e2   ;)    i32.const 4
(;@9e4   ;)    i32.add
(;@9e5   ;)    local.get 2
(;@9e7   ;)    local.get 0
(;@9e9   ;)    i32.load
(;@9ec   ;)    call_indirect (type $fun_2_1)
(;@9ef   ;)    local.set 3
(;@9f1   ;)    local.get 1
(;@9f3   ;)    i32.load offset=8
(;@9f6   ;)    local.set 4
(;@9f8   ;)    i32.const 16
(;@9fa   ;)    call $alloc
(;@9fc   ;)    local.set 5
(;@9fe   ;)    local.get 5
(;@a00   ;)    i32.const 8
(;@a02   ;)    i32.store
(;@a05   ;)    local.get 5
(;@a07   ;)    i32.const 33
(;@a09   ;)    i32.store offset=4
(;@a0c   ;)    local.get 5
(;@a0e   ;)    local.get 3
(;@a10   ;)    i32.store offset=8
(;@a13   ;)    local.get 5
(;@a15   ;)    local.get 4
(;@a17   ;)    i32.store offset=12
(;@a1a   ;)    local.get 5
(;@a1c   ;)    return
             )
(;@a20   ;)  (func $__mon_prompt (;35;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@a21   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@a23   ;)    local.get 1
(;@a25   ;)    i32.const 4
(;@a27   ;)    i32.add
(;@a28   ;)    local.get 3
(;@a2a   ;)    local.get 1
(;@a2c   ;)    i32.load
(;@a2f   ;)    call_indirect (type $fun_2_1)
(;@a32   ;)    local.set 4
(;@a34   ;)    local.get 2
(;@a36   ;)    i32.const 4
(;@a38   ;)    i32.add
(;@a39   ;)    local.get 4
(;@a3b   ;)    local.get 2
(;@a3d   ;)    i32.load
(;@a40   ;)    call_indirect (type $fun_2_1)
(;@a43   ;)    local.set 5
(;@a45   ;)    local.get 5
(;@a47   ;)    i32.load
(;@a4a   ;)    local.set 6
(;@a4c   ;)    block (result i32) ;; label = @1
(;@a4e   ;)      block ;; label = @2
(;@a50   ;)        block ;; label = @3
(;@a52   ;)          block ;; label = @4
(;@a54   ;)            local.get 6
(;@a56   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@a5b   ;)          end
(;@a5c   ;)          local.get 5
(;@a5e   ;)          i32.load offset=4
(;@a61   ;)          local.set 7
(;@a63   ;)          i32.const 8
(;@a65   ;)          call $alloc
(;@a67   ;)          local.set 8
(;@a69   ;)          local.get 8
(;@a6b   ;)          i32.const 0
(;@a6d   ;)          i32.store
(;@a70   ;)          local.get 8
(;@a72   ;)          local.get 7
(;@a74   ;)          i32.store offset=4
(;@a77   ;)          local.get 8
(;@a79   ;)          br 2 (;@1;)
(;@a7b   ;)        end
(;@a7c   ;)        local.get 5
(;@a7e   ;)        i32.load offset=4
(;@a81   ;)        local.set 8
(;@a83   ;)        local.get 8
(;@a85   ;)        i32.load
(;@a88   ;)        local.set 9
(;@a8a   ;)        local.get 0
(;@a8c   ;)        local.get 9
(;@a8e   ;)        call $__mon_eqm
(;@a90   ;)        local.set 10
(;@a92   ;)        local.get 10
(;@a94   ;)        i32.load
(;@a97   ;)        local.set 11
(;@a99   ;)        block (result i32) ;; label = @3
(;@a9b   ;)          block ;; label = @4
(;@a9d   ;)            block ;; label = @5
(;@a9f   ;)              block ;; label = @6
(;@aa1   ;)                local.get 11
(;@aa3   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@aa8   ;)              end
(;@aa9   ;)              local.get 10
(;@aab   ;)              i32.load offset=4
(;@aae   ;)              local.set 12
(;@ab0   ;)              local.get 8
(;@ab2   ;)              i32.load
(;@ab5   ;)              local.set 13
(;@ab7   ;)              local.get 8
(;@ab9   ;)              i32.load offset=4
(;@abc   ;)              local.set 14
(;@abe   ;)              i32.const 20
(;@ac0   ;)              call $alloc
(;@ac2   ;)              local.set 15
(;@ac4   ;)              local.get 15
(;@ac6   ;)              i32.const 9
(;@ac8   ;)              i32.store
(;@acb   ;)              local.get 15
(;@acd   ;)              i32.const 36
(;@acf   ;)              i32.store offset=4
(;@ad2   ;)              local.get 15
(;@ad4   ;)              local.get 0
(;@ad6   ;)              i32.store offset=8
(;@ad9   ;)              local.get 15
(;@adb   ;)              local.get 1
(;@add   ;)              i32.store offset=12
(;@ae0   ;)              local.get 15
(;@ae2   ;)              local.get 8
(;@ae4   ;)              i32.store offset=16
(;@ae7   ;)              local.get 15
(;@ae9   ;)              local.set 15
(;@aeb   ;)              local.get 8
(;@aed   ;)              i32.load offset=8
(;@af0   ;)              local.set 16
(;@af2   ;)              i32.const 16
(;@af4   ;)              call $alloc
(;@af6   ;)              local.set 17
(;@af8   ;)              local.get 17
(;@afa   ;)              local.get 13
(;@afc   ;)              i32.store
(;@aff   ;)              local.get 17
(;@b01   ;)              local.get 14
(;@b03   ;)              i32.store offset=4
(;@b06   ;)              local.get 17
(;@b08   ;)              local.get 15
(;@b0a   ;)              i32.store offset=8
(;@b0d   ;)              local.get 17
(;@b0f   ;)              local.get 16
(;@b11   ;)              i32.store offset=12
(;@b14   ;)              local.get 17
(;@b16   ;)              local.set 17
(;@b18   ;)              i32.const 8
(;@b1a   ;)              call $alloc
(;@b1c   ;)              local.set 18
(;@b1e   ;)              local.get 18
(;@b20   ;)              i32.const 1
(;@b22   ;)              i32.store
(;@b25   ;)              local.get 18
(;@b27   ;)              local.get 17
(;@b29   ;)              i32.store offset=4
(;@b2c   ;)              local.get 18
(;@b2e   ;)              br 2 (;@3;)
(;@b30   ;)            end
(;@b31   ;)            local.get 10
(;@b33   ;)            i32.load offset=4
(;@b36   ;)            local.set 12
(;@b38   ;)            i32.const 20
(;@b3a   ;)            call $alloc
(;@b3c   ;)            local.set 18
(;@b3e   ;)            local.get 18
(;@b40   ;)            i32.const 9
(;@b42   ;)            i32.store
(;@b45   ;)            local.get 18
(;@b47   ;)            i32.const 37
(;@b49   ;)            i32.store offset=4
(;@b4c   ;)            local.get 18
(;@b4e   ;)            local.get 0
(;@b50   ;)            i32.store offset=8
(;@b53   ;)            local.get 18
(;@b55   ;)            local.get 1
(;@b57   ;)            i32.store offset=12
(;@b5a   ;)            local.get 18
(;@b5c   ;)            local.get 8
(;@b5e   ;)            i32.store offset=16
(;@b61   ;)            local.get 18
(;@b63   ;)            local.set 18
(;@b65   ;)            local.get 8
(;@b67   ;)            i32.load offset=4
(;@b6a   ;)            local.set 19
(;@b6c   ;)            local.get 19
(;@b6e   ;)            i32.const 4
(;@b70   ;)            i32.add
(;@b71   ;)            local.get 18
(;@b73   ;)            local.get 3
(;@b75   ;)            local.get 19
(;@b77   ;)            i32.load
(;@b7a   ;)            call_indirect (type $fun_3_1)
(;@b7d   ;)            br 1 (;@3;)
(;@b7f   ;)          end
(;@b80   ;)          unreachable
(;@b81   ;)        end
(;@b82   ;)        br 1 (;@1;)
(;@b84   ;)      end
(;@b85   ;)      unreachable
(;@b86   ;)    end
(;@b87   ;)    return
             )
(;@b8a   ;)  (func $__mon_prompt_lam_0 (;36;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@b8b   ;)    (local i32 i32 i32)
(;@b8d   ;)    local.get 2
(;@b8f   ;)    i32.load offset=8
(;@b92   ;)    local.set 4
(;@b94   ;)    local.get 4
(;@b96   ;)    i32.const 4
(;@b98   ;)    i32.add
(;@b99   ;)    local.get 3
(;@b9b   ;)    local.get 4
(;@b9d   ;)    i32.load
(;@ba0   ;)    call_indirect (type $fun_2_1)
(;@ba3   ;)    local.set 5
(;@ba5   ;)    i32.const 20
(;@ba7   ;)    call $alloc
(;@ba9   ;)    local.set 6
(;@bab   ;)    local.get 6
(;@bad   ;)    i32.const 9
(;@baf   ;)    i32.store
(;@bb2   ;)    local.get 6
(;@bb4   ;)    i32.const 35
(;@bb6   ;)    i32.store offset=4
(;@bb9   ;)    local.get 6
(;@bbb   ;)    local.get 0
(;@bbd   ;)    i32.store offset=8
(;@bc0   ;)    local.get 6
(;@bc2   ;)    local.get 1
(;@bc4   ;)    i32.store offset=12
(;@bc7   ;)    local.get 6
(;@bc9   ;)    local.get 5
(;@bcb   ;)    i32.store offset=16
(;@bce   ;)    local.get 6
(;@bd0   ;)    return
             )
(;@bd3   ;)  (func $__mon_prompt_lam_1 (;37;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@bd4   ;)    (local i32 i32 i32)
(;@bd6   ;)    local.get 2
(;@bd8   ;)    i32.load offset=8
(;@bdb   ;)    local.set 4
(;@bdd   ;)    local.get 4
(;@bdf   ;)    i32.const 4
(;@be1   ;)    i32.add
(;@be2   ;)    local.get 3
(;@be4   ;)    local.get 4
(;@be6   ;)    i32.load
(;@be9   ;)    call_indirect (type $fun_2_1)
(;@bec   ;)    local.set 5
(;@bee   ;)    i32.const 20
(;@bf0   ;)    call $alloc
(;@bf2   ;)    local.set 6
(;@bf4   ;)    local.get 6
(;@bf6   ;)    i32.const 9
(;@bf8   ;)    i32.store
(;@bfb   ;)    local.get 6
(;@bfd   ;)    i32.const 35
(;@bff   ;)    i32.store offset=4
(;@c02   ;)    local.get 6
(;@c04   ;)    local.get 0
(;@c06   ;)    i32.store offset=8
(;@c09   ;)    local.get 6
(;@c0b   ;)    local.get 1
(;@c0d   ;)    i32.store offset=12
(;@c10   ;)    local.get 6
(;@c12   ;)    local.get 5
(;@c14   ;)    i32.store offset=16
(;@c17   ;)    local.get 6
(;@c19   ;)    return
             )
(;@a9    ;)  (table (;0;) 38 38 funcref)
(;@b0    ;)  (memory (;0;) 1)
(;@b5    ;)  (global (;0;) (mut i32) i32.const 0)
(;@bd    ;)  (export "main" (func $main))
(;@c4    ;)  (export "mem" (memory 0))
(;@cd    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $__apply_4_3 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11 $main_lam_12 $main_lam_13 $main_lam_14 $main_lam_15 $main_lam_16 $main_lam_17 $main_lam_18 $main_lam_19 $main_lam_20 $main_lam_21 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
