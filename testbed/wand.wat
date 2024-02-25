(module $reader
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_4_1 (;4;) (func (param i32 i32 i32 i32) (result i32)))
(;@29    ;)  (type $fun_5_1 (;5;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@32    ;)  (type $fun_6_1 (;6;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
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
(;@127   ;)  (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@128   ;)    local.get 1
(;@12a   ;)    local.get 0
(;@12c   ;)    i32.load
(;@12f   ;)    call_indirect (type $fun_1_1)
             )
(;@134   ;)  (func $__apply_2_0 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@135   ;)    local.get 1
(;@137   ;)    local.get 2
(;@139   ;)    local.get 0
(;@13b   ;)    i32.load
(;@13e   ;)    call_indirect (type $fun_2_1)
             )
(;@143   ;)  (func $__apply_2_1 (;5;) (type $fun_2_1) (param i32 i32) (result i32)
(;@144   ;)    local.get 0
(;@146   ;)    i32.load offset=4
(;@149   ;)    local.get 1
(;@14b   ;)    local.get 0
(;@14d   ;)    i32.load
(;@150   ;)    call_indirect (type $fun_2_1)
             )
(;@155   ;)  (func $__apply_3_0 (;6;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@156   ;)    local.get 1
(;@158   ;)    local.get 2
(;@15a   ;)    local.get 3
(;@15c   ;)    local.get 0
(;@15e   ;)    i32.load
(;@161   ;)    call_indirect (type $fun_3_1)
             )
(;@166   ;)  (func $__apply_3_1 (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@167   ;)    local.get 0
(;@169   ;)    i32.load offset=4
(;@16c   ;)    local.get 1
(;@16e   ;)    local.get 2
(;@170   ;)    local.get 0
(;@172   ;)    i32.load
(;@175   ;)    call_indirect (type $fun_3_1)
             )
(;@17a   ;)  (func $__apply_3_2 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
(;@17b   ;)    local.get 0
(;@17d   ;)    i32.load offset=4
(;@180   ;)    local.get 0
(;@182   ;)    i32.load offset=8
(;@185   ;)    local.get 1
(;@187   ;)    local.get 0
(;@189   ;)    i32.load
(;@18c   ;)    call_indirect (type $fun_3_1)
             )
(;@191   ;)  (func $__apply_4_2 (;9;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@192   ;)    local.get 0
(;@194   ;)    i32.load offset=4
(;@197   ;)    local.get 0
(;@199   ;)    i32.load offset=8
(;@19c   ;)    local.get 1
(;@19e   ;)    local.get 2
(;@1a0   ;)    local.get 0
(;@1a2   ;)    i32.load
(;@1a5   ;)    call_indirect (type $fun_4_1)
             )
(;@1aa   ;)  (func $__apply_4_3 (;10;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1ab   ;)    local.get 0
(;@1ad   ;)    i32.load offset=4
(;@1b0   ;)    local.get 0
(;@1b2   ;)    i32.load offset=8
(;@1b5   ;)    local.get 0
(;@1b7   ;)    i32.load offset=12
(;@1ba   ;)    local.get 1
(;@1bc   ;)    local.get 0
(;@1be   ;)    i32.load
(;@1c1   ;)    call_indirect (type $fun_4_1)
             )
(;@1c6   ;)  (func $__apply_5_3 (;11;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1c7   ;)    local.get 0
(;@1c9   ;)    i32.load offset=4
(;@1cc   ;)    local.get 0
(;@1ce   ;)    i32.load offset=8
(;@1d1   ;)    local.get 0
(;@1d3   ;)    i32.load offset=12
(;@1d6   ;)    local.get 1
(;@1d8   ;)    local.get 2
(;@1da   ;)    local.get 0
(;@1dc   ;)    i32.load
(;@1df   ;)    call_indirect (type $fun_5_1)
             )
(;@1e4   ;)  (func $__apply_5_4 (;12;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1e5   ;)    local.get 0
(;@1e7   ;)    i32.load offset=4
(;@1ea   ;)    local.get 0
(;@1ec   ;)    i32.load offset=8
(;@1ef   ;)    local.get 0
(;@1f1   ;)    i32.load offset=12
(;@1f4   ;)    local.get 0
(;@1f6   ;)    i32.load offset=16
(;@1f9   ;)    local.get 1
(;@1fb   ;)    local.get 0
(;@1fd   ;)    i32.load
(;@200   ;)    call_indirect (type $fun_5_1)
             )
(;@205   ;)  (func $__apply_6_4 (;13;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@206   ;)    local.get 0
(;@208   ;)    i32.load offset=4
(;@20b   ;)    local.get 0
(;@20d   ;)    i32.load offset=8
(;@210   ;)    local.get 0
(;@212   ;)    i32.load offset=12
(;@215   ;)    local.get 0
(;@217   ;)    i32.load offset=16
(;@21a   ;)    local.get 1
(;@21c   ;)    local.get 2
(;@21e   ;)    local.get 0
(;@220   ;)    i32.load
(;@223   ;)    call_indirect (type $fun_6_1)
             )
(;@228   ;)  (func $f (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@229   ;)    (local i32 i32)
(;@22b   ;)    i32.const 16
(;@22d   ;)    call $alloc
(;@22f   ;)    local.set 2
(;@231   ;)    local.get 2
(;@233   ;)    i32.const 8
(;@235   ;)    i32.store
(;@238   ;)    local.get 2
(;@23a   ;)    i32.const 42
(;@23c   ;)    i32.store offset=4
(;@23f   ;)    local.get 2
(;@241   ;)    local.get 0
(;@243   ;)    i32.store offset=8
(;@246   ;)    local.get 2
(;@248   ;)    local.get 0
(;@24a   ;)    i32.store offset=12
(;@24d   ;)    local.get 2
(;@24f   ;)    local.set 2
(;@251   ;)    i32.const 8
(;@253   ;)    call $alloc
(;@255   ;)    local.set 3
(;@257   ;)    local.get 3
(;@259   ;)    i32.const 0
(;@25b   ;)    i32.store
(;@25e   ;)    local.get 3
(;@260   ;)    local.get 2
(;@262   ;)    i32.store offset=4
(;@265   ;)    local.get 3
(;@267   ;)    return
             )
(;@26a   ;)  (func $f_lam_0 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@26b   ;)    (local i32)
(;@26d   ;)    local.get 1
(;@26f   ;)    i32.const 4
(;@271   ;)    i32.add
(;@272   ;)    local.get 0
(;@274   ;)    local.get 1
(;@276   ;)    i32.load
(;@279   ;)    call_indirect (type $fun_2_1)
(;@27c   ;)    return
             )
(;@27f   ;)  (func $f_lam_1 (;16;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@280   ;)    (local i32 i32)
(;@282   ;)    i32.const 12
(;@284   ;)    call $alloc
(;@286   ;)    local.set 3
(;@288   ;)    local.get 3
(;@28a   ;)    i32.const 5
(;@28c   ;)    i32.store
(;@28f   ;)    local.get 3
(;@291   ;)    i32.const 15
(;@293   ;)    i32.store offset=4
(;@296   ;)    local.get 3
(;@298   ;)    local.get 0
(;@29a   ;)    i32.store offset=8
(;@29d   ;)    local.get 3
(;@29f   ;)    local.set 3
(;@2a1   ;)    i32.const 8
(;@2a3   ;)    call $alloc
(;@2a5   ;)    local.set 4
(;@2a7   ;)    local.get 4
(;@2a9   ;)    i32.const 0
(;@2ab   ;)    i32.store
(;@2ae   ;)    local.get 4
(;@2b0   ;)    local.get 3
(;@2b2   ;)    i32.store offset=4
(;@2b5   ;)    local.get 4
(;@2b7   ;)    return
             )
(;@2ba   ;)  (func $f_lam_2 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2bb   ;)    (local i32)
(;@2bd   ;)    local.get 1
(;@2bf   ;)    i32.const 4
(;@2c1   ;)    i32.add
(;@2c2   ;)    local.get 0
(;@2c4   ;)    local.get 1
(;@2c6   ;)    i32.load
(;@2c9   ;)    call_indirect (type $fun_2_1)
(;@2cc   ;)    return
             )
(;@2cf   ;)  (func $f_lam_3 (;18;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@2d0   ;)    (local i32 i32 i32 i32)
(;@2d2   ;)    local.get 1
(;@2d4   ;)    i32.load offset=8
(;@2d7   ;)    local.set 4
(;@2d9   ;)    local.get 4
(;@2db   ;)    i32.const 4
(;@2dd   ;)    i32.add
(;@2de   ;)    local.get 2
(;@2e0   ;)    local.get 4
(;@2e2   ;)    i32.load
(;@2e5   ;)    call_indirect (type $fun_2_1)
(;@2e8   ;)    local.set 5
(;@2ea   ;)    i32.const 12
(;@2ec   ;)    call $alloc
(;@2ee   ;)    local.set 6
(;@2f0   ;)    local.get 6
(;@2f2   ;)    i32.const 5
(;@2f4   ;)    i32.store
(;@2f7   ;)    local.get 6
(;@2f9   ;)    i32.const 17
(;@2fb   ;)    i32.store offset=4
(;@2fe   ;)    local.get 6
(;@300   ;)    local.get 0
(;@302   ;)    i32.store offset=8
(;@305   ;)    local.get 6
(;@307   ;)    local.set 6
(;@309   ;)    local.get 5
(;@30b   ;)    local.get 6
(;@30d   ;)    local.get 3
(;@30f   ;)    call $__mon_bind
(;@311   ;)    return
             )
(;@315   ;)  (func $f_lam_4 (;19;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@316   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@318   ;)    local.get 2
(;@31a   ;)    i32.const 4
(;@31c   ;)    i32.add
(;@31d   ;)    local.get 3
(;@31f   ;)    local.get 2
(;@321   ;)    i32.load
(;@324   ;)    call_indirect (type $fun_2_1)
(;@327   ;)    local.set 3
(;@329   ;)    local.get 3
(;@32b   ;)    i32.load
(;@32e   ;)    local.set 4
(;@330   ;)    block (result i32) ;; label = @1
(;@332   ;)      block ;; label = @2
(;@334   ;)        block ;; label = @3
(;@336   ;)          block ;; label = @4
(;@338   ;)            local.get 4
(;@33a   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@33f   ;)          end
(;@340   ;)          local.get 3
(;@342   ;)          i32.load offset=4
(;@345   ;)          local.set 5
(;@347   ;)          local.get 5
(;@349   ;)          i32.const 4
(;@34b   ;)          i32.add
(;@34c   ;)          local.get 1
(;@34e   ;)          local.get 3
(;@350   ;)          local.get 5
(;@352   ;)          i32.load
(;@355   ;)          call_indirect (type $fun_3_1)
(;@358   ;)          br 2 (;@1;)
(;@35a   ;)        end
(;@35b   ;)        local.get 3
(;@35d   ;)        i32.load offset=4
(;@360   ;)        local.set 6
(;@362   ;)        local.get 6
(;@364   ;)        local.set 7
(;@366   ;)        local.get 7
(;@368   ;)        i32.load
(;@36b   ;)        local.set 8
(;@36d   ;)        local.get 7
(;@36f   ;)        i32.load offset=4
(;@372   ;)        local.set 9
(;@374   ;)        i32.const 16
(;@376   ;)        call $alloc
(;@378   ;)        local.set 10
(;@37a   ;)        local.get 10
(;@37c   ;)        i32.const 9
(;@37e   ;)        i32.store
(;@381   ;)        local.get 10
(;@383   ;)        i32.const 18
(;@385   ;)        i32.store offset=4
(;@388   ;)        local.get 10
(;@38a   ;)        local.get 1
(;@38c   ;)        i32.store offset=8
(;@38f   ;)        local.get 10
(;@391   ;)        local.get 7
(;@393   ;)        i32.store offset=12
(;@396   ;)        local.get 10
(;@398   ;)        local.set 10
(;@39a   ;)        i32.const 12
(;@39c   ;)        call $alloc
(;@39e   ;)        local.set 11
(;@3a0   ;)        local.get 11
(;@3a2   ;)        local.get 8
(;@3a4   ;)        i32.store
(;@3a7   ;)        local.get 11
(;@3a9   ;)        local.get 9
(;@3ab   ;)        i32.store offset=4
(;@3ae   ;)        local.get 11
(;@3b0   ;)        local.get 10
(;@3b2   ;)        i32.store offset=8
(;@3b5   ;)        local.get 11
(;@3b7   ;)        local.set 11
(;@3b9   ;)        i32.const 8
(;@3bb   ;)        call $alloc
(;@3bd   ;)        local.set 12
(;@3bf   ;)        local.get 12
(;@3c1   ;)        i32.const 1
(;@3c3   ;)        i32.store
(;@3c6   ;)        local.get 12
(;@3c8   ;)        local.get 11
(;@3ca   ;)        i32.store offset=4
(;@3cd   ;)        local.get 12
(;@3cf   ;)        br 1 (;@1;)
(;@3d1   ;)      end
(;@3d2   ;)      unreachable
(;@3d3   ;)    end
(;@3d4   ;)    return
             )
(;@3d7   ;)  (func $f_lam_5 (;20;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3d8   ;)    (local i32 i32 i32 i32)
(;@3da   ;)    i32.const 0
(;@3dc   ;)    call $alloc
(;@3de   ;)    local.set 2
(;@3e0   ;)    local.get 2
(;@3e2   ;)    local.set 2
(;@3e4   ;)    local.get 0
(;@3e6   ;)    i32.load offset=4
(;@3e9   ;)    local.set 3
(;@3eb   ;)    local.get 3
(;@3ed   ;)    i32.const 4
(;@3ef   ;)    i32.add
(;@3f0   ;)    local.get 2
(;@3f2   ;)    local.get 3
(;@3f4   ;)    i32.load
(;@3f7   ;)    call_indirect (type $fun_2_1)
(;@3fa   ;)    local.set 4
(;@3fc   ;)    i32.const 20
(;@3fe   ;)    call $alloc
(;@400   ;)    local.set 5
(;@402   ;)    local.get 5
(;@404   ;)    i32.const 10
(;@406   ;)    i32.store
(;@409   ;)    local.get 5
(;@40b   ;)    i32.const 19
(;@40d   ;)    i32.store offset=4
(;@410   ;)    local.get 5
(;@412   ;)    local.get 1
(;@414   ;)    i32.store offset=8
(;@417   ;)    local.get 5
(;@419   ;)    local.get 1
(;@41b   ;)    i32.store offset=12
(;@41e   ;)    local.get 5
(;@420   ;)    local.get 4
(;@422   ;)    i32.store offset=16
(;@425   ;)    local.get 5
(;@427   ;)    return
             )
(;@42a   ;)  (func $f_lam_6 (;21;) (type $fun_2_1) (param i32 i32) (result i32)
(;@42b   ;)    (local i32)
(;@42d   ;)    local.get 1
(;@42f   ;)    i32.const 4
(;@431   ;)    i32.add
(;@432   ;)    local.get 0
(;@434   ;)    local.get 1
(;@436   ;)    i32.load
(;@439   ;)    call_indirect (type $fun_2_1)
(;@43c   ;)    return
             )
(;@43f   ;)  (func $f_lam_7 (;22;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@440   ;)    (local i32 i32)
(;@442   ;)    i32.const 12
(;@444   ;)    call $alloc
(;@446   ;)    local.set 3
(;@448   ;)    local.get 3
(;@44a   ;)    i32.const 5
(;@44c   ;)    i32.store
(;@44f   ;)    local.get 3
(;@451   ;)    i32.const 21
(;@453   ;)    i32.store offset=4
(;@456   ;)    local.get 3
(;@458   ;)    local.get 0
(;@45a   ;)    i32.store offset=8
(;@45d   ;)    local.get 3
(;@45f   ;)    local.set 3
(;@461   ;)    i32.const 8
(;@463   ;)    call $alloc
(;@465   ;)    local.set 4
(;@467   ;)    local.get 4
(;@469   ;)    i32.const 0
(;@46b   ;)    i32.store
(;@46e   ;)    local.get 4
(;@470   ;)    local.get 3
(;@472   ;)    i32.store offset=4
(;@475   ;)    local.get 4
(;@477   ;)    return
             )
(;@47a   ;)  (func $f_lam_8 (;23;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@47b   ;)    (local i32 i32 i32 i32)
(;@47d   ;)    i32.const 12
(;@47f   ;)    call $alloc
(;@481   ;)    local.set 4
(;@483   ;)    local.get 4
(;@485   ;)    i32.const 7
(;@487   ;)    i32.store
(;@48a   ;)    local.get 4
(;@48c   ;)    i32.const 22
(;@48e   ;)    i32.store offset=4
(;@491   ;)    local.get 4
(;@493   ;)    local.get 1
(;@495   ;)    i32.store offset=8
(;@498   ;)    local.get 4
(;@49a   ;)    local.set 4
(;@49c   ;)    i32.const 8
(;@49e   ;)    call $alloc
(;@4a0   ;)    local.set 5
(;@4a2   ;)    local.get 5
(;@4a4   ;)    local.get 2
(;@4a6   ;)    i32.store
(;@4a9   ;)    local.get 5
(;@4ab   ;)    local.get 4
(;@4ad   ;)    i32.store offset=4
(;@4b0   ;)    local.get 5
(;@4b2   ;)    local.set 5
(;@4b4   ;)    local.get 0
(;@4b6   ;)    i32.load
(;@4b9   ;)    local.set 6
(;@4bb   ;)    local.get 6
(;@4bd   ;)    i32.const 4
(;@4bf   ;)    i32.add
(;@4c0   ;)    local.get 3
(;@4c2   ;)    local.get 5
(;@4c4   ;)    local.get 6
(;@4c6   ;)    i32.load
(;@4c9   ;)    call_indirect (type $fun_3_1)
(;@4cc   ;)    return
             )
(;@4cf   ;)  (func $f_lam_9 (;24;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4d0   ;)    (local i32)
(;@4d2   ;)    i32.const 8
(;@4d4   ;)    call $alloc
(;@4d6   ;)    local.set 2
(;@4d8   ;)    local.get 2
(;@4da   ;)    i32.const 0
(;@4dc   ;)    i32.store
(;@4df   ;)    local.get 2
(;@4e1   ;)    local.get 0
(;@4e3   ;)    i32.store offset=4
(;@4e6   ;)    local.get 2
(;@4e8   ;)    return
             )
(;@4eb   ;)  (func $f_lam_10 (;25;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4ec   ;)    (local i32)
(;@4ee   ;)    i32.const 8
(;@4f0   ;)    call $alloc
(;@4f2   ;)    local.set 2
(;@4f4   ;)    local.get 2
(;@4f6   ;)    i32.const 0
(;@4f8   ;)    i32.store
(;@4fb   ;)    local.get 2
(;@4fd   ;)    local.get 0
(;@4ff   ;)    i32.store offset=4
(;@502   ;)    local.get 2
(;@504   ;)    return
             )
(;@507   ;)  (func $f_lam_11 (;26;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@508   ;)    (local i32 i32 i32 i32)
(;@50a   ;)    i32.const 20
(;@50c   ;)    call $alloc
(;@50e   ;)    local.set 5
(;@510   ;)    local.get 5
(;@512   ;)    i32.const 10
(;@514   ;)    i32.store
(;@517   ;)    local.get 5
(;@519   ;)    i32.const 23
(;@51b   ;)    i32.store offset=4
(;@51e   ;)    local.get 5
(;@520   ;)    local.get 0
(;@522   ;)    i32.store offset=8
(;@525   ;)    local.get 5
(;@527   ;)    local.get 1
(;@529   ;)    i32.store offset=12
(;@52c   ;)    local.get 5
(;@52e   ;)    local.get 2
(;@530   ;)    i32.store offset=16
(;@533   ;)    local.get 5
(;@535   ;)    local.set 5
(;@537   ;)    i32.const 8
(;@539   ;)    call $alloc
(;@53b   ;)    local.set 6
(;@53d   ;)    local.get 6
(;@53f   ;)    i32.const 4
(;@541   ;)    i32.store
(;@544   ;)    local.get 6
(;@546   ;)    i32.const 24
(;@548   ;)    i32.store offset=4
(;@54b   ;)    local.get 6
(;@54d   ;)    local.set 6
(;@54f   ;)    i32.const 12
(;@551   ;)    call $alloc
(;@553   ;)    local.set 7
(;@555   ;)    local.get 7
(;@557   ;)    i32.const 5
(;@559   ;)    i32.store
(;@55c   ;)    local.get 7
(;@55e   ;)    i32.const 25
(;@560   ;)    i32.store offset=4
(;@563   ;)    local.get 7
(;@565   ;)    local.get 3
(;@567   ;)    i32.store offset=8
(;@56a   ;)    local.get 7
(;@56c   ;)    local.set 7
(;@56e   ;)    local.get 2
(;@570   ;)    local.get 5
(;@572   ;)    local.get 6
(;@574   ;)    local.get 7
(;@576   ;)    local.get 4
(;@578   ;)    call $__mon_prompt
(;@57a   ;)    return
             )
(;@57d   ;)  (func $f_lam_12 (;27;) (type $fun_2_1) (param i32 i32) (result i32)
(;@57e   ;)    (local i32)
(;@580   ;)    local.get 1
(;@582   ;)    i32.const 4
(;@584   ;)    i32.add
(;@585   ;)    local.get 0
(;@587   ;)    local.get 1
(;@589   ;)    i32.load
(;@58c   ;)    call_indirect (type $fun_2_1)
(;@58f   ;)    return
             )
(;@592   ;)  (func $f_lam_13 (;28;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@593   ;)    (local i32 i32)
(;@595   ;)    i32.const 12
(;@597   ;)    call $alloc
(;@599   ;)    local.set 3
(;@59b   ;)    local.get 3
(;@59d   ;)    i32.const 5
(;@59f   ;)    i32.store
(;@5a2   ;)    local.get 3
(;@5a4   ;)    i32.const 27
(;@5a6   ;)    i32.store offset=4
(;@5a9   ;)    local.get 3
(;@5ab   ;)    local.get 0
(;@5ad   ;)    i32.store offset=8
(;@5b0   ;)    local.get 3
(;@5b2   ;)    local.set 3
(;@5b4   ;)    i32.const 8
(;@5b6   ;)    call $alloc
(;@5b8   ;)    local.set 4
(;@5ba   ;)    local.get 4
(;@5bc   ;)    i32.const 0
(;@5be   ;)    i32.store
(;@5c1   ;)    local.get 4
(;@5c3   ;)    local.get 3
(;@5c5   ;)    i32.store offset=4
(;@5c8   ;)    local.get 4
(;@5ca   ;)    return
             )
(;@5cd   ;)  (func $f_lam_14 (;29;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@5ce   ;)    (local i32 i32 i32 i32)
(;@5d0   ;)    i32.const 12
(;@5d2   ;)    call $alloc
(;@5d4   ;)    local.set 4
(;@5d6   ;)    local.get 4
(;@5d8   ;)    i32.const 7
(;@5da   ;)    i32.store
(;@5dd   ;)    local.get 4
(;@5df   ;)    i32.const 28
(;@5e1   ;)    i32.store offset=4
(;@5e4   ;)    local.get 4
(;@5e6   ;)    local.get 1
(;@5e8   ;)    i32.store offset=8
(;@5eb   ;)    local.get 4
(;@5ed   ;)    local.set 4
(;@5ef   ;)    i32.const 8
(;@5f1   ;)    call $alloc
(;@5f3   ;)    local.set 5
(;@5f5   ;)    local.get 5
(;@5f7   ;)    local.get 2
(;@5f9   ;)    i32.store
(;@5fc   ;)    local.get 5
(;@5fe   ;)    local.get 4
(;@600   ;)    i32.store offset=4
(;@603   ;)    local.get 5
(;@605   ;)    local.set 5
(;@607   ;)    local.get 0
(;@609   ;)    i32.load
(;@60c   ;)    local.set 6
(;@60e   ;)    local.get 6
(;@610   ;)    i32.const 4
(;@612   ;)    i32.add
(;@613   ;)    local.get 3
(;@615   ;)    local.get 5
(;@617   ;)    local.get 6
(;@619   ;)    i32.load
(;@61c   ;)    call_indirect (type $fun_3_1)
(;@61f   ;)    return
             )
(;@622   ;)  (func $f_lam_15 (;30;) (type $fun_2_1) (param i32 i32) (result i32)
(;@623   ;)    (local i32)
(;@625   ;)    i32.const 8
(;@627   ;)    call $alloc
(;@629   ;)    local.set 2
(;@62b   ;)    local.get 2
(;@62d   ;)    i32.const 0
(;@62f   ;)    i32.store
(;@632   ;)    local.get 2
(;@634   ;)    local.get 0
(;@636   ;)    i32.store offset=4
(;@639   ;)    local.get 2
(;@63b   ;)    return
             )
(;@63e   ;)  (func $f_lam_16 (;31;) (type $fun_2_1) (param i32 i32) (result i32)
(;@63f   ;)    (local i32)
(;@641   ;)    i32.const 8
(;@643   ;)    call $alloc
(;@645   ;)    local.set 2
(;@647   ;)    local.get 2
(;@649   ;)    i32.const 0
(;@64b   ;)    i32.store
(;@64e   ;)    local.get 2
(;@650   ;)    local.get 0
(;@652   ;)    i32.store offset=4
(;@655   ;)    local.get 2
(;@657   ;)    return
             )
(;@65a   ;)  (func $f_lam_17 (;32;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@65b   ;)    (local i32 i32 i32 i32)
(;@65d   ;)    i32.const 20
(;@65f   ;)    call $alloc
(;@661   ;)    local.set 5
(;@663   ;)    local.get 5
(;@665   ;)    i32.const 10
(;@667   ;)    i32.store
(;@66a   ;)    local.get 5
(;@66c   ;)    i32.const 29
(;@66e   ;)    i32.store offset=4
(;@671   ;)    local.get 5
(;@673   ;)    local.get 0
(;@675   ;)    i32.store offset=8
(;@678   ;)    local.get 5
(;@67a   ;)    local.get 1
(;@67c   ;)    i32.store offset=12
(;@67f   ;)    local.get 5
(;@681   ;)    local.get 2
(;@683   ;)    i32.store offset=16
(;@686   ;)    local.get 5
(;@688   ;)    local.set 5
(;@68a   ;)    i32.const 8
(;@68c   ;)    call $alloc
(;@68e   ;)    local.set 6
(;@690   ;)    local.get 6
(;@692   ;)    i32.const 4
(;@694   ;)    i32.store
(;@697   ;)    local.get 6
(;@699   ;)    i32.const 30
(;@69b   ;)    i32.store offset=4
(;@69e   ;)    local.get 6
(;@6a0   ;)    local.set 6
(;@6a2   ;)    i32.const 12
(;@6a4   ;)    call $alloc
(;@6a6   ;)    local.set 7
(;@6a8   ;)    local.get 7
(;@6aa   ;)    i32.const 5
(;@6ac   ;)    i32.store
(;@6af   ;)    local.get 7
(;@6b1   ;)    i32.const 31
(;@6b3   ;)    i32.store offset=4
(;@6b6   ;)    local.get 7
(;@6b8   ;)    local.get 3
(;@6ba   ;)    i32.store offset=8
(;@6bd   ;)    local.get 7
(;@6bf   ;)    local.set 7
(;@6c1   ;)    local.get 2
(;@6c3   ;)    local.get 5
(;@6c5   ;)    local.get 6
(;@6c7   ;)    local.get 7
(;@6c9   ;)    local.get 4
(;@6cb   ;)    call $__mon_prompt
(;@6cd   ;)    return
             )
(;@6d0   ;)  (func $f_lam_18 (;33;) (type $fun_2_1) (param i32 i32) (result i32)
(;@6d1   ;)    (local i32)
(;@6d3   ;)    local.get 1
(;@6d5   ;)    i32.const 4
(;@6d7   ;)    i32.add
(;@6d8   ;)    local.get 0
(;@6da   ;)    local.get 1
(;@6dc   ;)    i32.load
(;@6df   ;)    call_indirect (type $fun_2_1)
(;@6e2   ;)    return
             )
(;@6e5   ;)  (func $f_lam_19 (;34;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@6e6   ;)    (local i32 i32)
(;@6e8   ;)    i32.const 12
(;@6ea   ;)    call $alloc
(;@6ec   ;)    local.set 3
(;@6ee   ;)    local.get 3
(;@6f0   ;)    i32.const 5
(;@6f2   ;)    i32.store
(;@6f5   ;)    local.get 3
(;@6f7   ;)    i32.const 33
(;@6f9   ;)    i32.store offset=4
(;@6fc   ;)    local.get 3
(;@6fe   ;)    local.get 0
(;@700   ;)    i32.store offset=8
(;@703   ;)    local.get 3
(;@705   ;)    local.set 3
(;@707   ;)    i32.const 8
(;@709   ;)    call $alloc
(;@70b   ;)    local.set 4
(;@70d   ;)    local.get 4
(;@70f   ;)    i32.const 0
(;@711   ;)    i32.store
(;@714   ;)    local.get 4
(;@716   ;)    local.get 3
(;@718   ;)    i32.store offset=4
(;@71b   ;)    local.get 4
(;@71d   ;)    return
             )
(;@720   ;)  (func $f_lam_20 (;35;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@721   ;)    (local i32 i32 i32 i32)
(;@723   ;)    i32.const 12
(;@725   ;)    call $alloc
(;@727   ;)    local.set 4
(;@729   ;)    local.get 4
(;@72b   ;)    i32.const 7
(;@72d   ;)    i32.store
(;@730   ;)    local.get 4
(;@732   ;)    i32.const 34
(;@734   ;)    i32.store offset=4
(;@737   ;)    local.get 4
(;@739   ;)    local.get 1
(;@73b   ;)    i32.store offset=8
(;@73e   ;)    local.get 4
(;@740   ;)    local.set 4
(;@742   ;)    i32.const 8
(;@744   ;)    call $alloc
(;@746   ;)    local.set 5
(;@748   ;)    local.get 5
(;@74a   ;)    local.get 2
(;@74c   ;)    i32.store
(;@74f   ;)    local.get 5
(;@751   ;)    local.get 4
(;@753   ;)    i32.store offset=4
(;@756   ;)    local.get 5
(;@758   ;)    local.set 5
(;@75a   ;)    local.get 0
(;@75c   ;)    i32.load
(;@75f   ;)    local.set 6
(;@761   ;)    local.get 6
(;@763   ;)    i32.const 4
(;@765   ;)    i32.add
(;@766   ;)    local.get 3
(;@768   ;)    local.get 5
(;@76a   ;)    local.get 6
(;@76c   ;)    i32.load
(;@76f   ;)    call_indirect (type $fun_3_1)
(;@772   ;)    return
             )
(;@775   ;)  (func $f_lam_21 (;36;) (type $fun_2_1) (param i32 i32) (result i32)
(;@776   ;)    (local i32)
(;@778   ;)    i32.const 8
(;@77a   ;)    call $alloc
(;@77c   ;)    local.set 2
(;@77e   ;)    local.get 2
(;@780   ;)    i32.const 0
(;@782   ;)    i32.store
(;@785   ;)    local.get 2
(;@787   ;)    local.get 0
(;@789   ;)    i32.store offset=4
(;@78c   ;)    local.get 2
(;@78e   ;)    return
             )
(;@791   ;)  (func $f_lam_22 (;37;) (type $fun_2_1) (param i32 i32) (result i32)
(;@792   ;)    (local i32)
(;@794   ;)    i32.const 8
(;@796   ;)    call $alloc
(;@798   ;)    local.set 2
(;@79a   ;)    local.get 2
(;@79c   ;)    i32.const 0
(;@79e   ;)    i32.store
(;@7a1   ;)    local.get 2
(;@7a3   ;)    local.get 0
(;@7a5   ;)    i32.store offset=4
(;@7a8   ;)    local.get 2
(;@7aa   ;)    return
             )
(;@7ad   ;)  (func $f_lam_23 (;38;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@7ae   ;)    (local i32 i32 i32 i32)
(;@7b0   ;)    i32.const 20
(;@7b2   ;)    call $alloc
(;@7b4   ;)    local.set 5
(;@7b6   ;)    local.get 5
(;@7b8   ;)    i32.const 10
(;@7ba   ;)    i32.store
(;@7bd   ;)    local.get 5
(;@7bf   ;)    i32.const 35
(;@7c1   ;)    i32.store offset=4
(;@7c4   ;)    local.get 5
(;@7c6   ;)    local.get 0
(;@7c8   ;)    i32.store offset=8
(;@7cb   ;)    local.get 5
(;@7cd   ;)    local.get 1
(;@7cf   ;)    i32.store offset=12
(;@7d2   ;)    local.get 5
(;@7d4   ;)    local.get 2
(;@7d6   ;)    i32.store offset=16
(;@7d9   ;)    local.get 5
(;@7db   ;)    local.set 5
(;@7dd   ;)    i32.const 8
(;@7df   ;)    call $alloc
(;@7e1   ;)    local.set 6
(;@7e3   ;)    local.get 6
(;@7e5   ;)    i32.const 4
(;@7e7   ;)    i32.store
(;@7ea   ;)    local.get 6
(;@7ec   ;)    i32.const 36
(;@7ee   ;)    i32.store offset=4
(;@7f1   ;)    local.get 6
(;@7f3   ;)    local.set 6
(;@7f5   ;)    i32.const 12
(;@7f7   ;)    call $alloc
(;@7f9   ;)    local.set 7
(;@7fb   ;)    local.get 7
(;@7fd   ;)    i32.const 5
(;@7ff   ;)    i32.store
(;@802   ;)    local.get 7
(;@804   ;)    i32.const 37
(;@806   ;)    i32.store offset=4
(;@809   ;)    local.get 7
(;@80b   ;)    local.get 3
(;@80d   ;)    i32.store offset=8
(;@810   ;)    local.get 7
(;@812   ;)    local.set 7
(;@814   ;)    local.get 2
(;@816   ;)    local.get 5
(;@818   ;)    local.get 6
(;@81a   ;)    local.get 7
(;@81c   ;)    local.get 4
(;@81e   ;)    call $__mon_prompt
(;@820   ;)    return
             )
(;@823   ;)  (func $f_lam_24 (;39;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@824   ;)    (local i32 i32)
(;@826   ;)    i32.const 20
(;@828   ;)    call $alloc
(;@82a   ;)    local.set 4
(;@82c   ;)    local.get 4
(;@82e   ;)    i32.const 11
(;@830   ;)    i32.store
(;@833   ;)    local.get 4
(;@835   ;)    i32.const 38
(;@837   ;)    i32.store offset=4
(;@83a   ;)    local.get 4
(;@83c   ;)    local.get 0
(;@83e   ;)    i32.store offset=8
(;@841   ;)    local.get 4
(;@843   ;)    local.get 1
(;@845   ;)    i32.store offset=12
(;@848   ;)    local.get 4
(;@84a   ;)    local.get 2
(;@84c   ;)    i32.store offset=16
(;@84f   ;)    local.get 4
(;@851   ;)    local.set 4
(;@853   ;)    local.get 3
(;@855   ;)    i32.const 4
(;@857   ;)    i32.add
(;@858   ;)    local.get 4
(;@85a   ;)    local.get 3
(;@85c   ;)    i32.load
(;@85f   ;)    call_indirect (type $fun_2_1)
(;@862   ;)    return
             )
(;@865   ;)  (func $f_lam_25 (;40;) (type $fun_6_1) (param i32 i32 i32 i32 i32 i32) (result i32)
(;@866   ;)    (local i32 i32 i32 i32)
(;@868   ;)    local.get 3
(;@86a   ;)    i32.load offset=8
(;@86d   ;)    local.set 6
(;@86f   ;)    local.get 6
(;@871   ;)    i32.const 4
(;@873   ;)    i32.add
(;@874   ;)    local.get 4
(;@876   ;)    local.get 6
(;@878   ;)    i32.load
(;@87b   ;)    call_indirect (type $fun_2_1)
(;@87e   ;)    local.set 7
(;@880   ;)    i32.const 20
(;@882   ;)    call $alloc
(;@884   ;)    local.set 8
(;@886   ;)    local.get 8
(;@888   ;)    i32.const 10
(;@88a   ;)    i32.store
(;@88d   ;)    local.get 8
(;@88f   ;)    i32.const 39
(;@891   ;)    i32.store offset=4
(;@894   ;)    local.get 8
(;@896   ;)    local.get 0
(;@898   ;)    i32.store offset=8
(;@89b   ;)    local.get 8
(;@89d   ;)    local.get 1
(;@89f   ;)    i32.store offset=12
(;@8a2   ;)    local.get 8
(;@8a4   ;)    local.get 2
(;@8a6   ;)    i32.store offset=16
(;@8a9   ;)    local.get 8
(;@8ab   ;)    local.set 8
(;@8ad   ;)    local.get 7
(;@8af   ;)    local.get 8
(;@8b1   ;)    local.get 5
(;@8b3   ;)    call $__mon_bind
(;@8b5   ;)    return
             )
(;@8b9   ;)  (func $f_lam_26 (;41;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@8ba   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@8bc   ;)    i32.const 12
(;@8be   ;)    call $alloc
(;@8c0   ;)    local.set 4
(;@8c2   ;)    local.get 4
(;@8c4   ;)    i32.const 7
(;@8c6   ;)    i32.store
(;@8c9   ;)    local.get 4
(;@8cb   ;)    i32.const 16
(;@8cd   ;)    i32.store offset=4
(;@8d0   ;)    local.get 4
(;@8d2   ;)    local.get 2
(;@8d4   ;)    i32.store offset=8
(;@8d7   ;)    local.get 4
(;@8d9   ;)    local.set 4
(;@8db   ;)    i32.const 8
(;@8dd   ;)    call $alloc
(;@8df   ;)    local.set 5
(;@8e1   ;)    local.get 5
(;@8e3   ;)    local.get 3
(;@8e5   ;)    i32.store
(;@8e8   ;)    local.get 5
(;@8ea   ;)    local.get 4
(;@8ec   ;)    i32.store offset=4
(;@8ef   ;)    local.get 5
(;@8f1   ;)    local.set 5
(;@8f3   ;)    local.get 1
(;@8f5   ;)    i32.load
(;@8f8   ;)    local.set 6
(;@8fa   ;)    local.get 6
(;@8fc   ;)    i32.const 4
(;@8fe   ;)    i32.add
(;@8ff   ;)    local.get 4
(;@901   ;)    local.get 5
(;@903   ;)    local.get 6
(;@905   ;)    i32.load
(;@908   ;)    call_indirect (type $fun_3_1)
(;@90b   ;)    local.set 7
(;@90d   ;)    local.get 1
(;@90f   ;)    i32.load offset=12
(;@912   ;)    local.set 8
(;@914   ;)    local.get 8
(;@916   ;)    i32.load
(;@919   ;)    local.set 9
(;@91b   ;)    local.get 9
(;@91d   ;)    i32.const 4
(;@91f   ;)    i32.add
(;@920   ;)    local.get 7
(;@922   ;)    local.get 9
(;@924   ;)    i32.load
(;@927   ;)    call_indirect (type $fun_2_1)
(;@92a   ;)    local.set 10
(;@92c   ;)    local.get 10
(;@92e   ;)    i32.load
(;@931   ;)    local.set 11
(;@933   ;)    local.get 3
(;@935   ;)    local.get 11
(;@937   ;)    call $__mon_eqm
(;@939   ;)    local.set 12
(;@93b   ;)    local.get 12
(;@93d   ;)    i32.load
(;@940   ;)    local.set 13
(;@942   ;)    block (result i32) ;; label = @1
(;@944   ;)      block ;; label = @2
(;@946   ;)        block ;; label = @3
(;@948   ;)          block ;; label = @4
(;@94a   ;)            local.get 13
(;@94c   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@951   ;)          end
(;@952   ;)          local.get 12
(;@954   ;)          i32.load offset=4
(;@957   ;)          local.set 14
(;@959   ;)          local.get 10
(;@95b   ;)          i32.load
(;@95e   ;)          local.set 15
(;@960   ;)          i32.const 12
(;@962   ;)          call $alloc
(;@964   ;)          local.set 16
(;@966   ;)          local.get 16
(;@968   ;)          i32.const 5
(;@96a   ;)          i32.store
(;@96d   ;)          local.get 16
(;@96f   ;)          i32.const 20
(;@971   ;)          i32.store offset=4
(;@974   ;)          local.get 16
(;@976   ;)          local.get 10
(;@978   ;)          i32.store offset=8
(;@97b   ;)          local.get 16
(;@97d   ;)          local.set 16
(;@97f   ;)          i32.const 20
(;@981   ;)          call $alloc
(;@983   ;)          local.set 17
(;@985   ;)          local.get 17
(;@987   ;)          i32.const 11
(;@989   ;)          i32.store
(;@98c   ;)          local.get 17
(;@98e   ;)          i32.const 26
(;@990   ;)          i32.store offset=4
(;@993   ;)          local.get 17
(;@995   ;)          local.get 1
(;@997   ;)          i32.store offset=8
(;@99a   ;)          local.get 17
(;@99c   ;)          local.get 2
(;@99e   ;)          i32.store offset=12
(;@9a1   ;)          local.get 17
(;@9a3   ;)          local.get 3
(;@9a5   ;)          i32.store offset=16
(;@9a8   ;)          local.get 17
(;@9aa   ;)          local.set 17
(;@9ac   ;)          i32.const 12
(;@9ae   ;)          call $alloc
(;@9b0   ;)          local.set 18
(;@9b2   ;)          local.get 18
(;@9b4   ;)          local.get 15
(;@9b6   ;)          i32.store
(;@9b9   ;)          local.get 18
(;@9bb   ;)          local.get 16
(;@9bd   ;)          i32.store offset=4
(;@9c0   ;)          local.get 18
(;@9c2   ;)          local.get 17
(;@9c4   ;)          i32.store offset=8
(;@9c7   ;)          local.get 18
(;@9c9   ;)          local.set 18
(;@9cb   ;)          i32.const 8
(;@9cd   ;)          call $alloc
(;@9cf   ;)          local.set 19
(;@9d1   ;)          local.get 19
(;@9d3   ;)          i32.const 1
(;@9d5   ;)          i32.store
(;@9d8   ;)          local.get 19
(;@9da   ;)          local.get 18
(;@9dc   ;)          i32.store offset=4
(;@9df   ;)          local.get 19
(;@9e1   ;)          br 2 (;@1;)
(;@9e3   ;)        end
(;@9e4   ;)        local.get 12
(;@9e6   ;)        i32.load offset=4
(;@9e9   ;)        local.set 19
(;@9eb   ;)        i32.const 0
(;@9ed   ;)        call $alloc
(;@9ef   ;)        local.set 20
(;@9f1   ;)        local.get 20
(;@9f3   ;)        local.set 20
(;@9f5   ;)        local.get 10
(;@9f7   ;)        i32.load offset=4
(;@9fa   ;)        local.set 21
(;@9fc   ;)        local.get 21
(;@9fe   ;)        i32.const 4
(;@a00   ;)        i32.add
(;@a01   ;)        local.get 20
(;@a03   ;)        local.get 4
(;@a05   ;)        local.get 21
(;@a07   ;)        i32.load
(;@a0a   ;)        call_indirect (type $fun_3_1)
(;@a0d   ;)        local.set 22
(;@a0f   ;)        local.get 22
(;@a11   ;)        i32.load
(;@a14   ;)        local.set 23
(;@a16   ;)        block (result i32) ;; label = @3
(;@a18   ;)          block ;; label = @4
(;@a1a   ;)            block ;; label = @5
(;@a1c   ;)              block ;; label = @6
(;@a1e   ;)                local.get 23
(;@a20   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@a25   ;)              end
(;@a26   ;)              local.get 22
(;@a28   ;)              i32.load offset=4
(;@a2b   ;)              local.set 24
(;@a2d   ;)              i32.const 20
(;@a2f   ;)              call $alloc
(;@a31   ;)              local.set 25
(;@a33   ;)              local.get 25
(;@a35   ;)              i32.const 11
(;@a37   ;)              i32.store
(;@a3a   ;)              local.get 25
(;@a3c   ;)              i32.const 32
(;@a3e   ;)              i32.store offset=4
(;@a41   ;)              local.get 25
(;@a43   ;)              local.get 1
(;@a45   ;)              i32.store offset=8
(;@a48   ;)              local.get 25
(;@a4a   ;)              local.get 2
(;@a4c   ;)              i32.store offset=12
(;@a4f   ;)              local.get 25
(;@a51   ;)              local.get 3
(;@a53   ;)              i32.store offset=16
(;@a56   ;)              local.get 25
(;@a58   ;)              local.set 25
(;@a5a   ;)              local.get 24
(;@a5c   ;)              i32.const 4
(;@a5e   ;)              i32.add
(;@a5f   ;)              local.get 25
(;@a61   ;)              local.get 4
(;@a63   ;)              local.get 24
(;@a65   ;)              i32.load
(;@a68   ;)              call_indirect (type $fun_3_1)
(;@a6b   ;)              br 2 (;@3;)
(;@a6d   ;)            end
(;@a6e   ;)            local.get 22
(;@a70   ;)            i32.load offset=4
(;@a73   ;)            local.set 26
(;@a75   ;)            local.get 26
(;@a77   ;)            local.set 27
(;@a79   ;)            local.get 27
(;@a7b   ;)            i32.load
(;@a7e   ;)            local.set 28
(;@a80   ;)            local.get 27
(;@a82   ;)            i32.load offset=4
(;@a85   ;)            local.set 29
(;@a87   ;)            i32.const 24
(;@a89   ;)            call $alloc
(;@a8b   ;)            local.set 30
(;@a8d   ;)            local.get 30
(;@a8f   ;)            i32.const 13
(;@a91   ;)            i32.store
(;@a94   ;)            local.get 30
(;@a96   ;)            i32.const 40
(;@a98   ;)            i32.store offset=4
(;@a9b   ;)            local.get 30
(;@a9d   ;)            local.get 1
(;@a9f   ;)            i32.store offset=8
(;@aa2   ;)            local.get 30
(;@aa4   ;)            local.get 2
(;@aa6   ;)            i32.store offset=12
(;@aa9   ;)            local.get 30
(;@aab   ;)            local.get 3
(;@aad   ;)            i32.store offset=16
(;@ab0   ;)            local.get 30
(;@ab2   ;)            local.get 27
(;@ab4   ;)            i32.store offset=20
(;@ab7   ;)            local.get 30
(;@ab9   ;)            local.set 30
(;@abb   ;)            i32.const 12
(;@abd   ;)            call $alloc
(;@abf   ;)            local.set 31
(;@ac1   ;)            local.get 31
(;@ac3   ;)            local.get 28
(;@ac5   ;)            i32.store
(;@ac8   ;)            local.get 31
(;@aca   ;)            local.get 29
(;@acc   ;)            i32.store offset=4
(;@acf   ;)            local.get 31
(;@ad1   ;)            local.get 30
(;@ad3   ;)            i32.store offset=8
(;@ad6   ;)            local.get 31
(;@ad8   ;)            local.set 31
(;@ada   ;)            i32.const 8
(;@adc   ;)            call $alloc
(;@ade   ;)            local.set 32
(;@ae0   ;)            local.get 32
(;@ae2   ;)            i32.const 1
(;@ae4   ;)            i32.store
(;@ae7   ;)            local.get 32
(;@ae9   ;)            local.get 31
(;@aeb   ;)            i32.store offset=4
(;@aee   ;)            local.get 32
(;@af0   ;)            br 1 (;@3;)
(;@af2   ;)          end
(;@af3   ;)          unreachable
(;@af4   ;)        end
(;@af5   ;)        br 1 (;@1;)
(;@af7   ;)      end
(;@af8   ;)      unreachable
(;@af9   ;)    end
(;@afa   ;)    return
             )
(;@afd   ;)  (func $f_lam_27 (;42;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@afe   ;)    (local i32 i32)
(;@b00   ;)    i32.const 0
(;@b02   ;)    call $alloc
(;@b04   ;)    local.set 2
(;@b06   ;)    local.get 2
(;@b08   ;)    local.set 2
(;@b0a   ;)    local.get 2
(;@b0c   ;)    call $__mon_generate_marker
(;@b0e   ;)    local.set 3
(;@b10   ;)    i32.const 24
(;@b12   ;)    call $alloc
(;@b14   ;)    local.set 4
(;@b16   ;)    local.get 4
(;@b18   ;)    i32.const 12
(;@b1a   ;)    i32.store
(;@b1d   ;)    local.get 4
(;@b1f   ;)    i32.const 41
(;@b21   ;)    i32.store offset=4
(;@b24   ;)    local.get 4
(;@b26   ;)    local.get 1
(;@b28   ;)    i32.store offset=8
(;@b2b   ;)    local.get 4
(;@b2d   ;)    local.get 1
(;@b2f   ;)    i32.store offset=12
(;@b32   ;)    local.get 4
(;@b34   ;)    local.get 2
(;@b36   ;)    i32.store offset=16
(;@b39   ;)    local.get 4
(;@b3b   ;)    local.get 3
(;@b3d   ;)    i32.store offset=20
(;@b40   ;)    local.get 4
(;@b42   ;)    return
             )
(;@b46   ;)  (func $main (;43;) (type $fun_0_1) (result i32)
(;@b47   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@b49   ;)    i32.const 8
(;@b4b   ;)    call $alloc
(;@b4d   ;)    local.set 0
(;@b4f   ;)    local.get 0
(;@b51   ;)    i32.const 3
(;@b53   ;)    i32.store
(;@b56   ;)    local.get 0
(;@b58   ;)    i32.const 50
(;@b5a   ;)    i32.store offset=4
(;@b5d   ;)    local.get 0
(;@b5f   ;)    local.set 0
(;@b61   ;)    i32.const 0
(;@b63   ;)    call $alloc
(;@b65   ;)    local.set 1
(;@b67   ;)    local.get 1
(;@b69   ;)    local.set 1
(;@b6b   ;)    local.get 0
(;@b6d   ;)    i32.const 4
(;@b6f   ;)    i32.add
(;@b70   ;)    local.get 1
(;@b72   ;)    local.get 0
(;@b74   ;)    i32.load
(;@b77   ;)    call_indirect (type $fun_2_1)
(;@b7a   ;)    local.set 2
(;@b7c   ;)    local.get 2
(;@b7e   ;)    i32.load
(;@b81   ;)    local.set 3
(;@b83   ;)    block (result i32) ;; label = @1
(;@b85   ;)      block ;; label = @2
(;@b87   ;)        block ;; label = @3
(;@b89   ;)          block ;; label = @4
(;@b8b   ;)            local.get 3
(;@b8d   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@b92   ;)          end
(;@b93   ;)          local.get 2
(;@b95   ;)          i32.load offset=4
(;@b98   ;)          local.set 4
(;@b9a   ;)          i32.const 0
(;@b9c   ;)          call $alloc
(;@b9e   ;)          local.set 5
(;@ba0   ;)          local.get 5
(;@ba2   ;)          local.set 5
(;@ba4   ;)          local.get 4
(;@ba6   ;)          i32.const 4
(;@ba8   ;)          i32.add
(;@ba9   ;)          i32.const 16777215
(;@bae   ;)          local.get 5
(;@bb0   ;)          local.get 4
(;@bb2   ;)          i32.load
(;@bb5   ;)          call_indirect (type $fun_3_1)
(;@bb8   ;)          br 2 (;@1;)
(;@bba   ;)        end
(;@bbb   ;)        local.get 2
(;@bbd   ;)        i32.load offset=4
(;@bc0   ;)        local.set 6
(;@bc2   ;)        local.get 6
(;@bc4   ;)        local.set 7
(;@bc6   ;)        local.get 7
(;@bc8   ;)        i32.load
(;@bcb   ;)        local.set 8
(;@bcd   ;)        local.get 7
(;@bcf   ;)        i32.load offset=4
(;@bd2   ;)        local.set 9
(;@bd4   ;)        i32.const 12
(;@bd6   ;)        call $alloc
(;@bd8   ;)        local.set 10
(;@bda   ;)        local.get 10
(;@bdc   ;)        i32.const 7
(;@bde   ;)        i32.store
(;@be1   ;)        local.get 10
(;@be3   ;)        i32.const 52
(;@be5   ;)        i32.store offset=4
(;@be8   ;)        local.get 10
(;@bea   ;)        local.get 7
(;@bec   ;)        i32.store offset=8
(;@bef   ;)        local.get 10
(;@bf1   ;)        local.set 10
(;@bf3   ;)        i32.const 12
(;@bf5   ;)        call $alloc
(;@bf7   ;)        local.set 11
(;@bf9   ;)        local.get 11
(;@bfb   ;)        local.get 8
(;@bfd   ;)        i32.store
(;@c00   ;)        local.get 11
(;@c02   ;)        local.get 9
(;@c04   ;)        i32.store offset=4
(;@c07   ;)        local.get 11
(;@c09   ;)        local.get 10
(;@c0b   ;)        i32.store offset=8
(;@c0e   ;)        local.get 11
(;@c10   ;)        local.set 11
(;@c12   ;)        i32.const 8
(;@c14   ;)        call $alloc
(;@c16   ;)        local.set 12
(;@c18   ;)        local.get 12
(;@c1a   ;)        i32.const 1
(;@c1c   ;)        i32.store
(;@c1f   ;)        local.get 12
(;@c21   ;)        local.get 11
(;@c23   ;)        i32.store offset=4
(;@c26   ;)        local.get 12
(;@c28   ;)        br 1 (;@1;)
(;@c2a   ;)      end
(;@c2b   ;)      unreachable
(;@c2c   ;)    end
(;@c2d   ;)    local.set 12
(;@c2f   ;)    local.get 12
(;@c31   ;)    i32.load
(;@c34   ;)    local.set 13
(;@c36   ;)    block (result i32) ;; label = @1
(;@c38   ;)      block ;; label = @2
(;@c3a   ;)        block ;; label = @3
(;@c3c   ;)          block ;; label = @4
(;@c3e   ;)            local.get 13
(;@c40   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@c45   ;)          end
(;@c46   ;)          local.get 12
(;@c48   ;)          i32.load offset=4
(;@c4b   ;)          local.set 14
(;@c4d   ;)          local.get 14
(;@c4f   ;)          br 2 (;@1;)
(;@c51   ;)        end
(;@c52   ;)        local.get 12
(;@c54   ;)        i32.load offset=4
(;@c57   ;)        local.set 14
(;@c59   ;)        i32.const 5467
(;@c5c   ;)        br 1 (;@1;)
(;@c5e   ;)      end
(;@c5f   ;)      unreachable
(;@c60   ;)    end
(;@c61   ;)    return
             )
(;@c64   ;)  (func $main_lam_0 (;44;) (type $fun_2_1) (param i32 i32) (result i32)
(;@c65   ;)    (local i32)
(;@c67   ;)    local.get 1
(;@c69   ;)    return
             )
(;@c6c   ;)  (func $main_lam_1 (;45;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@c6d   ;)    (local i32)
(;@c6f   ;)    local.get 1
(;@c71   ;)    i32.const 4
(;@c73   ;)    i32.add
(;@c74   ;)    local.get 2
(;@c76   ;)    local.get 1
(;@c78   ;)    i32.load
(;@c7b   ;)    call_indirect (type $fun_2_1)
(;@c7e   ;)    return
             )
(;@c81   ;)  (func $main_lam_2 (;46;) (type $fun_1_1) (param i32) (result i32)
(;@c82   ;)    (local i32)
(;@c84   ;)    i32.const 0
(;@c86   ;)    call $alloc
(;@c88   ;)    local.set 1
(;@c8a   ;)    local.get 1
(;@c8c   ;)    return
             )
(;@c8f   ;)  (func $main_lam_3 (;47;) (type $fun_1_1) (param i32) (result i32)
(;@c90   ;)    (local i32 i32)
(;@c92   ;)    local.get 0
(;@c94   ;)    i32.load
(;@c97   ;)    local.set 1
(;@c99   ;)    block (result i32) ;; label = @1
(;@c9b   ;)      block ;; label = @2
(;@c9d   ;)        local.get 1
(;@c9f   ;)        br_table 0 (;@2;)
(;@ca2   ;)      end
(;@ca3   ;)      unreachable
(;@ca4   ;)    end
(;@ca5   ;)    return
             )
(;@ca8   ;)  (func $main_lam_4 (;48;) (type $fun_1_1) (param i32) (result i32)
(;@ca9   ;)    (local i32)
(;@cab   ;)    local.get 0
(;@cad   ;)    return
             )
(;@cb0   ;)  (func $main_lam_5 (;49;) (type $fun_1_1) (param i32) (result i32)
(;@cb1   ;)    (local i32)
(;@cb3   ;)    local.get 0
(;@cb5   ;)    return
             )
(;@cb9   ;)  (func $main_lam_6 (;50;) (type $fun_1_1) (param i32) (result i32)
(;@cba   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@cbc   ;)    i32.const 8
(;@cbe   ;)    call $alloc
(;@cc0   ;)    local.set 1
(;@cc2   ;)    local.get 1
(;@cc4   ;)    i32.const 4
(;@cc6   ;)    i32.store
(;@cc9   ;)    local.get 1
(;@ccb   ;)    i32.const 44
(;@ccd   ;)    i32.store offset=4
(;@cd0   ;)    local.get 1
(;@cd2   ;)    local.set 1
(;@cd4   ;)    i32.const 8
(;@cd6   ;)    call $alloc
(;@cd8   ;)    local.set 2
(;@cda   ;)    local.get 2
(;@cdc   ;)    i32.const 6
(;@cde   ;)    i32.store
(;@ce1   ;)    local.get 2
(;@ce3   ;)    i32.const 45
(;@ce5   ;)    i32.store offset=4
(;@ce8   ;)    local.get 2
(;@cea   ;)    local.set 2
(;@cec   ;)    i32.const 8
(;@cee   ;)    call $alloc
(;@cf0   ;)    local.set 3
(;@cf2   ;)    local.get 3
(;@cf4   ;)    i32.const 3
(;@cf6   ;)    i32.store
(;@cf9   ;)    local.get 3
(;@cfb   ;)    i32.const 46
(;@cfd   ;)    i32.store offset=4
(;@d00   ;)    local.get 3
(;@d02   ;)    local.set 3
(;@d04   ;)    i32.const 8
(;@d06   ;)    call $alloc
(;@d08   ;)    local.set 4
(;@d0a   ;)    local.get 4
(;@d0c   ;)    i32.const 3
(;@d0e   ;)    i32.store
(;@d11   ;)    local.get 4
(;@d13   ;)    i32.const 47
(;@d15   ;)    i32.store offset=4
(;@d18   ;)    local.get 4
(;@d1a   ;)    local.set 4
(;@d1c   ;)    i32.const 8
(;@d1e   ;)    call $alloc
(;@d20   ;)    local.set 5
(;@d22   ;)    local.get 5
(;@d24   ;)    local.get 3
(;@d26   ;)    i32.store
(;@d29   ;)    local.get 5
(;@d2b   ;)    local.get 4
(;@d2d   ;)    i32.store offset=4
(;@d30   ;)    local.get 5
(;@d32   ;)    local.set 5
(;@d34   ;)    i32.const 8
(;@d36   ;)    call $alloc
(;@d38   ;)    local.set 6
(;@d3a   ;)    local.get 6
(;@d3c   ;)    i32.const 3
(;@d3e   ;)    i32.store
(;@d41   ;)    local.get 6
(;@d43   ;)    i32.const 48
(;@d45   ;)    i32.store offset=4
(;@d48   ;)    local.get 6
(;@d4a   ;)    local.set 6
(;@d4c   ;)    i32.const 8
(;@d4e   ;)    call $alloc
(;@d50   ;)    local.set 7
(;@d52   ;)    local.get 7
(;@d54   ;)    i32.const 3
(;@d56   ;)    i32.store
(;@d59   ;)    local.get 7
(;@d5b   ;)    i32.const 49
(;@d5d   ;)    i32.store offset=4
(;@d60   ;)    local.get 7
(;@d62   ;)    local.set 7
(;@d64   ;)    i32.const 8
(;@d66   ;)    call $alloc
(;@d68   ;)    local.set 8
(;@d6a   ;)    local.get 8
(;@d6c   ;)    local.get 6
(;@d6e   ;)    i32.store
(;@d71   ;)    local.get 8
(;@d73   ;)    local.get 7
(;@d75   ;)    i32.store offset=4
(;@d78   ;)    local.get 8
(;@d7a   ;)    local.set 8
(;@d7c   ;)    i32.const 16
(;@d7e   ;)    call $alloc
(;@d80   ;)    local.set 9
(;@d82   ;)    local.get 9
(;@d84   ;)    local.get 1
(;@d86   ;)    i32.store
(;@d89   ;)    local.get 9
(;@d8b   ;)    local.get 2
(;@d8d   ;)    i32.store offset=4
(;@d90   ;)    local.get 9
(;@d92   ;)    local.get 5
(;@d94   ;)    i32.store offset=8
(;@d97   ;)    local.get 9
(;@d99   ;)    local.get 8
(;@d9b   ;)    i32.store offset=12
(;@d9e   ;)    local.get 9
(;@da0   ;)    local.set 9
(;@da2   ;)    local.get 9
(;@da4   ;)    local.get 0
(;@da6   ;)    call $f
(;@da8   ;)    return
             )
(;@dab   ;)  (func $main_lam_7 (;51;) (type $fun_1_1) (param i32) (result i32)
(;@dac   ;)    (local i32)
(;@dae   ;)    local.get 0
(;@db0   ;)    i32.const 4
(;@db2   ;)    i32.add
(;@db3   ;)    i32.const 16777215
(;@db8   ;)    local.get 0
(;@dba   ;)    i32.load
(;@dbd   ;)    call_indirect (type $fun_2_1)
(;@dc0   ;)    return
             )
(;@dc3   ;)  (func $main_lam_8 (;52;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@dc4   ;)    (local i32 i32 i32 i32)
(;@dc6   ;)    local.get 0
(;@dc8   ;)    i32.load offset=8
(;@dcb   ;)    local.set 3
(;@dcd   ;)    local.get 3
(;@dcf   ;)    i32.const 4
(;@dd1   ;)    i32.add
(;@dd2   ;)    local.get 1
(;@dd4   ;)    local.get 3
(;@dd6   ;)    i32.load
(;@dd9   ;)    call_indirect (type $fun_2_1)
(;@ddc   ;)    local.set 4
(;@dde   ;)    i32.const 8
(;@de0   ;)    call $alloc
(;@de2   ;)    local.set 5
(;@de4   ;)    local.get 5
(;@de6   ;)    i32.const 3
(;@de8   ;)    i32.store
(;@deb   ;)    local.get 5
(;@ded   ;)    i32.const 51
(;@def   ;)    i32.store offset=4
(;@df2   ;)    local.get 5
(;@df4   ;)    local.set 5
(;@df6   ;)    local.get 4
(;@df8   ;)    local.get 5
(;@dfa   ;)    local.get 2
(;@dfc   ;)    call $__mon_bind
(;@dfe   ;)    return
             )
(;@e02   ;)  (func $__mon_bind (;53;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@e03   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@e05   ;)    local.get 0
(;@e07   ;)    i32.const 4
(;@e09   ;)    i32.add
(;@e0a   ;)    local.get 2
(;@e0c   ;)    local.get 0
(;@e0e   ;)    i32.load
(;@e11   ;)    call_indirect (type $fun_2_1)
(;@e14   ;)    local.set 3
(;@e16   ;)    local.get 3
(;@e18   ;)    i32.load
(;@e1b   ;)    local.set 4
(;@e1d   ;)    block (result i32) ;; label = @1
(;@e1f   ;)      block ;; label = @2
(;@e21   ;)        block ;; label = @3
(;@e23   ;)          block ;; label = @4
(;@e25   ;)            local.get 4
(;@e27   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@e2c   ;)          end
(;@e2d   ;)          local.get 3
(;@e2f   ;)          i32.load offset=4
(;@e32   ;)          local.set 5
(;@e34   ;)          local.get 1
(;@e36   ;)          i32.const 4
(;@e38   ;)          i32.add
(;@e39   ;)          local.get 5
(;@e3b   ;)          local.get 2
(;@e3d   ;)          local.get 1
(;@e3f   ;)          i32.load
(;@e42   ;)          call_indirect (type $fun_3_1)
(;@e45   ;)          br 2 (;@1;)
(;@e47   ;)        end
(;@e48   ;)        local.get 3
(;@e4a   ;)        i32.load offset=4
(;@e4d   ;)        local.set 6
(;@e4f   ;)        local.get 6
(;@e51   ;)        local.set 7
(;@e53   ;)        local.get 7
(;@e55   ;)        i32.load
(;@e58   ;)        local.set 8
(;@e5a   ;)        local.get 7
(;@e5c   ;)        i32.load offset=4
(;@e5f   ;)        local.set 9
(;@e61   ;)        i32.const 16
(;@e63   ;)        call $alloc
(;@e65   ;)        local.set 10
(;@e67   ;)        local.get 10
(;@e69   ;)        i32.const 8
(;@e6b   ;)        i32.store
(;@e6e   ;)        local.get 10
(;@e70   ;)        i32.const 54
(;@e72   ;)        i32.store offset=4
(;@e75   ;)        local.get 10
(;@e77   ;)        local.get 1
(;@e79   ;)        i32.store offset=8
(;@e7c   ;)        local.get 10
(;@e7e   ;)        local.get 7
(;@e80   ;)        i32.store offset=12
(;@e83   ;)        local.get 10
(;@e85   ;)        local.set 10
(;@e87   ;)        i32.const 12
(;@e89   ;)        call $alloc
(;@e8b   ;)        local.set 11
(;@e8d   ;)        local.get 11
(;@e8f   ;)        local.get 8
(;@e91   ;)        i32.store
(;@e94   ;)        local.get 11
(;@e96   ;)        local.get 9
(;@e98   ;)        i32.store offset=4
(;@e9b   ;)        local.get 11
(;@e9d   ;)        local.get 10
(;@e9f   ;)        i32.store offset=8
(;@ea2   ;)        local.get 11
(;@ea4   ;)        local.set 11
(;@ea6   ;)        i32.const 8
(;@ea8   ;)        call $alloc
(;@eaa   ;)        local.set 12
(;@eac   ;)        local.get 12
(;@eae   ;)        i32.const 1
(;@eb0   ;)        i32.store
(;@eb3   ;)        local.get 12
(;@eb5   ;)        local.get 11
(;@eb7   ;)        i32.store offset=4
(;@eba   ;)        local.get 12
(;@ebc   ;)        br 1 (;@1;)
(;@ebe   ;)      end
(;@ebf   ;)      unreachable
(;@ec0   ;)    end
(;@ec1   ;)    return
             )
(;@ec4   ;)  (func $__mon_bind_lam_0 (;54;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@ec5   ;)    (local i32 i32 i32)
(;@ec7   ;)    local.get 1
(;@ec9   ;)    i32.load offset=8
(;@ecc   ;)    local.set 3
(;@ece   ;)    local.get 3
(;@ed0   ;)    i32.const 4
(;@ed2   ;)    i32.add
(;@ed3   ;)    local.get 2
(;@ed5   ;)    local.get 3
(;@ed7   ;)    i32.load
(;@eda   ;)    call_indirect (type $fun_2_1)
(;@edd   ;)    local.set 4
(;@edf   ;)    i32.const 16
(;@ee1   ;)    call $alloc
(;@ee3   ;)    local.set 5
(;@ee5   ;)    local.get 5
(;@ee7   ;)    i32.const 8
(;@ee9   ;)    i32.store
(;@eec   ;)    local.get 5
(;@eee   ;)    i32.const 53
(;@ef0   ;)    i32.store offset=4
(;@ef3   ;)    local.get 5
(;@ef5   ;)    local.get 4
(;@ef7   ;)    i32.store offset=8
(;@efa   ;)    local.get 5
(;@efc   ;)    local.get 0
(;@efe   ;)    i32.store offset=12
(;@f01   ;)    local.get 5
(;@f03   ;)    return
             )
(;@f07   ;)  (func $__mon_prompt (;55;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@f08   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@f0a   ;)    local.get 1
(;@f0c   ;)    i32.const 4
(;@f0e   ;)    i32.add
(;@f0f   ;)    local.get 4
(;@f11   ;)    local.get 1
(;@f13   ;)    i32.load
(;@f16   ;)    call_indirect (type $fun_2_1)
(;@f19   ;)    local.set 5
(;@f1b   ;)    local.get 3
(;@f1d   ;)    i32.const 4
(;@f1f   ;)    i32.add
(;@f20   ;)    local.get 5
(;@f22   ;)    local.get 3
(;@f24   ;)    i32.load
(;@f27   ;)    call_indirect (type $fun_2_1)
(;@f2a   ;)    local.set 6
(;@f2c   ;)    local.get 6
(;@f2e   ;)    i32.load
(;@f31   ;)    local.set 7
(;@f33   ;)    block (result i32) ;; label = @1
(;@f35   ;)      block ;; label = @2
(;@f37   ;)        block ;; label = @3
(;@f39   ;)          block ;; label = @4
(;@f3b   ;)            local.get 7
(;@f3d   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@f42   ;)          end
(;@f43   ;)          local.get 6
(;@f45   ;)          i32.load offset=4
(;@f48   ;)          local.set 8
(;@f4a   ;)          local.get 2
(;@f4c   ;)          i32.const 4
(;@f4e   ;)          i32.add
(;@f4f   ;)          local.get 8
(;@f51   ;)          local.get 4
(;@f53   ;)          local.get 2
(;@f55   ;)          i32.load
(;@f58   ;)          call_indirect (type $fun_3_1)
(;@f5b   ;)          br 2 (;@1;)
(;@f5d   ;)        end
(;@f5e   ;)        local.get 6
(;@f60   ;)        i32.load offset=4
(;@f63   ;)        local.set 9
(;@f65   ;)        local.get 9
(;@f67   ;)        i32.load
(;@f6a   ;)        local.set 10
(;@f6c   ;)        local.get 0
(;@f6e   ;)        local.get 10
(;@f70   ;)        call $__mon_eqm
(;@f72   ;)        local.set 11
(;@f74   ;)        local.get 11
(;@f76   ;)        i32.load
(;@f79   ;)        local.set 12
(;@f7b   ;)        block (result i32) ;; label = @3
(;@f7d   ;)          block ;; label = @4
(;@f7f   ;)            block ;; label = @5
(;@f81   ;)              block ;; label = @6
(;@f83   ;)                local.get 12
(;@f85   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@f8a   ;)              end
(;@f8b   ;)              local.get 11
(;@f8d   ;)              i32.load offset=4
(;@f90   ;)              local.set 13
(;@f92   ;)              local.get 9
(;@f94   ;)              local.set 14
(;@f96   ;)              local.get 14
(;@f98   ;)              i32.load
(;@f9b   ;)              local.set 15
(;@f9d   ;)              local.get 14
(;@f9f   ;)              i32.load offset=4
(;@fa2   ;)              local.set 16
(;@fa4   ;)              i32.const 24
(;@fa6   ;)              call $alloc
(;@fa8   ;)              local.set 17
(;@faa   ;)              local.get 17
(;@fac   ;)              i32.const 13
(;@fae   ;)              i32.store
(;@fb1   ;)              local.get 17
(;@fb3   ;)              i32.const 56
(;@fb5   ;)              i32.store offset=4
(;@fb8   ;)              local.get 17
(;@fba   ;)              local.get 0
(;@fbc   ;)              i32.store offset=8
(;@fbf   ;)              local.get 17
(;@fc1   ;)              local.get 1
(;@fc3   ;)              i32.store offset=12
(;@fc6   ;)              local.get 17
(;@fc8   ;)              local.get 2
(;@fca   ;)              i32.store offset=16
(;@fcd   ;)              local.get 17
(;@fcf   ;)              local.get 14
(;@fd1   ;)              i32.store offset=20
(;@fd4   ;)              local.get 17
(;@fd6   ;)              local.set 17
(;@fd8   ;)              i32.const 12
(;@fda   ;)              call $alloc
(;@fdc   ;)              local.set 18
(;@fde   ;)              local.get 18
(;@fe0   ;)              local.get 15
(;@fe2   ;)              i32.store
(;@fe5   ;)              local.get 18
(;@fe7   ;)              local.get 16
(;@fe9   ;)              i32.store offset=4
(;@fec   ;)              local.get 18
(;@fee   ;)              local.get 17
(;@ff0   ;)              i32.store offset=8
(;@ff3   ;)              local.get 18
(;@ff5   ;)              local.set 18
(;@ff7   ;)              i32.const 8
(;@ff9   ;)              call $alloc
(;@ffb   ;)              local.set 19
(;@ffd   ;)              local.get 19
(;@fff   ;)              i32.const 1
(;@1001  ;)              i32.store
(;@1004  ;)              local.get 19
(;@1006  ;)              local.get 18
(;@1008  ;)              i32.store offset=4
(;@100b  ;)              local.get 19
(;@100d  ;)              br 2 (;@3;)
(;@100f  ;)            end
(;@1010  ;)            local.get 11
(;@1012  ;)            i32.load offset=4
(;@1015  ;)            local.set 13
(;@1017  ;)            i32.const 24
(;@1019  ;)            call $alloc
(;@101b  ;)            local.set 19
(;@101d  ;)            local.get 19
(;@101f  ;)            i32.const 13
(;@1021  ;)            i32.store
(;@1024  ;)            local.get 19
(;@1026  ;)            i32.const 57
(;@1028  ;)            i32.store offset=4
(;@102b  ;)            local.get 19
(;@102d  ;)            local.get 0
(;@102f  ;)            i32.store offset=8
(;@1032  ;)            local.get 19
(;@1034  ;)            local.get 1
(;@1036  ;)            i32.store offset=12
(;@1039  ;)            local.get 19
(;@103b  ;)            local.get 2
(;@103d  ;)            i32.store offset=16
(;@1040  ;)            local.get 19
(;@1042  ;)            local.get 9
(;@1044  ;)            i32.store offset=20
(;@1047  ;)            local.get 19
(;@1049  ;)            local.set 19
(;@104b  ;)            local.get 9
(;@104d  ;)            i32.load offset=4
(;@1050  ;)            local.set 20
(;@1052  ;)            local.get 20
(;@1054  ;)            i32.const 4
(;@1056  ;)            i32.add
(;@1057  ;)            local.get 19
(;@1059  ;)            local.get 4
(;@105b  ;)            local.get 20
(;@105d  ;)            i32.load
(;@1060  ;)            call_indirect (type $fun_3_1)
(;@1063  ;)            br 1 (;@3;)
(;@1065  ;)          end
(;@1066  ;)          unreachable
(;@1067  ;)        end
(;@1068  ;)        br 1 (;@1;)
(;@106a  ;)      end
(;@106b  ;)      unreachable
(;@106c  ;)    end
(;@106d  ;)    return
             )
(;@1070  ;)  (func $__mon_prompt_lam_0 (;56;) (type $fun_6_1) (param i32 i32 i32 i32 i32 i32) (result i32)
(;@1071  ;)    (local i32 i32 i32)
(;@1073  ;)    local.get 3
(;@1075  ;)    i32.load offset=8
(;@1078  ;)    local.set 6
(;@107a  ;)    local.get 6
(;@107c  ;)    i32.const 4
(;@107e  ;)    i32.add
(;@107f  ;)    local.get 4
(;@1081  ;)    local.get 6
(;@1083  ;)    i32.load
(;@1086  ;)    call_indirect (type $fun_2_1)
(;@1089  ;)    local.set 7
(;@108b  ;)    local.get 0
(;@108d  ;)    local.get 1
(;@108f  ;)    local.get 2
(;@1091  ;)    local.get 7
(;@1093  ;)    local.get 5
(;@1095  ;)    call $__mon_prompt
(;@1097  ;)    return
             )
(;@109a  ;)  (func $__mon_prompt_lam_1 (;57;) (type $fun_6_1) (param i32 i32 i32 i32 i32 i32) (result i32)
(;@109b  ;)    (local i32 i32 i32)
(;@109d  ;)    local.get 3
(;@109f  ;)    i32.load offset=8
(;@10a2  ;)    local.set 6
(;@10a4  ;)    local.get 6
(;@10a6  ;)    i32.const 4
(;@10a8  ;)    i32.add
(;@10a9  ;)    local.get 4
(;@10ab  ;)    local.get 6
(;@10ad  ;)    i32.load
(;@10b0  ;)    call_indirect (type $fun_2_1)
(;@10b3  ;)    local.set 7
(;@10b5  ;)    local.get 0
(;@10b7  ;)    local.get 1
(;@10b9  ;)    local.get 2
(;@10bb  ;)    local.get 7
(;@10bd  ;)    local.get 5
(;@10bf  ;)    call $__mon_prompt
(;@10c1  ;)    return
             )
(;@7f    ;)  (table (;0;) 58 58 funcref)
(;@86    ;)  (memory (;0;) 1)
(;@8b    ;)  (global (;0;) (mut i32) i32.const 0)
(;@90    ;)  (global (;1;) (mut i32) i32.const 0)
(;@98    ;)  (export "main" (func $main))
(;@9f    ;)  (export "mem" (memory 0))
(;@a8    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_1 $__apply_3_2 $__apply_4_2 $__apply_4_3 $__apply_5_3 $__apply_5_4 $__apply_6_4 $f $f_lam_0 $f_lam_1 $f_lam_2 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_6 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_10 $f_lam_11 $f_lam_12 $f_lam_13 $f_lam_14 $f_lam_15 $f_lam_16 $f_lam_17 $f_lam_18 $f_lam_19 $f_lam_20 $f_lam_21 $f_lam_22 $f_lam_23 $f_lam_24 $f_lam_25 $f_lam_26 $f_lam_27 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
