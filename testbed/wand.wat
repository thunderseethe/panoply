(module $example
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
(;@eb    ;)  (func $__mon_eqm (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@ec    ;)    (local i32)
(;@ee    ;)    i32.const 4
(;@f0    ;)    call $alloc
(;@f2    ;)    local.tee 2
(;@f4    ;)    i32.const 1
(;@f6    ;)    i32.const 0
(;@f8    ;)    local.get 0
(;@fa    ;)    local.get 1
(;@fc    ;)    i32.eq
(;@fd    ;)    select
(;@fe    ;)    i32.store
(;@101   ;)    local.get 2
(;@103   ;)    return
             )
(;@106   ;)  (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
(;@107   ;)    local.get 1
(;@109   ;)    local.get 0
(;@10b   ;)    i32.load
(;@10e   ;)    call_indirect (type $fun_1_1)
             )
(;@113   ;)  (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@114   ;)    local.get 1
(;@116   ;)    local.get 2
(;@118   ;)    local.get 0
(;@11a   ;)    i32.load
(;@11d   ;)    call_indirect (type $fun_2_1)
             )
(;@122   ;)  (func $__apply_2_1 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@123   ;)    local.get 0
(;@125   ;)    i32.load offset=4
(;@128   ;)    local.get 1
(;@12a   ;)    local.get 0
(;@12c   ;)    i32.load
(;@12f   ;)    call_indirect (type $fun_2_1)
             )
(;@134   ;)  (func $__apply_3_2 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
(;@135   ;)    local.get 0
(;@137   ;)    i32.load offset=4
(;@13a   ;)    local.get 0
(;@13c   ;)    i32.load offset=8
(;@13f   ;)    local.get 1
(;@141   ;)    local.get 0
(;@143   ;)    i32.load
(;@146   ;)    call_indirect (type $fun_3_1)
             )
(;@14b   ;)  (func $__apply_4_3 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
(;@14c   ;)    local.get 0
(;@14e   ;)    i32.load offset=4
(;@151   ;)    local.get 0
(;@153   ;)    i32.load offset=8
(;@156   ;)    local.get 0
(;@158   ;)    i32.load offset=12
(;@15b   ;)    local.get 1
(;@15d   ;)    local.get 0
(;@15f   ;)    i32.load
(;@162   ;)    call_indirect (type $fun_4_1)
             )
(;@168   ;)  (func $main (;9;) (type $fun_0_1) (result i32)
(;@169   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@16b   ;)    i32.const 0
(;@16d   ;)    call $alloc
(;@16f   ;)    local.set 0
(;@171   ;)    local.get 0
(;@173   ;)    local.set 0
(;@175   ;)    local.get 0
(;@177   ;)    call $__mon_generate_marker
(;@179   ;)    local.set 1
(;@17b   ;)    i32.const 0
(;@17d   ;)    call $alloc
(;@17f   ;)    local.set 2
(;@181   ;)    local.get 2
(;@183   ;)    local.set 2
(;@185   ;)    local.get 1
(;@187   ;)    local.get 1
(;@189   ;)    call $__mon_eqm
(;@18b   ;)    local.set 3
(;@18d   ;)    local.get 3
(;@18f   ;)    i32.load
(;@192   ;)    local.set 4
(;@194   ;)    block (result i32) ;; label = @1
(;@196   ;)      block ;; label = @2
(;@198   ;)        block ;; label = @3
(;@19a   ;)          block ;; label = @4
(;@19c   ;)            local.get 4
(;@19e   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1a3   ;)          end
(;@1a4   ;)          local.get 3
(;@1a6   ;)          i32.load offset=4
(;@1a9   ;)          local.set 5
(;@1ab   ;)          i32.const 8
(;@1ad   ;)          call $alloc
(;@1af   ;)          local.set 6
(;@1b1   ;)          local.get 6
(;@1b3   ;)          i32.const 4
(;@1b5   ;)          i32.store
(;@1b8   ;)          local.get 6
(;@1ba   ;)          i32.const 10
(;@1bc   ;)          i32.store offset=4
(;@1bf   ;)          local.get 6
(;@1c1   ;)          local.set 6
(;@1c3   ;)          i32.const 12
(;@1c5   ;)          call $alloc
(;@1c7   ;)          local.set 7
(;@1c9   ;)          local.get 7
(;@1cb   ;)          i32.const 6
(;@1cd   ;)          i32.store
(;@1d0   ;)          local.get 7
(;@1d2   ;)          i32.const 15
(;@1d4   ;)          i32.store offset=4
(;@1d7   ;)          local.get 7
(;@1d9   ;)          local.get 1
(;@1db   ;)          i32.store offset=8
(;@1de   ;)          local.get 7
(;@1e0   ;)          local.set 7
(;@1e2   ;)          i32.const 8
(;@1e4   ;)          call $alloc
(;@1e6   ;)          local.set 8
(;@1e8   ;)          local.get 8
(;@1ea   ;)          i32.const 4
(;@1ec   ;)          i32.store
(;@1ef   ;)          local.get 8
(;@1f1   ;)          i32.const 18
(;@1f3   ;)          i32.store offset=4
(;@1f6   ;)          local.get 8
(;@1f8   ;)          local.set 8
(;@1fa   ;)          i32.const 16
(;@1fc   ;)          call $alloc
(;@1fe   ;)          local.set 9
(;@200   ;)          local.get 9
(;@202   ;)          local.get 1
(;@204   ;)          i32.store
(;@207   ;)          local.get 9
(;@209   ;)          local.get 6
(;@20b   ;)          i32.store offset=4
(;@20e   ;)          local.get 9
(;@210   ;)          local.get 7
(;@212   ;)          i32.store offset=8
(;@215   ;)          local.get 9
(;@217   ;)          local.get 8
(;@219   ;)          i32.store offset=12
(;@21c   ;)          local.get 9
(;@21e   ;)          local.set 9
(;@220   ;)          i32.const 8
(;@222   ;)          call $alloc
(;@224   ;)          local.set 10
(;@226   ;)          local.get 10
(;@228   ;)          i32.const 1
(;@22a   ;)          i32.store
(;@22d   ;)          local.get 10
(;@22f   ;)          local.get 9
(;@231   ;)          i32.store offset=4
(;@234   ;)          local.get 10
(;@236   ;)          br 2 (;@1;)
(;@238   ;)        end
(;@239   ;)        local.get 3
(;@23b   ;)        i32.load offset=4
(;@23e   ;)        local.set 5
(;@240   ;)        i32.const 12
(;@242   ;)        call $alloc
(;@244   ;)        local.set 10
(;@246   ;)        local.get 10
(;@248   ;)        i32.const 6
(;@24a   ;)        i32.store
(;@24d   ;)        local.get 10
(;@24f   ;)        i32.const 20
(;@251   ;)        i32.store offset=4
(;@254   ;)        local.get 10
(;@256   ;)        local.get 1
(;@258   ;)        i32.store offset=8
(;@25b   ;)        local.get 10
(;@25d   ;)        local.set 10
(;@25f   ;)        i32.const 8
(;@261   ;)        call $alloc
(;@263   ;)        local.set 11
(;@265   ;)        local.get 11
(;@267   ;)        i32.const 4
(;@269   ;)        i32.store
(;@26c   ;)        local.get 11
(;@26e   ;)        i32.const 21
(;@270   ;)        i32.store offset=4
(;@273   ;)        local.get 11
(;@275   ;)        local.set 11
(;@277   ;)        i32.const 8
(;@279   ;)        call $alloc
(;@27b   ;)        local.set 12
(;@27d   ;)        local.get 12
(;@27f   ;)        i32.const 5
(;@281   ;)        i32.store
(;@284   ;)        local.get 12
(;@286   ;)        i32.const 22
(;@288   ;)        i32.store offset=4
(;@28b   ;)        local.get 12
(;@28d   ;)        local.set 12
(;@28f   ;)        i32.const 16
(;@291   ;)        call $alloc
(;@293   ;)        local.set 13
(;@295   ;)        local.get 13
(;@297   ;)        i32.const 7
(;@299   ;)        i32.store
(;@29c   ;)        local.get 13
(;@29e   ;)        i32.const 23
(;@2a0   ;)        i32.store offset=4
(;@2a3   ;)        local.get 13
(;@2a5   ;)        local.get 11
(;@2a7   ;)        i32.store offset=8
(;@2aa   ;)        local.get 13
(;@2ac   ;)        local.get 12
(;@2ae   ;)        i32.store offset=12
(;@2b1   ;)        local.get 13
(;@2b3   ;)        local.set 13
(;@2b5   ;)        local.get 1
(;@2b7   ;)        local.get 10
(;@2b9   ;)        local.get 13
(;@2bb   ;)        local.get 2
(;@2bd   ;)        call $__mon_prompt
(;@2bf   ;)        br 1 (;@1;)
(;@2c1   ;)      end
(;@2c2   ;)      unreachable
(;@2c3   ;)    end
(;@2c4   ;)    local.set 14
(;@2c6   ;)    local.get 14
(;@2c8   ;)    i32.load
(;@2cb   ;)    local.set 15
(;@2cd   ;)    block (result i32) ;; label = @1
(;@2cf   ;)      block ;; label = @2
(;@2d1   ;)        block ;; label = @3
(;@2d3   ;)          block ;; label = @4
(;@2d5   ;)            local.get 15
(;@2d7   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2dc   ;)          end
(;@2dd   ;)          local.get 14
(;@2df   ;)          i32.load offset=4
(;@2e2   ;)          local.set 16
(;@2e4   ;)          local.get 16
(;@2e6   ;)          br 2 (;@1;)
(;@2e8   ;)        end
(;@2e9   ;)        local.get 14
(;@2eb   ;)        i32.load offset=4
(;@2ee   ;)        local.set 16
(;@2f0   ;)        i32.const 5467
(;@2f3   ;)        br 1 (;@1;)
(;@2f5   ;)      end
(;@2f6   ;)      unreachable
(;@2f7   ;)    end
(;@2f8   ;)    return
             )
(;@2fb   ;)  (func $main_lam_0 (;10;) (type $fun_1_1) (param i32) (result i32)
(;@2fc   ;)    (local i32)
(;@2fe   ;)    local.get 0
(;@300   ;)    i32.const 4
(;@302   ;)    i32.add
(;@303   ;)    i32.const 52
(;@305   ;)    local.get 0
(;@307   ;)    i32.load
(;@30a   ;)    call_indirect (type $fun_2_1)
(;@30d   ;)    return
             )
(;@310   ;)  (func $main_lam_1 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@311   ;)    (local i32)
(;@313   ;)    local.get 1
(;@315   ;)    i32.const 4
(;@317   ;)    i32.add
(;@318   ;)    i32.const 52
(;@31a   ;)    local.get 1
(;@31c   ;)    i32.load
(;@31f   ;)    call_indirect (type $fun_2_1)
(;@322   ;)    return
             )
(;@325   ;)  (func $main_lam_2 (;12;) (type $fun_2_1) (param i32 i32) (result i32)
(;@326   ;)    (local i32 i32)
(;@328   ;)    i32.const 8
(;@32a   ;)    call $alloc
(;@32c   ;)    local.set 2
(;@32e   ;)    local.get 2
(;@330   ;)    i32.const 5
(;@332   ;)    i32.store
(;@335   ;)    local.get 2
(;@337   ;)    i32.const 11
(;@339   ;)    i32.store offset=4
(;@33c   ;)    local.get 2
(;@33e   ;)    local.set 2
(;@340   ;)    i32.const 8
(;@342   ;)    call $alloc
(;@344   ;)    local.set 3
(;@346   ;)    local.get 3
(;@348   ;)    local.get 0
(;@34a   ;)    i32.store
(;@34d   ;)    local.get 3
(;@34f   ;)    local.get 2
(;@351   ;)    i32.store offset=4
(;@354   ;)    local.get 3
(;@356   ;)    return
             )
(;@359   ;)  (func $main_lam_3 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@35a   ;)    (local i32)
(;@35c   ;)    i32.const 8
(;@35e   ;)    call $alloc
(;@360   ;)    local.set 2
(;@362   ;)    local.get 2
(;@364   ;)    i32.const 0
(;@366   ;)    i32.store
(;@369   ;)    local.get 2
(;@36b   ;)    local.get 0
(;@36d   ;)    i32.store offset=4
(;@370   ;)    local.get 2
(;@372   ;)    return
             )
(;@375   ;)  (func $main_lam_4 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@376   ;)    (local i32)
(;@378   ;)    i32.const 8
(;@37a   ;)    call $alloc
(;@37c   ;)    local.set 2
(;@37e   ;)    local.get 2
(;@380   ;)    i32.const 0
(;@382   ;)    i32.store
(;@385   ;)    local.get 2
(;@387   ;)    local.get 0
(;@389   ;)    i32.store offset=4
(;@38c   ;)    local.get 2
(;@38e   ;)    return
             )
(;@392   ;)  (func $main_lam_5 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@393   ;)    (local i32 i32 i32 i32 i32)
(;@395   ;)    i32.const 12
(;@397   ;)    call $alloc
(;@399   ;)    local.set 2
(;@39b   ;)    local.get 2
(;@39d   ;)    i32.const 6
(;@39f   ;)    i32.store
(;@3a2   ;)    local.get 2
(;@3a4   ;)    i32.const 12
(;@3a6   ;)    i32.store offset=4
(;@3a9   ;)    local.get 2
(;@3ab   ;)    local.get 0
(;@3ad   ;)    i32.store offset=8
(;@3b0   ;)    local.get 2
(;@3b2   ;)    local.set 2
(;@3b4   ;)    i32.const 12
(;@3b6   ;)    call $alloc
(;@3b8   ;)    local.set 3
(;@3ba   ;)    local.get 3
(;@3bc   ;)    i32.const 6
(;@3be   ;)    i32.store
(;@3c1   ;)    local.get 3
(;@3c3   ;)    i32.const 13
(;@3c5   ;)    i32.store offset=4
(;@3c8   ;)    local.get 3
(;@3ca   ;)    local.get 1
(;@3cc   ;)    i32.store offset=8
(;@3cf   ;)    local.get 3
(;@3d1   ;)    local.set 3
(;@3d3   ;)    i32.const 8
(;@3d5   ;)    call $alloc
(;@3d7   ;)    local.set 4
(;@3d9   ;)    local.get 4
(;@3db   ;)    i32.const 5
(;@3dd   ;)    i32.store
(;@3e0   ;)    local.get 4
(;@3e2   ;)    i32.const 14
(;@3e4   ;)    i32.store offset=4
(;@3e7   ;)    local.get 4
(;@3e9   ;)    local.set 4
(;@3eb   ;)    i32.const 16
(;@3ed   ;)    call $alloc
(;@3ef   ;)    local.set 5
(;@3f1   ;)    local.get 5
(;@3f3   ;)    i32.const 7
(;@3f5   ;)    i32.store
(;@3f8   ;)    local.get 5
(;@3fa   ;)    i32.const 23
(;@3fc   ;)    i32.store offset=4
(;@3ff   ;)    local.get 5
(;@401   ;)    local.get 3
(;@403   ;)    i32.store offset=8
(;@406   ;)    local.get 5
(;@408   ;)    local.get 4
(;@40a   ;)    i32.store offset=12
(;@40d   ;)    local.get 5
(;@40f   ;)    local.set 5
(;@411   ;)    i32.const 20
(;@413   ;)    call $alloc
(;@415   ;)    local.set 6
(;@417   ;)    local.get 6
(;@419   ;)    i32.const 8
(;@41b   ;)    i32.store
(;@41e   ;)    local.get 6
(;@420   ;)    i32.const 25
(;@422   ;)    i32.store offset=4
(;@425   ;)    local.get 6
(;@427   ;)    local.get 0
(;@429   ;)    i32.store offset=8
(;@42c   ;)    local.get 6
(;@42e   ;)    local.get 2
(;@430   ;)    i32.store offset=12
(;@433   ;)    local.get 6
(;@435   ;)    local.get 5
(;@437   ;)    i32.store offset=16
(;@43a   ;)    local.get 6
(;@43c   ;)    return
             )
(;@43f   ;)  (func $main_lam_6 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@440   ;)    (local i32)
(;@442   ;)    i32.const 8
(;@444   ;)    call $alloc
(;@446   ;)    local.set 2
(;@448   ;)    local.get 2
(;@44a   ;)    i32.const 0
(;@44c   ;)    i32.store
(;@44f   ;)    local.get 2
(;@451   ;)    local.get 0
(;@453   ;)    i32.store offset=4
(;@456   ;)    local.get 2
(;@458   ;)    return
             )
(;@45b   ;)  (func $main_lam_7 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
(;@45c   ;)    (local i32)
(;@45e   ;)    i32.const 8
(;@460   ;)    call $alloc
(;@462   ;)    local.set 2
(;@464   ;)    local.get 2
(;@466   ;)    i32.const 0
(;@468   ;)    i32.store
(;@46b   ;)    local.get 2
(;@46d   ;)    local.get 0
(;@46f   ;)    i32.store offset=4
(;@472   ;)    local.get 2
(;@474   ;)    return
             )
(;@477   ;)  (func $main_lam_8 (;18;) (type $fun_1_1) (param i32) (result i32)
(;@478   ;)    (local i32 i32 i32)
(;@47a   ;)    i32.const 12
(;@47c   ;)    call $alloc
(;@47e   ;)    local.set 1
(;@480   ;)    local.get 1
(;@482   ;)    i32.const 6
(;@484   ;)    i32.store
(;@487   ;)    local.get 1
(;@489   ;)    i32.const 16
(;@48b   ;)    i32.store offset=4
(;@48e   ;)    local.get 1
(;@490   ;)    local.get 0
(;@492   ;)    i32.store offset=8
(;@495   ;)    local.get 1
(;@497   ;)    local.set 1
(;@499   ;)    i32.const 8
(;@49b   ;)    call $alloc
(;@49d   ;)    local.set 2
(;@49f   ;)    local.get 2
(;@4a1   ;)    i32.const 5
(;@4a3   ;)    i32.store
(;@4a6   ;)    local.get 2
(;@4a8   ;)    i32.const 17
(;@4aa   ;)    i32.store offset=4
(;@4ad   ;)    local.get 2
(;@4af   ;)    local.set 2
(;@4b1   ;)    i32.const 16
(;@4b3   ;)    call $alloc
(;@4b5   ;)    local.set 3
(;@4b7   ;)    local.get 3
(;@4b9   ;)    i32.const 7
(;@4bb   ;)    i32.store
(;@4be   ;)    local.get 3
(;@4c0   ;)    i32.const 23
(;@4c2   ;)    i32.store offset=4
(;@4c5   ;)    local.get 3
(;@4c7   ;)    local.get 1
(;@4c9   ;)    i32.store offset=8
(;@4cc   ;)    local.get 3
(;@4ce   ;)    local.get 2
(;@4d0   ;)    i32.store offset=12
(;@4d3   ;)    local.get 3
(;@4d5   ;)    return
             )
(;@4d8   ;)  (func $main_lam_9 (;19;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4d9   ;)    (local i32)
(;@4db   ;)    local.get 1
(;@4dd   ;)    i32.const 4
(;@4df   ;)    i32.add
(;@4e0   ;)    i32.const 52
(;@4e2   ;)    local.get 1
(;@4e4   ;)    i32.load
(;@4e7   ;)    call_indirect (type $fun_2_1)
(;@4ea   ;)    return
             )
(;@4ed   ;)  (func $main_lam_10 (;20;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4ee   ;)    (local i32 i32)
(;@4f0   ;)    i32.const 8
(;@4f2   ;)    call $alloc
(;@4f4   ;)    local.set 2
(;@4f6   ;)    local.get 2
(;@4f8   ;)    i32.const 5
(;@4fa   ;)    i32.store
(;@4fd   ;)    local.get 2
(;@4ff   ;)    i32.const 19
(;@501   ;)    i32.store offset=4
(;@504   ;)    local.get 2
(;@506   ;)    local.set 2
(;@508   ;)    i32.const 8
(;@50a   ;)    call $alloc
(;@50c   ;)    local.set 3
(;@50e   ;)    local.get 3
(;@510   ;)    local.get 0
(;@512   ;)    i32.store
(;@515   ;)    local.get 3
(;@517   ;)    local.get 2
(;@519   ;)    i32.store offset=4
(;@51c   ;)    local.get 3
(;@51e   ;)    return
             )
(;@521   ;)  (func $main_lam_11 (;21;) (type $fun_1_1) (param i32) (result i32)
(;@522   ;)    (local i32)
(;@524   ;)    i32.const 8
(;@526   ;)    call $alloc
(;@528   ;)    local.set 1
(;@52a   ;)    local.get 1
(;@52c   ;)    i32.const 0
(;@52e   ;)    i32.store
(;@531   ;)    local.get 1
(;@533   ;)    i32.const 52
(;@535   ;)    i32.store offset=4
(;@538   ;)    local.get 1
(;@53a   ;)    return
             )
(;@53d   ;)  (func $main_lam_12 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
(;@53e   ;)    (local i32)
(;@540   ;)    i32.const 8
(;@542   ;)    call $alloc
(;@544   ;)    local.set 2
(;@546   ;)    local.get 2
(;@548   ;)    i32.const 0
(;@54a   ;)    i32.store
(;@54d   ;)    local.get 2
(;@54f   ;)    local.get 0
(;@551   ;)    i32.store offset=4
(;@554   ;)    local.get 2
(;@556   ;)    return
             )
(;@55a   ;)  (func $__mon_bind (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@55b   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@55d   ;)    local.get 0
(;@55f   ;)    i32.const 4
(;@561   ;)    i32.add
(;@562   ;)    local.get 2
(;@564   ;)    local.get 0
(;@566   ;)    i32.load
(;@569   ;)    call_indirect (type $fun_2_1)
(;@56c   ;)    local.set 3
(;@56e   ;)    local.get 3
(;@570   ;)    i32.load
(;@573   ;)    local.set 4
(;@575   ;)    block (result i32) ;; label = @1
(;@577   ;)      block ;; label = @2
(;@579   ;)        block ;; label = @3
(;@57b   ;)          block ;; label = @4
(;@57d   ;)            local.get 4
(;@57f   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@584   ;)          end
(;@585   ;)          local.get 3
(;@587   ;)          i32.load offset=4
(;@58a   ;)          local.set 5
(;@58c   ;)          local.get 1
(;@58e   ;)          i32.const 4
(;@590   ;)          i32.add
(;@591   ;)          local.get 5
(;@593   ;)          local.get 2
(;@595   ;)          local.get 1
(;@597   ;)          i32.load
(;@59a   ;)          call_indirect (type $fun_3_1)
(;@59d   ;)          br 2 (;@1;)
(;@59f   ;)        end
(;@5a0   ;)        local.get 3
(;@5a2   ;)        i32.load offset=4
(;@5a5   ;)        local.set 6
(;@5a7   ;)        local.get 6
(;@5a9   ;)        local.set 7
(;@5ab   ;)        local.get 7
(;@5ad   ;)        i32.load
(;@5b0   ;)        local.set 8
(;@5b2   ;)        local.get 7
(;@5b4   ;)        i32.load offset=4
(;@5b7   ;)        local.set 9
(;@5b9   ;)        i32.const 16
(;@5bb   ;)        call $alloc
(;@5bd   ;)        local.set 10
(;@5bf   ;)        local.get 10
(;@5c1   ;)        i32.const 7
(;@5c3   ;)        i32.store
(;@5c6   ;)        local.get 10
(;@5c8   ;)        i32.const 24
(;@5ca   ;)        i32.store offset=4
(;@5cd   ;)        local.get 10
(;@5cf   ;)        local.get 1
(;@5d1   ;)        i32.store offset=8
(;@5d4   ;)        local.get 10
(;@5d6   ;)        local.get 7
(;@5d8   ;)        i32.store offset=12
(;@5db   ;)        local.get 10
(;@5dd   ;)        local.set 10
(;@5df   ;)        i32.const 12
(;@5e1   ;)        call $alloc
(;@5e3   ;)        local.set 11
(;@5e5   ;)        local.get 11
(;@5e7   ;)        local.get 8
(;@5e9   ;)        i32.store
(;@5ec   ;)        local.get 11
(;@5ee   ;)        local.get 9
(;@5f0   ;)        i32.store offset=4
(;@5f3   ;)        local.get 11
(;@5f5   ;)        local.get 10
(;@5f7   ;)        i32.store offset=8
(;@5fa   ;)        local.get 11
(;@5fc   ;)        local.set 11
(;@5fe   ;)        i32.const 8
(;@600   ;)        call $alloc
(;@602   ;)        local.set 12
(;@604   ;)        local.get 12
(;@606   ;)        i32.const 1
(;@608   ;)        i32.store
(;@60b   ;)        local.get 12
(;@60d   ;)        local.get 11
(;@60f   ;)        i32.store offset=4
(;@612   ;)        local.get 12
(;@614   ;)        br 1 (;@1;)
(;@616   ;)      end
(;@617   ;)      unreachable
(;@618   ;)    end
(;@619   ;)    return
             )
(;@61c   ;)  (func $__mon_bind_lam_0 (;24;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@61d   ;)    (local i32 i32 i32)
(;@61f   ;)    local.get 0
(;@621   ;)    i32.const 4
(;@623   ;)    i32.add
(;@624   ;)    local.get 2
(;@626   ;)    local.get 0
(;@628   ;)    i32.load
(;@62b   ;)    call_indirect (type $fun_2_1)
(;@62e   ;)    local.set 3
(;@630   ;)    local.get 1
(;@632   ;)    i32.load offset=8
(;@635   ;)    local.set 4
(;@637   ;)    i32.const 16
(;@639   ;)    call $alloc
(;@63b   ;)    local.set 5
(;@63d   ;)    local.get 5
(;@63f   ;)    i32.const 7
(;@641   ;)    i32.store
(;@644   ;)    local.get 5
(;@646   ;)    i32.const 23
(;@648   ;)    i32.store offset=4
(;@64b   ;)    local.get 5
(;@64d   ;)    local.get 3
(;@64f   ;)    i32.store offset=8
(;@652   ;)    local.get 5
(;@654   ;)    local.get 4
(;@656   ;)    i32.store offset=12
(;@659   ;)    local.get 5
(;@65b   ;)    return
             )
(;@65f   ;)  (func $__mon_prompt (;25;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@660   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@662   ;)    local.get 1
(;@664   ;)    i32.const 4
(;@666   ;)    i32.add
(;@667   ;)    local.get 3
(;@669   ;)    local.get 1
(;@66b   ;)    i32.load
(;@66e   ;)    call_indirect (type $fun_2_1)
(;@671   ;)    local.set 4
(;@673   ;)    local.get 2
(;@675   ;)    i32.const 4
(;@677   ;)    i32.add
(;@678   ;)    local.get 4
(;@67a   ;)    local.get 2
(;@67c   ;)    i32.load
(;@67f   ;)    call_indirect (type $fun_2_1)
(;@682   ;)    local.set 5
(;@684   ;)    local.get 5
(;@686   ;)    i32.load
(;@689   ;)    local.set 6
(;@68b   ;)    block (result i32) ;; label = @1
(;@68d   ;)      block ;; label = @2
(;@68f   ;)        block ;; label = @3
(;@691   ;)          block ;; label = @4
(;@693   ;)            local.get 6
(;@695   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@69a   ;)          end
(;@69b   ;)          local.get 5
(;@69d   ;)          i32.load offset=4
(;@6a0   ;)          local.set 7
(;@6a2   ;)          i32.const 8
(;@6a4   ;)          call $alloc
(;@6a6   ;)          local.set 8
(;@6a8   ;)          local.get 8
(;@6aa   ;)          i32.const 0
(;@6ac   ;)          i32.store
(;@6af   ;)          local.get 8
(;@6b1   ;)          local.get 7
(;@6b3   ;)          i32.store offset=4
(;@6b6   ;)          local.get 8
(;@6b8   ;)          br 2 (;@1;)
(;@6ba   ;)        end
(;@6bb   ;)        local.get 5
(;@6bd   ;)        i32.load offset=4
(;@6c0   ;)        local.set 8
(;@6c2   ;)        local.get 8
(;@6c4   ;)        i32.load
(;@6c7   ;)        local.set 9
(;@6c9   ;)        local.get 0
(;@6cb   ;)        local.get 9
(;@6cd   ;)        call $__mon_eqm
(;@6cf   ;)        local.set 10
(;@6d1   ;)        local.get 10
(;@6d3   ;)        i32.load
(;@6d6   ;)        local.set 11
(;@6d8   ;)        block (result i32) ;; label = @3
(;@6da   ;)          block ;; label = @4
(;@6dc   ;)            block ;; label = @5
(;@6de   ;)              block ;; label = @6
(;@6e0   ;)                local.get 11
(;@6e2   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@6e7   ;)              end
(;@6e8   ;)              local.get 10
(;@6ea   ;)              i32.load offset=4
(;@6ed   ;)              local.set 12
(;@6ef   ;)              local.get 8
(;@6f1   ;)              i32.load
(;@6f4   ;)              local.set 13
(;@6f6   ;)              local.get 8
(;@6f8   ;)              i32.load offset=4
(;@6fb   ;)              local.set 14
(;@6fd   ;)              i32.const 20
(;@6ff   ;)              call $alloc
(;@701   ;)              local.set 15
(;@703   ;)              local.get 15
(;@705   ;)              i32.const 8
(;@707   ;)              i32.store
(;@70a   ;)              local.get 15
(;@70c   ;)              i32.const 26
(;@70e   ;)              i32.store offset=4
(;@711   ;)              local.get 15
(;@713   ;)              local.get 0
(;@715   ;)              i32.store offset=8
(;@718   ;)              local.get 15
(;@71a   ;)              local.get 1
(;@71c   ;)              i32.store offset=12
(;@71f   ;)              local.get 15
(;@721   ;)              local.get 8
(;@723   ;)              i32.store offset=16
(;@726   ;)              local.get 15
(;@728   ;)              local.set 15
(;@72a   ;)              local.get 8
(;@72c   ;)              i32.load offset=8
(;@72f   ;)              local.set 16
(;@731   ;)              i32.const 16
(;@733   ;)              call $alloc
(;@735   ;)              local.set 17
(;@737   ;)              local.get 17
(;@739   ;)              local.get 13
(;@73b   ;)              i32.store
(;@73e   ;)              local.get 17
(;@740   ;)              local.get 14
(;@742   ;)              i32.store offset=4
(;@745   ;)              local.get 17
(;@747   ;)              local.get 15
(;@749   ;)              i32.store offset=8
(;@74c   ;)              local.get 17
(;@74e   ;)              local.get 16
(;@750   ;)              i32.store offset=12
(;@753   ;)              local.get 17
(;@755   ;)              local.set 17
(;@757   ;)              i32.const 8
(;@759   ;)              call $alloc
(;@75b   ;)              local.set 18
(;@75d   ;)              local.get 18
(;@75f   ;)              i32.const 1
(;@761   ;)              i32.store
(;@764   ;)              local.get 18
(;@766   ;)              local.get 17
(;@768   ;)              i32.store offset=4
(;@76b   ;)              local.get 18
(;@76d   ;)              br 2 (;@3;)
(;@76f   ;)            end
(;@770   ;)            local.get 10
(;@772   ;)            i32.load offset=4
(;@775   ;)            local.set 12
(;@777   ;)            i32.const 20
(;@779   ;)            call $alloc
(;@77b   ;)            local.set 18
(;@77d   ;)            local.get 18
(;@77f   ;)            i32.const 8
(;@781   ;)            i32.store
(;@784   ;)            local.get 18
(;@786   ;)            i32.const 27
(;@788   ;)            i32.store offset=4
(;@78b   ;)            local.get 18
(;@78d   ;)            local.get 0
(;@78f   ;)            i32.store offset=8
(;@792   ;)            local.get 18
(;@794   ;)            local.get 1
(;@796   ;)            i32.store offset=12
(;@799   ;)            local.get 18
(;@79b   ;)            local.get 8
(;@79d   ;)            i32.store offset=16
(;@7a0   ;)            local.get 18
(;@7a2   ;)            local.set 18
(;@7a4   ;)            local.get 8
(;@7a6   ;)            i32.load offset=4
(;@7a9   ;)            local.set 19
(;@7ab   ;)            local.get 19
(;@7ad   ;)            i32.const 4
(;@7af   ;)            i32.add
(;@7b0   ;)            local.get 18
(;@7b2   ;)            local.get 3
(;@7b4   ;)            local.get 19
(;@7b6   ;)            i32.load
(;@7b9   ;)            call_indirect (type $fun_3_1)
(;@7bc   ;)            br 1 (;@3;)
(;@7be   ;)          end
(;@7bf   ;)          unreachable
(;@7c0   ;)        end
(;@7c1   ;)        br 1 (;@1;)
(;@7c3   ;)      end
(;@7c4   ;)      unreachable
(;@7c5   ;)    end
(;@7c6   ;)    return
             )
(;@7c9   ;)  (func $__mon_prompt_lam_0 (;26;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@7ca   ;)    (local i32 i32 i32)
(;@7cc   ;)    local.get 2
(;@7ce   ;)    i32.load offset=8
(;@7d1   ;)    local.set 4
(;@7d3   ;)    local.get 4
(;@7d5   ;)    i32.const 4
(;@7d7   ;)    i32.add
(;@7d8   ;)    local.get 3
(;@7da   ;)    local.get 4
(;@7dc   ;)    i32.load
(;@7df   ;)    call_indirect (type $fun_2_1)
(;@7e2   ;)    local.set 5
(;@7e4   ;)    i32.const 20
(;@7e6   ;)    call $alloc
(;@7e8   ;)    local.set 6
(;@7ea   ;)    local.get 6
(;@7ec   ;)    i32.const 8
(;@7ee   ;)    i32.store
(;@7f1   ;)    local.get 6
(;@7f3   ;)    i32.const 25
(;@7f5   ;)    i32.store offset=4
(;@7f8   ;)    local.get 6
(;@7fa   ;)    local.get 0
(;@7fc   ;)    i32.store offset=8
(;@7ff   ;)    local.get 6
(;@801   ;)    local.get 1
(;@803   ;)    i32.store offset=12
(;@806   ;)    local.get 6
(;@808   ;)    local.get 5
(;@80a   ;)    i32.store offset=16
(;@80d   ;)    local.get 6
(;@80f   ;)    return
             )
(;@812   ;)  (func $__mon_prompt_lam_1 (;27;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@813   ;)    (local i32 i32 i32)
(;@815   ;)    local.get 2
(;@817   ;)    i32.load offset=8
(;@81a   ;)    local.set 4
(;@81c   ;)    local.get 4
(;@81e   ;)    i32.const 4
(;@820   ;)    i32.add
(;@821   ;)    local.get 3
(;@823   ;)    local.get 4
(;@825   ;)    i32.load
(;@828   ;)    call_indirect (type $fun_2_1)
(;@82b   ;)    local.set 5
(;@82d   ;)    i32.const 20
(;@82f   ;)    call $alloc
(;@831   ;)    local.set 6
(;@833   ;)    local.get 6
(;@835   ;)    i32.const 8
(;@837   ;)    i32.store
(;@83a   ;)    local.get 6
(;@83c   ;)    i32.const 25
(;@83e   ;)    i32.store offset=4
(;@841   ;)    local.get 6
(;@843   ;)    local.get 0
(;@845   ;)    i32.store offset=8
(;@848   ;)    local.get 6
(;@84a   ;)    local.get 1
(;@84c   ;)    i32.store offset=12
(;@84f   ;)    local.get 6
(;@851   ;)    local.get 5
(;@853   ;)    i32.store offset=16
(;@856   ;)    local.get 6
(;@858   ;)    return
             )
(;@9f    ;)  (table (;0;) 28 28 funcref)
(;@a6    ;)  (memory (;0;) 1)
(;@ab    ;)  (global (;0;) (mut i32) i32.const 0)
(;@b3    ;)  (export "main" (func $main))
(;@ba    ;)  (export "mem" (memory 0))
(;@c3    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $trace $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_2 $__apply_4_3 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11 $main_lam_12 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
