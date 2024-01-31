(module $state
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_4_1 (;4;) (func (param i32 i32 i32 i32) (result i32)))
(;@93    ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@94    ;)    (local i32)
(;@96    ;)    global.get 1
(;@98    ;)    global.get 1
(;@9a    ;)    i32.const 1
(;@9c    ;)    i32.add
(;@9d    ;)    global.set 1
(;@9f    ;)    return
             )
(;@a2    ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@a3    ;)    (local i32)
(;@a5    ;)    global.get 0
(;@a7    ;)    global.get 0
(;@a9    ;)    local.get 0
(;@ab    ;)    i32.add
(;@ac    ;)    global.set 0
(;@ae    ;)    return
             )
(;@b1    ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@b2    ;)    (local i32)
(;@b4    ;)    i32.const 4
(;@b6    ;)    call $alloc
(;@b8    ;)    local.tee 2
(;@ba    ;)    i32.const 1
(;@bc    ;)    i32.const 0
(;@be    ;)    local.get 0
(;@c0    ;)    local.get 1
(;@c2    ;)    i32.eq
(;@c3    ;)    select
(;@c4    ;)    i32.store
(;@c7    ;)    local.get 2
(;@c9    ;)    return
             )
(;@cc    ;)  (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@cd    ;)    local.get 1
(;@cf    ;)    local.get 0
(;@d1    ;)    i32.load
(;@d4    ;)    call_indirect (type $fun_1_1)
             )
(;@d9    ;)  (func $__apply_2_0 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@da    ;)    local.get 1
(;@dc    ;)    local.get 2
(;@de    ;)    local.get 0
(;@e0    ;)    i32.load
(;@e3    ;)    call_indirect (type $fun_2_1)
             )
(;@e8    ;)  (func $__apply_2_1 (;5;) (type $fun_2_1) (param i32 i32) (result i32)
(;@e9    ;)    local.get 0
(;@eb    ;)    i32.load offset=4
(;@ee    ;)    local.get 1
(;@f0    ;)    local.get 0
(;@f2    ;)    i32.load
(;@f5    ;)    call_indirect (type $fun_2_1)
             )
(;@fa    ;)  (func $__apply_3_1 (;6;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@fb    ;)    local.get 0
(;@fd    ;)    i32.load offset=4
(;@100   ;)    local.get 1
(;@102   ;)    local.get 2
(;@104   ;)    local.get 0
(;@106   ;)    i32.load
(;@109   ;)    call_indirect (type $fun_3_1)
             )
(;@10e   ;)  (func $__apply_3_2 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
(;@10f   ;)    local.get 0
(;@111   ;)    i32.load offset=4
(;@114   ;)    local.get 0
(;@116   ;)    i32.load offset=8
(;@119   ;)    local.get 1
(;@11b   ;)    local.get 0
(;@11d   ;)    i32.load
(;@120   ;)    call_indirect (type $fun_3_1)
             )
(;@126   ;)  (func $main (;8;) (type $fun_0_1) (result i32)
(;@127   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@129   ;)    i32.const 0
(;@12b   ;)    call $alloc
(;@12d   ;)    local.set 0
(;@12f   ;)    local.get 0
(;@131   ;)    local.set 0
(;@133   ;)    local.get 0
(;@135   ;)    call $__mon_generate_marker
(;@137   ;)    local.set 1
(;@139   ;)    i32.const 8
(;@13b   ;)    call $alloc
(;@13d   ;)    local.set 2
(;@13f   ;)    local.get 2
(;@141   ;)    i32.const 4
(;@143   ;)    i32.store
(;@146   ;)    local.get 2
(;@148   ;)    i32.const 9
(;@14a   ;)    i32.store offset=4
(;@14d   ;)    local.get 2
(;@14f   ;)    local.set 2
(;@151   ;)    i32.const 8
(;@153   ;)    call $alloc
(;@155   ;)    local.set 3
(;@157   ;)    local.get 3
(;@159   ;)    i32.const 4
(;@15b   ;)    i32.store
(;@15e   ;)    local.get 3
(;@160   ;)    i32.const 13
(;@162   ;)    i32.store offset=4
(;@165   ;)    local.get 3
(;@167   ;)    local.set 3
(;@169   ;)    i32.const 12
(;@16b   ;)    call $alloc
(;@16d   ;)    local.set 4
(;@16f   ;)    local.get 4
(;@171   ;)    local.get 1
(;@173   ;)    i32.store
(;@176   ;)    local.get 4
(;@178   ;)    local.get 2
(;@17a   ;)    i32.store offset=4
(;@17d   ;)    local.get 4
(;@17f   ;)    local.get 3
(;@181   ;)    i32.store offset=8
(;@184   ;)    local.get 4
(;@186   ;)    local.set 4
(;@188   ;)    local.get 4
(;@18a   ;)    i32.load offset=8
(;@18d   ;)    local.set 5
(;@18f   ;)    i32.const 0
(;@191   ;)    call $alloc
(;@193   ;)    local.set 6
(;@195   ;)    local.get 6
(;@197   ;)    local.set 6
(;@199   ;)    local.get 4
(;@19b   ;)    i32.load offset=4
(;@19e   ;)    local.set 7
(;@1a0   ;)    local.get 7
(;@1a2   ;)    i32.const 4
(;@1a4   ;)    i32.add
(;@1a5   ;)    local.get 5
(;@1a7   ;)    local.get 6
(;@1a9   ;)    local.get 7
(;@1ab   ;)    i32.load
(;@1ae   ;)    call_indirect (type $fun_3_1)
(;@1b1   ;)    local.set 8
(;@1b3   ;)    local.get 8
(;@1b5   ;)    i32.load
(;@1b8   ;)    local.set 9
(;@1ba   ;)    block (result i32) ;; label = @1
(;@1bc   ;)      block ;; label = @2
(;@1be   ;)        block ;; label = @3
(;@1c0   ;)          block ;; label = @4
(;@1c2   ;)            local.get 9
(;@1c4   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1c9   ;)          end
(;@1ca   ;)          local.get 8
(;@1cc   ;)          i32.load offset=4
(;@1cf   ;)          local.set 10
(;@1d1   ;)          local.get 10
(;@1d3   ;)          i32.const 4
(;@1d5   ;)          i32.add
(;@1d6   ;)          i32.const 16777215
(;@1db   ;)          local.get 10
(;@1dd   ;)          i32.load
(;@1e0   ;)          call_indirect (type $fun_2_1)
(;@1e3   ;)          local.set 11
(;@1e5   ;)          i32.const 8
(;@1e7   ;)          call $alloc
(;@1e9   ;)          local.set 12
(;@1eb   ;)          local.get 12
(;@1ed   ;)          i32.const 0
(;@1ef   ;)          i32.store
(;@1f2   ;)          local.get 12
(;@1f4   ;)          local.get 11
(;@1f6   ;)          i32.store offset=4
(;@1f9   ;)          local.get 12
(;@1fb   ;)          br 2 (;@1;)
(;@1fd   ;)        end
(;@1fe   ;)        local.get 8
(;@200   ;)        i32.load offset=4
(;@203   ;)        local.set 12
(;@205   ;)        local.get 12
(;@207   ;)        local.set 13
(;@209   ;)        local.get 13
(;@20b   ;)        i32.load
(;@20e   ;)        local.set 14
(;@210   ;)        local.get 13
(;@212   ;)        i32.load offset=4
(;@215   ;)        local.set 15
(;@217   ;)        i32.const 12
(;@219   ;)        call $alloc
(;@21b   ;)        local.set 16
(;@21d   ;)        local.get 16
(;@21f   ;)        i32.const 6
(;@221   ;)        i32.store
(;@224   ;)        local.get 16
(;@226   ;)        i32.const 16
(;@228   ;)        i32.store offset=4
(;@22b   ;)        local.get 16
(;@22d   ;)        local.get 13
(;@22f   ;)        i32.store offset=8
(;@232   ;)        local.get 16
(;@234   ;)        local.set 16
(;@236   ;)        i32.const 12
(;@238   ;)        call $alloc
(;@23a   ;)        local.set 17
(;@23c   ;)        local.get 17
(;@23e   ;)        local.get 14
(;@240   ;)        i32.store
(;@243   ;)        local.get 17
(;@245   ;)        local.get 15
(;@247   ;)        i32.store offset=4
(;@24a   ;)        local.get 17
(;@24c   ;)        local.get 16
(;@24e   ;)        i32.store offset=8
(;@251   ;)        local.get 17
(;@253   ;)        local.set 17
(;@255   ;)        i32.const 8
(;@257   ;)        call $alloc
(;@259   ;)        local.set 18
(;@25b   ;)        local.get 18
(;@25d   ;)        i32.const 1
(;@25f   ;)        i32.store
(;@262   ;)        local.get 18
(;@264   ;)        local.get 17
(;@266   ;)        i32.store offset=4
(;@269   ;)        local.get 18
(;@26b   ;)        br 1 (;@1;)
(;@26d   ;)      end
(;@26e   ;)      unreachable
(;@26f   ;)    end
(;@270   ;)    local.set 18
(;@272   ;)    local.get 18
(;@274   ;)    i32.load
(;@277   ;)    local.set 19
(;@279   ;)    block (result i32) ;; label = @1
(;@27b   ;)      block ;; label = @2
(;@27d   ;)        block ;; label = @3
(;@27f   ;)          block ;; label = @4
(;@281   ;)            local.get 19
(;@283   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@288   ;)          end
(;@289   ;)          local.get 18
(;@28b   ;)          i32.load offset=4
(;@28e   ;)          local.set 10
(;@290   ;)          local.get 10
(;@292   ;)          i32.load offset=4
(;@295   ;)          local.set 20
(;@297   ;)          i32.const 8
(;@299   ;)          call $alloc
(;@29b   ;)          local.set 21
(;@29d   ;)          local.get 21
(;@29f   ;)          i32.const 0
(;@2a1   ;)          i32.store
(;@2a4   ;)          local.get 21
(;@2a6   ;)          local.get 20
(;@2a8   ;)          i32.store offset=4
(;@2ab   ;)          local.get 21
(;@2ad   ;)          br 2 (;@1;)
(;@2af   ;)        end
(;@2b0   ;)        local.get 18
(;@2b2   ;)        i32.load offset=4
(;@2b5   ;)        local.set 12
(;@2b7   ;)        local.get 12
(;@2b9   ;)        local.set 13
(;@2bb   ;)        local.get 13
(;@2bd   ;)        i32.load
(;@2c0   ;)        local.set 21
(;@2c2   ;)        local.get 13
(;@2c4   ;)        i32.load offset=4
(;@2c7   ;)        local.set 22
(;@2c9   ;)        i32.const 12
(;@2cb   ;)        call $alloc
(;@2cd   ;)        local.set 23
(;@2cf   ;)        local.get 23
(;@2d1   ;)        i32.const 6
(;@2d3   ;)        i32.store
(;@2d6   ;)        local.get 23
(;@2d8   ;)        i32.const 18
(;@2da   ;)        i32.store offset=4
(;@2dd   ;)        local.get 23
(;@2df   ;)        local.get 13
(;@2e1   ;)        i32.store offset=8
(;@2e4   ;)        local.get 23
(;@2e6   ;)        local.set 23
(;@2e8   ;)        i32.const 12
(;@2ea   ;)        call $alloc
(;@2ec   ;)        local.set 24
(;@2ee   ;)        local.get 24
(;@2f0   ;)        local.get 21
(;@2f2   ;)        i32.store
(;@2f5   ;)        local.get 24
(;@2f7   ;)        local.get 22
(;@2f9   ;)        i32.store offset=4
(;@2fc   ;)        local.get 24
(;@2fe   ;)        local.get 23
(;@300   ;)        i32.store offset=8
(;@303   ;)        local.get 24
(;@305   ;)        local.set 24
(;@307   ;)        i32.const 8
(;@309   ;)        call $alloc
(;@30b   ;)        local.set 25
(;@30d   ;)        local.get 25
(;@30f   ;)        i32.const 1
(;@311   ;)        i32.store
(;@314   ;)        local.get 25
(;@316   ;)        local.get 24
(;@318   ;)        i32.store offset=4
(;@31b   ;)        local.get 25
(;@31d   ;)        br 1 (;@1;)
(;@31f   ;)      end
(;@320   ;)      unreachable
(;@321   ;)    end
(;@322   ;)    local.set 25
(;@324   ;)    local.get 25
(;@326   ;)    i32.load
(;@329   ;)    local.set 26
(;@32b   ;)    block (result i32) ;; label = @1
(;@32d   ;)      block ;; label = @2
(;@32f   ;)        block ;; label = @3
(;@331   ;)          block ;; label = @4
(;@333   ;)            local.get 26
(;@335   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@33a   ;)          end
(;@33b   ;)          local.get 25
(;@33d   ;)          i32.load offset=4
(;@340   ;)          local.set 27
(;@342   ;)          local.get 27
(;@344   ;)          br 2 (;@1;)
(;@346   ;)        end
(;@347   ;)        local.get 25
(;@349   ;)        i32.load offset=4
(;@34c   ;)        local.set 27
(;@34e   ;)        i32.const 5467
(;@351   ;)        br 1 (;@1;)
(;@353   ;)      end
(;@354   ;)      unreachable
(;@355   ;)    end
(;@356   ;)    return
             )
(;@359   ;)  (func $main_lam_0 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@35a   ;)    (local i32)
(;@35c   ;)    local.get 0
(;@35e   ;)    i32.const 4
(;@360   ;)    i32.add
(;@361   ;)    local.get 1
(;@363   ;)    local.get 1
(;@365   ;)    local.get 0
(;@367   ;)    i32.load
(;@36a   ;)    call_indirect (type $fun_3_1)
(;@36d   ;)    return
             )
(;@370   ;)  (func $main_lam_1 (;10;) (type $fun_2_1) (param i32 i32) (result i32)
(;@371   ;)    (local i32)
(;@373   ;)    i32.const 8
(;@375   ;)    call $alloc
(;@377   ;)    local.set 2
(;@379   ;)    local.get 2
(;@37b   ;)    i32.const 0
(;@37d   ;)    i32.store
(;@380   ;)    local.get 2
(;@382   ;)    local.get 0
(;@384   ;)    i32.store offset=4
(;@387   ;)    local.get 2
(;@389   ;)    return
             )
(;@38c   ;)  (func $main_lam_2 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@38d   ;)    (local i32)
(;@38f   ;)    i32.const 8
(;@391   ;)    call $alloc
(;@393   ;)    local.set 2
(;@395   ;)    local.get 2
(;@397   ;)    local.get 1
(;@399   ;)    i32.store
(;@39c   ;)    local.get 2
(;@39e   ;)    local.get 0
(;@3a0   ;)    i32.store offset=4
(;@3a3   ;)    local.get 2
(;@3a5   ;)    return
             )
(;@3a8   ;)  (func $main_lam_3 (;12;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3a9   ;)    (local i32 i32)
(;@3ab   ;)    i32.const 12
(;@3ad   ;)    call $alloc
(;@3af   ;)    local.set 2
(;@3b1   ;)    local.get 2
(;@3b3   ;)    i32.const 5
(;@3b5   ;)    i32.store
(;@3b8   ;)    local.get 2
(;@3ba   ;)    i32.const 11
(;@3bc   ;)    i32.store offset=4
(;@3bf   ;)    local.get 2
(;@3c1   ;)    local.get 0
(;@3c3   ;)    i32.store offset=8
(;@3c6   ;)    local.get 2
(;@3c8   ;)    local.set 2
(;@3ca   ;)    i32.const 8
(;@3cc   ;)    call $alloc
(;@3ce   ;)    local.set 3
(;@3d0   ;)    local.get 3
(;@3d2   ;)    i32.const 0
(;@3d4   ;)    i32.store
(;@3d7   ;)    local.get 3
(;@3d9   ;)    local.get 2
(;@3db   ;)    i32.store offset=4
(;@3de   ;)    local.get 3
(;@3e0   ;)    return
             )
(;@3e3   ;)  (func $main_lam_4 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@3e4   ;)    (local i32 i32 i32)
(;@3e6   ;)    i32.const 12
(;@3e8   ;)    call $alloc
(;@3ea   ;)    local.set 2
(;@3ec   ;)    local.get 2
(;@3ee   ;)    i32.const 5
(;@3f0   ;)    i32.store
(;@3f3   ;)    local.get 2
(;@3f5   ;)    i32.const 10
(;@3f7   ;)    i32.store offset=4
(;@3fa   ;)    local.get 2
(;@3fc   ;)    local.get 0
(;@3fe   ;)    i32.store offset=8
(;@401   ;)    local.get 2
(;@403   ;)    local.set 2
(;@405   ;)    i32.const 8
(;@407   ;)    call $alloc
(;@409   ;)    local.set 3
(;@40b   ;)    local.get 3
(;@40d   ;)    i32.const 4
(;@40f   ;)    i32.store
(;@412   ;)    local.get 3
(;@414   ;)    i32.const 12
(;@416   ;)    i32.store offset=4
(;@419   ;)    local.get 3
(;@41b   ;)    local.set 3
(;@41d   ;)    local.get 2
(;@41f   ;)    local.get 3
(;@421   ;)    local.get 1
(;@423   ;)    call $__mon_bind
(;@425   ;)    return
             )
(;@428   ;)  (func $main_lam_5 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@429   ;)    (local i32)
(;@42b   ;)    i32.const 8
(;@42d   ;)    call $alloc
(;@42f   ;)    local.set 2
(;@431   ;)    local.get 2
(;@433   ;)    i32.const 0
(;@435   ;)    i32.store
(;@438   ;)    local.get 2
(;@43a   ;)    local.get 0
(;@43c   ;)    i32.store offset=4
(;@43f   ;)    local.get 2
(;@441   ;)    return
             )
(;@444   ;)  (func $main_lam_6 (;15;) (type $fun_1_1) (param i32) (result i32)
(;@445   ;)    (local i32 i32)
(;@447   ;)    local.get 0
(;@449   ;)    i32.const 4
(;@44b   ;)    i32.add
(;@44c   ;)    i32.const 16777215
(;@451   ;)    local.get 0
(;@453   ;)    i32.load
(;@456   ;)    call_indirect (type $fun_2_1)
(;@459   ;)    local.set 1
(;@45b   ;)    i32.const 12
(;@45d   ;)    call $alloc
(;@45f   ;)    local.set 2
(;@461   ;)    local.get 2
(;@463   ;)    i32.const 5
(;@465   ;)    i32.store
(;@468   ;)    local.get 2
(;@46a   ;)    i32.const 14
(;@46c   ;)    i32.store offset=4
(;@46f   ;)    local.get 2
(;@471   ;)    local.get 1
(;@473   ;)    i32.store offset=8
(;@476   ;)    local.get 2
(;@478   ;)    return
             )
(;@47b   ;)  (func $main_lam_7 (;16;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@47c   ;)    (local i32 i32 i32 i32)
(;@47e   ;)    local.get 0
(;@480   ;)    i32.load offset=8
(;@483   ;)    local.set 3
(;@485   ;)    local.get 3
(;@487   ;)    i32.const 4
(;@489   ;)    i32.add
(;@48a   ;)    local.get 1
(;@48c   ;)    local.get 3
(;@48e   ;)    i32.load
(;@491   ;)    call_indirect (type $fun_2_1)
(;@494   ;)    local.set 4
(;@496   ;)    i32.const 8
(;@498   ;)    call $alloc
(;@49a   ;)    local.set 5
(;@49c   ;)    local.get 5
(;@49e   ;)    i32.const 3
(;@4a0   ;)    i32.store
(;@4a3   ;)    local.get 5
(;@4a5   ;)    i32.const 15
(;@4a7   ;)    i32.store offset=4
(;@4aa   ;)    local.get 5
(;@4ac   ;)    local.set 5
(;@4ae   ;)    local.get 4
(;@4b0   ;)    local.get 5
(;@4b2   ;)    local.get 2
(;@4b4   ;)    call $__mon_bind
(;@4b6   ;)    return
             )
(;@4b9   ;)  (func $main_lam_8 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4ba   ;)    (local i32 i32)
(;@4bc   ;)    local.get 0
(;@4be   ;)    i32.load offset=4
(;@4c1   ;)    local.set 2
(;@4c3   ;)    i32.const 8
(;@4c5   ;)    call $alloc
(;@4c7   ;)    local.set 3
(;@4c9   ;)    local.get 3
(;@4cb   ;)    i32.const 0
(;@4cd   ;)    i32.store
(;@4d0   ;)    local.get 3
(;@4d2   ;)    local.get 2
(;@4d4   ;)    i32.store offset=4
(;@4d7   ;)    local.get 3
(;@4d9   ;)    return
             )
(;@4dc   ;)  (func $main_lam_9 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@4dd   ;)    (local i32 i32 i32 i32)
(;@4df   ;)    local.get 0
(;@4e1   ;)    i32.load offset=8
(;@4e4   ;)    local.set 3
(;@4e6   ;)    local.get 3
(;@4e8   ;)    i32.const 4
(;@4ea   ;)    i32.add
(;@4eb   ;)    local.get 1
(;@4ed   ;)    local.get 3
(;@4ef   ;)    i32.load
(;@4f2   ;)    call_indirect (type $fun_2_1)
(;@4f5   ;)    local.set 4
(;@4f7   ;)    i32.const 8
(;@4f9   ;)    call $alloc
(;@4fb   ;)    local.set 5
(;@4fd   ;)    local.get 5
(;@4ff   ;)    i32.const 4
(;@501   ;)    i32.store
(;@504   ;)    local.get 5
(;@506   ;)    i32.const 17
(;@508   ;)    i32.store offset=4
(;@50b   ;)    local.get 5
(;@50d   ;)    local.set 5
(;@50f   ;)    local.get 4
(;@511   ;)    local.get 5
(;@513   ;)    local.get 2
(;@515   ;)    call $__mon_bind
(;@517   ;)    return
             )
(;@51b   ;)  (func $__mon_bind (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@51c   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@51e   ;)    local.get 0
(;@520   ;)    i32.const 4
(;@522   ;)    i32.add
(;@523   ;)    local.get 2
(;@525   ;)    local.get 0
(;@527   ;)    i32.load
(;@52a   ;)    call_indirect (type $fun_2_1)
(;@52d   ;)    local.set 3
(;@52f   ;)    local.get 3
(;@531   ;)    i32.load
(;@534   ;)    local.set 4
(;@536   ;)    block (result i32) ;; label = @1
(;@538   ;)      block ;; label = @2
(;@53a   ;)        block ;; label = @3
(;@53c   ;)          block ;; label = @4
(;@53e   ;)            local.get 4
(;@540   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@545   ;)          end
(;@546   ;)          local.get 3
(;@548   ;)          i32.load offset=4
(;@54b   ;)          local.set 5
(;@54d   ;)          local.get 1
(;@54f   ;)          i32.const 4
(;@551   ;)          i32.add
(;@552   ;)          local.get 5
(;@554   ;)          local.get 2
(;@556   ;)          local.get 1
(;@558   ;)          i32.load
(;@55b   ;)          call_indirect (type $fun_3_1)
(;@55e   ;)          br 2 (;@1;)
(;@560   ;)        end
(;@561   ;)        local.get 3
(;@563   ;)        i32.load offset=4
(;@566   ;)        local.set 6
(;@568   ;)        local.get 6
(;@56a   ;)        local.set 7
(;@56c   ;)        local.get 7
(;@56e   ;)        i32.load
(;@571   ;)        local.set 8
(;@573   ;)        local.get 7
(;@575   ;)        i32.load offset=4
(;@578   ;)        local.set 9
(;@57a   ;)        i32.const 16
(;@57c   ;)        call $alloc
(;@57e   ;)        local.set 10
(;@580   ;)        local.get 10
(;@582   ;)        i32.const 7
(;@584   ;)        i32.store
(;@587   ;)        local.get 10
(;@589   ;)        i32.const 20
(;@58b   ;)        i32.store offset=4
(;@58e   ;)        local.get 10
(;@590   ;)        local.get 1
(;@592   ;)        i32.store offset=8
(;@595   ;)        local.get 10
(;@597   ;)        local.get 7
(;@599   ;)        i32.store offset=12
(;@59c   ;)        local.get 10
(;@59e   ;)        local.set 10
(;@5a0   ;)        i32.const 12
(;@5a2   ;)        call $alloc
(;@5a4   ;)        local.set 11
(;@5a6   ;)        local.get 11
(;@5a8   ;)        local.get 8
(;@5aa   ;)        i32.store
(;@5ad   ;)        local.get 11
(;@5af   ;)        local.get 9
(;@5b1   ;)        i32.store offset=4
(;@5b4   ;)        local.get 11
(;@5b6   ;)        local.get 10
(;@5b8   ;)        i32.store offset=8
(;@5bb   ;)        local.get 11
(;@5bd   ;)        local.set 11
(;@5bf   ;)        i32.const 8
(;@5c1   ;)        call $alloc
(;@5c3   ;)        local.set 12
(;@5c5   ;)        local.get 12
(;@5c7   ;)        i32.const 1
(;@5c9   ;)        i32.store
(;@5cc   ;)        local.get 12
(;@5ce   ;)        local.get 11
(;@5d0   ;)        i32.store offset=4
(;@5d3   ;)        local.get 12
(;@5d5   ;)        br 1 (;@1;)
(;@5d7   ;)      end
(;@5d8   ;)      unreachable
(;@5d9   ;)    end
(;@5da   ;)    return
             )
(;@5dd   ;)  (func $__mon_bind_lam_0 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@5de   ;)    (local i32 i32 i32)
(;@5e0   ;)    local.get 1
(;@5e2   ;)    i32.load offset=8
(;@5e5   ;)    local.set 3
(;@5e7   ;)    local.get 3
(;@5e9   ;)    i32.const 4
(;@5eb   ;)    i32.add
(;@5ec   ;)    local.get 2
(;@5ee   ;)    local.get 3
(;@5f0   ;)    i32.load
(;@5f3   ;)    call_indirect (type $fun_2_1)
(;@5f6   ;)    local.set 4
(;@5f8   ;)    i32.const 16
(;@5fa   ;)    call $alloc
(;@5fc   ;)    local.set 5
(;@5fe   ;)    local.get 5
(;@600   ;)    i32.const 7
(;@602   ;)    i32.store
(;@605   ;)    local.get 5
(;@607   ;)    i32.const 19
(;@609   ;)    i32.store offset=4
(;@60c   ;)    local.get 5
(;@60e   ;)    local.get 4
(;@610   ;)    i32.store offset=8
(;@613   ;)    local.get 5
(;@615   ;)    local.get 0
(;@617   ;)    i32.store offset=12
(;@61a   ;)    local.get 5
(;@61c   ;)    return
             )
(;@620   ;)  (func $__mon_prompt (;21;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@621   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@623   ;)    local.get 1
(;@625   ;)    i32.const 4
(;@627   ;)    i32.add
(;@628   ;)    local.get 3
(;@62a   ;)    local.get 1
(;@62c   ;)    i32.load
(;@62f   ;)    call_indirect (type $fun_2_1)
(;@632   ;)    local.set 4
(;@634   ;)    local.get 2
(;@636   ;)    i32.const 4
(;@638   ;)    i32.add
(;@639   ;)    local.get 4
(;@63b   ;)    local.get 2
(;@63d   ;)    i32.load
(;@640   ;)    call_indirect (type $fun_2_1)
(;@643   ;)    local.set 5
(;@645   ;)    local.get 5
(;@647   ;)    i32.load
(;@64a   ;)    local.set 6
(;@64c   ;)    block (result i32) ;; label = @1
(;@64e   ;)      block ;; label = @2
(;@650   ;)        block ;; label = @3
(;@652   ;)          block ;; label = @4
(;@654   ;)            local.get 6
(;@656   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@65b   ;)          end
(;@65c   ;)          local.get 5
(;@65e   ;)          i32.load offset=4
(;@661   ;)          local.set 7
(;@663   ;)          i32.const 8
(;@665   ;)          call $alloc
(;@667   ;)          local.set 8
(;@669   ;)          local.get 8
(;@66b   ;)          i32.const 0
(;@66d   ;)          i32.store
(;@670   ;)          local.get 8
(;@672   ;)          local.get 7
(;@674   ;)          i32.store offset=4
(;@677   ;)          local.get 8
(;@679   ;)          br 2 (;@1;)
(;@67b   ;)        end
(;@67c   ;)        local.get 5
(;@67e   ;)        i32.load offset=4
(;@681   ;)        local.set 8
(;@683   ;)        local.get 8
(;@685   ;)        i32.load
(;@688   ;)        local.set 9
(;@68a   ;)        local.get 0
(;@68c   ;)        local.get 9
(;@68e   ;)        call $__mon_eqm
(;@690   ;)        local.set 10
(;@692   ;)        local.get 10
(;@694   ;)        i32.load
(;@697   ;)        local.set 11
(;@699   ;)        block (result i32) ;; label = @3
(;@69b   ;)          block ;; label = @4
(;@69d   ;)            block ;; label = @5
(;@69f   ;)              block ;; label = @6
(;@6a1   ;)                local.get 11
(;@6a3   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@6a8   ;)              end
(;@6a9   ;)              local.get 10
(;@6ab   ;)              i32.load offset=4
(;@6ae   ;)              local.set 12
(;@6b0   ;)              local.get 8
(;@6b2   ;)              local.set 13
(;@6b4   ;)              local.get 13
(;@6b6   ;)              i32.load
(;@6b9   ;)              local.set 14
(;@6bb   ;)              local.get 13
(;@6bd   ;)              i32.load offset=4
(;@6c0   ;)              local.set 15
(;@6c2   ;)              local.get 13
(;@6c4   ;)              i32.load offset=8
(;@6c7   ;)              local.set 16
(;@6c9   ;)              i32.const 12
(;@6cb   ;)              call $alloc
(;@6cd   ;)              local.set 17
(;@6cf   ;)              local.get 17
(;@6d1   ;)              local.get 14
(;@6d3   ;)              i32.store
(;@6d6   ;)              local.get 17
(;@6d8   ;)              local.get 15
(;@6da   ;)              i32.store offset=4
(;@6dd   ;)              local.get 17
(;@6df   ;)              local.get 16
(;@6e1   ;)              i32.store offset=8
(;@6e4   ;)              local.get 17
(;@6e6   ;)              local.set 17
(;@6e8   ;)              i32.const 8
(;@6ea   ;)              call $alloc
(;@6ec   ;)              local.set 18
(;@6ee   ;)              local.get 18
(;@6f0   ;)              i32.const 1
(;@6f2   ;)              i32.store
(;@6f5   ;)              local.get 18
(;@6f7   ;)              local.get 17
(;@6f9   ;)              i32.store offset=4
(;@6fc   ;)              local.get 18
(;@6fe   ;)              br 2 (;@3;)
(;@700   ;)            end
(;@701   ;)            local.get 10
(;@703   ;)            i32.load offset=4
(;@706   ;)            local.set 12
(;@708   ;)            local.get 8
(;@70a   ;)            local.set 18
(;@70c   ;)            local.get 18
(;@70e   ;)            i32.load offset=8
(;@711   ;)            local.set 19
(;@713   ;)            local.get 18
(;@715   ;)            i32.load offset=4
(;@718   ;)            local.set 20
(;@71a   ;)            local.get 20
(;@71c   ;)            i32.const 4
(;@71e   ;)            i32.add
(;@71f   ;)            local.get 19
(;@721   ;)            local.get 3
(;@723   ;)            local.get 20
(;@725   ;)            i32.load
(;@728   ;)            call_indirect (type $fun_3_1)
(;@72b   ;)            br 1 (;@3;)
(;@72d   ;)          end
(;@72e   ;)          unreachable
(;@72f   ;)        end
(;@730   ;)        br 1 (;@1;)
(;@732   ;)      end
(;@733   ;)      unreachable
(;@734   ;)    end
(;@735   ;)    return
             )
(;@48    ;)  (table (;0;) 22 22 funcref)
(;@4f    ;)  (memory (;0;) 1)
(;@54    ;)  (global (;0;) (mut i32) i32.const 0)
(;@59    ;)  (global (;1;) (mut i32) i32.const 0)
(;@61    ;)  (export "main" (func $main))
(;@68    ;)  (export "mem" (memory 0))
(;@71    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_1 $__apply_3_2 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $__mon_bind $__mon_bind_lam_0 $__mon_prompt)
           )
