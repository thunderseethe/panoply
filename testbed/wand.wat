(module $state
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_4_1 (;3;) (func (param i32 i32 i32 i32) (result i32)))
(;@22    ;)  (type $fun_3_1 (;4;) (func (param i32 i32 i32) (result i32)))
(;@b1    ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@b2    ;)    (local i32)
(;@b4    ;)    global.get 0
(;@b6    ;)    global.get 0
(;@b8    ;)    i32.const 1
(;@ba    ;)    i32.add
(;@bb    ;)    global.set 0
(;@bd    ;)    return
             )
(;@c0    ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@c1    ;)    (local i32)
(;@c3    ;)    global.get 0
(;@c5    ;)    global.get 0
(;@c7    ;)    local.get 0
(;@c9    ;)    i32.add
(;@ca    ;)    global.set 0
(;@cc    ;)    return
             )
(;@cf    ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@d0    ;)    (local i32)
(;@d2    ;)    i32.const 4
(;@d4    ;)    call $alloc
(;@d6    ;)    local.tee 2
(;@d8    ;)    i32.const 1
(;@da    ;)    i32.const 0
(;@dc    ;)    local.get 0
(;@de    ;)    local.get 1
(;@e0    ;)    i32.eq
(;@e1    ;)    select
(;@e2    ;)    i32.store
(;@e5    ;)    local.get 2
(;@e7    ;)    return
             )
(;@ea    ;)  (func $__apply_1_0 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@eb    ;)    local.get 1
(;@ed    ;)    local.get 0
(;@ef    ;)    i32.load
(;@f2    ;)    call_indirect (type $fun_1_1)
             )
(;@f7    ;)  (func $__apply_2_1 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
(;@f8    ;)    local.get 0
(;@fa    ;)    i32.load offset=4
(;@fd    ;)    local.get 1
(;@ff    ;)    local.get 0
(;@101   ;)    i32.load
(;@104   ;)    call_indirect (type $fun_2_1)
             )
(;@109   ;)  (func $__apply_3_0 (;5;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@10a   ;)    local.get 1
(;@10c   ;)    local.get 2
(;@10e   ;)    local.get 3
(;@110   ;)    local.get 0
(;@112   ;)    i32.load
(;@115   ;)    call_indirect (type $fun_3_1)
             )
(;@11a   ;)  (func $__apply_3_2 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@11b   ;)    local.get 0
(;@11d   ;)    i32.load offset=4
(;@120   ;)    local.get 0
(;@122   ;)    i32.load offset=8
(;@125   ;)    local.get 1
(;@127   ;)    local.get 0
(;@129   ;)    i32.load
(;@12c   ;)    call_indirect (type $fun_3_1)
             )
(;@131   ;)  (func $__apply_4_3 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
(;@132   ;)    local.get 0
(;@134   ;)    i32.load offset=4
(;@137   ;)    local.get 0
(;@139   ;)    i32.load offset=8
(;@13c   ;)    local.get 0
(;@13e   ;)    i32.load offset=12
(;@141   ;)    local.get 1
(;@143   ;)    local.get 0
(;@145   ;)    i32.load
(;@148   ;)    call_indirect (type $fun_4_1)
             )
(;@14e   ;)  (func $main (;8;) (type $fun_0_1) (result i32)
(;@14f   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@151   ;)    i32.const 0
(;@153   ;)    call $alloc
(;@155   ;)    local.set 0
(;@157   ;)    local.get 0
(;@159   ;)    local.set 0
(;@15b   ;)    local.get 0
(;@15d   ;)    call $__mon_generate_marker
(;@15f   ;)    local.set 1
(;@161   ;)    local.get 1
(;@163   ;)    local.get 1
(;@165   ;)    call $__mon_eqm
(;@167   ;)    local.set 2
(;@169   ;)    local.get 2
(;@16b   ;)    i32.load
(;@16e   ;)    local.set 3
(;@170   ;)    block (result i32) ;; label = @1
(;@172   ;)      block ;; label = @2
(;@174   ;)        block ;; label = @3
(;@176   ;)          block ;; label = @4
(;@178   ;)            local.get 3
(;@17a   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@17f   ;)          end
(;@180   ;)          local.get 2
(;@182   ;)          i32.load offset=4
(;@185   ;)          local.set 4
(;@187   ;)          i32.const 8
(;@189   ;)          call $alloc
(;@18b   ;)          local.set 5
(;@18d   ;)          local.get 5
(;@18f   ;)          i32.const 3
(;@191   ;)          i32.store
(;@194   ;)          local.get 5
(;@196   ;)          i32.const 10
(;@198   ;)          i32.store offset=4
(;@19b   ;)          local.get 5
(;@19d   ;)          local.set 5
(;@19f   ;)          i32.const 12
(;@1a1   ;)          call $alloc
(;@1a3   ;)          local.set 6
(;@1a5   ;)          local.get 6
(;@1a7   ;)          i32.const 4
(;@1a9   ;)          i32.store
(;@1ac   ;)          local.get 6
(;@1ae   ;)          i32.const 18
(;@1b0   ;)          i32.store offset=4
(;@1b3   ;)          local.get 6
(;@1b5   ;)          local.get 1
(;@1b7   ;)          i32.store offset=8
(;@1ba   ;)          local.get 6
(;@1bc   ;)          local.set 6
(;@1be   ;)          i32.const 12
(;@1c0   ;)          call $alloc
(;@1c2   ;)          local.set 7
(;@1c4   ;)          local.get 7
(;@1c6   ;)          local.get 1
(;@1c8   ;)          i32.store
(;@1cb   ;)          local.get 7
(;@1cd   ;)          local.get 5
(;@1cf   ;)          i32.store offset=4
(;@1d2   ;)          local.get 7
(;@1d4   ;)          local.get 6
(;@1d6   ;)          i32.store offset=8
(;@1d9   ;)          local.get 7
(;@1db   ;)          local.set 7
(;@1dd   ;)          i32.const 8
(;@1df   ;)          call $alloc
(;@1e1   ;)          local.set 8
(;@1e3   ;)          local.get 8
(;@1e5   ;)          i32.const 1
(;@1e7   ;)          i32.store
(;@1ea   ;)          local.get 8
(;@1ec   ;)          local.get 7
(;@1ee   ;)          i32.store offset=4
(;@1f1   ;)          local.get 8
(;@1f3   ;)          br 2 (;@1;)
(;@1f5   ;)        end
(;@1f6   ;)        local.get 2
(;@1f8   ;)        i32.load offset=4
(;@1fb   ;)        local.set 4
(;@1fd   ;)        i32.const 12
(;@1ff   ;)        call $alloc
(;@201   ;)        local.set 8
(;@203   ;)        local.get 8
(;@205   ;)        i32.const 4
(;@207   ;)        i32.store
(;@20a   ;)        local.get 8
(;@20c   ;)        i32.const 21
(;@20e   ;)        i32.store offset=4
(;@211   ;)        local.get 8
(;@213   ;)        local.get 1
(;@215   ;)        i32.store offset=8
(;@218   ;)        local.get 8
(;@21a   ;)        local.set 8
(;@21c   ;)        i32.const 8
(;@21e   ;)        call $alloc
(;@220   ;)        local.set 9
(;@222   ;)        local.get 9
(;@224   ;)        i32.const 3
(;@226   ;)        i32.store
(;@229   ;)        local.get 9
(;@22b   ;)        i32.const 22
(;@22d   ;)        i32.store offset=4
(;@230   ;)        local.get 9
(;@232   ;)        local.set 9
(;@234   ;)        i32.const 8
(;@236   ;)        call $alloc
(;@238   ;)        local.set 10
(;@23a   ;)        local.get 10
(;@23c   ;)        i32.const 3
(;@23e   ;)        i32.store
(;@241   ;)        local.get 10
(;@243   ;)        i32.const 25
(;@245   ;)        i32.store offset=4
(;@248   ;)        local.get 10
(;@24a   ;)        local.set 10
(;@24c   ;)        i32.const 16
(;@24e   ;)        call $alloc
(;@250   ;)        local.set 11
(;@252   ;)        local.get 11
(;@254   ;)        i32.const 6
(;@256   ;)        i32.store
(;@259   ;)        local.get 11
(;@25b   ;)        i32.const 32
(;@25d   ;)        i32.store offset=4
(;@260   ;)        local.get 11
(;@262   ;)        local.get 9
(;@264   ;)        i32.store offset=8
(;@267   ;)        local.get 11
(;@269   ;)        local.get 10
(;@26b   ;)        i32.store offset=12
(;@26e   ;)        local.get 11
(;@270   ;)        local.set 11
(;@272   ;)        i32.const 0
(;@274   ;)        call $alloc
(;@276   ;)        local.set 12
(;@278   ;)        local.get 12
(;@27a   ;)        local.set 12
(;@27c   ;)        local.get 1
(;@27e   ;)        local.get 8
(;@280   ;)        local.get 11
(;@282   ;)        local.get 12
(;@284   ;)        call $__mon_prompt
(;@286   ;)        br 1 (;@1;)
(;@288   ;)      end
(;@289   ;)      unreachable
(;@28a   ;)    end
(;@28b   ;)    local.set 13
(;@28d   ;)    local.get 13
(;@28f   ;)    i32.load
(;@292   ;)    local.set 14
(;@294   ;)    block (result i32) ;; label = @1
(;@296   ;)      block ;; label = @2
(;@298   ;)        block ;; label = @3
(;@29a   ;)          block ;; label = @4
(;@29c   ;)            local.get 14
(;@29e   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2a3   ;)          end
(;@2a4   ;)          local.get 13
(;@2a6   ;)          i32.load offset=4
(;@2a9   ;)          local.set 15
(;@2ab   ;)          local.get 15
(;@2ad   ;)          i32.const 4
(;@2af   ;)          i32.add
(;@2b0   ;)          i32.const 825
(;@2b3   ;)          local.get 15
(;@2b5   ;)          i32.load
(;@2b8   ;)          call_indirect (type $fun_2_1)
(;@2bb   ;)          local.set 16
(;@2bd   ;)          i32.const 8
(;@2bf   ;)          call $alloc
(;@2c1   ;)          local.set 17
(;@2c3   ;)          local.get 17
(;@2c5   ;)          i32.const 0
(;@2c7   ;)          i32.store
(;@2ca   ;)          local.get 17
(;@2cc   ;)          local.get 16
(;@2ce   ;)          i32.store offset=4
(;@2d1   ;)          local.get 17
(;@2d3   ;)          br 2 (;@1;)
(;@2d5   ;)        end
(;@2d6   ;)        local.get 13
(;@2d8   ;)        i32.load offset=4
(;@2db   ;)        local.set 17
(;@2dd   ;)        local.get 17
(;@2df   ;)        local.set 18
(;@2e1   ;)        local.get 18
(;@2e3   ;)        i32.load
(;@2e6   ;)        local.set 19
(;@2e8   ;)        local.get 18
(;@2ea   ;)        i32.load offset=4
(;@2ed   ;)        local.set 20
(;@2ef   ;)        i32.const 12
(;@2f1   ;)        call $alloc
(;@2f3   ;)        local.set 21
(;@2f5   ;)        local.get 21
(;@2f7   ;)        i32.const 4
(;@2f9   ;)        i32.store
(;@2fc   ;)        local.get 21
(;@2fe   ;)        i32.const 28
(;@300   ;)        i32.store offset=4
(;@303   ;)        local.get 21
(;@305   ;)        local.get 18
(;@307   ;)        i32.store offset=8
(;@30a   ;)        local.get 21
(;@30c   ;)        local.set 21
(;@30e   ;)        i32.const 12
(;@310   ;)        call $alloc
(;@312   ;)        local.set 22
(;@314   ;)        local.get 22
(;@316   ;)        local.get 19
(;@318   ;)        i32.store
(;@31b   ;)        local.get 22
(;@31d   ;)        local.get 20
(;@31f   ;)        i32.store offset=4
(;@322   ;)        local.get 22
(;@324   ;)        local.get 21
(;@326   ;)        i32.store offset=8
(;@329   ;)        local.get 22
(;@32b   ;)        local.set 22
(;@32d   ;)        i32.const 8
(;@32f   ;)        call $alloc
(;@331   ;)        local.set 23
(;@333   ;)        local.get 23
(;@335   ;)        i32.const 1
(;@337   ;)        i32.store
(;@33a   ;)        local.get 23
(;@33c   ;)        local.get 22
(;@33e   ;)        i32.store offset=4
(;@341   ;)        local.get 23
(;@343   ;)        br 1 (;@1;)
(;@345   ;)      end
(;@346   ;)      unreachable
(;@347   ;)    end
(;@348   ;)    local.set 23
(;@34a   ;)    local.get 23
(;@34c   ;)    i32.load
(;@34f   ;)    local.set 24
(;@351   ;)    block (result i32) ;; label = @1
(;@353   ;)      block ;; label = @2
(;@355   ;)        block ;; label = @3
(;@357   ;)          block ;; label = @4
(;@359   ;)            local.get 24
(;@35b   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@360   ;)          end
(;@361   ;)          local.get 23
(;@363   ;)          i32.load offset=4
(;@366   ;)          local.set 15
(;@368   ;)          local.get 15
(;@36a   ;)          i32.load offset=4
(;@36d   ;)          local.set 25
(;@36f   ;)          i32.const 8
(;@371   ;)          call $alloc
(;@373   ;)          local.set 26
(;@375   ;)          local.get 26
(;@377   ;)          i32.const 0
(;@379   ;)          i32.store
(;@37c   ;)          local.get 26
(;@37e   ;)          local.get 25
(;@380   ;)          i32.store offset=4
(;@383   ;)          local.get 26
(;@385   ;)          br 2 (;@1;)
(;@387   ;)        end
(;@388   ;)        local.get 23
(;@38a   ;)        i32.load offset=4
(;@38d   ;)        local.set 17
(;@38f   ;)        local.get 17
(;@391   ;)        local.set 18
(;@393   ;)        local.get 18
(;@395   ;)        i32.load
(;@398   ;)        local.set 26
(;@39a   ;)        local.get 18
(;@39c   ;)        i32.load offset=4
(;@39f   ;)        local.set 27
(;@3a1   ;)        i32.const 12
(;@3a3   ;)        call $alloc
(;@3a5   ;)        local.set 28
(;@3a7   ;)        local.get 28
(;@3a9   ;)        i32.const 4
(;@3ab   ;)        i32.store
(;@3ae   ;)        local.get 28
(;@3b0   ;)        i32.const 31
(;@3b2   ;)        i32.store offset=4
(;@3b5   ;)        local.get 28
(;@3b7   ;)        local.get 18
(;@3b9   ;)        i32.store offset=8
(;@3bc   ;)        local.get 28
(;@3be   ;)        local.set 28
(;@3c0   ;)        i32.const 12
(;@3c2   ;)        call $alloc
(;@3c4   ;)        local.set 29
(;@3c6   ;)        local.get 29
(;@3c8   ;)        local.get 26
(;@3ca   ;)        i32.store
(;@3cd   ;)        local.get 29
(;@3cf   ;)        local.get 27
(;@3d1   ;)        i32.store offset=4
(;@3d4   ;)        local.get 29
(;@3d6   ;)        local.get 28
(;@3d8   ;)        i32.store offset=8
(;@3db   ;)        local.get 29
(;@3dd   ;)        local.set 29
(;@3df   ;)        i32.const 8
(;@3e1   ;)        call $alloc
(;@3e3   ;)        local.set 30
(;@3e5   ;)        local.get 30
(;@3e7   ;)        i32.const 1
(;@3e9   ;)        i32.store
(;@3ec   ;)        local.get 30
(;@3ee   ;)        local.get 29
(;@3f0   ;)        i32.store offset=4
(;@3f3   ;)        local.get 30
(;@3f5   ;)        br 1 (;@1;)
(;@3f7   ;)      end
(;@3f8   ;)      unreachable
(;@3f9   ;)    end
(;@3fa   ;)    local.set 30
(;@3fc   ;)    local.get 30
(;@3fe   ;)    i32.load
(;@401   ;)    local.set 31
(;@403   ;)    block (result i32) ;; label = @1
(;@405   ;)      block ;; label = @2
(;@407   ;)        block ;; label = @3
(;@409   ;)          block ;; label = @4
(;@40b   ;)            local.get 31
(;@40d   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@412   ;)          end
(;@413   ;)          local.get 30
(;@415   ;)          i32.load offset=4
(;@418   ;)          local.set 32
(;@41a   ;)          local.get 32
(;@41c   ;)          br 2 (;@1;)
(;@41e   ;)        end
(;@41f   ;)        local.get 30
(;@421   ;)        i32.load offset=4
(;@424   ;)        local.set 32
(;@426   ;)        i32.const 5467
(;@429   ;)        br 1 (;@1;)
(;@42b   ;)      end
(;@42c   ;)      unreachable
(;@42d   ;)    end
(;@42e   ;)    return
             )
(;@431   ;)  (func $main_lam_0 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@432   ;)    (local i32)
(;@434   ;)    local.get 0
(;@436   ;)    i32.const 4
(;@438   ;)    i32.add
(;@439   ;)    local.get 1
(;@43b   ;)    local.get 1
(;@43d   ;)    local.get 0
(;@43f   ;)    i32.load
(;@442   ;)    call_indirect (type $fun_3_1)
(;@445   ;)    return
             )
(;@448   ;)  (func $main_lam_1 (;10;) (type $fun_1_1) (param i32) (result i32)
(;@449   ;)    (local i32)
(;@44b   ;)    i32.const 12
(;@44d   ;)    call $alloc
(;@44f   ;)    local.set 1
(;@451   ;)    local.get 1
(;@453   ;)    i32.const 4
(;@455   ;)    i32.store
(;@458   ;)    local.get 1
(;@45a   ;)    i32.const 9
(;@45c   ;)    i32.store offset=4
(;@45f   ;)    local.get 1
(;@461   ;)    local.get 0
(;@463   ;)    i32.store offset=8
(;@466   ;)    local.get 1
(;@468   ;)    return
             )
(;@46b   ;)  (func $main_lam_2 (;11;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@46c   ;)    (local i32)
(;@46e   ;)    local.get 1
(;@470   ;)    i32.const 4
(;@472   ;)    i32.add
(;@473   ;)    local.get 2
(;@475   ;)    local.get 2
(;@477   ;)    local.get 1
(;@479   ;)    i32.load
(;@47c   ;)    call_indirect (type $fun_3_1)
(;@47f   ;)    return
             )
(;@482   ;)  (func $main_lam_3 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@483   ;)    (local i32 i32)
(;@485   ;)    i32.const 0
(;@487   ;)    call $alloc
(;@489   ;)    local.set 3
(;@48b   ;)    local.get 3
(;@48d   ;)    local.set 3
(;@48f   ;)    local.get 1
(;@491   ;)    i32.const 4
(;@493   ;)    i32.add
(;@494   ;)    local.get 3
(;@496   ;)    local.get 0
(;@498   ;)    local.get 1
(;@49a   ;)    i32.load
(;@49d   ;)    call_indirect (type $fun_3_1)
(;@4a0   ;)    return
             )
(;@4a3   ;)  (func $main_lam_4 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4a4   ;)    (local i32 i32 i32 i32)
(;@4a6   ;)    i32.const 8
(;@4a8   ;)    call $alloc
(;@4aa   ;)    local.set 2
(;@4ac   ;)    local.get 2
(;@4ae   ;)    i32.const 5
(;@4b0   ;)    i32.store
(;@4b3   ;)    local.get 2
(;@4b5   ;)    i32.const 11
(;@4b7   ;)    i32.store offset=4
(;@4ba   ;)    local.get 2
(;@4bc   ;)    local.set 2
(;@4be   ;)    i32.const 8
(;@4c0   ;)    call $alloc
(;@4c2   ;)    local.set 3
(;@4c4   ;)    local.get 3
(;@4c6   ;)    i32.const 5
(;@4c8   ;)    i32.store
(;@4cb   ;)    local.get 3
(;@4cd   ;)    i32.const 12
(;@4cf   ;)    i32.store offset=4
(;@4d2   ;)    local.get 3
(;@4d4   ;)    local.set 3
(;@4d6   ;)    i32.const 8
(;@4d8   ;)    call $alloc
(;@4da   ;)    local.set 4
(;@4dc   ;)    local.get 4
(;@4de   ;)    local.get 2
(;@4e0   ;)    i32.store
(;@4e3   ;)    local.get 4
(;@4e5   ;)    local.get 3
(;@4e7   ;)    i32.store offset=4
(;@4ea   ;)    local.get 4
(;@4ec   ;)    local.set 4
(;@4ee   ;)    i32.const 8
(;@4f0   ;)    call $alloc
(;@4f2   ;)    local.set 5
(;@4f4   ;)    local.get 5
(;@4f6   ;)    local.get 0
(;@4f8   ;)    i32.store
(;@4fb   ;)    local.get 5
(;@4fd   ;)    local.get 4
(;@4ff   ;)    i32.store offset=4
(;@502   ;)    local.get 5
(;@504   ;)    return
             )
(;@507   ;)  (func $main_lam_5 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@508   ;)    (local i32)
(;@50a   ;)    i32.const 8
(;@50c   ;)    call $alloc
(;@50e   ;)    local.set 2
(;@510   ;)    local.get 2
(;@512   ;)    i32.const 0
(;@514   ;)    i32.store
(;@517   ;)    local.get 2
(;@519   ;)    local.get 0
(;@51b   ;)    i32.store offset=4
(;@51e   ;)    local.get 2
(;@520   ;)    return
             )
(;@523   ;)  (func $main_lam_6 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@524   ;)    (local i32)
(;@526   ;)    i32.const 8
(;@528   ;)    call $alloc
(;@52a   ;)    local.set 2
(;@52c   ;)    local.get 2
(;@52e   ;)    local.get 1
(;@530   ;)    i32.store
(;@533   ;)    local.get 2
(;@535   ;)    local.get 0
(;@537   ;)    i32.store offset=4
(;@53a   ;)    local.get 2
(;@53c   ;)    return
             )
(;@53f   ;)  (func $main_lam_7 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@540   ;)    (local i32 i32)
(;@542   ;)    i32.const 12
(;@544   ;)    call $alloc
(;@546   ;)    local.set 2
(;@548   ;)    local.get 2
(;@54a   ;)    i32.const 4
(;@54c   ;)    i32.store
(;@54f   ;)    local.get 2
(;@551   ;)    i32.const 15
(;@553   ;)    i32.store offset=4
(;@556   ;)    local.get 2
(;@558   ;)    local.get 0
(;@55a   ;)    i32.store offset=8
(;@55d   ;)    local.get 2
(;@55f   ;)    local.set 2
(;@561   ;)    i32.const 8
(;@563   ;)    call $alloc
(;@565   ;)    local.set 3
(;@567   ;)    local.get 3
(;@569   ;)    i32.const 0
(;@56b   ;)    i32.store
(;@56e   ;)    local.get 3
(;@570   ;)    local.get 2
(;@572   ;)    i32.store offset=4
(;@575   ;)    local.get 3
(;@577   ;)    return
             )
(;@57a   ;)  (func $main_lam_8 (;17;) (type $fun_1_1) (param i32) (result i32)
(;@57b   ;)    (local i32)
(;@57d   ;)    i32.const 12
(;@57f   ;)    call $alloc
(;@581   ;)    local.set 1
(;@583   ;)    local.get 1
(;@585   ;)    i32.const 4
(;@587   ;)    i32.store
(;@58a   ;)    local.get 1
(;@58c   ;)    i32.const 16
(;@58e   ;)    i32.store offset=4
(;@591   ;)    local.get 1
(;@593   ;)    local.get 0
(;@595   ;)    i32.store offset=8
(;@598   ;)    local.get 1
(;@59a   ;)    return
             )
(;@59e   ;)  (func $main_lam_9 (;18;) (type $fun_2_1) (param i32 i32) (result i32)
(;@59f   ;)    (local i32 i32 i32 i32 i32)
(;@5a1   ;)    i32.const 12
(;@5a3   ;)    call $alloc
(;@5a5   ;)    local.set 2
(;@5a7   ;)    local.get 2
(;@5a9   ;)    i32.const 4
(;@5ab   ;)    i32.store
(;@5ae   ;)    local.get 2
(;@5b0   ;)    i32.const 13
(;@5b2   ;)    i32.store offset=4
(;@5b5   ;)    local.get 2
(;@5b7   ;)    local.get 0
(;@5b9   ;)    i32.store offset=8
(;@5bc   ;)    local.get 2
(;@5be   ;)    local.set 2
(;@5c0   ;)    i32.const 12
(;@5c2   ;)    call $alloc
(;@5c4   ;)    local.set 3
(;@5c6   ;)    local.get 3
(;@5c8   ;)    i32.const 4
(;@5ca   ;)    i32.store
(;@5cd   ;)    local.get 3
(;@5cf   ;)    i32.const 14
(;@5d1   ;)    i32.store offset=4
(;@5d4   ;)    local.get 3
(;@5d6   ;)    local.get 1
(;@5d8   ;)    i32.store offset=8
(;@5db   ;)    local.get 3
(;@5dd   ;)    local.set 3
(;@5df   ;)    i32.const 8
(;@5e1   ;)    call $alloc
(;@5e3   ;)    local.set 4
(;@5e5   ;)    local.get 4
(;@5e7   ;)    i32.const 3
(;@5e9   ;)    i32.store
(;@5ec   ;)    local.get 4
(;@5ee   ;)    i32.const 17
(;@5f0   ;)    i32.store offset=4
(;@5f3   ;)    local.get 4
(;@5f5   ;)    local.set 4
(;@5f7   ;)    i32.const 16
(;@5f9   ;)    call $alloc
(;@5fb   ;)    local.set 5
(;@5fd   ;)    local.get 5
(;@5ff   ;)    i32.const 6
(;@601   ;)    i32.store
(;@604   ;)    local.get 5
(;@606   ;)    i32.const 32
(;@608   ;)    i32.store offset=4
(;@60b   ;)    local.get 5
(;@60d   ;)    local.get 3
(;@60f   ;)    i32.store offset=8
(;@612   ;)    local.get 5
(;@614   ;)    local.get 4
(;@616   ;)    i32.store offset=12
(;@619   ;)    local.get 5
(;@61b   ;)    local.set 5
(;@61d   ;)    i32.const 20
(;@61f   ;)    call $alloc
(;@621   ;)    local.set 6
(;@623   ;)    local.get 6
(;@625   ;)    i32.const 7
(;@627   ;)    i32.store
(;@62a   ;)    local.get 6
(;@62c   ;)    i32.const 34
(;@62e   ;)    i32.store offset=4
(;@631   ;)    local.get 6
(;@633   ;)    local.get 0
(;@635   ;)    i32.store offset=8
(;@638   ;)    local.get 6
(;@63a   ;)    local.get 2
(;@63c   ;)    i32.store offset=12
(;@63f   ;)    local.get 6
(;@641   ;)    local.get 5
(;@643   ;)    i32.store offset=16
(;@646   ;)    local.get 6
(;@648   ;)    return
             )
(;@64b   ;)  (func $main_lam_10 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@64c   ;)    (local i32)
(;@64e   ;)    local.get 1
(;@650   ;)    i32.const 4
(;@652   ;)    i32.add
(;@653   ;)    local.get 2
(;@655   ;)    local.get 2
(;@657   ;)    local.get 1
(;@659   ;)    i32.load
(;@65c   ;)    call_indirect (type $fun_3_1)
(;@65f   ;)    return
             )
(;@662   ;)  (func $main_lam_11 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@663   ;)    (local i32 i32)
(;@665   ;)    i32.const 0
(;@667   ;)    call $alloc
(;@669   ;)    local.set 3
(;@66b   ;)    local.get 3
(;@66d   ;)    local.set 3
(;@66f   ;)    local.get 1
(;@671   ;)    i32.const 4
(;@673   ;)    i32.add
(;@674   ;)    local.get 3
(;@676   ;)    local.get 0
(;@678   ;)    local.get 1
(;@67a   ;)    i32.load
(;@67d   ;)    call_indirect (type $fun_3_1)
(;@680   ;)    return
             )
(;@683   ;)  (func $main_lam_12 (;21;) (type $fun_2_1) (param i32 i32) (result i32)
(;@684   ;)    (local i32 i32 i32 i32)
(;@686   ;)    i32.const 8
(;@688   ;)    call $alloc
(;@68a   ;)    local.set 2
(;@68c   ;)    local.get 2
(;@68e   ;)    i32.const 5
(;@690   ;)    i32.store
(;@693   ;)    local.get 2
(;@695   ;)    i32.const 19
(;@697   ;)    i32.store offset=4
(;@69a   ;)    local.get 2
(;@69c   ;)    local.set 2
(;@69e   ;)    i32.const 8
(;@6a0   ;)    call $alloc
(;@6a2   ;)    local.set 3
(;@6a4   ;)    local.get 3
(;@6a6   ;)    i32.const 5
(;@6a8   ;)    i32.store
(;@6ab   ;)    local.get 3
(;@6ad   ;)    i32.const 20
(;@6af   ;)    i32.store offset=4
(;@6b2   ;)    local.get 3
(;@6b4   ;)    local.set 3
(;@6b6   ;)    i32.const 8
(;@6b8   ;)    call $alloc
(;@6ba   ;)    local.set 4
(;@6bc   ;)    local.get 4
(;@6be   ;)    local.get 2
(;@6c0   ;)    i32.store
(;@6c3   ;)    local.get 4
(;@6c5   ;)    local.get 3
(;@6c7   ;)    i32.store offset=4
(;@6ca   ;)    local.get 4
(;@6cc   ;)    local.set 4
(;@6ce   ;)    i32.const 8
(;@6d0   ;)    call $alloc
(;@6d2   ;)    local.set 5
(;@6d4   ;)    local.get 5
(;@6d6   ;)    local.get 0
(;@6d8   ;)    i32.store
(;@6db   ;)    local.get 5
(;@6dd   ;)    local.get 4
(;@6df   ;)    i32.store offset=4
(;@6e2   ;)    local.get 5
(;@6e4   ;)    return
             )
(;@6e7   ;)  (func $main_lam_13 (;22;) (type $fun_1_1) (param i32) (result i32)
(;@6e8   ;)    (local i32 i32)
(;@6ea   ;)    i32.const 0
(;@6ec   ;)    call $alloc
(;@6ee   ;)    local.set 1
(;@6f0   ;)    local.get 1
(;@6f2   ;)    local.set 1
(;@6f4   ;)    i32.const 8
(;@6f6   ;)    call $alloc
(;@6f8   ;)    local.set 2
(;@6fa   ;)    local.get 2
(;@6fc   ;)    i32.const 0
(;@6fe   ;)    i32.store
(;@701   ;)    local.get 2
(;@703   ;)    local.get 1
(;@705   ;)    i32.store offset=4
(;@708   ;)    local.get 2
(;@70a   ;)    return
             )
(;@70d   ;)  (func $main_lam_14 (;23;) (type $fun_2_1) (param i32 i32) (result i32)
(;@70e   ;)    (local i32)
(;@710   ;)    i32.const 8
(;@712   ;)    call $alloc
(;@714   ;)    local.set 2
(;@716   ;)    local.get 2
(;@718   ;)    local.get 1
(;@71a   ;)    i32.store
(;@71d   ;)    local.get 2
(;@71f   ;)    local.get 0
(;@721   ;)    i32.store offset=4
(;@724   ;)    local.get 2
(;@726   ;)    return
             )
(;@729   ;)  (func $main_lam_15 (;24;) (type $fun_2_1) (param i32 i32) (result i32)
(;@72a   ;)    (local i32 i32)
(;@72c   ;)    i32.const 12
(;@72e   ;)    call $alloc
(;@730   ;)    local.set 2
(;@732   ;)    local.get 2
(;@734   ;)    i32.const 4
(;@736   ;)    i32.store
(;@739   ;)    local.get 2
(;@73b   ;)    i32.const 23
(;@73d   ;)    i32.store offset=4
(;@740   ;)    local.get 2
(;@742   ;)    local.get 0
(;@744   ;)    i32.store offset=8
(;@747   ;)    local.get 2
(;@749   ;)    local.set 2
(;@74b   ;)    i32.const 8
(;@74d   ;)    call $alloc
(;@74f   ;)    local.set 3
(;@751   ;)    local.get 3
(;@753   ;)    i32.const 0
(;@755   ;)    i32.store
(;@758   ;)    local.get 3
(;@75a   ;)    local.get 2
(;@75c   ;)    i32.store offset=4
(;@75f   ;)    local.get 3
(;@761   ;)    return
             )
(;@764   ;)  (func $main_lam_16 (;25;) (type $fun_1_1) (param i32) (result i32)
(;@765   ;)    (local i32)
(;@767   ;)    i32.const 12
(;@769   ;)    call $alloc
(;@76b   ;)    local.set 1
(;@76d   ;)    local.get 1
(;@76f   ;)    i32.const 4
(;@771   ;)    i32.store
(;@774   ;)    local.get 1
(;@776   ;)    i32.const 24
(;@778   ;)    i32.store offset=4
(;@77b   ;)    local.get 1
(;@77d   ;)    local.get 0
(;@77f   ;)    i32.store offset=8
(;@782   ;)    local.get 1
(;@784   ;)    return
             )
(;@787   ;)  (func $main_lam_17 (;26;) (type $fun_2_1) (param i32 i32) (result i32)
(;@788   ;)    (local i32)
(;@78a   ;)    i32.const 8
(;@78c   ;)    call $alloc
(;@78e   ;)    local.set 2
(;@790   ;)    local.get 2
(;@792   ;)    i32.const 0
(;@794   ;)    i32.store
(;@797   ;)    local.get 2
(;@799   ;)    local.get 0
(;@79b   ;)    i32.store offset=4
(;@79e   ;)    local.get 2
(;@7a0   ;)    return
             )
(;@7a3   ;)  (func $main_lam_18 (;27;) (type $fun_1_1) (param i32) (result i32)
(;@7a4   ;)    (local i32 i32)
(;@7a6   ;)    local.get 0
(;@7a8   ;)    i32.const 4
(;@7aa   ;)    i32.add
(;@7ab   ;)    i32.const 825
(;@7ae   ;)    local.get 0
(;@7b0   ;)    i32.load
(;@7b3   ;)    call_indirect (type $fun_2_1)
(;@7b6   ;)    local.set 1
(;@7b8   ;)    i32.const 12
(;@7ba   ;)    call $alloc
(;@7bc   ;)    local.set 2
(;@7be   ;)    local.get 2
(;@7c0   ;)    i32.const 4
(;@7c2   ;)    i32.store
(;@7c5   ;)    local.get 2
(;@7c7   ;)    i32.const 26
(;@7c9   ;)    i32.store offset=4
(;@7cc   ;)    local.get 2
(;@7ce   ;)    local.get 1
(;@7d0   ;)    i32.store offset=8
(;@7d3   ;)    local.get 2
(;@7d5   ;)    return
             )
(;@7d8   ;)  (func $main_lam_19 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
(;@7d9   ;)    (local i32 i32 i32 i32)
(;@7db   ;)    local.get 0
(;@7dd   ;)    i32.load offset=8
(;@7e0   ;)    local.set 2
(;@7e2   ;)    local.get 2
(;@7e4   ;)    i32.const 4
(;@7e6   ;)    i32.add
(;@7e7   ;)    local.get 1
(;@7e9   ;)    local.get 2
(;@7eb   ;)    i32.load
(;@7ee   ;)    call_indirect (type $fun_2_1)
(;@7f1   ;)    local.set 3
(;@7f3   ;)    i32.const 8
(;@7f5   ;)    call $alloc
(;@7f7   ;)    local.set 4
(;@7f9   ;)    local.get 4
(;@7fb   ;)    i32.const 3
(;@7fd   ;)    i32.store
(;@800   ;)    local.get 4
(;@802   ;)    i32.const 27
(;@804   ;)    i32.store offset=4
(;@807   ;)    local.get 4
(;@809   ;)    local.set 4
(;@80b   ;)    i32.const 16
(;@80d   ;)    call $alloc
(;@80f   ;)    local.set 5
(;@811   ;)    local.get 5
(;@813   ;)    i32.const 6
(;@815   ;)    i32.store
(;@818   ;)    local.get 5
(;@81a   ;)    i32.const 32
(;@81c   ;)    i32.store offset=4
(;@81f   ;)    local.get 5
(;@821   ;)    local.get 3
(;@823   ;)    i32.store offset=8
(;@826   ;)    local.get 5
(;@828   ;)    local.get 4
(;@82a   ;)    i32.store offset=12
(;@82d   ;)    local.get 5
(;@82f   ;)    return
             )
(;@832   ;)  (func $main_lam_20 (;29;) (type $fun_2_1) (param i32 i32) (result i32)
(;@833   ;)    (local i32 i32)
(;@835   ;)    local.get 0
(;@837   ;)    i32.load offset=4
(;@83a   ;)    local.set 2
(;@83c   ;)    i32.const 8
(;@83e   ;)    call $alloc
(;@840   ;)    local.set 3
(;@842   ;)    local.get 3
(;@844   ;)    i32.const 0
(;@846   ;)    i32.store
(;@849   ;)    local.get 3
(;@84b   ;)    local.get 2
(;@84d   ;)    i32.store offset=4
(;@850   ;)    local.get 3
(;@852   ;)    return
             )
(;@855   ;)  (func $main_lam_21 (;30;) (type $fun_1_1) (param i32) (result i32)
(;@856   ;)    (local i32)
(;@858   ;)    i32.const 12
(;@85a   ;)    call $alloc
(;@85c   ;)    local.set 1
(;@85e   ;)    local.get 1
(;@860   ;)    i32.const 4
(;@862   ;)    i32.store
(;@865   ;)    local.get 1
(;@867   ;)    i32.const 29
(;@869   ;)    i32.store offset=4
(;@86c   ;)    local.get 1
(;@86e   ;)    local.get 0
(;@870   ;)    i32.store offset=8
(;@873   ;)    local.get 1
(;@875   ;)    return
             )
(;@878   ;)  (func $main_lam_22 (;31;) (type $fun_2_1) (param i32 i32) (result i32)
(;@879   ;)    (local i32 i32 i32 i32)
(;@87b   ;)    local.get 0
(;@87d   ;)    i32.load offset=8
(;@880   ;)    local.set 2
(;@882   ;)    local.get 2
(;@884   ;)    i32.const 4
(;@886   ;)    i32.add
(;@887   ;)    local.get 1
(;@889   ;)    local.get 2
(;@88b   ;)    i32.load
(;@88e   ;)    call_indirect (type $fun_2_1)
(;@891   ;)    local.set 3
(;@893   ;)    i32.const 8
(;@895   ;)    call $alloc
(;@897   ;)    local.set 4
(;@899   ;)    local.get 4
(;@89b   ;)    i32.const 3
(;@89d   ;)    i32.store
(;@8a0   ;)    local.get 4
(;@8a2   ;)    i32.const 30
(;@8a4   ;)    i32.store offset=4
(;@8a7   ;)    local.get 4
(;@8a9   ;)    local.set 4
(;@8ab   ;)    i32.const 16
(;@8ad   ;)    call $alloc
(;@8af   ;)    local.set 5
(;@8b1   ;)    local.get 5
(;@8b3   ;)    i32.const 6
(;@8b5   ;)    i32.store
(;@8b8   ;)    local.get 5
(;@8ba   ;)    i32.const 32
(;@8bc   ;)    i32.store offset=4
(;@8bf   ;)    local.get 5
(;@8c1   ;)    local.get 3
(;@8c3   ;)    i32.store offset=8
(;@8c6   ;)    local.get 5
(;@8c8   ;)    local.get 4
(;@8ca   ;)    i32.store offset=12
(;@8cd   ;)    local.get 5
(;@8cf   ;)    return
             )
(;@8d3   ;)  (func $__mon_bind (;32;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@8d4   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@8d6   ;)    local.get 0
(;@8d8   ;)    i32.const 4
(;@8da   ;)    i32.add
(;@8db   ;)    local.get 2
(;@8dd   ;)    local.get 0
(;@8df   ;)    i32.load
(;@8e2   ;)    call_indirect (type $fun_2_1)
(;@8e5   ;)    local.set 3
(;@8e7   ;)    local.get 3
(;@8e9   ;)    i32.load
(;@8ec   ;)    local.set 4
(;@8ee   ;)    block (result i32) ;; label = @1
(;@8f0   ;)      block ;; label = @2
(;@8f2   ;)        block ;; label = @3
(;@8f4   ;)          block ;; label = @4
(;@8f6   ;)            local.get 4
(;@8f8   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@8fd   ;)          end
(;@8fe   ;)          local.get 3
(;@900   ;)          i32.load offset=4
(;@903   ;)          local.set 5
(;@905   ;)          local.get 1
(;@907   ;)          i32.const 4
(;@909   ;)          i32.add
(;@90a   ;)          local.get 5
(;@90c   ;)          local.get 2
(;@90e   ;)          local.get 1
(;@910   ;)          i32.load
(;@913   ;)          call_indirect (type $fun_3_1)
(;@916   ;)          br 2 (;@1;)
(;@918   ;)        end
(;@919   ;)        local.get 3
(;@91b   ;)        i32.load offset=4
(;@91e   ;)        local.set 6
(;@920   ;)        local.get 6
(;@922   ;)        local.set 7
(;@924   ;)        local.get 7
(;@926   ;)        i32.load
(;@929   ;)        local.set 8
(;@92b   ;)        local.get 7
(;@92d   ;)        i32.load offset=4
(;@930   ;)        local.set 9
(;@932   ;)        i32.const 16
(;@934   ;)        call $alloc
(;@936   ;)        local.set 10
(;@938   ;)        local.get 10
(;@93a   ;)        i32.const 6
(;@93c   ;)        i32.store
(;@93f   ;)        local.get 10
(;@941   ;)        i32.const 33
(;@943   ;)        i32.store offset=4
(;@946   ;)        local.get 10
(;@948   ;)        local.get 1
(;@94a   ;)        i32.store offset=8
(;@94d   ;)        local.get 10
(;@94f   ;)        local.get 7
(;@951   ;)        i32.store offset=12
(;@954   ;)        local.get 10
(;@956   ;)        local.set 10
(;@958   ;)        i32.const 12
(;@95a   ;)        call $alloc
(;@95c   ;)        local.set 11
(;@95e   ;)        local.get 11
(;@960   ;)        local.get 8
(;@962   ;)        i32.store
(;@965   ;)        local.get 11
(;@967   ;)        local.get 9
(;@969   ;)        i32.store offset=4
(;@96c   ;)        local.get 11
(;@96e   ;)        local.get 10
(;@970   ;)        i32.store offset=8
(;@973   ;)        local.get 11
(;@975   ;)        local.set 11
(;@977   ;)        i32.const 8
(;@979   ;)        call $alloc
(;@97b   ;)        local.set 12
(;@97d   ;)        local.get 12
(;@97f   ;)        i32.const 1
(;@981   ;)        i32.store
(;@984   ;)        local.get 12
(;@986   ;)        local.get 11
(;@988   ;)        i32.store offset=4
(;@98b   ;)        local.get 12
(;@98d   ;)        br 1 (;@1;)
(;@98f   ;)      end
(;@990   ;)      unreachable
(;@991   ;)    end
(;@992   ;)    return
             )
(;@995   ;)  (func $__mon_bind_lam_0 (;33;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@996   ;)    (local i32 i32 i32)
(;@998   ;)    local.get 1
(;@99a   ;)    i32.load offset=8
(;@99d   ;)    local.set 3
(;@99f   ;)    local.get 3
(;@9a1   ;)    i32.const 4
(;@9a3   ;)    i32.add
(;@9a4   ;)    local.get 2
(;@9a6   ;)    local.get 3
(;@9a8   ;)    i32.load
(;@9ab   ;)    call_indirect (type $fun_2_1)
(;@9ae   ;)    local.set 4
(;@9b0   ;)    i32.const 16
(;@9b2   ;)    call $alloc
(;@9b4   ;)    local.set 5
(;@9b6   ;)    local.get 5
(;@9b8   ;)    i32.const 6
(;@9ba   ;)    i32.store
(;@9bd   ;)    local.get 5
(;@9bf   ;)    i32.const 32
(;@9c1   ;)    i32.store offset=4
(;@9c4   ;)    local.get 5
(;@9c6   ;)    local.get 4
(;@9c8   ;)    i32.store offset=8
(;@9cb   ;)    local.get 5
(;@9cd   ;)    local.get 0
(;@9cf   ;)    i32.store offset=12
(;@9d2   ;)    local.get 5
(;@9d4   ;)    return
             )
(;@9d8   ;)  (func $__mon_prompt (;34;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@9d9   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@9db   ;)    local.get 1
(;@9dd   ;)    i32.const 4
(;@9df   ;)    i32.add
(;@9e0   ;)    local.get 3
(;@9e2   ;)    local.get 1
(;@9e4   ;)    i32.load
(;@9e7   ;)    call_indirect (type $fun_2_1)
(;@9ea   ;)    local.set 4
(;@9ec   ;)    local.get 2
(;@9ee   ;)    i32.const 4
(;@9f0   ;)    i32.add
(;@9f1   ;)    local.get 4
(;@9f3   ;)    local.get 2
(;@9f5   ;)    i32.load
(;@9f8   ;)    call_indirect (type $fun_2_1)
(;@9fb   ;)    local.set 5
(;@9fd   ;)    local.get 5
(;@9ff   ;)    i32.load
(;@a02   ;)    local.set 6
(;@a04   ;)    block (result i32) ;; label = @1
(;@a06   ;)      block ;; label = @2
(;@a08   ;)        block ;; label = @3
(;@a0a   ;)          block ;; label = @4
(;@a0c   ;)            local.get 6
(;@a0e   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@a13   ;)          end
(;@a14   ;)          local.get 5
(;@a16   ;)          i32.load offset=4
(;@a19   ;)          local.set 7
(;@a1b   ;)          i32.const 8
(;@a1d   ;)          call $alloc
(;@a1f   ;)          local.set 8
(;@a21   ;)          local.get 8
(;@a23   ;)          i32.const 0
(;@a25   ;)          i32.store
(;@a28   ;)          local.get 8
(;@a2a   ;)          local.get 7
(;@a2c   ;)          i32.store offset=4
(;@a2f   ;)          local.get 8
(;@a31   ;)          br 2 (;@1;)
(;@a33   ;)        end
(;@a34   ;)        local.get 5
(;@a36   ;)        i32.load offset=4
(;@a39   ;)        local.set 8
(;@a3b   ;)        local.get 8
(;@a3d   ;)        i32.load
(;@a40   ;)        local.set 9
(;@a42   ;)        local.get 0
(;@a44   ;)        local.get 9
(;@a46   ;)        call $__mon_eqm
(;@a48   ;)        local.set 10
(;@a4a   ;)        local.get 10
(;@a4c   ;)        i32.load
(;@a4f   ;)        local.set 11
(;@a51   ;)        block (result i32) ;; label = @3
(;@a53   ;)          block ;; label = @4
(;@a55   ;)            block ;; label = @5
(;@a57   ;)              block ;; label = @6
(;@a59   ;)                local.get 11
(;@a5b   ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@a60   ;)              end
(;@a61   ;)              local.get 10
(;@a63   ;)              i32.load offset=4
(;@a66   ;)              local.set 12
(;@a68   ;)              local.get 8
(;@a6a   ;)              local.set 13
(;@a6c   ;)              local.get 13
(;@a6e   ;)              i32.load
(;@a71   ;)              local.set 14
(;@a73   ;)              local.get 13
(;@a75   ;)              i32.load offset=4
(;@a78   ;)              local.set 15
(;@a7a   ;)              i32.const 20
(;@a7c   ;)              call $alloc
(;@a7e   ;)              local.set 16
(;@a80   ;)              local.get 16
(;@a82   ;)              i32.const 7
(;@a84   ;)              i32.store
(;@a87   ;)              local.get 16
(;@a89   ;)              i32.const 35
(;@a8b   ;)              i32.store offset=4
(;@a8e   ;)              local.get 16
(;@a90   ;)              local.get 0
(;@a92   ;)              i32.store offset=8
(;@a95   ;)              local.get 16
(;@a97   ;)              local.get 1
(;@a99   ;)              i32.store offset=12
(;@a9c   ;)              local.get 16
(;@a9e   ;)              local.get 13
(;@aa0   ;)              i32.store offset=16
(;@aa3   ;)              local.get 16
(;@aa5   ;)              local.set 16
(;@aa7   ;)              i32.const 12
(;@aa9   ;)              call $alloc
(;@aab   ;)              local.set 17
(;@aad   ;)              local.get 17
(;@aaf   ;)              local.get 14
(;@ab1   ;)              i32.store
(;@ab4   ;)              local.get 17
(;@ab6   ;)              local.get 15
(;@ab8   ;)              i32.store offset=4
(;@abb   ;)              local.get 17
(;@abd   ;)              local.get 16
(;@abf   ;)              i32.store offset=8
(;@ac2   ;)              local.get 17
(;@ac4   ;)              local.set 17
(;@ac6   ;)              i32.const 8
(;@ac8   ;)              call $alloc
(;@aca   ;)              local.set 18
(;@acc   ;)              local.get 18
(;@ace   ;)              i32.const 1
(;@ad0   ;)              i32.store
(;@ad3   ;)              local.get 18
(;@ad5   ;)              local.get 17
(;@ad7   ;)              i32.store offset=4
(;@ada   ;)              local.get 18
(;@adc   ;)              br 2 (;@3;)
(;@ade   ;)            end
(;@adf   ;)            local.get 10
(;@ae1   ;)            i32.load offset=4
(;@ae4   ;)            local.set 12
(;@ae6   ;)            i32.const 20
(;@ae8   ;)            call $alloc
(;@aea   ;)            local.set 18
(;@aec   ;)            local.get 18
(;@aee   ;)            i32.const 7
(;@af0   ;)            i32.store
(;@af3   ;)            local.get 18
(;@af5   ;)            i32.const 36
(;@af7   ;)            i32.store offset=4
(;@afa   ;)            local.get 18
(;@afc   ;)            local.get 0
(;@afe   ;)            i32.store offset=8
(;@b01   ;)            local.get 18
(;@b03   ;)            local.get 1
(;@b05   ;)            i32.store offset=12
(;@b08   ;)            local.get 18
(;@b0a   ;)            local.get 8
(;@b0c   ;)            i32.store offset=16
(;@b0f   ;)            local.get 18
(;@b11   ;)            local.set 18
(;@b13   ;)            local.get 8
(;@b15   ;)            i32.load offset=4
(;@b18   ;)            local.set 19
(;@b1a   ;)            local.get 19
(;@b1c   ;)            i32.const 4
(;@b1e   ;)            i32.add
(;@b1f   ;)            local.get 18
(;@b21   ;)            local.get 3
(;@b23   ;)            local.get 19
(;@b25   ;)            i32.load
(;@b28   ;)            call_indirect (type $fun_3_1)
(;@b2b   ;)            br 1 (;@3;)
(;@b2d   ;)          end
(;@b2e   ;)          unreachable
(;@b2f   ;)        end
(;@b30   ;)        br 1 (;@1;)
(;@b32   ;)      end
(;@b33   ;)      unreachable
(;@b34   ;)    end
(;@b35   ;)    return
             )
(;@b38   ;)  (func $__mon_prompt_lam_0 (;35;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@b39   ;)    (local i32 i32 i32)
(;@b3b   ;)    local.get 2
(;@b3d   ;)    i32.load offset=8
(;@b40   ;)    local.set 4
(;@b42   ;)    local.get 4
(;@b44   ;)    i32.const 4
(;@b46   ;)    i32.add
(;@b47   ;)    local.get 3
(;@b49   ;)    local.get 4
(;@b4b   ;)    i32.load
(;@b4e   ;)    call_indirect (type $fun_2_1)
(;@b51   ;)    local.set 5
(;@b53   ;)    i32.const 20
(;@b55   ;)    call $alloc
(;@b57   ;)    local.set 6
(;@b59   ;)    local.get 6
(;@b5b   ;)    i32.const 7
(;@b5d   ;)    i32.store
(;@b60   ;)    local.get 6
(;@b62   ;)    i32.const 34
(;@b64   ;)    i32.store offset=4
(;@b67   ;)    local.get 6
(;@b69   ;)    local.get 0
(;@b6b   ;)    i32.store offset=8
(;@b6e   ;)    local.get 6
(;@b70   ;)    local.get 1
(;@b72   ;)    i32.store offset=12
(;@b75   ;)    local.get 6
(;@b77   ;)    local.get 5
(;@b79   ;)    i32.store offset=16
(;@b7c   ;)    local.get 6
(;@b7e   ;)    return
             )
(;@b81   ;)  (func $__mon_prompt_lam_1 (;36;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@b82   ;)    (local i32 i32 i32)
(;@b84   ;)    local.get 2
(;@b86   ;)    i32.load offset=8
(;@b89   ;)    local.set 4
(;@b8b   ;)    local.get 4
(;@b8d   ;)    i32.const 4
(;@b8f   ;)    i32.add
(;@b90   ;)    local.get 3
(;@b92   ;)    local.get 4
(;@b94   ;)    i32.load
(;@b97   ;)    call_indirect (type $fun_2_1)
(;@b9a   ;)    local.set 5
(;@b9c   ;)    i32.const 20
(;@b9e   ;)    call $alloc
(;@ba0   ;)    local.set 6
(;@ba2   ;)    local.get 6
(;@ba4   ;)    i32.const 7
(;@ba6   ;)    i32.store
(;@ba9   ;)    local.get 6
(;@bab   ;)    i32.const 34
(;@bad   ;)    i32.store offset=4
(;@bb0   ;)    local.get 6
(;@bb2   ;)    local.get 0
(;@bb4   ;)    i32.store offset=8
(;@bb7   ;)    local.get 6
(;@bb9   ;)    local.get 1
(;@bbb   ;)    i32.store offset=12
(;@bbe   ;)    local.get 6
(;@bc0   ;)    local.get 5
(;@bc2   ;)    i32.store offset=16
(;@bc5   ;)    local.get 6
(;@bc7   ;)    return
             )
(;@57    ;)  (table (;0;) 37 37 funcref)
(;@5e    ;)  (memory (;0;) 1)
(;@63    ;)  (global (;0;) (mut i32) i32.const 0)
(;@68    ;)  (global (;1;) (mut i32) i32.const 0)
(;@70    ;)  (export "main" (func $main))
(;@77    ;)  (export "mem" (memory 0))
(;@80    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__apply_1_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $__apply_4_3 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $main_lam_10 $main_lam_11 $main_lam_12 $main_lam_13 $main_lam_14 $main_lam_15 $main_lam_16 $main_lam_17 $main_lam_18 $main_lam_19 $main_lam_20 $main_lam_21 $main_lam_22 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
