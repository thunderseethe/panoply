(module $state
(;@b     ;)  (type $fun_0_1 (;0;) (func (result i32)))
(;@f     ;)  (type $fun_1_1 (;1;) (func (param i32) (result i32)))
(;@14    ;)  (type $fun_2_1 (;2;) (func (param i32 i32) (result i32)))
(;@1a    ;)  (type $fun_3_1 (;3;) (func (param i32 i32 i32) (result i32)))
(;@21    ;)  (type $fun_4_1 (;4;) (func (param i32 i32 i32 i32) (result i32)))
(;@29    ;)  (type $fun_5_1 (;5;) (func (param i32 i32 i32 i32 i32) (result i32)))
(;@140   ;)  (func $__mon_generate_marker (;0;) (type $fun_0_1) (result i32)
(;@141   ;)    (local i32)
(;@143   ;)    global.get 1
(;@145   ;)    global.get 1
(;@147   ;)    i32.const 1
(;@149   ;)    i32.add
(;@14a   ;)    global.set 1
(;@14c   ;)    return
             )
(;@14f   ;)  (func $alloc (;1;) (type $fun_1_1) (param i32) (result i32)
(;@150   ;)    (local i32)
(;@152   ;)    global.get 0
(;@154   ;)    global.get 0
(;@156   ;)    local.get 0
(;@158   ;)    i32.add
(;@159   ;)    global.set 0
(;@15b   ;)    return
             )
(;@15e   ;)  (func $__mon_eqm (;2;) (type $fun_2_1) (param i32 i32) (result i32)
(;@15f   ;)    (local i32)
(;@161   ;)    i32.const 4
(;@163   ;)    call $alloc
(;@165   ;)    local.tee 2
(;@167   ;)    i32.const 1
(;@169   ;)    i32.const 0
(;@16b   ;)    local.get 0
(;@16d   ;)    local.get 1
(;@16f   ;)    i32.eq
(;@170   ;)    select
(;@171   ;)    i32.store
(;@174   ;)    local.get 2
(;@176   ;)    return
             )
(;@179   ;)  (func $__call_1 (;3;) (type $fun_2_1) (param i32 i32) (result i32)
(;@17a   ;)    (local i32)
(;@17c   ;)    local.get 0
(;@17e   ;)    i32.const 12
(;@180   ;)    i32.add
(;@181   ;)    local.set 2
(;@183   ;)    block ;; label = @1
(;@185   ;)      block ;; label = @2
(;@187   ;)        block ;; label = @3
(;@189   ;)          local.get 0
(;@18b   ;)          i32.load
(;@18e   ;)          br_table 0 (;@3;) 1 (;@2;) 2 (;@1;)
(;@193   ;)        end
(;@194   ;)        local.get 2
(;@196   ;)        local.get 0
(;@198   ;)        i32.load offset=8
(;@19b   ;)        call_indirect (type $fun_1_1)
(;@19e   ;)        local.get 1
(;@1a0   ;)        call $__call_1
(;@1a2   ;)        return
(;@1a3   ;)      end
(;@1a4   ;)      local.get 2
(;@1a6   ;)      local.get 1
(;@1a8   ;)      local.get 0
(;@1aa   ;)      i32.load offset=8
(;@1ad   ;)      call_indirect (type $fun_2_1)
(;@1b0   ;)      return
(;@1b1   ;)    end
(;@1b2   ;)    unreachable
             )
(;@1b5   ;)  (func $__call_2 (;4;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1b6   ;)    (local i32)
(;@1b8   ;)    local.get 0
(;@1ba   ;)    i32.const 12
(;@1bc   ;)    i32.add
(;@1bd   ;)    local.set 3
(;@1bf   ;)    block ;; label = @1
(;@1c1   ;)      block ;; label = @2
(;@1c3   ;)        block ;; label = @3
(;@1c5   ;)          block ;; label = @4
(;@1c7   ;)            local.get 0
(;@1c9   ;)            i32.load
(;@1cc   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 3 (;@1;)
(;@1d2   ;)          end
(;@1d3   ;)          local.get 3
(;@1d5   ;)          local.get 0
(;@1d7   ;)          i32.load offset=8
(;@1da   ;)          call_indirect (type $fun_1_1)
(;@1dd   ;)          local.get 1
(;@1df   ;)          local.get 2
(;@1e1   ;)          call $__call_2
(;@1e3   ;)          return
(;@1e4   ;)        end
(;@1e5   ;)        local.get 3
(;@1e7   ;)        local.get 1
(;@1e9   ;)        local.get 0
(;@1eb   ;)        i32.load offset=8
(;@1ee   ;)        call_indirect (type $fun_2_1)
(;@1f1   ;)        local.get 2
(;@1f3   ;)        call $__call_1
(;@1f5   ;)        return
(;@1f6   ;)      end
(;@1f7   ;)      local.get 3
(;@1f9   ;)      local.get 1
(;@1fb   ;)      local.get 2
(;@1fd   ;)      local.get 0
(;@1ff   ;)      i32.load offset=8
(;@202   ;)      call_indirect (type $fun_3_1)
(;@205   ;)      return
(;@206   ;)    end
(;@207   ;)    unreachable
             )
(;@20a   ;)  (func $f (;5;) (type $fun_2_1) (param i32 i32) (result i32)
(;@20b   ;)    (local i32 i32)
(;@20d   ;)    i32.const 16
(;@20f   ;)    call $alloc
(;@211   ;)    local.set 2
(;@213   ;)    local.get 2
(;@215   ;)    i32.const 1
(;@217   ;)    i32.store
(;@21a   ;)    local.get 2
(;@21c   ;)    i32.const 1
(;@21e   ;)    i32.store offset=4
(;@221   ;)    local.get 2
(;@223   ;)    i32.const 87
(;@226   ;)    i32.store offset=8
(;@229   ;)    local.get 2
(;@22b   ;)    local.get 0
(;@22d   ;)    i32.store offset=12
(;@230   ;)    local.get 2
(;@232   ;)    local.set 2
(;@234   ;)    i32.const 8
(;@236   ;)    call $alloc
(;@238   ;)    local.set 3
(;@23a   ;)    local.get 3
(;@23c   ;)    i32.const 0
(;@23e   ;)    i32.store
(;@241   ;)    local.get 3
(;@243   ;)    local.get 2
(;@245   ;)    i32.store offset=4
(;@248   ;)    local.get 3
(;@24a   ;)    return
             )
(;@24d   ;)  (func $f_lam_0 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@24e   ;)    (local i32 i32 i32)
(;@250   ;)    local.get 0
(;@252   ;)    i32.load
(;@255   ;)    local.set 2
(;@257   ;)    local.get 0
(;@259   ;)    i32.load offset=4
(;@25c   ;)    local.set 3
(;@25e   ;)    local.get 2
(;@260   ;)    local.get 3
(;@262   ;)    local.get 1
(;@264   ;)    call $__call_2
(;@266   ;)    return
             )
(;@269   ;)  (func $f_lam_1 (;7;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@26a   ;)    (local i32 i32)
(;@26c   ;)    local.get 0
(;@26e   ;)    i32.load
(;@271   ;)    local.set 3
(;@273   ;)    local.get 1
(;@275   ;)    local.get 3
(;@277   ;)    local.get 2
(;@279   ;)    call $__call_2
(;@27b   ;)    return
             )
(;@27e   ;)  (func $f_lam_2 (;8;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@27f   ;)    (local i32 i32 i32 i32 i32 i32)
(;@281   ;)    local.get 0
(;@283   ;)    i32.load
(;@286   ;)    local.set 3
(;@288   ;)    local.get 0
(;@28a   ;)    i32.load offset=4
(;@28d   ;)    local.set 4
(;@28f   ;)    local.get 4
(;@291   ;)    i32.load offset=8
(;@294   ;)    local.set 5
(;@296   ;)    local.get 5
(;@298   ;)    local.get 1
(;@29a   ;)    call $__call_1
(;@29c   ;)    local.set 6
(;@29e   ;)    i32.const 16
(;@2a0   ;)    call $alloc
(;@2a2   ;)    local.set 7
(;@2a4   ;)    local.get 7
(;@2a6   ;)    i32.const 2
(;@2a8   ;)    i32.store
(;@2ab   ;)    local.get 7
(;@2ad   ;)    i32.const 1
(;@2af   ;)    i32.store offset=4
(;@2b2   ;)    local.get 7
(;@2b4   ;)    i32.const 7
(;@2b6   ;)    i32.store offset=8
(;@2b9   ;)    local.get 7
(;@2bb   ;)    local.get 3
(;@2bd   ;)    i32.store offset=12
(;@2c0   ;)    local.get 7
(;@2c2   ;)    local.set 7
(;@2c4   ;)    local.get 6
(;@2c6   ;)    local.get 7
(;@2c8   ;)    local.get 2
(;@2ca   ;)    call $__mon_bind
(;@2cc   ;)    return
             )
(;@2d0   ;)  (func $f_lam_3 (;9;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2d1   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@2d3   ;)    local.get 0
(;@2d5   ;)    i32.load
(;@2d8   ;)    local.set 2
(;@2da   ;)    local.get 0
(;@2dc   ;)    i32.load offset=4
(;@2df   ;)    local.set 3
(;@2e1   ;)    local.get 3
(;@2e3   ;)    local.get 1
(;@2e5   ;)    call $__call_1
(;@2e7   ;)    local.set 4
(;@2e9   ;)    local.get 4
(;@2eb   ;)    i32.load
(;@2ee   ;)    local.set 5
(;@2f0   ;)    block (result i32) ;; label = @1
(;@2f2   ;)      block ;; label = @2
(;@2f4   ;)        block ;; label = @3
(;@2f6   ;)          block ;; label = @4
(;@2f8   ;)            local.get 5
(;@2fa   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2ff   ;)          end
(;@300   ;)          local.get 4
(;@302   ;)          i32.load offset=4
(;@305   ;)          local.set 6
(;@307   ;)          local.get 6
(;@309   ;)          local.get 2
(;@30b   ;)          local.get 1
(;@30d   ;)          call $__call_2
(;@30f   ;)          br 2 (;@1;)
(;@311   ;)        end
(;@312   ;)        local.get 4
(;@314   ;)        i32.load offset=4
(;@317   ;)        local.set 7
(;@319   ;)        local.get 7
(;@31b   ;)        local.set 8
(;@31d   ;)        local.get 8
(;@31f   ;)        i32.load
(;@322   ;)        local.set 9
(;@324   ;)        local.get 8
(;@326   ;)        i32.load offset=4
(;@329   ;)        local.set 10
(;@32b   ;)        i32.const 20
(;@32d   ;)        call $alloc
(;@32f   ;)        local.set 11
(;@331   ;)        local.get 11
(;@333   ;)        i32.const 2
(;@335   ;)        i32.store
(;@338   ;)        local.get 11
(;@33a   ;)        i32.const 2
(;@33c   ;)        i32.store offset=4
(;@33f   ;)        local.get 11
(;@341   ;)        i32.const 8
(;@343   ;)        i32.store offset=8
(;@346   ;)        local.get 11
(;@348   ;)        local.get 2
(;@34a   ;)        i32.store offset=12
(;@34d   ;)        local.get 11
(;@34f   ;)        local.get 8
(;@351   ;)        i32.store offset=16
(;@354   ;)        local.get 11
(;@356   ;)        local.set 11
(;@358   ;)        i32.const 12
(;@35a   ;)        call $alloc
(;@35c   ;)        local.set 12
(;@35e   ;)        local.get 12
(;@360   ;)        local.get 9
(;@362   ;)        i32.store
(;@365   ;)        local.get 12
(;@367   ;)        local.get 10
(;@369   ;)        i32.store offset=4
(;@36c   ;)        local.get 12
(;@36e   ;)        local.get 11
(;@370   ;)        i32.store offset=8
(;@373   ;)        local.get 12
(;@375   ;)        local.set 12
(;@377   ;)        i32.const 8
(;@379   ;)        call $alloc
(;@37b   ;)        local.set 13
(;@37d   ;)        local.get 13
(;@37f   ;)        i32.const 1
(;@381   ;)        i32.store
(;@384   ;)        local.get 13
(;@386   ;)        local.get 12
(;@388   ;)        i32.store offset=4
(;@38b   ;)        local.get 13
(;@38d   ;)        br 1 (;@1;)
(;@38f   ;)      end
(;@390   ;)      unreachable
(;@391   ;)    end
(;@392   ;)    return
             )
(;@395   ;)  (func $f_lam_4 (;10;) (type $fun_2_1) (param i32 i32) (result i32)
(;@396   ;)    (local i32 i32 i32)
(;@398   ;)    local.get 0
(;@39a   ;)    i32.load
(;@39d   ;)    local.set 2
(;@39f   ;)    i32.const 20
(;@3a1   ;)    call $alloc
(;@3a3   ;)    local.set 3
(;@3a5   ;)    local.get 3
(;@3a7   ;)    i32.const 1
(;@3a9   ;)    i32.store
(;@3ac   ;)    local.get 3
(;@3ae   ;)    i32.const 2
(;@3b0   ;)    i32.store offset=4
(;@3b3   ;)    local.get 3
(;@3b5   ;)    i32.const 6
(;@3b7   ;)    i32.store offset=8
(;@3ba   ;)    local.get 3
(;@3bc   ;)    local.get 2
(;@3be   ;)    i32.store offset=12
(;@3c1   ;)    local.get 3
(;@3c3   ;)    local.get 1
(;@3c5   ;)    i32.store offset=16
(;@3c8   ;)    local.get 3
(;@3ca   ;)    local.set 3
(;@3cc   ;)    i32.const 20
(;@3ce   ;)    call $alloc
(;@3d0   ;)    local.set 4
(;@3d2   ;)    local.get 4
(;@3d4   ;)    i32.const 1
(;@3d6   ;)    i32.store
(;@3d9   ;)    local.get 4
(;@3db   ;)    i32.const 2
(;@3dd   ;)    i32.store offset=4
(;@3e0   ;)    local.get 4
(;@3e2   ;)    i32.const 9
(;@3e4   ;)    i32.store offset=8
(;@3e7   ;)    local.get 4
(;@3e9   ;)    local.get 1
(;@3eb   ;)    i32.store offset=12
(;@3ee   ;)    local.get 4
(;@3f0   ;)    local.get 3
(;@3f2   ;)    i32.store offset=16
(;@3f5   ;)    local.get 4
(;@3f7   ;)    return
             )
(;@3fa   ;)  (func $f_lam_5 (;11;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@3fb   ;)    (local i32 i32)
(;@3fd   ;)    i32.const 16
(;@3ff   ;)    call $alloc
(;@401   ;)    local.set 3
(;@403   ;)    local.get 3
(;@405   ;)    i32.const 1
(;@407   ;)    i32.store
(;@40a   ;)    local.get 3
(;@40c   ;)    i32.const 1
(;@40e   ;)    i32.store offset=4
(;@411   ;)    local.get 3
(;@413   ;)    i32.const 10
(;@415   ;)    i32.store offset=8
(;@418   ;)    local.get 3
(;@41a   ;)    local.get 1
(;@41c   ;)    i32.store offset=12
(;@41f   ;)    local.get 3
(;@421   ;)    local.set 3
(;@423   ;)    i32.const 8
(;@425   ;)    call $alloc
(;@427   ;)    local.set 4
(;@429   ;)    local.get 4
(;@42b   ;)    i32.const 0
(;@42d   ;)    i32.store
(;@430   ;)    local.get 4
(;@432   ;)    local.get 3
(;@434   ;)    i32.store offset=4
(;@437   ;)    local.get 4
(;@439   ;)    return
             )
(;@43c   ;)  (func $f_lam_6 (;12;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@43d   ;)    (local i32 i32)
(;@43f   ;)    i32.const 12
(;@441   ;)    call $alloc
(;@443   ;)    local.set 3
(;@445   ;)    local.get 3
(;@447   ;)    i32.const 2
(;@449   ;)    i32.store
(;@44c   ;)    local.get 3
(;@44e   ;)    i32.const 0
(;@450   ;)    i32.store offset=4
(;@453   ;)    local.get 3
(;@455   ;)    i32.const 11
(;@457   ;)    i32.store offset=8
(;@45a   ;)    local.get 3
(;@45c   ;)    local.set 3
(;@45e   ;)    i32.const 8
(;@460   ;)    call $alloc
(;@462   ;)    local.set 4
(;@464   ;)    local.get 4
(;@466   ;)    i32.const 0
(;@468   ;)    i32.store
(;@46b   ;)    local.get 4
(;@46d   ;)    local.get 3
(;@46f   ;)    i32.store offset=4
(;@472   ;)    local.get 4
(;@474   ;)    return
             )
(;@477   ;)  (func $f_lam_7 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@478   ;)    (local i32 i32 i32)
(;@47a   ;)    local.get 0
(;@47c   ;)    i32.load
(;@47f   ;)    local.set 2
(;@481   ;)    i32.const 0
(;@483   ;)    call $alloc
(;@485   ;)    local.set 3
(;@487   ;)    local.get 3
(;@489   ;)    local.set 3
(;@48b   ;)    local.get 2
(;@48d   ;)    local.get 3
(;@48f   ;)    local.get 1
(;@491   ;)    call $__call_2
(;@493   ;)    return
             )
(;@496   ;)  (func $f_lam_8 (;14;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@497   ;)    (local i32 i32)
(;@499   ;)    local.get 0
(;@49b   ;)    i32.load
(;@49e   ;)    local.set 3
(;@4a0   ;)    local.get 1
(;@4a2   ;)    local.get 3
(;@4a4   ;)    local.get 2
(;@4a6   ;)    call $__call_2
(;@4a8   ;)    return
             )
(;@4ab   ;)  (func $f_lam_9 (;15;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@4ac   ;)    (local i32 i32 i32 i32 i32 i32)
(;@4ae   ;)    local.get 0
(;@4b0   ;)    i32.load
(;@4b3   ;)    local.set 3
(;@4b5   ;)    local.get 0
(;@4b7   ;)    i32.load offset=4
(;@4ba   ;)    local.set 4
(;@4bc   ;)    local.get 4
(;@4be   ;)    i32.load offset=8
(;@4c1   ;)    local.set 5
(;@4c3   ;)    local.get 5
(;@4c5   ;)    local.get 1
(;@4c7   ;)    call $__call_1
(;@4c9   ;)    local.set 6
(;@4cb   ;)    i32.const 16
(;@4cd   ;)    call $alloc
(;@4cf   ;)    local.set 7
(;@4d1   ;)    local.get 7
(;@4d3   ;)    i32.const 2
(;@4d5   ;)    i32.store
(;@4d8   ;)    local.get 7
(;@4da   ;)    i32.const 1
(;@4dc   ;)    i32.store offset=4
(;@4df   ;)    local.get 7
(;@4e1   ;)    i32.const 14
(;@4e3   ;)    i32.store offset=8
(;@4e6   ;)    local.get 7
(;@4e8   ;)    local.get 3
(;@4ea   ;)    i32.store offset=12
(;@4ed   ;)    local.get 7
(;@4ef   ;)    local.set 7
(;@4f1   ;)    local.get 6
(;@4f3   ;)    local.get 7
(;@4f5   ;)    local.get 2
(;@4f7   ;)    call $__mon_bind
(;@4f9   ;)    return
             )
(;@4fd   ;)  (func $f_lam_10 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@4fe   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@500   ;)    local.get 0
(;@502   ;)    i32.load
(;@505   ;)    local.set 2
(;@507   ;)    local.get 0
(;@509   ;)    i32.load offset=4
(;@50c   ;)    local.set 3
(;@50e   ;)    local.get 3
(;@510   ;)    local.get 1
(;@512   ;)    call $__call_1
(;@514   ;)    local.set 4
(;@516   ;)    local.get 4
(;@518   ;)    i32.load
(;@51b   ;)    local.set 5
(;@51d   ;)    block (result i32) ;; label = @1
(;@51f   ;)      block ;; label = @2
(;@521   ;)        block ;; label = @3
(;@523   ;)          block ;; label = @4
(;@525   ;)            local.get 5
(;@527   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@52c   ;)          end
(;@52d   ;)          local.get 4
(;@52f   ;)          i32.load offset=4
(;@532   ;)          local.set 6
(;@534   ;)          local.get 6
(;@536   ;)          local.get 2
(;@538   ;)          local.get 1
(;@53a   ;)          call $__call_2
(;@53c   ;)          br 2 (;@1;)
(;@53e   ;)        end
(;@53f   ;)        local.get 4
(;@541   ;)        i32.load offset=4
(;@544   ;)        local.set 7
(;@546   ;)        local.get 7
(;@548   ;)        local.set 8
(;@54a   ;)        local.get 8
(;@54c   ;)        i32.load
(;@54f   ;)        local.set 9
(;@551   ;)        local.get 8
(;@553   ;)        i32.load offset=4
(;@556   ;)        local.set 10
(;@558   ;)        i32.const 20
(;@55a   ;)        call $alloc
(;@55c   ;)        local.set 11
(;@55e   ;)        local.get 11
(;@560   ;)        i32.const 2
(;@562   ;)        i32.store
(;@565   ;)        local.get 11
(;@567   ;)        i32.const 2
(;@569   ;)        i32.store offset=4
(;@56c   ;)        local.get 11
(;@56e   ;)        i32.const 15
(;@570   ;)        i32.store offset=8
(;@573   ;)        local.get 11
(;@575   ;)        local.get 2
(;@577   ;)        i32.store offset=12
(;@57a   ;)        local.get 11
(;@57c   ;)        local.get 8
(;@57e   ;)        i32.store offset=16
(;@581   ;)        local.get 11
(;@583   ;)        local.set 11
(;@585   ;)        i32.const 12
(;@587   ;)        call $alloc
(;@589   ;)        local.set 12
(;@58b   ;)        local.get 12
(;@58d   ;)        local.get 9
(;@58f   ;)        i32.store
(;@592   ;)        local.get 12
(;@594   ;)        local.get 10
(;@596   ;)        i32.store offset=4
(;@599   ;)        local.get 12
(;@59b   ;)        local.get 11
(;@59d   ;)        i32.store offset=8
(;@5a0   ;)        local.get 12
(;@5a2   ;)        local.set 12
(;@5a4   ;)        i32.const 8
(;@5a6   ;)        call $alloc
(;@5a8   ;)        local.set 13
(;@5aa   ;)        local.get 13
(;@5ac   ;)        i32.const 1
(;@5ae   ;)        i32.store
(;@5b1   ;)        local.get 13
(;@5b3   ;)        local.get 12
(;@5b5   ;)        i32.store offset=4
(;@5b8   ;)        local.get 13
(;@5ba   ;)        br 1 (;@1;)
(;@5bc   ;)      end
(;@5bd   ;)      unreachable
(;@5be   ;)    end
(;@5bf   ;)    return
             )
(;@5c2   ;)  (func $f_lam_11 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
(;@5c3   ;)    (local i32 i32 i32 i32)
(;@5c5   ;)    local.get 0
(;@5c7   ;)    i32.load
(;@5ca   ;)    local.set 2
(;@5cc   ;)    local.get 0
(;@5ce   ;)    i32.load offset=4
(;@5d1   ;)    local.set 3
(;@5d3   ;)    i32.const 16
(;@5d5   ;)    call $alloc
(;@5d7   ;)    local.set 4
(;@5d9   ;)    local.get 4
(;@5db   ;)    i32.const 1
(;@5dd   ;)    i32.store
(;@5e0   ;)    local.get 4
(;@5e2   ;)    i32.const 1
(;@5e4   ;)    i32.store offset=4
(;@5e7   ;)    local.get 4
(;@5e9   ;)    i32.const 13
(;@5eb   ;)    i32.store offset=8
(;@5ee   ;)    local.get 4
(;@5f0   ;)    local.get 3
(;@5f2   ;)    i32.store offset=12
(;@5f5   ;)    local.get 4
(;@5f7   ;)    local.set 4
(;@5f9   ;)    i32.const 20
(;@5fb   ;)    call $alloc
(;@5fd   ;)    local.set 5
(;@5ff   ;)    local.get 5
(;@601   ;)    i32.const 1
(;@603   ;)    i32.store
(;@606   ;)    local.get 5
(;@608   ;)    i32.const 2
(;@60a   ;)    i32.store offset=4
(;@60d   ;)    local.get 5
(;@60f   ;)    i32.const 16
(;@611   ;)    i32.store offset=8
(;@614   ;)    local.get 5
(;@616   ;)    local.get 2
(;@618   ;)    i32.store offset=12
(;@61b   ;)    local.get 5
(;@61d   ;)    local.get 4
(;@61f   ;)    i32.store offset=16
(;@622   ;)    local.get 5
(;@624   ;)    return
             )
(;@627   ;)  (func $f_lam_12 (;18;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@628   ;)    (local i32 i32 i32)
(;@62a   ;)    local.get 0
(;@62c   ;)    i32.load
(;@62f   ;)    local.set 3
(;@631   ;)    i32.const 20
(;@633   ;)    call $alloc
(;@635   ;)    local.set 4
(;@637   ;)    local.get 4
(;@639   ;)    i32.const 1
(;@63b   ;)    i32.store
(;@63e   ;)    local.get 4
(;@640   ;)    i32.const 2
(;@642   ;)    i32.store offset=4
(;@645   ;)    local.get 4
(;@647   ;)    i32.const 17
(;@649   ;)    i32.store offset=8
(;@64c   ;)    local.get 4
(;@64e   ;)    local.get 3
(;@650   ;)    i32.store offset=12
(;@653   ;)    local.get 4
(;@655   ;)    local.get 1
(;@657   ;)    i32.store offset=16
(;@65a   ;)    local.get 4
(;@65c   ;)    local.set 4
(;@65e   ;)    i32.const 8
(;@660   ;)    call $alloc
(;@662   ;)    local.set 5
(;@664   ;)    local.get 5
(;@666   ;)    i32.const 0
(;@668   ;)    i32.store
(;@66b   ;)    local.get 5
(;@66d   ;)    local.get 4
(;@66f   ;)    i32.store offset=4
(;@672   ;)    local.get 5
(;@674   ;)    return
             )
(;@677   ;)  (func $f_lam_13 (;19;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@678   ;)    (local i32 i32)
(;@67a   ;)    i32.const 16
(;@67c   ;)    call $alloc
(;@67e   ;)    local.set 3
(;@680   ;)    local.get 3
(;@682   ;)    i32.const 2
(;@684   ;)    i32.store
(;@687   ;)    local.get 3
(;@689   ;)    i32.const 1
(;@68b   ;)    i32.store offset=4
(;@68e   ;)    local.get 3
(;@690   ;)    i32.const 18
(;@692   ;)    i32.store offset=8
(;@695   ;)    local.get 3
(;@697   ;)    local.get 1
(;@699   ;)    i32.store offset=12
(;@69c   ;)    local.get 3
(;@69e   ;)    local.set 3
(;@6a0   ;)    i32.const 8
(;@6a2   ;)    call $alloc
(;@6a4   ;)    local.set 4
(;@6a6   ;)    local.get 4
(;@6a8   ;)    i32.const 0
(;@6aa   ;)    i32.store
(;@6ad   ;)    local.get 4
(;@6af   ;)    local.get 3
(;@6b1   ;)    i32.store offset=4
(;@6b4   ;)    local.get 4
(;@6b6   ;)    return
             )
(;@6b9   ;)  (func $f_lam_14 (;20;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@6ba   ;)    (local i32 i32)
(;@6bc   ;)    local.get 0
(;@6be   ;)    i32.load
(;@6c1   ;)    local.set 3
(;@6c3   ;)    local.get 1
(;@6c5   ;)    local.get 3
(;@6c7   ;)    local.get 2
(;@6c9   ;)    call $__call_2
(;@6cb   ;)    return
             )
(;@6ce   ;)  (func $f_lam_15 (;21;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@6cf   ;)    (local i32 i32 i32 i32 i32 i32)
(;@6d1   ;)    local.get 0
(;@6d3   ;)    i32.load
(;@6d6   ;)    local.set 3
(;@6d8   ;)    local.get 0
(;@6da   ;)    i32.load offset=4
(;@6dd   ;)    local.set 4
(;@6df   ;)    local.get 4
(;@6e1   ;)    i32.load offset=8
(;@6e4   ;)    local.set 5
(;@6e6   ;)    local.get 5
(;@6e8   ;)    local.get 1
(;@6ea   ;)    call $__call_1
(;@6ec   ;)    local.set 6
(;@6ee   ;)    i32.const 16
(;@6f0   ;)    call $alloc
(;@6f2   ;)    local.set 7
(;@6f4   ;)    local.get 7
(;@6f6   ;)    i32.const 2
(;@6f8   ;)    i32.store
(;@6fb   ;)    local.get 7
(;@6fd   ;)    i32.const 1
(;@6ff   ;)    i32.store offset=4
(;@702   ;)    local.get 7
(;@704   ;)    i32.const 20
(;@706   ;)    i32.store offset=8
(;@709   ;)    local.get 7
(;@70b   ;)    local.get 3
(;@70d   ;)    i32.store offset=12
(;@710   ;)    local.get 7
(;@712   ;)    local.set 7
(;@714   ;)    local.get 6
(;@716   ;)    local.get 7
(;@718   ;)    local.get 2
(;@71a   ;)    call $__mon_bind
(;@71c   ;)    return
             )
(;@720   ;)  (func $f_lam_16 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
(;@721   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@723   ;)    local.get 0
(;@725   ;)    i32.load
(;@728   ;)    local.set 2
(;@72a   ;)    local.get 0
(;@72c   ;)    i32.load offset=4
(;@72f   ;)    local.set 3
(;@731   ;)    local.get 3
(;@733   ;)    local.get 1
(;@735   ;)    call $__call_1
(;@737   ;)    local.set 4
(;@739   ;)    local.get 4
(;@73b   ;)    i32.load
(;@73e   ;)    local.set 5
(;@740   ;)    block (result i32) ;; label = @1
(;@742   ;)      block ;; label = @2
(;@744   ;)        block ;; label = @3
(;@746   ;)          block ;; label = @4
(;@748   ;)            local.get 5
(;@74a   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@74f   ;)          end
(;@750   ;)          local.get 4
(;@752   ;)          i32.load offset=4
(;@755   ;)          local.set 6
(;@757   ;)          local.get 6
(;@759   ;)          local.get 2
(;@75b   ;)          local.get 1
(;@75d   ;)          call $__call_2
(;@75f   ;)          br 2 (;@1;)
(;@761   ;)        end
(;@762   ;)        local.get 4
(;@764   ;)        i32.load offset=4
(;@767   ;)        local.set 7
(;@769   ;)        local.get 7
(;@76b   ;)        local.set 8
(;@76d   ;)        local.get 8
(;@76f   ;)        i32.load
(;@772   ;)        local.set 9
(;@774   ;)        local.get 8
(;@776   ;)        i32.load offset=4
(;@779   ;)        local.set 10
(;@77b   ;)        i32.const 20
(;@77d   ;)        call $alloc
(;@77f   ;)        local.set 11
(;@781   ;)        local.get 11
(;@783   ;)        i32.const 2
(;@785   ;)        i32.store
(;@788   ;)        local.get 11
(;@78a   ;)        i32.const 2
(;@78c   ;)        i32.store offset=4
(;@78f   ;)        local.get 11
(;@791   ;)        i32.const 21
(;@793   ;)        i32.store offset=8
(;@796   ;)        local.get 11
(;@798   ;)        local.get 2
(;@79a   ;)        i32.store offset=12
(;@79d   ;)        local.get 11
(;@79f   ;)        local.get 8
(;@7a1   ;)        i32.store offset=16
(;@7a4   ;)        local.get 11
(;@7a6   ;)        local.set 11
(;@7a8   ;)        i32.const 12
(;@7aa   ;)        call $alloc
(;@7ac   ;)        local.set 12
(;@7ae   ;)        local.get 12
(;@7b0   ;)        local.get 9
(;@7b2   ;)        i32.store
(;@7b5   ;)        local.get 12
(;@7b7   ;)        local.get 10
(;@7b9   ;)        i32.store offset=4
(;@7bc   ;)        local.get 12
(;@7be   ;)        local.get 11
(;@7c0   ;)        i32.store offset=8
(;@7c3   ;)        local.get 12
(;@7c5   ;)        local.set 12
(;@7c7   ;)        i32.const 8
(;@7c9   ;)        call $alloc
(;@7cb   ;)        local.set 13
(;@7cd   ;)        local.get 13
(;@7cf   ;)        i32.const 1
(;@7d1   ;)        i32.store
(;@7d4   ;)        local.get 13
(;@7d6   ;)        local.get 12
(;@7d8   ;)        i32.store offset=4
(;@7db   ;)        local.get 13
(;@7dd   ;)        br 1 (;@1;)
(;@7df   ;)      end
(;@7e0   ;)      unreachable
(;@7e1   ;)    end
(;@7e2   ;)    return
             )
(;@7e5   ;)  (func $f_lam_17 (;23;) (type $fun_2_1) (param i32 i32) (result i32)
(;@7e6   ;)    (local i32 i32 i32 i32 i32 i32)
(;@7e8   ;)    local.get 0
(;@7ea   ;)    i32.load
(;@7ed   ;)    local.set 2
(;@7ef   ;)    i32.const 0
(;@7f1   ;)    call $alloc
(;@7f3   ;)    local.set 3
(;@7f5   ;)    local.get 3
(;@7f7   ;)    local.set 3
(;@7f9   ;)    local.get 2
(;@7fb   ;)    i32.load offset=4
(;@7fe   ;)    local.set 4
(;@800   ;)    local.get 4
(;@802   ;)    i32.load
(;@805   ;)    local.set 5
(;@807   ;)    local.get 5
(;@809   ;)    local.get 3
(;@80b   ;)    call $__call_1
(;@80d   ;)    local.set 6
(;@80f   ;)    i32.const 20
(;@811   ;)    call $alloc
(;@813   ;)    local.set 7
(;@815   ;)    local.get 7
(;@817   ;)    i32.const 1
(;@819   ;)    i32.store
(;@81c   ;)    local.get 7
(;@81e   ;)    i32.const 2
(;@820   ;)    i32.store offset=4
(;@823   ;)    local.get 7
(;@825   ;)    i32.const 22
(;@827   ;)    i32.store offset=8
(;@82a   ;)    local.get 7
(;@82c   ;)    local.get 1
(;@82e   ;)    i32.store offset=12
(;@831   ;)    local.get 7
(;@833   ;)    local.get 6
(;@835   ;)    i32.store offset=16
(;@838   ;)    local.get 7
(;@83a   ;)    return
             )
(;@83d   ;)  (func $f_lam_18 (;24;) (type $fun_2_1) (param i32 i32) (result i32)
(;@83e   ;)    (local i32 i32 i32)
(;@840   ;)    local.get 0
(;@842   ;)    i32.load
(;@845   ;)    local.set 2
(;@847   ;)    local.get 0
(;@849   ;)    i32.load offset=4
(;@84c   ;)    local.set 3
(;@84e   ;)    local.get 2
(;@850   ;)    local.get 3
(;@852   ;)    local.get 1
(;@854   ;)    call $__call_2
(;@856   ;)    return
             )
(;@859   ;)  (func $f_lam_19 (;25;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@85a   ;)    (local i32 i32)
(;@85c   ;)    local.get 0
(;@85e   ;)    i32.load
(;@861   ;)    local.set 3
(;@863   ;)    local.get 1
(;@865   ;)    local.get 3
(;@867   ;)    local.get 2
(;@869   ;)    call $__call_2
(;@86b   ;)    return
             )
(;@86e   ;)  (func $f_lam_20 (;26;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@86f   ;)    (local i32 i32 i32 i32 i32 i32)
(;@871   ;)    local.get 0
(;@873   ;)    i32.load
(;@876   ;)    local.set 3
(;@878   ;)    local.get 0
(;@87a   ;)    i32.load offset=4
(;@87d   ;)    local.set 4
(;@87f   ;)    local.get 4
(;@881   ;)    i32.load offset=8
(;@884   ;)    local.set 5
(;@886   ;)    local.get 5
(;@888   ;)    local.get 1
(;@88a   ;)    call $__call_1
(;@88c   ;)    local.set 6
(;@88e   ;)    i32.const 16
(;@890   ;)    call $alloc
(;@892   ;)    local.set 7
(;@894   ;)    local.get 7
(;@896   ;)    i32.const 2
(;@898   ;)    i32.store
(;@89b   ;)    local.get 7
(;@89d   ;)    i32.const 1
(;@89f   ;)    i32.store offset=4
(;@8a2   ;)    local.get 7
(;@8a4   ;)    i32.const 25
(;@8a6   ;)    i32.store offset=8
(;@8a9   ;)    local.get 7
(;@8ab   ;)    local.get 3
(;@8ad   ;)    i32.store offset=12
(;@8b0   ;)    local.get 7
(;@8b2   ;)    local.set 7
(;@8b4   ;)    local.get 6
(;@8b6   ;)    local.get 7
(;@8b8   ;)    local.get 2
(;@8ba   ;)    call $__mon_bind
(;@8bc   ;)    return
             )
(;@8c0   ;)  (func $f_lam_21 (;27;) (type $fun_2_1) (param i32 i32) (result i32)
(;@8c1   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@8c3   ;)    local.get 0
(;@8c5   ;)    i32.load
(;@8c8   ;)    local.set 2
(;@8ca   ;)    local.get 0
(;@8cc   ;)    i32.load offset=4
(;@8cf   ;)    local.set 3
(;@8d1   ;)    local.get 3
(;@8d3   ;)    local.get 1
(;@8d5   ;)    call $__call_1
(;@8d7   ;)    local.set 4
(;@8d9   ;)    local.get 4
(;@8db   ;)    i32.load
(;@8de   ;)    local.set 5
(;@8e0   ;)    block (result i32) ;; label = @1
(;@8e2   ;)      block ;; label = @2
(;@8e4   ;)        block ;; label = @3
(;@8e6   ;)          block ;; label = @4
(;@8e8   ;)            local.get 5
(;@8ea   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@8ef   ;)          end
(;@8f0   ;)          local.get 4
(;@8f2   ;)          i32.load offset=4
(;@8f5   ;)          local.set 6
(;@8f7   ;)          local.get 6
(;@8f9   ;)          local.get 2
(;@8fb   ;)          local.get 1
(;@8fd   ;)          call $__call_2
(;@8ff   ;)          br 2 (;@1;)
(;@901   ;)        end
(;@902   ;)        local.get 4
(;@904   ;)        i32.load offset=4
(;@907   ;)        local.set 7
(;@909   ;)        local.get 7
(;@90b   ;)        local.set 8
(;@90d   ;)        local.get 8
(;@90f   ;)        i32.load
(;@912   ;)        local.set 9
(;@914   ;)        local.get 8
(;@916   ;)        i32.load offset=4
(;@919   ;)        local.set 10
(;@91b   ;)        i32.const 20
(;@91d   ;)        call $alloc
(;@91f   ;)        local.set 11
(;@921   ;)        local.get 11
(;@923   ;)        i32.const 2
(;@925   ;)        i32.store
(;@928   ;)        local.get 11
(;@92a   ;)        i32.const 2
(;@92c   ;)        i32.store offset=4
(;@92f   ;)        local.get 11
(;@931   ;)        i32.const 26
(;@933   ;)        i32.store offset=8
(;@936   ;)        local.get 11
(;@938   ;)        local.get 2
(;@93a   ;)        i32.store offset=12
(;@93d   ;)        local.get 11
(;@93f   ;)        local.get 8
(;@941   ;)        i32.store offset=16
(;@944   ;)        local.get 11
(;@946   ;)        local.set 11
(;@948   ;)        i32.const 12
(;@94a   ;)        call $alloc
(;@94c   ;)        local.set 12
(;@94e   ;)        local.get 12
(;@950   ;)        local.get 9
(;@952   ;)        i32.store
(;@955   ;)        local.get 12
(;@957   ;)        local.get 10
(;@959   ;)        i32.store offset=4
(;@95c   ;)        local.get 12
(;@95e   ;)        local.get 11
(;@960   ;)        i32.store offset=8
(;@963   ;)        local.get 12
(;@965   ;)        local.set 12
(;@967   ;)        i32.const 8
(;@969   ;)        call $alloc
(;@96b   ;)        local.set 13
(;@96d   ;)        local.get 13
(;@96f   ;)        i32.const 1
(;@971   ;)        i32.store
(;@974   ;)        local.get 13
(;@976   ;)        local.get 12
(;@978   ;)        i32.store offset=4
(;@97b   ;)        local.get 13
(;@97d   ;)        br 1 (;@1;)
(;@97f   ;)      end
(;@980   ;)      unreachable
(;@981   ;)    end
(;@982   ;)    return
             )
(;@985   ;)  (func $f_lam_22 (;28;) (type $fun_2_1) (param i32 i32) (result i32)
(;@986   ;)    (local i32 i32 i32)
(;@988   ;)    local.get 0
(;@98a   ;)    i32.load
(;@98d   ;)    local.set 2
(;@98f   ;)    i32.const 20
(;@991   ;)    call $alloc
(;@993   ;)    local.set 3
(;@995   ;)    local.get 3
(;@997   ;)    i32.const 1
(;@999   ;)    i32.store
(;@99c   ;)    local.get 3
(;@99e   ;)    i32.const 2
(;@9a0   ;)    i32.store offset=4
(;@9a3   ;)    local.get 3
(;@9a5   ;)    i32.const 24
(;@9a7   ;)    i32.store offset=8
(;@9aa   ;)    local.get 3
(;@9ac   ;)    local.get 2
(;@9ae   ;)    i32.store offset=12
(;@9b1   ;)    local.get 3
(;@9b3   ;)    local.get 1
(;@9b5   ;)    i32.store offset=16
(;@9b8   ;)    local.get 3
(;@9ba   ;)    local.set 3
(;@9bc   ;)    i32.const 20
(;@9be   ;)    call $alloc
(;@9c0   ;)    local.set 4
(;@9c2   ;)    local.get 4
(;@9c4   ;)    i32.const 1
(;@9c6   ;)    i32.store
(;@9c9   ;)    local.get 4
(;@9cb   ;)    i32.const 2
(;@9cd   ;)    i32.store offset=4
(;@9d0   ;)    local.get 4
(;@9d2   ;)    i32.const 27
(;@9d4   ;)    i32.store offset=8
(;@9d7   ;)    local.get 4
(;@9d9   ;)    local.get 1
(;@9db   ;)    i32.store offset=12
(;@9de   ;)    local.get 4
(;@9e0   ;)    local.get 3
(;@9e2   ;)    i32.store offset=16
(;@9e5   ;)    local.get 4
(;@9e7   ;)    return
             )
(;@9ea   ;)  (func $f_lam_23 (;29;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@9eb   ;)    (local i32 i32)
(;@9ed   ;)    i32.const 16
(;@9ef   ;)    call $alloc
(;@9f1   ;)    local.set 3
(;@9f3   ;)    local.get 3
(;@9f5   ;)    i32.const 1
(;@9f7   ;)    i32.store
(;@9fa   ;)    local.get 3
(;@9fc   ;)    i32.const 1
(;@9fe   ;)    i32.store offset=4
(;@a01   ;)    local.get 3
(;@a03   ;)    i32.const 28
(;@a05   ;)    i32.store offset=8
(;@a08   ;)    local.get 3
(;@a0a   ;)    local.get 1
(;@a0c   ;)    i32.store offset=12
(;@a0f   ;)    local.get 3
(;@a11   ;)    local.set 3
(;@a13   ;)    i32.const 8
(;@a15   ;)    call $alloc
(;@a17   ;)    local.set 4
(;@a19   ;)    local.get 4
(;@a1b   ;)    i32.const 0
(;@a1d   ;)    i32.store
(;@a20   ;)    local.get 4
(;@a22   ;)    local.get 3
(;@a24   ;)    i32.store offset=4
(;@a27   ;)    local.get 4
(;@a29   ;)    return
             )
(;@a2c   ;)  (func $f_lam_24 (;30;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@a2d   ;)    (local i32 i32)
(;@a2f   ;)    i32.const 12
(;@a31   ;)    call $alloc
(;@a33   ;)    local.set 3
(;@a35   ;)    local.get 3
(;@a37   ;)    i32.const 2
(;@a39   ;)    i32.store
(;@a3c   ;)    local.get 3
(;@a3e   ;)    i32.const 0
(;@a40   ;)    i32.store offset=4
(;@a43   ;)    local.get 3
(;@a45   ;)    i32.const 29
(;@a47   ;)    i32.store offset=8
(;@a4a   ;)    local.get 3
(;@a4c   ;)    local.set 3
(;@a4e   ;)    i32.const 8
(;@a50   ;)    call $alloc
(;@a52   ;)    local.set 4
(;@a54   ;)    local.get 4
(;@a56   ;)    i32.const 0
(;@a58   ;)    i32.store
(;@a5b   ;)    local.get 4
(;@a5d   ;)    local.get 3
(;@a5f   ;)    i32.store offset=4
(;@a62   ;)    local.get 4
(;@a64   ;)    return
             )
(;@a67   ;)  (func $f_lam_25 (;31;) (type $fun_2_1) (param i32 i32) (result i32)
(;@a68   ;)    (local i32 i32 i32)
(;@a6a   ;)    local.get 0
(;@a6c   ;)    i32.load
(;@a6f   ;)    local.set 2
(;@a71   ;)    i32.const 0
(;@a73   ;)    call $alloc
(;@a75   ;)    local.set 3
(;@a77   ;)    local.get 3
(;@a79   ;)    local.set 3
(;@a7b   ;)    local.get 2
(;@a7d   ;)    local.get 3
(;@a7f   ;)    local.get 1
(;@a81   ;)    call $__call_2
(;@a83   ;)    return
             )
(;@a86   ;)  (func $f_lam_26 (;32;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@a87   ;)    (local i32 i32)
(;@a89   ;)    local.get 0
(;@a8b   ;)    i32.load
(;@a8e   ;)    local.set 3
(;@a90   ;)    local.get 1
(;@a92   ;)    local.get 3
(;@a94   ;)    local.get 2
(;@a96   ;)    call $__call_2
(;@a98   ;)    return
             )
(;@a9b   ;)  (func $f_lam_27 (;33;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@a9c   ;)    (local i32 i32 i32 i32 i32 i32)
(;@a9e   ;)    local.get 0
(;@aa0   ;)    i32.load
(;@aa3   ;)    local.set 3
(;@aa5   ;)    local.get 0
(;@aa7   ;)    i32.load offset=4
(;@aaa   ;)    local.set 4
(;@aac   ;)    local.get 4
(;@aae   ;)    i32.load offset=8
(;@ab1   ;)    local.set 5
(;@ab3   ;)    local.get 5
(;@ab5   ;)    local.get 1
(;@ab7   ;)    call $__call_1
(;@ab9   ;)    local.set 6
(;@abb   ;)    i32.const 16
(;@abd   ;)    call $alloc
(;@abf   ;)    local.set 7
(;@ac1   ;)    local.get 7
(;@ac3   ;)    i32.const 2
(;@ac5   ;)    i32.store
(;@ac8   ;)    local.get 7
(;@aca   ;)    i32.const 1
(;@acc   ;)    i32.store offset=4
(;@acf   ;)    local.get 7
(;@ad1   ;)    i32.const 32
(;@ad3   ;)    i32.store offset=8
(;@ad6   ;)    local.get 7
(;@ad8   ;)    local.get 3
(;@ada   ;)    i32.store offset=12
(;@add   ;)    local.get 7
(;@adf   ;)    local.set 7
(;@ae1   ;)    local.get 6
(;@ae3   ;)    local.get 7
(;@ae5   ;)    local.get 2
(;@ae7   ;)    call $__mon_bind
(;@ae9   ;)    return
             )
(;@aed   ;)  (func $f_lam_28 (;34;) (type $fun_2_1) (param i32 i32) (result i32)
(;@aee   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@af0   ;)    local.get 0
(;@af2   ;)    i32.load
(;@af5   ;)    local.set 2
(;@af7   ;)    local.get 0
(;@af9   ;)    i32.load offset=4
(;@afc   ;)    local.set 3
(;@afe   ;)    local.get 3
(;@b00   ;)    local.get 1
(;@b02   ;)    call $__call_1
(;@b04   ;)    local.set 4
(;@b06   ;)    local.get 4
(;@b08   ;)    i32.load
(;@b0b   ;)    local.set 5
(;@b0d   ;)    block (result i32) ;; label = @1
(;@b0f   ;)      block ;; label = @2
(;@b11   ;)        block ;; label = @3
(;@b13   ;)          block ;; label = @4
(;@b15   ;)            local.get 5
(;@b17   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@b1c   ;)          end
(;@b1d   ;)          local.get 4
(;@b1f   ;)          i32.load offset=4
(;@b22   ;)          local.set 6
(;@b24   ;)          local.get 6
(;@b26   ;)          local.get 2
(;@b28   ;)          local.get 1
(;@b2a   ;)          call $__call_2
(;@b2c   ;)          br 2 (;@1;)
(;@b2e   ;)        end
(;@b2f   ;)        local.get 4
(;@b31   ;)        i32.load offset=4
(;@b34   ;)        local.set 7
(;@b36   ;)        local.get 7
(;@b38   ;)        local.set 8
(;@b3a   ;)        local.get 8
(;@b3c   ;)        i32.load
(;@b3f   ;)        local.set 9
(;@b41   ;)        local.get 8
(;@b43   ;)        i32.load offset=4
(;@b46   ;)        local.set 10
(;@b48   ;)        i32.const 20
(;@b4a   ;)        call $alloc
(;@b4c   ;)        local.set 11
(;@b4e   ;)        local.get 11
(;@b50   ;)        i32.const 2
(;@b52   ;)        i32.store
(;@b55   ;)        local.get 11
(;@b57   ;)        i32.const 2
(;@b59   ;)        i32.store offset=4
(;@b5c   ;)        local.get 11
(;@b5e   ;)        i32.const 33
(;@b60   ;)        i32.store offset=8
(;@b63   ;)        local.get 11
(;@b65   ;)        local.get 2
(;@b67   ;)        i32.store offset=12
(;@b6a   ;)        local.get 11
(;@b6c   ;)        local.get 8
(;@b6e   ;)        i32.store offset=16
(;@b71   ;)        local.get 11
(;@b73   ;)        local.set 11
(;@b75   ;)        i32.const 12
(;@b77   ;)        call $alloc
(;@b79   ;)        local.set 12
(;@b7b   ;)        local.get 12
(;@b7d   ;)        local.get 9
(;@b7f   ;)        i32.store
(;@b82   ;)        local.get 12
(;@b84   ;)        local.get 10
(;@b86   ;)        i32.store offset=4
(;@b89   ;)        local.get 12
(;@b8b   ;)        local.get 11
(;@b8d   ;)        i32.store offset=8
(;@b90   ;)        local.get 12
(;@b92   ;)        local.set 12
(;@b94   ;)        i32.const 8
(;@b96   ;)        call $alloc
(;@b98   ;)        local.set 13
(;@b9a   ;)        local.get 13
(;@b9c   ;)        i32.const 1
(;@b9e   ;)        i32.store
(;@ba1   ;)        local.get 13
(;@ba3   ;)        local.get 12
(;@ba5   ;)        i32.store offset=4
(;@ba8   ;)        local.get 13
(;@baa   ;)        br 1 (;@1;)
(;@bac   ;)      end
(;@bad   ;)      unreachable
(;@bae   ;)    end
(;@baf   ;)    return
             )
(;@bb2   ;)  (func $f_lam_29 (;35;) (type $fun_2_1) (param i32 i32) (result i32)
(;@bb3   ;)    (local i32 i32 i32 i32)
(;@bb5   ;)    local.get 0
(;@bb7   ;)    i32.load
(;@bba   ;)    local.set 2
(;@bbc   ;)    local.get 0
(;@bbe   ;)    i32.load offset=4
(;@bc1   ;)    local.set 3
(;@bc3   ;)    i32.const 16
(;@bc5   ;)    call $alloc
(;@bc7   ;)    local.set 4
(;@bc9   ;)    local.get 4
(;@bcb   ;)    i32.const 1
(;@bcd   ;)    i32.store
(;@bd0   ;)    local.get 4
(;@bd2   ;)    i32.const 1
(;@bd4   ;)    i32.store offset=4
(;@bd7   ;)    local.get 4
(;@bd9   ;)    i32.const 31
(;@bdb   ;)    i32.store offset=8
(;@bde   ;)    local.get 4
(;@be0   ;)    local.get 3
(;@be2   ;)    i32.store offset=12
(;@be5   ;)    local.get 4
(;@be7   ;)    local.set 4
(;@be9   ;)    i32.const 20
(;@beb   ;)    call $alloc
(;@bed   ;)    local.set 5
(;@bef   ;)    local.get 5
(;@bf1   ;)    i32.const 1
(;@bf3   ;)    i32.store
(;@bf6   ;)    local.get 5
(;@bf8   ;)    i32.const 2
(;@bfa   ;)    i32.store offset=4
(;@bfd   ;)    local.get 5
(;@bff   ;)    i32.const 34
(;@c01   ;)    i32.store offset=8
(;@c04   ;)    local.get 5
(;@c06   ;)    local.get 2
(;@c08   ;)    i32.store offset=12
(;@c0b   ;)    local.get 5
(;@c0d   ;)    local.get 4
(;@c0f   ;)    i32.store offset=16
(;@c12   ;)    local.get 5
(;@c14   ;)    return
             )
(;@c17   ;)  (func $f_lam_30 (;36;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@c18   ;)    (local i32 i32 i32)
(;@c1a   ;)    local.get 0
(;@c1c   ;)    i32.load
(;@c1f   ;)    local.set 3
(;@c21   ;)    i32.const 20
(;@c23   ;)    call $alloc
(;@c25   ;)    local.set 4
(;@c27   ;)    local.get 4
(;@c29   ;)    i32.const 1
(;@c2b   ;)    i32.store
(;@c2e   ;)    local.get 4
(;@c30   ;)    i32.const 2
(;@c32   ;)    i32.store offset=4
(;@c35   ;)    local.get 4
(;@c37   ;)    i32.const 35
(;@c39   ;)    i32.store offset=8
(;@c3c   ;)    local.get 4
(;@c3e   ;)    local.get 3
(;@c40   ;)    i32.store offset=12
(;@c43   ;)    local.get 4
(;@c45   ;)    local.get 1
(;@c47   ;)    i32.store offset=16
(;@c4a   ;)    local.get 4
(;@c4c   ;)    local.set 4
(;@c4e   ;)    i32.const 8
(;@c50   ;)    call $alloc
(;@c52   ;)    local.set 5
(;@c54   ;)    local.get 5
(;@c56   ;)    i32.const 0
(;@c58   ;)    i32.store
(;@c5b   ;)    local.get 5
(;@c5d   ;)    local.get 4
(;@c5f   ;)    i32.store offset=4
(;@c62   ;)    local.get 5
(;@c64   ;)    return
             )
(;@c67   ;)  (func $f_lam_31 (;37;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@c68   ;)    (local i32 i32)
(;@c6a   ;)    i32.const 16
(;@c6c   ;)    call $alloc
(;@c6e   ;)    local.set 3
(;@c70   ;)    local.get 3
(;@c72   ;)    i32.const 2
(;@c74   ;)    i32.store
(;@c77   ;)    local.get 3
(;@c79   ;)    i32.const 1
(;@c7b   ;)    i32.store offset=4
(;@c7e   ;)    local.get 3
(;@c80   ;)    i32.const 36
(;@c82   ;)    i32.store offset=8
(;@c85   ;)    local.get 3
(;@c87   ;)    local.get 1
(;@c89   ;)    i32.store offset=12
(;@c8c   ;)    local.get 3
(;@c8e   ;)    local.set 3
(;@c90   ;)    i32.const 8
(;@c92   ;)    call $alloc
(;@c94   ;)    local.set 4
(;@c96   ;)    local.get 4
(;@c98   ;)    i32.const 0
(;@c9a   ;)    i32.store
(;@c9d   ;)    local.get 4
(;@c9f   ;)    local.get 3
(;@ca1   ;)    i32.store offset=4
(;@ca4   ;)    local.get 4
(;@ca6   ;)    return
             )
(;@caa   ;)  (func $f_lam_32 (;38;) (type $fun_2_1) (param i32 i32) (result i32)
(;@cab   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32)
(;@cad   ;)    local.get 0
(;@caf   ;)    i32.load
(;@cb2   ;)    local.set 2
(;@cb4   ;)    local.get 0
(;@cb6   ;)    i32.load offset=4
(;@cb9   ;)    local.set 3
(;@cbb   ;)    i32.const 12
(;@cbd   ;)    call $alloc
(;@cbf   ;)    local.set 4
(;@cc1   ;)    local.get 4
(;@cc3   ;)    i32.const 2
(;@cc5   ;)    i32.store
(;@cc8   ;)    local.get 4
(;@cca   ;)    i32.const 0
(;@ccc   ;)    i32.store offset=4
(;@ccf   ;)    local.get 4
(;@cd1   ;)    i32.const 30
(;@cd3   ;)    i32.store offset=8
(;@cd6   ;)    local.get 4
(;@cd8   ;)    local.set 4
(;@cda   ;)    i32.const 12
(;@cdc   ;)    call $alloc
(;@cde   ;)    local.set 5
(;@ce0   ;)    local.get 5
(;@ce2   ;)    i32.const 2
(;@ce4   ;)    i32.store
(;@ce7   ;)    local.get 5
(;@ce9   ;)    i32.const 0
(;@ceb   ;)    i32.store offset=4
(;@cee   ;)    local.get 5
(;@cf0   ;)    i32.const 37
(;@cf2   ;)    i32.store offset=8
(;@cf5   ;)    local.get 5
(;@cf7   ;)    local.set 5
(;@cf9   ;)    i32.const 8
(;@cfb   ;)    call $alloc
(;@cfd   ;)    local.set 6
(;@cff   ;)    local.get 6
(;@d01   ;)    local.get 4
(;@d03   ;)    i32.store
(;@d06   ;)    local.get 6
(;@d08   ;)    local.get 5
(;@d0a   ;)    i32.store offset=4
(;@d0d   ;)    local.get 6
(;@d0f   ;)    local.set 6
(;@d11   ;)    i32.const 8
(;@d13   ;)    call $alloc
(;@d15   ;)    local.set 7
(;@d17   ;)    local.get 7
(;@d19   ;)    local.get 3
(;@d1b   ;)    i32.store
(;@d1e   ;)    local.get 7
(;@d20   ;)    local.get 6
(;@d22   ;)    i32.store offset=4
(;@d25   ;)    local.get 7
(;@d27   ;)    local.set 7
(;@d29   ;)    local.get 2
(;@d2b   ;)    i32.load
(;@d2e   ;)    local.set 8
(;@d30   ;)    local.get 8
(;@d32   ;)    local.get 1
(;@d34   ;)    local.get 7
(;@d36   ;)    call $__call_2
(;@d38   ;)    return
             )
(;@d3b   ;)  (func $f_lam_33 (;39;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@d3c   ;)    (local i32 i32 i32)
(;@d3e   ;)    local.get 0
(;@d40   ;)    i32.load
(;@d43   ;)    local.set 3
(;@d45   ;)    i32.const 8
(;@d47   ;)    call $alloc
(;@d49   ;)    local.set 4
(;@d4b   ;)    local.get 4
(;@d4d   ;)    local.get 1
(;@d4f   ;)    i32.store
(;@d52   ;)    local.get 4
(;@d54   ;)    local.get 3
(;@d56   ;)    i32.store offset=4
(;@d59   ;)    local.get 4
(;@d5b   ;)    local.set 4
(;@d5d   ;)    i32.const 8
(;@d5f   ;)    call $alloc
(;@d61   ;)    local.set 5
(;@d63   ;)    local.get 5
(;@d65   ;)    i32.const 0
(;@d67   ;)    i32.store
(;@d6a   ;)    local.get 5
(;@d6c   ;)    local.get 4
(;@d6e   ;)    i32.store offset=4
(;@d71   ;)    local.get 5
(;@d73   ;)    return
             )
(;@d76   ;)  (func $f_lam_34 (;40;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@d77   ;)    (local i32 i32)
(;@d79   ;)    i32.const 16
(;@d7b   ;)    call $alloc
(;@d7d   ;)    local.set 3
(;@d7f   ;)    local.get 3
(;@d81   ;)    i32.const 2
(;@d83   ;)    i32.store
(;@d86   ;)    local.get 3
(;@d88   ;)    i32.const 1
(;@d8a   ;)    i32.store offset=4
(;@d8d   ;)    local.get 3
(;@d8f   ;)    i32.const 39
(;@d91   ;)    i32.store offset=8
(;@d94   ;)    local.get 3
(;@d96   ;)    local.get 1
(;@d98   ;)    i32.store offset=12
(;@d9b   ;)    local.get 3
(;@d9d   ;)    local.set 3
(;@d9f   ;)    i32.const 8
(;@da1   ;)    call $alloc
(;@da3   ;)    local.set 4
(;@da5   ;)    local.get 4
(;@da7   ;)    i32.const 0
(;@da9   ;)    i32.store
(;@dac   ;)    local.get 4
(;@dae   ;)    local.get 3
(;@db0   ;)    i32.store offset=4
(;@db3   ;)    local.get 4
(;@db5   ;)    return
             )
(;@db8   ;)  (func $f_lam_35 (;41;) (type $fun_2_1) (param i32 i32) (result i32)
(;@db9   ;)    (local i32 i32)
(;@dbb   ;)    local.get 0
(;@dbd   ;)    i32.load
(;@dc0   ;)    local.set 2
(;@dc2   ;)    i32.const 8
(;@dc4   ;)    call $alloc
(;@dc6   ;)    local.set 3
(;@dc8   ;)    local.get 3
(;@dca   ;)    i32.const 0
(;@dcc   ;)    i32.store
(;@dcf   ;)    local.get 3
(;@dd1   ;)    local.get 2
(;@dd3   ;)    i32.store offset=4
(;@dd6   ;)    local.get 3
(;@dd8   ;)    return
             )
(;@ddc   ;)  (func $f_lam_36 (;42;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@ddd   ;)    (local i32 i32 i32 i32 i32 i32)
(;@ddf   ;)    local.get 0
(;@de1   ;)    i32.load
(;@de4   ;)    local.set 3
(;@de6   ;)    local.get 0
(;@de8   ;)    i32.load offset=4
(;@deb   ;)    local.set 4
(;@ded   ;)    i32.const 20
(;@def   ;)    call $alloc
(;@df1   ;)    local.set 5
(;@df3   ;)    local.get 5
(;@df5   ;)    i32.const 1
(;@df7   ;)    i32.store
(;@dfa   ;)    local.get 5
(;@dfc   ;)    i32.const 2
(;@dfe   ;)    i32.store offset=4
(;@e01   ;)    local.get 5
(;@e03   ;)    i32.const 38
(;@e05   ;)    i32.store offset=8
(;@e08   ;)    local.get 5
(;@e0a   ;)    local.get 3
(;@e0c   ;)    i32.store offset=12
(;@e0f   ;)    local.get 5
(;@e11   ;)    local.get 4
(;@e13   ;)    i32.store offset=16
(;@e16   ;)    local.get 5
(;@e18   ;)    local.set 5
(;@e1a   ;)    i32.const 12
(;@e1c   ;)    call $alloc
(;@e1e   ;)    local.set 6
(;@e20   ;)    local.get 6
(;@e22   ;)    i32.const 2
(;@e24   ;)    i32.store
(;@e27   ;)    local.get 6
(;@e29   ;)    i32.const 0
(;@e2b   ;)    i32.store offset=4
(;@e2e   ;)    local.get 6
(;@e30   ;)    i32.const 40
(;@e32   ;)    i32.store offset=8
(;@e35   ;)    local.get 6
(;@e37   ;)    local.set 6
(;@e39   ;)    i32.const 16
(;@e3b   ;)    call $alloc
(;@e3d   ;)    local.set 7
(;@e3f   ;)    local.get 7
(;@e41   ;)    i32.const 1
(;@e43   ;)    i32.store
(;@e46   ;)    local.get 7
(;@e48   ;)    i32.const 1
(;@e4a   ;)    i32.store offset=4
(;@e4d   ;)    local.get 7
(;@e4f   ;)    i32.const 41
(;@e51   ;)    i32.store offset=8
(;@e54   ;)    local.get 7
(;@e56   ;)    local.get 1
(;@e58   ;)    i32.store offset=12
(;@e5b   ;)    local.get 7
(;@e5d   ;)    local.set 7
(;@e5f   ;)    local.get 4
(;@e61   ;)    local.get 5
(;@e63   ;)    local.get 6
(;@e65   ;)    local.get 7
(;@e67   ;)    local.get 2
(;@e69   ;)    call $__mon_prompt
(;@e6b   ;)    return
             )
(;@e6e   ;)  (func $f_lam_37 (;43;) (type $fun_2_1) (param i32 i32) (result i32)
(;@e6f   ;)    (local i32 i32 i32)
(;@e71   ;)    local.get 0
(;@e73   ;)    i32.load
(;@e76   ;)    local.set 2
(;@e78   ;)    local.get 0
(;@e7a   ;)    i32.load offset=4
(;@e7d   ;)    local.set 3
(;@e7f   ;)    local.get 2
(;@e81   ;)    local.get 3
(;@e83   ;)    local.get 1
(;@e85   ;)    call $__call_2
(;@e87   ;)    return
             )
(;@e8a   ;)  (func $f_lam_38 (;44;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@e8b   ;)    (local i32 i32)
(;@e8d   ;)    local.get 0
(;@e8f   ;)    i32.load
(;@e92   ;)    local.set 3
(;@e94   ;)    local.get 1
(;@e96   ;)    local.get 3
(;@e98   ;)    local.get 2
(;@e9a   ;)    call $__call_2
(;@e9c   ;)    return
             )
(;@e9f   ;)  (func $f_lam_39 (;45;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@ea0   ;)    (local i32 i32 i32 i32 i32 i32)
(;@ea2   ;)    local.get 0
(;@ea4   ;)    i32.load
(;@ea7   ;)    local.set 3
(;@ea9   ;)    local.get 0
(;@eab   ;)    i32.load offset=4
(;@eae   ;)    local.set 4
(;@eb0   ;)    local.get 4
(;@eb2   ;)    i32.load offset=8
(;@eb5   ;)    local.set 5
(;@eb7   ;)    local.get 5
(;@eb9   ;)    local.get 1
(;@ebb   ;)    call $__call_1
(;@ebd   ;)    local.set 6
(;@ebf   ;)    i32.const 16
(;@ec1   ;)    call $alloc
(;@ec3   ;)    local.set 7
(;@ec5   ;)    local.get 7
(;@ec7   ;)    i32.const 2
(;@ec9   ;)    i32.store
(;@ecc   ;)    local.get 7
(;@ece   ;)    i32.const 1
(;@ed0   ;)    i32.store offset=4
(;@ed3   ;)    local.get 7
(;@ed5   ;)    i32.const 44
(;@ed7   ;)    i32.store offset=8
(;@eda   ;)    local.get 7
(;@edc   ;)    local.get 3
(;@ede   ;)    i32.store offset=12
(;@ee1   ;)    local.get 7
(;@ee3   ;)    local.set 7
(;@ee5   ;)    local.get 6
(;@ee7   ;)    local.get 7
(;@ee9   ;)    local.get 2
(;@eeb   ;)    call $__mon_bind
(;@eed   ;)    return
             )
(;@ef1   ;)  (func $f_lam_40 (;46;) (type $fun_2_1) (param i32 i32) (result i32)
(;@ef2   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@ef4   ;)    local.get 0
(;@ef6   ;)    i32.load
(;@ef9   ;)    local.set 2
(;@efb   ;)    local.get 0
(;@efd   ;)    i32.load offset=4
(;@f00   ;)    local.set 3
(;@f02   ;)    local.get 3
(;@f04   ;)    local.get 1
(;@f06   ;)    call $__call_1
(;@f08   ;)    local.set 4
(;@f0a   ;)    local.get 4
(;@f0c   ;)    i32.load
(;@f0f   ;)    local.set 5
(;@f11   ;)    block (result i32) ;; label = @1
(;@f13   ;)      block ;; label = @2
(;@f15   ;)        block ;; label = @3
(;@f17   ;)          block ;; label = @4
(;@f19   ;)            local.get 5
(;@f1b   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@f20   ;)          end
(;@f21   ;)          local.get 4
(;@f23   ;)          i32.load offset=4
(;@f26   ;)          local.set 6
(;@f28   ;)          local.get 6
(;@f2a   ;)          local.get 2
(;@f2c   ;)          local.get 1
(;@f2e   ;)          call $__call_2
(;@f30   ;)          br 2 (;@1;)
(;@f32   ;)        end
(;@f33   ;)        local.get 4
(;@f35   ;)        i32.load offset=4
(;@f38   ;)        local.set 7
(;@f3a   ;)        local.get 7
(;@f3c   ;)        local.set 8
(;@f3e   ;)        local.get 8
(;@f40   ;)        i32.load
(;@f43   ;)        local.set 9
(;@f45   ;)        local.get 8
(;@f47   ;)        i32.load offset=4
(;@f4a   ;)        local.set 10
(;@f4c   ;)        i32.const 20
(;@f4e   ;)        call $alloc
(;@f50   ;)        local.set 11
(;@f52   ;)        local.get 11
(;@f54   ;)        i32.const 2
(;@f56   ;)        i32.store
(;@f59   ;)        local.get 11
(;@f5b   ;)        i32.const 2
(;@f5d   ;)        i32.store offset=4
(;@f60   ;)        local.get 11
(;@f62   ;)        i32.const 45
(;@f64   ;)        i32.store offset=8
(;@f67   ;)        local.get 11
(;@f69   ;)        local.get 2
(;@f6b   ;)        i32.store offset=12
(;@f6e   ;)        local.get 11
(;@f70   ;)        local.get 8
(;@f72   ;)        i32.store offset=16
(;@f75   ;)        local.get 11
(;@f77   ;)        local.set 11
(;@f79   ;)        i32.const 12
(;@f7b   ;)        call $alloc
(;@f7d   ;)        local.set 12
(;@f7f   ;)        local.get 12
(;@f81   ;)        local.get 9
(;@f83   ;)        i32.store
(;@f86   ;)        local.get 12
(;@f88   ;)        local.get 10
(;@f8a   ;)        i32.store offset=4
(;@f8d   ;)        local.get 12
(;@f8f   ;)        local.get 11
(;@f91   ;)        i32.store offset=8
(;@f94   ;)        local.get 12
(;@f96   ;)        local.set 12
(;@f98   ;)        i32.const 8
(;@f9a   ;)        call $alloc
(;@f9c   ;)        local.set 13
(;@f9e   ;)        local.get 13
(;@fa0   ;)        i32.const 1
(;@fa2   ;)        i32.store
(;@fa5   ;)        local.get 13
(;@fa7   ;)        local.get 12
(;@fa9   ;)        i32.store offset=4
(;@fac   ;)        local.get 13
(;@fae   ;)        br 1 (;@1;)
(;@fb0   ;)      end
(;@fb1   ;)      unreachable
(;@fb2   ;)    end
(;@fb3   ;)    return
             )
(;@fb6   ;)  (func $f_lam_41 (;47;) (type $fun_2_1) (param i32 i32) (result i32)
(;@fb7   ;)    (local i32 i32 i32)
(;@fb9   ;)    local.get 0
(;@fbb   ;)    i32.load
(;@fbe   ;)    local.set 2
(;@fc0   ;)    i32.const 20
(;@fc2   ;)    call $alloc
(;@fc4   ;)    local.set 3
(;@fc6   ;)    local.get 3
(;@fc8   ;)    i32.const 1
(;@fca   ;)    i32.store
(;@fcd   ;)    local.get 3
(;@fcf   ;)    i32.const 2
(;@fd1   ;)    i32.store offset=4
(;@fd4   ;)    local.get 3
(;@fd6   ;)    i32.const 43
(;@fd8   ;)    i32.store offset=8
(;@fdb   ;)    local.get 3
(;@fdd   ;)    local.get 2
(;@fdf   ;)    i32.store offset=12
(;@fe2   ;)    local.get 3
(;@fe4   ;)    local.get 1
(;@fe6   ;)    i32.store offset=16
(;@fe9   ;)    local.get 3
(;@feb   ;)    local.set 3
(;@fed   ;)    i32.const 20
(;@fef   ;)    call $alloc
(;@ff1   ;)    local.set 4
(;@ff3   ;)    local.get 4
(;@ff5   ;)    i32.const 1
(;@ff7   ;)    i32.store
(;@ffa   ;)    local.get 4
(;@ffc   ;)    i32.const 2
(;@ffe   ;)    i32.store offset=4
(;@1001  ;)    local.get 4
(;@1003  ;)    i32.const 46
(;@1005  ;)    i32.store offset=8
(;@1008  ;)    local.get 4
(;@100a  ;)    local.get 1
(;@100c  ;)    i32.store offset=12
(;@100f  ;)    local.get 4
(;@1011  ;)    local.get 3
(;@1013  ;)    i32.store offset=16
(;@1016  ;)    local.get 4
(;@1018  ;)    return
             )
(;@101b  ;)  (func $f_lam_42 (;48;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@101c  ;)    (local i32 i32)
(;@101e  ;)    i32.const 16
(;@1020  ;)    call $alloc
(;@1022  ;)    local.set 3
(;@1024  ;)    local.get 3
(;@1026  ;)    i32.const 1
(;@1028  ;)    i32.store
(;@102b  ;)    local.get 3
(;@102d  ;)    i32.const 1
(;@102f  ;)    i32.store offset=4
(;@1032  ;)    local.get 3
(;@1034  ;)    i32.const 47
(;@1036  ;)    i32.store offset=8
(;@1039  ;)    local.get 3
(;@103b  ;)    local.get 1
(;@103d  ;)    i32.store offset=12
(;@1040  ;)    local.get 3
(;@1042  ;)    local.set 3
(;@1044  ;)    i32.const 8
(;@1046  ;)    call $alloc
(;@1048  ;)    local.set 4
(;@104a  ;)    local.get 4
(;@104c  ;)    i32.const 0
(;@104e  ;)    i32.store
(;@1051  ;)    local.get 4
(;@1053  ;)    local.get 3
(;@1055  ;)    i32.store offset=4
(;@1058  ;)    local.get 4
(;@105a  ;)    return
             )
(;@105d  ;)  (func $f_lam_43 (;49;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@105e  ;)    (local i32 i32)
(;@1060  ;)    i32.const 12
(;@1062  ;)    call $alloc
(;@1064  ;)    local.set 3
(;@1066  ;)    local.get 3
(;@1068  ;)    i32.const 2
(;@106a  ;)    i32.store
(;@106d  ;)    local.get 3
(;@106f  ;)    i32.const 0
(;@1071  ;)    i32.store offset=4
(;@1074  ;)    local.get 3
(;@1076  ;)    i32.const 48
(;@1078  ;)    i32.store offset=8
(;@107b  ;)    local.get 3
(;@107d  ;)    local.set 3
(;@107f  ;)    i32.const 8
(;@1081  ;)    call $alloc
(;@1083  ;)    local.set 4
(;@1085  ;)    local.get 4
(;@1087  ;)    i32.const 0
(;@1089  ;)    i32.store
(;@108c  ;)    local.get 4
(;@108e  ;)    local.get 3
(;@1090  ;)    i32.store offset=4
(;@1093  ;)    local.get 4
(;@1095  ;)    return
             )
(;@1098  ;)  (func $f_lam_44 (;50;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1099  ;)    (local i32 i32 i32)
(;@109b  ;)    local.get 0
(;@109d  ;)    i32.load
(;@10a0  ;)    local.set 2
(;@10a2  ;)    i32.const 0
(;@10a4  ;)    call $alloc
(;@10a6  ;)    local.set 3
(;@10a8  ;)    local.get 3
(;@10aa  ;)    local.set 3
(;@10ac  ;)    local.get 2
(;@10ae  ;)    local.get 3
(;@10b0  ;)    local.get 1
(;@10b2  ;)    call $__call_2
(;@10b4  ;)    return
             )
(;@10b7  ;)  (func $f_lam_45 (;51;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@10b8  ;)    (local i32 i32)
(;@10ba  ;)    local.get 0
(;@10bc  ;)    i32.load
(;@10bf  ;)    local.set 3
(;@10c1  ;)    local.get 1
(;@10c3  ;)    local.get 3
(;@10c5  ;)    local.get 2
(;@10c7  ;)    call $__call_2
(;@10c9  ;)    return
             )
(;@10cc  ;)  (func $f_lam_46 (;52;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@10cd  ;)    (local i32 i32 i32 i32 i32 i32)
(;@10cf  ;)    local.get 0
(;@10d1  ;)    i32.load
(;@10d4  ;)    local.set 3
(;@10d6  ;)    local.get 0
(;@10d8  ;)    i32.load offset=4
(;@10db  ;)    local.set 4
(;@10dd  ;)    local.get 4
(;@10df  ;)    i32.load offset=8
(;@10e2  ;)    local.set 5
(;@10e4  ;)    local.get 5
(;@10e6  ;)    local.get 1
(;@10e8  ;)    call $__call_1
(;@10ea  ;)    local.set 6
(;@10ec  ;)    i32.const 16
(;@10ee  ;)    call $alloc
(;@10f0  ;)    local.set 7
(;@10f2  ;)    local.get 7
(;@10f4  ;)    i32.const 2
(;@10f6  ;)    i32.store
(;@10f9  ;)    local.get 7
(;@10fb  ;)    i32.const 1
(;@10fd  ;)    i32.store offset=4
(;@1100  ;)    local.get 7
(;@1102  ;)    i32.const 51
(;@1104  ;)    i32.store offset=8
(;@1107  ;)    local.get 7
(;@1109  ;)    local.get 3
(;@110b  ;)    i32.store offset=12
(;@110e  ;)    local.get 7
(;@1110  ;)    local.set 7
(;@1112  ;)    local.get 6
(;@1114  ;)    local.get 7
(;@1116  ;)    local.get 2
(;@1118  ;)    call $__mon_bind
(;@111a  ;)    return
             )
(;@111e  ;)  (func $f_lam_47 (;53;) (type $fun_2_1) (param i32 i32) (result i32)
(;@111f  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@1121  ;)    local.get 0
(;@1123  ;)    i32.load
(;@1126  ;)    local.set 2
(;@1128  ;)    local.get 0
(;@112a  ;)    i32.load offset=4
(;@112d  ;)    local.set 3
(;@112f  ;)    local.get 3
(;@1131  ;)    local.get 1
(;@1133  ;)    call $__call_1
(;@1135  ;)    local.set 4
(;@1137  ;)    local.get 4
(;@1139  ;)    i32.load
(;@113c  ;)    local.set 5
(;@113e  ;)    block (result i32) ;; label = @1
(;@1140  ;)      block ;; label = @2
(;@1142  ;)        block ;; label = @3
(;@1144  ;)          block ;; label = @4
(;@1146  ;)            local.get 5
(;@1148  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@114d  ;)          end
(;@114e  ;)          local.get 4
(;@1150  ;)          i32.load offset=4
(;@1153  ;)          local.set 6
(;@1155  ;)          local.get 6
(;@1157  ;)          local.get 2
(;@1159  ;)          local.get 1
(;@115b  ;)          call $__call_2
(;@115d  ;)          br 2 (;@1;)
(;@115f  ;)        end
(;@1160  ;)        local.get 4
(;@1162  ;)        i32.load offset=4
(;@1165  ;)        local.set 7
(;@1167  ;)        local.get 7
(;@1169  ;)        local.set 8
(;@116b  ;)        local.get 8
(;@116d  ;)        i32.load
(;@1170  ;)        local.set 9
(;@1172  ;)        local.get 8
(;@1174  ;)        i32.load offset=4
(;@1177  ;)        local.set 10
(;@1179  ;)        i32.const 20
(;@117b  ;)        call $alloc
(;@117d  ;)        local.set 11
(;@117f  ;)        local.get 11
(;@1181  ;)        i32.const 2
(;@1183  ;)        i32.store
(;@1186  ;)        local.get 11
(;@1188  ;)        i32.const 2
(;@118a  ;)        i32.store offset=4
(;@118d  ;)        local.get 11
(;@118f  ;)        i32.const 52
(;@1191  ;)        i32.store offset=8
(;@1194  ;)        local.get 11
(;@1196  ;)        local.get 2
(;@1198  ;)        i32.store offset=12
(;@119b  ;)        local.get 11
(;@119d  ;)        local.get 8
(;@119f  ;)        i32.store offset=16
(;@11a2  ;)        local.get 11
(;@11a4  ;)        local.set 11
(;@11a6  ;)        i32.const 12
(;@11a8  ;)        call $alloc
(;@11aa  ;)        local.set 12
(;@11ac  ;)        local.get 12
(;@11ae  ;)        local.get 9
(;@11b0  ;)        i32.store
(;@11b3  ;)        local.get 12
(;@11b5  ;)        local.get 10
(;@11b7  ;)        i32.store offset=4
(;@11ba  ;)        local.get 12
(;@11bc  ;)        local.get 11
(;@11be  ;)        i32.store offset=8
(;@11c1  ;)        local.get 12
(;@11c3  ;)        local.set 12
(;@11c5  ;)        i32.const 8
(;@11c7  ;)        call $alloc
(;@11c9  ;)        local.set 13
(;@11cb  ;)        local.get 13
(;@11cd  ;)        i32.const 1
(;@11cf  ;)        i32.store
(;@11d2  ;)        local.get 13
(;@11d4  ;)        local.get 12
(;@11d6  ;)        i32.store offset=4
(;@11d9  ;)        local.get 13
(;@11db  ;)        br 1 (;@1;)
(;@11dd  ;)      end
(;@11de  ;)      unreachable
(;@11df  ;)    end
(;@11e0  ;)    return
             )
(;@11e3  ;)  (func $f_lam_48 (;54;) (type $fun_2_1) (param i32 i32) (result i32)
(;@11e4  ;)    (local i32 i32 i32 i32)
(;@11e6  ;)    local.get 0
(;@11e8  ;)    i32.load
(;@11eb  ;)    local.set 2
(;@11ed  ;)    local.get 0
(;@11ef  ;)    i32.load offset=4
(;@11f2  ;)    local.set 3
(;@11f4  ;)    i32.const 16
(;@11f6  ;)    call $alloc
(;@11f8  ;)    local.set 4
(;@11fa  ;)    local.get 4
(;@11fc  ;)    i32.const 1
(;@11fe  ;)    i32.store
(;@1201  ;)    local.get 4
(;@1203  ;)    i32.const 1
(;@1205  ;)    i32.store offset=4
(;@1208  ;)    local.get 4
(;@120a  ;)    i32.const 50
(;@120c  ;)    i32.store offset=8
(;@120f  ;)    local.get 4
(;@1211  ;)    local.get 3
(;@1213  ;)    i32.store offset=12
(;@1216  ;)    local.get 4
(;@1218  ;)    local.set 4
(;@121a  ;)    i32.const 20
(;@121c  ;)    call $alloc
(;@121e  ;)    local.set 5
(;@1220  ;)    local.get 5
(;@1222  ;)    i32.const 1
(;@1224  ;)    i32.store
(;@1227  ;)    local.get 5
(;@1229  ;)    i32.const 2
(;@122b  ;)    i32.store offset=4
(;@122e  ;)    local.get 5
(;@1230  ;)    i32.const 53
(;@1232  ;)    i32.store offset=8
(;@1235  ;)    local.get 5
(;@1237  ;)    local.get 2
(;@1239  ;)    i32.store offset=12
(;@123c  ;)    local.get 5
(;@123e  ;)    local.get 4
(;@1240  ;)    i32.store offset=16
(;@1243  ;)    local.get 5
(;@1245  ;)    return
             )
(;@1248  ;)  (func $f_lam_49 (;55;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1249  ;)    (local i32 i32 i32)
(;@124b  ;)    local.get 0
(;@124d  ;)    i32.load
(;@1250  ;)    local.set 3
(;@1252  ;)    i32.const 20
(;@1254  ;)    call $alloc
(;@1256  ;)    local.set 4
(;@1258  ;)    local.get 4
(;@125a  ;)    i32.const 1
(;@125c  ;)    i32.store
(;@125f  ;)    local.get 4
(;@1261  ;)    i32.const 2
(;@1263  ;)    i32.store offset=4
(;@1266  ;)    local.get 4
(;@1268  ;)    i32.const 54
(;@126a  ;)    i32.store offset=8
(;@126d  ;)    local.get 4
(;@126f  ;)    local.get 3
(;@1271  ;)    i32.store offset=12
(;@1274  ;)    local.get 4
(;@1276  ;)    local.get 1
(;@1278  ;)    i32.store offset=16
(;@127b  ;)    local.get 4
(;@127d  ;)    local.set 4
(;@127f  ;)    i32.const 8
(;@1281  ;)    call $alloc
(;@1283  ;)    local.set 5
(;@1285  ;)    local.get 5
(;@1287  ;)    i32.const 0
(;@1289  ;)    i32.store
(;@128c  ;)    local.get 5
(;@128e  ;)    local.get 4
(;@1290  ;)    i32.store offset=4
(;@1293  ;)    local.get 5
(;@1295  ;)    return
             )
(;@1298  ;)  (func $f_lam_50 (;56;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1299  ;)    (local i32 i32)
(;@129b  ;)    i32.const 16
(;@129d  ;)    call $alloc
(;@129f  ;)    local.set 3
(;@12a1  ;)    local.get 3
(;@12a3  ;)    i32.const 2
(;@12a5  ;)    i32.store
(;@12a8  ;)    local.get 3
(;@12aa  ;)    i32.const 1
(;@12ac  ;)    i32.store offset=4
(;@12af  ;)    local.get 3
(;@12b1  ;)    i32.const 55
(;@12b3  ;)    i32.store offset=8
(;@12b6  ;)    local.get 3
(;@12b8  ;)    local.get 1
(;@12ba  ;)    i32.store offset=12
(;@12bd  ;)    local.get 3
(;@12bf  ;)    local.set 3
(;@12c1  ;)    i32.const 8
(;@12c3  ;)    call $alloc
(;@12c5  ;)    local.set 4
(;@12c7  ;)    local.get 4
(;@12c9  ;)    i32.const 0
(;@12cb  ;)    i32.store
(;@12ce  ;)    local.get 4
(;@12d0  ;)    local.get 3
(;@12d2  ;)    i32.store offset=4
(;@12d5  ;)    local.get 4
(;@12d7  ;)    return
             )
(;@12db  ;)  (func $f_lam_51 (;57;) (type $fun_2_1) (param i32 i32) (result i32)
(;@12dc  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32)
(;@12de  ;)    local.get 0
(;@12e0  ;)    i32.load
(;@12e3  ;)    local.set 2
(;@12e5  ;)    local.get 0
(;@12e7  ;)    i32.load offset=4
(;@12ea  ;)    local.set 3
(;@12ec  ;)    i32.const 12
(;@12ee  ;)    call $alloc
(;@12f0  ;)    local.set 4
(;@12f2  ;)    local.get 4
(;@12f4  ;)    i32.const 2
(;@12f6  ;)    i32.store
(;@12f9  ;)    local.get 4
(;@12fb  ;)    i32.const 0
(;@12fd  ;)    i32.store offset=4
(;@1300  ;)    local.get 4
(;@1302  ;)    i32.const 49
(;@1304  ;)    i32.store offset=8
(;@1307  ;)    local.get 4
(;@1309  ;)    local.set 4
(;@130b  ;)    i32.const 12
(;@130d  ;)    call $alloc
(;@130f  ;)    local.set 5
(;@1311  ;)    local.get 5
(;@1313  ;)    i32.const 2
(;@1315  ;)    i32.store
(;@1318  ;)    local.get 5
(;@131a  ;)    i32.const 0
(;@131c  ;)    i32.store offset=4
(;@131f  ;)    local.get 5
(;@1321  ;)    i32.const 56
(;@1323  ;)    i32.store offset=8
(;@1326  ;)    local.get 5
(;@1328  ;)    local.set 5
(;@132a  ;)    i32.const 8
(;@132c  ;)    call $alloc
(;@132e  ;)    local.set 6
(;@1330  ;)    local.get 6
(;@1332  ;)    local.get 4
(;@1334  ;)    i32.store
(;@1337  ;)    local.get 6
(;@1339  ;)    local.get 5
(;@133b  ;)    i32.store offset=4
(;@133e  ;)    local.get 6
(;@1340  ;)    local.set 6
(;@1342  ;)    i32.const 8
(;@1344  ;)    call $alloc
(;@1346  ;)    local.set 7
(;@1348  ;)    local.get 7
(;@134a  ;)    local.get 3
(;@134c  ;)    i32.store
(;@134f  ;)    local.get 7
(;@1351  ;)    local.get 6
(;@1353  ;)    i32.store offset=4
(;@1356  ;)    local.get 7
(;@1358  ;)    local.set 7
(;@135a  ;)    local.get 2
(;@135c  ;)    i32.load
(;@135f  ;)    local.set 8
(;@1361  ;)    local.get 8
(;@1363  ;)    local.get 1
(;@1365  ;)    local.get 7
(;@1367  ;)    call $__call_2
(;@1369  ;)    return
             )
(;@136c  ;)  (func $f_lam_52 (;58;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@136d  ;)    (local i32 i32 i32)
(;@136f  ;)    local.get 0
(;@1371  ;)    i32.load
(;@1374  ;)    local.set 3
(;@1376  ;)    i32.const 8
(;@1378  ;)    call $alloc
(;@137a  ;)    local.set 4
(;@137c  ;)    local.get 4
(;@137e  ;)    local.get 1
(;@1380  ;)    i32.store
(;@1383  ;)    local.get 4
(;@1385  ;)    local.get 3
(;@1387  ;)    i32.store offset=4
(;@138a  ;)    local.get 4
(;@138c  ;)    local.set 4
(;@138e  ;)    i32.const 8
(;@1390  ;)    call $alloc
(;@1392  ;)    local.set 5
(;@1394  ;)    local.get 5
(;@1396  ;)    i32.const 0
(;@1398  ;)    i32.store
(;@139b  ;)    local.get 5
(;@139d  ;)    local.get 4
(;@139f  ;)    i32.store offset=4
(;@13a2  ;)    local.get 5
(;@13a4  ;)    return
             )
(;@13a7  ;)  (func $f_lam_53 (;59;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@13a8  ;)    (local i32 i32)
(;@13aa  ;)    i32.const 16
(;@13ac  ;)    call $alloc
(;@13ae  ;)    local.set 3
(;@13b0  ;)    local.get 3
(;@13b2  ;)    i32.const 2
(;@13b4  ;)    i32.store
(;@13b7  ;)    local.get 3
(;@13b9  ;)    i32.const 1
(;@13bb  ;)    i32.store offset=4
(;@13be  ;)    local.get 3
(;@13c0  ;)    i32.const 58
(;@13c2  ;)    i32.store offset=8
(;@13c5  ;)    local.get 3
(;@13c7  ;)    local.get 1
(;@13c9  ;)    i32.store offset=12
(;@13cc  ;)    local.get 3
(;@13ce  ;)    local.set 3
(;@13d0  ;)    i32.const 8
(;@13d2  ;)    call $alloc
(;@13d4  ;)    local.set 4
(;@13d6  ;)    local.get 4
(;@13d8  ;)    i32.const 0
(;@13da  ;)    i32.store
(;@13dd  ;)    local.get 4
(;@13df  ;)    local.get 3
(;@13e1  ;)    i32.store offset=4
(;@13e4  ;)    local.get 4
(;@13e6  ;)    return
             )
(;@13e9  ;)  (func $f_lam_54 (;60;) (type $fun_2_1) (param i32 i32) (result i32)
(;@13ea  ;)    (local i32 i32)
(;@13ec  ;)    local.get 0
(;@13ee  ;)    i32.load
(;@13f1  ;)    local.set 2
(;@13f3  ;)    i32.const 8
(;@13f5  ;)    call $alloc
(;@13f7  ;)    local.set 3
(;@13f9  ;)    local.get 3
(;@13fb  ;)    i32.const 0
(;@13fd  ;)    i32.store
(;@1400  ;)    local.get 3
(;@1402  ;)    local.get 2
(;@1404  ;)    i32.store offset=4
(;@1407  ;)    local.get 3
(;@1409  ;)    return
             )
(;@140d  ;)  (func $f_lam_55 (;61;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@140e  ;)    (local i32 i32 i32 i32 i32 i32)
(;@1410  ;)    local.get 0
(;@1412  ;)    i32.load
(;@1415  ;)    local.set 3
(;@1417  ;)    local.get 0
(;@1419  ;)    i32.load offset=4
(;@141c  ;)    local.set 4
(;@141e  ;)    i32.const 20
(;@1420  ;)    call $alloc
(;@1422  ;)    local.set 5
(;@1424  ;)    local.get 5
(;@1426  ;)    i32.const 1
(;@1428  ;)    i32.store
(;@142b  ;)    local.get 5
(;@142d  ;)    i32.const 2
(;@142f  ;)    i32.store offset=4
(;@1432  ;)    local.get 5
(;@1434  ;)    i32.const 57
(;@1436  ;)    i32.store offset=8
(;@1439  ;)    local.get 5
(;@143b  ;)    local.get 3
(;@143d  ;)    i32.store offset=12
(;@1440  ;)    local.get 5
(;@1442  ;)    local.get 4
(;@1444  ;)    i32.store offset=16
(;@1447  ;)    local.get 5
(;@1449  ;)    local.set 5
(;@144b  ;)    i32.const 12
(;@144d  ;)    call $alloc
(;@144f  ;)    local.set 6
(;@1451  ;)    local.get 6
(;@1453  ;)    i32.const 2
(;@1455  ;)    i32.store
(;@1458  ;)    local.get 6
(;@145a  ;)    i32.const 0
(;@145c  ;)    i32.store offset=4
(;@145f  ;)    local.get 6
(;@1461  ;)    i32.const 59
(;@1463  ;)    i32.store offset=8
(;@1466  ;)    local.get 6
(;@1468  ;)    local.set 6
(;@146a  ;)    i32.const 16
(;@146c  ;)    call $alloc
(;@146e  ;)    local.set 7
(;@1470  ;)    local.get 7
(;@1472  ;)    i32.const 1
(;@1474  ;)    i32.store
(;@1477  ;)    local.get 7
(;@1479  ;)    i32.const 1
(;@147b  ;)    i32.store offset=4
(;@147e  ;)    local.get 7
(;@1480  ;)    i32.const 60
(;@1482  ;)    i32.store offset=8
(;@1485  ;)    local.get 7
(;@1487  ;)    local.get 1
(;@1489  ;)    i32.store offset=12
(;@148c  ;)    local.get 7
(;@148e  ;)    local.set 7
(;@1490  ;)    local.get 4
(;@1492  ;)    local.get 5
(;@1494  ;)    local.get 6
(;@1496  ;)    local.get 7
(;@1498  ;)    local.get 2
(;@149a  ;)    call $__mon_prompt
(;@149c  ;)    return
             )
(;@149f  ;)  (func $f_lam_56 (;62;) (type $fun_2_1) (param i32 i32) (result i32)
(;@14a0  ;)    (local i32 i32 i32)
(;@14a2  ;)    local.get 0
(;@14a4  ;)    i32.load
(;@14a7  ;)    local.set 2
(;@14a9  ;)    local.get 0
(;@14ab  ;)    i32.load offset=4
(;@14ae  ;)    local.set 3
(;@14b0  ;)    local.get 2
(;@14b2  ;)    local.get 3
(;@14b4  ;)    local.get 1
(;@14b6  ;)    call $__call_2
(;@14b8  ;)    return
             )
(;@14bb  ;)  (func $f_lam_57 (;63;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@14bc  ;)    (local i32 i32)
(;@14be  ;)    local.get 0
(;@14c0  ;)    i32.load
(;@14c3  ;)    local.set 3
(;@14c5  ;)    local.get 1
(;@14c7  ;)    local.get 3
(;@14c9  ;)    local.get 2
(;@14cb  ;)    call $__call_2
(;@14cd  ;)    return
             )
(;@14d0  ;)  (func $f_lam_58 (;64;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@14d1  ;)    (local i32 i32 i32 i32 i32 i32)
(;@14d3  ;)    local.get 0
(;@14d5  ;)    i32.load
(;@14d8  ;)    local.set 3
(;@14da  ;)    local.get 0
(;@14dc  ;)    i32.load offset=4
(;@14df  ;)    local.set 4
(;@14e1  ;)    local.get 4
(;@14e3  ;)    i32.load offset=8
(;@14e6  ;)    local.set 5
(;@14e8  ;)    local.get 5
(;@14ea  ;)    local.get 1
(;@14ec  ;)    call $__call_1
(;@14ee  ;)    local.set 6
(;@14f0  ;)    i32.const 16
(;@14f2  ;)    call $alloc
(;@14f4  ;)    local.set 7
(;@14f6  ;)    local.get 7
(;@14f8  ;)    i32.const 2
(;@14fa  ;)    i32.store
(;@14fd  ;)    local.get 7
(;@14ff  ;)    i32.const 1
(;@1501  ;)    i32.store offset=4
(;@1504  ;)    local.get 7
(;@1506  ;)    i32.const 63
(;@1508  ;)    i32.store offset=8
(;@150b  ;)    local.get 7
(;@150d  ;)    local.get 3
(;@150f  ;)    i32.store offset=12
(;@1512  ;)    local.get 7
(;@1514  ;)    local.set 7
(;@1516  ;)    local.get 6
(;@1518  ;)    local.get 7
(;@151a  ;)    local.get 2
(;@151c  ;)    call $__mon_bind
(;@151e  ;)    return
             )
(;@1522  ;)  (func $f_lam_59 (;65;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1523  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@1525  ;)    local.get 0
(;@1527  ;)    i32.load
(;@152a  ;)    local.set 2
(;@152c  ;)    local.get 0
(;@152e  ;)    i32.load offset=4
(;@1531  ;)    local.set 3
(;@1533  ;)    local.get 3
(;@1535  ;)    local.get 1
(;@1537  ;)    call $__call_1
(;@1539  ;)    local.set 4
(;@153b  ;)    local.get 4
(;@153d  ;)    i32.load
(;@1540  ;)    local.set 5
(;@1542  ;)    block (result i32) ;; label = @1
(;@1544  ;)      block ;; label = @2
(;@1546  ;)        block ;; label = @3
(;@1548  ;)          block ;; label = @4
(;@154a  ;)            local.get 5
(;@154c  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1551  ;)          end
(;@1552  ;)          local.get 4
(;@1554  ;)          i32.load offset=4
(;@1557  ;)          local.set 6
(;@1559  ;)          local.get 6
(;@155b  ;)          local.get 2
(;@155d  ;)          local.get 1
(;@155f  ;)          call $__call_2
(;@1561  ;)          br 2 (;@1;)
(;@1563  ;)        end
(;@1564  ;)        local.get 4
(;@1566  ;)        i32.load offset=4
(;@1569  ;)        local.set 7
(;@156b  ;)        local.get 7
(;@156d  ;)        local.set 8
(;@156f  ;)        local.get 8
(;@1571  ;)        i32.load
(;@1574  ;)        local.set 9
(;@1576  ;)        local.get 8
(;@1578  ;)        i32.load offset=4
(;@157b  ;)        local.set 10
(;@157d  ;)        i32.const 20
(;@157f  ;)        call $alloc
(;@1581  ;)        local.set 11
(;@1583  ;)        local.get 11
(;@1585  ;)        i32.const 2
(;@1587  ;)        i32.store
(;@158a  ;)        local.get 11
(;@158c  ;)        i32.const 2
(;@158e  ;)        i32.store offset=4
(;@1591  ;)        local.get 11
(;@1593  ;)        i32.const 64
(;@1596  ;)        i32.store offset=8
(;@1599  ;)        local.get 11
(;@159b  ;)        local.get 2
(;@159d  ;)        i32.store offset=12
(;@15a0  ;)        local.get 11
(;@15a2  ;)        local.get 8
(;@15a4  ;)        i32.store offset=16
(;@15a7  ;)        local.get 11
(;@15a9  ;)        local.set 11
(;@15ab  ;)        i32.const 12
(;@15ad  ;)        call $alloc
(;@15af  ;)        local.set 12
(;@15b1  ;)        local.get 12
(;@15b3  ;)        local.get 9
(;@15b5  ;)        i32.store
(;@15b8  ;)        local.get 12
(;@15ba  ;)        local.get 10
(;@15bc  ;)        i32.store offset=4
(;@15bf  ;)        local.get 12
(;@15c1  ;)        local.get 11
(;@15c3  ;)        i32.store offset=8
(;@15c6  ;)        local.get 12
(;@15c8  ;)        local.set 12
(;@15ca  ;)        i32.const 8
(;@15cc  ;)        call $alloc
(;@15ce  ;)        local.set 13
(;@15d0  ;)        local.get 13
(;@15d2  ;)        i32.const 1
(;@15d4  ;)        i32.store
(;@15d7  ;)        local.get 13
(;@15d9  ;)        local.get 12
(;@15db  ;)        i32.store offset=4
(;@15de  ;)        local.get 13
(;@15e0  ;)        br 1 (;@1;)
(;@15e2  ;)      end
(;@15e3  ;)      unreachable
(;@15e4  ;)    end
(;@15e5  ;)    return
             )
(;@15e8  ;)  (func $f_lam_60 (;66;) (type $fun_2_1) (param i32 i32) (result i32)
(;@15e9  ;)    (local i32 i32 i32)
(;@15eb  ;)    local.get 0
(;@15ed  ;)    i32.load
(;@15f0  ;)    local.set 2
(;@15f2  ;)    i32.const 20
(;@15f4  ;)    call $alloc
(;@15f6  ;)    local.set 3
(;@15f8  ;)    local.get 3
(;@15fa  ;)    i32.const 1
(;@15fc  ;)    i32.store
(;@15ff  ;)    local.get 3
(;@1601  ;)    i32.const 2
(;@1603  ;)    i32.store offset=4
(;@1606  ;)    local.get 3
(;@1608  ;)    i32.const 62
(;@160a  ;)    i32.store offset=8
(;@160d  ;)    local.get 3
(;@160f  ;)    local.get 2
(;@1611  ;)    i32.store offset=12
(;@1614  ;)    local.get 3
(;@1616  ;)    local.get 1
(;@1618  ;)    i32.store offset=16
(;@161b  ;)    local.get 3
(;@161d  ;)    local.set 3
(;@161f  ;)    i32.const 20
(;@1621  ;)    call $alloc
(;@1623  ;)    local.set 4
(;@1625  ;)    local.get 4
(;@1627  ;)    i32.const 1
(;@1629  ;)    i32.store
(;@162c  ;)    local.get 4
(;@162e  ;)    i32.const 2
(;@1630  ;)    i32.store offset=4
(;@1633  ;)    local.get 4
(;@1635  ;)    i32.const 65
(;@1638  ;)    i32.store offset=8
(;@163b  ;)    local.get 4
(;@163d  ;)    local.get 1
(;@163f  ;)    i32.store offset=12
(;@1642  ;)    local.get 4
(;@1644  ;)    local.get 3
(;@1646  ;)    i32.store offset=16
(;@1649  ;)    local.get 4
(;@164b  ;)    return
             )
(;@164e  ;)  (func $f_lam_61 (;67;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@164f  ;)    (local i32 i32)
(;@1651  ;)    i32.const 16
(;@1653  ;)    call $alloc
(;@1655  ;)    local.set 3
(;@1657  ;)    local.get 3
(;@1659  ;)    i32.const 1
(;@165b  ;)    i32.store
(;@165e  ;)    local.get 3
(;@1660  ;)    i32.const 1
(;@1662  ;)    i32.store offset=4
(;@1665  ;)    local.get 3
(;@1667  ;)    i32.const 66
(;@166a  ;)    i32.store offset=8
(;@166d  ;)    local.get 3
(;@166f  ;)    local.get 1
(;@1671  ;)    i32.store offset=12
(;@1674  ;)    local.get 3
(;@1676  ;)    local.set 3
(;@1678  ;)    i32.const 8
(;@167a  ;)    call $alloc
(;@167c  ;)    local.set 4
(;@167e  ;)    local.get 4
(;@1680  ;)    i32.const 0
(;@1682  ;)    i32.store
(;@1685  ;)    local.get 4
(;@1687  ;)    local.get 3
(;@1689  ;)    i32.store offset=4
(;@168c  ;)    local.get 4
(;@168e  ;)    return
             )
(;@1691  ;)  (func $f_lam_62 (;68;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1692  ;)    (local i32 i32)
(;@1694  ;)    i32.const 12
(;@1696  ;)    call $alloc
(;@1698  ;)    local.set 3
(;@169a  ;)    local.get 3
(;@169c  ;)    i32.const 2
(;@169e  ;)    i32.store
(;@16a1  ;)    local.get 3
(;@16a3  ;)    i32.const 0
(;@16a5  ;)    i32.store offset=4
(;@16a8  ;)    local.get 3
(;@16aa  ;)    i32.const 67
(;@16ad  ;)    i32.store offset=8
(;@16b0  ;)    local.get 3
(;@16b2  ;)    local.set 3
(;@16b4  ;)    i32.const 8
(;@16b6  ;)    call $alloc
(;@16b8  ;)    local.set 4
(;@16ba  ;)    local.get 4
(;@16bc  ;)    i32.const 0
(;@16be  ;)    i32.store
(;@16c1  ;)    local.get 4
(;@16c3  ;)    local.get 3
(;@16c5  ;)    i32.store offset=4
(;@16c8  ;)    local.get 4
(;@16ca  ;)    return
             )
(;@16cd  ;)  (func $f_lam_63 (;69;) (type $fun_2_1) (param i32 i32) (result i32)
(;@16ce  ;)    (local i32 i32 i32)
(;@16d0  ;)    local.get 0
(;@16d2  ;)    i32.load
(;@16d5  ;)    local.set 2
(;@16d7  ;)    i32.const 0
(;@16d9  ;)    call $alloc
(;@16db  ;)    local.set 3
(;@16dd  ;)    local.get 3
(;@16df  ;)    local.set 3
(;@16e1  ;)    local.get 2
(;@16e3  ;)    local.get 3
(;@16e5  ;)    local.get 1
(;@16e7  ;)    call $__call_2
(;@16e9  ;)    return
             )
(;@16ec  ;)  (func $f_lam_64 (;70;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@16ed  ;)    (local i32 i32)
(;@16ef  ;)    local.get 0
(;@16f1  ;)    i32.load
(;@16f4  ;)    local.set 3
(;@16f6  ;)    local.get 1
(;@16f8  ;)    local.get 3
(;@16fa  ;)    local.get 2
(;@16fc  ;)    call $__call_2
(;@16fe  ;)    return
             )
(;@1701  ;)  (func $f_lam_65 (;71;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1702  ;)    (local i32 i32 i32 i32 i32 i32)
(;@1704  ;)    local.get 0
(;@1706  ;)    i32.load
(;@1709  ;)    local.set 3
(;@170b  ;)    local.get 0
(;@170d  ;)    i32.load offset=4
(;@1710  ;)    local.set 4
(;@1712  ;)    local.get 4
(;@1714  ;)    i32.load offset=8
(;@1717  ;)    local.set 5
(;@1719  ;)    local.get 5
(;@171b  ;)    local.get 1
(;@171d  ;)    call $__call_1
(;@171f  ;)    local.set 6
(;@1721  ;)    i32.const 16
(;@1723  ;)    call $alloc
(;@1725  ;)    local.set 7
(;@1727  ;)    local.get 7
(;@1729  ;)    i32.const 2
(;@172b  ;)    i32.store
(;@172e  ;)    local.get 7
(;@1730  ;)    i32.const 1
(;@1732  ;)    i32.store offset=4
(;@1735  ;)    local.get 7
(;@1737  ;)    i32.const 70
(;@173a  ;)    i32.store offset=8
(;@173d  ;)    local.get 7
(;@173f  ;)    local.get 3
(;@1741  ;)    i32.store offset=12
(;@1744  ;)    local.get 7
(;@1746  ;)    local.set 7
(;@1748  ;)    local.get 6
(;@174a  ;)    local.get 7
(;@174c  ;)    local.get 2
(;@174e  ;)    call $__mon_bind
(;@1750  ;)    return
             )
(;@1754  ;)  (func $f_lam_66 (;72;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1755  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@1757  ;)    local.get 0
(;@1759  ;)    i32.load
(;@175c  ;)    local.set 2
(;@175e  ;)    local.get 0
(;@1760  ;)    i32.load offset=4
(;@1763  ;)    local.set 3
(;@1765  ;)    local.get 3
(;@1767  ;)    local.get 1
(;@1769  ;)    call $__call_1
(;@176b  ;)    local.set 4
(;@176d  ;)    local.get 4
(;@176f  ;)    i32.load
(;@1772  ;)    local.set 5
(;@1774  ;)    block (result i32) ;; label = @1
(;@1776  ;)      block ;; label = @2
(;@1778  ;)        block ;; label = @3
(;@177a  ;)          block ;; label = @4
(;@177c  ;)            local.get 5
(;@177e  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1783  ;)          end
(;@1784  ;)          local.get 4
(;@1786  ;)          i32.load offset=4
(;@1789  ;)          local.set 6
(;@178b  ;)          local.get 6
(;@178d  ;)          local.get 2
(;@178f  ;)          local.get 1
(;@1791  ;)          call $__call_2
(;@1793  ;)          br 2 (;@1;)
(;@1795  ;)        end
(;@1796  ;)        local.get 4
(;@1798  ;)        i32.load offset=4
(;@179b  ;)        local.set 7
(;@179d  ;)        local.get 7
(;@179f  ;)        local.set 8
(;@17a1  ;)        local.get 8
(;@17a3  ;)        i32.load
(;@17a6  ;)        local.set 9
(;@17a8  ;)        local.get 8
(;@17aa  ;)        i32.load offset=4
(;@17ad  ;)        local.set 10
(;@17af  ;)        i32.const 20
(;@17b1  ;)        call $alloc
(;@17b3  ;)        local.set 11
(;@17b5  ;)        local.get 11
(;@17b7  ;)        i32.const 2
(;@17b9  ;)        i32.store
(;@17bc  ;)        local.get 11
(;@17be  ;)        i32.const 2
(;@17c0  ;)        i32.store offset=4
(;@17c3  ;)        local.get 11
(;@17c5  ;)        i32.const 71
(;@17c8  ;)        i32.store offset=8
(;@17cb  ;)        local.get 11
(;@17cd  ;)        local.get 2
(;@17cf  ;)        i32.store offset=12
(;@17d2  ;)        local.get 11
(;@17d4  ;)        local.get 8
(;@17d6  ;)        i32.store offset=16
(;@17d9  ;)        local.get 11
(;@17db  ;)        local.set 11
(;@17dd  ;)        i32.const 12
(;@17df  ;)        call $alloc
(;@17e1  ;)        local.set 12
(;@17e3  ;)        local.get 12
(;@17e5  ;)        local.get 9
(;@17e7  ;)        i32.store
(;@17ea  ;)        local.get 12
(;@17ec  ;)        local.get 10
(;@17ee  ;)        i32.store offset=4
(;@17f1  ;)        local.get 12
(;@17f3  ;)        local.get 11
(;@17f5  ;)        i32.store offset=8
(;@17f8  ;)        local.get 12
(;@17fa  ;)        local.set 12
(;@17fc  ;)        i32.const 8
(;@17fe  ;)        call $alloc
(;@1800  ;)        local.set 13
(;@1802  ;)        local.get 13
(;@1804  ;)        i32.const 1
(;@1806  ;)        i32.store
(;@1809  ;)        local.get 13
(;@180b  ;)        local.get 12
(;@180d  ;)        i32.store offset=4
(;@1810  ;)        local.get 13
(;@1812  ;)        br 1 (;@1;)
(;@1814  ;)      end
(;@1815  ;)      unreachable
(;@1816  ;)    end
(;@1817  ;)    return
             )
(;@181a  ;)  (func $f_lam_67 (;73;) (type $fun_2_1) (param i32 i32) (result i32)
(;@181b  ;)    (local i32 i32 i32 i32)
(;@181d  ;)    local.get 0
(;@181f  ;)    i32.load
(;@1822  ;)    local.set 2
(;@1824  ;)    local.get 0
(;@1826  ;)    i32.load offset=4
(;@1829  ;)    local.set 3
(;@182b  ;)    i32.const 16
(;@182d  ;)    call $alloc
(;@182f  ;)    local.set 4
(;@1831  ;)    local.get 4
(;@1833  ;)    i32.const 1
(;@1835  ;)    i32.store
(;@1838  ;)    local.get 4
(;@183a  ;)    i32.const 1
(;@183c  ;)    i32.store offset=4
(;@183f  ;)    local.get 4
(;@1841  ;)    i32.const 69
(;@1844  ;)    i32.store offset=8
(;@1847  ;)    local.get 4
(;@1849  ;)    local.get 3
(;@184b  ;)    i32.store offset=12
(;@184e  ;)    local.get 4
(;@1850  ;)    local.set 4
(;@1852  ;)    i32.const 20
(;@1854  ;)    call $alloc
(;@1856  ;)    local.set 5
(;@1858  ;)    local.get 5
(;@185a  ;)    i32.const 1
(;@185c  ;)    i32.store
(;@185f  ;)    local.get 5
(;@1861  ;)    i32.const 2
(;@1863  ;)    i32.store offset=4
(;@1866  ;)    local.get 5
(;@1868  ;)    i32.const 72
(;@186b  ;)    i32.store offset=8
(;@186e  ;)    local.get 5
(;@1870  ;)    local.get 2
(;@1872  ;)    i32.store offset=12
(;@1875  ;)    local.get 5
(;@1877  ;)    local.get 4
(;@1879  ;)    i32.store offset=16
(;@187c  ;)    local.get 5
(;@187e  ;)    return
             )
(;@1881  ;)  (func $f_lam_68 (;74;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1882  ;)    (local i32 i32 i32)
(;@1884  ;)    local.get 0
(;@1886  ;)    i32.load
(;@1889  ;)    local.set 3
(;@188b  ;)    i32.const 20
(;@188d  ;)    call $alloc
(;@188f  ;)    local.set 4
(;@1891  ;)    local.get 4
(;@1893  ;)    i32.const 1
(;@1895  ;)    i32.store
(;@1898  ;)    local.get 4
(;@189a  ;)    i32.const 2
(;@189c  ;)    i32.store offset=4
(;@189f  ;)    local.get 4
(;@18a1  ;)    i32.const 73
(;@18a4  ;)    i32.store offset=8
(;@18a7  ;)    local.get 4
(;@18a9  ;)    local.get 3
(;@18ab  ;)    i32.store offset=12
(;@18ae  ;)    local.get 4
(;@18b0  ;)    local.get 1
(;@18b2  ;)    i32.store offset=16
(;@18b5  ;)    local.get 4
(;@18b7  ;)    local.set 4
(;@18b9  ;)    i32.const 8
(;@18bb  ;)    call $alloc
(;@18bd  ;)    local.set 5
(;@18bf  ;)    local.get 5
(;@18c1  ;)    i32.const 0
(;@18c3  ;)    i32.store
(;@18c6  ;)    local.get 5
(;@18c8  ;)    local.get 4
(;@18ca  ;)    i32.store offset=4
(;@18cd  ;)    local.get 5
(;@18cf  ;)    return
             )
(;@18d2  ;)  (func $f_lam_69 (;75;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@18d3  ;)    (local i32 i32)
(;@18d5  ;)    i32.const 16
(;@18d7  ;)    call $alloc
(;@18d9  ;)    local.set 3
(;@18db  ;)    local.get 3
(;@18dd  ;)    i32.const 2
(;@18df  ;)    i32.store
(;@18e2  ;)    local.get 3
(;@18e4  ;)    i32.const 1
(;@18e6  ;)    i32.store offset=4
(;@18e9  ;)    local.get 3
(;@18eb  ;)    i32.const 74
(;@18ee  ;)    i32.store offset=8
(;@18f1  ;)    local.get 3
(;@18f3  ;)    local.get 1
(;@18f5  ;)    i32.store offset=12
(;@18f8  ;)    local.get 3
(;@18fa  ;)    local.set 3
(;@18fc  ;)    i32.const 8
(;@18fe  ;)    call $alloc
(;@1900  ;)    local.set 4
(;@1902  ;)    local.get 4
(;@1904  ;)    i32.const 0
(;@1906  ;)    i32.store
(;@1909  ;)    local.get 4
(;@190b  ;)    local.get 3
(;@190d  ;)    i32.store offset=4
(;@1910  ;)    local.get 4
(;@1912  ;)    return
             )
(;@1916  ;)  (func $f_lam_70 (;76;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1917  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32)
(;@1919  ;)    local.get 0
(;@191b  ;)    i32.load
(;@191e  ;)    local.set 2
(;@1920  ;)    local.get 0
(;@1922  ;)    i32.load offset=4
(;@1925  ;)    local.set 3
(;@1927  ;)    i32.const 12
(;@1929  ;)    call $alloc
(;@192b  ;)    local.set 4
(;@192d  ;)    local.get 4
(;@192f  ;)    i32.const 2
(;@1931  ;)    i32.store
(;@1934  ;)    local.get 4
(;@1936  ;)    i32.const 0
(;@1938  ;)    i32.store offset=4
(;@193b  ;)    local.get 4
(;@193d  ;)    i32.const 68
(;@1940  ;)    i32.store offset=8
(;@1943  ;)    local.get 4
(;@1945  ;)    local.set 4
(;@1947  ;)    i32.const 12
(;@1949  ;)    call $alloc
(;@194b  ;)    local.set 5
(;@194d  ;)    local.get 5
(;@194f  ;)    i32.const 2
(;@1951  ;)    i32.store
(;@1954  ;)    local.get 5
(;@1956  ;)    i32.const 0
(;@1958  ;)    i32.store offset=4
(;@195b  ;)    local.get 5
(;@195d  ;)    i32.const 75
(;@1960  ;)    i32.store offset=8
(;@1963  ;)    local.get 5
(;@1965  ;)    local.set 5
(;@1967  ;)    i32.const 8
(;@1969  ;)    call $alloc
(;@196b  ;)    local.set 6
(;@196d  ;)    local.get 6
(;@196f  ;)    local.get 4
(;@1971  ;)    i32.store
(;@1974  ;)    local.get 6
(;@1976  ;)    local.get 5
(;@1978  ;)    i32.store offset=4
(;@197b  ;)    local.get 6
(;@197d  ;)    local.set 6
(;@197f  ;)    i32.const 8
(;@1981  ;)    call $alloc
(;@1983  ;)    local.set 7
(;@1985  ;)    local.get 7
(;@1987  ;)    local.get 3
(;@1989  ;)    i32.store
(;@198c  ;)    local.get 7
(;@198e  ;)    local.get 6
(;@1990  ;)    i32.store offset=4
(;@1993  ;)    local.get 7
(;@1995  ;)    local.set 7
(;@1997  ;)    local.get 2
(;@1999  ;)    i32.load
(;@199c  ;)    local.set 8
(;@199e  ;)    local.get 8
(;@19a0  ;)    local.get 1
(;@19a2  ;)    local.get 7
(;@19a4  ;)    call $__call_2
(;@19a6  ;)    return
             )
(;@19a9  ;)  (func $f_lam_71 (;77;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@19aa  ;)    (local i32 i32 i32)
(;@19ac  ;)    local.get 0
(;@19ae  ;)    i32.load
(;@19b1  ;)    local.set 3
(;@19b3  ;)    i32.const 8
(;@19b5  ;)    call $alloc
(;@19b7  ;)    local.set 4
(;@19b9  ;)    local.get 4
(;@19bb  ;)    local.get 1
(;@19bd  ;)    i32.store
(;@19c0  ;)    local.get 4
(;@19c2  ;)    local.get 3
(;@19c4  ;)    i32.store offset=4
(;@19c7  ;)    local.get 4
(;@19c9  ;)    local.set 4
(;@19cb  ;)    i32.const 8
(;@19cd  ;)    call $alloc
(;@19cf  ;)    local.set 5
(;@19d1  ;)    local.get 5
(;@19d3  ;)    i32.const 0
(;@19d5  ;)    i32.store
(;@19d8  ;)    local.get 5
(;@19da  ;)    local.get 4
(;@19dc  ;)    i32.store offset=4
(;@19df  ;)    local.get 5
(;@19e1  ;)    return
             )
(;@19e4  ;)  (func $f_lam_72 (;78;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@19e5  ;)    (local i32 i32)
(;@19e7  ;)    i32.const 16
(;@19e9  ;)    call $alloc
(;@19eb  ;)    local.set 3
(;@19ed  ;)    local.get 3
(;@19ef  ;)    i32.const 2
(;@19f1  ;)    i32.store
(;@19f4  ;)    local.get 3
(;@19f6  ;)    i32.const 1
(;@19f8  ;)    i32.store offset=4
(;@19fb  ;)    local.get 3
(;@19fd  ;)    i32.const 77
(;@1a00  ;)    i32.store offset=8
(;@1a03  ;)    local.get 3
(;@1a05  ;)    local.get 1
(;@1a07  ;)    i32.store offset=12
(;@1a0a  ;)    local.get 3
(;@1a0c  ;)    local.set 3
(;@1a0e  ;)    i32.const 8
(;@1a10  ;)    call $alloc
(;@1a12  ;)    local.set 4
(;@1a14  ;)    local.get 4
(;@1a16  ;)    i32.const 0
(;@1a18  ;)    i32.store
(;@1a1b  ;)    local.get 4
(;@1a1d  ;)    local.get 3
(;@1a1f  ;)    i32.store offset=4
(;@1a22  ;)    local.get 4
(;@1a24  ;)    return
             )
(;@1a27  ;)  (func $f_lam_73 (;79;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1a28  ;)    (local i32 i32)
(;@1a2a  ;)    local.get 0
(;@1a2c  ;)    i32.load
(;@1a2f  ;)    local.set 2
(;@1a31  ;)    i32.const 8
(;@1a33  ;)    call $alloc
(;@1a35  ;)    local.set 3
(;@1a37  ;)    local.get 3
(;@1a39  ;)    i32.const 0
(;@1a3b  ;)    i32.store
(;@1a3e  ;)    local.get 3
(;@1a40  ;)    local.get 2
(;@1a42  ;)    i32.store offset=4
(;@1a45  ;)    local.get 3
(;@1a47  ;)    return
             )
(;@1a4b  ;)  (func $f_lam_74 (;80;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1a4c  ;)    (local i32 i32 i32 i32 i32 i32)
(;@1a4e  ;)    local.get 0
(;@1a50  ;)    i32.load
(;@1a53  ;)    local.set 3
(;@1a55  ;)    local.get 0
(;@1a57  ;)    i32.load offset=4
(;@1a5a  ;)    local.set 4
(;@1a5c  ;)    i32.const 20
(;@1a5e  ;)    call $alloc
(;@1a60  ;)    local.set 5
(;@1a62  ;)    local.get 5
(;@1a64  ;)    i32.const 1
(;@1a66  ;)    i32.store
(;@1a69  ;)    local.get 5
(;@1a6b  ;)    i32.const 2
(;@1a6d  ;)    i32.store offset=4
(;@1a70  ;)    local.get 5
(;@1a72  ;)    i32.const 76
(;@1a75  ;)    i32.store offset=8
(;@1a78  ;)    local.get 5
(;@1a7a  ;)    local.get 3
(;@1a7c  ;)    i32.store offset=12
(;@1a7f  ;)    local.get 5
(;@1a81  ;)    local.get 4
(;@1a83  ;)    i32.store offset=16
(;@1a86  ;)    local.get 5
(;@1a88  ;)    local.set 5
(;@1a8a  ;)    i32.const 12
(;@1a8c  ;)    call $alloc
(;@1a8e  ;)    local.set 6
(;@1a90  ;)    local.get 6
(;@1a92  ;)    i32.const 2
(;@1a94  ;)    i32.store
(;@1a97  ;)    local.get 6
(;@1a99  ;)    i32.const 0
(;@1a9b  ;)    i32.store offset=4
(;@1a9e  ;)    local.get 6
(;@1aa0  ;)    i32.const 78
(;@1aa3  ;)    i32.store offset=8
(;@1aa6  ;)    local.get 6
(;@1aa8  ;)    local.set 6
(;@1aaa  ;)    i32.const 16
(;@1aac  ;)    call $alloc
(;@1aae  ;)    local.set 7
(;@1ab0  ;)    local.get 7
(;@1ab2  ;)    i32.const 1
(;@1ab4  ;)    i32.store
(;@1ab7  ;)    local.get 7
(;@1ab9  ;)    i32.const 1
(;@1abb  ;)    i32.store offset=4
(;@1abe  ;)    local.get 7
(;@1ac0  ;)    i32.const 79
(;@1ac3  ;)    i32.store offset=8
(;@1ac6  ;)    local.get 7
(;@1ac8  ;)    local.get 1
(;@1aca  ;)    i32.store offset=12
(;@1acd  ;)    local.get 7
(;@1acf  ;)    local.set 7
(;@1ad1  ;)    local.get 4
(;@1ad3  ;)    local.get 5
(;@1ad5  ;)    local.get 6
(;@1ad7  ;)    local.get 7
(;@1ad9  ;)    local.get 2
(;@1adb  ;)    call $__mon_prompt
(;@1add  ;)    return
             )
(;@1ae0  ;)  (func $f_lam_75 (;81;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1ae1  ;)    (local i32 i32 i32 i32)
(;@1ae3  ;)    local.get 0
(;@1ae5  ;)    i32.load
(;@1ae8  ;)    local.set 3
(;@1aea  ;)    local.get 0
(;@1aec  ;)    i32.load offset=4
(;@1aef  ;)    local.set 4
(;@1af1  ;)    i32.const 20
(;@1af3  ;)    call $alloc
(;@1af5  ;)    local.set 5
(;@1af7  ;)    local.get 5
(;@1af9  ;)    i32.const 2
(;@1afb  ;)    i32.store
(;@1afe  ;)    local.get 5
(;@1b00  ;)    i32.const 2
(;@1b02  ;)    i32.store offset=4
(;@1b05  ;)    local.get 5
(;@1b07  ;)    i32.const 80
(;@1b0a  ;)    i32.store offset=8
(;@1b0d  ;)    local.get 5
(;@1b0f  ;)    local.get 3
(;@1b11  ;)    i32.store offset=12
(;@1b14  ;)    local.get 5
(;@1b16  ;)    local.get 4
(;@1b18  ;)    i32.store offset=16
(;@1b1b  ;)    local.get 5
(;@1b1d  ;)    local.set 5
(;@1b1f  ;)    local.get 1
(;@1b21  ;)    local.get 5
(;@1b23  ;)    local.get 2
(;@1b25  ;)    call $__call_2
(;@1b27  ;)    return
             )
(;@1b2a  ;)  (func $f_lam_76 (;82;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1b2b  ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@1b2d  ;)    local.get 0
(;@1b2f  ;)    i32.load
(;@1b32  ;)    local.set 3
(;@1b34  ;)    local.get 0
(;@1b36  ;)    i32.load offset=4
(;@1b39  ;)    local.set 4
(;@1b3b  ;)    local.get 0
(;@1b3d  ;)    i32.load offset=8
(;@1b40  ;)    local.set 5
(;@1b42  ;)    local.get 5
(;@1b44  ;)    i32.load offset=8
(;@1b47  ;)    local.set 6
(;@1b49  ;)    local.get 6
(;@1b4b  ;)    local.get 1
(;@1b4d  ;)    call $__call_1
(;@1b4f  ;)    local.set 7
(;@1b51  ;)    i32.const 20
(;@1b53  ;)    call $alloc
(;@1b55  ;)    local.set 8
(;@1b57  ;)    local.get 8
(;@1b59  ;)    i32.const 2
(;@1b5b  ;)    i32.store
(;@1b5e  ;)    local.get 8
(;@1b60  ;)    i32.const 2
(;@1b62  ;)    i32.store offset=4
(;@1b65  ;)    local.get 8
(;@1b67  ;)    i32.const 81
(;@1b6a  ;)    i32.store offset=8
(;@1b6d  ;)    local.get 8
(;@1b6f  ;)    local.get 3
(;@1b71  ;)    i32.store offset=12
(;@1b74  ;)    local.get 8
(;@1b76  ;)    local.get 4
(;@1b78  ;)    i32.store offset=16
(;@1b7b  ;)    local.get 8
(;@1b7d  ;)    local.set 8
(;@1b7f  ;)    local.get 7
(;@1b81  ;)    local.get 8
(;@1b83  ;)    local.get 2
(;@1b85  ;)    call $__mon_bind
(;@1b87  ;)    return
             )
(;@1b8b  ;)  (func $f_lam_77 (;83;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1b8c  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@1b8e  ;)    local.get 0
(;@1b90  ;)    i32.load
(;@1b93  ;)    local.set 2
(;@1b95  ;)    local.get 0
(;@1b97  ;)    i32.load offset=4
(;@1b9a  ;)    local.set 3
(;@1b9c  ;)    i32.const 12
(;@1b9e  ;)    call $alloc
(;@1ba0  ;)    local.set 4
(;@1ba2  ;)    local.get 4
(;@1ba4  ;)    i32.const 2
(;@1ba6  ;)    i32.store
(;@1ba9  ;)    local.get 4
(;@1bab  ;)    i32.const 0
(;@1bad  ;)    i32.store offset=4
(;@1bb0  ;)    local.get 4
(;@1bb2  ;)    i32.const 12
(;@1bb4  ;)    i32.store offset=8
(;@1bb7  ;)    local.get 4
(;@1bb9  ;)    local.set 4
(;@1bbb  ;)    i32.const 12
(;@1bbd  ;)    call $alloc
(;@1bbf  ;)    local.set 5
(;@1bc1  ;)    local.get 5
(;@1bc3  ;)    i32.const 2
(;@1bc5  ;)    i32.store
(;@1bc8  ;)    local.get 5
(;@1bca  ;)    i32.const 0
(;@1bcc  ;)    i32.store offset=4
(;@1bcf  ;)    local.get 5
(;@1bd1  ;)    i32.const 19
(;@1bd3  ;)    i32.store offset=8
(;@1bd6  ;)    local.get 5
(;@1bd8  ;)    local.set 5
(;@1bda  ;)    i32.const 8
(;@1bdc  ;)    call $alloc
(;@1bde  ;)    local.set 6
(;@1be0  ;)    local.get 6
(;@1be2  ;)    local.get 4
(;@1be4  ;)    i32.store
(;@1be7  ;)    local.get 6
(;@1be9  ;)    local.get 5
(;@1beb  ;)    i32.store offset=4
(;@1bee  ;)    local.get 6
(;@1bf0  ;)    local.set 6
(;@1bf2  ;)    i32.const 8
(;@1bf4  ;)    call $alloc
(;@1bf6  ;)    local.set 7
(;@1bf8  ;)    local.get 7
(;@1bfa  ;)    local.get 3
(;@1bfc  ;)    i32.store
(;@1bff  ;)    local.get 7
(;@1c01  ;)    local.get 6
(;@1c03  ;)    i32.store offset=4
(;@1c06  ;)    local.get 7
(;@1c08  ;)    local.set 7
(;@1c0a  ;)    local.get 2
(;@1c0c  ;)    i32.load
(;@1c0f  ;)    local.set 8
(;@1c11  ;)    local.get 8
(;@1c13  ;)    local.get 1
(;@1c15  ;)    local.get 7
(;@1c17  ;)    call $__call_2
(;@1c19  ;)    local.set 9
(;@1c1b  ;)    local.get 2
(;@1c1d  ;)    i32.load offset=12
(;@1c20  ;)    local.set 10
(;@1c22  ;)    local.get 10
(;@1c24  ;)    i32.load
(;@1c27  ;)    local.set 11
(;@1c29  ;)    local.get 11
(;@1c2b  ;)    local.get 9
(;@1c2d  ;)    call $__call_1
(;@1c2f  ;)    local.set 12
(;@1c31  ;)    local.get 12
(;@1c33  ;)    i32.load
(;@1c36  ;)    local.set 13
(;@1c38  ;)    local.get 3
(;@1c3a  ;)    local.get 13
(;@1c3c  ;)    call $__mon_eqm
(;@1c3e  ;)    local.set 14
(;@1c40  ;)    local.get 14
(;@1c42  ;)    i32.load
(;@1c45  ;)    local.set 15
(;@1c47  ;)    block (result i32) ;; label = @1
(;@1c49  ;)      block ;; label = @2
(;@1c4b  ;)        block ;; label = @3
(;@1c4d  ;)          block ;; label = @4
(;@1c4f  ;)            local.get 15
(;@1c51  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1c56  ;)          end
(;@1c57  ;)          local.get 14
(;@1c59  ;)          i32.load offset=4
(;@1c5c  ;)          local.set 16
(;@1c5e  ;)          local.get 12
(;@1c60  ;)          i32.load
(;@1c63  ;)          local.set 17
(;@1c65  ;)          i32.const 16
(;@1c67  ;)          call $alloc
(;@1c69  ;)          local.set 18
(;@1c6b  ;)          local.get 18
(;@1c6d  ;)          i32.const 1
(;@1c6f  ;)          i32.store
(;@1c72  ;)          local.get 18
(;@1c74  ;)          i32.const 1
(;@1c76  ;)          i32.store offset=4
(;@1c79  ;)          local.get 18
(;@1c7b  ;)          i32.const 23
(;@1c7d  ;)          i32.store offset=8
(;@1c80  ;)          local.get 18
(;@1c82  ;)          local.get 12
(;@1c84  ;)          i32.store offset=12
(;@1c87  ;)          local.get 18
(;@1c89  ;)          local.set 18
(;@1c8b  ;)          i32.const 20
(;@1c8d  ;)          call $alloc
(;@1c8f  ;)          local.set 19
(;@1c91  ;)          local.get 19
(;@1c93  ;)          i32.const 2
(;@1c95  ;)          i32.store
(;@1c98  ;)          local.get 19
(;@1c9a  ;)          i32.const 2
(;@1c9c  ;)          i32.store offset=4
(;@1c9f  ;)          local.get 19
(;@1ca1  ;)          i32.const 42
(;@1ca3  ;)          i32.store offset=8
(;@1ca6  ;)          local.get 19
(;@1ca8  ;)          local.get 2
(;@1caa  ;)          i32.store offset=12
(;@1cad  ;)          local.get 19
(;@1caf  ;)          local.get 3
(;@1cb1  ;)          i32.store offset=16
(;@1cb4  ;)          local.get 19
(;@1cb6  ;)          local.set 19
(;@1cb8  ;)          i32.const 12
(;@1cba  ;)          call $alloc
(;@1cbc  ;)          local.set 20
(;@1cbe  ;)          local.get 20
(;@1cc0  ;)          local.get 17
(;@1cc2  ;)          i32.store
(;@1cc5  ;)          local.get 20
(;@1cc7  ;)          local.get 18
(;@1cc9  ;)          i32.store offset=4
(;@1ccc  ;)          local.get 20
(;@1cce  ;)          local.get 19
(;@1cd0  ;)          i32.store offset=8
(;@1cd3  ;)          local.get 20
(;@1cd5  ;)          local.set 20
(;@1cd7  ;)          i32.const 8
(;@1cd9  ;)          call $alloc
(;@1cdb  ;)          local.set 21
(;@1cdd  ;)          local.get 21
(;@1cdf  ;)          i32.const 1
(;@1ce1  ;)          i32.store
(;@1ce4  ;)          local.get 21
(;@1ce6  ;)          local.get 20
(;@1ce8  ;)          i32.store offset=4
(;@1ceb  ;)          local.get 21
(;@1ced  ;)          br 2 (;@1;)
(;@1cef  ;)        end
(;@1cf0  ;)        local.get 14
(;@1cf2  ;)        i32.load offset=4
(;@1cf5  ;)        local.set 21
(;@1cf7  ;)        i32.const 0
(;@1cf9  ;)        call $alloc
(;@1cfb  ;)        local.set 22
(;@1cfd  ;)        local.get 22
(;@1cff  ;)        local.set 22
(;@1d01  ;)        local.get 12
(;@1d03  ;)        i32.load offset=4
(;@1d06  ;)        local.set 23
(;@1d08  ;)        local.get 23
(;@1d0a  ;)        i32.load
(;@1d0d  ;)        local.set 24
(;@1d0f  ;)        local.get 24
(;@1d11  ;)        local.get 22
(;@1d13  ;)        local.get 1
(;@1d15  ;)        call $__call_2
(;@1d17  ;)        local.set 25
(;@1d19  ;)        local.get 25
(;@1d1b  ;)        i32.load
(;@1d1e  ;)        local.set 26
(;@1d20  ;)        block (result i32) ;; label = @3
(;@1d22  ;)          block ;; label = @4
(;@1d24  ;)            block ;; label = @5
(;@1d26  ;)              block ;; label = @6
(;@1d28  ;)                local.get 26
(;@1d2a  ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@1d2f  ;)              end
(;@1d30  ;)              local.get 25
(;@1d32  ;)              i32.load offset=4
(;@1d35  ;)              local.set 27
(;@1d37  ;)              i32.const 20
(;@1d39  ;)              call $alloc
(;@1d3b  ;)              local.set 28
(;@1d3d  ;)              local.get 28
(;@1d3f  ;)              i32.const 2
(;@1d41  ;)              i32.store
(;@1d44  ;)              local.get 28
(;@1d46  ;)              i32.const 2
(;@1d48  ;)              i32.store offset=4
(;@1d4b  ;)              local.get 28
(;@1d4d  ;)              i32.const 61
(;@1d4f  ;)              i32.store offset=8
(;@1d52  ;)              local.get 28
(;@1d54  ;)              local.get 2
(;@1d56  ;)              i32.store offset=12
(;@1d59  ;)              local.get 28
(;@1d5b  ;)              local.get 3
(;@1d5d  ;)              i32.store offset=16
(;@1d60  ;)              local.get 28
(;@1d62  ;)              local.set 28
(;@1d64  ;)              local.get 27
(;@1d66  ;)              local.get 28
(;@1d68  ;)              local.get 1
(;@1d6a  ;)              call $__call_2
(;@1d6c  ;)              br 2 (;@3;)
(;@1d6e  ;)            end
(;@1d6f  ;)            local.get 25
(;@1d71  ;)            i32.load offset=4
(;@1d74  ;)            local.set 29
(;@1d76  ;)            local.get 29
(;@1d78  ;)            local.set 30
(;@1d7a  ;)            local.get 30
(;@1d7c  ;)            i32.load
(;@1d7f  ;)            local.set 31
(;@1d81  ;)            local.get 30
(;@1d83  ;)            i32.load offset=4
(;@1d86  ;)            local.set 32
(;@1d88  ;)            i32.const 24
(;@1d8a  ;)            call $alloc
(;@1d8c  ;)            local.set 33
(;@1d8e  ;)            local.get 33
(;@1d90  ;)            i32.const 2
(;@1d92  ;)            i32.store
(;@1d95  ;)            local.get 33
(;@1d97  ;)            i32.const 3
(;@1d99  ;)            i32.store offset=4
(;@1d9c  ;)            local.get 33
(;@1d9e  ;)            i32.const 82
(;@1da1  ;)            i32.store offset=8
(;@1da4  ;)            local.get 33
(;@1da6  ;)            local.get 2
(;@1da8  ;)            i32.store offset=12
(;@1dab  ;)            local.get 33
(;@1dad  ;)            local.get 3
(;@1daf  ;)            i32.store offset=16
(;@1db2  ;)            local.get 33
(;@1db4  ;)            local.get 30
(;@1db6  ;)            i32.store offset=20
(;@1db9  ;)            local.get 33
(;@1dbb  ;)            local.set 33
(;@1dbd  ;)            i32.const 12
(;@1dbf  ;)            call $alloc
(;@1dc1  ;)            local.set 34
(;@1dc3  ;)            local.get 34
(;@1dc5  ;)            local.get 31
(;@1dc7  ;)            i32.store
(;@1dca  ;)            local.get 34
(;@1dcc  ;)            local.get 32
(;@1dce  ;)            i32.store offset=4
(;@1dd1  ;)            local.get 34
(;@1dd3  ;)            local.get 33
(;@1dd5  ;)            i32.store offset=8
(;@1dd8  ;)            local.get 34
(;@1dda  ;)            local.set 34
(;@1ddc  ;)            i32.const 8
(;@1dde  ;)            call $alloc
(;@1de0  ;)            local.set 35
(;@1de2  ;)            local.get 35
(;@1de4  ;)            i32.const 1
(;@1de6  ;)            i32.store
(;@1de9  ;)            local.get 35
(;@1deb  ;)            local.get 34
(;@1ded  ;)            i32.store offset=4
(;@1df0  ;)            local.get 35
(;@1df2  ;)            br 1 (;@3;)
(;@1df4  ;)          end
(;@1df5  ;)          unreachable
(;@1df6  ;)        end
(;@1df7  ;)        br 1 (;@1;)
(;@1df9  ;)      end
(;@1dfa  ;)      unreachable
(;@1dfb  ;)    end
(;@1dfc  ;)    return
             )
(;@1dff  ;)  (func $f_lam_78 (;84;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1e00  ;)    (local i32 i32)
(;@1e02  ;)    local.get 0
(;@1e04  ;)    i32.load
(;@1e07  ;)    local.set 3
(;@1e09  ;)    local.get 1
(;@1e0b  ;)    local.get 3
(;@1e0d  ;)    local.get 2
(;@1e0f  ;)    call $__call_2
(;@1e11  ;)    return
             )
(;@1e14  ;)  (func $f_lam_79 (;85;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@1e15  ;)    (local i32 i32 i32 i32 i32 i32)
(;@1e17  ;)    local.get 0
(;@1e19  ;)    i32.load
(;@1e1c  ;)    local.set 3
(;@1e1e  ;)    local.get 0
(;@1e20  ;)    i32.load offset=4
(;@1e23  ;)    local.set 4
(;@1e25  ;)    local.get 4
(;@1e27  ;)    i32.load offset=8
(;@1e2a  ;)    local.set 5
(;@1e2c  ;)    local.get 5
(;@1e2e  ;)    local.get 1
(;@1e30  ;)    call $__call_1
(;@1e32  ;)    local.set 6
(;@1e34  ;)    i32.const 16
(;@1e36  ;)    call $alloc
(;@1e38  ;)    local.set 7
(;@1e3a  ;)    local.get 7
(;@1e3c  ;)    i32.const 2
(;@1e3e  ;)    i32.store
(;@1e41  ;)    local.get 7
(;@1e43  ;)    i32.const 1
(;@1e45  ;)    i32.store offset=4
(;@1e48  ;)    local.get 7
(;@1e4a  ;)    i32.const 84
(;@1e4d  ;)    i32.store offset=8
(;@1e50  ;)    local.get 7
(;@1e52  ;)    local.get 3
(;@1e54  ;)    i32.store offset=12
(;@1e57  ;)    local.get 7
(;@1e59  ;)    local.set 7
(;@1e5b  ;)    local.get 6
(;@1e5d  ;)    local.get 7
(;@1e5f  ;)    local.get 2
(;@1e61  ;)    call $__mon_bind
(;@1e63  ;)    return
             )
(;@1e67  ;)  (func $f_lam_80 (;86;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1e68  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@1e6a  ;)    local.get 0
(;@1e6c  ;)    i32.load
(;@1e6f  ;)    local.set 2
(;@1e71  ;)    local.get 0
(;@1e73  ;)    i32.load offset=4
(;@1e76  ;)    local.set 3
(;@1e78  ;)    local.get 3
(;@1e7a  ;)    local.get 1
(;@1e7c  ;)    call $__call_1
(;@1e7e  ;)    local.set 4
(;@1e80  ;)    local.get 4
(;@1e82  ;)    i32.load
(;@1e85  ;)    local.set 5
(;@1e87  ;)    block (result i32) ;; label = @1
(;@1e89  ;)      block ;; label = @2
(;@1e8b  ;)        block ;; label = @3
(;@1e8d  ;)          block ;; label = @4
(;@1e8f  ;)            local.get 5
(;@1e91  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1e96  ;)          end
(;@1e97  ;)          local.get 4
(;@1e99  ;)          i32.load offset=4
(;@1e9c  ;)          local.set 6
(;@1e9e  ;)          local.get 6
(;@1ea0  ;)          local.get 2
(;@1ea2  ;)          local.get 1
(;@1ea4  ;)          call $__call_2
(;@1ea6  ;)          br 2 (;@1;)
(;@1ea8  ;)        end
(;@1ea9  ;)        local.get 4
(;@1eab  ;)        i32.load offset=4
(;@1eae  ;)        local.set 7
(;@1eb0  ;)        local.get 7
(;@1eb2  ;)        local.set 8
(;@1eb4  ;)        local.get 8
(;@1eb6  ;)        i32.load
(;@1eb9  ;)        local.set 9
(;@1ebb  ;)        local.get 8
(;@1ebd  ;)        i32.load offset=4
(;@1ec0  ;)        local.set 10
(;@1ec2  ;)        i32.const 20
(;@1ec4  ;)        call $alloc
(;@1ec6  ;)        local.set 11
(;@1ec8  ;)        local.get 11
(;@1eca  ;)        i32.const 2
(;@1ecc  ;)        i32.store
(;@1ecf  ;)        local.get 11
(;@1ed1  ;)        i32.const 2
(;@1ed3  ;)        i32.store offset=4
(;@1ed6  ;)        local.get 11
(;@1ed8  ;)        i32.const 85
(;@1edb  ;)        i32.store offset=8
(;@1ede  ;)        local.get 11
(;@1ee0  ;)        local.get 2
(;@1ee2  ;)        i32.store offset=12
(;@1ee5  ;)        local.get 11
(;@1ee7  ;)        local.get 8
(;@1ee9  ;)        i32.store offset=16
(;@1eec  ;)        local.get 11
(;@1eee  ;)        local.set 11
(;@1ef0  ;)        i32.const 12
(;@1ef2  ;)        call $alloc
(;@1ef4  ;)        local.set 12
(;@1ef6  ;)        local.get 12
(;@1ef8  ;)        local.get 9
(;@1efa  ;)        i32.store
(;@1efd  ;)        local.get 12
(;@1eff  ;)        local.get 10
(;@1f01  ;)        i32.store offset=4
(;@1f04  ;)        local.get 12
(;@1f06  ;)        local.get 11
(;@1f08  ;)        i32.store offset=8
(;@1f0b  ;)        local.get 12
(;@1f0d  ;)        local.set 12
(;@1f0f  ;)        i32.const 8
(;@1f11  ;)        call $alloc
(;@1f13  ;)        local.set 13
(;@1f15  ;)        local.get 13
(;@1f17  ;)        i32.const 1
(;@1f19  ;)        i32.store
(;@1f1c  ;)        local.get 13
(;@1f1e  ;)        local.get 12
(;@1f20  ;)        i32.store offset=4
(;@1f23  ;)        local.get 13
(;@1f25  ;)        br 1 (;@1;)
(;@1f27  ;)      end
(;@1f28  ;)      unreachable
(;@1f29  ;)    end
(;@1f2a  ;)    return
             )
(;@1f2d  ;)  (func $f_lam_81 (;87;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1f2e  ;)    (local i32 i32 i32 i32 i32)
(;@1f30  ;)    local.get 0
(;@1f32  ;)    i32.load
(;@1f35  ;)    local.set 2
(;@1f37  ;)    i32.const 0
(;@1f39  ;)    call $alloc
(;@1f3b  ;)    local.set 3
(;@1f3d  ;)    local.get 3
(;@1f3f  ;)    local.set 3
(;@1f41  ;)    local.get 3
(;@1f43  ;)    call $__mon_generate_marker
(;@1f45  ;)    local.set 4
(;@1f47  ;)    i32.const 20
(;@1f49  ;)    call $alloc
(;@1f4b  ;)    local.set 5
(;@1f4d  ;)    local.get 5
(;@1f4f  ;)    i32.const 1
(;@1f51  ;)    i32.store
(;@1f54  ;)    local.get 5
(;@1f56  ;)    i32.const 2
(;@1f58  ;)    i32.store offset=4
(;@1f5b  ;)    local.get 5
(;@1f5d  ;)    i32.const 83
(;@1f60  ;)    i32.store offset=8
(;@1f63  ;)    local.get 5
(;@1f65  ;)    local.get 2
(;@1f67  ;)    i32.store offset=12
(;@1f6a  ;)    local.get 5
(;@1f6c  ;)    local.get 4
(;@1f6e  ;)    i32.store offset=16
(;@1f71  ;)    local.get 5
(;@1f73  ;)    local.set 5
(;@1f75  ;)    i32.const 20
(;@1f77  ;)    call $alloc
(;@1f79  ;)    local.set 6
(;@1f7b  ;)    local.get 6
(;@1f7d  ;)    i32.const 1
(;@1f7f  ;)    i32.store
(;@1f82  ;)    local.get 6
(;@1f84  ;)    i32.const 2
(;@1f86  ;)    i32.store offset=4
(;@1f89  ;)    local.get 6
(;@1f8b  ;)    i32.const 86
(;@1f8e  ;)    i32.store offset=8
(;@1f91  ;)    local.get 6
(;@1f93  ;)    local.get 1
(;@1f95  ;)    i32.store offset=12
(;@1f98  ;)    local.get 6
(;@1f9a  ;)    local.get 5
(;@1f9c  ;)    i32.store offset=16
(;@1f9f  ;)    local.get 6
(;@1fa1  ;)    return
             )
(;@1fa5  ;)  (func $main (;88;) (type $fun_0_1) (result i32)
(;@1fa6  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@1fa8  ;)    i32.const 12
(;@1faa  ;)    call $alloc
(;@1fac  ;)    local.set 0
(;@1fae  ;)    local.get 0
(;@1fb0  ;)    i32.const 2
(;@1fb2  ;)    i32.store
(;@1fb5  ;)    local.get 0
(;@1fb7  ;)    i32.const 0
(;@1fb9  ;)    i32.store offset=4
(;@1fbc  ;)    local.get 0
(;@1fbe  ;)    i32.const 89
(;@1fc1  ;)    i32.store offset=8
(;@1fc4  ;)    local.get 0
(;@1fc6  ;)    local.set 0
(;@1fc8  ;)    i32.const 12
(;@1fca  ;)    call $alloc
(;@1fcc  ;)    local.set 1
(;@1fce  ;)    local.get 1
(;@1fd0  ;)    i32.const 3
(;@1fd2  ;)    i32.store
(;@1fd5  ;)    local.get 1
(;@1fd7  ;)    i32.const 0
(;@1fd9  ;)    i32.store offset=4
(;@1fdc  ;)    local.get 1
(;@1fde  ;)    i32.const 90
(;@1fe1  ;)    i32.store offset=8
(;@1fe4  ;)    local.get 1
(;@1fe6  ;)    local.set 1
(;@1fe8  ;)    i32.const 12
(;@1fea  ;)    call $alloc
(;@1fec  ;)    local.set 2
(;@1fee  ;)    local.get 2
(;@1ff0  ;)    i32.const 1
(;@1ff2  ;)    i32.store
(;@1ff5  ;)    local.get 2
(;@1ff7  ;)    i32.const 0
(;@1ff9  ;)    i32.store offset=4
(;@1ffc  ;)    local.get 2
(;@1ffe  ;)    i32.const 91
(;@2001  ;)    i32.store offset=8
(;@2004  ;)    local.get 2
(;@2006  ;)    local.set 2
(;@2008  ;)    i32.const 12
(;@200a  ;)    call $alloc
(;@200c  ;)    local.set 3
(;@200e  ;)    local.get 3
(;@2010  ;)    i32.const 1
(;@2012  ;)    i32.store
(;@2015  ;)    local.get 3
(;@2017  ;)    i32.const 0
(;@2019  ;)    i32.store offset=4
(;@201c  ;)    local.get 3
(;@201e  ;)    i32.const 92
(;@2021  ;)    i32.store offset=8
(;@2024  ;)    local.get 3
(;@2026  ;)    local.set 3
(;@2028  ;)    i32.const 8
(;@202a  ;)    call $alloc
(;@202c  ;)    local.set 4
(;@202e  ;)    local.get 4
(;@2030  ;)    local.get 2
(;@2032  ;)    i32.store
(;@2035  ;)    local.get 4
(;@2037  ;)    local.get 3
(;@2039  ;)    i32.store offset=4
(;@203c  ;)    local.get 4
(;@203e  ;)    local.set 4
(;@2040  ;)    i32.const 12
(;@2042  ;)    call $alloc
(;@2044  ;)    local.set 5
(;@2046  ;)    local.get 5
(;@2048  ;)    i32.const 1
(;@204a  ;)    i32.store
(;@204d  ;)    local.get 5
(;@204f  ;)    i32.const 0
(;@2051  ;)    i32.store offset=4
(;@2054  ;)    local.get 5
(;@2056  ;)    i32.const 93
(;@2059  ;)    i32.store offset=8
(;@205c  ;)    local.get 5
(;@205e  ;)    local.set 5
(;@2060  ;)    i32.const 12
(;@2062  ;)    call $alloc
(;@2064  ;)    local.set 6
(;@2066  ;)    local.get 6
(;@2068  ;)    i32.const 1
(;@206a  ;)    i32.store
(;@206d  ;)    local.get 6
(;@206f  ;)    i32.const 0
(;@2071  ;)    i32.store offset=4
(;@2074  ;)    local.get 6
(;@2076  ;)    i32.const 94
(;@2079  ;)    i32.store offset=8
(;@207c  ;)    local.get 6
(;@207e  ;)    local.set 6
(;@2080  ;)    i32.const 8
(;@2082  ;)    call $alloc
(;@2084  ;)    local.set 7
(;@2086  ;)    local.get 7
(;@2088  ;)    local.get 5
(;@208a  ;)    i32.store
(;@208d  ;)    local.get 7
(;@208f  ;)    local.get 6
(;@2091  ;)    i32.store offset=4
(;@2094  ;)    local.get 7
(;@2096  ;)    local.set 7
(;@2098  ;)    i32.const 16
(;@209a  ;)    call $alloc
(;@209c  ;)    local.set 8
(;@209e  ;)    local.get 8
(;@20a0  ;)    local.get 0
(;@20a2  ;)    i32.store
(;@20a5  ;)    local.get 8
(;@20a7  ;)    local.get 1
(;@20a9  ;)    i32.store offset=4
(;@20ac  ;)    local.get 8
(;@20ae  ;)    local.get 4
(;@20b0  ;)    i32.store offset=8
(;@20b3  ;)    local.get 8
(;@20b5  ;)    local.get 7
(;@20b7  ;)    i32.store offset=12
(;@20ba  ;)    local.get 8
(;@20bc  ;)    local.set 8
(;@20be  ;)    i32.const 0
(;@20c0  ;)    call $alloc
(;@20c2  ;)    local.set 9
(;@20c4  ;)    local.get 9
(;@20c6  ;)    local.set 9
(;@20c8  ;)    local.get 8
(;@20ca  ;)    local.get 9
(;@20cc  ;)    call $f
(;@20ce  ;)    local.set 10
(;@20d0  ;)    local.get 10
(;@20d2  ;)    i32.load
(;@20d5  ;)    local.set 11
(;@20d7  ;)    block (result i32) ;; label = @1
(;@20d9  ;)      block ;; label = @2
(;@20db  ;)        block ;; label = @3
(;@20dd  ;)          block ;; label = @4
(;@20df  ;)            local.get 11
(;@20e1  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@20e6  ;)          end
(;@20e7  ;)          local.get 10
(;@20e9  ;)          i32.load offset=4
(;@20ec  ;)          local.set 12
(;@20ee  ;)          i32.const 0
(;@20f0  ;)          call $alloc
(;@20f2  ;)          local.set 13
(;@20f4  ;)          local.get 13
(;@20f6  ;)          local.set 13
(;@20f8  ;)          local.get 12
(;@20fa  ;)          i32.const 16777215
(;@20ff  ;)          local.get 13
(;@2101  ;)          call $__call_2
(;@2103  ;)          br 2 (;@1;)
(;@2105  ;)        end
(;@2106  ;)        local.get 10
(;@2108  ;)        i32.load offset=4
(;@210b  ;)        local.set 14
(;@210d  ;)        local.get 14
(;@210f  ;)        local.set 15
(;@2111  ;)        local.get 15
(;@2113  ;)        i32.load
(;@2116  ;)        local.set 16
(;@2118  ;)        local.get 15
(;@211a  ;)        i32.load offset=4
(;@211d  ;)        local.set 17
(;@211f  ;)        i32.const 16
(;@2121  ;)        call $alloc
(;@2123  ;)        local.set 18
(;@2125  ;)        local.get 18
(;@2127  ;)        i32.const 2
(;@2129  ;)        i32.store
(;@212c  ;)        local.get 18
(;@212e  ;)        i32.const 1
(;@2130  ;)        i32.store offset=4
(;@2133  ;)        local.get 18
(;@2135  ;)        i32.const 96
(;@2138  ;)        i32.store offset=8
(;@213b  ;)        local.get 18
(;@213d  ;)        local.get 15
(;@213f  ;)        i32.store offset=12
(;@2142  ;)        local.get 18
(;@2144  ;)        local.set 18
(;@2146  ;)        i32.const 12
(;@2148  ;)        call $alloc
(;@214a  ;)        local.set 19
(;@214c  ;)        local.get 19
(;@214e  ;)        local.get 16
(;@2150  ;)        i32.store
(;@2153  ;)        local.get 19
(;@2155  ;)        local.get 17
(;@2157  ;)        i32.store offset=4
(;@215a  ;)        local.get 19
(;@215c  ;)        local.get 18
(;@215e  ;)        i32.store offset=8
(;@2161  ;)        local.get 19
(;@2163  ;)        local.set 19
(;@2165  ;)        i32.const 8
(;@2167  ;)        call $alloc
(;@2169  ;)        local.set 20
(;@216b  ;)        local.get 20
(;@216d  ;)        i32.const 1
(;@216f  ;)        i32.store
(;@2172  ;)        local.get 20
(;@2174  ;)        local.get 19
(;@2176  ;)        i32.store offset=4
(;@2179  ;)        local.get 20
(;@217b  ;)        br 1 (;@1;)
(;@217d  ;)      end
(;@217e  ;)      unreachable
(;@217f  ;)    end
(;@2180  ;)    local.set 20
(;@2182  ;)    local.get 20
(;@2184  ;)    i32.load
(;@2187  ;)    local.set 21
(;@2189  ;)    block (result i32) ;; label = @1
(;@218b  ;)      block ;; label = @2
(;@218d  ;)        block ;; label = @3
(;@218f  ;)          block ;; label = @4
(;@2191  ;)            local.get 21
(;@2193  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2198  ;)          end
(;@2199  ;)          local.get 20
(;@219b  ;)          i32.load offset=4
(;@219e  ;)          local.set 22
(;@21a0  ;)          local.get 22
(;@21a2  ;)          i32.load offset=4
(;@21a5  ;)          local.set 23
(;@21a7  ;)          i32.const 8
(;@21a9  ;)          call $alloc
(;@21ab  ;)          local.set 24
(;@21ad  ;)          local.get 24
(;@21af  ;)          i32.const 0
(;@21b1  ;)          i32.store
(;@21b4  ;)          local.get 24
(;@21b6  ;)          local.get 23
(;@21b8  ;)          i32.store offset=4
(;@21bb  ;)          local.get 24
(;@21bd  ;)          br 2 (;@1;)
(;@21bf  ;)        end
(;@21c0  ;)        local.get 20
(;@21c2  ;)        i32.load offset=4
(;@21c5  ;)        local.set 24
(;@21c7  ;)        local.get 24
(;@21c9  ;)        local.set 25
(;@21cb  ;)        local.get 25
(;@21cd  ;)        i32.load
(;@21d0  ;)        local.set 26
(;@21d2  ;)        local.get 25
(;@21d4  ;)        i32.load offset=4
(;@21d7  ;)        local.set 27
(;@21d9  ;)        i32.const 16
(;@21db  ;)        call $alloc
(;@21dd  ;)        local.set 28
(;@21df  ;)        local.get 28
(;@21e1  ;)        i32.const 2
(;@21e3  ;)        i32.store
(;@21e6  ;)        local.get 28
(;@21e8  ;)        i32.const 1
(;@21ea  ;)        i32.store offset=4
(;@21ed  ;)        local.get 28
(;@21ef  ;)        i32.const 98
(;@21f2  ;)        i32.store offset=8
(;@21f5  ;)        local.get 28
(;@21f7  ;)        local.get 25
(;@21f9  ;)        i32.store offset=12
(;@21fc  ;)        local.get 28
(;@21fe  ;)        local.set 28
(;@2200  ;)        i32.const 12
(;@2202  ;)        call $alloc
(;@2204  ;)        local.set 29
(;@2206  ;)        local.get 29
(;@2208  ;)        local.get 26
(;@220a  ;)        i32.store
(;@220d  ;)        local.get 29
(;@220f  ;)        local.get 27
(;@2211  ;)        i32.store offset=4
(;@2214  ;)        local.get 29
(;@2216  ;)        local.get 28
(;@2218  ;)        i32.store offset=8
(;@221b  ;)        local.get 29
(;@221d  ;)        local.set 29
(;@221f  ;)        i32.const 8
(;@2221  ;)        call $alloc
(;@2223  ;)        local.set 30
(;@2225  ;)        local.get 30
(;@2227  ;)        i32.const 1
(;@2229  ;)        i32.store
(;@222c  ;)        local.get 30
(;@222e  ;)        local.get 29
(;@2230  ;)        i32.store offset=4
(;@2233  ;)        local.get 30
(;@2235  ;)        br 1 (;@1;)
(;@2237  ;)      end
(;@2238  ;)      unreachable
(;@2239  ;)    end
(;@223a  ;)    local.set 30
(;@223c  ;)    local.get 30
(;@223e  ;)    i32.load
(;@2241  ;)    local.set 31
(;@2243  ;)    block (result i32) ;; label = @1
(;@2245  ;)      block ;; label = @2
(;@2247  ;)        block ;; label = @3
(;@2249  ;)          block ;; label = @4
(;@224b  ;)            local.get 31
(;@224d  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@2252  ;)          end
(;@2253  ;)          local.get 30
(;@2255  ;)          i32.load offset=4
(;@2258  ;)          local.set 32
(;@225a  ;)          local.get 32
(;@225c  ;)          br 2 (;@1;)
(;@225e  ;)        end
(;@225f  ;)        local.get 30
(;@2261  ;)        i32.load offset=4
(;@2264  ;)        local.set 32
(;@2266  ;)        i32.const 5467
(;@2269  ;)        br 1 (;@1;)
(;@226b  ;)      end
(;@226c  ;)      unreachable
(;@226d  ;)    end
(;@226e  ;)    return
             )
(;@2271  ;)  (func $main_lam_0 (;89;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@2272  ;)    (local i32)
(;@2274  ;)    local.get 2
(;@2276  ;)    return
             )
(;@2279  ;)  (func $main_lam_1 (;90;) (type $fun_4_1) (param i32 i32 i32 i32) (result i32)
(;@227a  ;)    (local i32)
(;@227c  ;)    local.get 2
(;@227e  ;)    local.get 3
(;@2280  ;)    call $__call_1
(;@2282  ;)    return
             )
(;@2285  ;)  (func $main_lam_2 (;91;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2286  ;)    (local i32)
(;@2288  ;)    i32.const 0
(;@228a  ;)    call $alloc
(;@228c  ;)    local.set 2
(;@228e  ;)    local.get 2
(;@2290  ;)    return
             )
(;@2293  ;)  (func $main_lam_3 (;92;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2294  ;)    (local i32 i32)
(;@2296  ;)    local.get 1
(;@2298  ;)    i32.load
(;@229b  ;)    local.set 2
(;@229d  ;)    block (result i32) ;; label = @1
(;@229f  ;)      block ;; label = @2
(;@22a1  ;)        local.get 2
(;@22a3  ;)        br_table 0 (;@2;)
(;@22a6  ;)      end
(;@22a7  ;)      unreachable
(;@22a8  ;)    end
(;@22a9  ;)    return
             )
(;@22ac  ;)  (func $main_lam_4 (;93;) (type $fun_2_1) (param i32 i32) (result i32)
(;@22ad  ;)    (local i32)
(;@22af  ;)    local.get 1
(;@22b1  ;)    return
             )
(;@22b4  ;)  (func $main_lam_5 (;94;) (type $fun_2_1) (param i32 i32) (result i32)
(;@22b5  ;)    (local i32)
(;@22b7  ;)    local.get 1
(;@22b9  ;)    return
             )
(;@22bc  ;)  (func $main_lam_6 (;95;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@22bd  ;)    (local i32)
(;@22bf  ;)    local.get 1
(;@22c1  ;)    i32.const 16777215
(;@22c6  ;)    local.get 2
(;@22c8  ;)    call $__call_2
(;@22ca  ;)    return
             )
(;@22cd  ;)  (func $main_lam_7 (;96;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@22ce  ;)    (local i32 i32 i32 i32 i32)
(;@22d0  ;)    local.get 0
(;@22d2  ;)    i32.load
(;@22d5  ;)    local.set 3
(;@22d7  ;)    local.get 3
(;@22d9  ;)    i32.load offset=8
(;@22dc  ;)    local.set 4
(;@22de  ;)    local.get 4
(;@22e0  ;)    local.get 1
(;@22e2  ;)    call $__call_1
(;@22e4  ;)    local.set 5
(;@22e6  ;)    i32.const 12
(;@22e8  ;)    call $alloc
(;@22ea  ;)    local.set 6
(;@22ec  ;)    local.get 6
(;@22ee  ;)    i32.const 2
(;@22f0  ;)    i32.store
(;@22f3  ;)    local.get 6
(;@22f5  ;)    i32.const 0
(;@22f7  ;)    i32.store offset=4
(;@22fa  ;)    local.get 6
(;@22fc  ;)    i32.const 95
(;@22ff  ;)    i32.store offset=8
(;@2302  ;)    local.get 6
(;@2304  ;)    local.set 6
(;@2306  ;)    local.get 5
(;@2308  ;)    local.get 6
(;@230a  ;)    local.get 2
(;@230c  ;)    call $__mon_bind
(;@230e  ;)    return
             )
(;@2311  ;)  (func $main_lam_8 (;97;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@2312  ;)    (local i32 i32)
(;@2314  ;)    local.get 1
(;@2316  ;)    i32.load offset=4
(;@2319  ;)    local.set 3
(;@231b  ;)    i32.const 8
(;@231d  ;)    call $alloc
(;@231f  ;)    local.set 4
(;@2321  ;)    local.get 4
(;@2323  ;)    i32.const 0
(;@2325  ;)    i32.store
(;@2328  ;)    local.get 4
(;@232a  ;)    local.get 3
(;@232c  ;)    i32.store offset=4
(;@232f  ;)    local.get 4
(;@2331  ;)    return
             )
(;@2334  ;)  (func $main_lam_9 (;98;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@2335  ;)    (local i32 i32 i32 i32 i32)
(;@2337  ;)    local.get 0
(;@2339  ;)    i32.load
(;@233c  ;)    local.set 3
(;@233e  ;)    local.get 3
(;@2340  ;)    i32.load offset=8
(;@2343  ;)    local.set 4
(;@2345  ;)    local.get 4
(;@2347  ;)    local.get 1
(;@2349  ;)    call $__call_1
(;@234b  ;)    local.set 5
(;@234d  ;)    i32.const 12
(;@234f  ;)    call $alloc
(;@2351  ;)    local.set 6
(;@2353  ;)    local.get 6
(;@2355  ;)    i32.const 2
(;@2357  ;)    i32.store
(;@235a  ;)    local.get 6
(;@235c  ;)    i32.const 0
(;@235e  ;)    i32.store offset=4
(;@2361  ;)    local.get 6
(;@2363  ;)    i32.const 97
(;@2366  ;)    i32.store offset=8
(;@2369  ;)    local.get 6
(;@236b  ;)    local.set 6
(;@236d  ;)    local.get 5
(;@236f  ;)    local.get 6
(;@2371  ;)    local.get 2
(;@2373  ;)    call $__mon_bind
(;@2375  ;)    return
             )
(;@2379  ;)  (func $__mon_bind (;99;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@237a  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@237c  ;)    local.get 0
(;@237e  ;)    local.get 2
(;@2380  ;)    call $__call_1
(;@2382  ;)    local.set 3
(;@2384  ;)    local.get 3
(;@2386  ;)    i32.load
(;@2389  ;)    local.set 4
(;@238b  ;)    block (result i32) ;; label = @1
(;@238d  ;)      block ;; label = @2
(;@238f  ;)        block ;; label = @3
(;@2391  ;)          block ;; label = @4
(;@2393  ;)            local.get 4
(;@2395  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@239a  ;)          end
(;@239b  ;)          local.get 3
(;@239d  ;)          i32.load offset=4
(;@23a0  ;)          local.set 5
(;@23a2  ;)          local.get 1
(;@23a4  ;)          local.get 5
(;@23a6  ;)          local.get 2
(;@23a8  ;)          call $__call_2
(;@23aa  ;)          br 2 (;@1;)
(;@23ac  ;)        end
(;@23ad  ;)        local.get 3
(;@23af  ;)        i32.load offset=4
(;@23b2  ;)        local.set 6
(;@23b4  ;)        local.get 6
(;@23b6  ;)        local.set 7
(;@23b8  ;)        local.get 7
(;@23ba  ;)        i32.load
(;@23bd  ;)        local.set 8
(;@23bf  ;)        local.get 7
(;@23c1  ;)        i32.load offset=4
(;@23c4  ;)        local.set 9
(;@23c6  ;)        i32.const 20
(;@23c8  ;)        call $alloc
(;@23ca  ;)        local.set 10
(;@23cc  ;)        local.get 10
(;@23ce  ;)        i32.const 1
(;@23d0  ;)        i32.store
(;@23d3  ;)        local.get 10
(;@23d5  ;)        i32.const 2
(;@23d7  ;)        i32.store offset=4
(;@23da  ;)        local.get 10
(;@23dc  ;)        i32.const 100
(;@23df  ;)        i32.store offset=8
(;@23e2  ;)        local.get 10
(;@23e4  ;)        local.get 1
(;@23e6  ;)        i32.store offset=12
(;@23e9  ;)        local.get 10
(;@23eb  ;)        local.get 7
(;@23ed  ;)        i32.store offset=16
(;@23f0  ;)        local.get 10
(;@23f2  ;)        local.set 10
(;@23f4  ;)        i32.const 12
(;@23f6  ;)        call $alloc
(;@23f8  ;)        local.set 11
(;@23fa  ;)        local.get 11
(;@23fc  ;)        local.get 8
(;@23fe  ;)        i32.store
(;@2401  ;)        local.get 11
(;@2403  ;)        local.get 9
(;@2405  ;)        i32.store offset=4
(;@2408  ;)        local.get 11
(;@240a  ;)        local.get 10
(;@240c  ;)        i32.store offset=8
(;@240f  ;)        local.get 11
(;@2411  ;)        local.set 11
(;@2413  ;)        i32.const 8
(;@2415  ;)        call $alloc
(;@2417  ;)        local.set 12
(;@2419  ;)        local.get 12
(;@241b  ;)        i32.const 1
(;@241d  ;)        i32.store
(;@2420  ;)        local.get 12
(;@2422  ;)        local.get 11
(;@2424  ;)        i32.store offset=4
(;@2427  ;)        local.get 12
(;@2429  ;)        br 1 (;@1;)
(;@242b  ;)      end
(;@242c  ;)      unreachable
(;@242d  ;)    end
(;@242e  ;)    return
             )
(;@2431  ;)  (func $__mon_bind_lam_0 (;100;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2432  ;)    (local i32 i32 i32 i32 i32)
(;@2434  ;)    local.get 0
(;@2436  ;)    i32.load
(;@2439  ;)    local.set 2
(;@243b  ;)    local.get 0
(;@243d  ;)    i32.load offset=4
(;@2440  ;)    local.set 3
(;@2442  ;)    local.get 3
(;@2444  ;)    i32.load offset=8
(;@2447  ;)    local.set 4
(;@2449  ;)    local.get 4
(;@244b  ;)    local.get 1
(;@244d  ;)    call $__call_1
(;@244f  ;)    local.set 5
(;@2451  ;)    i32.const 12
(;@2453  ;)    call $alloc
(;@2455  ;)    local.set 6
(;@2457  ;)    local.get 6
(;@2459  ;)    i32.const 2
(;@245b  ;)    i32.store
(;@245e  ;)    local.get 6
(;@2460  ;)    i32.const 2
(;@2462  ;)    i32.store offset=4
(;@2465  ;)    local.get 6
(;@2467  ;)    i32.const 99
(;@246a  ;)    i32.store offset=8
(;@246d  ;)    local.get 6
(;@246f  ;)    local.get 5
(;@2471  ;)    i32.store offset=12
(;@2474  ;)    local.get 6
(;@2476  ;)    local.get 2
(;@2478  ;)    i32.store offset=16
(;@247b  ;)    local.get 6
(;@247d  ;)    return
             )
(;@2481  ;)  (func $__mon_prompt (;101;) (type $fun_5_1) (param i32 i32 i32 i32 i32) (result i32)
(;@2482  ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@2484  ;)    local.get 1
(;@2486  ;)    local.get 4
(;@2488  ;)    call $__call_1
(;@248a  ;)    local.set 5
(;@248c  ;)    local.get 3
(;@248e  ;)    local.get 5
(;@2490  ;)    call $__call_1
(;@2492  ;)    local.set 6
(;@2494  ;)    local.get 6
(;@2496  ;)    i32.load
(;@2499  ;)    local.set 7
(;@249b  ;)    block (result i32) ;; label = @1
(;@249d  ;)      block ;; label = @2
(;@249f  ;)        block ;; label = @3
(;@24a1  ;)          block ;; label = @4
(;@24a3  ;)            local.get 7
(;@24a5  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@24aa  ;)          end
(;@24ab  ;)          local.get 6
(;@24ad  ;)          i32.load offset=4
(;@24b0  ;)          local.set 8
(;@24b2  ;)          local.get 2
(;@24b4  ;)          local.get 8
(;@24b6  ;)          local.get 4
(;@24b8  ;)          call $__call_2
(;@24ba  ;)          br 2 (;@1;)
(;@24bc  ;)        end
(;@24bd  ;)        local.get 6
(;@24bf  ;)        i32.load offset=4
(;@24c2  ;)        local.set 9
(;@24c4  ;)        local.get 9
(;@24c6  ;)        i32.load
(;@24c9  ;)        local.set 10
(;@24cb  ;)        local.get 0
(;@24cd  ;)        local.get 10
(;@24cf  ;)        call $__mon_eqm
(;@24d1  ;)        local.set 11
(;@24d3  ;)        local.get 11
(;@24d5  ;)        i32.load
(;@24d8  ;)        local.set 12
(;@24da  ;)        block (result i32) ;; label = @3
(;@24dc  ;)          block ;; label = @4
(;@24de  ;)            block ;; label = @5
(;@24e0  ;)              block ;; label = @6
(;@24e2  ;)                local.get 12
(;@24e4  ;)                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;)
(;@24e9  ;)              end
(;@24ea  ;)              local.get 11
(;@24ec  ;)              i32.load offset=4
(;@24ef  ;)              local.set 13
(;@24f1  ;)              local.get 9
(;@24f3  ;)              local.set 14
(;@24f5  ;)              local.get 14
(;@24f7  ;)              i32.load
(;@24fa  ;)              local.set 15
(;@24fc  ;)              local.get 14
(;@24fe  ;)              i32.load offset=4
(;@2501  ;)              local.set 16
(;@2503  ;)              i32.const 28
(;@2505  ;)              call $alloc
(;@2507  ;)              local.set 17
(;@2509  ;)              local.get 17
(;@250b  ;)              i32.const 2
(;@250d  ;)              i32.store
(;@2510  ;)              local.get 17
(;@2512  ;)              i32.const 4
(;@2514  ;)              i32.store offset=4
(;@2517  ;)              local.get 17
(;@2519  ;)              i32.const 102
(;@251c  ;)              i32.store offset=8
(;@251f  ;)              local.get 17
(;@2521  ;)              local.get 0
(;@2523  ;)              i32.store offset=12
(;@2526  ;)              local.get 17
(;@2528  ;)              local.get 1
(;@252a  ;)              i32.store offset=16
(;@252d  ;)              local.get 17
(;@252f  ;)              local.get 2
(;@2531  ;)              i32.store offset=20
(;@2534  ;)              local.get 17
(;@2536  ;)              local.get 14
(;@2538  ;)              i32.store offset=24
(;@253b  ;)              local.get 17
(;@253d  ;)              local.set 17
(;@253f  ;)              i32.const 12
(;@2541  ;)              call $alloc
(;@2543  ;)              local.set 18
(;@2545  ;)              local.get 18
(;@2547  ;)              local.get 15
(;@2549  ;)              i32.store
(;@254c  ;)              local.get 18
(;@254e  ;)              local.get 16
(;@2550  ;)              i32.store offset=4
(;@2553  ;)              local.get 18
(;@2555  ;)              local.get 17
(;@2557  ;)              i32.store offset=8
(;@255a  ;)              local.get 18
(;@255c  ;)              local.set 18
(;@255e  ;)              i32.const 8
(;@2560  ;)              call $alloc
(;@2562  ;)              local.set 19
(;@2564  ;)              local.get 19
(;@2566  ;)              i32.const 1
(;@2568  ;)              i32.store
(;@256b  ;)              local.get 19
(;@256d  ;)              local.get 18
(;@256f  ;)              i32.store offset=4
(;@2572  ;)              local.get 19
(;@2574  ;)              br 2 (;@3;)
(;@2576  ;)            end
(;@2577  ;)            local.get 11
(;@2579  ;)            i32.load offset=4
(;@257c  ;)            local.set 13
(;@257e  ;)            i32.const 28
(;@2580  ;)            call $alloc
(;@2582  ;)            local.set 19
(;@2584  ;)            local.get 19
(;@2586  ;)            i32.const 2
(;@2588  ;)            i32.store
(;@258b  ;)            local.get 19
(;@258d  ;)            i32.const 4
(;@258f  ;)            i32.store offset=4
(;@2592  ;)            local.get 19
(;@2594  ;)            i32.const 103
(;@2597  ;)            i32.store offset=8
(;@259a  ;)            local.get 19
(;@259c  ;)            local.get 0
(;@259e  ;)            i32.store offset=12
(;@25a1  ;)            local.get 19
(;@25a3  ;)            local.get 1
(;@25a5  ;)            i32.store offset=16
(;@25a8  ;)            local.get 19
(;@25aa  ;)            local.get 2
(;@25ac  ;)            i32.store offset=20
(;@25af  ;)            local.get 19
(;@25b1  ;)            local.get 9
(;@25b3  ;)            i32.store offset=24
(;@25b6  ;)            local.get 19
(;@25b8  ;)            local.set 19
(;@25ba  ;)            local.get 9
(;@25bc  ;)            i32.load offset=4
(;@25bf  ;)            local.set 20
(;@25c1  ;)            local.get 20
(;@25c3  ;)            local.get 19
(;@25c5  ;)            local.get 4
(;@25c7  ;)            call $__call_2
(;@25c9  ;)            br 1 (;@3;)
(;@25cb  ;)          end
(;@25cc  ;)          unreachable
(;@25cd  ;)        end
(;@25ce  ;)        br 1 (;@1;)
(;@25d0  ;)      end
(;@25d1  ;)      unreachable
(;@25d2  ;)    end
(;@25d3  ;)    return
             )
(;@25d6  ;)  (func $__mon_prompt_lam_0 (;102;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@25d7  ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@25d9  ;)    local.get 0
(;@25db  ;)    i32.load
(;@25de  ;)    local.set 3
(;@25e0  ;)    local.get 0
(;@25e2  ;)    i32.load offset=4
(;@25e5  ;)    local.set 4
(;@25e7  ;)    local.get 0
(;@25e9  ;)    i32.load offset=8
(;@25ec  ;)    local.set 5
(;@25ee  ;)    local.get 0
(;@25f0  ;)    i32.load offset=12
(;@25f3  ;)    local.set 6
(;@25f5  ;)    local.get 6
(;@25f7  ;)    i32.load offset=8
(;@25fa  ;)    local.set 7
(;@25fc  ;)    local.get 7
(;@25fe  ;)    local.get 1
(;@2600  ;)    call $__call_1
(;@2602  ;)    local.set 8
(;@2604  ;)    local.get 3
(;@2606  ;)    local.get 4
(;@2608  ;)    local.get 5
(;@260a  ;)    local.get 8
(;@260c  ;)    local.get 2
(;@260e  ;)    call $__mon_prompt
(;@2610  ;)    return
             )
(;@2613  ;)  (func $__mon_prompt_lam_1 (;103;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@2614  ;)    (local i32 i32 i32 i32 i32 i32 i32)
(;@2616  ;)    local.get 0
(;@2618  ;)    i32.load
(;@261b  ;)    local.set 3
(;@261d  ;)    local.get 0
(;@261f  ;)    i32.load offset=4
(;@2622  ;)    local.set 4
(;@2624  ;)    local.get 0
(;@2626  ;)    i32.load offset=8
(;@2629  ;)    local.set 5
(;@262b  ;)    local.get 0
(;@262d  ;)    i32.load offset=12
(;@2630  ;)    local.set 6
(;@2632  ;)    local.get 6
(;@2634  ;)    i32.load offset=8
(;@2637  ;)    local.set 7
(;@2639  ;)    local.get 7
(;@263b  ;)    local.get 1
(;@263d  ;)    call $__call_1
(;@263f  ;)    local.set 8
(;@2641  ;)    local.get 3
(;@2643  ;)    local.get 4
(;@2645  ;)    local.get 5
(;@2647  ;)    local.get 8
(;@2649  ;)    local.get 2
(;@264b  ;)    call $__mon_prompt
(;@264d  ;)    return
             )
(;@a3    ;)  (table (;0;) 104 104 funcref)
(;@aa    ;)  (memory (;0;) 1)
(;@af    ;)  (global (;0;) (mut i32) i32.const 0)
(;@b4    ;)  (global (;1;) (mut i32) i32.const 0)
(;@bc    ;)  (export "main" (func $main))
(;@c3    ;)  (export "mem" (memory 0))
(;@cc    ;)  (elem (;0;) (i32.const 0) func $__mon_generate_marker $alloc $__mon_eqm $__call_1 $__call_2 $f $f_lam_0 $f_lam_1 $f_lam_2 $f_lam_3 $f_lam_4 $f_lam_5 $f_lam_6 $f_lam_7 $f_lam_8 $f_lam_9 $f_lam_10 $f_lam_11 $f_lam_12 $f_lam_13 $f_lam_14 $f_lam_15 $f_lam_16 $f_lam_17 $f_lam_18 $f_lam_19 $f_lam_20 $f_lam_21 $f_lam_22 $f_lam_23 $f_lam_24 $f_lam_25 $f_lam_26 $f_lam_27 $f_lam_28 $f_lam_29 $f_lam_30 $f_lam_31 $f_lam_32 $f_lam_33 $f_lam_34 $f_lam_35 $f_lam_36 $f_lam_37 $f_lam_38 $f_lam_39 $f_lam_40 $f_lam_41 $f_lam_42 $f_lam_43 $f_lam_44 $f_lam_45 $f_lam_46 $f_lam_47 $f_lam_48 $f_lam_49 $f_lam_50 $f_lam_51 $f_lam_52 $f_lam_53 $f_lam_54 $f_lam_55 $f_lam_56 $f_lam_57 $f_lam_58 $f_lam_59 $f_lam_60 $f_lam_61 $f_lam_62 $f_lam_63 $f_lam_64 $f_lam_65 $f_lam_66 $f_lam_67 $f_lam_68 $f_lam_69 $f_lam_70 $f_lam_71 $f_lam_72 $f_lam_73 $f_lam_74 $f_lam_75 $f_lam_76 $f_lam_77 $f_lam_78 $f_lam_79 $f_lam_80 $f_lam_81 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $__mon_bind $__mon_bind_lam_0 $__mon_prompt $__mon_prompt_lam_0 $__mon_prompt_lam_1)
           )
