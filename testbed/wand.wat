(module $example
(;@b     ;)  (type (;0;) (func (result i32)))
(;@f     ;)  (type (;1;) (func (param i32 i32) (result i32)))
(;@15    ;)  (type (;2;) (func (param i32 i32 i32) (result i32)))
(;@1c    ;)  (type $fun_2_1 (;3;) (func (param i32 i32) (result i32)))
(;@22    ;)  (type $fun_1_1 (;4;) (func (param i32) (result i32)))
(;@27    ;)  (type $fun_3_1 (;5;) (func (param i32 i32 i32) (result i32)))
(;@2e    ;)  (type $fun_4_1 (;6;) (func (param i32 i32 i32 i32) (result i32)))
(;@36    ;)  (type $fun_0_1 (;7;) (func (result i32)))
(;@3d    ;)  (import "intrinsic" "__mon_generate_marker" (func (;0;) (type 0)))
(;@5f    ;)  (import "intrinsic" "__mon_prompt" (func (;1;) (type 2)))
(;@78    ;)  (import "intrinsic" "__mon_bind" (func (;2;) (type 1)))
(;@8f    ;)  (import "intrinsic" "__mon_eqm" (func (;3;) (type 1)))
(;@ff    ;)  (func $__apply_1_0 (;4;) (type $fun_2_1) (param i32 i32) (result i32)
(;@100   ;)    local.get 1
(;@102   ;)    local.get 0
(;@104   ;)    i32.load
(;@107   ;)    call_indirect (type $fun_1_1)
             )
(;@10c   ;)  (func $__apply_2_0 (;5;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@10d   ;)    local.get 1
(;@10f   ;)    local.get 2
(;@111   ;)    local.get 0
(;@113   ;)    i32.load
(;@116   ;)    call_indirect (type $fun_2_1)
             )
(;@11b   ;)  (func $__apply_2_1 (;6;) (type $fun_2_1) (param i32 i32) (result i32)
(;@11c   ;)    local.get 0
(;@11e   ;)    i32.load offset=4
(;@121   ;)    local.get 1
(;@123   ;)    local.get 0
(;@125   ;)    i32.load
(;@128   ;)    call_indirect (type $fun_2_1)
             )
(;@12d   ;)  (func $__apply_3_2 (;7;) (type $fun_2_1) (param i32 i32) (result i32)
(;@12e   ;)    local.get 0
(;@130   ;)    i32.load offset=4
(;@133   ;)    local.get 0
(;@135   ;)    i32.load offset=8
(;@138   ;)    local.get 1
(;@13a   ;)    local.get 0
(;@13c   ;)    i32.load
(;@13f   ;)    call_indirect (type $fun_3_1)
             )
(;@144   ;)  (func $__apply_4_3 (;8;) (type $fun_2_1) (param i32 i32) (result i32)
(;@145   ;)    local.get 0
(;@147   ;)    i32.load offset=4
(;@14a   ;)    local.get 0
(;@14c   ;)    i32.load offset=8
(;@14f   ;)    local.get 0
(;@151   ;)    i32.load offset=12
(;@154   ;)    local.get 1
(;@156   ;)    local.get 0
(;@158   ;)    i32.load
(;@15b   ;)    call_indirect (type $fun_4_1)
             )
(;@161   ;)  (func $main (;9;) (type $fun_0_1) (result i32)
(;@162   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@164   ;)    global.get 0
(;@166   ;)    local.set 0
(;@168   ;)    local.get 0
(;@16a   ;)    call 0
(;@16c   ;)    local.set 1
(;@16e   ;)    global.get 0
(;@170   ;)    local.set 2
(;@172   ;)    local.get 1
(;@174   ;)    local.get 1
(;@176   ;)    call 3
(;@178   ;)    local.set 3
(;@17a   ;)    local.get 3
(;@17c   ;)    i32.load
(;@17f   ;)    local.set 4
(;@181   ;)    block (result i32) ;; label = @1
(;@183   ;)      block (result i32) ;; label = @2
(;@185   ;)        block (result i32) ;; label = @3
(;@187   ;)          block (result i32) ;; label = @4
(;@189   ;)            local.get 4
(;@18b   ;)            local.get 4
(;@18d   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@192   ;)          end
(;@193   ;)          local.get 3
(;@195   ;)          i32.load offset=4
(;@198   ;)          local.set 5
(;@19a   ;)          global.get 0
(;@19c   ;)          i32.const 4
(;@19e   ;)          i32.store
(;@1a1   ;)          global.get 0
(;@1a3   ;)          i32.const 10
(;@1a5   ;)          i32.store offset=4
(;@1a8   ;)          global.get 0
(;@1aa   ;)          global.get 0
(;@1ac   ;)          i32.const 8
(;@1ae   ;)          i32.add
(;@1af   ;)          global.set 0
(;@1b1   ;)          local.set 6
(;@1b3   ;)          global.get 0
(;@1b5   ;)          i32.const 6
(;@1b7   ;)          i32.store
(;@1ba   ;)          global.get 0
(;@1bc   ;)          i32.const 15
(;@1be   ;)          i32.store offset=4
(;@1c1   ;)          global.get 0
(;@1c3   ;)          local.get 1
(;@1c5   ;)          i32.store offset=8
(;@1c8   ;)          global.get 0
(;@1ca   ;)          global.get 0
(;@1cc   ;)          i32.const 12
(;@1ce   ;)          i32.add
(;@1cf   ;)          global.set 0
(;@1d1   ;)          local.set 7
(;@1d3   ;)          global.get 0
(;@1d5   ;)          local.get 1
(;@1d7   ;)          i32.store
(;@1da   ;)          global.get 0
(;@1dc   ;)          local.get 6
(;@1de   ;)          i32.store offset=4
(;@1e1   ;)          global.get 0
(;@1e3   ;)          local.get 7
(;@1e5   ;)          i32.store offset=8
(;@1e8   ;)          global.get 0
(;@1ea   ;)          global.get 0
(;@1ec   ;)          i32.const 12
(;@1ee   ;)          i32.add
(;@1ef   ;)          global.set 0
(;@1f1   ;)          local.set 8
(;@1f3   ;)          global.get 0
(;@1f5   ;)          i32.const 1
(;@1f7   ;)          i32.store
(;@1fa   ;)          global.get 0
(;@1fc   ;)          local.get 8
(;@1fe   ;)          i32.store offset=4
(;@201   ;)          global.get 0
(;@203   ;)          global.get 0
(;@205   ;)          i32.const 8
(;@207   ;)          i32.add
(;@208   ;)          global.set 0
(;@20a   ;)          br 2 (;@1;)
(;@20c   ;)        end
(;@20d   ;)        local.get 3
(;@20f   ;)        i32.load offset=4
(;@212   ;)        local.set 5
(;@214   ;)        global.get 0
(;@216   ;)        i32.const 6
(;@218   ;)        i32.store
(;@21b   ;)        global.get 0
(;@21d   ;)        i32.const 17
(;@21f   ;)        i32.store offset=4
(;@222   ;)        global.get 0
(;@224   ;)        local.get 1
(;@226   ;)        i32.store offset=8
(;@229   ;)        global.get 0
(;@22b   ;)        global.get 0
(;@22d   ;)        i32.const 12
(;@22f   ;)        i32.add
(;@230   ;)        global.set 0
(;@232   ;)        local.set 9
(;@234   ;)        global.get 0
(;@236   ;)        i32.const 4
(;@238   ;)        i32.store
(;@23b   ;)        global.get 0
(;@23d   ;)        i32.const 18
(;@23f   ;)        i32.store offset=4
(;@242   ;)        global.get 0
(;@244   ;)        global.get 0
(;@246   ;)        i32.const 8
(;@248   ;)        i32.add
(;@249   ;)        global.set 0
(;@24b   ;)        local.set 10
(;@24d   ;)        global.get 0
(;@24f   ;)        i32.const 5
(;@251   ;)        i32.store
(;@254   ;)        global.get 0
(;@256   ;)        i32.const 19
(;@258   ;)        i32.store offset=4
(;@25b   ;)        global.get 0
(;@25d   ;)        global.get 0
(;@25f   ;)        i32.const 8
(;@261   ;)        i32.add
(;@262   ;)        global.set 0
(;@264   ;)        local.set 11
(;@266   ;)        global.get 0
(;@268   ;)        i32.const 7
(;@26a   ;)        i32.store
(;@26d   ;)        global.get 0
(;@26f   ;)        i32.const 1
(;@271   ;)        i32.store offset=4
(;@274   ;)        global.get 0
(;@276   ;)        local.get 11
(;@278   ;)        i32.store offset=8
(;@27b   ;)        global.get 0
(;@27d   ;)        local.get 10
(;@27f   ;)        i32.store offset=12
(;@282   ;)        global.get 0
(;@284   ;)        global.get 0
(;@286   ;)        i32.const 16
(;@288   ;)        i32.add
(;@289   ;)        global.set 0
(;@28b   ;)        local.set 12
(;@28d   ;)        local.get 1
(;@28f   ;)        local.get 9
(;@291   ;)        local.get 12
(;@293   ;)        local.get 2
(;@295   ;)        call 2
(;@297   ;)        br 1 (;@1;)
(;@299   ;)      end
(;@29a   ;)      unreachable
(;@29b   ;)    end
(;@29c   ;)    local.set 13
(;@29e   ;)    local.get 13
(;@2a0   ;)    i32.load
(;@2a3   ;)    local.set 14
(;@2a5   ;)    block (result i32) ;; label = @1
(;@2a7   ;)      block (result i32) ;; label = @2
(;@2a9   ;)        block (result i32) ;; label = @3
(;@2ab   ;)          local.get 14
(;@2ad   ;)          local.get 14
(;@2af   ;)          br_table 0 (;@3;) 1 (;@2;)
(;@2b3   ;)        end
(;@2b4   ;)        local.get 13
(;@2b6   ;)        i32.load offset=4
(;@2b9   ;)        local.set 15
(;@2bb   ;)        local.get 15
(;@2bd   ;)        br 1 (;@1;)
(;@2bf   ;)      end
(;@2c0   ;)      unreachable
(;@2c1   ;)    end
             )
(;@2c4   ;)  (func $main_lam_0 (;10;) (type $fun_1_1) (param i32) (result i32)
(;@2c5   ;)    (local i32)
(;@2c7   ;)    local.get 0
(;@2c9   ;)    i32.const 4
(;@2cb   ;)    i32.add
(;@2cc   ;)    i32.const 52
(;@2ce   ;)    local.get 0
(;@2d0   ;)    i32.load
(;@2d3   ;)    call_indirect (type $fun_2_1)
             )
(;@2d8   ;)  (func $main_lam_1 (;11;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2d9   ;)    (local i32)
(;@2db   ;)    local.get 1
(;@2dd   ;)    i32.const 4
(;@2df   ;)    i32.add
(;@2e0   ;)    i32.const 52
(;@2e2   ;)    local.get 1
(;@2e4   ;)    i32.load
(;@2e7   ;)    call_indirect (type $fun_2_1)
             )
(;@2ec   ;)  (func $main_lam_2 (;12;) (type $fun_2_1) (param i32 i32) (result i32)
(;@2ed   ;)    (local i32 i32)
(;@2ef   ;)    global.get 0
(;@2f1   ;)    i32.const 5
(;@2f3   ;)    i32.store
(;@2f6   ;)    global.get 0
(;@2f8   ;)    i32.const 11
(;@2fa   ;)    i32.store offset=4
(;@2fd   ;)    global.get 0
(;@2ff   ;)    global.get 0
(;@301   ;)    i32.const 8
(;@303   ;)    i32.add
(;@304   ;)    global.set 0
(;@306   ;)    local.set 2
(;@308   ;)    global.get 0
(;@30a   ;)    local.get 0
(;@30c   ;)    i32.store
(;@30f   ;)    global.get 0
(;@311   ;)    local.get 2
(;@313   ;)    i32.store offset=4
(;@316   ;)    global.get 0
(;@318   ;)    global.get 0
(;@31a   ;)    i32.const 8
(;@31c   ;)    i32.add
(;@31d   ;)    global.set 0
             )
(;@321   ;)  (func $main_lam_3 (;13;) (type $fun_2_1) (param i32 i32) (result i32)
(;@322   ;)    (local i32)
(;@324   ;)    global.get 0
(;@326   ;)    i32.const 0
(;@328   ;)    i32.store
(;@32b   ;)    global.get 0
(;@32d   ;)    local.get 0
(;@32f   ;)    i32.store offset=4
(;@332   ;)    global.get 0
(;@334   ;)    global.get 0
(;@336   ;)    i32.const 8
(;@338   ;)    i32.add
(;@339   ;)    global.set 0
             )
(;@33d   ;)  (func $main_lam_4 (;14;) (type $fun_2_1) (param i32 i32) (result i32)
(;@33e   ;)    (local i32)
(;@340   ;)    global.get 0
(;@342   ;)    i32.const 0
(;@344   ;)    i32.store
(;@347   ;)    global.get 0
(;@349   ;)    local.get 0
(;@34b   ;)    i32.store offset=4
(;@34e   ;)    global.get 0
(;@350   ;)    global.get 0
(;@352   ;)    i32.const 8
(;@354   ;)    i32.add
(;@355   ;)    global.set 0
             )
(;@35a   ;)  (func $main_lam_5 (;15;) (type $fun_2_1) (param i32 i32) (result i32)
(;@35b   ;)    (local i32 i32 i32 i32 i32)
(;@35d   ;)    global.get 0
(;@35f   ;)    i32.const 6
(;@361   ;)    i32.store
(;@364   ;)    global.get 0
(;@366   ;)    i32.const 12
(;@368   ;)    i32.store offset=4
(;@36b   ;)    global.get 0
(;@36d   ;)    local.get 0
(;@36f   ;)    i32.store offset=8
(;@372   ;)    global.get 0
(;@374   ;)    global.get 0
(;@376   ;)    i32.const 12
(;@378   ;)    i32.add
(;@379   ;)    global.set 0
(;@37b   ;)    local.set 2
(;@37d   ;)    global.get 0
(;@37f   ;)    i32.const 6
(;@381   ;)    i32.store
(;@384   ;)    global.get 0
(;@386   ;)    i32.const 13
(;@388   ;)    i32.store offset=4
(;@38b   ;)    global.get 0
(;@38d   ;)    local.get 1
(;@38f   ;)    i32.store offset=8
(;@392   ;)    global.get 0
(;@394   ;)    global.get 0
(;@396   ;)    i32.const 12
(;@398   ;)    i32.add
(;@399   ;)    global.set 0
(;@39b   ;)    local.set 3
(;@39d   ;)    global.get 0
(;@39f   ;)    i32.const 5
(;@3a1   ;)    i32.store
(;@3a4   ;)    global.get 0
(;@3a6   ;)    i32.const 14
(;@3a8   ;)    i32.store offset=4
(;@3ab   ;)    global.get 0
(;@3ad   ;)    global.get 0
(;@3af   ;)    i32.const 8
(;@3b1   ;)    i32.add
(;@3b2   ;)    global.set 0
(;@3b4   ;)    local.set 4
(;@3b6   ;)    global.get 0
(;@3b8   ;)    i32.const 7
(;@3ba   ;)    i32.store
(;@3bd   ;)    global.get 0
(;@3bf   ;)    i32.const 1
(;@3c1   ;)    i32.store offset=4
(;@3c4   ;)    global.get 0
(;@3c6   ;)    local.get 4
(;@3c8   ;)    i32.store offset=8
(;@3cb   ;)    global.get 0
(;@3cd   ;)    local.get 3
(;@3cf   ;)    i32.store offset=12
(;@3d2   ;)    global.get 0
(;@3d4   ;)    global.get 0
(;@3d6   ;)    i32.const 16
(;@3d8   ;)    i32.add
(;@3d9   ;)    global.set 0
(;@3db   ;)    local.set 5
(;@3dd   ;)    global.get 0
(;@3df   ;)    i32.const 8
(;@3e1   ;)    i32.store
(;@3e4   ;)    global.get 0
(;@3e6   ;)    i32.const 2
(;@3e8   ;)    i32.store offset=4
(;@3eb   ;)    global.get 0
(;@3ed   ;)    local.get 5
(;@3ef   ;)    i32.store offset=8
(;@3f2   ;)    global.get 0
(;@3f4   ;)    local.get 2
(;@3f6   ;)    i32.store offset=12
(;@3f9   ;)    global.get 0
(;@3fb   ;)    local.get 0
(;@3fd   ;)    i32.store offset=16
(;@400   ;)    global.get 0
(;@402   ;)    global.get 0
(;@404   ;)    i32.const 20
(;@406   ;)    i32.add
(;@407   ;)    global.set 0
             )
(;@40b   ;)  (func $main_lam_6 (;16;) (type $fun_2_1) (param i32 i32) (result i32)
(;@40c   ;)    (local i32)
(;@40e   ;)    local.get 1
(;@410   ;)    i32.const 4
(;@412   ;)    i32.add
(;@413   ;)    i32.const 52
(;@415   ;)    local.get 1
(;@417   ;)    i32.load
(;@41a   ;)    call_indirect (type $fun_2_1)
             )
(;@41f   ;)  (func $main_lam_7 (;17;) (type $fun_2_1) (param i32 i32) (result i32)
(;@420   ;)    (local i32 i32)
(;@422   ;)    global.get 0
(;@424   ;)    i32.const 5
(;@426   ;)    i32.store
(;@429   ;)    global.get 0
(;@42b   ;)    i32.const 16
(;@42d   ;)    i32.store offset=4
(;@430   ;)    global.get 0
(;@432   ;)    global.get 0
(;@434   ;)    i32.const 8
(;@436   ;)    i32.add
(;@437   ;)    global.set 0
(;@439   ;)    local.set 2
(;@43b   ;)    global.get 0
(;@43d   ;)    local.get 0
(;@43f   ;)    i32.store
(;@442   ;)    global.get 0
(;@444   ;)    local.get 2
(;@446   ;)    i32.store offset=4
(;@449   ;)    global.get 0
(;@44b   ;)    global.get 0
(;@44d   ;)    i32.const 8
(;@44f   ;)    i32.add
(;@450   ;)    global.set 0
             )
(;@454   ;)  (func $main_lam_8 (;18;) (type $fun_1_1) (param i32) (result i32)
(;@455   ;)    (local i32)
(;@457   ;)    global.get 0
(;@459   ;)    i32.const 0
(;@45b   ;)    i32.store
(;@45e   ;)    global.get 0
(;@460   ;)    i32.const 52
(;@462   ;)    i32.store offset=4
(;@465   ;)    global.get 0
(;@467   ;)    global.get 0
(;@469   ;)    i32.const 8
(;@46b   ;)    i32.add
(;@46c   ;)    global.set 0
             )
(;@470   ;)  (func $main_lam_9 (;19;) (type $fun_2_1) (param i32 i32) (result i32)
(;@471   ;)    (local i32)
(;@473   ;)    global.get 0
(;@475   ;)    i32.const 0
(;@477   ;)    i32.store
(;@47a   ;)    global.get 0
(;@47c   ;)    local.get 0
(;@47e   ;)    i32.store offset=4
(;@481   ;)    global.get 0
(;@483   ;)    global.get 0
(;@485   ;)    i32.const 8
(;@487   ;)    i32.add
(;@488   ;)    global.set 0
             )
(;@bb    ;)  (table (;0;) 20 20 funcref)
(;@c2    ;)  (memory (;0;) 1)
(;@c7    ;)  (global (;0;) (mut i32) i32.const 0)
(;@cf    ;)  (export "main" (func $main))
(;@d6    ;)  (export "mem" (memory 0))
(;@df    ;)  (elem (;0;) (i32.const 0) func 0 1 2 3 $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_2 $__apply_4_3 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9)
           )
515   ;)    i32.const 5
(;@517   ;)    i32.store
(;@51a   ;)    global.get 0
(;@51c   ;)    i32.const 22
(;@51e   ;)    i32.store offset=4
(;@521   ;)    global.get 0
(;@523   ;)    global.get 0
(;@525   ;)    i32.const 8
(;@527   ;)    i32.add
(;@528   ;)    global.set 0
(;@52a   ;)    local.set 1
(;@52c   ;)    global.get 0
(;@52e   ;)    i32.const 7
(;@530   ;)    i32.store
(;@533   ;)    global.get 0
(;@535   ;)    i32.const 23
(;@537   ;)    i32.store offset=4
(;@53a   ;)    global.get 0
(;@53c   ;)    global.get 0
(;@53e   ;)    i32.const 8
(;@540   ;)    i32.add
(;@541   ;)    global.set 0
(;@543   ;)    local.set 2
(;@545   ;)    global.get 0
(;@547   ;)    i32.const 4
(;@549   ;)    i32.store
(;@54c   ;)    global.get 0
(;@54e   ;)    i32.const 24
(;@550   ;)    i32.store offset=4
(;@553   ;)    global.get 0
(;@555   ;)    global.get 0
(;@557   ;)    i32.const 8
(;@559   ;)    i32.add
(;@55a   ;)    global.set 0
(;@55c   ;)    local.set 3
(;@55e   ;)    global.get 0
(;@560   ;)    i32.const 4
(;@562   ;)    i32.store
(;@565   ;)    global.get 0
(;@567   ;)    i32.const 25
(;@569   ;)    i32.store offset=4
(;@56c   ;)    global.get 0
(;@56e   ;)    global.get 0
(;@570   ;)    i32.const 8
(;@572   ;)    i32.add
(;@573   ;)    global.set 0
(;@575   ;)    local.set 4
(;@577   ;)    global.get 0
(;@579   ;)    local.get 3
(;@57b   ;)    i32.store
(;@57e   ;)    global.get 0
(;@580   ;)    local.get 4
(;@582   ;)    i32.store offset=4
(;@585   ;)    global.get 0
(;@587   ;)    global.get 0
(;@589   ;)    i32.const 8
(;@58b   ;)    i32.add
(;@58c   ;)    global.set 0
(;@58e   ;)    local.set 5
(;@590   ;)    global.get 0
(;@592   ;)    i32.const 4
(;@594   ;)    i32.store
(;@597   ;)    global.get 0
(;@599   ;)    i32.const 26
(;@59b   ;)    i32.store offset=4
(;@59e   ;)    global.get 0
(;@5a0   ;)    global.get 0
(;@5a2   ;)    i32.const 8
(;@5a4   ;)    i32.add
(;@5a5   ;)    global.set 0
(;@5a7   ;)    local.set 6
(;@5a9   ;)    global.get 0
(;@5ab   ;)    i32.const 4
(;@5ad   ;)    i32.store
(;@5b0   ;)    global.get 0
(;@5b2   ;)    i32.const 27
(;@5b4   ;)    i32.store offset=4
(;@5b7   ;)    global.get 0
(;@5b9   ;)    global.get 0
(;@5bb   ;)    i32.const 8
(;@5bd   ;)    i32.add
(;@5be   ;)    global.set 0
(;@5c0   ;)    local.set 7
(;@5c2   ;)    global.get 0
(;@5c4   ;)    local.get 6
(;@5c6   ;)    i32.store
(;@5c9   ;)    global.get 0
(;@5cb   ;)    local.get 7
(;@5cd   ;)    i32.store offset=4
(;@5d0   ;)    global.get 0
(;@5d2   ;)    global.get 0
(;@5d4   ;)    i32.const 8
(;@5d6   ;)    i32.add
(;@5d7   ;)    global.set 0
(;@5d9   ;)    local.set 8
(;@5db   ;)    global.get 0
(;@5dd   ;)    local.get 1
(;@5df   ;)    i32.store
(;@5e2   ;)    global.get 0
(;@5e4   ;)    local.get 2
(;@5e6   ;)    i32.store offset=4
(;@5e9   ;)    global.get 0
(;@5eb   ;)    local.get 5
(;@5ed   ;)    i32.store offset=8
(;@5f0   ;)    global.get 0
(;@5f2   ;)    local.get 8
(;@5f4   ;)    i32.store offset=12
(;@5f7   ;)    global.get 0
(;@5f9   ;)    global.get 0
(;@5fb   ;)    i32.const 16
(;@5fd   ;)    i32.add
(;@5fe   ;)    global.set 0
             )
(;@602   ;)  (func $_row_simple__ask_lam_0 (;22;) (type $fun_2_1) (param i32 i32) (result i32)
(;@603   ;)    (local i32)
(;@605   ;)    local.get 1
             )
(;@609   ;)  (func $_row_simple__ask_lam_1 (;23;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@60a   ;)    (local i32 i32)
(;@60c   ;)    local.get 2
(;@60e   ;)    local.set 3
(;@610   ;)    local.get 1
(;@612   ;)    i32.const 4
(;@614   ;)    i32.add
(;@615   ;)    local.get 3
(;@617   ;)    local.get 1
(;@619   ;)    i32.load
(;@61c   ;)    call_indirect (type $fun_2_1)
             )
(;@621   ;)  (func $_row_simple__ask_lam_2 (;24;) (type $fun_1_1) (param i32) (result i32)
(;@622   ;)    (local i32)
(;@624   ;)    global.get 0
             )
(;@628   ;)  (func $_row_simple__ask_lam_3 (;25;) (type $fun_1_1) (param i32) (result i32)
(;@629   ;)    (local i32 i32)
(;@62b   ;)    local.get 0
(;@62d   ;)    i32.load
(;@630   ;)    local.set 1
(;@632   ;)    block (result i32) ;; label = @1
(;@634   ;)      block (result i32) ;; label = @2
(;@636   ;)        local.get 1
(;@638   ;)        local.get 1
(;@63a   ;)        br_table 0 (;@2;)
(;@63d   ;)      end
(;@63e   ;)      unreachable
(;@63f   ;)    end
             )
(;@642   ;)  (func $_row_simple__ask_lam_4 (;26;) (type $fun_1_1) (param i32) (result i32)
(;@643   ;)    (local i32)
(;@645   ;)    local.get 0
             )
(;@649   ;)  (func $_row_simple__ask_lam_5 (;27;) (type $fun_1_1) (param i32) (result i32)
(;@64a   ;)    (local i32 i32)
(;@64c   ;)    local.get 0
(;@64e   ;)    local.set 1
(;@650   ;)    local.get 1
             )
(;@655   ;)  (func $_row_scoped__ask (;28;) (type $fun_1_1) (param i32) (result i32)
(;@656   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@658   ;)    global.get 0
(;@65a   ;)    i32.const 5
(;@65c   ;)    i32.store
(;@65f   ;)    global.get 0
(;@661   ;)    i32.const 29
(;@663   ;)    i32.store offset=4
(;@666   ;)    global.get 0
(;@668   ;)    global.get 0
(;@66a   ;)    i32.const 8
(;@66c   ;)    i32.add
(;@66d   ;)    global.set 0
(;@66f   ;)    local.set 1
(;@671   ;)    global.get 0
(;@673   ;)    i32.const 7
(;@675   ;)    i32.store
(;@678   ;)    global.get 0
(;@67a   ;)    i32.const 30
(;@67c   ;)    i32.store offset=4
(;@67f   ;)    global.get 0
(;@681   ;)    global.get 0
(;@683   ;)    i32.const 8
(;@685   ;)    i32.add
(;@686   ;)    global.set 0
(;@688   ;)    local.set 2
(;@68a   ;)    global.get 0
(;@68c   ;)    i32.const 4
(;@68e   ;)    i32.store
(;@691   ;)    global.get 0
(;@693   ;)    i32.const 31
(;@695   ;)    i32.store offset=4
(;@698   ;)    global.get 0
(;@69a   ;)    global.get 0
(;@69c   ;)    i32.const 8
(;@69e   ;)    i32.add
(;@69f   ;)    global.set 0
(;@6a1   ;)    local.set 3
(;@6a3   ;)    global.get 0
(;@6a5   ;)    i32.const 4
(;@6a7   ;)    i32.store
(;@6aa   ;)    global.get 0
(;@6ac   ;)    i32.const 32
(;@6ae   ;)    i32.store offset=4
(;@6b1   ;)    global.get 0
(;@6b3   ;)    global.get 0
(;@6b5   ;)    i32.const 8
(;@6b7   ;)    i32.add
(;@6b8   ;)    global.set 0
(;@6ba   ;)    local.set 4
(;@6bc   ;)    global.get 0
(;@6be   ;)    local.get 3
(;@6c0   ;)    i32.store
(;@6c3   ;)    global.get 0
(;@6c5   ;)    local.get 4
(;@6c7   ;)    i32.store offset=4
(;@6ca   ;)    global.get 0
(;@6cc   ;)    global.get 0
(;@6ce   ;)    i32.const 8
(;@6d0   ;)    i32.add
(;@6d1   ;)    global.set 0
(;@6d3   ;)    local.set 5
(;@6d5   ;)    global.get 0
(;@6d7   ;)    i32.const 4
(;@6d9   ;)    i32.store
(;@6dc   ;)    global.get 0
(;@6de   ;)    i32.const 33
(;@6e0   ;)    i32.store offset=4
(;@6e3   ;)    global.get 0
(;@6e5   ;)    global.get 0
(;@6e7   ;)    i32.const 8
(;@6e9   ;)    i32.add
(;@6ea   ;)    global.set 0
(;@6ec   ;)    local.set 6
(;@6ee   ;)    global.get 0
(;@6f0   ;)    i32.const 4
(;@6f2   ;)    i32.store
(;@6f5   ;)    global.get 0
(;@6f7   ;)    i32.const 34
(;@6f9   ;)    i32.store offset=4
(;@6fc   ;)    global.get 0
(;@6fe   ;)    global.get 0
(;@700   ;)    i32.const 8
(;@702   ;)    i32.add
(;@703   ;)    global.set 0
(;@705   ;)    local.set 7
(;@707   ;)    global.get 0
(;@709   ;)    local.get 6
(;@70b   ;)    i32.store
(;@70e   ;)    global.get 0
(;@710   ;)    local.get 7
(;@712   ;)    i32.store offset=4
(;@715   ;)    global.get 0
(;@717   ;)    global.get 0
(;@719   ;)    i32.const 8
(;@71b   ;)    i32.add
(;@71c   ;)    global.set 0
(;@71e   ;)    local.set 8
(;@720   ;)    global.get 0
(;@722   ;)    local.get 1
(;@724   ;)    i32.store
(;@727   ;)    global.get 0
(;@729   ;)    local.get 2
(;@72b   ;)    i32.store offset=4
(;@72e   ;)    global.get 0
(;@730   ;)    local.get 5
(;@732   ;)    i32.store offset=8
(;@735   ;)    global.get 0
(;@737   ;)    local.get 8
(;@739   ;)    i32.store offset=12
(;@73c   ;)    global.get 0
(;@73e   ;)    global.get 0
(;@740   ;)    i32.const 16
(;@742   ;)    i32.add
(;@743   ;)    global.set 0
             )
(;@747   ;)  (func $_row_scoped__ask_lam_0 (;29;) (type $fun_2_1) (param i32 i32) (result i32)
(;@748   ;)    (local i32)
(;@74a   ;)    local.get 1
             )
(;@74e   ;)  (func $_row_scoped__ask_lam_1 (;30;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@74f   ;)    (local i32 i32)
(;@751   ;)    local.get 2
(;@753   ;)    local.set 3
(;@755   ;)    local.get 1
(;@757   ;)    i32.const 4
(;@759   ;)    i32.add
(;@75a   ;)    local.get 3
(;@75c   ;)    local.get 1
(;@75e   ;)    i32.load
(;@761   ;)    call_indirect (type $fun_2_1)
             )
(;@766   ;)  (func $_row_scoped__ask_lam_2 (;31;) (type $fun_1_1) (param i32) (result i32)
(;@767   ;)    (local i32)
(;@769   ;)    global.get 0
             )
(;@76d   ;)  (func $_row_scoped__ask_lam_3 (;32;) (type $fun_1_1) (param i32) (result i32)
(;@76e   ;)    (local i32 i32)
(;@770   ;)    local.get 0
(;@772   ;)    i32.load
(;@775   ;)    local.set 1
(;@777   ;)    block (result i32) ;; label = @1
(;@779   ;)      block (result i32) ;; label = @2
(;@77b   ;)        local.get 1
(;@77d   ;)        local.get 1
(;@77f   ;)        br_table 0 (;@2;)
(;@782   ;)      end
(;@783   ;)      unreachable
(;@784   ;)    end
             )
(;@787   ;)  (func $_row_scoped__ask_lam_4 (;33;) (type $fun_1_1) (param i32) (result i32)
(;@788   ;)    (local i32)
(;@78a   ;)    local.get 0
             )
(;@78e   ;)  (func $_row_scoped__ask_lam_5 (;34;) (type $fun_1_1) (param i32) (result i32)
(;@78f   ;)    (local i32 i32)
(;@791   ;)    local.get 0
(;@793   ;)    local.set 1
(;@795   ;)    local.get 1
             )
(;@79a   ;)  (func $_row_simple_return_ask (;35;) (type $fun_1_1) (param i32) (result i32)
(;@79b   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@79d   ;)    global.get 0
(;@79f   ;)    i32.const 5
(;@7a1   ;)    i32.store
(;@7a4   ;)    global.get 0
(;@7a6   ;)    i32.const 36
(;@7a8   ;)    i32.store offset=4
(;@7ab   ;)    global.get 0
(;@7ad   ;)    global.get 0
(;@7af   ;)    i32.const 8
(;@7b1   ;)    i32.add
(;@7b2   ;)    global.set 0
(;@7b4   ;)    local.set 1
(;@7b6   ;)    global.get 0
(;@7b8   ;)    i32.const 7
(;@7ba   ;)    i32.store
(;@7bd   ;)    global.get 0
(;@7bf   ;)    i32.const 37
(;@7c1   ;)    i32.store offset=4
(;@7c4   ;)    global.get 0
(;@7c6   ;)    global.get 0
(;@7c8   ;)    i32.const 8
(;@7ca   ;)    i32.add
(;@7cb   ;)    global.set 0
(;@7cd   ;)    local.set 2
(;@7cf   ;)    global.get 0
(;@7d1   ;)    i32.const 4
(;@7d3   ;)    i32.store
(;@7d6   ;)    global.get 0
(;@7d8   ;)    i32.const 38
(;@7da   ;)    i32.store offset=4
(;@7dd   ;)    global.get 0
(;@7df   ;)    global.get 0
(;@7e1   ;)    i32.const 8
(;@7e3   ;)    i32.add
(;@7e4   ;)    global.set 0
(;@7e6   ;)    local.set 3
(;@7e8   ;)    global.get 0
(;@7ea   ;)    i32.const 4
(;@7ec   ;)    i32.store
(;@7ef   ;)    global.get 0
(;@7f1   ;)    i32.const 39
(;@7f3   ;)    i32.store offset=4
(;@7f6   ;)    global.get 0
(;@7f8   ;)    global.get 0
(;@7fa   ;)    i32.const 8
(;@7fc   ;)    i32.add
(;@7fd   ;)    global.set 0
(;@7ff   ;)    local.set 4
(;@801   ;)    global.get 0
(;@803   ;)    local.get 3
(;@805   ;)    i32.store
(;@808   ;)    global.get 0
(;@80a   ;)    local.get 4
(;@80c   ;)    i32.store offset=4
(;@80f   ;)    global.get 0
(;@811   ;)    global.get 0
(;@813   ;)    i32.const 8
(;@815   ;)    i32.add
(;@816   ;)    global.set 0
(;@818   ;)    local.set 5
(;@81a   ;)    global.get 0
(;@81c   ;)    i32.const 4
(;@81e   ;)    i32.store
(;@821   ;)    global.get 0
(;@823   ;)    i32.const 40
(;@825   ;)    i32.store offset=4
(;@828   ;)    global.get 0
(;@82a   ;)    global.get 0
(;@82c   ;)    i32.const 8
(;@82e   ;)    i32.add
(;@82f   ;)    global.set 0
(;@831   ;)    local.set 6
(;@833   ;)    global.get 0
(;@835   ;)    i32.const 4
(;@837   ;)    i32.store
(;@83a   ;)    global.get 0
(;@83c   ;)    i32.const 41
(;@83e   ;)    i32.store offset=4
(;@841   ;)    global.get 0
(;@843   ;)    global.get 0
(;@845   ;)    i32.const 8
(;@847   ;)    i32.add
(;@848   ;)    global.set 0
(;@84a   ;)    local.set 7
(;@84c   ;)    global.get 0
(;@84e   ;)    local.get 6
(;@850   ;)    i32.store
(;@853   ;)    global.get 0
(;@855   ;)    local.get 7
(;@857   ;)    i32.store offset=4
(;@85a   ;)    global.get 0
(;@85c   ;)    global.get 0
(;@85e   ;)    i32.const 8
(;@860   ;)    i32.add
(;@861   ;)    global.set 0
(;@863   ;)    local.set 8
(;@865   ;)    global.get 0
(;@867   ;)    local.get 1
(;@869   ;)    i32.store
(;@86c   ;)    global.get 0
(;@86e   ;)    local.get 2
(;@870   ;)    i32.store offset=4
(;@873   ;)    global.get 0
(;@875   ;)    local.get 5
(;@877   ;)    i32.store offset=8
(;@87a   ;)    global.get 0
(;@87c   ;)    local.get 8
(;@87e   ;)    i32.store offset=12
(;@881   ;)    global.get 0
(;@883   ;)    global.get 0
(;@885   ;)    i32.const 16
(;@887   ;)    i32.add
(;@888   ;)    global.set 0
             )
(;@88c   ;)  (func $_row_simple_return_ask_lam_0 (;36;) (type $fun_2_1) (param i32 i32) (result i32)
(;@88d   ;)    (local i32)
(;@88f   ;)    global.get 0
(;@891   ;)    local.get 1
(;@893   ;)    i32.store
(;@896   ;)    global.get 0
(;@898   ;)    local.get 0
(;@89a   ;)    i32.store offset=4
(;@89d   ;)    global.get 0
(;@89f   ;)    global.get 0
(;@8a1   ;)    i32.const 8
(;@8a3   ;)    i32.add
(;@8a4   ;)    global.set 0
             )
(;@8a8   ;)  (func $_row_simple_return_ask_lam_1 (;37;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@8a9   ;)    (local i32 i32 i32)
(;@8ab   ;)    local.get 2
(;@8ad   ;)    i32.load
(;@8b0   ;)    local.set 3
(;@8b2   ;)    block (result i32) ;; label = @1
(;@8b4   ;)      block (result i32) ;; label = @2
(;@8b6   ;)        block (result i32) ;; label = @3
(;@8b8   ;)          block (result i32) ;; label = @4
(;@8ba   ;)            local.get 3
(;@8bc   ;)            local.get 3
(;@8be   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@8c3   ;)          end
(;@8c4   ;)          local.get 2
(;@8c6   ;)          i32.load offset=4
(;@8c9   ;)          local.set 4
(;@8cb   ;)          local.get 1
(;@8cd   ;)          i32.const 4
(;@8cf   ;)          i32.add
(;@8d0   ;)          local.get 4
(;@8d2   ;)          local.get 1
(;@8d4   ;)          i32.load
(;@8d7   ;)          call_indirect (type $fun_2_1)
(;@8da   ;)          br 2 (;@1;)
(;@8dc   ;)        end
(;@8dd   ;)        local.get 2
(;@8df   ;)        i32.load offset=4
(;@8e2   ;)        local.set 4
(;@8e4   ;)        local.get 0
(;@8e6   ;)        i32.const 4
(;@8e8   ;)        i32.add
(;@8e9   ;)        local.get 4
(;@8eb   ;)        local.get 0
(;@8ed   ;)        i32.load
(;@8f0   ;)        call_indirect (type $fun_2_1)
(;@8f3   ;)        br 1 (;@1;)
(;@8f5   ;)      end
(;@8f6   ;)      unreachable
(;@8f7   ;)    end
             )
(;@8fa   ;)  (func $_row_simple_return_ask_lam_2 (;38;) (type $fun_1_1) (param i32) (result i32)
(;@8fb   ;)    (local i32)
(;@8fd   ;)    local.get 0
(;@8ff   ;)    i32.load offset=4
             )
(;@904   ;)  (func $_row_simple_return_ask_lam_3 (;39;) (type $fun_1_1) (param i32) (result i32)
(;@905   ;)    (local i32 i32)
(;@907   ;)    local.get 0
(;@909   ;)    local.set 1
(;@90b   ;)    global.get 0
(;@90d   ;)    i32.const 1
(;@90f   ;)    i32.store
(;@912   ;)    global.get 0
(;@914   ;)    local.get 1
(;@916   ;)    i32.store offset=4
(;@919   ;)    global.get 0
(;@91b   ;)    global.get 0
(;@91d   ;)    i32.const 8
(;@91f   ;)    i32.add
(;@920   ;)    global.set 0
             )
(;@924   ;)  (func $_row_simple_return_ask_lam_4 (;40;) (type $fun_1_1) (param i32) (result i32)
(;@925   ;)    (local i32)
(;@927   ;)    local.get 0
(;@929   ;)    i32.load
             )
(;@92e   ;)  (func $_row_simple_return_ask_lam_5 (;41;) (type $fun_1_1) (param i32) (result i32)
(;@92f   ;)    (local i32 i32)
(;@931   ;)    local.get 0
(;@933   ;)    local.set 1
(;@935   ;)    global.get 0
(;@937   ;)    i32.const 0
(;@939   ;)    i32.store
(;@93c   ;)    global.get 0
(;@93e   ;)    local.get 1
(;@940   ;)    i32.store offset=4
(;@943   ;)    global.get 0
(;@945   ;)    global.get 0
(;@947   ;)    i32.const 8
(;@949   ;)    i32.add
(;@94a   ;)    global.set 0
             )
(;@94f   ;)  (func $_row_scoped_return_ask (;42;) (type $fun_1_1) (param i32) (result i32)
(;@950   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@952   ;)    global.get 0
(;@954   ;)    i32.const 5
(;@956   ;)    i32.store
(;@959   ;)    global.get 0
(;@95b   ;)    i32.const 43
(;@95d   ;)    i32.store offset=4
(;@960   ;)    global.get 0
(;@962   ;)    global.get 0
(;@964   ;)    i32.const 8
(;@966   ;)    i32.add
(;@967   ;)    global.set 0
(;@969   ;)    local.set 1
(;@96b   ;)    global.get 0
(;@96d   ;)    i32.const 7
(;@96f   ;)    i32.store
(;@972   ;)    global.get 0
(;@974   ;)    i32.const 44
(;@976   ;)    i32.store offset=4
(;@979   ;)    global.get 0
(;@97b   ;)    global.get 0
(;@97d   ;)    i32.const 8
(;@97f   ;)    i32.add
(;@980   ;)    global.set 0
(;@982   ;)    local.set 2
(;@984   ;)    global.get 0
(;@986   ;)    i32.const 4
(;@988   ;)    i32.store
(;@98b   ;)    global.get 0
(;@98d   ;)    i32.const 45
(;@98f   ;)    i32.store offset=4
(;@992   ;)    global.get 0
(;@994   ;)    global.get 0
(;@996   ;)    i32.const 8
(;@998   ;)    i32.add
(;@999   ;)    global.set 0
(;@99b   ;)    local.set 3
(;@99d   ;)    global.get 0
(;@99f   ;)    i32.const 4
(;@9a1   ;)    i32.store
(;@9a4   ;)    global.get 0
(;@9a6   ;)    i32.const 46
(;@9a8   ;)    i32.store offset=4
(;@9ab   ;)    global.get 0
(;@9ad   ;)    global.get 0
(;@9af   ;)    i32.const 8
(;@9b1   ;)    i32.add
(;@9b2   ;)    global.set 0
(;@9b4   ;)    local.set 4
(;@9b6   ;)    global.get 0
(;@9b8   ;)    local.get 3
(;@9ba   ;)    i32.store
(;@9bd   ;)    global.get 0
(;@9bf   ;)    local.get 4
(;@9c1   ;)    i32.store offset=4
(;@9c4   ;)    global.get 0
(;@9c6   ;)    global.get 0
(;@9c8   ;)    i32.const 8
(;@9ca   ;)    i32.add
(;@9cb   ;)    global.set 0
(;@9cd   ;)    local.set 5
(;@9cf   ;)    global.get 0
(;@9d1   ;)    i32.const 4
(;@9d3   ;)    i32.store
(;@9d6   ;)    global.get 0
(;@9d8   ;)    i32.const 47
(;@9da   ;)    i32.store offset=4
(;@9dd   ;)    global.get 0
(;@9df   ;)    global.get 0
(;@9e1   ;)    i32.const 8
(;@9e3   ;)    i32.add
(;@9e4   ;)    global.set 0
(;@9e6   ;)    local.set 6
(;@9e8   ;)    global.get 0
(;@9ea   ;)    i32.const 4
(;@9ec   ;)    i32.store
(;@9ef   ;)    global.get 0
(;@9f1   ;)    i32.const 48
(;@9f3   ;)    i32.store offset=4
(;@9f6   ;)    global.get 0
(;@9f8   ;)    global.get 0
(;@9fa   ;)    i32.const 8
(;@9fc   ;)    i32.add
(;@9fd   ;)    global.set 0
(;@9ff   ;)    local.set 7
(;@a01   ;)    global.get 0
(;@a03   ;)    local.get 6
(;@a05   ;)    i32.store
(;@a08   ;)    global.get 0
(;@a0a   ;)    local.get 7
(;@a0c   ;)    i32.store offset=4
(;@a0f   ;)    global.get 0
(;@a11   ;)    global.get 0
(;@a13   ;)    i32.const 8
(;@a15   ;)    i32.add
(;@a16   ;)    global.set 0
(;@a18   ;)    local.set 8
(;@a1a   ;)    global.get 0
(;@a1c   ;)    local.get 1
(;@a1e   ;)    i32.store
(;@a21   ;)    global.get 0
(;@a23   ;)    local.get 2
(;@a25   ;)    i32.store offset=4
(;@a28   ;)    global.get 0
(;@a2a   ;)    local.get 5
(;@a2c   ;)    i32.store offset=8
(;@a2f   ;)    global.get 0
(;@a31   ;)    local.get 8
(;@a33   ;)    i32.store offset=12
(;@a36   ;)    global.get 0
(;@a38   ;)    global.get 0
(;@a3a   ;)    i32.const 16
(;@a3c   ;)    i32.add
(;@a3d   ;)    global.set 0
             )
(;@a41   ;)  (func $_row_scoped_return_ask_lam_0 (;43;) (type $fun_2_1) (param i32 i32) (result i32)
(;@a42   ;)    (local i32)
(;@a44   ;)    global.get 0
(;@a46   ;)    local.get 1
(;@a48   ;)    i32.store
(;@a4b   ;)    global.get 0
(;@a4d   ;)    local.get 0
(;@a4f   ;)    i32.store offset=4
(;@a52   ;)    global.get 0
(;@a54   ;)    global.get 0
(;@a56   ;)    i32.const 8
(;@a58   ;)    i32.add
(;@a59   ;)    global.set 0
             )
(;@a5d   ;)  (func $_row_scoped_return_ask_lam_1 (;44;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@a5e   ;)    (local i32 i32 i32)
(;@a60   ;)    local.get 2
(;@a62   ;)    i32.load
(;@a65   ;)    local.set 3
(;@a67   ;)    block (result i32) ;; label = @1
(;@a69   ;)      block (result i32) ;; label = @2
(;@a6b   ;)        block (result i32) ;; label = @3
(;@a6d   ;)          block (result i32) ;; label = @4
(;@a6f   ;)            local.get 3
(;@a71   ;)            local.get 3
(;@a73   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@a78   ;)          end
(;@a79   ;)          local.get 2
(;@a7b   ;)          i32.load offset=4
(;@a7e   ;)          local.set 4
(;@a80   ;)          local.get 1
(;@a82   ;)          i32.const 4
(;@a84   ;)          i32.add
(;@a85   ;)          local.get 4
(;@a87   ;)          local.get 1
(;@a89   ;)          i32.load
(;@a8c   ;)          call_indirect (type $fun_2_1)
(;@a8f   ;)          br 2 (;@1;)
(;@a91   ;)        end
(;@a92   ;)        local.get 2
(;@a94   ;)        i32.load offset=4
(;@a97   ;)        local.set 4
(;@a99   ;)        local.get 0
(;@a9b   ;)        i32.const 4
(;@a9d   ;)        i32.add
(;@a9e   ;)        local.get 4
(;@aa0   ;)        local.get 0
(;@aa2   ;)        i32.load
(;@aa5   ;)        call_indirect (type $fun_2_1)
(;@aa8   ;)        br 1 (;@1;)
(;@aaa   ;)      end
(;@aab   ;)      unreachable
(;@aac   ;)    end
             )
(;@aaf   ;)  (func $_row_scoped_return_ask_lam_2 (;45;) (type $fun_1_1) (param i32) (result i32)
(;@ab0   ;)    (local i32)
(;@ab2   ;)    local.get 0
(;@ab4   ;)    i32.load offset=4
             )
(;@ab9   ;)  (func $_row_scoped_return_ask_lam_3 (;46;) (type $fun_1_1) (param i32) (result i32)
(;@aba   ;)    (local i32 i32)
(;@abc   ;)    local.get 0
(;@abe   ;)    local.set 1
(;@ac0   ;)    global.get 0
(;@ac2   ;)    i32.const 1
(;@ac4   ;)    i32.store
(;@ac7   ;)    global.get 0
(;@ac9   ;)    local.get 1
(;@acb   ;)    i32.store offset=4
(;@ace   ;)    global.get 0
(;@ad0   ;)    global.get 0
(;@ad2   ;)    i32.const 8
(;@ad4   ;)    i32.add
(;@ad5   ;)    global.set 0
             )
(;@ad9   ;)  (func $_row_scoped_return_ask_lam_4 (;47;) (type $fun_1_1) (param i32) (result i32)
(;@ada   ;)    (local i32)
(;@adc   ;)    local.get 0
(;@ade   ;)    i32.load
             )
(;@ae3   ;)  (func $_row_scoped_return_ask_lam_5 (;48;) (type $fun_1_1) (param i32) (result i32)
(;@ae4   ;)    (local i32 i32)
(;@ae6   ;)    local.get 0
(;@ae8   ;)    local.set 1
(;@aea   ;)    global.get 0
(;@aec   ;)    i32.const 0
(;@aee   ;)    i32.store
(;@af1   ;)    global.get 0
(;@af3   ;)    local.get 1
(;@af5   ;)    i32.store offset=4
(;@af8   ;)    global.get 0
(;@afa   ;)    global.get 0
(;@afc   ;)    i32.const 8
(;@afe   ;)    i32.add
(;@aff   ;)    global.set 0
             )
(;@b04   ;)  (func $_row_simple__Reader (;49;) (type $fun_1_1) (param i32) (result i32)
(;@b05   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@b07   ;)    global.get 0
(;@b09   ;)    i32.const 5
(;@b0b   ;)    i32.store
(;@b0e   ;)    global.get 0
(;@b10   ;)    i32.const 50
(;@b12   ;)    i32.store offset=4
(;@b15   ;)    global.get 0
(;@b17   ;)    global.get 0
(;@b19   ;)    i32.const 8
(;@b1b   ;)    i32.add
(;@b1c   ;)    global.set 0
(;@b1e   ;)    local.set 1
(;@b20   ;)    global.get 0
(;@b22   ;)    i32.const 7
(;@b24   ;)    i32.store
(;@b27   ;)    global.get 0
(;@b29   ;)    i32.const 51
(;@b2b   ;)    i32.store offset=4
(;@b2e   ;)    global.get 0
(;@b30   ;)    global.get 0
(;@b32   ;)    i32.const 8
(;@b34   ;)    i32.add
(;@b35   ;)    global.set 0
(;@b37   ;)    local.set 2
(;@b39   ;)    global.get 0
(;@b3b   ;)    i32.const 4
(;@b3d   ;)    i32.store
(;@b40   ;)    global.get 0
(;@b42   ;)    i32.const 52
(;@b44   ;)    i32.store offset=4
(;@b47   ;)    global.get 0
(;@b49   ;)    global.get 0
(;@b4b   ;)    i32.const 8
(;@b4d   ;)    i32.add
(;@b4e   ;)    global.set 0
(;@b50   ;)    local.set 3
(;@b52   ;)    global.get 0
(;@b54   ;)    i32.const 4
(;@b56   ;)    i32.store
(;@b59   ;)    global.get 0
(;@b5b   ;)    i32.const 53
(;@b5d   ;)    i32.store offset=4
(;@b60   ;)    global.get 0
(;@b62   ;)    global.get 0
(;@b64   ;)    i32.const 8
(;@b66   ;)    i32.add
(;@b67   ;)    global.set 0
(;@b69   ;)    local.set 4
(;@b6b   ;)    global.get 0
(;@b6d   ;)    local.get 3
(;@b6f   ;)    i32.store
(;@b72   ;)    global.get 0
(;@b74   ;)    local.get 4
(;@b76   ;)    i32.store offset=4
(;@b79   ;)    global.get 0
(;@b7b   ;)    global.get 0
(;@b7d   ;)    i32.const 8
(;@b7f   ;)    i32.add
(;@b80   ;)    global.set 0
(;@b82   ;)    local.set 5
(;@b84   ;)    global.get 0
(;@b86   ;)    i32.const 4
(;@b88   ;)    i32.store
(;@b8b   ;)    global.get 0
(;@b8d   ;)    i32.const 54
(;@b8f   ;)    i32.store offset=4
(;@b92   ;)    global.get 0
(;@b94   ;)    global.get 0
(;@b96   ;)    i32.const 8
(;@b98   ;)    i32.add
(;@b99   ;)    global.set 0
(;@b9b   ;)    local.set 6
(;@b9d   ;)    global.get 0
(;@b9f   ;)    i32.const 4
(;@ba1   ;)    i32.store
(;@ba4   ;)    global.get 0
(;@ba6   ;)    i32.const 55
(;@ba8   ;)    i32.store offset=4
(;@bab   ;)    global.get 0
(;@bad   ;)    global.get 0
(;@baf   ;)    i32.const 8
(;@bb1   ;)    i32.add
(;@bb2   ;)    global.set 0
(;@bb4   ;)    local.set 7
(;@bb6   ;)    global.get 0
(;@bb8   ;)    local.get 6
(;@bba   ;)    i32.store
(;@bbd   ;)    global.get 0
(;@bbf   ;)    local.get 7
(;@bc1   ;)    i32.store offset=4
(;@bc4   ;)    global.get 0
(;@bc6   ;)    global.get 0
(;@bc8   ;)    i32.const 8
(;@bca   ;)    i32.add
(;@bcb   ;)    global.set 0
(;@bcd   ;)    local.set 8
(;@bcf   ;)    global.get 0
(;@bd1   ;)    local.get 1
(;@bd3   ;)    i32.store
(;@bd6   ;)    global.get 0
(;@bd8   ;)    local.get 2
(;@bda   ;)    i32.store offset=4
(;@bdd   ;)    global.get 0
(;@bdf   ;)    local.get 5
(;@be1   ;)    i32.store offset=8
(;@be4   ;)    global.get 0
(;@be6   ;)    local.get 8
(;@be8   ;)    i32.store offset=12
(;@beb   ;)    global.get 0
(;@bed   ;)    global.get 0
(;@bef   ;)    i32.const 16
(;@bf1   ;)    i32.add
(;@bf2   ;)    global.set 0
             )
(;@bf6   ;)  (func $_row_simple__Reader_lam_0 (;50;) (type $fun_2_1) (param i32 i32) (result i32)
(;@bf7   ;)    (local i32)
(;@bf9   ;)    local.get 1
             )
(;@bfd   ;)  (func $_row_simple__Reader_lam_1 (;51;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@bfe   ;)    (local i32 i32)
(;@c00   ;)    local.get 2
(;@c02   ;)    local.set 3
(;@c04   ;)    local.get 1
(;@c06   ;)    i32.const 4
(;@c08   ;)    i32.add
(;@c09   ;)    local.get 3
(;@c0b   ;)    local.get 1
(;@c0d   ;)    i32.load
(;@c10   ;)    call_indirect (type $fun_2_1)
             )
(;@c15   ;)  (func $_row_simple__Reader_lam_2 (;52;) (type $fun_1_1) (param i32) (result i32)
(;@c16   ;)    (local i32)
(;@c18   ;)    global.get 0
             )
(;@c1c   ;)  (func $_row_simple__Reader_lam_3 (;53;) (type $fun_1_1) (param i32) (result i32)
(;@c1d   ;)    (local i32 i32)
(;@c1f   ;)    local.get 0
(;@c21   ;)    i32.load
(;@c24   ;)    local.set 1
(;@c26   ;)    block (result i32) ;; label = @1
(;@c28   ;)      block (result i32) ;; label = @2
(;@c2a   ;)        local.get 1
(;@c2c   ;)        local.get 1
(;@c2e   ;)        br_table 0 (;@2;)
(;@c31   ;)      end
(;@c32   ;)      unreachable
(;@c33   ;)    end
             )
(;@c36   ;)  (func $_row_simple__Reader_lam_4 (;54;) (type $fun_1_1) (param i32) (result i32)
(;@c37   ;)    (local i32)
(;@c39   ;)    local.get 0
             )
(;@c3d   ;)  (func $_row_simple__Reader_lam_5 (;55;) (type $fun_1_1) (param i32) (result i32)
(;@c3e   ;)    (local i32 i32)
(;@c40   ;)    local.get 0
(;@c42   ;)    local.set 1
(;@c44   ;)    local.get 1
             )
(;@c49   ;)  (func $_row_scoped__Reader (;56;) (type $fun_1_1) (param i32) (result i32)
(;@c4a   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@c4c   ;)    global.get 0
(;@c4e   ;)    i32.const 5
(;@c50   ;)    i32.store
(;@c53   ;)    global.get 0
(;@c55   ;)    i32.const 57
(;@c57   ;)    i32.store offset=4
(;@c5a   ;)    global.get 0
(;@c5c   ;)    global.get 0
(;@c5e   ;)    i32.const 8
(;@c60   ;)    i32.add
(;@c61   ;)    global.set 0
(;@c63   ;)    local.set 1
(;@c65   ;)    global.get 0
(;@c67   ;)    i32.const 7
(;@c69   ;)    i32.store
(;@c6c   ;)    global.get 0
(;@c6e   ;)    i32.const 58
(;@c70   ;)    i32.store offset=4
(;@c73   ;)    global.get 0
(;@c75   ;)    global.get 0
(;@c77   ;)    i32.const 8
(;@c79   ;)    i32.add
(;@c7a   ;)    global.set 0
(;@c7c   ;)    local.set 2
(;@c7e   ;)    global.get 0
(;@c80   ;)    i32.const 4
(;@c82   ;)    i32.store
(;@c85   ;)    global.get 0
(;@c87   ;)    i32.const 59
(;@c89   ;)    i32.store offset=4
(;@c8c   ;)    global.get 0
(;@c8e   ;)    global.get 0
(;@c90   ;)    i32.const 8
(;@c92   ;)    i32.add
(;@c93   ;)    global.set 0
(;@c95   ;)    local.set 3
(;@c97   ;)    global.get 0
(;@c99   ;)    i32.const 4
(;@c9b   ;)    i32.store
(;@c9e   ;)    global.get 0
(;@ca0   ;)    i32.const 60
(;@ca2   ;)    i32.store offset=4
(;@ca5   ;)    global.get 0
(;@ca7   ;)    global.get 0
(;@ca9   ;)    i32.const 8
(;@cab   ;)    i32.add
(;@cac   ;)    global.set 0
(;@cae   ;)    local.set 4
(;@cb0   ;)    global.get 0
(;@cb2   ;)    local.get 3
(;@cb4   ;)    i32.store
(;@cb7   ;)    global.get 0
(;@cb9   ;)    local.get 4
(;@cbb   ;)    i32.store offset=4
(;@cbe   ;)    global.get 0
(;@cc0   ;)    global.get 0
(;@cc2   ;)    i32.const 8
(;@cc4   ;)    i32.add
(;@cc5   ;)    global.set 0
(;@cc7   ;)    local.set 5
(;@cc9   ;)    global.get 0
(;@ccb   ;)    i32.const 4
(;@ccd   ;)    i32.store
(;@cd0   ;)    global.get 0
(;@cd2   ;)    i32.const 61
(;@cd4   ;)    i32.store offset=4
(;@cd7   ;)    global.get 0
(;@cd9   ;)    global.get 0
(;@cdb   ;)    i32.const 8
(;@cdd   ;)    i32.add
(;@cde   ;)    global.set 0
(;@ce0   ;)    local.set 6
(;@ce2   ;)    global.get 0
(;@ce4   ;)    i32.const 4
(;@ce6   ;)    i32.store
(;@ce9   ;)    global.get 0
(;@ceb   ;)    i32.const 62
(;@ced   ;)    i32.store offset=4
(;@cf0   ;)    global.get 0
(;@cf2   ;)    global.get 0
(;@cf4   ;)    i32.const 8
(;@cf6   ;)    i32.add
(;@cf7   ;)    global.set 0
(;@cf9   ;)    local.set 7
(;@cfb   ;)    global.get 0
(;@cfd   ;)    local.get 6
(;@cff   ;)    i32.store
(;@d02   ;)    global.get 0
(;@d04   ;)    local.get 7
(;@d06   ;)    i32.store offset=4
(;@d09   ;)    global.get 0
(;@d0b   ;)    global.get 0
(;@d0d   ;)    i32.const 8
(;@d0f   ;)    i32.add
(;@d10   ;)    global.set 0
(;@d12   ;)    local.set 8
(;@d14   ;)    global.get 0
(;@d16   ;)    local.get 1
(;@d18   ;)    i32.store
(;@d1b   ;)    global.get 0
(;@d1d   ;)    local.get 2
(;@d1f   ;)    i32.store offset=4
(;@d22   ;)    global.get 0
(;@d24   ;)    local.get 5
(;@d26   ;)    i32.store offset=8
(;@d29   ;)    global.get 0
(;@d2b   ;)    local.get 8
(;@d2d   ;)    i32.store offset=12
(;@d30   ;)    global.get 0
(;@d32   ;)    global.get 0
(;@d34   ;)    i32.const 16
(;@d36   ;)    i32.add
(;@d37   ;)    global.set 0
             )
(;@d3b   ;)  (func $_row_scoped__Reader_lam_0 (;57;) (type $fun_2_1) (param i32 i32) (result i32)
(;@d3c   ;)    (local i32)
(;@d3e   ;)    local.get 1
             )
(;@d42   ;)  (func $_row_scoped__Reader_lam_1 (;58;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@d43   ;)    (local i32 i32)
(;@d45   ;)    local.get 2
(;@d47   ;)    local.set 3
(;@d49   ;)    local.get 1
(;@d4b   ;)    i32.const 4
(;@d4d   ;)    i32.add
(;@d4e   ;)    local.get 3
(;@d50   ;)    local.get 1
(;@d52   ;)    i32.load
(;@d55   ;)    call_indirect (type $fun_2_1)
             )
(;@d5a   ;)  (func $_row_scoped__Reader_lam_2 (;59;) (type $fun_1_1) (param i32) (result i32)
(;@d5b   ;)    (local i32)
(;@d5d   ;)    global.get 0
             )
(;@d61   ;)  (func $_row_scoped__Reader_lam_3 (;60;) (type $fun_1_1) (param i32) (result i32)
(;@d62   ;)    (local i32 i32)
(;@d64   ;)    local.get 0
(;@d66   ;)    i32.load
(;@d69   ;)    local.set 1
(;@d6b   ;)    block (result i32) ;; label = @1
(;@d6d   ;)      block (result i32) ;; label = @2
(;@d6f   ;)        local.get 1
(;@d71   ;)        local.get 1
(;@d73   ;)        br_table 0 (;@2;)
(;@d76   ;)      end
(;@d77   ;)      unreachable
(;@d78   ;)    end
             )
(;@d7b   ;)  (func $_row_scoped__Reader_lam_4 (;61;) (type $fun_1_1) (param i32) (result i32)
(;@d7c   ;)    (local i32)
(;@d7e   ;)    local.get 0
             )
(;@d82   ;)  (func $_row_scoped__Reader_lam_5 (;62;) (type $fun_1_1) (param i32) (result i32)
(;@d83   ;)    (local i32 i32)
(;@d85   ;)    local.get 0
(;@d87   ;)    local.set 1
(;@d89   ;)    local.get 1
             )
(;@d8e   ;)  (func $_row_simple_ask_return (;63;) (type $fun_1_1) (param i32) (result i32)
(;@d8f   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@d91   ;)    global.get 0
(;@d93   ;)    i32.const 5
(;@d95   ;)    i32.store
(;@d98   ;)    global.get 0
(;@d9a   ;)    i32.const 64
(;@d9d   ;)    i32.store offset=4
(;@da0   ;)    global.get 0
(;@da2   ;)    global.get 0
(;@da4   ;)    i32.const 8
(;@da6   ;)    i32.add
(;@da7   ;)    global.set 0
(;@da9   ;)    local.set 1
(;@dab   ;)    global.get 0
(;@dad   ;)    i32.const 7
(;@daf   ;)    i32.store
(;@db2   ;)    global.get 0
(;@db4   ;)    i32.const 65
(;@db7   ;)    i32.store offset=4
(;@dba   ;)    global.get 0
(;@dbc   ;)    global.get 0
(;@dbe   ;)    i32.const 8
(;@dc0   ;)    i32.add
(;@dc1   ;)    global.set 0
(;@dc3   ;)    local.set 2
(;@dc5   ;)    global.get 0
(;@dc7   ;)    i32.const 4
(;@dc9   ;)    i32.store
(;@dcc   ;)    global.get 0
(;@dce   ;)    i32.const 66
(;@dd1   ;)    i32.store offset=4
(;@dd4   ;)    global.get 0
(;@dd6   ;)    global.get 0
(;@dd8   ;)    i32.const 8
(;@dda   ;)    i32.add
(;@ddb   ;)    global.set 0
(;@ddd   ;)    local.set 3
(;@ddf   ;)    global.get 0
(;@de1   ;)    i32.const 4
(;@de3   ;)    i32.store
(;@de6   ;)    global.get 0
(;@de8   ;)    i32.const 67
(;@deb   ;)    i32.store offset=4
(;@dee   ;)    global.get 0
(;@df0   ;)    global.get 0
(;@df2   ;)    i32.const 8
(;@df4   ;)    i32.add
(;@df5   ;)    global.set 0
(;@df7   ;)    local.set 4
(;@df9   ;)    global.get 0
(;@dfb   ;)    local.get 3
(;@dfd   ;)    i32.store
(;@e00   ;)    global.get 0
(;@e02   ;)    local.get 4
(;@e04   ;)    i32.store offset=4
(;@e07   ;)    global.get 0
(;@e09   ;)    global.get 0
(;@e0b   ;)    i32.const 8
(;@e0d   ;)    i32.add
(;@e0e   ;)    global.set 0
(;@e10   ;)    local.set 5
(;@e12   ;)    global.get 0
(;@e14   ;)    i32.const 4
(;@e16   ;)    i32.store
(;@e19   ;)    global.get 0
(;@e1b   ;)    i32.const 68
(;@e1e   ;)    i32.store offset=4
(;@e21   ;)    global.get 0
(;@e23   ;)    global.get 0
(;@e25   ;)    i32.const 8
(;@e27   ;)    i32.add
(;@e28   ;)    global.set 0
(;@e2a   ;)    local.set 6
(;@e2c   ;)    global.get 0
(;@e2e   ;)    i32.const 4
(;@e30   ;)    i32.store
(;@e33   ;)    global.get 0
(;@e35   ;)    i32.const 69
(;@e38   ;)    i32.store offset=4
(;@e3b   ;)    global.get 0
(;@e3d   ;)    global.get 0
(;@e3f   ;)    i32.const 8
(;@e41   ;)    i32.add
(;@e42   ;)    global.set 0
(;@e44   ;)    local.set 7
(;@e46   ;)    global.get 0
(;@e48   ;)    local.get 6
(;@e4a   ;)    i32.store
(;@e4d   ;)    global.get 0
(;@e4f   ;)    local.get 7
(;@e51   ;)    i32.store offset=4
(;@e54   ;)    global.get 0
(;@e56   ;)    global.get 0
(;@e58   ;)    i32.const 8
(;@e5a   ;)    i32.add
(;@e5b   ;)    global.set 0
(;@e5d   ;)    local.set 8
(;@e5f   ;)    global.get 0
(;@e61   ;)    local.get 1
(;@e63   ;)    i32.store
(;@e66   ;)    global.get 0
(;@e68   ;)    local.get 2
(;@e6a   ;)    i32.store offset=4
(;@e6d   ;)    global.get 0
(;@e6f   ;)    local.get 5
(;@e71   ;)    i32.store offset=8
(;@e74   ;)    global.get 0
(;@e76   ;)    local.get 8
(;@e78   ;)    i32.store offset=12
(;@e7b   ;)    global.get 0
(;@e7d   ;)    global.get 0
(;@e7f   ;)    i32.const 16
(;@e81   ;)    i32.add
(;@e82   ;)    global.set 0
             )
(;@e86   ;)  (func $_row_simple_ask_return_lam_0 (;64;) (type $fun_2_1) (param i32 i32) (result i32)
(;@e87   ;)    (local i32)
(;@e89   ;)    global.get 0
(;@e8b   ;)    local.get 0
(;@e8d   ;)    i32.store
(;@e90   ;)    global.get 0
(;@e92   ;)    local.get 1
(;@e94   ;)    i32.store offset=4
(;@e97   ;)    global.get 0
(;@e99   ;)    global.get 0
(;@e9b   ;)    i32.const 8
(;@e9d   ;)    i32.add
(;@e9e   ;)    global.set 0
             )
(;@ea2   ;)  (func $_row_simple_ask_return_lam_1 (;65;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@ea3   ;)    (local i32 i32 i32)
(;@ea5   ;)    local.get 2
(;@ea7   ;)    i32.load
(;@eaa   ;)    local.set 3
(;@eac   ;)    block (result i32) ;; label = @1
(;@eae   ;)      block (result i32) ;; label = @2
(;@eb0   ;)        block (result i32) ;; label = @3
(;@eb2   ;)          block (result i32) ;; label = @4
(;@eb4   ;)            local.get 3
(;@eb6   ;)            local.get 3
(;@eb8   ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@ebd   ;)          end
(;@ebe   ;)          local.get 2
(;@ec0   ;)          i32.load offset=4
(;@ec3   ;)          local.set 4
(;@ec5   ;)          local.get 0
(;@ec7   ;)          i32.const 4
(;@ec9   ;)          i32.add
(;@eca   ;)          local.get 4
(;@ecc   ;)          local.get 0
(;@ece   ;)          i32.load
(;@ed1   ;)          call_indirect (type $fun_2_1)
(;@ed4   ;)          br 2 (;@1;)
(;@ed6   ;)        end
(;@ed7   ;)        local.get 2
(;@ed9   ;)        i32.load offset=4
(;@edc   ;)        local.set 4
(;@ede   ;)        local.get 1
(;@ee0   ;)        i32.const 4
(;@ee2   ;)        i32.add
(;@ee3   ;)        local.get 4
(;@ee5   ;)        local.get 1
(;@ee7   ;)        i32.load
(;@eea   ;)        call_indirect (type $fun_2_1)
(;@eed   ;)        br 1 (;@1;)
(;@eef   ;)      end
(;@ef0   ;)      unreachable
(;@ef1   ;)    end
             )
(;@ef4   ;)  (func $_row_simple_ask_return_lam_2 (;66;) (type $fun_1_1) (param i32) (result i32)
(;@ef5   ;)    (local i32)
(;@ef7   ;)    local.get 0
(;@ef9   ;)    i32.load
             )
(;@efe   ;)  (func $_row_simple_ask_return_lam_3 (;67;) (type $fun_1_1) (param i32) (result i32)
(;@eff   ;)    (local i32 i32)
(;@f01   ;)    local.get 0
(;@f03   ;)    local.set 1
(;@f05   ;)    global.get 0
(;@f07   ;)    i32.const 0
(;@f09   ;)    i32.store
(;@f0c   ;)    global.get 0
(;@f0e   ;)    local.get 1
(;@f10   ;)    i32.store offset=4
(;@f13   ;)    global.get 0
(;@f15   ;)    global.get 0
(;@f17   ;)    i32.const 8
(;@f19   ;)    i32.add
(;@f1a   ;)    global.set 0
             )
(;@f1e   ;)  (func $_row_simple_ask_return_lam_4 (;68;) (type $fun_1_1) (param i32) (result i32)
(;@f1f   ;)    (local i32)
(;@f21   ;)    local.get 0
(;@f23   ;)    i32.load offset=4
             )
(;@f28   ;)  (func $_row_simple_ask_return_lam_5 (;69;) (type $fun_1_1) (param i32) (result i32)
(;@f29   ;)    (local i32 i32)
(;@f2b   ;)    local.get 0
(;@f2d   ;)    local.set 1
(;@f2f   ;)    global.get 0
(;@f31   ;)    i32.const 1
(;@f33   ;)    i32.store
(;@f36   ;)    global.get 0
(;@f38   ;)    local.get 1
(;@f3a   ;)    i32.store offset=4
(;@f3d   ;)    global.get 0
(;@f3f   ;)    global.get 0
(;@f41   ;)    i32.const 8
(;@f43   ;)    i32.add
(;@f44   ;)    global.set 0
             )
(;@f49   ;)  (func $_row_scoped_ask_return (;70;) (type $fun_1_1) (param i32) (result i32)
(;@f4a   ;)    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
(;@f4c   ;)    global.get 0
(;@f4e   ;)    i32.const 5
(;@f50   ;)    i32.store
(;@f53   ;)    global.get 0
(;@f55   ;)    i32.const 71
(;@f58   ;)    i32.store offset=4
(;@f5b   ;)    global.get 0
(;@f5d   ;)    global.get 0
(;@f5f   ;)    i32.const 8
(;@f61   ;)    i32.add
(;@f62   ;)    global.set 0
(;@f64   ;)    local.set 1
(;@f66   ;)    global.get 0
(;@f68   ;)    i32.const 7
(;@f6a   ;)    i32.store
(;@f6d   ;)    global.get 0
(;@f6f   ;)    i32.const 72
(;@f72   ;)    i32.store offset=4
(;@f75   ;)    global.get 0
(;@f77   ;)    global.get 0
(;@f79   ;)    i32.const 8
(;@f7b   ;)    i32.add
(;@f7c   ;)    global.set 0
(;@f7e   ;)    local.set 2
(;@f80   ;)    global.get 0
(;@f82   ;)    i32.const 4
(;@f84   ;)    i32.store
(;@f87   ;)    global.get 0
(;@f89   ;)    i32.const 73
(;@f8c   ;)    i32.store offset=4
(;@f8f   ;)    global.get 0
(;@f91   ;)    global.get 0
(;@f93   ;)    i32.const 8
(;@f95   ;)    i32.add
(;@f96   ;)    global.set 0
(;@f98   ;)    local.set 3
(;@f9a   ;)    global.get 0
(;@f9c   ;)    i32.const 4
(;@f9e   ;)    i32.store
(;@fa1   ;)    global.get 0
(;@fa3   ;)    i32.const 74
(;@fa6   ;)    i32.store offset=4
(;@fa9   ;)    global.get 0
(;@fab   ;)    global.get 0
(;@fad   ;)    i32.const 8
(;@faf   ;)    i32.add
(;@fb0   ;)    global.set 0
(;@fb2   ;)    local.set 4
(;@fb4   ;)    global.get 0
(;@fb6   ;)    local.get 3
(;@fb8   ;)    i32.store
(;@fbb   ;)    global.get 0
(;@fbd   ;)    local.get 4
(;@fbf   ;)    i32.store offset=4
(;@fc2   ;)    global.get 0
(;@fc4   ;)    global.get 0
(;@fc6   ;)    i32.const 8
(;@fc8   ;)    i32.add
(;@fc9   ;)    global.set 0
(;@fcb   ;)    local.set 5
(;@fcd   ;)    global.get 0
(;@fcf   ;)    i32.const 4
(;@fd1   ;)    i32.store
(;@fd4   ;)    global.get 0
(;@fd6   ;)    i32.const 75
(;@fd9   ;)    i32.store offset=4
(;@fdc   ;)    global.get 0
(;@fde   ;)    global.get 0
(;@fe0   ;)    i32.const 8
(;@fe2   ;)    i32.add
(;@fe3   ;)    global.set 0
(;@fe5   ;)    local.set 6
(;@fe7   ;)    global.get 0
(;@fe9   ;)    i32.const 4
(;@feb   ;)    i32.store
(;@fee   ;)    global.get 0
(;@ff0   ;)    i32.const 76
(;@ff3   ;)    i32.store offset=4
(;@ff6   ;)    global.get 0
(;@ff8   ;)    global.get 0
(;@ffa   ;)    i32.const 8
(;@ffc   ;)    i32.add
(;@ffd   ;)    global.set 0
(;@fff   ;)    local.set 7
(;@1001  ;)    global.get 0
(;@1003  ;)    local.get 6
(;@1005  ;)    i32.store
(;@1008  ;)    global.get 0
(;@100a  ;)    local.get 7
(;@100c  ;)    i32.store offset=4
(;@100f  ;)    global.get 0
(;@1011  ;)    global.get 0
(;@1013  ;)    i32.const 8
(;@1015  ;)    i32.add
(;@1016  ;)    global.set 0
(;@1018  ;)    local.set 8
(;@101a  ;)    global.get 0
(;@101c  ;)    local.get 1
(;@101e  ;)    i32.store
(;@1021  ;)    global.get 0
(;@1023  ;)    local.get 2
(;@1025  ;)    i32.store offset=4
(;@1028  ;)    global.get 0
(;@102a  ;)    local.get 5
(;@102c  ;)    i32.store offset=8
(;@102f  ;)    global.get 0
(;@1031  ;)    local.get 8
(;@1033  ;)    i32.store offset=12
(;@1036  ;)    global.get 0
(;@1038  ;)    global.get 0
(;@103a  ;)    i32.const 16
(;@103c  ;)    i32.add
(;@103d  ;)    global.set 0
             )
(;@1041  ;)  (func $_row_scoped_ask_return_lam_0 (;71;) (type $fun_2_1) (param i32 i32) (result i32)
(;@1042  ;)    (local i32)
(;@1044  ;)    global.get 0
(;@1046  ;)    local.get 0
(;@1048  ;)    i32.store
(;@104b  ;)    global.get 0
(;@104d  ;)    local.get 1
(;@104f  ;)    i32.store offset=4
(;@1052  ;)    global.get 0
(;@1054  ;)    global.get 0
(;@1056  ;)    i32.const 8
(;@1058  ;)    i32.add
(;@1059  ;)    global.set 0
             )
(;@105d  ;)  (func $_row_scoped_ask_return_lam_1 (;72;) (type $fun_3_1) (param i32 i32 i32) (result i32)
(;@105e  ;)    (local i32 i32 i32)
(;@1060  ;)    local.get 2
(;@1062  ;)    i32.load
(;@1065  ;)    local.set 3
(;@1067  ;)    block (result i32) ;; label = @1
(;@1069  ;)      block (result i32) ;; label = @2
(;@106b  ;)        block (result i32) ;; label = @3
(;@106d  ;)          block (result i32) ;; label = @4
(;@106f  ;)            local.get 3
(;@1071  ;)            local.get 3
(;@1073  ;)            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;)
(;@1078  ;)          end
(;@1079  ;)          local.get 2
(;@107b  ;)          i32.load offset=4
(;@107e  ;)          local.set 4
(;@1080  ;)          local.get 0
(;@1082  ;)          i32.const 4
(;@1084  ;)          i32.add
(;@1085  ;)          local.get 4
(;@1087  ;)          local.get 0
(;@1089  ;)          i32.load
(;@108c  ;)          call_indirect (type $fun_2_1)
(;@108f  ;)          br 2 (;@1;)
(;@1091  ;)        end
(;@1092  ;)        local.get 2
(;@1094  ;)        i32.load offset=4
(;@1097  ;)        local.set 4
(;@1099  ;)        local.get 1
(;@109b  ;)        i32.const 4
(;@109d  ;)        i32.add
(;@109e  ;)        local.get 4
(;@10a0  ;)        local.get 1
(;@10a2  ;)        i32.load
(;@10a5  ;)        call_indirect (type $fun_2_1)
(;@10a8  ;)        br 1 (;@1;)
(;@10aa  ;)      end
(;@10ab  ;)      unreachable
(;@10ac  ;)    end
             )
(;@10af  ;)  (func $_row_scoped_ask_return_lam_2 (;73;) (type $fun_1_1) (param i32) (result i32)
(;@10b0  ;)    (local i32)
(;@10b2  ;)    local.get 0
(;@10b4  ;)    i32.load
             )
(;@10b9  ;)  (func $_row_scoped_ask_return_lam_3 (;74;) (type $fun_1_1) (param i32) (result i32)
(;@10ba  ;)    (local i32 i32)
(;@10bc  ;)    local.get 0
(;@10be  ;)    local.set 1
(;@10c0  ;)    global.get 0
(;@10c2  ;)    i32.const 0
(;@10c4  ;)    i32.store
(;@10c7  ;)    global.get 0
(;@10c9  ;)    local.get 1
(;@10cb  ;)    i32.store offset=4
(;@10ce  ;)    global.get 0
(;@10d0  ;)    global.get 0
(;@10d2  ;)    i32.const 8
(;@10d4  ;)    i32.add
(;@10d5  ;)    global.set 0
             )
(;@10d9  ;)  (func $_row_scoped_ask_return_lam_4 (;75;) (type $fun_1_1) (param i32) (result i32)
(;@10da  ;)    (local i32)
(;@10dc  ;)    local.get 0
(;@10de  ;)    i32.load offset=4
             )
(;@10e3  ;)  (func $_row_scoped_ask_return_lam_5 (;76;) (type $fun_1_1) (param i32) (result i32)
(;@10e4  ;)    (local i32 i32)
(;@10e6  ;)    local.get 0
(;@10e8  ;)    local.set 1
(;@10ea  ;)    global.get 0
(;@10ec  ;)    i32.const 1
(;@10ee  ;)    i32.store
(;@10f1  ;)    global.get 0
(;@10f3  ;)    local.get 1
(;@10f5  ;)    i32.store offset=4
(;@10f8  ;)    global.get 0
(;@10fa  ;)    global.get 0
(;@10fc  ;)    i32.const 8
(;@10fe  ;)    i32.add
(;@10ff  ;)    global.set 0
             )
(;@f4    ;)  (table (;0;) 77 77 funcref)
(;@fb    ;)  (memory (;0;) 1)
(;@100   ;)  (global (;0;) (mut i32) i32.const 0)
(;@108   ;)  (export "main" (func $main))
(;@10f   ;)  (export "mem" (memory 0))
(;@118   ;)  (elem (;0;) (i32.const 0) func 0 1 2 3 $__apply_1_0 $__apply_2_0 $__apply_2_1 $__apply_3_0 $__apply_3_2 $__apply_4_3 $main $main_lam_0 $main_lam_1 $main_lam_2 $main_lam_3 $main_lam_4 $main_lam_5 $main_lam_6 $main_lam_7 $main_lam_8 $main_lam_9 $_row_simple__ask $_row_simple__ask_lam_0 $_row_simple__ask_lam_1 $_row_simple__ask_lam_2 $_row_simple__ask_lam_3 $_row_simple__ask_lam_4 $_row_simple__ask_lam_5 $_row_scoped__ask $_row_scoped__ask_lam_0 $_row_scoped__ask_lam_1 $_row_scoped__ask_lam_2 $_row_scoped__ask_lam_3 $_row_scoped__ask_lam_4 $_row_scoped__ask_lam_5 $_row_simple_return_ask $_row_simple_return_ask_lam_0 $_row_simple_return_ask_lam_1 $_row_simple_return_ask_lam_2 $_row_simple_return_ask_lam_3 $_row_simple_return_ask_lam_4 $_row_simple_return_ask_lam_5 $_row_scoped_return_ask $_row_scoped_return_ask_lam_0 $_row_scoped_return_ask_lam_1 $_row_scoped_return_ask_lam_2 $_row_scoped_return_ask_lam_3 $_row_scoped_return_ask_lam_4 $_row_scoped_return_ask_lam_5 $_row_simple__Reader $_row_simple__Reader_lam_0 $_row_simple__Reader_lam_1 $_row_simple__Reader_lam_2 $_row_simple__Reader_lam_3 $_row_simple__Reader_lam_4 $_row_simple__Reader_lam_5 $_row_scoped__Reader $_row_scoped__Reader_lam_0 $_row_scoped__Reader_lam_1 $_row_scoped__Reader_lam_2 $_row_scoped__Reader_lam_3 $_row_scoped__Reader_lam_4 $_row_scoped__Reader_lam_5 $_row_simple_ask_return $_row_simple_ask_return_lam_0 $_row_simple_ask_return_lam_1 $_row_simple_ask_return_lam_2 $_row_simple_ask_return_lam_3 $_row_simple_ask_return_lam_4 $_row_simple_ask_return_lam_5 $_row_scoped_ask_return $_row_scoped_ask_return_lam_0 $_row_scoped_ask_return_lam_1 $_row_scoped_ask_return_lam_2 $_row_scoped_ask_return_lam_3 $_row_scoped_ask_return_lam_4 $_row_scoped_ask_return_lam_5)
           )
d_return_ask $_row_scoped_return_ask_lam_0 $_row_scoped_return_ask_lam_1 $_row_scoped_return_ask_lam_2 $_row_scoped_return_ask_lam_3 $_row_scoped_return_ask_lam_4 $_row_scoped_return_ask_lam_5 $_row_simple__Reader $_row_simple__Reader_lam_0 $_row_simple__Reader_lam_1 $_row_simple__Reader_lam_2 $_row_simple__Reader_lam_3 $_row_simple__Reader_lam_4 $_row_simple__Reader_lam_5 $_row_scoped__Reader $_row_scoped__Reader_lam_0 $_row_scoped__Reader_lam_1 $_row_scoped__Reader_lam_2 $_row_scoped__Reader_lam_3 $_row_scoped__Reader_lam_4 $_row_scoped__Reader_lam_5 $_row_simple_ask_return $_row_simple_ask_return_lam_0 $_row_simple_ask_return_lam_1 $_row_simple_ask_return_lam_2 $_row_simple_ask_return_lam_3 $_row_simple_ask_return_lam_4 $_row_simple_ask_return_lam_5 $_row_scoped_ask_return $_row_scoped_ask_return_lam_0 $_row_scoped_ask_return_lam_1 $_row_scoped_ask_return_lam_2 $_row_scoped_ask_return_lam_3 $_row_scoped_ask_return_lam_4 $_row_scoped_ask_return_lam_5)
           )
