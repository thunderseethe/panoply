Short term:
- [ ] Write test for calling top level item with effect
- [ ] Test multiple effects
- [ ] Figure out better solution for lowering effects than AbsE and FunETy
- [ ] Better testing solutions for lower-medir and emit-wasm.
    - At these stages code is quite big so expect_test starts to have performance problems.
    - It's also not very easy for a human to determine if diffs of that size are reasonable
    - As an easy first step these passes could use file diffing instead of inline diffing

Long term:
- [ ] Better diagnostics (especially for parse errors)
- [ ] Mixin' Modules 
- [ ] Playground
