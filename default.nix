{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ 
    cargo
    cargo-expand
    clippy
    gdb
    glibc
    jq
    rustfmt
    rust-analyzer
    rustc
    wabt
    wasmtime
    rustc.llvmPackages.llvm
  ];
} 
