{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ 
    cargo
    cargo-expand
    clippy
    gdb
    glibc
    rustfmt
    rust-analyzer
    rustc
  ];
} 
