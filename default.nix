{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ 
    cargo
    cargo-expand
    gdb
    glibc
    rustfmt
    rust-analyzer
    rustc
    clippy
  ];
} 
