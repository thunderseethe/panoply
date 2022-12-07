{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  #nativeBuildInputs = with pkgs; [ rustc cargo gcc ];
  buildInputs = with pkgs; [ 
    cargo
    cargo-expand
    glibc
    rustfmt
    rust-analyzer
    rustc
    clippy
  ];
  RUST_BACKTRACE = 1;
} 
