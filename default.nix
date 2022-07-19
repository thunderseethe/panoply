{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  #nativeBuildInputs = with pkgs; [ rustc cargo gcc ];
  buildInputs = with pkgs; [ 
    cargo
    cargo-expand
    glibc
    rust-analyzer
    rustc
  ];
} 
