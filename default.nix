{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [ 
    cargo
    cargo-tarpaulin
    clippy
    gdb
    rust-analyzer
    rustc
    jq
    wabt
    wasmtime
 ];
 shellHook = ''
    export PATH=$PATH:~/.cargo/bin
    export PATH=$PATH:~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin
 '';
}
