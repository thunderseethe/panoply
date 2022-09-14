{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
let 
     pkgs = import (builtins.fetchGit {
         # Descriptive name to make the store path easier to identify                
         name = "my-old-revision";                                                 
         url = "https://github.com/NixOS/nixpkgs/";                       
         ref = "refs/heads/nixpkgs-unstable";                     
         rev = "d1c3fea7ecbed758168787fe4e4a3157e52bc808";                                           
     }) {};
in nixpkgs.haskell.packages.${compiler}.developPackage {
  root = ./.;
  withHoogle = true;
  modifier = drv:
    nixpkgs.haskell.lib.addBuildTools drv (with nixpkgs.haskellPackages;
      [ cabal-install hoogle ghc haskell-language-server ]); #pkgs.haskell-language-server 
}
