{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
nixpkgs.haskell.packages.${compiler}.developPackage {
  root = ./.;
  withHoogle = true;
  modifier = drv:
    nixpkgs.haskell.lib.addBuildTools drv (with nixpkgs.haskellPackages;
      [ cabal-install hoogle ghc nixpkgs.haskell-language-server ]);
}
