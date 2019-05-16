{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./kmeans-par.nix { }