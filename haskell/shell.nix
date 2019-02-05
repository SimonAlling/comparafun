{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "comparafun-haskell";
  buildInputs = [
    # Project dependencies
    (haskell.packages.ghc844.ghcWithPackages (
    ps: with ps; with pkgs.haskell.lib; (
      [
        safe

        # Test suite
        temporary
        criterion

        # Profiling
        threadscope

        # IDE tooling
        hlint
      ]
    )))
    gnumake
    hasklig
  ];
}
