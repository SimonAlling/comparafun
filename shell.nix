{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "comparafun";
  buildInputs = [
    # Project dependencies

    # Haskell
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

    # Scala
    openjdk
    sbt
    scala
  ];
}
