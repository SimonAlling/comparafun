{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "comparafun";
  buildInputs = [
    # Project dependencies

    # Haskell
    (haskell.packages.ghc864.ghcWithPackages (
    ps: with ps; with haskell.lib; (
      [
        safe
        monad-parallel
        process
        regexpr

        # Test suite
        temporary
        criterion
      ]
    )))
    hasklig
    stack

    # Scala
    openjdk
    sbt
    scala

    # Manticore
    manticore

    # Erlang
    erlangR21
  ];
}
