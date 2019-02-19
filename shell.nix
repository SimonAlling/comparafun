{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  # Manticore requires version 110.81; NixOS only had 110.79.
  newer_smlnj = (
    let
      version = "110.81";
      baseurl = "http://smlnj.cs.uchicago.edu/dist/working/${version}";

      # I wrote a Haskell program to obtain the hashes.
      sources = map fetchurl [
        { url = "${baseurl}/config.tgz";              sha256 = "0bqgcaxbnc0bbvp41kp6yl6cirywlziqwd49fs6w8pj5czcx54x9"; }
        { url = "${baseurl}/cm.tgz";                  sha256 = "1wyvnh7k99pafdxg3q8pnwyc49hdm00y7cfaibmmrrb7d684hm1b"; }
        { url = "${baseurl}/compiler.tgz";            sha256 = "13qsz2aj8yrpbqfx2nz1ly8zfgyhw4l9s77mhxy0vl7dra5zzfs4"; }
        { url = "${baseurl}/runtime.tgz";             sha256 = "0k73y0h1qkv33w49bxx3n15w5d1xsl1njphk0bff9ansprz87kb5"; }
        { url = "${baseurl}/system.tgz";              sha256 = "0z6gi1xccdx9kjmxdvnrm0bpqxw9xc3d0sr0m1ggin25xk2zg1q0"; }
        { url = "${baseurl}/MLRISC.tgz";              sha256 = "1nsgkjcfihw5pzbhn960lhr4znd3m42kkrzdiif0sk85dawnhn43"; }
        { url = "${baseurl}/smlnj-lib.tgz";           sha256 = "1vm7g28z969bmys1if23iv3dpd9k76n5swjliq86psi0zbi9zp2v"; }
        { url = "${baseurl}/old-basis.tgz";           sha256 = "0mf1aihb0y7m6czc6wbj07vbrfykd3d4m0j7bvvlm76r9ngnss63"; }
        { url = "${baseurl}/ckit.tgz";                sha256 = "0385dd8ayw5nf0vwmaz05yrvqsxvvbc5mwxl8al8lybflfddfk79"; }
        { url = "${baseurl}/nlffi.tgz";               sha256 = "091x4j0camarv9bridrrr644ggc8zfwba12c99cyx5dhcwmzxd0i"; }
        { url = "${baseurl}/cml.tgz";                 sha256 = "05zrn6rkghxdpdx3zcci33z45f4mdi5gx8465knqp4cp0m5hj2b8"; }
        { url = "${baseurl}/eXene.tgz";               sha256 = "118d2q6f1ydadg84dq2j7kgfaywcvj23nv8gfair1ggr53rzvl26"; }
        { url = "${baseurl}/ml-lpt.tgz";              sha256 = "15p8pk3bv6y7n9yi8kwb9l0zvzn1giyvk6lyxnnm23pah3zhw7h6"; }
        { url = "${baseurl}/ml-lex.tgz";              sha256 = "1d54fnjc680rhbjcg53cjyzzwsshff81sz6d4samcqr53bid1k9y"; }
        { url = "${baseurl}/ml-yacc.tgz";             sha256 = "19p258qf9z0sh7m7hm6axlg7cj5zkhc5d8zf1a473wl7qng79p7w"; }
        { url = "${baseurl}/ml-burg.tgz";             sha256 = "0ix7p914sqdif2mrigimfc9gqcl9ra3gq0842vly7rwbawrrz54i"; }
        { url = "${baseurl}/pgraph.tgz";              sha256 = "0bcspi4zwypvr121840q6hczls0w0v57n4gs4kxvibvlmv6nf3fh"; }
        { url = "${baseurl}/trace-debug-profile.tgz"; sha256 = "0nbinrlpm1fgfrnxcnv65fnn671796bjjhdisw3h3a7i718alz3b"; }
        { url = "${baseurl}/heap2asm.tgz";            sha256 = "1zjs1wx2kkcfrj36scf1nndkkmg8cdbhzj1v40d7j6z2519b017c"; }
        { url = "${baseurl}/smlnj-c.tgz";             sha256 = "15f3wczhbmflin3v5gspk528dwz2jiqf3qn532sq6g9s9bhagp9i"; }
        { url = "${baseurl}/doc.tgz";                 sha256 = "1zvjhsdm1l1cdjyfriblwy9328fyksrjy1kw4rxxja54768gcpv4"; }
        { url = "${baseurl}/boot.x86-unix.tgz";       sha256 = "0bjkxxjn3n05qm8p09rk8x2mnd07wr8rnlc26m9ppsg001kgjs2i"; }
      ];
    in
      pkgsi686Linux.stdenv.mkDerivation { # pkgsi686Linux makes it work on 64-bit systems.
        name = "smlnj-${version}";

        inherit sources;

        patchPhase = ''
          sed -i '/PATH=/d' config/_arch-n-opsys base/runtime/config/gen-posix-names.sh
          echo SRCARCHIVEURL="file:/$TMP" > config/srcarchiveurl
        '' + stdenv.lib.optionalString stdenv.isDarwin (with darwin; ''
          sed -i '/^[[:space:]]*\*x86-darwin\*)$/,/^[[:space:]]*\*) ;;/ c\
      \  \*x86-darwin\*)\
      \    INCLFILE=${stdenv.lib.getDev apple_sdk.sdk}/include/unistd.h\
      \    ;;\
      \  \*) ;;
      ' base/runtime/config/gen-posix-names.sh
          sed -i 's|^AS =\([[:space:]]*\)/usr/bin/as|AS =\1as|' base/runtime/objs/mk.x86-darwin
        '');

        unpackPhase = ''
          for s in $sources; do
            b=$(basename $s)
            cp $s ''${b#*-}
          done
          unpackFile config.tgz
          mkdir base
          ./config/unpack $TMP runtime
        '';

        buildPhase = ''
          ./config/install.sh
        '';

        installPhase = ''
          mkdir -pv $out
          cp -rv bin lib $out
          cd $out/bin
          for i in *; do
            sed -i "2iSMLNJ_HOME=$out/" $i
          done
        '';

        meta = with stdenv.lib; {
          description = "Standard ML of New Jersey, a compiler";
          homepage    = http://smlnj.org;
          license     = licenses.bsd3;
          platforms   = [ "i686-linux" ] ++ platforms.darwin;
          maintainers = with maintainers; [ thoughtpolice ];
        };
      }
  );
in

stdenv.mkDerivation {
  name = "comparafun";
  buildInputs = [
    # Project dependencies

    # Haskell
    (haskell.packages.ghc844.ghcWithPackages (
    ps: with ps; with pkgs.haskell.lib; (
      [
        safe
        monad-parallel

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

    # Manticore
    newer_smlnj # Standard ML of New Jersey
    autoconf
    gnumake
    # Manticore must be installed manually.
    # http://manticore.cs.uchicago.edu/install.html
  ];
}
