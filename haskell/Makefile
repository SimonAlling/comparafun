all : benchmark_par clean

benchmark_seq: Main.hs
	ghc -O2 -rtsopts -eventlog --make Main.hs

benchmark_par: Main.hs
	ghc -O2 -threaded -rtsopts -eventlog --make Main.hs

clean:
	-rm -f */*.hi */*.o *.hi *.o       # Haskell stuff

distclean: clean


.PHONY: all clean distclean
