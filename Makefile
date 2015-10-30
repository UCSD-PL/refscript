THREADS=1
CABAL=cabal
CABALP=$(CABAL) install -p
FASTOPTS="-O0"
DISTOPTS="-W -O2 -XStandaloneDeriving"
PROFOPTS="-rtsopts -auto-all -caf-all"

##############################################################################
##############################################################################
##############################################################################

fast:
	$(CABAL) install --ghc-options=$(FASTOPTS)

dist:
	$(CABAL) install --ghc-options=$(DISTOPTS)

prof:
	$(CABAL) install --enable-executable-profiling --enable-library-profiling --ghc-options=$(PROFOPTS)

prof710:
	$(CABAL) install --enable-profiling --ghc-options=$(PROFOPTS)

clean:
	cabal clean
	rm -f ./include/prelude.d.ts ./include/build-prelude.js

test:
	tsc -p scripts	
	node scripts/runtests.js

cleantest:
	tsc -p scripts
	node scripts/cleanup.js
	
docs:
	$(CABAL) haddock --executables --internal --hoogle --hyperlink-source

lint:
	hlint --colour --report .

tags:
	hasktags -b src/
