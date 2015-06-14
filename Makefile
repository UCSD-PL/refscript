THREADS=1
CABAL=cabal
CABALP=$(CABAL) install -p
FASTOPTS="-O0"
DISTOPTS="-W -O2 -XStandaloneDeriving"
PROFOPTS="-rtsopts -auto-all -caf-all"

TASTY=./dist/build/test/test

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

test:
	cd tests && ./regrtest.py -t $(THREADS) && cd ../

docs:
	$(CABAL) haddock --executables --internal --hoogle --hyperlink-source

lint:
	hlint --colour --report .

tags:
	hasktags -b src/

tasty:
	cabal configure -fdevel --enable-tests --disable-library-profiling
	cabal build
	$(TASTY)

tasty-dist:
	cabal configure -fdevel --enable-tests --disable-library-profiling -O2
	cabal build
	$(TASTY)

