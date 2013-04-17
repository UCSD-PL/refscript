PROFOPTS="-O2 -rtsopts -prof -auto-all -caf-all -XStandaloneDeriving -XDeriveDataTypeable"

CABAL=cabal
CABALP=$(CABAL) install -p

all:
	$(CABAL) install

build:
	$(CABAL) build

clean:
	cabal clean

docs:
	$(CABAL) haddock --executables --internal --hoogle --hyperlink-source #--html-location=http://goto.ucsd.edu/~rjhala/llvm-haskell/

lint:
	hlint --colour --report .
