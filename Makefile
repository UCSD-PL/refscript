
SERVERHOME=/home/rjhala/public_html/liquid/nanojs

THREADS=1
PROFOPTS="-O2 -rtsopts -prof -auto-all -caf-all -XStandaloneDeriving -XDeriveDataTypeable"
CABAL=cabal
CABALP=$(CABAL) install -p

all:
	$(CABAL) install

build:
	$(CABAL) build

clean:
	cabal clean

test:
	cd tests && ./regrtest.py -t $(THREADS) && cd ../

docs:
	$(CABAL) haddock --executables --internal --hoogle --hyperlink-source 

lint:
	hlint --colour --report .


web: web/demo.php
	cp dist_liquid/build/nanojs/nanojs $(SERVERHOME)/nanojs
	cp -rf web/* $(SERVERHOME)/

siteperms:
	sudo chgrp -R www-data $(SERVERHOME)
	sudo chmod -R g+rx $(SERVERHOME)
	sudo chmod    g+rwx $(SERVERHOME)/
	sudo chmod -R g+rwx $(SERVERHOME)/saved/


