PREFIX=/usr/local
SETUP=./Setup.lhs

all: build


configure: *.cabal Setup.*
	$(SETUP) configure --prefix=$(PREFIX)

build: configure 
	$(SETUP) build


install: build
	$(SETUP) install

uninstall: 
	$(SETUP) unregister

upload:
	./upload.sh

clean: 
	$(SETUP) clean

.PHONY: clean