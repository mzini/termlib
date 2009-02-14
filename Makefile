PREFIX=/usr/local

all: build doc_haskell

build: build_haskell

install: install_haskell

uninstall: unregister_haskell

clean: clean_haskell

.PHONY: clean

include ./template.mk