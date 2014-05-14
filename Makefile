# you can either build via "make build" or "cabal build".
# however, to run the tests, you may only use "make test", not "cabal test".

NAME="$(shell basename `pwd`)"

.PHONY: all config build test small-tests big-tests clean clobber

all: build test

config: dist/setup-config

dist/setup-config:
	cabal sandbox init
	cabal install --only-dependencies

build: config
	cabal build


test: small-tests big-tests

small-tests: build
	cabal install QuickCheck
	find src -name '*.hs' | xargs doctest -package-db "$$(ls -d .cabal-sandbox/*-packages.conf.d)"


big-tests: $(patsubst %,proofs/%.proof,$(shell ls tests))
	-@echo '*** ALL TESTS OK ***'

proofs/%.proof: proofs/%/cabal.out tests/%/cabal.expected
	diff $^
	touch $@

proofs/%/cabal.out: tests/%/cabal.in build
	mkdir -p $(dir $@)
	cd $(dir $<); cat cabal.in > $(shell basename $(shell dirname $<)).cabal
	cd $(dir $<); ../../dist/build/$(NAME)/$(NAME) $(shell basename $(shell dirname $<)).cabal
	cat $(dir $<)/$(shell basename $(shell dirname $<)).cabal > $@


clean:
	rm -rf proofs

clobber: clean
	rm -rf dist

distclean: clobber
	rm -rf cabal.sandbox.config .cabal-sandbox
