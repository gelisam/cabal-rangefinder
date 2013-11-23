NAME="$(shell basename `pwd`)"

.PHONY: all config build test small-tests big-tests clean clobber

all: build test

config: dist/setup-config

dist/setup-config:
	cabal-dev install-deps
	cabal-dev configure

build: config
	cabal-dev build | cat
	@cabal-dev build &> /dev/null


test: small-tests big-tests

small-tests: build
	find src -name '*.hs' | xargs doctest -package-db "$$(ls -d cabal-dev/packages-*.conf)"
	@echo


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
	rm -rf cabal-dev
