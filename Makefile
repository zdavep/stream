.PHONY: build format lint test run clean

build: format
	@stack build

format:
	@ormolu -m inplace src/*.hs
	@ormolu -m inplace app/*.hs
	@ormolu -m inplace test/*.hs

lint:
	@hlint src/*.hs app/*.hs

test:
	@stack test

run:
	@stack run

clean:
	@rm -rf .stack-work minq.cabal
