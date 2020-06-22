.PHONY: build format lint test run clean repl

build: format
	@stack build

format:
	@stylish-haskell -i src/*.hs
	@stylish-haskell -i app/*.hs
	@stylish-haskell -i test/*.hs

lint:
	@hlint src/*.hs app/*.hs

test:
	@stack test

run:
	@stack run

clean:
	@rm -rf .stack-work

repl:
	@stack repl
