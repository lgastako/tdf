help:
	@cat Makefile

.PHONY: test

haddock:
	stack haddock

hlint:
	hlint ./src

install:
	#stack install

test:
	stack test --fast --file-watch

watch:
	stack build --fast --file-watch

hl: hlint
t: test
w: watch
