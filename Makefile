help:
	@cat Makefil

hlint-all:
	hlint					\
		tdf/src				\
		tdf/test			\
		ihaskell-tdf/src	\
		ihaskell-tdf/test	\
		tdf-tools/src		\
		tdf-tools/test

hla: hlint-all
