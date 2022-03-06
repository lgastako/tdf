help:
	@cat Makefil

hlint-all:
	hlint					\
		ihaskell-tdf/src	\
		ihaskell-tdf/test	\
		relativ/src         \
		relativ/test        \
		tdf-tools/src		\
		tdf-tools/test      \
		tdf/src				\
		tdf/test

hla: hlint-all
