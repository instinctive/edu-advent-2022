.PHONY: build tests pdf

build:
	@cabal build

tests:
	@./runtests

DAYS := $(wildcard day*.md)

pdf: ${DAYS}
	@sed -se '$$a\\\newpage' ${DAYS} | pandoc -s --toc -o advent.pdf
