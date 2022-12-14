.PHONY: build tests doc

build:
	@cabal build

tests:
	@./runtests

doc: advent.pdf

DAYS := $(wildcard day*.md)

advent.pdf: ${DAYS}
	@sed -se '$$a\\\newpage' ${DAYS} | pandoc --pdf-engine xelatex -s --toc -o advent.pdf
