EMACS ?= emacs

.PHONY: all fmt check compile clean

all: fmt check compile

fmt:
	$(EMACS) -Q --batch -l scripts/build.el -f build-fmt

check:
	$(EMACS) --batch -l scripts/build.el -f build-check

compile:
	$(EMACS) --batch -l scripts/build.el -f build-compile

clean:
	find lisp -name '*.elc' -delete
