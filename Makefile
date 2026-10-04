EMACS ?= emacs
ELPA ?= $(HOME)/.emacs.d/elpa
BATCH = $(EMACS) -Q --batch --eval '(setq package-user-dir "$(ELPA)")' -f package-initialize -L .

.PHONY: compile test clean

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile org-literature.el

test:
	$(BATCH) -l test/org-literature-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
