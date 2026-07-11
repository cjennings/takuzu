EMACS ?= emacs
SRC   := $(wildcard takuzu*.el)
TESTS := $(wildcard tests/test-*.el)

.PHONY: test compile lint clean

# Run the full ERT suite headless.
test:
	$(EMACS) -Q --batch -L . -L tests \
	  $(foreach t,$(TESTS),-l $(t)) \
	  -f ert-run-tests-batch-and-exit

# Run one test file: make test-file FILE=tests/test-takuzu-board.el
test-file:
	$(EMACS) -Q --batch -L . -L tests -l $(FILE) \
	  -f ert-run-tests-batch-and-exit

# Byte-compile all sources, warnings are errors.
compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC)

# checkdoc pass.
lint:
	$(EMACS) -Q --batch -L . \
	  --eval "(dolist (f (file-expand-wildcards \"takuzu*.el\")) (checkdoc-file f))"

clean:
	rm -f *.elc tests/*.elc
