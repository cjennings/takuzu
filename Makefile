EMACS ?= emacs
SRC   := $(wildcard takuzu*.el)
TESTS := $(wildcard tests/test-*.el)

COVERAGE_DIR     ?= .coverage
COVERAGE_FILE    ?= $(COVERAGE_DIR)/simplecov.json
COVERAGE_SUMMARY ?= .claude/scripts/coverage-summary.el

.PHONY: test compile lint clean coverage coverage-summary

# Run the full ERT suite headless.
test:
	$(EMACS) -Q --batch -L . -L tests \
	  --eval '(setq load-prefer-newer t)' \
	  $(foreach t,$(TESTS),-l $(t)) \
	  -f ert-run-tests-batch-and-exit

# Run one test file: make test-file FILE=tests/test-takuzu-board.el
test-file:
	$(EMACS) -Q --batch -L . -L tests \
	  --eval '(setq load-prefer-newer t)' \
	  -l $(FILE) \
	  -f ert-run-tests-batch-and-exit

# Byte-compile all sources, warnings are errors.
compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC)

# checkdoc pass, plus package-lint when it is installed.
lint:
	$(EMACS) -Q --batch -L . \
	  --eval "(dolist (f (file-expand-wildcards \"takuzu*.el\")) (checkdoc-file f))"
	$(EMACS) -Q --batch -L . \
	  --eval '(package-initialize)' \
	  --eval '(setq package-lint-main-file "takuzu.el")' \
	  --eval "(if (require 'package-lint nil t) \
	              (package-lint-batch-and-exit) \
	            (message \"package-lint not installed -- skipping\"))" \
	  $(SRC)

# Run the suite under undercover, writing a SimpleCov report.
# Sources must load from .el (not .elc) for instrumentation to attach, and
# undercover must be armed before the test files require the source.
coverage:
	@rm -f $(COVERAGE_FILE) *.elc tests/*.elc
	@mkdir -p $(COVERAGE_DIR)
	@UNDERCOVER_FORCE=true $(EMACS) -Q --batch -L . -L tests \
	  --eval '(package-initialize)' \
	  --eval "(when (require 'undercover nil t) (undercover \"takuzu*.el\" (:report-format 'simplecov) (:report-file \"$(COVERAGE_FILE)\") (:merge-report nil)))" \
	  $(foreach t,$(TESTS),-l $(t)) \
	  -f ert-run-tests-batch-and-exit
	@$(MAKE) coverage-summary

# Print the per-file table and the unit-weighted project number.
# The summary script is local dev tooling; without it the raw SimpleCov
# report still lands at $(COVERAGE_FILE).
coverage-summary:
	@if [ ! -f $(COVERAGE_FILE) ]; then \
	  echo "[!] No coverage file at $(COVERAGE_FILE). Run 'make coverage' first."; exit 1; \
	fi
	@if [ ! -f $(COVERAGE_SUMMARY) ]; then \
	  echo "[i] No summary script at $(COVERAGE_SUMMARY); raw report is $(COVERAGE_FILE)."; exit 0; \
	fi
	@$(EMACS) --batch -q -l $(COVERAGE_SUMMARY) \
	  --eval '(cj/coverage-print-module-summary "$(COVERAGE_FILE)" "." "$(CURDIR)")'

clean:
	rm -f *.elc tests/*.elc
	rm -rf $(COVERAGE_DIR)
