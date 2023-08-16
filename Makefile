EMACS ?= emacs
ELS = igist.el
ELCS = igist.elc
TEST_ELS = tests/test-bootstrap.el tests/igist-test.el

# If the first argument is "test"...
ifeq (test, $(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SELECTOR):;@:)
endif

all: compile test clean

%.elc:%.el
	$(EMACS) -batch -L . -l tests/test-bootstrap.el -f batch-byte-compile $(ELS)

compile:$(ELCS)

.PHONY: test

test:$(ELCS)
ifeq ($(SELECTOR),)
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) -f ert-run-tests-batch-and-exit
else
	$(EMACS) -Q --batch -L . $(addprefix -l , $(TEST_ELS)) --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"
endif


help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make clean

clean:
	@rm -f *.elc
