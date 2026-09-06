.PHONY: compile selftest replay state clean

EMACS ?= emacs
FZF_NATIVE_DIR ?= ../../fzf-native
CASES ?= 300
STEPS ?= 40

ELISP_LOAD_PATH := -L .. -L . -L $(FZF_NATIVE_DIR)
FUZZ_SRC := $(wildcard fzfa-fuzz-*.el)
STATE_RUNNER := $(EMACS) -Q --batch $(ELISP_LOAD_PATH) \
	-l fzfa-fuzz-core.el -l fzfa-fuzz-state.el

compile: clean
	$(EMACS) -Q --batch $(ELISP_LOAD_PATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(FUZZ_SRC)

selftest:
	$(STATE_RUNNER) -f fzfa-fuzz-state-selftest-batch

replay:
	$(STATE_RUNNER) -f fzfa-fuzz-replay-batch

state:
	FZFA_FUZZ_CASES=$(CASES) FZFA_FUZZ_STEPS=$(STEPS) \
	  $(STATE_RUNNER) -f fzfa-fuzz-state-batch

clean:
	rm -f *.elc
