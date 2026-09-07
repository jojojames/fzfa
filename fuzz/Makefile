.PHONY: compile selftest trace-selftest reduce-selftest replay replay-trace \
	replay-trace-live reduce-trace reduce-trace-live state producer-selftest \
	producer tools live clean

EMACS ?= emacs
FZF_NATIVE_DIR ?= ../../fzf-native
CASES ?= 300
STEPS ?= 40
LIVE_CASES ?= 20
LIVE_EMACS_FLAGS ?=

ELISP_LOAD_PATH := -L .. -L . -L $(FZF_NATIVE_DIR)
FUZZ_SRC := $(wildcard fzfa-fuzz-*.el)
STATE_RUNNER := $(EMACS) -Q --batch $(ELISP_LOAD_PATH) \
	-l fzfa-fuzz-core.el -l fzfa-fuzz-state.el
NATIVE_RUNNER := $(EMACS) -Q --batch $(ELISP_LOAD_PATH) \
	-l fzf-native -f fzf-native-load-dyn \
	-l fzfa-fuzz-core.el -l fzfa-fuzz-producer.el

compile: clean
	$(EMACS) -Q --batch $(ELISP_LOAD_PATH) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(FUZZ_SRC)

selftest:
	$(STATE_RUNNER) -f fzfa-fuzz-state-selftest-batch

trace-selftest:
	$(NATIVE_RUNNER) -l fzfa-fuzz-replay.el \
	  -f fzfa-fuzz-replay-selftest-batch

reduce-selftest:
	$(NATIVE_RUNNER) -l fzfa-fuzz-reduce.el \
	  -f fzfa-fuzz-reduce-selftest-batch

replay:
	$(STATE_RUNNER) -f fzfa-fuzz-replay-batch

replay-trace:
	@if [ -z "$(TRACE)" ]; then \
	  echo "usage: make replay-trace TRACE=/path/to/failure.sexp"; \
	  exit 2; \
	fi
	FZFA_FUZZ_TRACE_FILE="$(abspath $(TRACE))" \
	  $(NATIVE_RUNNER) -l fzfa-fuzz-replay.el \
	  -f fzfa-fuzz-replay-trace-batch

replay-trace-live:
	@if [ -z "$(TRACE)" ]; then \
	  echo "usage: make replay-trace-live TRACE=/path/to/failure.sexp"; \
	  exit 2; \
	fi
	FZFA_FUZZ_TRACE_FILE="$(abspath $(TRACE))" \
	  $(EMACS) -Q $(LIVE_EMACS_FLAGS) $(ELISP_LOAD_PATH) \
	  -l fzf-native -f fzf-native-load-dyn \
	  -l fzfa-fuzz-replay.el -f fzfa-fuzz-replay-trace-live

reduce-trace:
	@if [ -z "$(TRACE)" ]; then \
	  echo "usage: make reduce-trace TRACE=/path/to/failure.sexp [OUTPUT=/path/to/min.sexp]"; \
	  exit 2; \
	fi
	FZFA_FUZZ_TRACE_FILE="$(abspath $(TRACE))" \
	FZFA_FUZZ_REDUCED_FILE="$(if $(OUTPUT),$(abspath $(OUTPUT)),)" \
	  $(NATIVE_RUNNER) -l fzfa-fuzz-reduce.el \
	  -f fzfa-fuzz-reduce-trace-batch

reduce-trace-live:
	@if [ -z "$(TRACE)" ]; then \
	  echo "usage: make reduce-trace-live TRACE=/path/to/failure.sexp [OUTPUT=/path/to/min.sexp]"; \
	  exit 2; \
	fi
	FZFA_FUZZ_TRACE_FILE="$(abspath $(TRACE))" \
	FZFA_FUZZ_REDUCED_FILE="$(if $(OUTPUT),$(abspath $(OUTPUT)),)" \
	  $(EMACS) -Q $(LIVE_EMACS_FLAGS) $(ELISP_LOAD_PATH) \
	  -l fzf-native -f fzf-native-load-dyn \
	  -l fzfa-fuzz-reduce.el -f fzfa-fuzz-reduce-trace-live

state:
	FZFA_FUZZ_CASES=$(CASES) FZFA_FUZZ_STEPS=$(STEPS) \
	  $(STATE_RUNNER) -f fzfa-fuzz-state-batch

producer-selftest:
	$(NATIVE_RUNNER) -f fzfa-fuzz-producer-selftest-batch

producer:
	FZFA_FUZZ_CASES=$(CASES) $(NATIVE_RUNNER) \
	  -f fzfa-fuzz-producer-batch

tools:
	FZFA_FUZZ_CASES=$(CASES) $(NATIVE_RUNNER) \
	  -f fzfa-fuzz-tools-batch

# This target deliberately does not use --batch.  Batch Emacs cannot enter
# a minibuffer, so it cannot exercise mini-window redisplay.  CI gives terminal
# Emacs a pseudo-terminal; it can also run in a local terminal with `emacs -nw'.
live:
	FZFA_FUZZ_CASES=$(LIVE_CASES) \
	  $(EMACS) -Q $(LIVE_EMACS_FLAGS) $(ELISP_LOAD_PATH) \
	  -l fzf-native -f fzf-native-load-dyn \
	  -l fzfa-fuzz-core.el -l fzfa-fuzz-live.el \
	  -f fzfa-fuzz-live-run

clean:
	rm -f *.elc
