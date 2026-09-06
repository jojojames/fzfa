# fzfa fuzz trace format

fzfa fuzz traces are versioned Emacs Lisp data.  Generation finishes before a
driver receives a trace.  Saving a trace therefore saves the choices needed to
run the case again; replay does not ask the random generator to make those
choices a second time.

## Trace format 1

A trace is one property list with exactly these top-level keys:

```elisp
(:format 1
 :target state-producer
 :root-seed 123
 :case-seed 127
 :environment (...)
 :initial-state (...)
 :actions
 ((fetch "a")
  (fetch "ab")
  (deliver 0 ("old"))
  (stop)))
```

- `:format` selects the reader and driver contract.  Readers reject unknown
  versions instead of guessing.
- `:target` selects one driver.  Format 1 supports completion-list ownership,
  producer lifecycle, stale poll publication, message ownership, native
  producer bytes, the ugrep fixture, and live icomplete.
- `:root-seed` identifies the campaign.
- `:case-seed` identifies this case inside the campaign.
- `:environment` records Emacs, OS, locale, fzfa, and fzf-native versions and
  Git revisions.  Replay reports the recorded environment but runs against the
  current checkout so a maintainer can check whether a failure still exists.
- `:initial-state` contains the values that exist before the first action.
- `:actions` is the ordered input to the driver.

Top-level keys cannot be omitted, repeated, or added in format 1.  The reader
also rejects improper lists, reader evaluation, and a second form after the
trace.  Each driver validates its target's initial state and actions before it
uses them.

## Target formats

### Completion-list ownership

```elisp
(:initial-state
 (:sources (("zeta" "alpha" "same" "same")
            ("other" "value")))
 :actions
 ((lookup "")
  (frontend-mutate reverse nil)
  (lookup "")))
```

`:sources` stores each source's candidate list, including text properties.
The driver asks for the empty-query result, mutates the returned list as a
frontend might, and asks again.  The operation is one of `nconc`, `truncate`,
`dot`, `reverse`, `sort`, or `dedup`.  Only `dot` uses the third value; the
other operations store `nil` there.

### Elisp producer lifecycle

```elisp
(:initial-state (:source-name "state" :step-budget 5)
 :actions
 ((fetch "a")
  (deliver 0 ("first" "second"))
  (run 0)
  (stop)))
```

The selectors are stored, not regenerated.  Candidate strings, duplicates,
and text properties are part of the trace.  Supported actions are:

- `(fetch QUERY)`: ask the producer for a query.
- `(restart QUERY)`: replace its current request.
- `(deliver SELECTOR CANDIDATES)`: run a saved callback.  The selector chooses
  from the callbacks that exist at this point.
- `(run SELECTOR)`: run one queued refresh.
- `(stop)`: tear down the source.  No action may follow it.

### Stale poll publication

```elisp
(:initial-state
 (:generations ((old . 1) (new . 0)) :initial-handle old)
 :actions
 ((tick)
  (replace-handle new)
  (run-publication)))
```

The poll starts with `old`, saves a publication callback, switches the source
to `new`, and only then runs the old callback.  This is the short schedule that
checks that an old native handle cannot publish into a replacement session.

### Message ownership

```elisp
(:initial-state (:context process)
 :actions ((print "problem %d" 7)))
```

The context is `owner`, `process`, or `none`.  It says which buffer is current
when the recorded `fzfa--print` call runs.  The trace stores the format string
and its arguments instead of rebuilding them during replay.

### Native producer bytes

```elisp
(:initial-state
 (:kind split-utf8
  :failure nil
  :max-line-source ambient
  :max-line-length 256
  :expected-present t
  :expected-candidates ((:text "café"))
  :interim-present nil
  :interim-candidates nil)
 :actions
 ((emit :bytes-hex "636166c3" :pause 0.01)
  (emit :bytes-hex "a90a" :pause 0.01)
  (exit 0)
  (poll-until-terminal)
  (redraw 3)
  (stop)))
```

Raw bytes use lowercase hexadecimal text.  This preserves a split UTF-8 code
point, malformed byte, NUL, CRLF pair, or partial escape sequence across
locales.  Logical multibyte strings use `(:text STRING)`; unibyte candidate
strings use `(:bytes-hex HEX)`.

An ambient line cap records both its source and its effective value.  Exact
replay restores that ambient value while leaving the per-case option absent,
so it still exercises the ambient-policy bridge.

### Ugrep fixture

```elisp
(:initial-state
 (:sentinel "fzfa-fuzz-visible-normal-7319"
  :header-line "plain ASCII header"
  :header-count 800
  :late-tail-hex "6c6174652d62696e617279007461696c0a")
 :actions
 ((write-normal "normal.txt")
  (write-late "manual.info")
  (write-late "emms/cache")
  (write-late "late.bin")
  (run-ugrep)))
```

`write-normal` makes the positive-control file.  `write-late` makes a large
file with the saved byte tail.  The final action runs the command captured
from the current `fzfa-ugrep`.  File names must stay inside the temporary
fixture directory; absolute paths and `..` escapes are rejected.

### Live icomplete input

```elisp
(:initial-state
 (:frontend icomplete-vertical
  :target "alpha"
  :candidates ("alpha" "xxxxx-00" "xxxxx-01")
  :icomplete-prospects-height 10
  :max-mini-window-height 10
  :watchdog-seconds 5)
 :actions
 ((key 97 "a")
  (key 108 "al")
  ...
  (key 127 "")
  (key 13 "")))
```

Each key stores the query that must render before the next key is released.
Replay therefore preserves both input and the causal handshake.

## Failure artifact format 1

A failing oracle wraps its executable trace:

```elisp
(:artifact-format 1
 :failure
 (:signature "state-producer/0123456789abcdef"
  :oracle "snapshot is different from expected after %S"
  :seed 127
  :message "snapshot is different from expected after (deliver ...)"
  :expected (...)
  :observed (...))
 :trace
 (:format 1 ...))
```

For an oracle failure, the signature is derived from the target and the
oracle's stable format string.  Generated values do not change it.  For an
unexpected condition, its type and message are also part of the signature.
This lets replay and a later reducer distinguish “the same failure still
happens” from “a different error happened first.”

Every fzfa fuzz assertion records `:expected` and `:observed`.  An unexpected
Elisp error is recorded as expecting normal return and observing the signaled
condition.

By default, a failure writes a temporary `.sexp` file and prints its path.  Set
`FZFA_FUZZ_ARTIFACT_FILE` to choose the path.  The fuzz workflow sets a unique
path for each lane and uploads it when that lane fails.

## Exact replay

For state, native producer, or ugrep traces:

```sh
make -C fuzz replay-trace TRACE=/path/to/failure.sexp
```

For a live icomplete trace, run one real Emacs UI:

```sh
make -C fuzz replay-trace-live \
  TRACE=/path/to/failure.sexp LIVE_EMACS_FLAGS=-nw
```

A plain trace succeeds when its driver completes.  A failure artifact succeeds
only when replay reaches the recorded failure signature.  It fails when the
case now passes or a different oracle fails.

`make trace-selftest` checks the data round trip, exact raw bytes, strict
reader, replay without random calls, structured expected/observed state, and
same-signature reproduction.

## Compatibility rule

Format 1 is immutable.  A change that gives an existing field a different
meaning requires a new format number and an explicit reader for both versions.
Adding a generator option does not change the format when the resulting choice
is already represented in `:initial-state` or `:actions`.
