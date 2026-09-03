# fzfa fuzz contract catalog

This file defines the behavior that the fuzz harness is intended to test.  It
is based on the fixes between `9927468` and `caec167`, their ERT regressions,
and the observed gaps in the draft fuzz PRs.

The catalog separates three questions:

1. What behavior does fzfa promise?
2. What short event sequence can distinguish that behavior from a bug?
3. Does the current fuzz harness have an independent oracle for it?

A case reaching the relevant function is not sufficient.  A row is `partial`
until its generator can reach the witness and its oracle rejects a controlled
broken implementation.

## Scope

The catalog covers fzfa's Elisp-visible contracts at these boundaries:

- callback producers and their lifecycle;
- request-owned fzf-native sessions;
- frontend publication and ownership;
- process output as it crosses into Elisp;
- live completion frontend behavior; and
- teardown of buffers, processes, timers, and advice.

The fzf-native C implementation has its own ERT, C, session, and libFuzzer
coverage.  fzfa should test the contract at that seam rather than duplicate
the native parser and scorer internals.

Ordering, layout, or backend-tool behavior is a fuzz contract only when fzfa
documents it.  A differential mismatch in unspecified behavior is a triage
candidate, not automatically an fzfa bug.

## Status meanings

- `partial`: a draft fuzz lane reaches some of the contract, but an oracle,
  input dimension, or negative control is missing.
- `ERT only`: a deterministic regression exists outside `fuzz/`.
- `gap`: no draft fuzz lane exercises the contract.
- `known product gap`: the desired contract is recorded, but current fzfa is
  known not to satisfy it in every context.
- `qualified`: the generator reaches the witness and the named oracle rejects
  a controlled broken behavior.

Phase-two qualification is recorded per row.  A qualified row names the
controlled failure that its self-test rejects.

## Historical contract matrix

### FZFA-C01: only the newest producer callback may publish

- **Contract:** After a new fetch starts, callbacks from an older fetch must
  not change the source snapshot, total, or visible frontend state.
- **Minimal witness:** Fetch `"a"`; fetch `"ab"`; deliver the callback for
  `"a"`; observe the source before delivering `"ab"`.
- **Oracle:** Build the expected snapshot and total before calling the old
  callback.  Observe both immediately afterward.  Expected values must not
  share list structure or strings with callback input or fzfa state.
- **Generator neighborhood:** Repeated queries, equal queries, old/new callback
  permutations, duplicate candidate strings, and text properties.
- **Evidence:** `fzfa-source-fetch-stale-callback-discarded` and the token
  checks added around `fzfa--source-fetch`.
- **Draft status:** `qualified` in #21.  Expected callback values are copied
  before delivery.  The self-test rejects both an aliased snapshot mutation
  and a stale callback that publishes.

### FZFA-C02: stopped sources are inert

- **Contract:** After source cleanup, a captured producer callback or already
  queued refresh must not change source state or ask a frontend to redraw.
- **Minimal witness:** Fetch; deliver a result that queues refresh; stop;
  invoke the queued refresh and every captured callback.
- **Oracle:** Record snapshot, total, producer token, refresh count, and queued
  work at stop.  They must remain unchanged except for documented teardown
  fields.
- **Generator neighborhood:** Stop before delivery, stop after delivery but
  before refresh, repeated stop, restart then stop, and late callbacks from
  every prior fetch.
- **Evidence:** `f0fd0e0`, `e712837`,
  `fzfa-source-fetch-queued-refresh-rechecks-token`, and
  `fzfa-source-fetch-callback-after-stop-is-inert`.
- **Draft status:** `qualified` in #21.  Generated traces end at the first stop,
  a reachability check requires a queued refresh at stop, and the self-test
  rejects a late publication after teardown.

### FZFA-C03: classifying a producer must not run it

- **Contract:** Constructing a Helm source may inspect a candidate function's
  arity, but it must not call a producer.  The first real fetch calls it once.
- **Minimal witness:** Construct a source around a producer that increments a
  counter; observe zero calls; request candidates; observe one call.
- **Oracle:** Count calls and visible side effects before construction, after
  construction, and after the first fetch.
- **Generator neighborhood:** Lists, zero-argument functions, producer
  functions, optional arguments, synchronous callbacks, and asynchronous
  callbacks.
- **Evidence:** `0c4fcc7`, `fzfa-helm-producer-is-not-fired-during-construction`,
  and `fzfa-candidates-kind-preserves-existing-function-classes`.
- **Draft status:** `ERT only`.

### FZFA-C04: native request results belong to one request epoch

- **Contract:** A native result may publish only when handle, request ID,
  request signature, and local request epoch still identify the request that
  produced it.  Reusing equal numeric IDs must not revive revoked ownership.
- **Minimal witness:** Submit request ID 7; clear or restart the source;
  receive ID 7 again; finish materializing the first request.
- **Oracle:** Observe source state before materialization and after every
  reentrant native or reporting callback.  The obsolete candidates, counts,
  generation, and failure must not commit.
- **Generator neighborhood:** Equal-ID ABA, handle replacement, changed query,
  changed cap or matching policy, restart during snapshot, and restart during
  error reporting.
- **Evidence:** `84641e9`, `f28e18d`,
  `fzfa-session-snapshot-restart-discards-obsolete-result`,
  `fzfa-session-request-epoch-blocks-equal-id-aba-result`, and
  `fzfa-session-failure-report-rechecks-request-owner`.
- **Draft status:** `partial` in #21.  Its poller replay covers handle
  replacement, but not request-signature changes, equal-ID ABA, or reentrancy
  during snapshot and error callbacks.

### FZFA-C05: publication is committed only after the owning frontend renders

- **Contract:** Observing a new native generation does not acknowledge it.
  The generation is committed only after the owning frontend completes the
  scheduled refresh.  Revocation during that refresh leaves it uncommitted.
- **Minimal witness:** Poll generation 1; schedule publication; replace the
  owner or handle; run the publication closure.
- **Oracle:** Record frontend owner, handle, generation, exhibit result, and
  committed generation at callback time, not only after the trace finishes.
- **Generator neighborhood:** Unsupported frontend, nested minibuffer,
  ownership change during candidate lookup, handle replacement before the
  scheduled callback, and revocation during exhibit.
- **Evidence:** `dba133b`, `fzfa-minibuffer-owner-rejects-nested-session`,
  `fzfa-frontend-exhibit-acknowledges-supported-refresh`,
  `fzfa-session-poller-commits-after-scheduled-publication`, and
  `fzfa-session-poller-rejects-revoked-publication`.
- **Draft status:** `partial` in #21.  The fixed poller replay covers one
  replacement ordering; the generated model does not preserve callback-time
  observations.

### FZFA-C06: native redraws preserve completed work

- **Contract:** Re-rendering an unchanged request must poll rather than submit
  again.  An unchanged completed generation reuses the materialized result.
  A presentation-policy change may rematerialize without rescoring.  While a
  replacement request is pending or has failed, core and Helm frontends retain
  the last completed candidates; their displayed total may advance to the
  native live-pool boundary.
- **Minimal witness:** Complete one request; submit a replacement; return a
  running status with a larger pool; then fail it.  Separately render one
  completed request twice and change only highlight policy.
- **Oracle:** Count submits, status calls, and snapshots.  Compare candidate
  identity, filtered count, live total, and presentation after each status.
- **Generator neighborhood:** Query, cap, case mode, fuzzy mode, filter-only
  settings, highlight policy, stable versus growing pool generations, and
  single versus multi-source core and Helm adapters.
- **Evidence:** `84641e9`, `6e0c0f2`, `648a3a1`,
  `fzfa-source-submit-deduplicates-locally`,
  `fzfa-session-render-polls-without-resubmitting`, and
  `fzfa-session-presentation-change-rematerializes-without-rescore`, plus the
  `fzfa-helm-*-preserves-last-result-on-failure` regressions.
- **Draft status:** `gap`.

### FZFA-C07: producer and matcher failures are terminal and reported once

- **Contract:** A failed submit or matcher request does not retry forever.
  A producer failure is reported once even when useful partial candidates
  remain visible and the frontend redraws repeatedly.
- **Minimal witness:** Emit `"partial\n"`; exit 7; poll and redraw more than
  once.
- **Oracle:** Require the partial candidate, exact terminal state, one message,
  no extra submit, and stable repeated output including candidates and counts.
- **Generator neighborhood:** Failure before output, after partial output,
  during a running matcher request, repeated status reads, and stopped source.
- **Evidence:** `1ba6033`,
  `fzfa-session-running-status-reports-producer-failure-once`,
  `fzfa-producer-failure-with-partial-output-reports-once`, and
  `fzfa-session-end-to-end-reports-partial-producer-failure`.
- **Draft status:** `partial` in #22.  It now compares the complete stable
  redraw value with the terminal result.  The stable-redraw canary changes a
  count and is rejected.  The one-message failure assertion still lacks its
  own controlled canary.

### FZFA-C08: fzfa matching settings remain local to an fzfa call

- **Contract:** fzfa may bridge its matching policy into fzf-native while it
  scores, but setup and later direct native calls must retain their own global
  settings.
- **Minimal witness:** Configure direct native matching; run `fzfa-setup` and
  an fzfa scoring call with different policy; call fzf-native directly again.
- **Oracle:** Capture every dynamically visible setting at each call boundary
  and compare the final global values with their initial values.
- **Generator neighborhood:** Case mode, fuzzy mode, filter-only length and
  logic, highlight policy, normal return, interruption, and signaled error.
- **Evidence:** `e75ab46`,
  `fzfa-setup-does-not-change-direct-native-matching-options`, and
  `fzfa-all-completions-lazy-highlight-uses-fzfa-policy`.
- **Draft status:** `gap`.

### FZFA-C09: frontend mutation cannot corrupt cached candidate snapshots

- **Contract:** Completion frontends may destructively modify the list spine
  they receive.  That must not change fzfa's cached per-source snapshots,
  candidate multiplicity, or text properties.
- **Minimal witness:** Return the empty-query result; destructively sort,
  reverse, truncate, deduplicate, or attach a dotted tail; fetch it again.
- **Oracle:** Construct independent expected candidate values before invoking
  the completion table.  Require the mutation to change the returned list and
  compare every source snapshot and second lookup, including properties and
  duplicates.
- **Generator neighborhood:** One and multiple sources, duplicate values,
  shared-looking strings, source tags, empty query, and every destructive list
  operation above.
- **Evidence:** `836ae54` and the empty-query copies in the Ivy and pull-model
  collection paths.
- **Draft status:** `qualified` in #21.  The oracle builds independent expected
  strings and list spines before the table runs, requires every mutation to
  change its input, and compares the first result, cached snapshots, and second
  result including properties and duplicates.  Its self-test rejects nil
  results, stripped properties, and a result that aliases the snapshot.

### FZFA-C10: user messages respect minibuffer ownership

- **Contract:** Every notification is logged once.  With an active owning
  fzfa minibuffer, echo-area output is inhibited and the inline cue runs from
  that owner buffer.  Without an owner, no inline cue is emitted.
- **Minimal witness:** Call `fzfa--print` from the owner buffer, a producer
  worker buffer while the owner exists, and a normal buffer with no active
  minibuffer.
- **Oracle:** Check all event counts, inhibition flags, event order, and event
  buffers.  A known exception must match one exact event shape.
- **Generator neighborhood:** The three contexts above, owner replacement,
  nested minibuffers, buffer death, and errors during reporting.
- **Evidence:** `e04916c` and the draft #21 message ownership harness.
- **Draft status:** `known product gap` for the worker-buffer context; its #21
  oracle is `qualified`.  All three contexts run deterministically.  The known
  exception must match one exact event sequence, and the self-test rejects a
  partial repair that emits the inline cue from the worker buffer.

### FZFA-C11: the producer seam preserves valid records and rejects invalid tails

- **Contract:** Complete valid records before a protocol failure remain
  usable.  No NUL-bearing or post-NUL candidate may reach the frontend.  Raw
  non-UTF-8 pathname bytes remain byte-for-byte unchanged when valid.
- **Minimal witness:** Produce `valid\nab<NUL>cd\nlate\n`, with output split at
  different byte boundaries.
- **Oracle:** Validate every publishable interim and terminal result, including
  proper list shape, candidate bytes, forbidden NULs, filtered count, total,
  and stable redraw equality.
- **Generator neighborhood:** LF and CRLF, UTF-8 split inside a code point,
  invalid UTF-8, ANSI split inside an escape, empty rows, duplicates, long
  lines, partial final lines, NUL position, and nonzero exit.
- **Evidence:** fzf-native 2.7's producer contract,
  `fzfa-async-submit-preserves-raw-byte-query`, and draft #22.
- **Draft status:** `qualified` in #22.  A two-chunk producer is required to
  publish the first chunk before the second arrives.  Every publishable result
  is checked, and an injected NUL in that interim result is rejected.  Stable
  redraws must equal the full terminal value.

### FZFA-C12: the configured line cap is part of producer behavior

- **Contract:** Unless a caller explicitly disables it, the ambient
  `fzfa-max-line-length` policy reaches the producer command and bounds rows as
  documented.
- **Minimal witness:** Generate rows at cap minus one, cap, and cap plus one;
  repeat with an explicit unlimited setting.
- **Oracle:** Inspect the command bridge and final candidates.  The expected
  treatment of overlong rows must follow the selected backend's documented
  max-column behavior.
- **Generator neighborhood:** Boundary lengths, multibyte display width versus
  byte length, backend kind, nil, zero, and positive caps.
- **Evidence:** `fzfa--max-columns-flag` and draft #22's long-row category.
- **Draft status:** `qualified` in #22.  Missing spec keys preserve the ambient
  cap, while an explicit nil means unlimited.  Fixed rows at N-1, N, and N+1
  distinguish positive exclusion, negative truncation, and unlimited input.
  The self-test recreates the old implicit-nil mistake and rejects it.

### FZFA-C13: the ugrep adapter keeps valid output while excluding known NUL paths

- **Contract:** The assembled ugrep command succeeds and returns an ordinary
  matching file.  GNU Info files and `emms/cache` do not reach stdout or the
  final native candidate list.  An unrelated late-NUL file is either excluded
  by ugrep or rejected by the native seam.
- **Minimal witness:** Search a directory containing `normal.txt`,
  `manual.info`, `manual.info-1`, `emms/cache`, and `late.bin`; put a unique
  sentinel in the normal file.
- **Oracle:** Require exit status zero and the exact sentinel in raw stdout and
  final candidates before asserting the excluded sentinels are absent.
- **Generator neighborhood:** NUL near and far from the header, matching and
  nonmatching normal files, spaces in paths, and continuation suffixes.
- **Evidence:** `90f721a` and draft #22's tools lane.
- **Draft status:** `qualified` in #22.  The command must exit zero and a unique
  normal-file sentinel must appear in both raw stdout and final candidates
  before exclusions are checked.  Separate canaries remove each positive
  observation and are rejected.

### FZFA-C14: live icomplete growth follows a fresh render

- **Contract:** Initial multiline icomplete output grows a one-line
  mini-window.  A narrowing query displays fewer logical candidates.  Deleting
  back to empty produces a fresh broad display without collapsing the window.
- **Minimal witness:** Open with many distinguishable candidates; observe the
  initial empty render; type `alpha`; wait for its render; delete the query;
  wait for a new empty render.
- **Oracle:** Use observation-driven handshakes.  For initial growth require
  `before < target <= after`.  Compare candidate identities or a discriminating
  count for narrow and broad displays.  Identify renders by sequence number,
  not just query text.
- **Generator neighborhood:** Empty-to-narrow and narrow-to-empty edits,
  different query lengths, no matches, one match, many matches, repeated
  exhibits, max-height caps, and nested advice installation.
- **Evidence:** `9633ff6` and draft #23.
- **Draft status:** `partial` in #23.  No-filter, no-fit, and stale-empty
  controlled mutations currently pass.

## New behavior without a historical failure witness

These contracts were introduced in the same range but were not reconstructed
from a reported regression.  Keep them separate from the historical set until
a failing witness or controlled mutant demonstrates the oracle.

### FZFA-N01: Emacs 31 built-in completion remains bounded and refreshable

- **Contract:** The built-in eager `*Completions*` frontend receives at most
  `fzfa-default-minibuffer-max-candidates` when that positive cap is active,
  can refresh after an async generation, and returns the logical candidate at
  its visible selection.
- **Minimal witness:** Open more candidates than the frontend cap; publish a
  new generation; navigate and accept a candidate.
- **Evidence:** `caec167`.
- **Draft status:** `gap`; #23 covers icomplete only.

## Cross-cutting cleanup contract

Every witness above must finish with the same externally observable resource
inventory it started with, except for explicitly retained user results:

- no live producer or native handle owned by the finished session;
- no uncancelled session timer or queued publication;
- no leaked temporary buffer or process;
- no stale session ownership marker; and
- no additional frontend advice or positive advice refcount.

The current drafts check parts of this inventory.  Phase two should give it one
shared oracle and qualify that oracle by deliberately leaking each resource in
turn.
