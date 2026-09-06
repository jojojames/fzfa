# fzfa fuzz tests

These tests exercise the boundaries where the recent fzfa bugs appeared. They
live under `fuzz/`, are not loaded by the package, and do not replace any fzfa
function.

[`CONTRACTS.md`](CONTRACTS.md) records the historical failure witnesses,
expected behavior, generator neighborhoods, and current coverage gaps.  A
contract remains partial until a controlled broken implementation makes its
oracle fail.

The targets are:

- `make compile`: byte-compile every fuzz harness and treat warnings as errors.
- `make selftest`: require generated producer traces to reach their intended
  race witnesses, then inject eight controlled defects and require the matching
  state oracle to reject each one.
- `make replay`: run small fixed regression cases.
- `make state`: generate candidate-list mutations, late producer callbacks,
  restart/stop races, stale poll publications, and message ownership contexts.

`state` uses a fake clock and timer queue, but it calls fzfa's real state
functions.

## Run locally

The default directory layout is:

```text
parent/
  fzf-async/
  fzf-native/
```

From `fzf-async/fuzz`:

```sh
make selftest replay state
```

If fzf-native is elsewhere, pass it explicitly:

```sh
make state FZF_NATIVE_DIR=/path/to/fzf-native
```

The state job covers Emacs 29.1, 30.1, and the current snapshot.

## Reproduce a failure

Every failure prints its seed and generated trace. Run one case from that seed:

```sh
FZFA_FUZZ_SEED=123 make state CASES=1
```

Useful controls are:

- `CASES`: number of generated state cases.
- `STEPS`: operations in each producer-lifecycle state trace.
- `FZFA_FUZZ_SEED`: first deterministic seed.

The state lane currently labels the process-buffer `fzfa--print` ownership
case as `KNOWN` when it occurs. It does not require that gap to remain: once the
production behavior is fixed, the same replay prints `RESOLVED` and continues.
