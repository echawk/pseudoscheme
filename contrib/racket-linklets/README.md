# Racket's schemified linklets on Pseudoscheme

Racket CS compiles Racket's own runtime (the expander, io, thread,
regexp, and schemify, its compiler pass from linklets to Chez Scheme)
into Chez Scheme code, checked into Racket's repository as
`racket/src/cs/schemified/*.scm`. Each is the body of a Chez library
that runs on Racket's "rumble" layer, about 21,000 lines of Chez Scheme
built on Chez's internals (continuation attachments, `$record`
primitives, engines, stencil vectors).

This directory runs some of those linklets on Pseudoscheme's Chez mode,
unmodified, on a small stand-in for rumble:

- `racket-lite.sls`, the library `(racket-lite)`: Racket's primitives as
  the linklets use them: structure types, properties and applicable
  structures (`|#%app|`); Chez 10's `make-record-type-descriptor` with
  `(count . mutable-mask)` field specs; prefab uids; immutable and
  mutable hash tables; byte strings; keywords; correlated objects
  (schemify's syntax objects); prompts; continuation marks (as a dynamic
  list: a mark on `exception-handler-key` installs a handler, one on
  `parameterization-key` binds parameters); Racket's error procedures;
  Racket-style input ports (`make-input-port`); and the linklet runtime
  a schemified linklet's `lambda` expects (variables, the module
  prompt). Written for Pseudoscheme, not derived from rumble's source.
- `load-linklet.ss FILE EXPR ...`: evaluates a linklet's body at a Chez
  top level with `(racket-lite)` imported, then the expressions.
- `rx-tests.ss`: Racket's own regexp tests, from
  `pkgs/racket-test-core/tests/racket/rx.rktl` (its `test-regexp` cases
  and its PCRE table), which `extract-rx-tests.py` converts (Racket's
  `#"..."` byte strings to `#vu8(...)`).
- `schemify-demo.ss`: Racket's schemify, running here with Racket's
  tables of known primitives (`primitive/*.ss`), schemifies a small
  linklet; the result is evaluated here and its exports called.

Run, with a checkout of [racket/racket](https://github.com/racket/racket)
(tried at `46a423b607`):

```sh
contrib/racket-linklets/run.sh ~/src/racket
```

What it shows:

| linklet | forms | result |
|---|---|---|
| `regexp.scm` (Racket's regexp matcher) | 398, all load | Racket's regexp tests: 774 of 774 `test-regexp` checks and 6,092 of 6,092 PCRE-table checks pass |
| `known.scm` (schemify's "known" structures) | 329, all load | |
| `schemify.scm` (Racket's schemify pass) | 781, all load | schemifies a linklet with a recursive procedure, a structure type and `let-values`; the schemified code runs here: `(fact 20)`, the structure's constructor and predicate |

Not yet: `thread.scm`, `io.scm` and `expander.scm` need much more of
rumble: Racket's threads (engines), ports, paths and the file system,
namespaces. The continuation marks here don't replace a mark in tail
position, and immutable hash tables are association lists, so
performance isn't Racket's.
