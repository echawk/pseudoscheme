# Roadmap

Where things stand and what to do next, roughly in order. The numbers
come from the runners in `tests/` and the mode documents; re-run them
rather than trusting this file. Updated 2026-10-18.

## Before the rename: Quasischeme 1.0

At 1.0 the project becomes Quasischeme (docs/rename-quasischeme.md).
The bar set for it, and where each part is:

| 1.0 requirement | state |
|---|---|
| Every Revised Report, R1RS to R7RS | **done**: R7RS, R6RS, R5RS suites pass in full; R4RS, IEEE, R3RS, R2RS and the Revised Report have modes of their own, with all 993 of the reports' examples passing (docs/reports.md) |
| All the SRFIs | **done**: every final, unwithdrawn SRFI through 274 (209 of them), with 122 test programs (`make test-srfi`) |
| Guile | **done in substance**: Guile 3.0's own boot-9, psyntax and modules; its test suite with 4 failures and 1 error left (docs/guile.md); `.go` files on a VM in Lisp; Guix computes the same `hello` derivation as real Guix; a libguile that LilyPond typesets on |
| Chez | **done in substance**: 1257 of `(chezscheme)`'s 1715 names (the missing ones are mostly Chez's internals, docs/chez.md, "Not planned"); Chez's mats; Idris 2 bootstraps and compiles programs |
| Emacs through SLY/Slynk and SLIME/Swank | **done** (docs/emacs.md) |
| An LSP server | moved after 1.0 (section 7) |

So the requirements are met, and `utils/rename/rename.sh` was tried
again on 2026-10-18 (below). What is left is checking, and the
mechanical rename itself:

1. **A full test run on the current tree.** The optimizer's first pass
   (cp0 and cptypes, src/cp0.lisp and src/cptypes.lisp, on by default
   at level 2) landed on 2026-10-18 and changes the code every mode
   compiles. Every number in this file and the mode documents predates
   it. Run `make test`, `make test-escape`, `make test-cli`,
   `make test-srfi`, `make test-programs`, `make test-chez`,
   `make test-guile`, `sh tests/racket/run.sh linklet`,
   `sh tests/libguile/run.sh`, LilyPond and Guix in the container
   (tests/lilypond/, tests/guix/run.sh), and `make bootstrap-check`.
   Anything the optimizer breaks is fixed, or the default level is
   lowered (`PSEUDOSCHEME_OPTIMIZE=0|1|2`) until it is.
2. **Refresh the numbers** in README.md, this file and the mode
   documents from that run: the Chez mats (README.md says about 17,500
   pass; docs/chez.md's table, from 2026-10-13, has 16,391), the Guile
   suite (docs/guile.md has both 42,702 and 43,278), Racket's suite
   (docs/racket.md's full run predates its regexp layer), and the
   benchmarks (bench/RESULTS.md predates the optimizer).
3. **Decide what stays out of 1.0**, and say so in the README:
   - Guile's last 4 failures and 1 error, the VM instructions only
     libguile's generated code uses (docs/guile-vm.md, "Left"), and
     Guix building `hello` rather than only its derivation;
   - Racket, which isn't a 1.0 requirement: `racket/base` and `racket`
     run, Racket's own suite doesn't yet (section 6);
   - the user's open directions (section 7).
4. **The rename**, as docs/rename-quasischeme.md describes it:
   `sh utils/rename/rename.sh` (a dry run), then `--apply` on a clean
   tree, `utils/rename/check.sh`, the history prose by hand, the Emacs
   package, the version (3.0 to 1.0), regenerating the `.qso` files and
   psyntax's image, the full test run again, and the steps outside the
   repository (`utils/rename/out-of-repo.sh`). One commit. A trial on
   2026-10-18, at f21e07c in a scratch worktree, rewrote 500 files,
   moved 42, left nothing for `check.sh`, built `bin/quasischeme`, and
   passed the CLI tests (43 of 43) and R7RS's suite (978 of 978).

## Where things stand

| runner | result |
|---|---|
| `tests/run-r7rs-tests.lisp` (chibi's R7RS suite) | 978 of 978 (977 with `--continuations=escape`) |
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8902 of 8902 (8900 escape-only); all 25 programs run to completion |
| `tests/run-r5rs-tests.lisp` (chibi's R5RS suite) | 189 of 189 (188 escape-only; 183 of 188 with `--classic`) |
| the older reports (docs/reports.md) | 993 of 993 examples |
| `tests/run-interop-tests.lisp` | 116/116 |
| `tests/run-library-tests.lisp` | 61/61 |
| `tests/run-srfi-system-tests.lisp` (SRFIs 18, 106, 170, 229) | 62/62 |
| `tests/run-syntax-case-tests.lisp` | 17/17 |
| `tests/run-continuation-tests.lisp` | 45/45 |
| `make -C contrib/cli test` | 43/43 |
| `make test-programs` (`tests/programs/`, needs Quicklisp) | 5 of 5 programs |
| `make test-srfi` (`src/srfi/tests/`) | 122 of 122 test programs |
| `make test-chez` (Chez's mats) | 16,391 clauses pass, 986 fail, 391 error (docs/chez.md, 2026-10-13) |
| `make test-guile` (Guile's suite) | 43,278 pass, 4 fail, 1 error (docs/guile.md) |
| `tests/racket/run.sh linklet` | 6 linklets, 14 expander expressions, 18 `#lang racket/base` programs |
| `tests/run-library-corpus.lisp` (real libraries) | Akku: 228 of 387; snow-fort: 91 of 130 |
| `bench/` (r7rs-benchmarks) | 57/57; geometric mean 1.37× Chez's time, 1.30× escape-only (Guile 2.8×, Gauche 9.2×); before the optimizer |

```
 R1RS..R7RS source   Chez source        Guile source             Racket source
        \               |                    |                        |
         psyntax  (vendor/psyntax)     Guile's psyntax          Racket's expander
         |                             -> Tree-IL               -> linklets
         v                                   |                        |
   core Scheme <-----------------------------+------------------------+
         |
   cp0, cptypes (src/cp0.lisp, src/cptypes.lisp)
         |
   continuations (src/continuations.lisp) -> translator (src/*.scm) -> Common Lisp
```

The runtimes: src/r6rs/, src/r7rs/, src/numbers.lisp, src/chez/,
src/guile/, src/racket/native/, src/compat/, src/interop.lisp; the SRFIs
are Scheme in src/srfi/; libguile's C layer is src/libguile/.

## 1. Speed

bench/RESULTS.md has the last runs and what made the difference;
docs/optimizations.md has the ideas, their status and the optimizer's
plan. Next:

- **Measure the optimizer** (`bench/run.sh`, and `bench/run.sh modes`)
  against the last run, then its plan's later items: unboxed flonums,
  a closed world for `--compile` executables and library bodies, escape
  analysis, cpletrec, records as one object.
- **The slowest benchmarks**, `ack`, `fibc`, `divrec`, `browse` (about
  3.5×): what's left is SBCL's call and allocation costs.
- **R6RS overrides of open-coded primitives** (`list-tail`,
  `integer->char`, `assq`/`assv`, the character case procedures) are
  calls; `*inline-primitives*` (src/psyntax.lisp) could keep the common
  case inline.
- **psyntax's identifier lookup** searches ribs linearly; hashed ribs,
  as Ikarus and Chez have, would cut a third of a macro-heavy library's
  expansion.
- **Interpreting run-once code**: Guile mode interprets top-level forms
  that make no procedure (`trivial-core-p`, src/guile/boot.lisp) rather
  than compiling them; the same would help Racket module bodies, R7RS
  and Chez top levels, and REPL input.

## 2. The library cache

Compiled libraries are cached as fasls (src/library-cache.lisp). Left:
- Libraries holding something the Lisp printer can't print readably (a
  NaN literal) aren't cached.
- Libraries not found through the library path (defined at the REPL or
  in a program, `(cl <package>)` libraries), and their dependents, aren't
  cached.
- Nothing removes stale entries; `pseudoscheme --clear-cache` or a
  cleanup by age would do.
- ASDF's `:r7rs-library`/`:r6rs-library` components aren't cached.
- The build signature changes with any source file's date, so every
  edit recompiles every cached library.

## 3. Continuations

Full, re-entrant and delimited continuations are the default
(docs/continuations.md). Next, from its "Further work": frame-aware
versions of the Lisp primitives that call Scheme (the sorts, R6RS's
folds, the bridge), which today push a barrier that re-entry can't
cross; safety information across libraries; SRFIs 226 and 248 on the
native continuation marks rather than on call/cc and dynamic-wind.

## 4. The Lisp bridge

What exists is in docs/interop.md. Next:
- Assignment across the boundary (a Scheme variable is read-only inside
  a Lisp macro call, and the reverse).
- A package/system map (`(cl bt)` autoloading `bordeaux-threads`), and
  predicate overrides per import.
- An Akku/snow helper for Lisp.
- The small issues found writing SRFIs 18, 106, 126 and 170:
  `lisp-funcall` of a raw function passes `#f` unconverted; a predicate
  keeps only its first value; a Lisp condition is a plain `&assertion`
  to Scheme; `#f` through a Lisp macro's body comes back as `()`; a
  `(cl ...)` export that is both a function and a type; `pkg::sym`
  inside a `lisp` form.
- **Thread safety**: one lock around the expander is done
  (`*expander-lock*`). Left: `parameterize` binding a special variable,
  inherited by new threads, instead of assigning the global value
  (only the port parameters and Chez's `fork-thread` threads are
  per-thread today); then per-thread expander state, so threads can
  expand at the same time.

## 5. Real-world libraries

From `tests/run-library-corpus.lisp`: xitomatl loads 123 of 127
libraries; left are `(xitomatl profiler ...)` (a `let-syntax`-bound
`define` that doesn't take effect) and `(xitomatl R6RS-lexer)` (an
`identifier-append` identifier not found). Some libraries rely on
chibi's or Gauche's leniency, such as duplicate pattern variables in
`syntax-rules`, and aren't counted as bugs.

## 6. The other Schemes' modes

- **Chez** (docs/chez.md): what's left is the missing procedures that
  aren't Chez's internals, and the mats that still fail; busy work.
- **Guile** (docs/guile.md, docs/guile-vm.md): the suite's last
  failures; the VM instructions only libguile's generated code uses;
  `,trace` through natively compiled code; Guix building `hello` in the
  container.
- **libguile** (docs/libguile.md, "What's next"): the 137 functions
  guile-gnutls, guile-ssh and guile-avahi import (ports first), so that
  Guix's TLS runs; bigger LilyPond inputs; our own `libguile.h`.
- **Racket** (docs/racket.md): `--racket` is strategy B, our linklet
  compiler; `--racket=cs` (strategy A) reports "not implemented yet".
  Next: Racket's own suite file by file, a faster cold start, real
  places and futures, and Racket's thread and io layers in place of our
  natives.

## 7. Directions queued

- **An LSP server** for editors other than Emacs, after 1.0.

- **Racket parallelism** on bordeaux-threads, lparallel or chanl, for
  futures and places; Racket threads are SBCL threads with a baton
  today (src/racket/native/threads.lisp).
- **Opt-in optimizations**: a declarative top level (direct calls
  between top-level procedures), chosen by a flag, an environment
  variable or an importable declaration, as C compilers' flags are,
  since it changes what redefinition does.
- **Portability** beyond SBCL: implementation-specific code goes
  through portability libraries (float-features, cl-unicode,
  trivial-gray-streams, trivial-cltl2, trivial-garbage,
  bordeaux-threads); left per implementation are `disable-float-traps`,
  listing the environment, the standard binary ports, and the CLI's
  contribs and interrupts.
- **Bootstrapping** from other Schemes is done (boot/README.md); left
  are an s7 adapter for the `.pso` step and stage0 adapters for more
  hosts, if wanted.
