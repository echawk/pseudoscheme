# Chez Scheme on Pseudoscheme: a `--chez` mode

Status: investigation and plan. What exists is a partial `(chezscheme)`
library for the Chez variants of Akku packages (`foo.chezscheme.sls`),
src/compat/chezscheme.scm.

The goal: `pseudoscheme --chez prog.ss` runs programs written for Chez
Scheme 10: scripts that rely on Chez's top level, R6RS programs and
libraries that import `(chezscheme)`, and the libraries written for it.

The figures below come from Chez Scheme 10.4.1, installed here, and
from Pseudoscheme's own `(chezscheme)`. The comparison is of exported
names: `(library-exports '(chezscheme))` in Chez against psyntax's
`library-export-bindings` here.

## Summary

Of the three foreign Schemes considered (Racket in docs/racket.md,
Guile in docs/guile.md, and Chez), **Chez is the closest**, and a useful
`--chez` could come in stages, each usable on its own:

- **The expander is already Chez's.** psyntax (vendor/psyntax) is
  Ghuloum and Dybvig's portable version of the R6RS library and
  `syntax-case` system Chez is built on. Chez's `module`, `import` (of a
  module), `alias` and `define-property` already work here, through
  `(pseudoscheme)`; they just aren't exported from `(chezscheme)` yet.
- **R6RS is complete** (all 8902 of Racket's R6RS tests), and Chez's
  language is R6RS plus extensions. Racket and Guile each need a whole
  runtime layer (linklets and primitive instances; libguile and
  Tree-IL) before their own code can run. Chez needs no such layer.
- **A top level with a different base library already exists.** R5RS
  runs at a top level whose bindings are `(pseudoscheme r5rs)`
  (`ps-r7rs::eval-at-r5rs-repl`). A Chez top level would be the same
  thing with `(chezscheme)`.
- **Half the names are there.** Chez's `(chezscheme)` exports 1715
  names. Ours exports 803 of them, and 59 more exist elsewhere in
  Pseudoscheme (SRFI 19's time procedures, SRFI 226's continuation
  marks, SRFI 60's `logand` and so on).

The approach differs from the Racket and Guile plans. Chez's own
libraries are written in Scheme too (Chez's `s/*.ss`), but they're
written for its compiler and runtime: `$primitive`, `#%$` system
procedures, its object layouts. Hosting them would mean emulating Chez's
internals. Instead, `(chezscheme)` is implemented here, as the R6RS
libraries were (src/r6rs/), and checked against Chez's own behaviour.

## What `--chez` would do

Chez runs code three ways, and `--chez` would match each:

| Chez | meaning | here |
|---|---|---|
| `chez --script f.ss` | load `f.ss` into the interaction environment: all of `(chezscheme)` visible, no `import`, top-level definitions redefinable | a top level whose base is `(chezscheme)`, as R5RS's is `(pseudoscheme r5rs)` |
| `chez --program f.sps` | an R6RS top-level program | already works (`--r6rs`), with `(chezscheme)` importable |
| the REPL | the interaction environment | the same top level, as `-i` |

Libraries are found as now. `.ss` and `.sls` are already searched, and
`foo.chezscheme.sls` variants are already preferred to generic files.
Chez's `library-directories` and `library-extensions` parameters would
read and set the search path.

## The gap, measured

Of the 912 names Chez exports that our `(chezscheme)` doesn't, 59 exist
elsewhere here. The other 853 fall into these groups, sorted by name
(so approximately):

| group | names | examples | difficulty |
|---|---|---|---|
| `r6rs:` variants | 57 | `r6rs:case`, `r6rs:fx+`, `r6rs:open-input-file` | trivial: the R6RS bindings under another name |
| numbers | 76 | `1+`, `fx<`, `fl=`, `logor`, `isqrt`, `random`, `sinh`, `cfl*` | mostly easy; `cfl` (complex flonums) and `pseudo-random-generator` take some work |
| lists, symbols, paths, misc | about 90 | `sort`, `merge`, `list*`, `andmap`, `subst`, `putprop`/`getprop`, `gensym->unique-string`, `path-parent`, `bytevector-u24-ref` | easy: property lists are CL symbol plists, paths are string work |
| records | 26 | `define-record`, `record-case`, `record-type-descriptor`, `record-writer`, `csv7:` | easy to medium, on the R6RS records |
| hashtables | 23 | `eq-hashtable-ref`, `symbol-hashtable-set!`, `hashtable-cells`, the old `get-hash-table` | easy, on R6RS hashtables |
| fxvectors, flvectors, immutable data | 49 | `fxvector-ref`, `make-flvector`, `immutable-vector`, `string->immutable-string`, stencil vectors | easy (Lisp specialized arrays), except immutability (a flag, checked by the mutators) and stencil vectors |
| printer parameters | 23 | `print-radix`, `print-graph`, `print-length`, `pretty-line-length` | medium: the writer has to consult them |
| syntax and top level | about 55 | `module`, `import`, `meta`, `meta-cond`, `eval-when`, `extend-syntax`, `fluid-let-syntax`, `datum`, `rec`, `top-level-value`, `define-top-level-value` | `module`/`import`/`alias` exist; `eval-when`, `meta` and `fluid-let-syntax` need psyntax work; top-level values are easy |
| compiling, loading, fasl, expansion | 94 | `compile-file`, `compile-library`, `compile-program`, `load-library`, `expand`, `interpret`, `fasl-read`/`fasl-write`, `optimize-level`, `generate-inspector-information` | the knobs (about half) can be accepted and ignored; the rest map onto the compiled-library cache and the translator; `fasl-write` needs a format of our own |
| storage: GC, guardians, weak and ephemeron tables | 31 | `make-weak-eq-hashtable`, `make-ephemeron-eq-hashtable`, `collect`, `collect-request-handler`, wrapper procedures | medium: SBCL's weak hash tables (`:weakness :key` is an ephemeron table) and finalizers; the collector's knobs accepted |
| threads | 19 | `fork-thread`, `make-mutex`, `with-mutex`, `make-condition`, `condition-wait`, `make-thread-parameter` | medium: bordeaux-threads, as SRFI 18 is, after psyntax gets a lock (ROADMAP 4) |
| FFI and ftypes | 58 | `load-shared-object`, `foreign-procedure`, `foreign-callable`, `foreign-alloc`, `define-ftype`, `ftype-ref`, `ftype-set!` | medium to large: on CFFI (tests/programs/c-libraries.scm does all of this from Scheme already); ftypes are CFFI's structs plus accessor paths |
| ports beyond R6RS | about 100 | `make-input-port` (Chez's handler API), `open-process-ports`, `block-read`, `unread-char`, `file-position`, and about 60 buffer accessors (`port-input-buffer`, `set-port-output-index!`) | medium for the procedures programs use; the buffer accessors expose Chez's port representation, low priority |
| engines, timers, interrupts | 9 | `make-engine`, `set-timer`, `timer-interrupt-handler`, `with-interrupts-disabled` | hard: see below |
| debugging, inspection, profiling | about 130 | `trace-define`, `trace-lambda`, `inspect`, `debug`, `profile-dump-html`, cost centers, `sstats`, source objects and annotations | `trace-*` easy (macros); `inspect` and `debug` could map onto SBCL's; profiling, cost centers and source annotations mostly skipped |

Most of the first eight groups, about 400 names, are a few days of
work and cover what ordinary Chez programs use.

### Reader syntax

Chez's extensions to R6RS's datum syntax, against ours:

| syntax | Chez | here |
|---|---|---|
| `[...]` brackets, `#0=` datum labels | ✓ | ✓ |
| `#&x` boxes | ✓ | no (boxes exist, SRFI 111) |
| `#!eof`, `#!bwp`, `#!base-rtd` | ✓ | no |
| `#3(1 2 3)` length-prefixed vectors | ✓ | no |
| `#vfx(...)`, `#vfl(...)` fxvectors and flvectors | ✓ | no |
| `#%car` (`($primitive car)`) | ✓ | no |
| `#[...]` records | ✓ | no |
| `#{name unique}` gensyms | ✓ | no |

### Behaviour, not names

Chez programs also depend on how things behave. These differ today:

| | Chez 10.4.1 | Pseudoscheme |
|---|---|---|
| `(greatest-fixnum)` | 2⁶⁰ − 1 | 2⁶² − 1 (SBCL's fixnums) |
| `(format "~10,2f" 3.14159)` | `"      3.14"` | `"~10,2f"` (unsupported directive) |
| `(write 1e21)` | `1e21` | `1.0e21` |
| an uncaught `(car 5)` | `Exception in car: 5 is not a pair` | `Error: not a pair 5` |
| `sort` | `(sort < list)` | absent (`list-sort` has that order) |

- **`format`**: Chez's is close to Common Lisp's, so CL's `format` could
  do most of the work behind a translation of the directives that
  differ.
- **Messages**: Chez writes an uncaught error as `Exception in WHO:
  MESSAGE` with the irritants formatted into the message (Chez's
  `error` takes a format string), and tests and scripts that compare
  output depend on it. `(chezscheme)`'s `error` and the top level's
  reporter would need to follow it.
- **Fixnum width**: code that checks `fixnum?` at Chez's boundaries,
  or relies on `fx+` overflowing at 2⁶⁰, will see SBCL's wider fixnums.
  This is an R6RS-allowed difference, and it would stay.

## Engines

Chez's engines (`make-engine`) run a thunk for a number of *ticks*, then
suspend it and return a new engine for the rest. They are Chez's
preemptive multitasking, and its timer interrupts underlie them. Two
ways here:

- **Ticks at sites.** Full continuations already put a frame push at
  every non-tail call that might capture (docs/continuations.md).
  Decrementing a tick counter at those points, and at loop back edges,
  and capturing the continuation when it reaches zero, gives engines
  with Chez's semantics at those points. The cost would be paid only by
  code compiled for engines, a third compilation mode.
- **Threads.** An engine as a thread that runs until its time slice
  ends and is then suspended: simpler, but it changes what `dynamic-wind`
  and parameters see, and it's preemptive at arbitrary points rather
  than at Chez's.

Neither is needed for most programs. Engines can come last.

## What to test against

- **Chez's own tests**, the `mats` in Chez's source tree, which check
  each primitive's behaviour, error messages included. Run a subset as
  chibi's and Racket's suites are run here (tests/run-*-tests.lisp).
- **Real Chez code**: the Chez variants already in the Akku corpus
  (`tests/run-library-corpus.lisp`), the nanopass framework, chez-srfi,
  and programs written as Chez scripts.

## Plan

1. **The top level and the flag.** `--chez` (and `chez`-named links,
   as `scheme-r6rs` is one): a top level based on `(chezscheme)`, the
   script, program and REPL modes, and `--libdirs`/`--libexts` as Chez
   names them.
2. **Re-export what exists.** `module`, `import`, `alias`,
   `define-property`, and the 59 names found elsewhere.
3. **The easy groups**: `r6rs:` variants, numbers, lists and symbols,
   paths, records, hashtables, fx/flvectors, `trace-define` and
   friends, top-level values, and the optimization knobs (accepted and
   ignored). About 400 names, as Scheme in src/compat/chezscheme.scm or
   Lisp in src/compat/chezscheme-host.lisp.
4. **Reader syntax**: `#&`, `#!eof`, `#!bwp`, `#N(...)`, `#vfx`, `#vfl`,
   `#%`.
5. **Behaviour**: `format`'s directives, `Exception in` messages,
   flonum printing as Chez does when in `--chez` mode, printer
   parameters.
6. **The medium groups**: the FFI and ftypes on CFFI, threads (after
   ROADMAP 4's lock), weak and ephemeron tables, guardians, the compile
   and load API on the compiled-library cache, Chez's port procedures,
   `eval-when` / `meta` / `fluid-let-syntax` in psyntax.
7. **Last, or never**: engines, the inspector and debugger, profiling,
   source objects, boot files and the port-buffer internals.

Steps 1 to 5 would make a `--chez` that runs most Chez scripts and
`(chezscheme)` libraries.
