# Optimization: what's done, what could be, and how it could be chosen

Quasischeme's output is meant to stay readable: a Scheme procedure
becomes a Lisp function a person could have written, and SBCL does the
optimizing. This document lists what the translator already does for
speed and the ideas that remain, with each one's status and an example
of where it would help. Most of the ideas are for a later major version.
The aim throughout is to make the output faster by *adding*
declarations and knowledge, not by restructuring the code into
something unrecognizable.

Where speed stands: ecraven's r7rs-benchmarks (bench/) run at 1.37×
Chez Scheme's time as a geometric mean, with full continuations, and at
1.30× escape-only (bench/RESULTS.md, ROADMAP section 1). Those runs
predate the optimizer's first pass (cp0 and cptypes, 2026-10-18; "Plan",
below), which hasn't been measured on them yet.

## What the translator does now

| technique | where | what it does |
|---|---|---|
| definitions as `letrec` | `definitions-as-letrec`, src/psyntax.lisp | A body's definitions (a program's or library's too) bound once to lambdas become one `labels`, so calls between them are direct calls, not `funcall`s through boxed variables. |
| hoisting big bodies | `hoist-definitions`, src/psyntax.lisp | A top-level form too big for SBCL as one code object has its definitions hoisted into top-level ones, compiled one by one. |
| inline boolean tests | `truep`, src/core.lisp | A Scheme `if` on a value that isn't already a Lisp boolean is an inline `eq` test, not a call, against `#f` as a constant of the code (see idea 7). |
| inline arithmetic | `def-arith`, src/numbers.lisp | Two-argument `+`, `-`, `*`, the comparisons, `quotient`, `remainder`, `modulo` and `zero?` have an inline fixnum (and flonum) case before the generic one. Inexact complex arithmetic has typed parts. |
| open-coded primitives | src/builtin.scm | `car`, `vector-ref`, `string-ref` and the like become `(car (the cons x))`, `(svref (the simple-vector v) i)`: one type check, then the machine instruction. |
| inline host primitives | `*inline-primitives*`, src/psyntax.lisp | Compiler macros for procedures defined in the runtime (the bytevector accessors, the two-argument string comparisons, R7RS record accessors on a record of exactly their type) that keep the common case inline and call the full procedure otherwise. |
| continuation analysis | `find-safe-procedures`, `escape-call/cc-p`, src/continuations.lisp | Calls that can't reach `call/cc` aren't capture sites; a `call/cc` used only to escape is a Lisp `catch`. With full continuations, `fib` compiles to the same code as without (docs/continuations.md, "What makes it fast"). |
| shared compiled lambdas | `canonical-lambda`, src/psyntax.lisp | Macro transformers of the same shape are compiled once, with their constants as arguments. |
| compiled library cache | src/library-cache.lisp | Libraries are expanded and compiled once and loaded as fasls after that; SBCL's `compile` is about 90% of importing a library. |
| size limits | `*inline-arithmetic-limit*`, src/psyntax.lisp | Very big forms are compiled without inline arithmetic, which SBCL can't handle at that size. |

The policy is SBCL's default (`speed 1`, `safety 1`, `debug 1`),
except that full-continuation code is compiled with
`(sb-c::insert-debug-catch 0)` (`*full-policy*`, src/continuations.lisp).
`--continuations=escape` is the one speed option the user chooses
today.

Here is `fib`, as the translator writes it (identifiers simplified):

```lisp
(labels ((fib (n)
           (if (qs:scheme< n '2)
               n
               (qs:scheme+ (fib (qs:scheme- n '1))
                           (fib (qs:scheme- n '2))))))
  #'fib)
```

and a loop over a vector:

```scheme
(lambda (v)
  (let loop ((i 0) (acc 0.0))
    (if (= i (vector-length v))
        acc
        (loop (+ i 1) (+ acc (vector-ref v i))))))
```

```lisp
#'(lambda (v)
    (prog (i acc)
       (psetq i '0 acc '0.0d0)
       (go loop)
     loop
       (if (qs:scheme= i (length (the simple-vector v)))
           (return acc)
           (progn
             (psetq i (qs:scheme+ i '1)
                    acc (qs:scheme+ acc (svref v i)))
             (go loop)))))
```

The ideas below are judged against code like this.

## Ideas

Each idea has a status: **done**, **partly**, or **not started**. For
each, an example shows where it would help.

### 1. A chosen optimization level (not started; the first to do)

Every other idea fits under a level the user chooses. A level would map
to an SBCL policy and switch the more aggressive transformations on.

| level | SBCL policy | meaning |
|---|---|---|
| 0 | `(debug 2) (safety 3)` | debugging: full backtraces, every check |
| 1 (default) | today's | safe, with every error Scheme requires |
| 2 | `(speed 2) (debug 0)` | safe, fewer frames in backtraces (about 8% on the call-heavy benchmarks, ROADMAP section 1) |
| 3 | `(speed 3) (safety 0)` | unsafe, as Chez's `optimize-level 3`: a wrong type is undefined behavior, not an error |

How it could be chosen:
- on the command line, `--optimize-level=N`;
- per library or program, by a declaration form in a library of our
  own, `(import (quasischeme optimize))` then `(declare-optimize-level
  3)`, in the body it applies to;
- by each mode's own mechanism, which the modes already accept and
  ignore: Chez's `optimize-level` parameter and `(optimize-level 3)`
  in `compile-file` (src/chez/top-level.scm has the parameter, with no
  effect), and Guile's `-O0`…`-O3` to `guild compile` and
  `%auto-compilation-options`;
- from Lisp, a special variable, as `qsx::*full-continuations*` is
  now.

The level is part of the library cache's key, as the continuations
mode is.

Unsafe code has to stay unsafe only where it was asked for: a level-3
library calling a level-1 library mustn't remove the second one's
checks. Since a level is a policy around a form, this falls out of how
the translation is compiled.

### 2. Records as one object (not started)

A record is now a struct holding a vector of fields: two allocations,
and an extra indirection on every field access (`gcbench` 2.1×). A
record type could instead be a `defstruct` of its own (or one SBCL
instance of the right length), so `(point-x p)` would compile to a type
check and one load, and SBCL's `typep` of the struct would be the
predicate.

```scheme
(define-record-type point (make-point x y) point? (x point-x) (y point-y))
(define (norm2 p) (+ (* (point-x p) (point-x p)) (* (point-y p) (point-y p))))
```

`norm2` does four field accesses; each now costs two dependent loads
and a type check of the rtd.

Complications: R6RS's procedural record layer, record-type inheritance
(SBCL's `:include` covers single inheritance, which is all R6RS has),
nongenerative types redefined at the REPL, and Chez's and Guile's
reflection on record types (src/chez/, src/guile/ structs and vtables).

### 3. Cross-form and cross-library knowledge (partly)

Inside one top-level form, a known procedure is called directly and its
safety for continuations is known. Across forms (the REPL, or a script's
top level with hoisted definitions) and across libraries, a call goes
through the variable, is a capture site if the callee is unknown, and
can't be inlined. Racket's schemify keeps "known" information per
library export (a procedure of N arguments, a constant, a struct
accessor) for exactly this.

What it would take: the library cache records, for each export, what
the translator learned (known procedure, its arity, whether it's safe
for continuations, whether it's a record accessor or predicate, a small
body to inline). An importing library then uses that.

```scheme
(library (geometry) (export square) (import (rnrs)) (define (square x) (* x x)))
(import (geometry))
(define (sum-squares l) (fold-left (lambda (acc x) (+ acc (square x))) 0 l))
```

Today `square` here is an unknown procedure: the call isn't inlined,
and with full continuations the call is a capture site, so the lambda
needs a frame. With the knowledge exported, the lambda is safe, `square`
can be inlined, and the `fold-left` call becomes the plain primitive.

### 4. Type inference into declarations (not started)

Chez's `cptypes` pass learns types from predicates and from the
primitives' result types, and drops checks it has made redundant. Ours
could emit `(declare (type ...))` and `the` forms, which SBCL uses for
its own unboxing and dispatch. The code stays the same code, with
declarations added.

```scheme
(define (count-to n)
  (let loop ((i 0) (s 0))
    (if (fx<? i n) (loop (fx+ i 1) (fx+ s i)) s)))
```

`fx<?` and `fx+` already imply fixnums, and an R6RS `fx+` that
overflows must raise, so `i` and `s` can be declared `fixnum` and the
loop compiled without the generic case. With generic `+`, a variable
initialized to a fixnum and only incremented by a fixnum isn't
necessarily a fixnum (it can overflow to a bignum), so inference has to
stop there; at level 3 it could assume it.

Flonums profit most: `(fl+ acc (flvector-ref v i))` in a loop could keep
`acc` an unboxed `double-float`, where now each iteration allocates a
boxed one. In the vector loop above, `acc` starts as `0.0` but the
vector's elements are unknown, so nothing can be declared without
type-checking `(vector-ref v i)`.

### 5. `dynamic-extent` (partly done, October 2026: cpdx)

Rest-argument lists that don't escape, and closures passed to
`for-each`, `map`, `vector-for-each` and the like that the procedure
can't keep (the continuation analysis already knows which primitives
these are), could be declared `dynamic-extent` and allocated on the
stack.

```scheme
(define (sum . xs) (apply + xs))
(define (print-all l port) (for-each (lambda (x) (write x port)) l))
```

Done (src/cpdx.lisp, the third pass of OPTIMIZE-CORE, so every mode's
core forms get it): a lambda passed to a primitive that only calls it
during the call (`map`, `for-each`, `vector-for-each`, `filter`,
`call-with-values`, ...) is declared `dynamic-extent` when neither its
body nor the call's other arguments can capture a continuation; a rest
list is when the lambda's body can't capture and only reads the list
(`car`, `null?`, `length`, `cdr` read in turn). Not yet: a rest list
passed on to a loop or to `apply` (as in `sum` above: `apply`'s
procedure may keep its rest list), and procedures defined in Scheme
rather than as host primitives (Guile mode's `map` is boot-9's).
Why "can't capture": a continuation captured while the object is live
copies frames that refer to it, and re-entered after the call returned
they would refer to a stack that is gone. A first version that checked
only the lambda crashed four SRFI tests: in `(map (lambda (p) ...)
(permutations ...))` the closure, made before the other argument ran,
was live across that capturing call.

Effect: none measurable on bench/ (geometric mean within run-to-run
noise; few of its loops pass closures to `map`); 2-3% on a loop that
does nothing but call `map` with a closure, since SBCL's nursery
allocation is already cheap. `QUASISCHEME_CPDX=0` turns it off,
`closures` or `rest` keeps one kind; `QUASISCHEME_OPTIMIZE_STATS=1`
prints, at exit, how many forms each pass handled and what cpdx marked.

`xs` and the closure over `port` are each heap-allocated today.

### 6. Scheme-level inlining and constant folding (partly)

Chez's `cp0` inlines small procedures, folds constants and removes dead
code before compiling. We rely on SBCL for this, which works inside a
`labels`, but SBCL inlines a local function only when it's called once
or declared `inline`. Declaring small known procedures inline in the
`labels` (within a size budget) would get most of `cp0`'s benefit
without a pass of our own.

```scheme
(define (square x) (* x x))
(define (dist2 x y) (+ (square x) (square y)))
```

`square` is a `labels` function, called twice from `dist2`, so it isn't
inlined; declared `inline`, it would be.

**Small `labels` functions inline (done, October 2026).** In each
`labels` the translator writes, the functions of at most 40 conses that
can't reach themselves through the others are declared `inline`
(`inline-small-labels`, src/psyntax.lisp). On 16 of the r7rs-benchmarks,
best of two runs each way, it's 7% faster as a geometric mean: `simplex`
1.83 → 1.10 s, `earley` 1.26 → 0.98, `mazefun` and `puzzle` 18%. No
benchmark was consistently slower; `paraffins` and `conform` vary as
much from run to run as between the two.

### 7. Boolean results (partly)

`truep` is now an inline `eq` test, so a test whose value is a Scheme
boolean costs a comparison. It and `true?` compare with `#f` as a
constant of the code that uses them (`load-time-value`), not by reading
the special variable `qs:false`, which on arm64 cost two dependent loads
and a check that it's bound on every test (October 2026): `puzzle` went
from 71 to 45 billion instructions, `triangl` from 37 to 27, `browse`
5% fewer, `nqueens` and `matrix` 3-4%. A procedure known to return a CL boolean,
`null`, `consp` or one that only ever returns `#t`/`#f`, could be used
as a Lisp test directly. The gain is small since the test went inline;
it matters in predicates called in tight loops.

### 8. Faster compilation (not started)

A different direction: importing a library spends about 90% of its time
in SBCL's compiler. At level 0, or for code run once (a program's top
level, a REPL form), the translation could be evaluated by SBCL's
interpreter (`sb-eval`, or `sb-fasteval` where the SBCL was built with
it), or compiled with `(compilation-speed 3)`. The library cache
(done) already covers libraries loaded more than once.

### 9. Continuation frames for Lisp code (partly)

Already per form: calls that can't capture aren't sites (done).

**Across top-level forms in Guile mode (done, October 2026).**
- A top-level definition whose lambda can't capture is wrapped in
  `%note-safe`, which records the procedure in `qsx::*safe-functions*`
  when the definition runs, including when it's loaded from a cached
  fasl.
- `qsx::*safe-operator-hook*` lets a language mode say when an operator
  form the transformation can't see into is safe. Guile mode's says so
  for `(%guile-ref site)` in two cases:
  - the site's variable holds a recorded procedure, in a declarative
    module (bound once, as Guile's own compiler assumes);
  - it holds a primitive that calls no procedure: a host primitive the
    transformation knows doesn't, or one of Guile mode's whose
    parameters include none named as procedures (`proc`, `thunk`,
    `less`, ...).
- A call of such an operator isn't a capture site, and the operator
  counts as safe when passed to `map` and the like.
- The effect: Guix's package definitions call record constructors from
  other modules. Before, each of those calls was a site, and the
  transformation made a package's code about ten times bigger: `hello`
  went from 808 conses of core to 6,527 transformed.

**Across libraries in the psyntax modes (R6RS, R7RS, Chez; done,
October 2026).**
- A library's exports are locations psyntax makes for it: gensyms,
  assigned once by its invoke code, and immutable (R6RS requires it). The
  library cache marks them as it installs the library
  (`mark-library-locations`, src/continuations.lisp).
- When the invoke code assigns one a procedure that can't capture,
  `find-safe-locations` marks the location safe, and the cache writes
  that into the library's fasl, so it holds when the library is loaded
  from the cache in a later session.
- An importer's call of a safe location isn't a capture site. A loop in a
  program calling a one-line procedure from a library, 30 million times,
  went from 0.29 s to 0.07 s.
- A REPL's or a script's top-level variables are left out: they can be
  defined again.

**At psyntax's top level (Chez mode, R5RS and the earlier reports, the
REPL; done, October 2026).**
- There a definition's variable is a location that can be defined
  again, so `(define (fib n) ... (fib (- n 1)) ...)` calls whatever the
  location holds, and each such call was a site: Chez mode's `fib` ran
  at 3x R7RS mode's time.
- psyntax evaluates the defined lambda by itself (`eval-core`) before
  it assigns the location, so the transformation doesn't see the
  definition. While `eval-top-level` runs, a lambda that calls exactly
  one location with no value yet (a new definition's own, unless it
  calls a procedure defined later) is made W, F and M as Guile mode's
  definitions are (`guard-top-level-self-calls`, src/continuations.lisp).
  W reads the location only if it has a value: if the guess was wrong,
  W never finds the location holding it, and runs M, the lambda as
  written.
- The same corner case as in Guile mode: a continuation captured in a
  procedure the location was changed to during a call of F, by F's own
  extent, can't be re-entered after that call returns (an error says
  so).
- Chez mode's `fib` went from 209 to 69 billion instructions (35 to 13
  billion cycles), `tak` from 92 to 40; R5RS mode's the same.

What remains:
- procedures passed into the runtime's higher-order primitives that
  aren't open-coded (`sort`, `hashtable-walk`), which use the
  frame-aware versions whenever the procedure passed might capture.

## Plan (2026-10-18): a Chez-style optimizer between psyntax and the translator

Chez runs cp0, cpletrec and cptypes on its core forms; we hand psyntax's
core forms straight to the translator and rely on SBCL, which can't see
through our generic calls. The user approved this list, in this order:

1. **cp0: inlining, constant folding, copy propagation, dead code**
   (idea 6, extended) on core forms, so every mode gains: R7RS and Chez
   libraries, Racket linklets after lowering, Guile's Tree-IL. Library
   definitions are immutable, so their small procedures can be inlined.
   For Guile, running Guile's own `peval` on Tree-IL first is a cheap
   step. *First pass done 2026-10-18* (src/cp0.lisp): it runs on
   every core form, whatever made it; `QUASISCHEME_OPTIMIZE=0` turns
   it off, `1` runs cp0 alone, `2` (the default) adds cptypes, and
   `QUASISCHEME_OPTIMIZE_TRACE=1` prints each form before and after.
   The library cache's signature records a level other than 2.
2. **cptypes: type recovery** (idea 4): facts from predicate tests and
   result types (`(pair? x)` → `car` unchecked; fixnum loop counters;
   `fl` results are flonums), emitted as `truly-the`, typed bindings or
   specialized primitives. *First pass done 2026-10-18, with 1*
   (src/cptypes.lisp).
3. **Unboxed flonums**: flonum-typed loop variables declared
   `double-float`, so SBCL keeps them in registers (fibfp, mbrot,
   nucleic, fft, ray box every intermediate).
4. **Closed world**: in a `--compile` executable, and inside a library
   body, top-level definitions are constant, so calls between them are
   direct `labels` calls without capture frames; "may capture"
   summaries across the program or library extend the self-call guard
   (idea 9; src/continuations.lisp) to every known call. Outside those,
   the same is the opt-in "declarative top level" (pending: a flag, an
   environment variable or an importable declaration).
5. **Escape analysis** (idea 5): `dynamic-extent` for closures and rest
   lists that don't escape (a lambda passed to a known `for-each`,
   `map`, `vector-for-each`; `apply`'s lists); closures with no free
   variables lifted, so making one allocates nothing. *First pass done
   2026-10-19* (src/cpdx.lisp; idea 5 above): closures passed to
   primitives, and rest lists only read. SBCL already makes a lambda
   with no free variables once, so lifting isn't needed.
6. **cpletrec**: `letrec`, named `let` and internal defines of lambdas
   as `labels` everywhere, without letrec checks when the inits are
   lambdas (Racket's linklet compiler does this already).
7. **Representation**: records as one object (idea 2); boxes only for
   assigned variables a closure captures.

Where: 1, 2, 3, 5 and 6 are one new pass on core forms (after psyntax,
before the translator); 4 is src/continuations.lisp; 7 is the record
runtime (src/r6rs/). Each must keep every mode's conformance suites
passing.

## Interpreting instead of compiling (October 2026)

Much of what a mode evaluates runs once: a module's top-level forms, a
linklet's body, a REPL's input. Compiling it with SBCL costs more than
running it. `QUASISCHEME_EVALUATOR` (src/psyntax.lisp, `*EVALUATOR*`)
chooses, for every mode's top-level forms:

- `compile` (the default): SBCL compiles every form, as before (Guile
  mode still interprets forms that make no procedure, `trivial-core-p`);
- `auto`: a translated form that makes no closure (no `lambda`,
  `function`, `flet`, `labels`) is interpreted;
- `interpret`: every form is interpreted, and Racket mode doesn't
  compile linklets into its cache (it still loads the ones there).

SBCL's interpreter is sb-eval, or sb-fasteval in an SBCL built with
`sh make.sh --fancy --with-sb-fasteval --without-sb-eval` (Homebrew's
isn't); `:interpret` is whichever the image has, so nothing else
changes. Measured 2026-10-19 on SBCL 2.6.9, stock and built with
sb-fasteval, each building its own `bin/quasischeme`:

| run | stock (sb-eval) | sb-fasteval |
|---|---|---|
| Guile, five library modules, `QUASISCHEME_GUILE_CACHE=0`: compile | 2.0 s | 1.8 s |
| ... `auto` | 2.1 s | 1.8 s |
| ... `interpret` | 0.75 s | 0.78 s |
| Racket, `#lang racket/base` hello, cold cache: compile | 71 s | 70 s |
| ... `interpret` | over 900 s (stopped) | 212 s |
| Racket or Guile hello, warm cache, any setting | 2 s / under 1 s | the same |

So: interpreting pays where code runs once and little of it is called
afterwards (loading a Guile module without the cache, 2.7x faster with
either interpreter); it doesn't where the code is the hot path, as
Racket's expander is during a cold start: compiled, it expands
`racket/base` in 70 s; under sb-fasteval, which is about 4x faster than
sb-eval here, it takes 212 s. Interpreted procedures stay interpreted,
so `interpret` is for short scripts and one-off loads, not programs
that compute. `auto` gains nothing measurable in these runs: Guile mode
already interprets its run-once forms, and Racket's and the psyntax
modes' cold time is in compiling procedures, which `auto` compiles too.

## Summary

| # | idea | status | effect where it applies |
|---|---|---|---|
| 1 | an optimization level | not started | 8% at level 2; more at 3 |
| 2 | records as one object | not started | about halves record allocation and access |
| 3 | cross-library knowledge | partly (per form) | inlining and fewer capture sites across libraries |
| 4 | type inference into declarations | partly (cptypes, first pass) | fixnum and unboxed-flonum loops |
| 5 | `dynamic-extent` | partly (cpdx: closures passed to primitives, rest lists only read) | rest lists and non-escaping closures |
| 6 | inlining small known procedures | partly (small `labels` functions; cp0, first pass) | calls to small helpers |
| 7 | boolean results | partly | small |
| 8 | faster compilation | not started | start-up and the REPL |
| 9 | continuation frames | partly (across forms in Guile mode, across libraries, self-calls at psyntax's top level) | the runtime's higher-order primitives |

Ideas 1 and 2 are the cheapest and most certain; 3 and 4 are the
largest and would be the core of a later major version.

## Tried and not kept (October 2026)

Measured by instructions retired and cycles (`/usr/bin/time -l` on
macOS), which vary by well under 1% from run to run where wall-clock
times on a loaded machine varied by 2x.
- **One frame per call of a machine**, filled in by each site, in place
  of a stack vector per site: SBCL's sampling profiler charged
  `allocate-vector-on-stack` with a tenth of `lattice`, but the shared
  frame's stores carry GC card marks, and in a microbenchmark a site
  cost the same either way; `lattice` took 13% more instructions.
- **`(debug 0)`**, or `(speed 2) (debug 0)`, for the translated code:
  under 3% on `fib`, `tak` and `lattice` (full-continuation code already
  has `insert-debug-catch` off), and backtraces lose frames.
- **A smaller nursery** (`bytes-consed-between-gcs`, 429 MB with the
  command line's 8 GB heap): 32-128 MB made `lattice` slower, by 3-75%
  in cycles; the collections cost more than the cache misses saved.
- **A site's body inline** when the binding stack isn't deep, rather than
  in a `dynamic-extent` closure: about 0.5 ns of a site's 3-6 ns.
