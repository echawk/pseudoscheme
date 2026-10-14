# Optimization: what's done, what could be, and how it could be chosen

Pseudoscheme's output is meant to stay readable: a Scheme procedure
becomes a Lisp function a person could have written, and SBCL does the
optimizing. This document lists what the translator already does for
speed and the ideas that remain, with each one's status and an example
of where it would help. Most of the ideas are for a later major version.
The aim throughout is to make the output faster by *adding*
declarations and knowledge, not by restructuring the code into
something unrecognizable.

Where speed stands: ecraven's r7rs-benchmarks (bench/) run at 1.37×
Chez Scheme's time as a geometric mean, with full continuations, and at
1.30× escape-only (bench/RESULTS.md, ROADMAP section 2).

## What the translator does now

| technique | where | what it does |
|---|---|---|
| definitions as `letrec` | `definitions-as-letrec`, src/psyntax.lisp | A body's definitions (a program's or library's too) bound once to lambdas become one `labels`, so calls between them are direct calls, not `funcall`s through boxed variables. |
| hoisting big bodies | `hoist-definitions`, src/psyntax.lisp | A top-level form too big for SBCL as one code object has its definitions hoisted into top-level ones, compiled one by one. |
| inline boolean tests | `truep`, src/core.lisp | A Scheme `if` on a value that isn't already a Lisp boolean is an inline `eq` test, not a call. |
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
           (if (ps:scheme< n '2)
               n
               (ps:scheme+ (fib (ps:scheme- n '1))
                           (fib (ps:scheme- n '2))))))
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
       (if (ps:scheme= i (length (the simple-vector v)))
           (return acc)
           (progn
             (psetq i (ps:scheme+ i '1)
                    acc (ps:scheme+ acc (svref v i)))
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
| 2 | `(speed 2) (debug 0)` | safe, fewer frames in backtraces (about 8% on the call-heavy benchmarks, ROADMAP section 2) |
| 3 | `(speed 3) (safety 0)` | unsafe, as Chez's `optimize-level 3`: a wrong type is undefined behavior, not an error |

How it could be chosen:
- on the command line, `--optimize-level=N`;
- per library or program, by a declaration form in a library of our
  own, `(import (pseudoscheme optimize))` then `(declare-optimize-level
  3)`, in the body it applies to;
- by each mode's own mechanism, which the modes already accept and
  ignore: Chez's `optimize-level` parameter and `(optimize-level 3)`
  in `compile-file` (src/chez/top-level.scm has the parameter, with no
  effect), and Guile's `-O0`…`-O3` to `guild compile` and
  `%auto-compilation-options`;
- from Lisp, a special variable, as `psx::*full-continuations*` is
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

### 5. `dynamic-extent` (not started)

Rest-argument lists that don't escape, and closures passed to
`for-each`, `map`, `vector-for-each` and the like that the procedure
can't keep (the continuation analysis already knows which primitives
these are), could be declared `dynamic-extent` and allocated on the
stack.

```scheme
(define (sum . xs) (apply + xs))
(define (print-all l port) (for-each (lambda (x) (write x port)) l))
```

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

### 7. Boolean results (partly)

`truep` is now an inline `eq` test, so a test whose value is a Scheme
boolean costs a comparison. A procedure known to return a CL boolean,
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
  `%note-safe`, which records the procedure in `psx::*safe-functions*`
  when the definition runs, including when it's loaded from a cached
  fasl.
- `psx::*safe-operator-hook*` lets a language mode say when an operator
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

What remains:
- procedures passed into the runtime's higher-order primitives that
  aren't open-coded (`sort`, `hashtable-walk`), which use the
  frame-aware versions whenever the procedure passed might capture.

## Summary

| # | idea | status | effect where it applies |
|---|---|---|---|
| 1 | an optimization level | not started | 8% at level 2; more at 3 |
| 2 | records as one object | not started | about halves record allocation and access |
| 3 | cross-library knowledge | partly (per form) | inlining and fewer capture sites across libraries |
| 4 | type inference into declarations | not started | fixnum and unboxed-flonum loops |
| 5 | `dynamic-extent` | not started | rest lists and non-escaping closures |
| 6 | inlining small known procedures | partly (SBCL's own) | calls to small helpers |
| 7 | boolean results | partly | small |
| 8 | faster compilation | not started | start-up and the REPL |
| 9 | continuation frames | partly (across forms in Guile mode, across libraries) | the runtime's higher-order primitives |

Ideas 1 and 2 are the cheapest and most certain; 3 and 4 are the
largest and would be the core of a later major version.
