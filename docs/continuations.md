# Continuations

Continuations are **full and re-entrant** by default, for R5RS, R6RS and
R7RS alike. They come from *generalized stack inspection*: compiled code
stays ordinary direct-style Lisp, and a procedure that might be
captured in the middle of a call records, on the stack, what it would
need to resume. `--continuations=escape` on the command line (or
`psx::*full-continuations*` false from Lisp) compiles escape-only
continuations instead. `(cond-expand (full-continuations ...))` tells
which a program is compiled with.

The implementation is src/continuations.lisp. Its tests are
tests/run-continuation-tests.lisp (37 of 37), and the standards' suites
pass in full only with it: R5RS 189 of 189, R7RS 978 of 978, R6RS all
8902 (escape-only: 188, 977 and 8900).

Cost, measured on bench/ (ecraven's r7rs-benchmarks, October 2026): full
continuations take **5.0% more time than escape-only** as a geometric
mean of the 57 benchmarks (1.37× Chez's time against 1.30×). 39 of the
57 run within 5% of escape-only. The cost is in closure-heavy programs
(`lattice` 1.76×, `matrix` 1.75×, `conform` 1.70×, `quicksort` 1.66×).

This document shows what you get, how a program is transformed (with
the transformation's real output), what makes it fast, what the runtime
does for Lisp code that calls Scheme, what is built on it, and what is
left.

## What you get

Backtracking, by re-entering continuations (with
`(import (scheme base) (scheme write))`):

```scheme
(define choice-points '())
(define (fail)
  (if (null? choice-points)
      (error "no more choices")
      (let ((back (car choice-points)))
        (set! choice-points (cdr choice-points))
        (back #f))))
(define (amb choices)
  (call/cc
   (lambda (return)
     (for-each (lambda (choice)
                 (call/cc (lambda (next)
                            (set! choice-points (cons next choice-points))
                            (return choice))))
               choices)
     (fail))))
(define (require ok) (unless ok (fail)))

(let* ((a (amb '(1 2 3 4 5 6 7 8 9 10 11 12 13)))
       (b (amb '(1 2 3 4 5 6 7 8 9 10 11 12 13)))
       (c (amb '(1 2 3 4 5 6 7 8 9 10 11 12 13))))
  (require (< a b))
  (require (= (* c c) (+ (* a a) (* b b))))
  (write (list a b c)))
```

prints `(3 4 5)`. With `--continuations=escape` it stops with
`continuation invoked after its extent ended (continuations are
escape-only)`. Each `next` is captured inside `for-each`, a loop written
in Lisp, and re-entered after `amb` has returned. That is the hard case,
and section "Frames for Lisp code" shows how it works.

`dynamic-wind` re-entry runs the before thunk again:

```scheme
(define k #f)
(define trace '())
(define (note x) (set! trace (cons x trace)))
(dynamic-wind
  (lambda () (note 'in))
  (lambda () (call/cc (lambda (c) (set! k c))) (note 'body))
  (lambda () (note 'out)))
(when (< (length trace) 6) (k 'again))
(write (reverse trace))          ; (in body out in body out)
```

Generators made with `call/cc` (SRFI 158's `make-coroutine-generator`
is one) re-enter the producer at each call. R6RS's `guard` re-raises in
the dynamic environment of the `raise` when no clause matches, by
re-entering the handler's continuation.

## How it works, by example

With full continuations on, every core form psyntax produces goes
through `cc-transform` before the translator (`host-eval` in
src/psyntax.lisp; the compiled-library cache does the same). The output
below is real: core Scheme from psyntax, the same after `cc-transform`,
then the Lisp the translator makes. Gensyms are shortened to `v1`, `v2`
and so on.

### 1. Most code is untouched

```scheme
(letrec ((fib (lambda (n)
                (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
  fib)
```

`cc-transform` returns its input unchanged, and the translator makes
what one would write by hand:

```lisp
(labels ((v1 (v3)
           (if (ps:scheme< v3 '2)
               v3
               (ps:scheme+ (v1 (ps:scheme- v3 '1))
                           (v1 (ps:scheme- v3 '2))))))
  #'v1)
```

The non-tail calls `(fib (- n 1))` can't reach `call/cc`: `fib` calls
only itself and primitives that call no procedure. The analysis in
"What makes it fast" finds that, so `fib` runs at the same speed in both
modes.

### 2. A call that might capture: a state machine

`f` is a parameter, so `(f (car l))` could call anything, including
something that captures:

```scheme
(lambda (f l)
  (let loop ((l l))
    (if (null? l) '() (cons (f (car l)) (loop (cdr l))))))
```

`loop` becomes a *machine*: one function whose body is a flat
`tagbody`, with a label after each call that may capture (a *site*):

```scheme
(lambda (v5 v6)
  (letrec ((v16                                   ; loop's machine
            (lambda (v17 v18 v19 v12)             ; entry, value, frame, l
              ((lambda (v14 v15)                  ; the hoisted locals
                 (%machine v17 '((2 . %l5) (1 . %l4))
                   (%ignorable v18 v19)
                   (%go '%entry)
                   '%l4                           ; resuming site 1:
                   (set! v12 (%frame-ref v19 4))  ;   restore l from the frame
                   (set! v14 v18)                 ;   the call's value
                   (%go '%l2)
                   '%l5                           ; resuming site 2
                   (set! v14 (%frame-ref v19 4))
                   (set! v15 v18)
                   (%go '%l3)
                   '%entry
                   (if (null? v12) '#f (%go '%l1))
                   (%return '())
                   '%l1                           ; site 1: (f (car l))
                   (set! v14 (%site (vector v16 '() 1 1 v12)
                                    (v5 (car v12))))
                   '%l2                           ; site 2: (loop (cdr l))
                   (set! v15 (%site (vector v16 '() 2 1 v14)
                                    (v10 (cdr v12))))
                   '%l3
                   (%return (cons v14 v15))))
               '#f '#f)))
           (v10 (lambda (v12) (v16 0 '#f '#f v12))))   ; loop itself
    (v10 v6)))
```

- `loop` (`v10`) is a small function that calls the machine with entry
  point 0. Normal calls start at `%entry` and run straight through.
- Each **site** is `(%site frame call)`. The frame is a vector:
  `#(machine promoted site-number parameter-count live-variable ...)`.
  At site 1 the only variable still needed after the call is `l`, and
  at site 2 it is the value of site 1. `%site` pushes the frame onto
  `*fstack*` (a special variable) for the extent of the call:

  ```lisp
  (defmacro %site (frame call)
    `(let* ((f ,frame) (cell (cons f *fstack*)))
       (declare (dynamic-extent f cell))
       (let ((*fstack* cell))
         ,call)))
  ```

  Both the vector and the cons are stack-allocated (`dynamic-extent`).
  The normal path conses nothing on the heap and makes no closures.
- **Resuming** calls the machine with the site number, the value the
  call returned, and the frame. `%machine`'s `case` jumps to that
  site's resume label (`%l4` for site 1), which restores the live
  variables from the frame and goes to the label after the call
  (`%l2`). From there the procedure continues as if the call had just
  returned.
- `'()` and `'#f` print as `common-lisp:nil` and `ps:false` in the real
  output; they are written the Scheme way above.

The translator turns the machine into one Lisp function: `labels` for
`v16`, `let` for the hoisted locals, and `%machine` expands to a
`block` around a `tagbody`.

### 3. `call/cc` used as an escape: a `catch`

```scheme
(lambda (l)
  (call/cc (lambda (k)
             (for-each (lambda (x) (if (negative? x) (k x))) l)
             #f)))
```

`k` is only ever called while the `call/cc` is still running: the lambda
that calls it is a procedure argument of `for-each`, which can't keep
it. So nothing needs capturing, and the transformation uses
`%escape-call/cc`, which is a Lisp `catch`:

```scheme
(lambda (v20)
  (%escape-call/cc
   (letrec ((v26 (lambda (v27 v28 v29 v22)
                   (%machine v27 '((1 . %l2))
                     ...
                     '%entry
                     (%site (vector v26 '() 1 1)
                            (%full-for-each (lambda (v24)
                                              (if (negative? v24) (v22 v24) ...))
                                            v20))
                     '%l1
                     (%return '#f)))))
     (lambda (v22) (v26 0 '#f '#f v22)))))
```

```lisp
;; simplified: the real one also answers a query for its identity
(defun escape-call/cc (f)
  (let* ((tag (heap-cons 'continuation nil))
         (frame (vector :escape nil tag)))
    (declare (dynamic-extent frame))
    (catch tag
      (with-frame (frame)
        (funcall f (lambda (&rest values) (throw tag (values-list values))))))))
```

The `for-each` call is still a site, because the lambda passed to it
calls `k`, which isn't a known-safe procedure. That lets a continuation
captured somewhere inside it still be re-entered. The `:escape` frame
re-establishes the `catch` if that happens, so `k` keeps working.
`ctak` and `fibc` in bench/ are made of such escapes.

### 4. A real capture, and boxing

```scheme
(lambda (f)
  (let ((saved #f))
    (+ 1 (call/cc (lambda (k) (set! saved k) (f k) 0)))))
```

Here `k` is stored and passed to an unknown procedure, so this is a real
`%full-call/cc`, at a site:

```scheme
(set! v33 '#f)
(set! v33 (list v33))                        ; saved, boxed
(set! v37 (%site (vector v42 '() 1 1)
                 (%full-call/cc
                   ... (rplaca v33 v35)      ; (set! saved k)
                       (%site (vector v38 '() 1 1) (v30 v35))   ; (f k)
                       ...)))
'%l1
(%return (+ '1 v37))
```

`saved` is assigned and is live across a site. A resumed frame would
otherwise hold a copy of its old value, so it is **boxed**: a cons that
the frame and everything else share, as Scheme's semantics require.
Variables that are never assigned, or are assigned only where no site
can run between the binding and the assignment (psyntax's `letrec*` for
internal definitions), stay unboxed.

### Capture and re-entry

`full-call/cc` (src/continuations.lisp) captures by copying the frames
on `*fstack*` from the innermost to the base:

```lisp
;; simplified (shared winders, the identity query)
(defun full-call/cc (f)
  (multiple-value-bind (frames rebuildable) (capture-frames)
    (let ((winders *winders*) (live (list t)) (tag (list 'continuation)))
      (flet ((k (&rest values)
               (cond ((car live) (throw tag (values-list values)))        ; still running: escape
                     ((and rebuildable *base-tag*)                         ; re-entry
                      (throw *base-tag* (lambda () (reenter frames winders values ...))))
                     (t (error 'continuation-not-reentrant)))))
        (unwind-protect (catch tag (funcall f #'k))
          (setf (car live) nil))))))
```

- **Capture** copies only frames that haven't been copied before. A
  frame's `promoted` slot remembers its heap copy, together with the
  copies of every frame outside it. What's outside a frame can't change
  while the frame is on the stack, so the next capture copies only the
  frames pushed since and shares the rest. Repeated captures, as in
  generators and `ctak`, pay only for the new frames.
- **Invoking `k` while its `call/cc` is still running** is a `throw`,
  as in escape-only mode.
- **Re-entry** throws to the **base**, a `catch` around each top-level
  evaluation (`call-with-continuation-base`), with a thunk. The base
  runs the thunk, which:
  - runs the before thunks of the `dynamic-wind`s the target
    continuation has and the current one doesn't (only the unshared
    ones; the throw already ran the unshared afters);
  - rebuilds the frames, outermost first. Rebuilding a frame pushes it
    again, computes what its call returns (by rebuilding the frames
    inside it), and calls its machine with the site number and that
    value. The machine restores its live variables and jumps past the
    call.

## What makes it fast

**The normal path does almost nothing.** A site is a stack-allocated
vector and cons, and one special binding of `*fstack*`. Measured on an
Apple M4, in loops of 10⁸ calls:

| | full | escape-only |
|---|---|---|
| a non-tail call of an unknown procedure (a site) | 5.4 ns | 3.0 ns |
| a non-tail call of a known procedure (not a site) | 1.1–1.3 ns | 1.3 ns |
| `call/cc` used as an escape | 21 ns | 15–18 ns |
| a generator's yield: capture and re-entry, twice | ~200 ns | (impossible) |

So a site costs about 2.4 ns, and most calls aren't sites.

**Most calls aren't sites.** Within a top-level form (a program's or a
library's body is one, its definitions a `letrec`), the transformation
finds the calls that can't reach `call/cc`:

- **Safe procedures** (`find-safe-procedures`). A variable bound once
  and for all to a lambda is *known*. A known procedure is *safe* if
  nothing it calls, in any position, can capture: only primitives that
  call no procedure, and other safe procedures. The analysis assumes
  every known procedure is safe and strikes out those that call
  something unsafe, until nothing changes. That gives the largest
  consistent set, which is correct: a cycle of procedures that call
  nothing else never reaches `call/cc`. Calls to safe procedures aren't
  sites, and a procedure that calls only safe procedures needs no
  machine (example 1).
- **Safe calls of calling primitives.** `map`, `for-each`, `apply`, the
  folds, sorts and searches call only the procedures passed to them. A
  call that passes only safe procedures (safe variables, primitives,
  lambdas with safe bodies) can't capture, isn't a site, and calls the
  plain primitive rather than its frame-aware version.
  `call-with-values` needs the frame-aware version only if its producer
  may capture.
- **Escape-only `call/cc`** (`escape-call/cc-p`): when `k` is only
  called, or passed to a parameter of a known procedure that does the
  same, or called from a lambda that can't outlive the call, it is a
  `catch` (example 3).
- **Raising an error** isn't a site: the handler of a non-continuable
  `raise` can't return to the raiser. (`raise-continuable` is a site.)
- **Tail calls** aren't sites. A procedure whose only possibly-capturing
  calls are tail calls needs no machine.

**SBCL had to be kept compiling it.** These problems came up while
making the R6RS test libraries and the larger benchmarks compile:

- The first prototype used CL's condition system, as the paper's .NET
  implementation used exceptions: a `handler-bind` around each site, and
  two closures per site (the call and the rest of the procedure). It
  worked, but SBCL's compile time is superlinear in closures that share
  variables. A function with 1000 closures over one variable took 162 s
  to compile, and 2000 exhausted a 4 GB heap. The state machines
  replaced the closures.
- A 3,294-site test procedure became one function with 1,789 hoisted
  locals, which SBCL's analyses couldn't handle either. Long sequences
  are cut into chunks of about 32 sites (`*chunk-sites*`,
  `chunk-sequences`), each a procedure of its own, closure-converted and
  compiled as its own component (`%lifted`, a `load-time-value`
  lambda).
- With `debug` ≥ 1 and ≥ `speed`, SBCL keeps each function's binding
  stack pointer live across the whole component (`insert-debug-catch`),
  so the register allocator's tables grow as functions × blocks.
  Full-mode code is compiled with `(sb-c::insert-debug-catch 0)`
  (`*full-policy*`).
- A resumed site's live variables are restored with `%frame-ref`, a
  `notinline` `svref`. With an inline `svref`, compile time grew
  exponentially with the number of sites: a body of 20 calls took 16 s.
- A big program (bench/'s `compiler`, 11,000 lines) exhausted an 8 GB
  heap as one component. A top-level form bigger than
  `psx::*letrec-definitions-limit*` has its definitions hoisted into
  top-level definitions (`hoist-definitions`, src/psyntax.lisp),
  compiled one by one.

With these, the R6RS `base` test library compiled in 0.84 s when it was
measured, against 1.05 s without the transformation, since chunking
also helps SBCL with long sequences.

## Frames for Lisp code

Lisp code that calls Scheme procedures pushes frames of its own, so
that a continuation captured inside can be re-entered through it. Each
frame is a vector whose first element says what it is:

| frame | pushed by | rebuilding it |
|---|---|---|
| `#(:winder promoted (before . after))` | `dynamic-wind` | re-establishes the extent (the before already ran) |
| `#(:handler promoted (handler . outer))` | `with-exception-handler` | reinstalls the handler |
| `#(:handlers promoted handlers)`, `#(:k promoted k)` | a handler that `raise` calls | the outer handlers, and what `raise` does when the handler returns |
| `#(:extent promoted establish)` | `parameterize` | calls ESTABLISH again around the rest |
| `#(:resume promoted function state ...)` | `map`, `for-each`, `vector-map`, `vector-for-each`, `string-map`, `string-for-each`, the forms of a top-level `begin` | `(function value state ...)` goes on with the loop |
| `#(:escape promoted tag)` | an escape-only `call/cc` | re-establishes the `catch` |
| `#(:k promoted k)` | `call-with-values` | calls K with the values |
| `#(:barrier promoted name)` | other primitives that call procedures (the sorts, R6RS's folds and searches, `force`, `call-with-port`, the string-port procedures) and the Lisp bridge | an error: re-entering through it would resume as if the primitive had returned at once |

`map`'s loop shows the pattern. Each call of `f` is made in a frame
holding the loop's state, and rebuilding the frame calls
`map1-continue`, which goes on from that element:

```lisp
(defun map1-loop (f list acc captured)
  (loop
    (when (atom list) (return (if captured (reverse acc) (nreverse acc))))
    (let ((x (car list)) (promoted nil))
      (push (let ((frame (vector :resume nil 'map1-continue f list acc)))
              (declare (dynamic-extent frame))
              (multiple-value-prog1 (with-frame (frame) (funcall f x))
                (setq promoted (svref frame 1))))
            acc)
      (when promoted (setq captured t)))
    (setq list (cdr list))))

(defun map1-continue (value f list acc)
  (map1-loop f (cdr list) (cons value acc) t))
```

`map` reverses its result destructively only if nothing was captured
during the loop. Re-entering a captured continuation must not change a
list an earlier return already returned (R7RS 6.10).

Re-entry runs the afters and befores only of the `dynamic-wind`s that
the current and target continuations don't share. `eval` and `load`
called from a running program don't start a base of their own, so
continuations captured in the evaluated code include the frames outside
it.

## Built on it

- **SRFI 158**: `make-coroutine-generator`, and so
  `make-for-each-generator`, is the sample implementation's, re-entering
  the producer at each call. With `--continuations=escape` it runs the
  producer to completion on the first call and buffers the values.
- **SRFI 226** (control features: prompts, composable continuations,
  continuation marks, its own threads): Marc Nieper-Wißkirchen's sample
  implementation, written on `call/cc` and `dynamic-wind`. It needs to
  tell when two continuations are the same (a call in tail position):
  a continuation answers a private query with its identity
  (`continuation=` in src/continuations.lisp). Its procedures replace
  the standard `call/cc`, `dynamic-wind` and `parameterize`; code that
  uses the standard ones doesn't see its prompts or marks.
- **SRFI 248** (minimal delimited continuations): written on full
  continuations here (src/srfi/248.sld).
- **R6RS `guard`** re-raises in the dynamic environment of the `raise`
  (`%guard-reraise`). Escape-only, it re-raises from the guard's own
  context instead, which is one of the two R6RS tests that mode fails
  (the other re-enters a `dynamic-wind`).

## Limits

- **Re-entering through Lisp code is an error**, not a silent wrong
  answer. A barrier frame stands for any Lisp code between the capture
  and the base: a sort's comparison procedure, R6RS's `fold-left`, and
  any Scheme procedure handed to Lisp through the bridge (`lisp-facing`
  in src/interop.lisp, which covers every procedure passed to a
  `(cl ...)` function). Escaping through such code works.
- **A Scheme procedure called from Lisp outside any base** (a callback
  from Lisp code, not from a running Scheme program): continuations
  captured inside it can escape but not be re-entered. There is no base
  to rebuild the Lisp caller from.
- **Code compiled without the transformation** isn't recorded in
  captured continuations: its calls into transformed code are sites,
  but its own frames aren't there to rebuild. Booting compiles
  psyntax's image and the standard libraries escape-only, but those
  libraries are re-exports and syntax with almost no Scheme procedures
  of their own, and psyntax runs at expansion time. Otherwise it takes
  a program that mixes the modes on purpose: the compiled-library
  cache keeps the two modes' code apart.
- **Threads**: frames are per thread (`*fstack*` is bound per base).
  Invoking a continuation in a thread other than the one that captured
  it is undefined.
- **Rebuilding is O(depth) per re-entry**, and recursive: it nests a
  Lisp call per frame.

## Further work

- **More frame-aware primitives** in place of barriers, as `:resume`
  loops: R6RS's `fold-left`, `fold-right`, `find`, `filter`,
  `partition`, `exists`, `for-all`, `remp`, `memp`, `assp`; the sorts (a
  merge sort can keep its state in frames); `force`; `call-with-port`
  and the string-port procedures (an `:extent`-like frame).
- **Safety across libraries**: calls to procedures imported from
  another library are unknown, and so are sites. Each exported
  procedure's safety could be recorded with the library (the
  compiled-library cache stores its export environment already) and
  trusted for bindings that can't be assigned. That is most of what
  remains in the closure-heavy benchmarks.
- **Calls of procedure parameters** (`(f x)` with `f` a parameter) are
  sites even when every caller passes a safe procedure. Specializing,
  or a flag on safe closures checked at run time, could help.
- **Cheaper frame pushes**: a per-thread frame stack (a vector and a
  fill pointer) instead of a special binding per site; the non-local
  exits inside Scheme (a base, an escape's `catch`, `guard`) would
  restore the pointer.
- **Fewer hoisted locals**: only variables live across a label need to
  be hoisted; others could stay `let`-bound, which avoids SBCL's value
  cells for the ones closures capture.
- **Lazy rebuilding**: rebuild only the innermost few frames, on top of
  an *underflow* frame that rebuilds the next few when they return
  (Hieb, Dybvig and Bruggeman's segmented stacks). That bounds the work
  per re-entry and makes it iterative. Generators, which re-enter the
  same few frames repeatedly, benefit most.
- **Native prompts and continuation marks** on the same frames, rather
  than SRFI 226's library implementation on top of `call/cc`. A prompt
  would be a frame with its tag; a composable continuation would copy
  the frames up to the prompt and be rebuilt on top of the current
  stack, without a throw to the base; a mark would be a slot in a frame.
  Racket (docs/racket.md) and Guile (docs/guile.md) both need this.
- **One dynamic environment**, as Racket and SRFI 226 keep: winders,
  handlers, the parameterization and marks in one value a continuation
  captures. That would make capturing it O(1). `parameterize` still
  assigns each parameter's one global value, which is wrong across
  threads (ROADMAP.md, section 4).

## How we chose

Before implementing anything, there were four candidates:

1. **Whole-program CPS conversion** (Danvy and Filinski's one-pass
   transformation). It's simple, and `call/cc` becomes trivial. But
   every Scheme procedure would take an extra continuation argument, so
   passing a Scheme `lambda` to `sort`, `mapcar` or a CLOS method would
   need an adapter. Every call would be a tail call, needing a
   trampoline, since CL doesn't guarantee tail calls. Published
   CPS-to-host figures are 2–5× on call-heavy code.
2. **Generalized stack inspection** (Pettyjohn, Clements, Marshall,
   Krishnamurthi and Felleisen, ICFP 2005). Code stays direct style, a
   capture records the stack as it unwinds, and re-entry rebuilds it.
3. **One-shot continuations from threads** (Kumar, Bruggeman and
   Dybvig, 1998): enough for generators and coroutines, but not for
   multi-shot uses like `amb`.
4. **Delimited control locally** (CPS only inside a `reset`, as
   cl-cont does): pay-as-you-go, but not `call/cc`.

We chose 2. It keeps Scheme procedures ordinary Lisp functions, which
is what docs/interop.md depends on, and code that never captures pays
little. Its mechanism, frames on a stack, is also where continuation
marks and prompts can go later.

Two things changed from the paper. Frames live on an explicit stack
(`*fstack*`) that is pushed on the way in, rather than recorded by
handlers while unwinding. Procedures became state machines rather than
closures. Both were forced by SBCL's compile times (see "What makes it
fast").

Full continuations started as an opt-in mode. They became the default
once three conditions were met:
- re-entering through Lisp code was an error rather than a wrong answer;
- dynamic state (winders, handlers, parameterizations) was captured and
  re-established;
- ordinary code ran within about 10–15% of escape-only.

They cost 7.7% then, and 5.0% after the October 2026 speed pass. Booting
(psyntax's image, the standard libraries) and rebuilding psyntax still
run escape-only.
