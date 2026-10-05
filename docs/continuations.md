# Full continuations

Status: implemented and opt-in (src/continuations.lisp;
`--continuations=full` on the command line, `psx::*full-continuations*`
from Lisp). "Implementation" describes how it works; "Further work"
lists what is left before it can be the default, with the techniques
to try. The sections before them are the original analysis.

## Escape-only continuations (the default), and why

`call-with-current-continuation` compiles to a Lisp `block` and a closure
that does `return-from` it (`builtin.scm`). That gives **escaping**
continuations, used while the `call/cc` is still on the stack: early
exits, `guard`, generators that never resume. It cannot give
**re-entry**, calling a continuation after its `call/cc` has returned,
which is what coroutines, re-entrant generators, `amb`, and the classic
`dynamic-wind` re-entry tests need.

What escape-only continuations cost in the test suites:

* **R5RS (chibi):** one test, re-entering a `dynamic-wind` through a
  saved continuation (188 of 189; 189 with full continuations).
* **R7RS (chibi):** the same `dynamic-wind` test (976 of 978; 977 with
  full continuations, the other failure being the `sqrt` branch cut).
* **R6RS (Racket's suite):** two tests (8900 of 8902; all with full
  continuations): one in `base` re-enters a `dynamic-wind`, and one in
  `exceptions` has a `guard` re-raise in the dynamic environment of the
  `raise`. R6RS's `guard` re-enters its handler's continuation to do
  that; escape-only, it re-raises from the guard's own context instead
  (`%guard-reraise`, vendor/psyntax/psyntax/expander.ss).

So full continuations matter for correctness at the margins and for a
class of programs (coroutines, backtracking), not for most code.

## The options

### 1. Whole-program CPS conversion

Convert every procedure to take an explicit continuation, after psyntax
and before the translator. The one-pass, linear-time transformation that
produces no administrative redexes is usually credited to Danvy and
Filinski ("Representing Control", 1992), with a first-order variant by
Danvy and Nielsen (2003). Oleg Kiselyov's work is mostly on *delimited*
control, which also comes up below.

Pseudoscheme's architecture makes CPS a clean add-on: psyntax already
reduces everything to `lambda`/`if`/`set!`/`quote`/`begin`/`letrec`/calls,
and the transformation is a pass over exactly that language. `call/cc`
becomes trivial, and `dynamic-wind` becomes a winders list consulted
when a continuation is invoked.

The costs are real, and they mostly fall on interop:

* **Every Lisp→Scheme call needs an adapter.** A CPS-converted procedure
  takes an extra continuation argument, so passing a Scheme `lambda` to
  `sort`, `mapcar` or a CLOS method requires a direct-style wrapper that
  runs it to completion. Data sharing stays intact; procedure sharing
  doesn't.
* **Continuations can't cross Lisp frames.** A continuation captured in
  a callback called from Lisp can't include the Lisp frames above it.
  That's barrier semantics, same as option 2.
* **Proper tail calls need a trampoline.** ANSI CL doesn't guarantee
  tail calls; CPS code makes *every* call a tail call, so without SBCL's
  (optimization-dependent) tail merging the stack grows. A trampoline
  is portable but costs a return and a closure per call.
* **Speed.** Closure allocation per non-tail call plus the trampoline.
  Published numbers for CPS-to-a-host-language put this around 2x-5x on
  call-heavy code.

### 2. Continuations from generalized stack inspection

Pettyjohn, Clements, Marshall, Krishnamurthi and Felleisen
("Continuations from Generalized Stack Inspection", ICFP 2005) show how
to get full continuations on a host that only has exceptions: convert to
A-normal form, wrap each non-tail call in a handler, and on capture
throw an exception that unwinds the stack, with each frame's handler
recording its live variables as it passes. Re-entry rebuilds the frames
from those records.

This fits a CL host well (`handler-case`, or `catch`/`throw`), and the
costs are inverted compared with CPS:

* Normal execution stays **direct style**. Procedures are ordinary Lisp
  functions, so interop is unaffected except that capture stops at Lisp
  frames (barriers again).
* Capture costs time proportional to stack depth; code that never
  captures pays only for the handler around each non-tail call. On SBCL
  that's a dynamic-extent binding, which is cheap but not free.
* The transformation is more involved than CPS (ANF, frame records, and
  reconstruction code for each call site).

### 3. One-shot continuations from threads

Kumar, Bruggeman and Dybvig ("Threads Yield Continuations", 1998): a
one-shot continuation is a suspended thread. That covers generators and
coroutines, which are the common re-entry uses, with no compiler change
at all, using SBCL threads. It doesn't give multi-shot continuations
(`amb`, re-entering the same continuation twice). It's cheap to
prototype as `call/1cc` plus SRFI-158-style generators.

### 4. Delimited control, locally

`shift`/`reset` (or SRFI 226's control operators) implemented by
CPS-converting only the body of a `reset`, as the CL library `cl-cont`
does with macros. Code outside a prompt is untouched. That's attractive
because it's explicit and pay-as-you-go, but it's not `call/cc`.

## Decision

Full continuations come from **generalized stack inspection** (option
2), aiming for as much of Scheme's and Racket's control as we can
support. CPS is no longer the plan, not even as a reference
implementation.

Why it fits Pseudoscheme:

* Code stays direct style, so Scheme procedures remain ordinary Lisp
  functions and docs/interop.md is unaffected. Code that never captures
  a continuation pays only for a handler around each non-tail call.
* The paper's mechanism is continuation marks, and Racket's
  `parameterize`, exception handlers and prompts are built on
  continuation marks too (docs/racket.md). One mechanism serves both.
* CL has what the paper's .NET prototype used: `handler-case` (or
  `catch`/`throw`) to unwind while each frame records itself, and
  closures for the frame records.

How the paper's .NET implementation works (section 4.2). This was the
starting point; "Implementation" describes what Pseudoscheme ended up
doing, which differs in where frames live and how they're recorded.

1. **A-normal form** after psyntax, so every non-tail call's result is
   bound to a variable and each call site has a well-defined set of
   live variables.
2. **A handler around each non-tail call.** On capture, a dedicated
   condition (`save-continuation`) is signalled. Each handler on the way
   out adds a record of its frame (which call site, and the values of
   its live variables) and re-signals. The records are created only
   while unwinding, so they cost nothing until a capture.
3. **The top-level handler** turns the collected records into a
   continuation object and resumes the program with it, so capture
   looks like an ordinary return from `call/cc`.
4. **Re-entry rebuilds the stack** from the oldest record: each frame's
   resume function calls the next more recent one, then continues where
   its call site left off. A frame that is resumed this way must not be
   recorded twice by a later capture, so a restored frame's handler
   links to the already-built records instead (the paper's figure 14).
5. **Tail calls** aren't wrapped, so they stay tail calls (as far as the
   Lisp compiler merges them).

## Implementation

src/continuations.lisp. With full continuations on, every core form
psyntax produces goes through `cc-transform` before the translator. R5RS
runs on psyntax too (`(pseudoscheme r5rs)`), so all three standards get
it.

### Procedures become state machines

A procedure that makes a non-tail call that may capture (a *site*)
becomes one Lisp function whose body is a flat `tagbody`: the procedure
is flattened into statements, its local variables are hoisted to the
top, and there's a label after each site. A call "may capture" unless it
calls a primitive that never calls a procedure (`car`, `+`, `display`);
calls to the primitives that do (`apply`, `vector-map`, `sort`, ...) are
sites like calls to unknown procedures.

A site pushes a frame onto `*fstack*` (a special variable) for the extent
of the call:

```
#(machine promoted site-number parameter-count live-variable ...)
```

The vector and the cons holding it are `dynamic-extent`, so the normal
path allocates nothing on the heap and creates no closures. The live
variables are those assigned before the site and referred to after it
(control only jumps forward within a machine, so that's exact enough).

- **Capture** (`full-call/cc`) copies the frames up to the base. A
  frame's `promoted` slot remembers its heap copy, together with the
  copies of every frame outside it; what's outside a frame can't change
  while it's on the stack, so the next capture copies only the frames
  pushed since and shares the rest.
- **Escape**: a continuation invoked within the extent of its `call/cc`
  throws to a `catch` there, as escape-only continuations do.
- **Re-entry** throws to the base, a `catch` around each top-level
  evaluation, which runs the befores of the continuation's
  `dynamic-wind`s and rebuilds its frames, outermost first. Rebuilding a
  frame pushes it again, computes what its call returns (the frames
  inside it), and calls its machine with the site number and that
  value; the machine restores the live variables and jumps to the label
  after the call. The restoring statements are Scheme, translated with
  the rest, so they use the translator's names for the variables.
- **Boxing.** A variable that is assigned and that a frame may hold
  across an assignment is boxed (a cons), so that a re-entered frame and
  everything else that refers to it share it, as Scheme requires.
  psyntax's letrec* (internal definitions) is exempt when no site can
  run between the binding and the assignment.
- **Tail calls** aren't sites, and a procedure whose capturing calls are
  all tail calls needs no machine.
- **Runtime frames** for Lisp code that calls procedures:
  `#(:k promoted k)` for a Lisp continuation (`call-with-values`),
  `#(:winder promoted (before . after))` for `dynamic-wind`. `map` and
  `for-each` are written in Scheme and compiled with the transformation.

### Keeping SBCL fast

What failed first, and why:

- The first prototype used CL's condition system for the stack
  inspection: a `handler-bind` around each site, two closures per site
  (the call and the rest of the procedure). Capture signalled a
  condition, and each handler recorded its frame and declined, without
  unwinding. It worked, but SBCL's compile time is superlinear in
  closures that share variables: a function with 1000 closures over one
  variable takes 162 s to compile, 2000 exhaust a 4 GB heap. The R6RS
  test libraries, with thousands of calls in one procedure, couldn't be
  compiled.
- The state machines fixed the closures, but a 3,294-site test procedure
  became one function with 1,789 hoisted locals, which SBCL's analyses
  couldn't handle either. Long sequences are now cut into chunks of
  about 32 sites (`chunk-sequences`), each a procedure of its own.
- Still, the library took minutes. SBCL compiles a top-level form,
  closures and all, as one component; with `debug` ≥ 1 and ≥ `speed`,
  each function that binds specials keeps its binding stack pointer in a
  slot live across the whole component (`insert-debug-catch`), so the
  register allocator's tables grow as functions × blocks. Full-mode code
  is compiled with `(sb-c::insert-debug-catch 0)`, and each chunk is
  closure-converted and compiled as a component of its own (`%lifted`,
  a `load-time-value` lambda). The base test library now compiles in
  0.84 s (1.05 s without the transformation).

### Results

- chibi's R5RS suite 189 of 189, R7RS 977 of 978 (the `sqrt` branch-cut
  disagreement is the one left), Racket's R6RS suite all 8902 (two more
  than escape-only), tests/run-continuation-tests.lisp 16 of 16. `make
  test-full` runs them.
- Cost on bench/ against escape-only: ordinary code 1.36× slower as a
  geometric mean of 11 benchmarks (cpstak 0.98×, mazefun 1.17×, fib
  1.74×, earley 2.86×); call/cc-heavy code 2.1–2.3× (ctak, fibc).

## Further work

What is left before full continuations can be the default, and the
techniques to investigate for each.

### Re-entry through Lisp frames

**The problem.** A procedure written in Lisp that calls a Scheme
procedure isn't a frame. Examples: the R7RS and R6RS higher-order
procedures that aren't redefined yet (`vector-map`, `vector-for-each`,
`string-map`, `string-for-each`, `list-sort`, `vector-sort`, R6RS's
`find`, `filter`, `partition`, `fold-left`, `fold-right`, `exists`,
`for-all`, `member`/`assoc` with a predicate, `hashtable-update!`),
`call-with-port` and the file procedures that take a procedure,
the procedures of src/r7rs/base.scm (Scheme,
but compiled at boot without the transformation), and Lisp functions
called through the bridge (docs/interop.md). Escaping through one works.
But a continuation captured inside the callback and re-entered after the
Lisp function has returned resumes as if it had returned at once: the
loop's remaining iterations are lost, silently.

Techniques, roughly in order:

1. **Frame-aware versions written in Scheme**, compiled with the
   transformation, as `map` and `for-each` are (`*full-replacements*`,
   `*full-scheme-definitions*`). Easiest for the list and vector
   procedures. Move them into a library of their own so they're compiled
   once and cached, and check each against its standard's semantics
   (order of calls, what `vector-map` returns when re-entered: R7RS says
   earlier returns must not be mutated, as for `map`).
2. **Frame-aware versions written in Lisp with `:k` frames**: a loop
   written as recursion whose "rest of the loop" is a Lisp closure passed
   to `call-with-frame`. For procedures awkward to write in Scheme, such
   as sorting with a Scheme predicate, or hashtable traversal.
3. **Barrier frames for everything else.** At a site calling a calling
   primitive that has no frame-aware version, push `#(:barrier promoted
   name)`; the bridge's wrapper for Scheme procedures handed to Lisp
   (`lisp-facing`) can push one too. A capture records the barrier, and
   rebuilding one raises a clear error ("continuation passes through
   vector-sort, a Lisp procedure"). That makes the failure loud, which
   is the precondition for making full continuations the default.
4. **Code compiled without the transformation** (libraries loaded
   before full continuations were turned on, the command line's
   precompiled libraries, src/r7rs/base.scm) is a barrier too. Compile
   the standard libraries' Scheme parts in both modes (the library cache
   already keys on the mode), or mark machines' frames so that a capture
   can tell when a Lisp frame lies between two of them.
5. **Detecting a Lisp frame at capture time**, as a debugging aid: each
   frame could record SBCL's control stack pointer, and a capture
   compares consecutive frames with the stack (`sb-di`) to find foreign
   frames between them. SBCL-only.

### Dynamic state

Today a continuation captures the `dynamic-wind` winders and the
exception handlers, and re-entry is coarse.

1. **Shared winders.** Re-entry throws to the base, which unwinds the
   whole Lisp stack and runs the after of every active `dynamic-wind`;
   then the befores of all of the continuation's run. R7RS (and R6RS)
   say only those not shared should: the classic algorithm finds the
   common tail of the current and target winder lists (`eq` conses) and
   runs afters down to it and befores up from it. Here: bind the target's
   winders in a special around the throw, and have `winder-extent`'s
   unwind-protect skip the after of a winder the target shares; then run
   befores only for the target's winders outside the common tail.
2. **Exception handlers** (done). `with-exception-handler` binds
   `*handlers*` and a `handler-bind` (for Lisp errors) around its thunk,
   in a `#(:handler promoted (handler . outer))` frame that rebuilding
   re-establishes, as `:winder` frames re-establish a `dynamic-wind`;
   `raise` calls the handler in a `:handlers` frame (the outer handlers)
   under a `:k` frame (what `raise` does if the handler returns). The R7RS
   layer's `raise-object` and `with-exception-handler` reach these
   through hooks (`*call-handler*`, `*call-with-handler*`), so every
   raise is re-enterable, `error`'s and the R6RS layer's included. R6RS's
   `guard` relies on it: with no clause matching, it re-enters the
   handler to re-raise in the dynamic environment of the `raise`.
3. **Parameterizations.** `parameterize` assigns each parameter's one
   global value for the extent of its body (`parameterize*`), which is
   also wrong across threads (ROADMAP.md, 4). Bind parameters with
   specials, or keep a parameterization (an immutable map) in a special,
   and give `parameterize` a `#(:parameterize promoted bindings)` frame.
4. **One dynamic environment.** Racket and SRFI 226 keep winders,
   handlers, parameterization and continuation marks in one dynamic
   environment that a continuation captures as a value. Capturing it is
   then O(1), and each frame kind above becomes "set this part of the
   dynamic environment for the extent of the call".

### Speed

Where the cost is: a special binding and a stack-allocated vector per
site, the machine's hoisted (and setq'd) locals, boxes for assigned
variables, and SBCL value cells for hoisted locals that closures
capture.

1. **"May capture" analysis.** A call needs a site only if its callee
   can reach `call/cc`, or a procedure the analysis doesn't know. Compute
   a fixpoint over the procedures visible in a top-level form (letrec
   bindings, library-internal definitions): a procedure is capture-free
   if it calls only primitives that don't call procedures and other
   capture-free procedures. Calls to it aren't sites, and a capture-free
   procedure needs no machine. `fib` calling `fib` is the typical case;
   most calls in most code are like it. Across libraries, record each
   exported procedure's flag with the library (the compiled-library
   cache stores its export environment already) and trust it only for
   bindings that can't be assigned (R6RS forbids assigning imports;
   REPL redefinitions can't be trusted). Calling a procedure parameter
   is an unknown call, so higher-order code stays correct.
2. **Cheaper frame pushes.** Replace the special binding per site with
   a per-thread frame stack (a vector and a fill pointer, bound once per
   base): a site stores its frame and increments the pointer, and
   decrements it on return. Non-local exits leave the pointer stale, so
   every place that catches one inside Scheme (a base, an escape's
   `catch`, `guard`) saves and restores it; Lisp code that catches
   errors itself wouldn't, so each entry could also record SBCL's
   control stack pointer and a capture discard entries above the
   current one. Keep the special binding as the portable fallback.
3. **Fewer hoisted locals.** Only variables live across a label need to
   be hoisted; others can stay `let`-bound around the statements that use
   them, which avoids SBCL's value cells for those that closures capture.
4. **No vector for sites with no live variables**: a constant frame
   per (machine, site) would do, if the machine's closure is constant.
5. **Measure with SBCL's statistical profiler** on fib, earley and
   deriv before and after each of these.

### Capture and re-entry

1. **Rebuilding is O(depth) per re-entry**, and recursive (it nests a
   Lisp call per frame). Rebuild lazily instead, as the paper's
   conclusion and Hieb, Dybvig and Bruggeman's segmented stacks
   suggest: rebuild only the innermost few frames on top of an
   *underflow* frame which, when they return, rebuilds the next few.
   That bounds the work per re-entry and makes rebuilding iterative.
   Generators, which re-enter the same few frames repeatedly, benefit
   most.
2. **Escapes** are already a `catch`, and repeated captures share
   frames (the `promoted` slot), so the remaining capture cost is
   copying new frames.

### Delimited control and continuation marks

The frames make both straightforward:

- **SRFI 226** (prompts, composable continuations, `abort`): a prompt
  pushes a `:prompt` frame with its tag; capturing a composable
  continuation copies the frames up to the nearest matching prompt; and
  applying it rebuilds those frames on top of the current stack, without
  throwing to the base. `abort-current-continuation` throws to the
  prompt's `catch`.
- **Continuation marks** (Racket needs them, docs/racket.md): a mark
  slot in each frame. `with-continuation-mark` in tail position sets the
  mark on the frame below (copy-on-write if it's promoted), in non-tail
  position on a frame of its own; `current-continuation-marks` walks
  `*fstack*`.
- **SRFI 158**'s generators can then be real coroutines rather than
  buffered (src/srfi/README.md), and one-shot continuations from
  threads (option 3 above) are no longer needed.

### Interop and threads

- A Scheme procedure called from Lisp outside any base (a callback):
  captures inside it can escape but not be re-entered, and re-entering
  says so. That's by design: there's no base to rebuild the Lisp caller.
- Frames are per thread (`*fstack*` is bound per base). Invoking a
  continuation in a thread other than its own would rebuild its frames
  there, under that thread's dynamic state; leave it undefined.
- Mixing code compiled with and without the transformation: calls into
  untransformed Scheme are barriers (see above).

### Making it the default

When re-entry through Lisp frames is an error rather than silent, the
dynamic state is captured, and the cost on ordinary code is within
about 10–15% (the "may capture" analysis is the likely way there), make
full continuations the default, with `--continuations=escape` to opt
out.

## The earlier recommendation

Kept for the reasoning; superseded by the decision above.

1. **Don't make CPS the default.** It would make every Scheme procedure
   awkward to call from Lisp, the opposite of what docs/interop.md is
   for, to fix a handful of tests.
2. **Add full continuations as an opt-in compilation mode**: a pass
   between psyntax and the translator, selected per program or library
   (`pseudoscheme --continuations=full prog.scm`). Prototype it with
   plain CPS plus a trampoline, since that's the simplest to get right,
   and use it as the reference implementation. Measure, then decide
   whether generalized stack inspection is worth the extra complexity
   for its direct-style fast path.
3. **Make continuations that would cross a Lisp frame fail loudly**:
   detect the barrier and raise a clear condition rather than
   misbehaving.
4. **Independently, add one-shot continuations via threads** for
   generators and coroutines in the default mode. That covers the
   common re-entry uses without changing the compilation model.

Either way, the first step is the same and useful on its own: a pass
framework between psyntax output and the translator (core forms in, core
forms out). That's also where the interop work's predicate wrapping and
the ASDF compilation of libraries will want to hook in.
