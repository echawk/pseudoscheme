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

### Calls that can't capture

Most calls can't reach `call/cc`, and the transformation finds them, so
most code compiles as it would escape-only. Within a top-level form (a
program's or a library's body is one, its definitions a letrec):

- **Safe procedures.** A variable bound once and for all to a lambda is
  *known*. A known procedure is *safe* if nothing it calls, in any
  position, can capture: primitives that call no procedure, and other
  safe procedures. Assuming every known procedure safe and striking out
  those that call something unsafe, until nothing changes, gives the
  largest consistent set, which is right: a cycle of calls among
  procedures that call nothing else never reaches `call/cc`. Calls to a
  safe procedure aren't sites, and one that calls only safe procedures
  needs no machine. `fib` calling `fib` is the typical case.
- **Safe calls of calling primitives.** `map`, `for-each`, `apply`, the
  folds, sorts and searches call only the procedures passed to them, so a
  call passing only safe procedures (safe variables, primitives, lambdas
  with safe bodies) can't capture: it isn't a site, and it calls the
  primitive itself rather than its frame-aware version.
  `call-with-values` needs the frame-aware version only if its producer
  may capture; its consumer is a tail call.
- **Escape-only `call/cc`.** In `(call/cc (lambda (k) body))`, if BODY
  only calls `k`, or passes it to a parameter of a known procedure that
  does the same, or calls it from a lambda that can't outlive the call (a
  procedure argument of `map` and the like, a local loop that is itself
  only called), then `k` can only be invoked during the extent of the
  `call/cc`, and nothing need be captured: it's a `catch`
  (`%escape-call/cc`), in an `#(:escape promoted tag)` frame that
  re-establishes the catch if a continuation captured inside is
  re-entered. ctak and fibc are all such escapes.
- Raising an error isn't a site: the handler of a non-continuable raise
  can't return to the raiser, so a continuation captured in it never
  needs the raiser's frame. (`raise-continuable` is a site.)

### Frames for the runtime's own calls

Lisp code that calls Scheme procedures pushes frames of its own, which
rebuilding re-establishes:

- `#(:winder promoted (before . after))`: `dynamic-wind`.
- `#(:handler promoted (handler . outer))`: `with-exception-handler`'s
  thunk, with the handler and `handler-bind` around it; `#(:handlers
  promoted handlers)` and a `:k` frame around a handler `raise` calls.
  Hooks in the R7RS layer (`*call-handler*`, `*call-with-handler*`)
  route every raise through them. R6RS's `guard` re-enters its handler to
  re-raise in the dynamic environment of the `raise`.
- `#(:extent promoted establish)`: a dynamic context a primitive sets up
  around a call, re-established by calling ESTABLISH again:
  `parameterize`'s.
- `#(:resume promoted function state ...)`: one per call made by a loop
  written in Lisp (`map`, `for-each`, `vector-map`, `vector-for-each`,
  `string-map`, `string-for-each`); rebuilding calls `(function value
  state ...)` to go on with the loop. `map` and `vector-map` reverse their
  results destructively only if nothing was captured in the loop, since
  re-entry must not change what an earlier return returned.
- `#(:k promoted k)`: a Lisp continuation (`call-with-values`).
- `#(:barrier promoted name)`: around a call to a primitive that calls a
  procedure and then does more but has no frame-aware version (the sorts,
  R6RS's folds and searches, `force`, `call-with-port`, the string-port
  procedures, ...). Rebuilding one is an error: re-entering through it
  would resume as if the primitive had returned at once.

Re-entry runs the afters and befores only of the `dynamic-wind`s the
current and target continuations don't share. `eval` and `load` inside a
running program don't start a base of their own, so continuations
captured in the evaluated code include the frames outside it.

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
- A big program (bench/'s `compiler`, 11,000 lines) still exhausted an
  8 GB heap: its body, all its procedures closing over one another, was
  one component. A top-level form bigger than
  `psx::*letrec-definitions-limit*` now has its own definitions made
  top-level definitions of their unique names (`hoist-definitions`,
  src/psyntax.lisp), compiled one by one; the analyses above know them
  as defined once.

### Results

- chibi's R5RS suite 189 of 189, R7RS 977 of 978 (the `sqrt` branch-cut
  disagreement is the one left), Racket's R6RS suite all 8902 (two more
  than escape-only), tests/run-continuation-tests.lisp 33 of 33. `make
  test-full` runs them.
- Cost on bench/ against escape-only: BENCH_RESULTS

## Further work

### Re-entry through Lisp frames

Lisp code that calls a Scheme procedure is now either frame-aware (the
loops and dynamic contexts above) or a barrier, so re-entering through
it works or says it can't. What remains:

1. **More frame-aware versions** in place of barriers, as `:resume`
   loops: R6RS's `fold-left`, `fold-right`, `find`, `filter`,
   `partition`, `exists`, `for-all`, `remp`, `memp`, `assp`; the sorts (a
   merge sort can keep its state in frames); `force` (the promise
   internals aren't host globals yet); `call-with-port` and the
   string-port procedures (an `:extent`-like frame that runs the
   after-part on return).
2. **Procedures from Lisp aren't barriers yet**: the bridge's Lisp
   functions (docs/interop.md) and a record type's protocol procedures,
   called from Lisp constructors. The bridge's wrapper for Scheme
   procedures handed to Lisp (`lisp-facing`) could push a barrier.
3. **Code compiled without the transformation**: libraries loaded before
   full continuations were turned on, the command line's precompiled
   libraries. Calls into them are sites, but their own frames aren't
   recorded. Compile the standard libraries in both modes (the library
   cache already keys on the mode), or mark machine frames so a capture
   can tell when untransformed code lies between two.

### Dynamic state

Winders, exception handlers and parameterizations are re-established on
re-entry, and only the winders the two continuations don't share are
run. **One dynamic environment**, as Racket and SRFI 226 keep (winders,
handlers, parameterization and continuation marks in one value a
continuation captures), would make capturing it O(1) and each frame kind
"set this part of the dynamic environment for the extent of the call".
`parameterize` still assigns each parameter's one global value, which
is wrong across threads (ROADMAP.md, 4).

### Speed

The analyses leave sites only where a call can reach `call/cc` or an
unknown procedure. What's left:

1. **Across libraries**: calls to procedures imported from another
   library (or the standard libraries written in Scheme) are unknown.
   Record each exported procedure's safety with the library (the
   compiled-library cache stores its export environment already) and
   trust it for bindings that can't be assigned (R6RS forbids assigning
   imports; REPL redefinitions can't be trusted).
2. **Calls of procedure parameters** (`(f x)` with `f` a parameter) are
   unknown even when every caller passes a safe procedure. Specializing,
   or a runtime flag on safe closures, could help higher-order code.
3. **Cheaper frame pushes**: a per-thread frame stack (a vector and a
   fill pointer) in place of a special binding per site; non-local exits
   inside Scheme (a base, an escape's `catch`, `guard`) restore the
   pointer. A site already costs only about 4 ns.
4. **Fewer hoisted locals**: only variables live across a label need to
   be hoisted; others can stay `let`-bound, avoiding SBCL's value cells
   for those that closures capture.

### Capture and re-entry

1. **Rebuilding is O(depth) per re-entry**, and recursive (it nests a
   Lisp call per frame). Rebuild lazily instead, as the paper's
   conclusion and Hieb, Dybvig and Bruggeman's segmented stacks
   suggest: rebuild only the innermost few frames on top of an
   *underflow* frame which, when they return, rebuilds the next few.
   That bounds the work per re-entry and makes rebuilding iterative.
   Generators, which re-enter the same few frames repeatedly, benefit
   most.
2. Repeated captures share frames (the `promoted` slot), so the
   remaining capture cost is copying new frames.

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
  buffered (src/srfi/README.md).

### Interop and threads

- A Scheme procedure called from Lisp outside any base (a callback):
  captures inside it can escape but not be re-entered, and re-entering
  says so. That's by design: there's no base to rebuild the Lisp caller.
- Frames are per thread (`*fstack*` is bound per base). Invoking a
  continuation in a thread other than its own would rebuild its frames
  there, under that thread's dynamic state; leave it undefined.

### Making it the default

The conditions set earlier (re-entry through Lisp frames an error rather
than silent, the dynamic state captured, ordinary code within about
10–15%) are met for code compiled in full mode: BENCH_DEFAULT_NOTE.

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
