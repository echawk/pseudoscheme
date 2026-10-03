# Full continuations: should we CPS?

Status: analysis and recommendation; nothing implemented yet.

## What fails today, and why

`call-with-current-continuation` compiles to a Lisp `block` and a closure
that does `return-from` it (`builtin.scm`). That gives **escaping**
continuations, used while the `call/cc` is still on the stack: early
exits, `guard`, generators that never resume. It cannot give
**re-entry**, calling a continuation after its `call/cc` has returned,
which is what coroutines, re-entrant generators, `amb`, and the classic
`dynamic-wind` re-entry tests need.

How much this actually costs us in the test suites:

* **R5RS (chibi): 1 failure of 5.** Of the 5 failing R5RS tests, only
  one is a continuation test (re-entering a `dynamic-wind` through a
  saved continuation). The other four are macro problems in the old
  front end: a locally rebound `...`, locally rebound `unquote` and
  `unquote-splicing`, and one test using both R7RS's custom ellipsis
  `(syntax-rules ::: ...)` and patterns after an ellipsis. Through
  psyntax, all of these pass except the custom ellipsis, which the 2007
  psyntax doesn't implement (it's an R7RS addition; see ROADMAP.md).
* **R7RS (chibi):** two or three tests (the same `dynamic-wind` test,
  generators).
* **R6RS (Racket's suite):** psyntax's own `guard` expansion
  re-entered continuations, and was rewritten to escape only (see
  vendor/psyntax/psyntax/expander.ss); with that, the suite's
  `control` and `exceptions` tests don't depend on re-entry.

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

## Recommendation

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
