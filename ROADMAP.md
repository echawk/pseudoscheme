# Roadmap

Where things stand and what to do next, roughly in order. The numbers
come from the runners in `tests/`; re-run them rather than trusting
this file.

| runner | result |
|---|---|
| `tests/run-r7rs-tests.lisp` (chibi's R7RS suite) | 976 of 978 (977 with full continuations) |
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8900 pass, 2 fail (all 8902 with full continuations); all 25 programs run to completion |
| `tests/run-r5rs-tests.lisp` (chibi's R5RS suite) | 188 of 189 (189 with full continuations; 183 of 188 with `--classic`) |
| `tests/run-interop-tests.lisp` | 104/104 |
| `tests/run-library-tests.lisp` | 55/55 |
| `tests/run-srfi-system-tests.lisp` (SRFIs 18, 106, 170, 229) | 62/62 |
| `tests/run-syntax-case-tests.lisp` | 17/17 |
| `tests/run-continuation-tests.lisp` | 16/16 |
| `make -C contrib/cli test` | 20/20 |
| `tests/run-library-corpus.lisp` (real libraries) | Akku: 228 of 387; snow-fort: 91 of 130 |
| `bench/` (r7rs-benchmarks) | 57/57; geometric mean 3.0× Chez's time (Guile 2.8×, Gauche 9.2×) |

## Architecture now

```
 R5RS source     R6RS source     R7RS source (define-library -> library)
        \             |              /
         psyntax  (vendor/psyntax; library system, syntax-case,
         |         syntax-rules, (cl <package>) libraries)
         v
   core Scheme --[src/continuations.lisp, opt-in]--> translator (src/*.scm) ---> Common Lisp
         |
   host globals: src/r6rs/, src/r7rs/, src/numbers.lisp, src/compat/,
                 src/interop.lisp; SRFIs as Scheme in src/srfi/
```

psyntax is the front end for every standard; R5RS runs at a top level
whose bindings are `(pseudoscheme r5rs)`. The translator's native
classifier and `syntax-rules` remain only as its own bootstrap
expander (and `tests/run-r5rs-tests.lisp --classic`). The Lisp/Scheme
bridge is described in docs/interop.md.

## 1. Conformance: the remaining failures

Every remaining failure but one re-enters a continuation, and passes
with full continuations (section 3).

**R7RS (2).**

- `(sqrt -1.0-0.0i)`: chibi's test expects `+i`, but R7RS 6.2.4 puts
  the branch cut so that `(imag-part (log -1.0-0.0i))` is −π, which
  makes the answer `-i`, as here. Chibi doesn't distinguish `-0.0` in
  that position. Not a bug.
- One `dynamic-wind` test re-enters a continuation.

**R6RS (2).** `base`'s `dynamic-wind` re-entry test, and the
`exceptions` test of a `guard` that re-raises in the dynamic
environment of the `raise` (R6RS's `guard` re-enters the handler for
that; escape-only, it re-raises from the guard's own context).

**R5RS (1).** The continuation re-entry test.

## 2. Speed

`bench/` vendors ecraven's r7rs-benchmarks. In the last run
(bench/RESULTS.md) Pseudoscheme was 3.0× Chez's time as a geometric
mean, beside Guile (2.8×), with all 57 benchmarks completing.
Open-coding psyntax's primitives made it 2.4–7.8× faster than the
previous build. The worst ratios point at what to do next:

- **`lattice` (13.5×), `conform` (7.9×), `peval` (5.8×).** These are
  heavy on closures and on `apply`/`map`. Profile them; `map` and
  `for-each` are still called out of line (`*closed-primitives*` in
  src/psyntax.lisp), because the R6RS/R7RS versions replaced the
  integrated ones.
- **`gcbench` (10.2×).** Allocation of record instances and vectors.
  Check how R6RS record constructors compile.
- **`wc` (8.7×).** Character I/O, one `read-char` at a time through
  generic port code.
- **`ack` (8.6×), `takl` (6.9×), `cpstak` (5.9×).** Calls and
  arithmetic: `+`/`-` are CL's generic versions. Fixnum fast paths like
  the comparisons' (src/numbers.lisp) might help, and so might SBCL
  declarations in the translator's output.
- **`mbrotZ` (7.5×).** Complex arithmetic.
- **Compiled libraries.** Done for libraries loaded from files
  (src/library-cache.lisp). Importing a library spends about 90% of
  its time in SBCL's `compile` of the translated code, and only about
  10% in expansion, so the cache holds compiled fasls, under
  `~/.cache/pseudoscheme/libraries/`. Importing five SRFIs takes
  0.018 s from the cache instead of 0.39 s (0.50 s the first time,
  which writes the cache).
  - psyntax hooks (vendor/psyntax/README-pseudoscheme.md): a loader
    tried before expanding a library from source, and a hook given
    everything `install-library` needs once a library is expanded.
  - Keyed by the library's form (after `include` and `cond-expand`),
    a build signature (the Lisp, and the names and dates of
    Pseudoscheme's source files) and the continuations mode. A
    compiled library is installed only if each library it was expanded
    against has the same id; otherwise it is expanded and compiled
    again, so a changed dependency recompiles its dependents.
  - The R7RS standard libraries are expanded at boot with gensyms named
    the same every session (`with-boot-gensyms`), so their ids are
    stable. Session gensyms carry a random part, and a saved image
    (the command line's) gets a new prefix each time it starts.
  - `PSEUDOSCHEME_LIBRARY_CACHE=0` turns it off;
    `PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY` moves it.

  Left to do:
  - A library whose source holds something the Lisp printer can't print
    readably (a NaN literal, as R6RS's `base` and `flonums` test
    libraries do) isn't cached: the cache writes the translation as Lisp
    source for `compile-file`.
  - Libraries that aren't found through the library path aren't cached:
    those defined at the REPL or in a program file, and `(cl
    <package>)` libraries, whose expansions hold Lisp functions.
    Libraries that depend on one aren't either.
  - Nothing removes stale entries. A cleanup by age, or `pseudoscheme
    --clear-cache`, would do.
  - The `:r7rs-library` ASDF components (src/asdf.lisp) could compile
    into the system's own fasls instead of the cache, and the SRFIs
    could be compiled into the command line's image.
  - The build signature changes whenever any source file's date
    changes, which is safe but coarse: every edit to Pseudoscheme
    recompiles every cached library.
  - Two processes writing the same entry is safe (write to a temporary
    file, then rename), but there's no locking for cleanup.

## 3. Continuations

Decided: full continuations from generalized stack inspection
(Pettyjohn et al., ICFP 2005); docs/continuations.md, "Decision", has
the design. In order:
- a pass framework between psyntax's output and the translator;
- A-normal form, and a handler around each non-tail call that records
  its frame when a capture unwinds the stack; re-entry rebuilds the
  frames from the records;
- continuation marks and `dynamic-wind` on the same mechanism (Racket
  needs the marks too, section 7);
- barrier errors at Lisp frames;
- opt-in at first (`--continuations=full`), then measure;
- one-shot continuations from threads for code compiled without it.

**Implementation** (src/continuations.lisp, opt-in with
`psx::*full-continuations*`, or `--continuations=full` in the R5RS,
R7RS and R6RS test runners). A procedure that makes a non-tail call
that may capture becomes a state machine: one Lisp function, a flat
`tagbody` with a label after each such call and its locals hoisted.
Each such call pushes a stack-allocated frame
`#(machine site parameter-count live-variable ...)` on a special
variable; capturing copies the frames, and re-entry calls each frame's
machine with its site, which restores the live variables and jumps
after the call. Assigned variables held across calls are boxed; long
sequences are cut into chunks so no machine is huge. (A first version
made two closures per call site; SBCL can't compile that at scale:
a thousand closures over one variable take 162 s.)
- tests/run-continuation-tests.lisp: 16 of 16 (re-entry, multi-shot,
  `dynamic-wind`, re-entry into `map`).
- R7RS with full continuations: 977 of 978 (only the `sqrt`
  disagreement is left).
- R5RS with full continuations: 189 of 189, on psyntax (below).
- R6RS with full continuations: all 8902. Two things made
  the big test libraries compilable. SBCL compiles a top-level form,
  closures included, as one component; with `debug` ≥ 1 and ≥ `speed`,
  each function that binds specials keeps its binding stack pointer in
  a slot live across the whole component (`insert-debug-catch`), so the
  register allocator's tables grow as functions × blocks. Full-mode code
  is compiled with `(sb-c::insert-debug-catch 0)`. And each chunk of a
  long sequence is closure-converted and compiled as a component of its
  own (`%lifted`).
- Cost (bench/, `--continuations=full` vs escape-only): ordinary code
  1.36× slower as a geometric mean of 11 benchmarks (cpstak 0.98×,
  mazefun 1.17×, fib 1.74×, earley 2.86×); call/cc-heavy code ctak
  2.3×, fibc 2.1×, after captures began sharing the frames earlier
  captures copied (they were 15.6× and 5.6×).

Left before full continuations can be the default:
- **Re-entry through Lisp frames is silently wrong.** A Lisp function
  that calls a Scheme procedure (vector-map, string-for-each, the sorts,
  R6RS's find, filter and folds, hashtable-update!, call-with-port,
  the bridge's Lisp functions) isn't a frame: a
  continuation captured in the callback and re-entered later resumes as
  if the Lisp function had returned at once. Make the common ones
  frame-aware, as map and for-each are (Scheme compiled with the
  transformation), and push a barrier frame around calls to the rest,
  so that re-entering through one is an error (escaping still works).
  The same goes for procedures of src/r7rs/base.scm, compiled at boot
  without the transformation.
- **Dynamic state**: parameterizations aren't re-established on
  re-entry, and re-entry unwinds and rewinds every `dynamic-wind`, even
  those the current and target continuations share. (Exception
  handlers are: `with-exception-handler`'s thunk and a raised-to
  handler run in frames that rebind the handler stack.)
- **Cost**: a "may capture" analysis would let calls to procedures that
  can't reach call/cc or an unknown procedure (fib calling fib) skip the
  frame, which is most calls in most code; and the special binding per
  call could become an index into a per-thread stack.
- Then: make it the default, with `--continuations=escape` to opt out;
  SRFI 158's generators as real coroutines; SRFI 226 (delimited control),
  which the frames make straightforward (capture up to a prompt).

**R5RS.** R5RS mode's classic translator expands macros itself, so the
transformation can't reach it. R5RS can now also run on psyntax
(`ps-r7rs::eval-at-r5rs-repl`, a top level whose bindings are
`(pseudoscheme r5rs)`: `(scheme r5rs)` plus string ports), which is what
full continuations use. chibi's R5RS suite there: 188 of 189, and 189
of 189 with full continuations, against 183 of 188 on the classic
translator. Next: make psyntax R5RS's default front end (the API's
`r5rs:` functions, the command line's `--r5rs`), keeping the classic
translator for the translator's own bootstrap.

Without it, `call/cc` is escape-only (Lisp `catch`), and re-entering a
continuation signals an error. SRFI 158's coroutine generators are
buffered as a result (src/srfi/README.md). All but one of the
remaining test failures (section 1) need this.

## 4. The Lisp bridge, next

What exists is in docs/interop.md:
- from Scheme: `(cl <package>)` libraries (functions, variables, macros
  and special operators compiled as Lisp, symbols), autoloading,
  `#:keywords`, and `(pseudoscheme lisp)`;
- from Lisp: `r7rs:import` with import sets, Scheme macros as Lisp
  macros, `r7rs:define`, `r7rs:define-library` and `use-library`;
- ASDF components.

Next:

- **Scheme-syntax CLOS** (`(pseudoscheme clos)`): `define-class`,
  `define-generic`, `define-method` with Scheme bodies. Lisp's own
  `cl:defclass`/`cl:defmethod` already work from Scheme.
- **Assignment across the boundary**: inside a Lisp macro call a Scheme
  variable is read-only (it's passed by value), and so is a Lisp
  variable inside a Scheme macro's arguments.
- **Fasl-safe expansions**: code that uses Scheme macros from Lisp
  refers to the running image, which only matters once there are
  compiled libraries (2).
- **A package/system map** for the cases where they differ (package
  `BT`, system `bordeaux-threads`), so that `(cl bt)` autoloads.
- **Predicate overrides** per import, for functions like Ironclad's
  `verify-signature` whose names don't say they're predicates.
- **An Akku/snow helper** for Lisp: find a project's `.akku/lib` (the
  command line's `--akku` does) or install snow packages from Lisp.
  docs/libraries.md describes the manual way.
- **Bridge issues found while writing SRFIs 18, 106, 126 and 170:**
  - `lisp-funcall` passes `#f` to the function unconverted, contrary to
    docs/interop.md: `to-lisp` wraps the function and `scheme-facing`
    unwraps it again through `*originals*`.
  - A Lisp function treated as a predicate keeps only its first value
    (`mkstemp`, say).
  - A Lisp condition reaching a Scheme handler has been turned into a
    plain `&assertion` (`foreign-condition`, src/r6rs/conditions.lisp).
    Keeping the original in a condition component would let `guard`
    clauses test its type.
  - `#f` returned through a Lisp macro's body comes back as `()`.
  - A `(cl ...)` export that is both a function and a type
    (`cl:character`) is the function; `lisp-symbol` gets the type.
  - Inside a `lisp` form, `pkg::sym` isn't read as a Lisp symbol.
- **Thread safety**: psyntax's state is global and unlocked: the
  library table, the gensym counter, the interaction environment and
  the parameters the front end sets. Two steps would fix it:
  - first, one recursive lock (bordeaux-threads, already used by the
    R6RS test runner) around everything that expands or installs
    libraries: `psx:eval-library`, `eval-program`, `eval-top-level` and
    `expand`, and the library locator they call back into. Code that has
    already been expanded runs outside the lock, so only expansion is
    serialized. This is enough for SRFI 18 threads running ordinary
    code;
  - `parameterize` assigns the parameter's one global value for the
    extent of its body (`parameterize*`, src/r7rs/rts.lisp), so threads
    see each other's bindings. It should bind a special variable
    instead, which SRFI 18's `make-thread` would capture so that new
    threads inherit the current bindings. Only the port parameters are
    per-thread today;
  - later, per-thread state where it matters (the interaction library,
    `*include-directory*`), so that threads can expand at the same
    time.

## 5. Real-world libraries

From the corpus runs (`tests/run-library-corpus.lisp` over Akku and
snow-fort trees):

- xitomatl: 123 of its 127 libraries load, with the `(ikarus)` library
  (src/compat/ikarus.scm), `get-mode`/`chmod`/`file-change-time` and a
  real `machine-type` in `(chezscheme)` (chez-srfi derives `posix` from
  it), `%xx` escapes in file names, `(define x)` and definitions in
  `with-syntax` bodies. Left: `(xitomatl profiler ...)` (chez-srfi's
  SRFI 19 hides definitions with a `let-syntax`-bound `define`, which
  doesn't take effect) and `(xitomatl R6RS-lexer)` (an identifier made
  with `identifier-append` isn't found).
- Chez's `meta-cond`/`meta` would let some chez-srfi variants load (15).
- SRFIs not yet in `src/srfi/`: 38 (now possible: the reader and
  writer handle datum labels) and 226 (needs re-entrant continuations,
  section 3), among the commonly used ones.
- Some libraries depend on chibi- or Gauche-specific leniency, such as
  duplicate pattern variables in `syntax-rules`, and are not counted as
  bugs here.

## 6. Portability

Developed and tested on SBCL. Implementation-specific facilities go
through portability libraries: float-features (infinities, NaN, float
traps, IEEE bit access), cl-unicode (case mapping, normalization,
general categories), trivial-gray-streams (binary, custom and
transcoded ports), trivial-cltl2 (lexical environments for Scheme
macros in Lisp) and, in the R6RS test runner, bordeaux-threads
(timeouts). What is left per implementation, with fallbacks:

- `disable-float-traps` (float-features only masks traps around a body)
  and listing the process environment (`get-environment-variables`).
- The standard binary ports: `sb-sys:make-fd-stream` on SBCL,
  `/dev/fd/N` elsewhere.
- The CLI: finding SBCL's contribs, and recognizing an interactive
  interrupt.
- `string-titlecase` finds word boundaries with a simple rule rather
  than full Unicode word breaking.

## 7. Racket (`--racket`)

docs/racket.md has the investigation and plan. In short: make
Pseudoscheme a third backend for Racket's linklet layer, beside Racket
CS and BC, and run Racket's own expander on it, so that the module
system, `#lang`, macros and `racket/base` are Racket's own code.

- **Target linklets, not schemify's output.** Linklets are the
  documented interface (`racket/linklet`) that the expander hands to a
  backend. Schemify's output is Chez Scheme that depends on rumble,
  Racket CS's internal runtime.
- **Stages:** a linklet compiler to Lisp (reusing the translator's back
  end); the primitive instances (`#%kernel` has 1070 names, about 1650
  across all instances in Racket 9.3); hosting the expander linklet,
  built from Racket's source with an installed Racket as Chez builds
  psyntax's seed; then the command line and the Lisp bridge.
- **Depends on:** continuation marks and prompts (section 3; Racket's
  `parameterize` and exception handling are built on them), and the
  compiled-library cache (section 2), since `racket/base` can't be
  expanded from source on every start.

## 8. Smaller items

- Bootstrapping without an existing Pseudoscheme is done (boot/README.md):
  the `.pso` files come from any of seven Schemes, and psyntax's image
  from a seed built by Chez Scheme or, through boot/stage0/, by any R5RS
  Scheme (Chibi, Scheme 48, Gauche). Every seed ends in the same image,
  byte for byte; `make bootstrap-check` checks Chez's and Chibi's, CI
  Chez's and Scheme 48's.
  Left: an s7 adapter for the `.pso` step (untested), and stage0
  adapters for more hosts (CHICKEN, Guile) if wanted.
- ASDF's one-second timestamps can leave a stale fasl after `.pso`
  regeneration. Clear the fasl cache (README, "Bootstrap artifacts").
