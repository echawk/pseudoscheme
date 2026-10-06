# Roadmap

Where things stand and what to do next, roughly in order. The numbers
come from the runners in `tests/`; re-run them rather than trusting
this file.

| runner | result |
|---|---|
| `tests/run-r7rs-tests.lisp` (chibi's R7RS suite) | 978 of 978 (977 with `--continuations=escape`) |
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8902 of 8902 (8900 escape-only); all 25 programs run to completion |
| `tests/run-r5rs-tests.lisp` (chibi's R5RS suite) | 189 of 189 (188 escape-only; 183 of 188 with `--classic`) |
| `tests/run-interop-tests.lisp` | 107/107 |
| `tests/run-library-tests.lisp` | 55/55 |
| `tests/run-srfi-system-tests.lisp` (SRFIs 18, 106, 170, 229) | 62/62 |
| `tests/run-syntax-case-tests.lisp` | 17/17 |
| `tests/run-continuation-tests.lisp` | 34/34 |
| `make -C contrib/cli test` | 22/22 |
| `tests/run-library-corpus.lisp` (real libraries) | Akku: 228 of 387; snow-fort: 91 of 130 |
| `bench/` (r7rs-benchmarks) | 57/57; geometric mean 1.55× Chez's time, 1.44× escape-only (Guile 2.8×, Gauche 9.2×) |

## Architecture now

```
 R5RS source     R6RS source     R7RS source (define-library -> library)
        \             |              /
         psyntax  (vendor/psyntax; library system, syntax-case,
         |         syntax-rules, (cl <package>) libraries)
         v
   core Scheme --[src/continuations.lisp]--> translator (src/*.scm) ---> Common Lisp
         |
   host globals: src/r6rs/, src/r7rs/, src/numbers.lisp, src/compat/,
                 src/interop.lisp; SRFIs as Scheme in src/srfi/
```

psyntax is the front end for every standard; R5RS runs at a top level
whose bindings are `(pseudoscheme r5rs)`. The translator's native
classifier and `syntax-rules` remain only as its own bootstrap
expander (and `tests/run-r5rs-tests.lisp --classic`). The Lisp/Scheme
bridge is described in docs/interop.md.

## 1. Conformance

Every test of the three suites passes. With `--continuations=escape`,
the tests that re-enter continuations fail, as they must: R7RS's and
R5RS's `dynamic-wind` re-entry tests, R6RS's in `base`, and R6RS's
`guard` that re-raises in the dynamic environment of the `raise`.

chibi's R7RS suite expected `(sqrt -1.0-0.0i)` to be `+i`; the
imaginary part `-0.0` puts the argument just below the branch cut,
where IEEE 754 / C99's `csqrt` gives `-i`, as do Chez, Guile, Racket
and Chicken (Gauche and Chibi lose the sign of the zero). The test now
expects `-i` (tests/chibi/r7rs-tests.scm, marked `PSEUDOSCHEME:`).

## 2. Speed

`bench/` vendors ecraven's r7rs-benchmarks. In the last run
(bench/RESULTS.md) Pseudoscheme was 1.44× Chez's time as a geometric
mean (Guile: 2.8×), 1.55× with full continuations, with all 57
benchmarks completing; bench/RESULTS.md, "What made the difference",
lists the changes that took it from 3.0×. What's left:

- **`ack` (3.9×), `divrec` (3.6×), `browse` (3.5×), `fibc` (3.7×).**
  The translation is what one would write by hand (`labels` functions,
  inline arithmetic); the rest is SBCL's call and allocation costs.
  `(debug 0)` gains about 8%, not worth the backtraces.
- **Records** are a struct and a vector of fields, two allocations
  (`gcbench` 2.1×). One object per record (an SBCL instance of the
  right length) would halve that.
- **String comparisons** (`string=?` and friends) go through an n-ary
  wrapper in src/r7rs/base.scm, since CL's take two strings and
  keywords; a two-argument fast path would do.
- **R6RS overrides of open-coded primitives**: the R6RS layer redefines
  some procedures the translator would open-code (`list-tail`,
  `integer->char` with its check, `assq`/`assv`, the character case
  procedures), which makes them calls. Each is deliberate, but a
  compiler macro could keep the common case inline.
- **Big programs**: a top-level form bigger than
  `psx::*inline-arithmetic-limit*` is compiled without inline
  arithmetic, since SBCL compiles it as one code object; hoisting its
  definitions makes that rare.
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

Full, re-entrant continuations are the default, from generalized
stack inspection (Pettyjohn et al., ICFP 2005); `--continuations=escape`
(or `psx::*full-continuations*` false) makes them escape-only, a Lisp
`catch`. docs/continuations.md describes the design, the analyses that
keep most calls free of it, the frames the runtime pushes, and what's
left.

- A procedure that makes a non-tail call that may capture becomes a
  state machine; each such call pushes a stack-allocated frame, which a
  capture copies and a re-entry rebuilds.
- Most calls can't capture (calls to safe procedures, safe calls of
  `map` and the like, escape-only `call/cc`) and compile as they would
  escape-only.
- `dynamic-wind`, exception handlers, `parameterize`, and the loops
  written in Lisp (`map`, `vector-map`, ...) push frames of their own;
  other Lisp code that calls Scheme procedures (the sorts, R6RS's folds,
  the bridge) pushes a barrier, which makes re-entering through it an
  error.
- tests/run-continuation-tests.lisp: 34 of 34. Cost: 7.7% over
  escape-only as a geometric mean of bench/, most benchmarks at the same
  speed, 1.5–1.8× on a few closure-heavy programs.

Next (docs/continuations.md, "Further work"): frame-aware versions of
the barriers' primitives; safety information across libraries; SRFI 226
(delimited control) and continuation marks on the same frames; SRFI
158's generators as real coroutines (src/srfi/README.md).

**R5RS** runs on psyntax (`ps-r7rs::eval-at-r5rs-repl`, a top level whose
bindings are `(pseudoscheme r5rs)`), so it gets full continuations too;
the classic translator remains for the translator's own bootstrap and
`tests/run-r5rs-tests.lisp --classic` (183 of 188).

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
