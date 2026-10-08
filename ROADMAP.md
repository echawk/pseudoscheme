# Roadmap

Where things stand and what to do next, roughly in order. The numbers
come from the runners in `tests/`; re-run them rather than trusting
this file.

| runner | result |
|---|---|
| `tests/run-r7rs-tests.lisp` (chibi's R7RS suite) | 978 of 978 (977 with `--continuations=escape`) |
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8902 of 8902 (8900 escape-only); all 25 programs run to completion |
| `tests/run-r5rs-tests.lisp` (chibi's R5RS suite) | 189 of 189 (188 escape-only; 183 of 188 with `--classic`) |
| `tests/run-interop-tests.lisp` | 116/116 |
| `tests/run-library-tests.lisp` | 61/61 |
| `tests/run-srfi-system-tests.lisp` (SRFIs 18, 106, 170, 229) | 62/62 |
| `tests/run-syntax-case-tests.lisp` | 17/17 |
| `tests/run-continuation-tests.lisp` | 45/45 |
| `make -C contrib/cli test` | 31/31 |
| `make test-programs` (`tests/programs/`, needs Quicklisp) | 5 of 5 programs |
| `make test-srfi` (`src/srfi/tests/`) | 122 of 122 test programs |
| `tests/run-library-corpus.lisp` (real libraries) | Akku: 228 of 387; snow-fort: 91 of 130 |
| `bench/` (r7rs-benchmarks) | 57/57; geometric mean 1.37× Chez's time, 1.30× escape-only (1.55× and 1.44× before the second pass; Guile 2.8×, Gauche 9.2×) |

## Architecture now

```
 R5RS source     R6RS source     R7RS source (define-library -> library)
        \             |              /
         psyntax  (vendor/psyntax; library system, syntax-case,
         |         syntax-rules, (cl <package>) libraries)
         v
   core Scheme --[src/continuations.lisp]--> translator (src/*.scm) ---> Common Lisp
         |
   host globals: src/r6rs/, src/r7rs/, src/numbers.lisp, src/chez/, src/compat/,
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
(bench/RESULTS.md, "A second pass") Pseudoscheme was 1.37× Chez's time
as a geometric mean with full continuations, the default (Guile: 2.8×),
1.30× escape-only, with all 57 benchmarks completing; it was 1.55× and
1.44× before that pass. bench/RESULTS.md, "What made the difference", lists the
changes that took it from 3.0×. What's left:

- **`ack` (3.6×), `fibc` (3.6×), `divrec` (3.5×), `browse` (3.4×)**,
  with full continuations, in the last run.
  The translation is what one would write by hand (`labels` functions,
  inline arithmetic); the rest is SBCL's call and allocation costs.
  `(debug 0)` gains about 8%, not worth the backtraces.
- **Records** are a struct and a vector of fields, two allocations
  (`gcbench` 2.1×). One object per record (an SBCL instance of the
  right length) would halve that.
- **R6RS overrides of open-coded primitives**: the R6RS layer redefines
  some procedures the translator would open-code (`list-tail`,
  `integer->char` with its check, `assq`/`assv`, the character case
  procedures), which makes them calls; `assv` and `for-each` are even
  looked up at each call (`*closed-primitives*`). Each is deliberate,
  but `*inline-primitives*` (src/psyntax.lisp) can now keep the common
  case inline, as it does for the bytevector accessors and the
  two-argument string comparisons.
- **psyntax's identifier lookup**: `id->label` searches an
  identifier's ribs linearly, and `free-identifier=?` (every
  `syntax-case` literal) does it twice; that is a third of SRFI 148's
  expansion. Hashed ribs, as Ikarus and Chez have, would fix it, in
  vendor/psyntax.
- **Done in this pass** (October 2026):
  - *Shared compiled lambdas* (`canonical-lambda`, src/psyntax.lisp):
    every macro transformer psyntax evaluates was compiled on its own,
    and CPS-style macros make a new one at each step (SRFI 257's tests:
    10,174 of them, 149 shapes, 8.8 s of SBCL compiling). Lambdas are
    now compiled once per shape, with their constants as arguments.
    SRFI 257's tests: 12.1 s to 3.6 s.
  - *Thread stacks*: SBCL gives each thread a stack the size of the
    main thread's, and making one takes time in proportion: 2.6 ms at
    the command line's 256 MB. The command line now gives threads 32 MB
    (`PSEUDOSCHEME_THREAD_STACK_SIZE`): 500 threads in 0.19 s instead of
    1.29 s, SRFI 230's tests 6.9 s to 2.0 s.
  - *Inline host primitives* (`*inline-primitives*`): compiler macros
    on the host's `bytevector-u8-ref`, `-set!`, `bytevector-length` and
    the two-argument `string=?`, `string<?` and so on.
  - *UTF-8* in two passes (size, then fill) instead of through a string
    stream or an adjustable vector: with the bytevector accessors,
    `bv2string` 2.59 s to 0.81 s.
  - *Inexact complex arithmetic*: the out-of-line case of `+`, `-` and
    `*` does two `(complex double-float)` arguments with typed parts;
    SBCL's generic operation boxed every intermediate (`mbrotZ` 11.9 s
    to 3.4 s).
  - *`quotient`, `remainder`, `modulo`, `zero?`* are inline for fixnums,
    like `+`; they were CL's generic `TRUNCATE`, `REM`, `MOD` and
    `ZEROP` (src/builtin.scm; `primes` 1.24 s to 0.99 s).
  - *R6RS record predicates and accessors* test the exact type first,
    inline.
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
  - ASDF's `:r7rs-library`/`:r6rs-library` components (src/asdf.lisp)
    are evaluated with `psx:eval-library` and aren't cached at all, nor
    are libraries that depend on them; they could compile into the
    system's own fasls. The SRFIs can be compiled into the cache ahead
    of time (`make precompile-srfi`, `--precompile-srfi`, `--precompile
    DIR`), but not into the command line's image.
  - The build signature changes whenever any source file's date
    changes, which is safe but coarse: every edit to Pseudoscheme
    recompiles every cached library.
  - Two processes writing the same entry is safe (write to a temporary
    file, then rename), but there's no locking for cleanup.

## 3. Continuations

Full, re-entrant continuations are the default, from generalized
stack inspection (Pettyjohn et al., ICFP 2005); `--continuations=escape`
(or `psx::*full-continuations*` false) makes them escape-only, a Lisp
`catch`. docs/continuations.md shows the transformation's real output
for each case, the analyses that keep most calls free of it, the frames
the runtime pushes, measurements, and what's left.

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
- tests/run-continuation-tests.lisp: 45 of 45. Cost: 5.0% over
  escape-only as a geometric mean of bench/ (7.7% before the October
  2026 speed pass), 39 of 57 benchmarks within 5%, 1.4–1.8× on a few
  closure-heavy programs. A site costs about 2.4 ns; a generator's
  yield, two captures and re-entries, about 200 ns.
- **Delimited continuations** on the same frames: a prompt is a frame
  with a `catch`; `abort-to-prompt` copies the frames above it and
  throws; calling the composable continuation rebuilds them on top of
  the caller's stack. `(pseudoscheme control)` (src/control.sls) has
  Guile's interface: `call-with-prompt`, `abort-to-prompt`, `%`,
  `shift`/`reset`, `call/ec`. docs/continuations.md, "Delimited
  continuations".
- Built on it: SRFI 158's coroutine generators, SRFI 226 (the sample
  implementation, prompts and marks included), SRFI 248, and R6RS
  `guard`'s re-raise in the `raise`'s dynamic environment.

Next (docs/continuations.md, "Further work"): frame-aware versions of
the barriers' primitives; safety information across libraries;
continuation marks on the same frames as prompts (SRFIs 226 and 248
are still written on call/cc and dynamic-wind, src/srfi/).

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
  refers to the running image, so a Lisp file that uses them must be
  loaded into an image where Pseudoscheme is booted, and its fasl
  can't be loaded into a fresh one.
- **A package/system map** for the cases where they differ (package
  `BT`, system `bordeaux-threads`), so that `(cl bt)` autoloads.
- **Predicate overrides** per import, for functions like Ironclad's
  `verify-signature` whose names don't say they're predicates.
- **An Akku/snow helper** for Lisp: find a project's `.akku/lib` (the
  command line's `--akku` does) or install snow packages from Lisp.
  docs/libraries.md describes the manual way.
- **Bridge issues found while writing SRFIs 18, 106, 126 and 170:**
  - `lisp-funcall` of a raw function (from `lisp-function`) passes
    `#f` to it unconverted, contrary to docs/interop.md: `to-lisp`
    wraps the function and `scheme-facing` unwraps it again through
    `*originals*`.
  - A Lisp function treated as a predicate keeps only its first value
    (`mkstemp`, say).
  - A Lisp condition reaching a Scheme handler is a plain
    `&assertion` (`foreign-condition`, src/r6rs/conditions.lisp);
    passed back to Lisp it is the original again (`(cl:typep e ...)`
    works), but Scheme code can't ask its type without Lisp.
  - `#f` returned through a Lisp macro's body comes back as `()`.
  - A `(cl ...)` export that is both a function and a type
    (`cl:character`) is the function outside Lisp macro calls;
    `lisp-symbol` gets the type.
  - Inside a `lisp` form, `pkg::sym` isn't read as a Lisp symbol.
- **Thread safety**: psyntax's state is global and unlocked: the
  library table, the gensym counter, the interaction environment and
  the parameters the front end sets. Two steps would fix it:
  - first, one recursive lock (bordeaux-threads, already used by SRFI
    18 and the R6RS test runner) around everything that expands or installs
    libraries: `psx:eval-library`, `eval-program`, `eval-top-level` and
    `expand`, and the library locator they call back into. Code that has
    already been expanded runs outside the lock, so only expansion is
    serialized. This is enough for SRFI 18 threads running ordinary
    code;
  - `parameterize` assigns the parameter's one global value for the
    extent of its body (`parameterize*`, src/r7rs/rts.lisp), unless the
    thread has a table of its own values (`*thread-parameters*`, which
    Chez's `fork-thread` gives the threads it makes); otherwise threads
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
- Some libraries depend on chibi- or Gauche-specific leniency, such as
  duplicate pattern variables in `syntax-rules`, and are not counted as
  bugs here.

## 6. Portability

Developed and tested on SBCL. Implementation-specific facilities go
through portability libraries: float-features (infinities, NaN, float
traps, IEEE bit access), cl-unicode (case mapping, normalization,
general categories), trivial-gray-streams (binary, custom and
transcoded ports), trivial-cltl2 (lexical environments for Scheme
macros in Lisp), trivial-garbage (weak tables, finalizers) and
bordeaux-threads (SRFI 18; timeouts in the R6RS test runner); SRFI 106
uses usocket and SRFI 170 osicat when they're installed. What is left per implementation, with fallbacks:

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

## 8. Guile (`--guile`), and Guix

docs/guile.md has the investigation and plan. The approach is the same
as Racket's: implement the layer Guile's own Scheme code runs on, then
run that code unchanged. libguile is about 1,100 C primitives; boot-9
(the module system) and psyntax-pp, both Scheme, sit on top, and psyntax
expands to Tree-IL.

- **Done**: `--guile` runs Guile 3.0's own `boot-9.scm`, psyntax and
  modules, unmodified and loaded from a Guile installation. They run on
  libguile's primitives in Lisp, with a Tree-IL compiler to core Scheme
  and C extensions as Lisp `load-extension` init functions (src/guile/).
  Guile's own test suite is vendored (`make test-guile`): 5,982 tests pass
  on the first run. Prompts (section 3) are what its exceptions are built
  on. `(system foreign)` is on CFFI, through the FFI layer Chez's FFI
  uses too (src/ffi.lisp, system `pseudoscheme/cffi`). Guile's elisp compiler runs the basics.
- **Left:** the test files that stop on a missing primitive or extension;
  Guile-style printing and error keys; GOOPS's C half; a compiled-module
  cache; sockets and popen; source positions; Emacs Lisp's `boot.el` and its
  tests.
- **Guix**, the long-term test: `(guix records)`, G-expressions, the
  store protocol to a real `guix-daemon`, and the FFI libraries
  (guile-gcrypt, guile-git, guile-sqlite3, guile-zlib). Milestone: the
  same derivation as `guix build -d hello`.
- **Depends on:** the compiled-library cache (section 2; Guix is
  hundreds of modules), thread safety (section 4), and the CFFI
  experience of the bridge and of Chez's FFI.
- **First step now:** the "error=1" files of Guile's suite
  (docs/guile.md, "Where it stands").

## 9. Chez Scheme (`--chez`)

`pseudoscheme --chez` (or `scheme-script`) runs Chez programs: Chez's
top level, `(chezscheme)` with 1118 of Chez 10.4.1's 1715 names, and
library files whose library a macro makes. The code is in src/chez/;
docs/chez.md describes how each problem was solved, measured against
[e](https://github.com/paveluv/e), an editor written in Chez:
33 of its 63 test scripts pass.

- **Done**: the top level (a `begin` evaluated form by form, as Chez
  does), `meta define`, top-level `library` forms from macros, a body's
  `import` of libraries, Chez's extensions to R6RS procedures
  (`dynamic-wind`'s critical flag, file options, one-argument `eval`),
  threads with per-thread parameters, recursive mutexes and conditions,
  the FFI on CFFI, fd ports with nonblocking reads, `format` on CL's,
  `Exception in ...` messages, the reader's `#&`, `#vfx`, `#N(...)`,
  `#%`, `#!eof`, and about 300 procedures (lists, numbers, paths,
  hashtables, time, boxes, fxvectors, system).
- **Next**: engines and timer interrupts (a tick check at procedure
  entry in `--chez` code, with the existing frames to capture);
  annotations (`get-datum/annotations`); ftypes; `r6rs:` variants;
  `define-record`/`record-case`; the printer's parameters.

## 10. Smaller items

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
