# Roadmap

Where things stand and what to do next, roughly in order. The numbers
come from the runners in `tests/`; re-run them rather than trusting
this file.

| runner | result |
|---|---|
| `tests/run-r7rs-tests.lisp` (chibi's R7RS suite) | 969 of 976 |
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8709 pass, 193 fail; all 25 programs run to completion |
| `tests/run-r5rs-tests.lisp` (chibi's R5RS suite) | 183 of 188 |
| `tests/run-interop-tests.lisp` | 104/104 |
| `tests/run-library-tests.lisp` | 55/55 |
| `tests/run-srfi-system-tests.lisp` (SRFIs 18, 106, 170, 229) | 62/62 |
| `tests/run-syntax-case-tests.lisp` | 17/17 |
| `make -C contrib/cli test` | 15/15 |
| `tests/run-library-corpus.lisp` (real libraries) | Akku: 228 of 387; snow-fort: 91 of 130 |
| `bench/` (r7rs-benchmarks) | 57/57; geometric mean 3.0× Chez's time (Guile 2.8×, Gauche 9.2×) |

## Architecture now

```
 R6RS source   R7RS source (define-library -> library)    R5RS source
      \              /                                          |
       psyntax  (vendor/psyntax; library system, syntax-case,   |
       |         syntax-rules, (cl <package>) libraries)        |
       v                                                        v
   core Scheme -------> translator (src/*.scm, native syntax-rules) ---> Common Lisp
       |
   host globals: src/r6rs/, src/r7rs/, src/numbers.lisp, src/compat/,
                 src/interop.lisp; SRFIs as Scheme in src/srfi/
```

psyntax is the front end for R6RS and R7RS. The native classifier and
its `syntax-rules` remain as R5RS mode's expander and the translator's
own bootstrap expander. The Lisp/Scheme bridge is described in
docs/interop.md.

## 1. Conformance: the remaining failures

**R7RS (7).**

- `(sqrt -1.0-0.0i)`: chibi's test expects `+i`, but R7RS 6.2.4 puts
  the branch cut so that `(imag-part (log -1.0-0.0i))` is −π, which
  makes the answer `-i`, as here. Chibi doesn't distinguish `-0.0` in
  that position. Not a bug.
- The writer: `write-shared` doesn't write datum labels, `write`
  doesn't detect cycles, and symbols that need `|...|` aren't written
  with bars. (The reader handles datum labels.)
- One `dynamic-wind` test re-enters a continuation (see 3).

**R6RS (193).** By program:

| program | failures |
|---|---|
| bytevectors | 70 |
| io/ports | 65 |
| base | 27 |
| flonums | 9 |
| syntax-case | 8 |
| unicode | 6 |
| records/syntactic | 4 |
| exceptions | 2 |
| r5rs | 2 |

Not triaged yet; `tests/run-r6rs-tests.lisp -v NAME` lists them.

**R5RS (5).** These are continuations (3) and harness-level issues.

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
- **Compiled libraries.** psyntax's own image is compiled once by ASDF
  (the `psyntax-image` component; `psx::compile-image`), so booting
  psyntax takes milliseconds rather than about 1.7 s. Every run still
  re-expands the libraries it imports (`(srfi 1)` takes about 0.1 s).
  Next, serialize expanded libraries into fasls, as Ikarus's later
  psyntax does:
  - make psyntax's marks gensyms rather than fresh strings compared
    with `eq?`, so that syntax objects keep their identity across fasl
    files (labels already are gensyms, for the same reason);
  - write each library as its id, name, version, the ids of the
    libraries it was expanded against, its export substitution and
    environment, and its visit and invoke code translated to Lisp;
  - add a hook to psyntax's library manager to load such a file before
    expanding the source, valid only while every dependency still has
    the id it was compiled against;
  - cache under `~/.cache/pseudoscheme/`, keyed by the library form
    (after `include` and `cond-expand`), the dependencies' ids and the
    image; refuse to cache expansions holding live Lisp objects (the
    `(cl <package>)` bridge's `verbatim` closures);
  - then the `:r7rs-library` ASDF components (src/asdf.lisp) can really
    compile, and the SRFIs can be compiled into the CLI image.

## 3. Continuations

docs/continuations.md describes the plan:
- a pass framework between psyntax's output and the translator;
- an opt-in full-continuation mode, with CPS plus a trampoline as the
  reference and generalized stack inspection as the candidate fast
  path;
- barrier errors at Lisp frames;
- one-shot continuations from threads in the default mode.

Today `call/cc` is escape-only (Lisp `catch`), and re-entering a
continuation signals an error. SRFI 158's coroutine generators are
buffered as a result (src/srfi/README.md).

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
- SRFIs not yet in `src/srfi/`: 38 (needs datum labels, section 1)
  and 226 (needs re-entrant continuations, section 3), among the
  commonly used ones.
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

## 7. Smaller items

- Bootstrapping without an existing Pseudoscheme (the `todo` file's
  first item). Earlier analysis judged it feasible but substantial: it
  needs a portable record system, a mini CL-package system and a `.pso`
  printer. psyntax itself would be portable for free.
- ASDF's one-second timestamps can leave a stale fasl after `.pso`
  regeneration. Clear the fasl cache (README, "Bootstrap artifacts").
