# Roadmap

Where things stand and what to do next, roughly in order. The numbers
come from the runners in `tests/`; re-run them rather than trusting
this file.

| runner | result |
|---|---|
| `tests/run-r7rs-tests.lisp` (chibi's R7RS suite) | 948 of 975 |
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8690 pass, 212 fail; all 25 programs run to completion |
| `tests/run-r5rs-tests.lisp` (chibi's R5RS suite) | 183 of 188 |
| `tests/run-interop-tests.lisp` | 73/73 |
| `tests/run-library-tests.lisp` | 44/44 |
| `tests/run-syntax-case-tests.lisp` | 17/17 |
| `make -C contrib/cli test` | 13/13 |
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

**R7RS (27).** Most of these are small and independent.

- `syntax-rules`:
  - `_` should be a wildcard, not a pattern variable, so a pattern may
    use it twice;
  - `...` should be allowed as a literal, `(syntax-rules (...) ...)`.

  Both belong in psyntax's `syntax-rules` and `syntax-case`, since R6RS
  has the same rules.
- Numbers:
  - complex functions (`make-polar`, `magnitude`, `angle`, `sqrt` of a
    complex) compute in single floats;
  - `truncate/` rejects inexact integers;
  - `rationalize` of an inexact should be inexact;
  - the branch cut of `sqrt` of `-1.0-0.0i` is on the wrong side.
- `char-numeric?` returns the digit weight instead of `#t`, and
  `char-whitespace?` misses some Unicode spaces.
- `file-error?` and `read-error?` don't recognize the R6RS conditions
  that raise for those errors.
- Reader and writer:
  - datum labels: reading `#0=` / `#0#`, and `write-shared` (`write`
    also doesn't detect cycles);
  - `#!fold-case` inside a `read` stream;
  - symbols that need `|...|` aren't written with bars;
  - some invalid input isn't rejected.
- One `dynamic-wind` test re-enters a continuation (see 3).

**R6RS (212).** By program:

| program | failures |
|---|---|
| bytevectors | 70 |
| io/ports | 69 |
| base | 30 |
| flonums | 19 |
| syntax-case | 8 |
| unicode | 8 |
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
- **`mbrotZ` (7.5×).** Complex arithmetic, which is also wrong (single
  floats; section 1).
- **Compiled libraries.** Every run re-expands the libraries it
  imports, and booting psyntax re-translates its image (about 1.7 s; the
  CLI avoids this by booting at build time).
  - Translate `psyntax-pseudoscheme.pp` to a `.pso` that ASDF compiles.
  - Serialize expanded libraries (export substitution, environment,
    visit and invoke code) into fasls, as Ikarus's later psyntax does.
    Then the `:r7rs-library` ASDF components (src/asdf.lisp) can really
    compile.

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

What exists is in docs/interop.md: `(cl <package>)` libraries with
autoloading, `#:keywords`, `(pseudoscheme lisp)`, `use-library`, the
`R5RS`/`R6RS`/`R7RS` API, and ASDF components. Next:

- **`(pseudoscheme clos)`**: `define-class`, `define-generic`,
  `define-method` (Scheme procedures as method bodies; record types are
  structs, so methods can specialize on them).
- **Foreign syntax**: Lisp macros with expression-only arguments
  (`incf`, `when`, the body of `with-open-file`), imported from
  `(cl ...)` and expanded after translating the subforms.
- **A package/system map** for the cases where they differ (package
  `BT`, system `bordeaux-threads`), so that `(cl bt)` autoloads.
- **An Akku/snow helper**: point at a project's `.akku/lib`, or install
  snow packages, from Lisp.
- **Thread safety**: psyntax's state is global and unlocked, so only
  one thread can expand at a time.

## 5. Real-world libraries

From the corpus runs (`tests/run-library-corpus.lisp` over Akku and
snow-fort trees):

- An `(ikarus)` compat library would let xitomatl load (55 of the Akku
  failures). Chez's `meta-cond`/`meta` would let some chez-srfi variants
  load (15).
- More SRFIs in `src/srfi/`. Asked for so far: 60, 113, 115, 146, 225 and
  227.
- SRFI 14's char-sets are Latin-1 only; Unicode char-sets would
  replace the reference implementation's representation.
- Some libraries depend on chibi- or Gauche-specific leniency, such as
  duplicate pattern variables in `syntax-rules`, and are not counted as
  bugs here.

## 6. Portability

Developed and tested on SBCL only. SBCL-specific today:

- Gray streams (`sb-gray`) for binary, custom and transcoded ports;
  trivial-gray-streams would serve elsewhere.
- `sb-unicode` for case mapping, normalization and general categories.
- `sb-kernel` float bit access for `bytevector-ieee-*`.
- `sb-sys:make-fd-stream` for the standard binary ports, `sb-ext` for
  infinities, NaN and float traps, and `sb-ext:with-timeout` in the R6RS
  test runner.
- `sb-int:sbcl-homedir-pathname` in the CLI, to find contribs.
- The CLI Makefile accepts `LISP=ccl|ecl|clisp`, but only SBCL has been
  tried.

## 7. Smaller items

- Bootstrapping without an existing Pseudoscheme (the `todo` file's
  first item). Earlier analysis judged it feasible but substantial: it
  needs a portable record system, a mini CL-package system and a `.pso`
  printer. psyntax itself would be portable for free.
- ASDF's one-second timestamps can leave a stale fasl after `.pso`
  regeneration. Clear the fasl cache (README, "Bootstrap artifacts").
