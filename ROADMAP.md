# Roadmap

Where things stand and what to do next, roughly in order. Numbers come
from the runners in `tests/`; re-run them rather than trusting this file.

| runner | result |
|---|---|
| `tests/run-r6rs-tests.lisp` (Racket's R6RS suite) | 8689 pass, 213 fail; all 25 programs run to completion |
| `tests/run-r7rs-tests.lisp` (chibi) | 868 of 924 |
| `tests/run-r5rs-tests.lisp` (chibi) | 183 of 188 |
| `tests/run-syntax-case-tests.lisp` | psyntax 17/17, old syntax-case 17/17 |
| `tests/run-library-tests.lisp` | 44/44 |
| `make -C contrib/cli test` | 10/10 |

## Architecture now

```
            R6RS source                       R7RS source        R5RS source
                 |                                 |                  |
             psyntax  (vendor/psyntax)       src/library.lisp         |
                 |                          + native syntax-rules     |
                 v                                 v                  v
          core Scheme ------------------> translator (src/*.scm) ---> Common Lisp
                 |
      host globals: src/r6rs/*.lisp, src/r7rs/*, src/numbers.lisp
```

psyntax is the front end for R6RS; R7RS and R5RS still go through the
native classifier and its `syntax-rules`. Making psyntax the front end
for everything is the next step.

## 1. R7RS on psyntax

R7RS libraries are the same thing as R6RS libraries with a different
surface, and psyntax already has the library system. Plan:

1. **Synthesized libraries.** psyntax's `install-library` (exported by
   `(psyntax library-manager)`) can register a library whose exports are
   host globals (`core-prim` bindings). Use it to build, at boot,
   libraries over host procedures that aren't in psyntax's R6RS tables:
   `(pseudoscheme r7rs-primitives)` and so on. This is the same
   mechanism docs/interop.md needs for `(cl <package>)`, so build it once.
2. **`define-library` → `library`.** Translate R7RS declarations
   (`export` with `(rename a b)`, `import`, `begin`, `include`,
   `include-ci`, `include-library-declarations`, `cond-expand`) into an
   R6RS `library` form before psyntax sees it. Integers in library names
   need care: R6RS reads a trailing list of integers as a version.
3. **`(scheme base)` etc. as library source** over `(rnrs)` and the
   primitives library, defining the R7RS-specific syntax with
   `syntax-case`: R7RS's `define-record-type` (different syntax from
   R6RS's), `parameterize` with converters, `define-values`,
   `cond-expand` (procedural: it needs the feature list),
   `syntax-error`, `guard` (R7RS = R6RS), `delay-force`. Where R6RS and
   R7RS procedures differ under one name (`bytevector-copy!`'s argument
   order, `error`'s signature, character-by-character
   `string-upcase`), export the R7RS version under the standard name via
   `(rename ...)`.
4. **Custom ellipsis** `(syntax-rules ::: ...)`: add to psyntax's
   `syntax-rules` by rewriting the rules (custom ellipsis → `...`,
   literal `...` → `(... ...)`).
5. The REPL (`r7rs:repl`, the CLI default) on psyntax's interaction
   environment, as `--r6rs` already is.
6. R5RS: run through psyntax's `(rnrs r5rs)` plus a folding reader, then
   retire `src/library.lisp` and the native R7RS layer. The native
   classifier stays as the translator's own bootstrap expander.

## 2. The remaining R6RS failures

Run `tests/run-r6rs-tests.lisp -v NAME` to see them. By program:
bytevectors 70, io/ports 70, base 30, flonums 19, syntax-case 8,
unicode 8, records/syntactic 4, exceptions 2, r5rs 2. Not yet triaged.

## 3. Lisp interop

See docs/interop.md. In order: `(cl <package>)` libraries with predicate
wrapping and `#:keyword` syntax; Scheme libraries as Lisp packages
(`use-library`); ASDF component types for Scheme sources; CLOS from
Scheme.

## 4. Compiled libraries

Every run re-expands the library sources it imports, and booting
psyntax re-translates its 600 KB image (about 1.7 s; the command-line
program avoids this by booting at build time). Fixes:

- Translate `psyntax-pseudoscheme.pp` to a `.pso` and let ASDF compile
  it, like the translator's own sources.
- Serialize expanded libraries (export substitution, environment,
  visit/invoke code) into compiled output, as Ikarus's later psyntax
  does, so a compiled library re-installs without re-expansion. This is
  also the ASDF story of docs/interop.md 2.3.

## 5. Continuations

See docs/continuations.md: a pass framework between psyntax output and
the translator, an opt-in full-continuation mode (CPS + trampoline as
the reference, generalized stack inspection as the candidate fast
path), barrier errors at Lisp frames, and one-shot continuations from
threads in the default mode.

## 6. Portability

Developed and tested on SBCL only. SBCL-specific today:

- Gray streams (`sb-gray`) for binary, custom and transcoded ports:
  trivial-gray-streams elsewhere.
- `sb-unicode` for case mapping, normalization and general categories.
- `sb-kernel` float bit access for `bytevector-ieee-*`.
- `sb-sys:make-fd-stream` for the standard binary ports, `sb-ext` for
  infinities/NaN and float traps, and `sb-ext:with-timeout` in the R6RS
  test runner.
- The CLI Makefile accepts `LISP=ccl|ecl|clisp`, but only SBCL has been
  tried.

## 7. Smaller items

- `write` doesn't detect cycles (datum labels); writing a circular
  structure doesn't terminate. `equal?` does handle cycles.
- psyntax's state (installed libraries, gensym counter) is global and
  unlocked: one expanding thread at a time.
- `include`/`include-ci` resolve relative to the working directory,
  not the including file.
- Remove `vendor/syntax-case/` and `pseudoscheme/syntax-case`, now
  superseded by psyntax.
- Bootstrapping without an existing Pseudoscheme (the `todo` file's
  first item): judged feasible but substantial earlier (a portable
  record system, a mini CL-package system, a `.pso` printer). psyntax
  itself would be portable for free.
