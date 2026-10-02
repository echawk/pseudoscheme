# Roadmap

What exists, what the tests say about it, and what to do next, in rough
order of payoff. Numbers are from the test runners in `tests/` as of
this writing; re-run them rather than trusting this file.

| runner | what it checks | result |
|---|---|---|
| `tests/run-r5rs-tests.lisp` | chibi's R5RS suite | 183 / 188 |
| `tests/run-syntax-case-tests.lisp` | hygiene, syntax-case, derived forms | 17 / 17 |
| `tests/run-library-tests.lisp` | library layer, R7RS and R6RS front ends | 44 / 44 |
| `tests/run-r7rs-tests.lisp` | chibi's R7RS suite (`-v` lists failures) | 775 / 842 ran, 114 forms unreadable |
| `tests/check-r7rs-exports.py`, `check-r6rs-exports.py` | export tables vs. the PDFs | pass |

`(r7rs:print-coverage)`: 551 exports across the 16 R7RS-small libraries
(counting `(scheme r5rs)` and names that appear in several libraries
each time), 539 real, 12 stubs, 0 missing syntax. The stubs are all
binary I/O: bytevector ports, `read-u8`/`peek-u8`/`u8-ready?`/`write-u8`,
`read-bytevector(!)`/`write-bytevector`, `open-binary-{input,output}-file`.

## Architecture in one paragraph

A library is a `program-env` + `interface` = a `structure` (module.scm),
imports alias binding nodes, so variables and macros are shared, not
copied (`src/library.lisp`). R7RS and R6RS each have an export table
transcribed from the report and checked against the PDF, a set of Lisp
primitives (`rts.lisp`), Scheme source (`base.scm`), and an `r*.lisp`
that assembles them into an implementation environment and registers
the libraries, stubbing procedures that aren't there yet. R7RS bodies
are translated natively; R6RS bodies go through the vendored
syntax-case first (`sc-eval`). Everything else below is a gap in that
picture.

## R7RS: what the failing chibi tests are about

Grouped by root cause (run `tests/run-r7rs-tests.lisp -v`):

1. **The reader** (the 114 unreadable forms, plus several failures).
   `read.scm` is a Scheme48-derived R5RS reader. Needed, all in
   `read.scm` (then `regenerate-reader-writer`, see README): `#u8(...)`
   (and `write` for bytevectors); `#!fold-case` / `#!no-fold-case`;
   `#true` / `#false`; complex literals (`3+4i`, `+i`), `+inf.0`,
   `-inf.0`, `+nan.0` (and masking float traps so they can be
   produced); `0.` must read as a flonum (CL reads `0.` as the integer
   0); `\x41;` escapes in strings and `|...|` symbols; datum labels
   `#0=` / `#0#`; `#\x41`. Also **case sensitivity**: the reader folds,
   R7RS doesn't (`(symbol=? 'a 'A)` is #t here; `(eq? 'bitBlt
   (string->symbol "bitBlt"))` is #f). The as-typed-spelling stash
   papers over printing but not identity; the real fix is a
   case-preserving reader mode, which also affects how the translator
   names CL symbols (see README "Known gaps").
2. **Numeric fidelity of the R5RS base** (~20 failures, not R7RS
   specific). `floor`/`ceiling`/`round`/`truncate` return integers for
   floats (`(floor 3.5)` is `3`, should be `3.0`); `max`/`min` don't
   propagate inexactness; `(integer? 3.0)`, `(rational? 1e15)`,
   `(inexact? 3)` are wrong; `exp`/`log` use single floats (set
   `*read-default-float-format*` to `double-float` or coerce);
   `(char-numeric? #\0)` returns the digit weight rather than `#t`
   (the `pred` integration in `builtin.scm` should return a boolean,
   not a generalized one). Fix by overriding in `base.scm` first;
   `builtin.scm` later.
3. **`syntax-rules`** (`rules.scm`): custom ellipsis `(syntax-rules :::
   ...)`, `(... ...)`, `_`, patterns after an ellipsis, vector
   patterns, and `define-syntax`/`let-syntax` in internal-definition
   position. The vendored syntax-case has the same pre-R6RS limits
   except `(... ...)`. Options: extend `rules.scm`, or move R7RS over
   to syntax-case once it has per-library scoping (below).
4. **Ports**: binary ports and bytevector ports (the 12 stubs);
   `read-error?` never true (the reader signals plain errors: give it a
   condition type); `write`/`write-shared`/`write-simple` with datum
   labels, and *no cycle detection at all* today -- `write` of a
   circular list exhausts the heap (the runner survives this only
   because it catches `serious-condition`).
5. **Unicode string operations**: `string-upcase` etc. map character by
   character, so `ß`, final sigma, `İ`, `ǰ` are wrong; `string-foldcase`
   is just downcase. SBCL has `sb-unicode:` functions; other
   implementations need a table.
6. **Continuations**: `call/cc` is escape-only (block/return-from), so
   re-entry fails (one chibi test, plus the generators-and-coroutines
   idioms generally). `guard` here re-raises from the guard rather than
   from the raise point because of this (documented in `base.scm`);
   observable only for `raise-continuable` through a non-matching
   guard.
7. `include` / `include-ci` read relative to the process's working
   directory, not the including file; `include-ci` doesn't fold case.
   `load` takes no environment argument. `(scheme repl)`'s
   `interaction-environment` is the plain user env, not one that has
   the R7RS libraries imported.

## R6RS

Done: `library` form (names, versions, version references, export
renames, import sets incl. `for`/`library`), top-level programs,
`(rnrs base)` (cross-checked), `(rnrs syntax-case)`, slices of
`(rnrs exceptions)` and `(rnrs conditions)`, `guard`, `div`/`mod`
family, `assert`, `letrec*`, `let-values`.

To do, roughly by dependency:

- **The rest of the standard libraries** -- they are in the separate
  "Standard Libraries" report, which isn't in the repo; add it, then
  transcribe its export lists the same way (a `tests/check-r6rs-*.py`
  per library). `*planned-libraries*` in `src/r6rs/exports.lisp` names
  them. Much is reuse: `(rnrs bytevectors)` over the R7RS bytevector
  primitives (plus the `bytevector-*-ref/set!` integer and float
  families), `(rnrs lists)` over the R5RS list procedures,
  `(rnrs hashtables)` over CL hash tables (`make-hash-table` is already
  used by `p-utils.scm`), `(rnrs unicode)` over item 5 above,
  `(rnrs arithmetic fixnums/flonums/bitwise)` are CL one-liners,
  `(rnrs sorting)` over `sort`/`stable-sort`, `(rnrs records *)` over
  the record primitives in `src/r7rs/rts.lisp` (need parent types,
  sealed/opaque, inspection), `(rnrs eval)`, `(rnrs programs)`,
  `(rnrs control)` (`when unless do case-lambda`), `(rnrs r5rs)`, and
  the composite `(rnrs)`.
- **Compound conditions**. Today a condition is the R7RS error object
  plus `who`/`kind`. R6RS conditions are bags of simple conditions with
  a type hierarchy (`&condition &message &warning &serious &error
  &violation &assertion &irritants &who &non-continuable &implementation-
  restriction &lexical &syntax &undefined`), `condition`,
  `simple-conditions`, `condition-predicate`, `condition-accessor`,
  `define-condition-type`; and `raise` of a non-condition, `raise`
  vs `raise-continuable` handler-return rules, `&non-continuable`.
- **Reader syntax**: `[ ]` as parentheses, `#vu8(...)`, `#'x` `` #`x ``
  `#,x` `#,@x` for `syntax`/`quasisyntax`/`unsyntax`, `#!r6rs`,
  `#!fold-case`. R6RS is case-sensitive (item 1 above).
- **Phases and `for`**: accepted and ignored. Real phase separation
  needs the expander to be per-library (next section).
- **identifier-syntax / `make-variable-transformer`**: the 1992
  expander raises "invalid context for identifier" when a macro keyword
  is used as a plain identifier; supporting identifier macros means a
  change to `chi` in `expand.ss` and regenerating `expand.pp` (run
  `expand.ss` through our own `sc-expand`, as the original `loadpp.ss`
  does for Chez, and write the result out). `set!` forms with a macro
  as target need the same.
- **`datum->syntax` is only symbol-deep** (`implicit-identifier` per
  symbol leaf) and `syntax-violation` takes no `subform` blame.
- No R6RS test suite is in the repo. Write one from the report's own
  examples, as the chibi suites do for R5RS/R7RS.

## syntax-case and the two macro worlds

The vendored expander keeps macros in one global table on symbol
plists; the native classifier keeps them as nodes in per-environment
tables. Consequences today: syntax-case macros ignore library
boundaries (an exported macro is "exported" to everyone, can't be
renamed/prefixed/`only`-restricted on import), and the native macros of
`base.scm` (`parameterize`, `case-lambda`, ...) are invisible to
syntax-case-expanded code. That's why R6RS re-defines the few it needs
(`guard`, `let-values`, `letrec*`, `assert`) in `src/r6rs/macros.ss`.

Ways to unify, cheapest first:
1. Key the plist table by (library, symbol): `put/get-global-definition-
   hook` in `hooks-pseudoscheme.ss` already see every definition and
   lookup; bind a "current library" special in `sc-eval` and look macros
   up through the library's import graph. Gives per-library scoping and
   import renaming without touching the translator.
2. Teach the expander about native macro nodes: in `chi`, when the head
   identifier has no syntax-case binding but the *library env* binds it
   to a native macro, hand the form to the classifier's
   `classify-macro-application` (explicit-renaming transformers: it
   already takes `(proc form rename compare)`) and continue expanding
   its output.
3. Make syntax-case the front end of the translator: replace `alpha`'s
   macro-expansion step with `expand-syntax`. This is the "right"
   answer and the large one.

## Libraries (`src/library.lisp`)

- A library's body is evaluated one form at a time, in order, with no
  separate pass over definitions first, so R6RS's two-phase body
  treatment (collect definitions, then expand expressions) isn't
  modeled; ordinary forward references between procedures work.
- `import` at the REPL (R7RS 5.2) isn't wired up; use `psl:import-into`
  on `ps:scheme-user-environment`. Likewise there's no `load`-a-file
  entry point for R7RS programs beyond `psl:load-program`.
- Library versions: parsed and matched, but registering a second
  version of the same name replaces the first.
- Re-defining a library creates a fresh environment and replaces the
  registry entry; programs that imported the old one keep the old one.

## Bootstrapping without an existing Pseudoscheme

(the `todo` file's question: can `.pso` files be regenerated from
Racket or Guile instead of requiring a working Pseudoscheme first?)

Investigated and judged feasible but substantial -- not started.
Findings: most of the translator's apparent CL-specificity
(`generate.scm`, `builtin.scm`, `emit.scm`) is quoted data describing
CL output, portable regardless of host (confirmed by inspection and a
working Racket proof-of-concept of `p-record.scm`'s API using native
Racket `struct`s). The real cost centers, none fatal but all real work:
a portable record system (POC'd), a portable fluid/hash-table shim
(easy), a from-scratch mini CL-"package" system (Pseudoscheme uses real
CL packages pervasively for its naming strategy; neither Racket nor
Guile has an equivalent), and a from-scratch CL-syntax-faithful printer
for `.pso` output text. Multi-session scope; revisit if it becomes a
priority.
