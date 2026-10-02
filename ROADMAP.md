# Roadmap

Outline-level only -- these are sketches to pick up later, not plans to
execute now. See README.md for what's actually done (R5RS, the
dedicated reader, vendored syntax-case).

## R6RS

- New `r6rs-sig.scm` interface alongside `ssig.scm`, exported via a
  `pseudoscheme/r6rs` system (already stubbed in `pseudoscheme.asd`,
  currently just an alias for the R5RS stack).
- Most individual procedures are easy, thin CL wrappers in the existing
  `builtin.scm`/`rts.lisp` style: bytevectors (CL has them natively),
  fixnum/flonum operations, hashtables (`ps-lisp:make-hash-table`
  already used elsewhere, e.g. `p-utils.scm`'s tables).
- The real work is the `library`/`import` form and R6RS's condition
  system -- unlike individual procedures, these are structural and
  would lean on `module.scm`'s interface/program-env/structure
  machinery (or something new). Not scoped yet; needs its own design
  pass before writing code.
- `(rnrs base)`, `(rnrs io simple)` etc. would map to new ASDF
  components the same way `ssig.scm`/`closed.pso` back R5RS now.

## R7RS

- New `r7rs-sig.scm` interface, `pseudoscheme/r7rs` system (also
  already stubbed).
- Easy wins, each basically a `builtin.scm` entry or a `defune` in
  `rts.lisp`: `(scheme char)`, `(scheme cxr)`, `(scheme inexact)` are
  almost entirely one-line CL wrappers, same pattern as the existing
  R5RS entries. `case-lambda` is a `derive.scm`-style macro.
  `define-record-type` could reuse `p-record.scm`'s
  `make-record-type`/`record-constructor` primitives under a new
  surface syntax.
- `library`/`import`, `parameterize`, bytevectors, and the `(scheme
  base)` split need real design work, same caveat as R6RS's
  library/condition system above.
- `tests/chibi/r7rs-tests.scm` is already in the repo as the eventual
  acceptance test, same role `r5rs-tests.scm` plays now.

## syntax-case

See `vendor/syntax-case/README-pseudoscheme.md` for current status
(core expander works; `define-syntax` doesn't yet) and the open
question of whether it becomes Pseudoscheme's `define-syntax` or an
independent library.

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
