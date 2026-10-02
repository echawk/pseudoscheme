# Pseudoscheme

Pseudoscheme is Jonathan Rees' Scheme-to-Common-Lisp translator and
evaluator (originally 1991-1994, targeting CMU CL, Symbolics, VAX Lisp
and LispWorks). Scheme source is classified, alpha-converted, and
translated into Common Lisp, which is then compiled or `eval`'d by the
host Lisp. It loads and runs cleanly under modern SBCL via ASDF.

## Loading

```lisp
(push #P"/path/to/pseudoscheme-asdf/" asdf:*central-registry*)
(asdf:load-system :pseudoscheme)
```

(or symlink/clone the repo under `~/common-lisp/` or `~/.local/share/common-lisp/source/`, or a `quicklisp/local-projects/`, if your ASDF source-registry already searches one of those.)

`ps:scheme-eval`, `ps:scheme-load`, and `ps:scheme-compile-file` are
the main entry points; `ps:scheme-user-environment` is the default
environment to evaluate or load Scheme code into.

## Repository layout

```
pseudoscheme.asd    -- all ASDF system definitions (repo root, like any
                        normal CL project -- see below for why the
                        components still live under src/)
src/                 -- all implementation: .lisp (hand-written Common
                        Lisp), .scm (the self-hosted translator and
                        other Scheme source), .pso (that Scheme source,
                        pre-translated to Common Lisp -- see
                        "Bootstrap artifacts" below)
src/library.lisp     -- libraries and top-level programs (R6RS ch. 7-8,
                        R7RS 5.1-5.6), shared by both
src/syntax-case.lisp -- glue loading the vendored syntax-case
src/r7rs/, src/r6rs/ -- the R7RS-small and R6RS front ends
vendor/syntax-case/  -- Dybvig & Hieb's syntax-case, plus a Pseudoscheme
                        port of its hooks (see "syntax-case" below)
tests/               -- test runners and the chibi test suites they run
README.md, ROADMAP.md, LICENSE
```

## System layout

- `pseudoscheme/rts` -- the core run-time (booleans, pairs, numeric
  tower primitives, the reader/writer bridge to the host Lisp reader).
- `pseudoscheme/translator` -- the self-hosted Scheme-to-CL translator
  itself (classifier, alpha-converter, `syntax-rules` expander, CL code
  generator). Written in Scheme (the `.scm` files below); loaded
  pre-translated (see **Bootstrap artifacts**).
- `pseudoscheme/evaluator` -- `scheme-eval`/`scheme-load`/REPL support
  built on the two systems above.
- `pseudoscheme` -- the above three, for convenience.
- `pseudoscheme/reader` -- a proper, self-hosted Scheme reader/writer
  (`read.scm`/`write.scm`, adapted from Scheme48's). Loading it
  switches `ps:*scheme-read*`/`*scheme-write*`/`*scheme-display*` away
  from the default CL-reader bridge to this one -- see **Which reader
  to use** below.
- `pseudoscheme/r5rs` -- the R5RS-conformant surface. The runtime
  structure built from `revised^4-scheme-interface` (`ssig.scm`)
  already includes R5RS's additions over R4RS (`values`,
  `dynamic-wind`, `eval`, string ports, ...), so this system is the
  core stack plus `pseudoscheme/reader`, under its R5RS name;
  `tests/run-r5rs-tests.lisp` is what actually gates the R5RS claim.
- `pseudoscheme/syntax-case` -- Dybvig & Hieb's `syntax-case`, loaded
  into the Scheme user environment (see **syntax-case**).
- `pseudoscheme/library` -- `define-library` / `library` / `import`
  and top-level programs, built on `module.scm`'s environments and
  structures (see **Libraries**).
- `pseudoscheme/r7rs` -- R7RS-small, built on the library layer.
  Skeleton: ~98% of the exports of the standard libraries are
  present (a handful are stubs that signal "not implemented"), and
  chibi's R7RS suite runs. See **R7RS**.
- `pseudoscheme/r6rs` -- R6RS `(rnrs base)`, `(rnrs syntax-case)` and
  a slice of `(rnrs exceptions)`/`(rnrs conditions)`, the `library`
  form and programs, expanded with `syntax-case`. A sketch. See **R6RS**.
- `pseudoscheme/bootstrap` -- regenerates the translator's own `.pso`
  bootstrap files (and `pseudoscheme/reader`'s); see below.

## Which reader to use

Pseudoscheme has two Scheme readers, for two different jobs:

- **The CL-reader bridge** (`ps:scheme-read-using-commonlisp-reader`,
  in `readwrite.lisp`) is the default, installed as part of
  `pseudoscheme/rts`. It reads Scheme source using the host's own
  Common Lisp reader, which means CL package-qualified symbols like
  `ps-lisp:setf` read correctly without any special escaping -- used
  throughout the translator's own sources (`builtin.scm`, `p-utils.scm`,
  `generate.scm`, ...). But it inherits CL's reader syntax exactly, so
  it can't parse the `...` ellipsis identifier (`syntax-rules` uses it
  everywhere; CL treats a run of dots as an error) or Scheme string
  escapes like `\n` (CL reads a backslash as "take the next character
  literally", not as introducing a control character). **This is the
  reader `pseudoscheme/bootstrap` always uses**, regardless of what else
  is loaded (see below) -- regenerating the translator's own `.pso`
  files depends on it.
- **The dedicated reader** (`pseudoscheme/reader`, `read.scm`/`write.scm`)
  is a real Scheme reader/writer and is the right choice for evaluating
  or loading ordinary R5RS/R6RS/R7RS Scheme source: `...`, standard
  string escapes, `#| |#` block comments, `#;` datum comments, and more
  named characters all work, and `write`/`display` show a symbol in the
  case it was actually written in (`'Martin` prints as `Martin`, not
  `MARTIN`) even though, like the rest of the system, identifiers are
  still folded to a canonical case for matching purposes (see
  `read.scm`'s comments for why). It does *not* handle CL
  package-qualified symbols -- `:` has no special meaning in R5RS/R7RS
  symbol syntax (e.g. `:::` is a valid ordinary identifier, used as a
  custom ellipsis marker in some R7RS code) and giving it CL semantics
  here would break that. `pseudoscheme/r5rs` loads this automatically.

Don't load `pseudoscheme/reader` in the same image where you're about
to call `pseudoscheme-bootstrap:regenerate-pso-files` and expect it to
retranslate the *translator's* sources correctly -- those need the
CL-reader bridge's package-qualification support. `regenerate-pso-files`
and `regenerate-closed-pso` both force the CL-reader bridge back on for
the duration of the call regardless of which reader is globally active,
so this is only a concern if you're reaching into the translator
manually rather than through `pseudoscheme-bootstrap`.

## Bootstrap artifacts (`.pso` files)

The translator (`alpha.scm`, `derive.scm`, `generate.scm`, etc., listed
in `translator.files`) is itself Scheme, and is normally loaded
pre-translated to Common Lisp -- that's what the `.pso` files next to
each `.scm` file are. This lets `pseudoscheme/translator` load without
already having a working Pseudoscheme to translate it with (the
classic bootstrapping problem). They're checked in deliberately, not
build output to delete.

Whenever a translator `.scm` file changes, its `.pso` is stale and
must be regenerated by asking the (already-loaded, working) translator
to translate itself again:

```lisp
(asdf:load-system :pseudoscheme)
(asdf:load-system :pseudoscheme/bootstrap)
(pseudoscheme-bootstrap:regenerate-pso-files)   ; retranslates every .scm in translator.files
```

`closed.pso` is different: it isn't translated from a `.scm` source at
all. It's synthesized by `write-closed-definitions` from whatever is
*currently* in `builtin.scm`'s integration table, as real top-level
function definitions for every procedure in
`revised^4-scheme-interface`. So it goes stale whenever `builtin.scm`
changes which CL function backs a Scheme procedure, or how its result
gets coerced to a Scheme boolean -- even though no line of
`closed.pso` itself changed. Regenerate it with:

```lisp
(pseudoscheme-bootstrap:regenerate-closed-pso)
```

`pseudoscheme/reader`'s own `read.pso`/`write.pso` aren't part of
`translator.files` either (they're ordinary runtime code, not part of
the translator), but they do need to be retranslated the same way
whenever `read.scm`/`write.scm` change:

```lisp
(pseudoscheme-bootstrap:regenerate-reader-writer)
```

**After regenerating any `.pso` file**, always:
1. Delete your SBCL fasl cache for this project
   (`~/.cache/common-lisp/sbcl-*/.../pseudoscheme-asdf/`) or wait a
   full second before reloading. ASDF compares source/fasl
   modification times with one-second resolution, so a regenerate
   immediately followed by a reload in rapid succession can be
   mistaken for "already up to date" and silently load the stale fasl.
2. Reload `:pseudoscheme` from a **fresh** Lisp image.
3. Re-run `tests/run-r5rs-tests.lisp` before trusting the result.

## Running the R5RS test suite

```sh
sbcl --script tests/run-r5rs-tests.lisp
```

This loads `pseudoscheme/r5rs` (which pulls in `pseudoscheme/reader`)
and runs chibi's `tests/chibi/r5rs-tests.scm` against it, one top-level
form at a time (so a single failing or erroring test doesn't abort the
run). As of this writing: **183 of 188 tests pass.** The remaining 5
are all genuine, individually-documented gaps (below), not reader
issues -- the whole file now reads correctly.

## Known gaps

- `symbol->string` now preserves case too (`(symbol->string 'Martin)`
  => `"Martin"`) by consulting the same original-spelling stash
  `write`/`display` use (`read.scm`'s `record-original-spelling!`).
  Getting this right required separating it from how the translator's
  *own* internals name the real CL symbols/packages they create for
  Scheme variables and structures -- those now call
  `ps-lisp:symbol-name` directly (`classify.scm`'s `name->string`,
  `module.scm`, `emit.scm`, `reify.scm`, `p-utils.scm`), rather than
  going through `symbol->string` and picking up its case preference by
  accident, which previously renamed every top-level `define` to its
  as-typed (sometimes lower-case) spelling instead of the canonical one
  the rest of the system expects, and broke variable lookup entirely.
- **Only one-shot (escape) continuations work.** `call-with-current-
  continuation` is implemented with a CL `block`/`return-from` (see
  `builtin.scm`), which can only unwind back out to an enclosing call
  -- it can't re-enter a `dynamic-wind` after that call has already
  returned, the classic "re-invoke a captured continuation later"
  pattern (1 known failure). This is an architectural property of how
  call/cc is implemented on top of Common Lisp here, not a quick fix.
- **`syntax-rules` doesn't support a custom ellipsis identifier**
  (R7RS's `(syntax-rules ::: () ...)` form, letting a macro use `:::`
  or another symbol instead of `...`) **or segment patterns with fixed
  trailing arguments** (R7RS's `(args ... penultimate ultimate)`,
  matching a variadic segment followed by required final arguments) --
  3 of the known failures. Both are `rules.scm` (the translator's own
  `syntax-rules` expander) feature gaps, not reader issues; fixing them
  means editing translator.files source, with the regenerate-and-retest
  workflow above.
- Obscure R5RS corner cases around `unquote`/`unquote-splicing`/`...`
  being shadowable as ordinary local variable names without disturbing
  their syntactic role in `quasiquote`/`syntax-rules` are not handled.

## syntax-case

`vendor/syntax-case/` is Dybvig & Hieb's reference `syntax-case`
(originally for Chez Scheme), with `hooks-pseudoscheme.ss` as the port
of its one implementation-dependent file and a few small additions.
`src/syntax-case.lisp` (system `pseudoscheme/syntax-case`) loads it
into the Scheme user environment and provides:

```lisp
(asdf:load-system :pseudoscheme/syntax-case)
(sc:sc-eval form)       ; expand FORM hygienically, then evaluate it
(sc:sc-load "file.scm") ; the same for every form of a file
(sc:sc-expand form)     ; expansion only: core forms out
```

It lives **alongside** Pseudoscheme's own `define-syntax`/`syntax-rules`
(the classifier-based expander of `rules.scm`), not in place of them:
macros defined through `sc-eval` are invisible to plain `scheme-eval`,
and vice versa, because syntax-case keeps its macros in one global
table (on symbol plists) while the native ones are nodes in
per-environment tables. `tests/run-syntax-case-tests.lisp` covers
hygiene both ways, `syntax-case` with fenders, `with-syntax`, and the
derived forms of `macro-defs.ss`.

What the 1992 expander does *not* do: no `x ... ...` (R7RS-style
nested-ellipsis flattening), no identifier macros (a macro keyword used
as a plain identifier is an error, so no `identifier-syntax`), no
`syntax-rules` patterns after an ellipsis, no custom ellipsis.

## Libraries

`src/library.lisp` implements R7RS `define-library` and R6RS `library`
with one mechanism. A library is a `program-env` (a CL package and a
name->binding table) plus an `interface` (its exports): a `structure`,
which is exactly what `module.scm` already provides. An import
*aliases binding nodes* into the importing environment, so an imported
variable is the same location (a `set!` in the exporter is seen by the
importer) and an imported syntactic keyword is the same macro. Import
sets (`only`, `except`, `prefix`, `rename`, and R6RS `for`/`library`/
version references) are list transformations on `(name . node)` pairs,
and a renamed export is an alias in a scratch export environment.
Top-level programs are an `import` form followed by forms evaluated in
order in a fresh environment.

Not done: R6RS phases (`for` levels are accepted and ignored),
per-library scoping of syntax-case macros, library versions beyond
"newest wins".

## R7RS

```lisp
(asdf:load-system :pseudoscheme/r7rs)
(r7rs:print-coverage)       ; what each standard library has, stubs, gaps
(psl:run-program forms)     ; forms = (import ...) followed by the program
```

`src/r7rs/exports.lisp` is Appendix A of the report, transcribed and
mechanically cross-checked against the PDF
(`python3 tests/check-r7rs-exports.py`). The implementation
environment starts as a copy of the R5RS bindings, adds the Common
Lisp primitives of `rts.lisp` (bytevectors, `floor/` and friends,
UTF-8, records, the exception machinery, parameters, process context,
time) and the Scheme of `base.scm` (derived syntax: `when`, `unless`,
`let-values`, `define-values`, `parameterize`, `case-lambda`,
`define-record-type`, `guard`, lazy evaluation with the report's
iterative `force`; plus the R7RS generalizations of `member`,
`string-copy`, `vector-fill!`, n-ary `string=?`, ...). `cond-expand`,
`include` and `syntax-error` are procedural macros, so they behave as
specified (they see the real feature list and libraries, can read a
file, and fail at expansion time). Anything in a standard library that
isn't implemented yet is a stub that signals a clear error when
*called*, so programs that import it still load.

`tests/run-r7rs-tests.lisp` runs chibi's R7RS suite
(`tests/chibi/r7rs-tests.scm`, with a small `(chibi test)` stand-in
beside it). It currently passes **775 of the 842 tests that get as far
as running**; a further 114 top-level forms can't even be read yet
(`#u8(...)`, `#!fold-case`, `3+4i`, `+inf.0`, datum labels, `\x41;`
escapes -- all reader work), and some others hit the gaps in
`ROADMAP.md`. `tests/run-library-tests.lisp` covers the library layer
and the R7RS and R6RS front ends directly (44 tests, all passing).

## R6RS

```lisp
(asdf:load-system :pseudoscheme/r6rs)
(r6rs:evaluate-library-form '(library (my lib) (export f) (import (rnrs base)) ...))
(r6rs:load-r6rs-program "prog.scm")  ; (library ...) forms then an (import ...) program
```

`(rnrs base)` is chapter 11 of the language report, transcribed and
cross-checked (`python3 tests/check-r6rs-exports.py`); the other
standard libraries are in a separate report that isn't in this
repository, so only `(rnrs syntax-case)` and the slices of
`(rnrs exceptions)`/`(rnrs conditions)` that `(rnrs base)` depends on
are provided (`src/r6rs/exports.lisp` lists the planned ones). Library
bodies are expanded with syntax-case, so the macros of `(rnrs base)`
are syntax-case macros; `div`/`mod`/`div0`/`mod0` follow the report's
table. Conditions are a minimal single-type stand-in, not R6RS's
compound conditions.

