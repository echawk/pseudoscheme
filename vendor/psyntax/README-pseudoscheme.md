# psyntax under Pseudoscheme

This directory is Abdulaziz Ghuloum and Kent Dybvig's portable R6RS
library and `syntax-case` system ("psyntax", 2007, MIT license; see
`README.txt`). It came from the backup repository
<https://github.com/xingzheone/psyntax-r6rs-backup> (commit
`37731b04c26b`), a copy of the original
<https://scheme.com/syntax-case/r6rs-libraries/>.

It is Pseudoscheme's front end for R6RS: every form is expanded by
psyntax into core Scheme (`lambda`, `if`, `set!`, `define`, `quote`,
`begin`, `letrec`, calls), which the translator compiles to Common Lisp.
See `src/psyntax.lisp` for the host side.

## Files

| file | what |
|---|---|
| `psyntax/*.ss` | the expander, as R6RS libraries (patched, see below) |
| `psyntax-buildscript.ss` | expands those sources into a single image (patched) |
| `psyntax-pseudoscheme.pp` | **that image, built on Pseudoscheme**: what `(psx::boot)` loads, compiled by ASDF to a fasl (the `psyntax-image` component) |
| `pre-built/psyntax-scheme48.pp` | the original Scheme48 image (no longer usable as a seed, see below) |
| `scheme48.r6rs.ss` | the original Scheme48 adapter, for reference |

Only the Scheme48 image was kept of the original eleven pre-built ones;
it's plain R5RS, which is what the translator eats.

## Rebuilding

After changing anything under `psyntax/` or the build script, from the
repository root:

```sh
make bootstrap-psyntax
```

This builds an image from the sources with Chez Scheme, which runs
the build script natively. Pseudoscheme then rebuilds with it until the
image reproduces itself, and the result is installed as
`psyntax-pseudoscheme.pp`; check it in. No pre-built image is involved.
See boot/README.md. From Lisp, without Chez:

```lisp
(asdf:load-system :pseudoscheme/r6rs)
(psx:rebuild)              ; expands the sources with the current image
```

Run SBCL with a large control stack (`--control-stack-size 500MB`):
the expander recurses deeply. A rebuild takes a few seconds. It names
the image's gensyms `g$1`, `g$2`, ... in order of appearance, so a
rebuild from a rebuilt image reproduces it byte for byte.

`pre-built/psyntax-scheme48.pp` is the original upstream image. The
sources have outgrown it, so `(psx:rebuild :seed t)` no longer works:
`compat.ss` imports `lisp-keyword?`, which its `$bootstrap` lacks.

## Patches

Each is marked `PSEUDOSCHEME:` in the source.

**Host integration**

* `psyntax/expander.ss` excepts from `(rnrs)` only names it exports
  (`environment`, `eval` and `null-environment` are `(rnrs eval)`'s and
  `(rnrs r5rs)`'s), as R6RS requires and Chez enforces, so that Chez can
  load the sources natively (boot/README.md). The output doesn't change.

* `psyntax/main.ss` replaced: instead of running a script named on the
  command line and exiting, it hands the expander's entry points to the
  host as globals (`psyntax:eval-r6rs-top-level`,
  `psyntax:library-expander`, `psyntax:eval-top-level`, ...).
* `psyntax/library-manager.ss` exports `library-path`, `file-locator`
  and `library-locator`, so the host can search a library path for
  `.sls`, `.ss`, `.sld` and `.scm` files, and translate R7RS
  `define-library` forms (src/r7rs/front.lisp).
* The REPL's "all public bindings" library is `(pseudoscheme)` rather
  than `(ikarus)` (expander and build script).
* Build-script table: the `&foo-rtd` / `&foo-rcd` identifiers behind
  the `$core-rtd` bindings go to `$all` (the original leaves this to each
  port), and `$delay`, which `delay` now expands into.

**Escape-only continuations**

* `guard` re-entered continuations (it called the guard's continuation
  from a thunk invoked after that continuation's `call/cc` returned, and
  re-raised by jumping back into the handler). Rewritten to escape with
  a thunk and re-raise with `raise-continuable` from the guard's own
  context. See docs/continuations.md.

**Conformance with the final R6RS** (the 2007 code predates it)

* Default record mutator names are `<record>-<field>-set!`, not
  `set-<record>-<field>!` (library report 6.2).
* `let-values` accepts any formals, `((a b . c) e)`, `(a e)` (11.4.6).
* `(for <import set> <level> ...)` imports (7.1); with implicit phasing
  the levels are ignored.
* `(unsyntax e ...)` / `(unsyntax-splicing e ...)` with several operands
  (11.19).
* `assert` raises `&assertion` and returns the expression's value
  (11.14); it raised `&error` and returned unspecified.
* Version references: a bug compared sub-version references against the
  *spec* instead of the version being tested.
* The body of `with-syntax` is a body, `(let () ...)` (11.19), so it
  may contain definitions; it was a `begin`.
* `(define id)` with no expression (11.2.1) gives `id` an unspecified
  value; it was "not supported yet".
* Bytevectors are literals (11.4.1), and so are vectors (R7RS 4.1.2;
  most R6RS systems accept them).
* Three identifiers missing from the build script's table:
  `bytevector-ieee-single-set!`, `bytevector-ieee-double-set!`,
  `i/o-error-position`. (Found by comparing the table against the entries of
  the errata-corrected library report; nothing else is missing.)
* Truncated condition-type names in the table, `&non` and
  `&implementation`, and a typo, `&i/o-fie-is-read-only-rcd`.

**REPL**

* `set!` of a variable defined at the REPL is allowed (it was "cannot
  modify imported identifier").
* `let-syntax` / `letrec-syntax` at the REPL top level work as
  expressions (it was "not supported yet").

**R7RS and real-world libraries**

* Library bodies may interleave definitions and expressions, and keep a
  trailing expression's value (`chi-library-internal`).
* REPL `import`, in the extensible interaction library (parameters
  `interaction-library-name` / `interaction-source-name`).
* Custom ellipsis, `(syntax-rules ::: (lit ...) rule ...)`, by
  rewriting the rules; inside vector patterns too (`replace-ellipsis`).
  An ordinary `...` in such rules stays `...` in the output.
* `_` and the ellipsis may be `syntax-rules` literals (R7RS 4.3.2); an
  ellipsis literal is escaped in the templates (`escape-ellipsis`), and
  `syntax-case` accepts `...` among its literals for this. The
  keyword's position in a `syntax-rules` pattern is ignored even when
  `_` is a literal.
* `let*-values` as a macro.
* `let-syntax` / `letrec-syntax` bindings are scoped by their own rib,
  so imports in a macro's output can't shadow them.
* A definition may shadow an *import* (as chibi and Gauche allow;
  irregex and SSAX rely on it); defining a name twice is still an
  error (`extend-rib!`).
* Every imported library is invoked, not only those whose variables are
  referenced: R7RS libraries may rely on their imports' side effects.
* A library defined again replaces the old one (at the REPL, or when an
  ASDF system is reloaded), as in Chez; libraries already expanded keep
  the old one (`install-library`).
* `set!` of an exported variable inside its library updates the
  exported location too, so importers see the new value. psyntax copied
  each variable to its location once, at the end of initialization
  (`library-export-locs`, `chi-set!`).

**The Lisp bridge** (docs/interop.md)

* Lisp keywords (`#:name`) are self-evaluating constants, not
  identifiers (`id?`, `self-evaluating?`, the host primitive
  `lisp-keyword?`, added to the build-script table as `$boot`).
* So are the host's other literals that aren't R6RS datatypes, SRFI 4
  vectors such as `#s16(1 2 3)` (the host primitive `host-literal?`).
* `environment-symbols`, Ikarus's list of the names an environment
  binds, and `environment?`, as `psyntax:` entry points for `(ikarus)`
  (src/compat/ikarus.scm).
* `psyntax:library-export-bindings`, an entry point listing a
  library's exports with their binding types and locations, for
  `use-library`.

**Elsewhere**

* `delay` expands into `($delay thunk)`, building an R7RS promise, so
  there's one promise type for `delay`, `delay-force`, `make-promise`
  and `force`.
* `(file-options ...)` is the checked list of option symbols
  (`compat.ss`'s `file-options-spec` was "not implemented").
* For compiled libraries (src/library-cache.lisp): a `library-loader`
  parameter, tried in `find-external-library` before the library is
  looked for as source, and a `library-expanded-hook` that
  `library-expander` calls with everything `install-library` needs and
  thunks for the visit and invoke code (thunks because converting the
  code looks up primitives, which a bootstrap's table may lack).
  `psyntax:library-spec-by-name` finds or loads a library and returns
  its (id name version).
* Marks are gensyms, not fresh one-character strings, so that, like
  labels, they keep their identity across a write and a read: the first
  step towards serializing expanded libraries (ROADMAP.md, 2).

## Known gaps

* Ellipses inside *vector templates*, `#(x ...)`, aren't expanded
  (`gen-syntax` only handles vectors that aren't syntax objects).
* Phases are implicit (Ghuloum & Dybvig's "implicit phasing"), so
  `for` levels are accepted and ignored.
