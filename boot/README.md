# Bootstrapping from another Scheme

The translator is written in Scheme and normally loads pre-translated,
as the `.pso` files in `src/`. Those files are generated, so building
Pseudoscheme shouldn't depend on having them already. `boot/` produces
them from the `.scm` sources with any of several Schemes, without
Pseudoscheme:

```sh
make bootstrap                 # the first Scheme found
make bootstrap SCHEME=guile    # a particular one
make bootstrap-all             # every one installed; they must agree
make bootstrap-check           # don't install; compare with src/'s translator
```

or `boot/bootstrap.sh [--all] [--no-install] [--check]` directly. Each
Scheme writes to `boot/build/<scheme>/`, logging to
`boot/build/<scheme>.log`. Unless you pass `--no-install`, the files are
then copied into `src/`:

| file | from |
|---|---|
| `closed.pso` | `builtin.scm`'s integrations, as function definitions |
| `read.pso`, `write.pso` | `read.scm`, `write.scm` |
| `<f>.pso` for each `f` in `translator.files` | `<f>.scm` |
| `spack.lisp` | the translator's `DEFPACKAGE`s |

These are what `src/bootit.scm` (and `pseudoscheme/bootstrap`) produce
under Common Lisp.

## Schemes

| `SCHEME=` | runs | status |
|---|---|---|
| `chibi` | `chibi-scheme boot/hosts/chibi.scm` | works |
| `guile` | `guile --no-auto-compile -s boot/hosts/guile.scm` | works |
| `gauche` | `gosh boot/hosts/gauche.scm` | works |
| `chicken` | `csi -s boot/hosts/chicken.scm` | works |
| `chez` | `chez --script boot/hosts/chez.scm` | works |
| `racket` | `plt-r5rs boot/hosts/racket.scm` | works |
| `scheme48` | `scheme48 < boot/hosts/scheme48.scm` | works |
| `s7` | `s7 boot/hosts/s7.scm` | untested |

All the Schemes marked "works" write byte-identical files, apart from
the header comment naming the Scheme. Those files are also the same
forms, read by the CL reader, that Pseudoscheme's own translator
writes (`boot/check.lisp`).
Built from them, the translator reproduces them exactly: the bootstrap
is a fixpoint.

To add a Scheme, write `boot/hosts/<name>.scm` and add a line to
`host_command` in `bootstrap.sh`. An adapter defines:

- `boot:host-name`, a string for the header comment;
- `(boot:eval form)`, which evaluates at top level;
- an opaque record type: `(boot:make-record rtd serial fields)`,
  `boot:record?`, `boot:record-rtd`, `boot:record-serial`,
  `boot:record-fields` (SRFI 9 or R6RS `define-record-type`). It
  mustn't be a pair, vector or procedure: the translator looks at those.

Then it loads `base.scm`, `reader.scm`, `printer.scm`, `runtime.scm`
and `bootstrap.scm`, in that order, from the repository root. Apart
from the adapters, `boot/` is plain R5RS.

## How it works

The translator is mostly portable Scheme. Its dependencies on Common
Lisp are concentrated in `p-record.scm` and `p-utils.scm` (records,
tables, fluids, packages and the pretty printer). `runtime.scm` replaces
those two, and the other files of `translator.files` load unchanged.
What the translator actually works with is CL symbols. It reads its input
as symbols in the `SCHEME` package and builds CL code out of
`PS-LISP:SETQ`, `SCHEME::CAR`, keywords and symbols it interns in the
packages it makes. So `boot/` models enough of CL to cover that:

- **Symbols and packages** (`base.scm`). Every CL symbol is a host
  symbol, so `eq?`, `memq`, `assq` and `case` in the translator keep
  working. Its spelling encodes its package, and its name with the case
  inverted, as Pseudoscheme's `symbol->string` does: `SCHEME::FOO` is
  `foo`, `PS:CAR` is `ps:car`, `:TEST` is `:test`, and
  `SCHEME-TRANSLATOR::FOO` is `scheme-translator::foo`. Packages, with
  use lists, present and external symbols, are modeled as much as
  `intern`, `find-symbol`, `import` and `export` need. `PS`'s exports
  come from `src/pack.lisp`.
- **The reader** (`reader.scm`). This reads source as the CL reader
  does under Pseudoscheme. It folds case, reads `pkg:name`, `#'x`
  (`(function x)`), CL strings and character names, and reads
  `ps-lisp:nil`, `ps-lisp:t` and `ps:false` as `()`, `#t` and `#f`,
  which they are under Pseudoscheme. It doesn't use the host's `read`,
  which differs between hosts and doesn't fold case.
- **The printer** (`printer.scm`). This prints CL syntax in a target
  package, leaving off the prefix for accessible symbols. The layout is
  its own, not SBCL's pretty printer's, so text differs from
  Lisp-generated files but the forms don't.
- **Evaluation order** (`runtime.scm`, `boot:host-form`). Some of the
  translator's side effects show in its output, such as allocating
  names and collecting `SPECIAL` declarations. Under CL they happen left
  to right. R5RS leaves argument, `let` and `map` order unspecified, and
  Chez and Chibi go right to left. So when the translator is loaded,
  calls and `let`s evaluate their operands into temporaries in order,
  quasiquote is expanded into calls, and `map` is a left-to-right one.

## Checking

```sh
make bootstrap-check
# or, for results already in boot/build:
sbcl --script boot/check.lisp boot/build/guile boot/build/chez
```

`check.lisp` loads Pseudoscheme from `src/` and has its translator
translate the same files into `boot/build/sbcl/`. It then compares each
file with the bootstrapped one, form by form. A mismatch can mean that
`src/*.pso` is stale, so its translator no longer matches the `.scm`
sources, rather than that the bootstrap is wrong. After
`make bootstrap`, start a fresh image so that the new `.pso` files load
(clear the fasl cache if ASDF doesn't notice them). From then on,
`make bootstrap-check` must report no differences.

## Not covered yet

- `vendor/psyntax/psyntax-pseudoscheme.pp`, psyntax's expanded image, is
  generated as well: by psyntax running on Pseudoscheme, seeded with
  `pre-built/psyntax-scheme48.pp`. psyntax is portable R6RS, so it can
  be built the same way. See `vendor/psyntax/README-pseudoscheme.md`.
