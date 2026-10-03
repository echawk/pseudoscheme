# Pseudoscheme

Pseudoscheme is Jonathan Rees' Scheme-to-Common-Lisp translator and
evaluator (originally 1991-1994, targeting CMU CL, Symbolics, VAX Lisp
and LispWorks), brought up to date: it runs R5RS, R6RS and R7RS Scheme
on modern Common Lisp (developed on SBCL), as a library inside a Lisp
image or as a standalone `pseudoscheme` command.

Scheme is *compiled* to Lisp, not interpreted on top of it: R6RS source
is expanded by psyntax (the R6RS `syntax-case` expander) into a handful
of core forms, the translator turns those into Common Lisp, and the host
compiles that. Scheme procedures are Lisp functions, lists are Lisp
lists, strings are Lisp strings, and errors in either language are
conditions in both.

## Quick start

From the shell:

```sh
make -C contrib/cli                 # builds bin/pseudoscheme
bin/pseudoscheme                    # R7RS REPL (,q quits)
bin/pseudoscheme --r6rs prog.sps a b
bin/pseudoscheme -p '(exact-integer-sqrt 17)'
bin/pseudoscheme --help
```

From Lisp:

```lisp
(push #P"/path/to/pseudoscheme-asdf/" asdf:*central-registry*)
(asdf:load-system :pseudoscheme/api)

(r6rs:eval "(import (rnrs)) (display (list-sort < '(3 1 2)))")
(r6rs:eval '(let-values (((q r) (div-and-mod 17 5))) (list q r)))  ; => (3 2)
(r7rs:load "prog.scm")
(r6rs:repl)
```

`R5RS`, `R6RS` and `R7RS` are packages with `EVAL`, `LOAD`, `REPL` and
`TRUE-P` (see `src/api.lisp`). `docs/interop.md` describes where the
Lisp/Scheme bridge is going.

## Status

| | front end | tests |
|---|---|---|
| R6RS | psyntax | Racket's R6RS suite: 8689 pass, 213 fail; all 25 library test programs run |
| R7RS-small | library layer + native `syntax-rules` | chibi's R7RS suite: 868 of 924 |
| R5RS | native translator | chibi's R5RS suite: 183 of 188 |

All of `(rnrs ...)` is present. The R6RS libraries' namespaces and
syntax come from psyntax; the procedures behind them are in `src/r6rs/`
(records with inheritance, compound conditions, hashtables, bytevectors
with all the integer/float accessors, fixnums, flonums, bitwise, enums,
`(rnrs io ports)` with binary/textual/custom/transcoded ports, Unicode
case mapping and normalization, sorting, ...). `ROADMAP.md` has the
details, including what fails and why.

## Repository layout

```
pseudoscheme.asd     -- all ASDF systems
src/                 -- the implementation: .lisp (hand-written Lisp),
                        .scm (the self-hosted translator and other Scheme
                        source), .pso (that Scheme, pre-translated to
                        Lisp -- see "Bootstrap artifacts")
src/numbers.lisp     -- Scheme numerics on CL numbers: syntax, printing,
                        exactness, IEEE edge cases
src/psyntax.lisp     -- psyntax as the front end: host environment,
                        library search, entry points
src/r6rs/            -- the R6RS standard libraries' procedures
src/r7rs/, src/library.lisp
                     -- R7RS-small and its library layer
src/api.lisp         -- the R5RS / R6RS / R7RS packages for Lisp
vendor/psyntax/      -- Ghuloum & Dybvig's psyntax, patched, and the
                        image of it built on Pseudoscheme
vendor/syntax-case/  -- the 1992 Dybvig & Hieb syntax-case (superseded
                        by psyntax; see below)
contrib/cli/         -- the `pseudoscheme' command
tests/               -- test runners and the suites they run (chibi's
                        R5RS/R7RS, Racket's R6RS)
docs/                -- design notes: interop, continuations
```

## Systems

- `pseudoscheme/rts`, `pseudoscheme/translator`, `pseudoscheme/evaluator`
  -- the classic core: run-time, the self-hosted translator (loaded
  pre-translated), `scheme-eval`/`scheme-load`. `pseudoscheme` is all
  three.
- `pseudoscheme/reader` -- the Scheme reader/writer (`read.scm`,
  `write.scm`).
- `pseudoscheme/r5rs` -- the core plus the reader.
- `pseudoscheme/library` -- `define-library`/`import` for R7RS.
- `pseudoscheme/r7rs` -- R7RS-small.
- `pseudoscheme/r6rs` -- psyntax and the R6RS libraries.
- `pseudoscheme/api` -- the `R5RS`/`R6RS`/`R7RS` packages.
- `pseudoscheme-cli` (`contrib/cli/`) -- the command.
- `pseudoscheme/syntax-case` -- the old expander (superseded).
- `pseudoscheme/bootstrap` -- regenerates the `.pso` files.

## psyntax: the front end

`vendor/psyntax/` is Ghuloum and Dybvig's portable R6RS library and
`syntax-case` system (2007, MIT): the same expansion algorithm as
Chez Scheme's, with R6RS libraries, implicit phasing, identifier macros,
`datum->syntax`, `quasisyntax`, `define-record-type`, conditions, and
the rest of the R6RS syntax. It is written as R6RS libraries and runs
here *expanded into core Scheme*, as `psyntax-pseudoscheme.pp`, an image
that psyntax built of itself on Pseudoscheme (`(psx:rebuild)`; it
reproduces itself). Every R6RS library export that isn't syntax is a
reference to a host global of the same name, which is what `src/r6rs/`
provides.

The 2007 code predates the final R6RS in a few places and assumed full
continuations in one; the patches are listed in
`vendor/psyntax/README-pseudoscheme.md`.

Libraries are found on a search path (`psx:*library-path*`, or `-L` on
the command line): `(foo bar)` is `foo/bar.sls`, `.ss`, `.sld` or
`.scm`.

**Next**: R7RS onto psyntax too (its `define-library` is a different
surface for the same library system), so that one expander serves every
standard; the native `syntax-rules` then stays only as the translator's
own bootstrap expander. See `ROADMAP.md`.

## Symbols and case

Scheme symbols are CL symbols in the `SCHEME` package named by
*inverting* case, as CL's `:invert` readtable case does: `car` is
`SCHEME::CAR`, `Hello` is `SCHEME::|Hello|`, `ABC` is `SCHEME::|abc|`.
So Scheme is case-sensitive, as R6RS and R7RS require, while the
standard names are the upper-case CL symbols the translator and every
`.pso` file have always used, and Lisp code can write `'scheme::car`.
`#!fold-case` / `#!no-fold-case` switch the reader to R5RS-style
folding and back; `--r5rs` and `r5rs:eval` fold.

## Numbers

`src/numbers.lisp` makes CL's numeric tower behave as Scheme's: inexact
means `double-float` (CL's transcendental functions return single
floats for rational arguments); `(integer? 3.0)`, `(floor 3.5)` ⇒ `3.0`,
`(max 1 2.0)` ⇒ `2.0`; `(sqrt 16)` ⇒ `4` exact; comparisons that
handle infinities and NaN; and Scheme number syntax (`#x`, `#e`, `#i`,
ratios, any exponent marker, `+inf.0`, `-nan.0`, `-0.0`, `3+4i`,
`1@2`) parsed and printed by Scheme's rules rather than the CL reader's.
Float traps are turned off for IEEE behavior (`(/ 1. 0.)` ⇒ `+inf.0`)
by the command line program and the test runners, via
`ps:disable-float-traps`.

## Readers

- **The CL-reader bridge** (`ps:scheme-read-using-commonlisp-reader`)
  reads with the host's reader, so package-qualified symbols like
  `ps-lisp:setf` work. It's for the translator's own sources and is what
  `pseudoscheme/bootstrap` always uses.
- **The Scheme reader** (`pseudoscheme/reader`) is for Scheme: R6RS and
  R7RS syntax including `[ ]`, `#u8(...)`/`#vu8(...)`, `#'x` `` #`x ``
  `#,x` `#,@x`, `|symbols|`, `\x41;` escapes, `#\x41`, `#true`/`#false`,
  `#!fold-case`, `#;` and `#| |#`, Unicode identifiers, and the number
  syntax above. (`#'x` used to be a Pseudoscheme escape naming a CL
  function; it's R6RS's `(syntax x)` now, and Lisp access from Scheme is
  being redesigned, see docs/interop.md.)

## Bootstrap artifacts

The translator is Scheme, loaded pre-translated: the `.pso` file next to
each `.scm`. After changing a translator source, `builtin.scm`'s
integrations, or the reader/writer:

```lisp
(asdf:load-system :pseudoscheme/bootstrap)
(pseudoscheme-bootstrap:regenerate-pso-files)      ; or '("builtin") etc.
(pseudoscheme-bootstrap:regenerate-closed-pso)     ; after builtin.scm changes (fresh image)
(pseudoscheme-bootstrap:regenerate-reader-writer)  ; after read.scm / write.scm changes
```

Then clear the fasl cache (ASDF's timestamps have one-second
resolution), reload in a fresh image and run the tests. psyntax's image
is rebuilt with `(psx:rebuild)`, see vendor/psyntax/README-pseudoscheme.md.

## Tests

```sh
sbcl --script tests/run-r5rs-tests.lisp
sbcl --script tests/run-r7rs-tests.lisp [-v]
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-r6rs-tests.lisp [-v] [name ...]
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-library-tests.lisp
sbcl --control-stack-size 500MB --script tests/run-syntax-case-tests.lisp
make -C contrib/cli test
python3 tests/check-r7rs-exports.py      # export table vs. the R7RS PDF
```

`tests/r6rs/` is Racket's R6RS test suite (MIT/Apache-2.0, see its
`LICENSE-racket.txt`); `tests/chibi/` has chibi-scheme's R5RS and R7RS
suites.

## Known gaps

The big ones; `ROADMAP.md` has the rest.

- **Escape-only continuations.** `call/cc` is a Lisp `block`, so a
  continuation can't be re-entered after its `call/cc` returns. One R5RS
  test, a couple of R7RS ones. docs/continuations.md weighs CPS against
  the alternatives.
- **R7RS isn't on psyntax yet**, so R7RS macros use the native
  `syntax-rules` (no custom ellipsis, no `(... ...)`).
- **No compiled libraries.** Every run re-expands the library sources it
  imports.

## The 1992 syntax-case

`vendor/syntax-case/` and `pseudoscheme/syntax-case` are Dybvig & Hieb's
1992 expander, brought up earlier as an independent library
(`sc:sc-eval`). psyntax supersedes it, passing every test it passes
(`tests/run-syntax-case-tests.lisp` runs both), so it is kept only for
comparison and can be removed.
