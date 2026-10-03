# Pseudoscheme

Pseudoscheme is Jonathan Rees' Scheme-to-Common-Lisp translator and
evaluator (originally 1991-1994, targeting CMU CL, Symbolics, VAX Lisp
and LispWorks), brought up to date: it runs R5RS, R6RS and R7RS Scheme
on modern Common Lisp (developed on SBCL), as a library inside a Lisp
image or as a standalone `pseudoscheme` command.

Scheme is *compiled* to Lisp, not interpreted on top of it: R6RS and
R7RS source is expanded by psyntax (the R6RS `syntax-case` expander)
into a handful of core forms, the translator turns those into Common Lisp, and the host
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
(asdf:load-system :r7rs)                    ; or :r6rs, :r5rs: the same system

(r7rs:import (only (srfi 1) fold filter iota) (srfi 26))
(filter #'evenp (iota 10))                  ; => (0 2 4 6 8)
(mapcar (cut * 2 <>) '(1 2 3))              ; a Scheme macro, in Lisp

(r7rs:define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(mapcar #'fact '(1 2 3 4))                  ; => (1 2 6 24)

(r7rs:eval "(import (scheme base)) (exact-integer-sqrt 17)")
(r7rs:load "prog.scm")
(r7rs:repl)
```

And Scheme using Lisp: a Lisp package is a library, macros included.

```scheme
(import (scheme base)
        (prefix (cl common-lisp) cl:)
        (prefix (cl ironclad) ic:)          ; loaded on demand (ASDF/Quicklisp)
        (pseudoscheme lisp))

(cl:sort (list '(b . 2) '(a . 1)) cl:< #:key cl:cdr)
(cl:loop for x in '(1 2 3 4) when (even? x) collect (* x x))
(ic:byte-array-to-hex-string (ic:digest-sequence #:sha256 (string->utf8 "abc")))
(lisp-let ((cl:*print-base* 16)) (cl:princ-to-string 255))   ; => "FF"
```

The packages `R5RS`, `R6RS` and `R7RS` each have `IMPORT` (not R5RS),
`DEFINE`, `EVAL`, `SCHEME`, `LOAD`, `REPL`, `EXPAND`, `TRANSLATE`,
`PROCEDURE` and more. Scheme sources can be ASDF components
(`:r7rs-library`, `:r7rs-file`, ...). See `docs/interop.md` and
`examples/`.

## Status

| | front end | tests |
|---|---|---|
| R6RS | psyntax | Racket's R6RS suite: 8690 pass, 212 fail; all 25 library test programs run |
| R7RS-small | psyntax | chibi's R7RS suite: 948 of 975 |
| R5RS | native translator | chibi's R5RS suite: 183 of 188 |

Speed (`bench/`, ecraven's r7rs-benchmarks): all 57 run, at 3.0× Chez's
time as a geometric mean, close to Guile's 2.8×. See bench/RESULTS.md.

Real-world libraries (`tests/run-library-corpus.lisp`): 228 of 387 Akku
libraries and 91 of 130 snow-fort libraries load. SRFIs 1, 2, 6, 8, 9,
11, 13, 14, 16, 19, 23, 26, 27, 28, 31, 39, 41, 43, 45, 61, 64, 69, 87,
98, 111, 125, 128, 130, 132, 133, 141, 143, 145, 151 and 158 ship in
`src/srfi/`.

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
src/r7rs/            -- R7RS-small: its procedures, and define-library
                        on psyntax (front.lisp, syntax.sls)
src/srfi/            -- SRFI libraries, mostly reference implementations
                        (see src/srfi/README.md)
src/compat/          -- (chezscheme), for the Chez variants of Akku
                        packages
src/environments.lisp
                     -- native environments the R5RS/R7RS layers build on
src/api.lisp         -- the R5RS / R6RS / R7RS packages for Lisp
src/interop.lisp, src/interop/
                     -- the Scheme <-> Lisp bridge: (cl <package>)
                        libraries, (pseudoscheme lisp), use-library
src/asdf.lisp        -- Scheme sources as ASDF components
vendor/psyntax/      -- Ghuloum & Dybvig's psyntax, patched, and the
                        image of it built on Pseudoscheme
contrib/cli/         -- the `pseudoscheme' command
examples/            -- Scheme using Lisp, Lisp using Scheme, a mixed
                        ASDF system
bench/               -- ecraven's r7rs-benchmarks (vendored), a runner,
                        results
tests/               -- test runners and the suites they run (chibi's
                        R5RS/R7RS, Racket's R6RS)
docs/                -- interop (the bridge), continuations (design)
```

## Systems

- `pseudoscheme/rts`, `pseudoscheme/translator`, `pseudoscheme/evaluator`
  -- the classic core: run-time, the self-hosted translator (loaded
  pre-translated), `scheme-eval`/`scheme-load`. `pseudoscheme` is all
  three.
- `pseudoscheme/reader` -- the Scheme reader/writer (`read.scm`,
  `write.scm`).
- `pseudoscheme/r5rs` -- the core plus the reader.
- `pseudoscheme/environments` -- native environments for the R5RS and
  R7RS implementation layers.
- `pseudoscheme/r7rs-runtime` -- the procedures behind R7RS-small.
- `pseudoscheme/r6rs` -- psyntax, the R6RS libraries and `(chezscheme)`.
- `pseudoscheme/r7rs` -- R7RS-small on psyntax.
- `pseudoscheme/api` -- the `R5RS`/`R6RS`/`R7RS` packages and the
  Scheme/Lisp bridge. The systems `r5rs`, `r6rs` and `r7rs` are
  shorthands for it.
- `pseudoscheme/asdf` -- ASDF component types for Scheme sources.
- `pseudoscheme-cli` (`contrib/cli/`) -- the command.
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

R7RS is on psyntax too. A `define-library` is translated into an R6RS
`library` form: `include` and `cond-expand` are handled, and integers in
names become `(srfi :1)`-style symbols, as Akku does. The `(scheme ...)`
libraries are generated from the R7RS export table. One expander serves
every standard; the native `syntax-rules` remains as the translator's
own bootstrap expander and as R5RS mode's.

Libraries are found on a search path (`psx:*library-path*`, or `-L` on
the command line): `(foo bar)` is `foo/bar.sls`, `.ss`, `.sld` or
`.scm`. Akku's layout also works:
- implementation variants such as `foo.chezscheme.sls` are tried after
  a generic file (`psx:*implementation-variants*`);
- `:1` and `%3a1` are accepted in file names.

`src/srfi/` comes after the user's path, so `(srfi 1)`, `(srfi :1 lists)`
and the R7RS-large names like `(scheme list)` resolve with no setup.

R7RS systems such as chibi and Gauche let a library define a name it also
imports, shadowing the import, and real libraries (irregex, SSAX) rely on
that. psyntax therefore allows it too, while a name defined twice in one
library is still an error. An imported library's body runs on import,
not only when one of its variables is used.

## Loading real libraries: Akku and snow

Scheme has no ASDF. The two package managers that matter are:
- **Akku**: R6RS and R7RS, mirrors snow-fort, installs into a project's
  `.akku/lib`;
- **snow-fort**: R7RS, used through `snow-chibi`.

Both lay files out the way the search path expects, so pointing `-L` (or
`psx:*library-path*`) at `.akku/lib`, or at a snow install directory,
is all it takes. `tests/run-library-corpus.lisp DIR` imports every
portable library in such a tree and reports what loads.

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
  `#!fold-case`, `#;` and `#| |#`, Unicode identifiers, the number
  syntax above, and `#:name` for Lisp keywords. (`#'x` used to be a
  Pseudoscheme escape naming a CL function; it's R6RS's `(syntax x)`
  now, and Lisp is reached through `(cl <package>)` libraries, see
  docs/interop.md.)

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
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-syntax-case-tests.lisp
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-library-corpus.lisp DIR
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-interop-tests.lisp
make -C contrib/cli test
python3 tests/check-r7rs-exports.py      # export table vs. the R7RS PDF
```

`tests/r6rs/` is Racket's R6RS test suite (MIT/Apache-2.0, see its
`LICENSE-racket.txt`); `tests/chibi/` has chibi-scheme's R5RS and R7RS
suites.

## Known gaps

The big ones; `ROADMAP.md` has the rest.

- **Escape-only continuations.** `call/cc` is a Lisp `catch`, so a
  continuation can't be re-entered after its `call/cc` returns; trying
  signals an error. This affects one R5RS test and a couple of R7RS
  ones. docs/continuations.md weighs CPS against the alternatives.
- **No compiled libraries.** Every run re-expands the library sources it
  imports.
