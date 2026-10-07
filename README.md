# Pseudoscheme

Pseudoscheme 3.0 is Jonathan Rees' Scheme-to-Common-Lisp translator and
evaluator (versions up to 2.13 date from 1991-1994, targeting CMU CL,
Symbolics, VAX Lisp and LispWorks), brought up to date: it runs R5RS, R6RS and R7RS Scheme
on modern Common Lisp (developed on SBCL), as a library inside a Lisp
image or as a standalone `pseudoscheme` command.

Scheme is *compiled* to Lisp, not interpreted on top of it: R5RS, R6RS
and R7RS source is expanded by psyntax (the R6RS `syntax-case` expander)
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

Threads a program makes get 32 MB control stacks (the main thread has
256 MB), since SBCL takes time in proportion to make one;
`PSEUDOSCHEME_THREAD_STACK_SIZE` (in megabytes) changes that.

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
| R6RS | psyntax | Racket's R6RS suite: all 8902; all 25 library test programs run |
| R7RS-small | psyntax | chibi's R7RS suite: all 978 |
| R5RS | psyntax | chibi's R5RS suite: all 189 |

Continuations are full and re-entrant (docs/continuations.md);
`--continuations=escape` makes them escape-only, a little faster in code
that calls unknown procedures in loops.

Speed (`bench/`, ecraven's r7rs-benchmarks): all 57 run, at 1.37× Chez's
time as a geometric mean with full continuations (Guile: 2.8×). See
bench/RESULTS.md.

Real-world libraries (`tests/run-library-corpus.lisp`): 228 of 387 Akku
libraries and 91 of 130 snow-fort libraries load.

Every final SRFI that hasn't been withdrawn but SRFI 124 (ephemerons),
208 of them through SRFI 274, ships in `src/srfi/` or is built into the reader, the expander or
the command line (`(srfi 1)`, `(srfi :1 lists)` and `(srfi srfi-1)`
alike). `make test-srfi` runs their tests. See src/srfi/README.md for
where each comes from and what each leaves out.

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
src/continuations.lisp
                     -- full continuations (docs/continuations.md)
src/library-cache.lisp
                     -- the compiled-library cache
src/r6rs/            -- the R6RS standard libraries' procedures
src/r7rs/            -- R7RS-small: its procedures, and define-library
                        on psyntax (front.lisp, syntax.sls)
src/srfi/            -- SRFI libraries, mostly reference implementations
                        (see src/srfi/README.md)
src/compat/          -- (chezscheme) and (ikarus), for the Chez and
                        Ikarus variants of Akku packages
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
boot/                -- making the generated files from source, with
                        other Schemes (boot/README.md)
examples/            -- Scheme using Lisp, Lisp using Scheme, a mixed
                        ASDF system
bench/               -- ecraven's r7rs-benchmarks (vendored), a runner,
                        results
tests/               -- test runners and the suites they run (chibi's
                        R5RS/R7RS, Racket's R6RS)
docs/                -- interop (the bridge), libraries (Akku and
                        snow), continuations, and racket, guile and
                        chez (designs)
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
- `pseudoscheme/r6rs` -- psyntax, the R6RS libraries, `(chezscheme)`,
  full continuations and the compiled-library cache.
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
continuations in one. The patches, and the hooks added for compiled
libraries, are listed in
`vendor/psyntax/README-pseudoscheme.md`.

R7RS is on psyntax too. A `define-library` is translated into an R6RS
`library` form: `include` and `cond-expand` are handled, and integers in
names become `(srfi :1)`-style symbols, as Akku does. The `(scheme ...)`
libraries are generated from the R7RS export table. R5RS runs on
psyntax as well, at a top level whose bindings are `(pseudoscheme r5rs)`
(`(scheme r5rs)` plus string ports, `cond-expand` and `flush-output`), reading with case folding. One
expander serves every standard; the translator's native `syntax-rules`
remains only as its own bootstrap expander.

Libraries are found on a search path (`psx:*library-path*`, or `-L` on
the command line, or the colon-separated `PSEUDOSCHEME_LIBRARY_PATH`,
searched before `-L`'s): `(foo bar)` is `foo/bar.sls`, `.ss`, `.sld` or
`.scm`. Akku's layout also works:
- implementation variants such as `foo.chezscheme.sls` are tried after
  a generic file (`psx:*implementation-variants*`);
- `:1` is accepted in file names, and so are Akku's escapes for other
  characters: `(srfi :1)` is `srfi/%3a1.sls`, `let-optionals*` is
  `let-optionals%2a.sls`.

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
is all it takes; `--akku` finds the project's `.akku/lib`. docs/libraries.md
is a walkthrough of both, from the shell and from Lisp.
`tests/run-library-corpus.lisp DIR` imports every portable library in
such a tree and reports what loads.

Libraries loaded from files are compiled once and cached, in
`~/.cache/pseudoscheme/libraries/`; later imports load the compiled
code, unless the library or something it depends on has changed.
`PSEUDOSCHEME_LIBRARY_CACHE=0` turns that off, and
`PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY` puts the cache elsewhere
(src/library-cache.lisp).

To compile every bundled SRFI library into the cache ahead of time (359
libraries, about half a minute per continuation mode), run
`make precompile-srfi`, which does both modes, or
`bin/pseudoscheme --precompile-srfi` for one (`--continuations=escape`
before it for escape-only). `--precompile DIR` does the same for your own
libraries (`.sld` files) under `DIR`, and from Lisp it is
`(pseudoscheme-api:precompile-libraries :directory DIR)`. A library that
takes long to compile then loads at once (SRFI 148: 1.2 s to 0.14 s);
one that is quick to compile but large still takes the time to load.

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
is rebuilt with `(psx:rebuild)` or `make bootstrap-psyntax`, see
vendor/psyntax/README-pseudoscheme.md. ASDF compiles the image to a
fasl like any other source (the `psyntax-image` component), so booting
psyntax is quick; that fasl is a build product, not checked in, and a
boot falls back to the `.pp` while the fasl is older than it.

Both kinds of generated file can also be made from source alone, with
`make bootstrap`. First the translator's sources, running in Chibi,
Guile, Gauche, CHICKEN, Chez, Racket or Scheme 48, write the `.pso`
files (`SCHEME=guile` to choose; an s7 adapter is there, untested).
Then Pseudoscheme, loaded from those, builds psyntax's image from a
seed: one that Chez Scheme expands natively from psyntax's sources, or
(`SEED=stage0`) one that any R5RS or R7RS Scheme (Chibi, Scheme 48,
Gauche) builds through boot/stage0/. `make bootstrap-check` builds from
both seeds without installing anything and checks each result against
what's checked in. See boot/README.md.

## Tests

```sh
sbcl --script tests/run-r5rs-tests.lisp [--classic | --continuations=escape]
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-continuation-tests.lisp
sbcl --script tests/run-r7rs-tests.lisp [-v] [--continuations=escape]
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-r6rs-tests.lisp [-v] [--continuations=escape] [name ...]
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-library-tests.lisp
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-syntax-case-tests.lisp
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-library-corpus.lisp DIR
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-interop-tests.lisp
sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-srfi-system-tests.lisp
make -C contrib/cli test
make test-srfi                           # src/srfi/tests/, 121 programs
sh tests/run-program-tests.sh [bin/pseudoscheme [name ...]]   # needs Quicklisp
python3 tests/check-r7rs-exports.py      # export table vs. the R7RS PDF
```

`make test` runs the suites, `make test-escape` the standards' suites
with escape-only continuations, `make test-cli` the command line's. GitHub Actions
(`.github/workflows/ci.yml`) runs all of them, plus the bootstrap chain
from other Schemes (both kinds of generated file must come out as
checked in). The benchmarks, in both modes, are a workflow of their own
that runs by hand (`.github/workflows/bench.yml`: Actions, Benchmarks,
Run workflow).

`make test-programs` runs the programs in `tests/programs/`: Scheme and
Lisp using each other, with Common Lisp libraries from Quicklisp and C
libraries through CFFI (tests/programs/README.md). It needs Quicklisp,
which downloads the libraries the first time.

`tests/r6rs/` is Racket's R6RS test suite (MIT/Apache-2.0, see its
`LICENSE-racket.txt`); `tests/chibi/` has chibi-scheme's R5RS and R7RS
suites.

## Known gaps

The big ones; `ROADMAP.md` has the rest.

- **Continuations through Lisp code.** A continuation captured in a
  Scheme procedure that Lisp code called (through the bridge, or a Lisp
  primitive with no frame-aware version such as the sorts) can escape
  but not be re-entered: re-entering it is an error. Code compiled
  escape-only (`--continuations=escape`) isn't recorded in captured
  continuations at all (docs/continuations.md, "Further work").
- **Only libraries from files are cached.** Libraries defined at the
  REPL or in a program, and `(cl <package>)` libraries, are expanded
  and compiled again in every session, and so is whatever depends on
  them. Nothing cleans stale cache entries yet.
- **No Racket yet.** docs/racket.md plans a `--racket` mode that runs
  Racket's own expander on Pseudoscheme by compiling linklets.
- **No Chez mode yet.** `(chezscheme)` has what Akku's Chez variants
  need, 803 of Chez's 1715 names; docs/chez.md plans a `--chez` mode,
  the nearest of the three.
- **No Guile yet.** docs/guile.md plans a `--guile` mode that runs
  Guile's own boot-9 and libraries on libguile's primitives written in
  Lisp, and, much later, Guix's client side.
