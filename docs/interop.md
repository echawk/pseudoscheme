# Scheme and Common Lisp, both ways: a design

Status: proposal. Pieces marked **(done)** exist; everything else is a
plan to be argued with.

The goal is the one in the original Pseudoscheme paper, taken further:
any Scheme program or library (R5RS, R6RS, R7RS) loads and runs inside a
Common Lisp image, is callable from Lisp as ordinary Lisp, and can itself
use Lisp libraries. Pseudoscheme is unusually well placed for this,
because it doesn't *embed* Scheme in Lisp: it *compiles* Scheme to Lisp,
so most of the bridge is just "use the same objects".

## 1. What is already shared

| Scheme | Lisp | notes |
|---|---|---|
| pair, list | cons, list | `'()` **is** `NIL` |
| procedure | function | callable with `funcall`, no wrapper |
| number | number | full tower; inexact = `double-float` (done) |
| string, char | simple-string, character | Unicode |
| vector | simple-vector | |
| bytevector | `(simple-array (unsigned-byte 8) (*))` | (done) |
| symbol | symbol in `SCHEME` | name is case-inverted (done, see below) |
| port | stream | |
| record | `struct` instance | R6RS record types (done) |
| condition | condition ↔ R6RS condition | Lisp errors become `&assertion`/`&i/o` conditions inside Scheme handlers (done) |
| multiple values | multiple values | |
| `dynamic-wind` | `unwind-protect` | Lisp non-local exits run Scheme `after` thunks |

**Symbols (done).** Scheme symbols are CL symbols in the `SCHEME`
package whose names are the Scheme names with case *inverted*, as CL's
`:invert` readtable case does: `car` ↔ `SCHEME::CAR`, `Hello` ↔
`|Hello|`, `ABC` ↔ `|abc|`. Scheme is case-sensitive (as R6RS/R7RS
require) and a Lisp programmer can still write `'scheme::car`. This is
what lets `(r6rs:eval '(list-sort < (list 3 1 2)))` work when written in
Lisp: `CL-USER::LIST-SORT` maps to Scheme's `list-sort`.

The one real mismatch is **booleans**: `#f` is the symbol `PS:FALSE`,
`#t` is `T`, and `NIL` is the empty list. Every other choice is worse:

* `#f` = `NIL` with a distinct empty-list object breaks the most
  valuable sharing, Scheme lists *being* Lisp lists.
* `#f` = `'()` = `NIL` (Pseudoscheme's old optional mode) is not Scheme:
  `(if '() 'yes 'no)` must be `yes` in R6RS and R7RS.

So booleans are converted at the boundary, and the design below tries to
put that conversion where it can't be forgotten.

## 2. Lisp calling Scheme

### 2.1 Evaluate and load (done)

```lisp
(r7rs:load "prog.scm")
(r6rs:eval "(import (rnrs)) (display (fold-left + 0 '(1 2 3)))")
(r6rs:eval '(let-values (((q r) (div-and-mod 17 5))) (list q r)))
(r6rs:repl)
```

Strings are read by the Scheme reader (so `#f`, `#(...)`, `#\a` are
exact); Lisp data have their symbols moved into `SCHEME`. Results are the
shared objects above; test truth with `r6rs:true-p`.

### 2.2 Libraries as packages

A Scheme library should be usable from Lisp as a Lisp package:

```lisp
(pseudoscheme:use-library '(srfi 1) :package "SRFI-1")
(srfi-1:fold #'+ 0 '(1 2 3))
(srfi-1:any (lambda (x) (r6rs:true-p (scheme-even? x))) ...)
```

Each exported procedure becomes a function binding of a symbol in the
package (its name the export's name, case inverted back: `fold` →
`SRFI-1:FOLD`); exported variables become symbol macros so reads see the
current value. Since psyntax stores every library variable in a host
global, the function binding can simply *be* that global's value, so
there is no per-call cost.

Booleans: exported procedures whose names end in `?` get a second
binding with the conventional Lisp `-P` name that returns a Lisp boolean
(`null?` → `NULL?` raw, `NULL-P` converted). That is the one place a
naming convention can carry a type.

### 2.3 Scheme source in ASDF systems

The most useful form of "load arbitrary Scheme": Scheme files as
components of ordinary ASDF systems, compiled ahead of time to fasls.

```lisp
(defsystem :my-app
  :defsystem-depends-on (:pseudoscheme/asdf)
  :components ((:r7rs-library "lib/json")      ; lib/json.sld
               (:r6rs-program "tools/report")  ; tools/report.sps
               (:file "main")))                ; Lisp using both
```

Compiling a library runs psyntax over it, translates the core output to
Lisp and `compile-file`s that. The hard part is that a compiled library
must be re-installable without re-expanding: the fasl has to carry
psyntax's library descriptor (export substitution, environment, visit
and invoke code). The 2007 psyntax doesn't serialize libraries; Ikarus's
later version does, and that code is the model. This is also what makes
startup fast for large programs, so it's worth doing early.

## 3. Scheme calling Lisp

### 3.1 Lisp packages as libraries

The natural Scheme-side notation is the one Scheme already has for
namespaces: libraries. A virtual library `(cl <package>)` exports every
external symbol of a Lisp package:

```scheme
(import (rnrs)
        (prefix (cl alexandria) alex:)
        (only (cl common-lisp) format *print-base*))

(alex:flatten '((1 2) (3 (4))))
(format #t "~a~%" 42)
```

Why this shape:

* It is **explicit and hygienic**. `:` stays an ordinary Scheme
  identifier character (R6RS code writes `foo:bar` after
  `(prefix (lib) foo:)`; the old dedicated-reader convention of reading
  `pkg:sym` as a CL symbol broke that), and a macro that uses a Lisp
  function carries its binding with it like any other import.
* It **costs nothing at run time**. A `(cl ...)` binding is a psyntax
  `core-prim` whose name is the Lisp symbol itself, and the translator
  turns a reference to a package-qualified symbol into a direct call:
  `(alex:flatten x)` compiles to `(alexandria:flatten x)`.
* `only`, `except`, `rename` and `prefix` all work unchanged.

Implementation: psyntax's library locator (`psx:locate-library-file`
today) gets a second hook that recognizes `(cl <name>)` and installs a
synthesized library (`install-library` with a substitution built from
`do-external-symbols`). Lisp special variables are exported as
variables; Lisp macros are not exported (see 3.4).

The old `#'cl-function` escape in the dedicated reader is gone: R6RS
needs `#'x` to mean `(syntax x)` (done).

### 3.2 Keywords

Lisp keyword arguments need Lisp keywords, and `:test` read by a Scheme
reader is a Scheme symbol. Proposal: the reader reads `#:test` as the
keyword `:TEST` (Guile and Racket use similar `#:` syntax for their own
keywords; neither R6RS nor R7RS gives `#:` a meaning):

```scheme
(cl:sort (list 3 1 2) cl:< #:key cl:identity)
```

### 3.3 Booleans coming back

`(if (cl:evenp 3) ...)` is the trap: `evenp` returns `NIL`, which Scheme
reads as `'()`, which is *true*. Options, in order of preference:

1. Exports of `(cl ...)` libraries whose names end in `p`/`-p`, plus the
   ANSI standard's predicates that don't follow the convention (`eq`,
   `equal`, `string=`, `typep`, ...), are wrapped to return `#t`/`#f`.
   The rest come back raw.
2. `(pseudoscheme cl)` exports `cl-true?` / `cl-false?` for explicit
   conversion, and `cl-if` as syntax.
3. A per-import override: `(cl alexandria (predicates emptyp))`.

### 3.4 Lisp macros

A Lisp macro can't be a Scheme macro: its subforms would be Lisp, not
Scheme. Two partial answers, both later:

* Macros whose arguments are all *expressions* (`incf`, `when`,
  `with-open-file`'s body...) could be imported as "foreign syntax": the
  translator already passes Lisp special forms through with Scheme
  subexpressions translated (that's how `read.scm` uses
  `ps-lisp:setq`), and the same could apply to macro calls by expanding
  them with `macroexpand-1` after translating the subforms.
* An escape, `(cl-form <lisp form>)`, for when you really mean Lisp.

### 3.5 CLOS

Generic functions are functions, so calling them needs nothing. Defining
methods and classes from Scheme wants a small library, `(pseudoscheme
clos)`: `define-generic`, `define-method` (with Scheme-procedure
bodies), and `define-class` mapping to `defclass`. Record types already
are structs, so specializing methods on a Scheme record type works.

## 4. Things that cut across both directions

* **Tail calls.** Pseudoscheme relies on the Lisp compiler eliminating
  tail calls (SBCL does at its default optimization settings). Calls
  through Lisp frames are never guaranteed tail calls; that's
  unavoidable and acceptable at the boundary.
* **Continuations.** See `docs/continuations.md`. Whatever we do, a
  continuation captured in Scheme code called *from* Lisp can't
  re-enter through the Lisp frames above it: those frames are on the
  real stack. The usual answer (Racket's "continuation barriers") is to
  make capture work up to the nearest Lisp frame and raise an error
  beyond it.
* **Errors.** Lisp conditions become R6RS conditions inside Scheme
  handlers (done); uncaught Scheme raises become Lisp errors of type
  `ps-r7rs::uncaught-raise` carrying the raised object, printed with
  the R6RS message/irritants (done). Lisp handlers can `handler-case`
  on that type.
* **Threads.** The host's threads are Scheme's. psyntax's state
  (installed libraries, gensym counter) is global and unlocked: fine for
  one expanding thread, which should be documented and guarded.

## 5. Order of work

1. `(cl <package>)` libraries with predicate wrapping; `#:keyword`
   reader syntax. Small, and makes Scheme useful immediately.
2. `pseudoscheme:use-library`: Scheme libraries as Lisp packages.
3. ASDF component types and compiled libraries (library serialization).
4. `(pseudoscheme clos)`.
5. Foreign syntax for expression-only Lisp macros.
