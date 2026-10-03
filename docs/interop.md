# Scheme and Common Lisp, both ways

Pseudoscheme compiles Scheme *to* Lisp, so most of the bridge is just
"use the same objects". This document covers what is shared, what gets
converted at the boundary and why, the API in each direction, and how
libraries are found and distributed. The code is in `src/interop.lisp`,
`src/interop/lisp.sls`, `src/api.lisp` and `src/asdf.lisp`. Tests are in
`tests/run-interop-tests.lisp`; examples are in `examples/`.

## 1. Shared objects

| Scheme | Lisp | notes |
|---|---|---|
| pair, list | cons, list | `'()` **is** `NIL` |
| procedure | function | callable with `funcall` |
| number | number | the full tower; inexact means `double-float` |
| string, char | string, character | Unicode |
| vector | simple-vector | |
| bytevector | `(simple-array (unsigned-byte 8) (*))` | |
| symbol | symbol in `SCHEME` | name is case-inverted: `car` ↔ `SCHEME::CAR` |
| `#:name` | keyword `:NAME` | reader syntax, see 3.3 |
| port | stream | |
| record | struct instance | |
| condition | condition | Lisp errors are conditions in Scheme handlers, and vice versa |
| multiple values | multiple values | |
| `dynamic-wind` | `unwind-protect` | |

**Booleans** are the one real mismatch: `#f` is the symbol `PS:FALSE`,
`#t` is `T`, and `NIL` is the empty list. Making `#f` the same as `NIL`
would break `(if '() 'yes 'no)`, which must be `yes`. Giving the empty
list its own object would break the most valuable sharing, which is
that Scheme lists *are* Lisp lists. So booleans are converted where
values cross between the languages.

## 2. The boundary rules

| crossing | conversion |
|---|---|
| a Scheme value passed to Lisp (arguments of a `(cl ...)` function, results of a `use-library` function) | `#f` → `NIL` |
| a result of a Lisp predicate returned to Scheme | `NIL` → `#f` |
| a result of any other Lisp function returned to Scheme | none: `NIL` is `()` |
| a Lisp function given to Scheme | wrapped, and treated as a predicate: its `NIL` → `#f` |
| a Scheme procedure given to Lisp | wrapped: its `#f` → `NIL` |
| a wrapper crossing back | unwrapped (so `cl:equal` reaches `make-hash-table` as `#'equal`) |

A Lisp function counts as a predicate when:
- its name ends in `-P`;
- or it is a one-word name ending in `P` (`EVENP`, `TYPEP`, `EMPTYP`);
- or it is listed in `*lisp-predicates*`. That list holds ANSI functions
  whose `NIL` means false although their names don't say so: `EQUAL`,
  `STRING=`, `MEMBER`, `FIND`, `SOME`, and so on.

`*not-lisp-predicates*` lists the names that only look like predicates
(`MAP`).

The rule for callbacks covers the common case, `(filter #'evenp ...)`.
For a Lisp callback that returns *lists* to Scheme (where `NIL` means
`()`), mark it with `verbatim`: `(srfi-1:append-map (r7rs:verbatim
(lambda (x) ...)) ...)`.

Conversion happens only at the top level of argument and value lists.
A `#f` *inside* a list stays `#f`.

## 3. Scheme calling Lisp

### 3.1 Lisp packages are libraries: `(cl <package>)`

```scheme
(import (scheme base)
        (prefix (cl common-lisp) cl:)
        (prefix (cl alexandria) alex:))

(cl:sort (list 3 1 2) cl:<)            ; => (1 2 3)
(alex:flatten '((1 2) (3 (4))))         ; => (1 2 3 4)
```

`(cl <package>)` exports every external symbol of the package that
names a function or a variable, under its Lisp name in Scheme spelling
(`REMOVE-IF` becomes `remove-if`). Prefixing the import keeps these
names apart from Scheme's own and reads like Lisp. `only`, `except`,
`rename` and `prefix` all work. A name like `(cl foo bar)` refers to the
package `FOO/BAR`, the naming that package-inferred systems use.

- **Functions** are wrapped per section 2. Generic functions and struct
  accessors are functions too.
- **Variables** (specials and constants) are identifier macros over
  `symbol-value`. Reading `cl:*print-base*` gives its current value,
  `(set! cl:*print-base* 16)` assigns it, and `lisp-let` binds it
  dynamically (3.4).
- **Macros and special operators** are not exported. A Lisp macro's
  arguments are Lisp code, not Scheme. `lisp-set!` covers the most common
  one, `setf`; for the rest, use `lisp-eval-string`.

A library is built when first imported: about 0.3 s for all of
`COMMON-LISP` (748 functions and 112 variables). After that, a
`(cl ...)` import costs nothing.

### 3.2 Finding the Lisp code: autoloading

If the package doesn't exist, importing `(cl foo)` first loads the
*system* `foo` by calling `pseudoscheme-interop:*lisp-system-loader*`.
The default loader:

- uses `ql:quickload` if Quicklisp is loaded, which also downloads
  systems it doesn't have yet;
- otherwise uses `asdf:load-system`. This finds whatever ASDF's source
  registry knows about: `~/common-lisp/`, `CL_SOURCE_REGISTRY`, an ocicl
  project (whose ASDF hook can also fetch systems), or a qlot project run
  under `qlot exec`.

ASDF is the common denominator of every Lisp distribution tool, so it is
the default; Quicklisp is used only when the user has already chosen it
by loading it. Set `*autoload-lisp-systems*` to NIL to turn autoloading
off, or replace the loader. From the command line:
- `pseudoscheme -l cl-ppcre prog.scm` preloads a system;
- `pseudoscheme --quicklisp prog.scm` loads Quicklisp first.

When a package name differs from its system name (package `BT`, system
`bordeaux-threads`), load the system first: `(lisp-require
"bordeaux-threads")` at the REPL, `-l` on the command line, or a
dependency in an ASDF system (3.5).

### 3.3 Keywords: `#:name`

`#:test` reads as the Lisp keyword `:TEST`. The name is case-inverted
like a symbol's, and the keyword is self-evaluating and prints back as
`#:test`. Neither R6RS nor R7RS gives `#:` a meaning; Guile and Racket
use it for their own keywords.

```scheme
(cl:sort pairs cl:< #:key cl:car)
(cl:make-hash-table #:test cl:equal)
```

### 3.4 `(pseudoscheme lisp)`

| | |
|---|---|
| `(lisp-true? x)`, `(lisp-false? x)` | Lisp truth (`NIL` and `#f` are false) |
| `(lisp-set! (accessor arg ...) value)` | Lisp's `setf`: `(lisp-set! (cl:gethash k h) v)`; any place `setf` knows |
| `(lisp-let ((var value) ...) body ...)` | dynamically binds Lisp specials imported from `(cl ...)` libraries |
| `(lisp-symbol name [package])` | a symbol, written the Scheme way: `(lisp-symbol "equal" "cl")` |
| `(lisp-keyword name)` | what `#:name` reads as |
| `(lisp-function name [package])` | the raw function, which Scheme calls without conversion |
| `(lisp-funcall f arg ...)`, `(lisp-apply f arg ... list)` | call with `#f` passed as `NIL`; results unconverted |
| `(lisp-value symbol)`, `(set-lisp-value! symbol v)` | `symbol-value` |
| `(lisp-symbol-of id)` | the Lisp symbol behind a `(cl ...)` variable |
| `(call-with-lisp-bindings symbols values thunk)` | `progv` |
| `(lisp-require system)` | load a Lisp system now |
| `(lisp-eval-string string)` | read and evaluate Lisp source |
| `(verbatim proc)` | pass `proc` to Lisp without converting its results |

### 3.5 Distributing Scheme code that uses Lisp

Make it an ASDF system. Its `:depends-on` lists the Lisp libraries, so
whatever installs systems for the user resolves them like any other
dependency: Quicklisp, ocicl, qlot, CLPM, or a source registry. Scheme
sources are components of the system (section 5).

## 4. Lisp calling Scheme

`(asdf:load-system :r7rs)` loads the API. `:r6rs` and `:r5rs` are the
same system, `pseudoscheme/api`. The packages `R7RS`, `R6RS` and `R5RS`
each have:

| | |
|---|---|
| `eval source` | Scheme text, or a Lisp datum (CL's `FOO` is Scheme's `foo`). Values come back as they are (`#f` is `FALSE`). |
| `scheme form ...` | macro: evaluate unevaluated forms at the REPL top level, with results converted Lisp-style (`#f` → NIL) |
| `load file`, `repl` | |
| `expand source` | the core Scheme an expression expands to |
| `translate source` | the Lisp code it compiles to |
| `procedure name &key library convert` | a Scheme procedure as a Lisp function |
| `read-from-string`, `write-to-string` | the Scheme reader and writer |
| `true-p`, `false`, `verbatim` | |

`R7RS` and `R6RS` also have `use-library`, `library-exports`,
`*library-path*` and `add-library-directory`. These names shadow CL's,
so use them package-qualified (`r7rs:eval`).

### 4.1 Scheme libraries are packages: `use-library`

```lisp
(r7rs:use-library '(srfi 1))                  ; => #<PACKAGE "SRFI-1">
(srfi-1:filter #'evenp '(1 2 3 4))            ; => (2 4)
(r7rs:use-library "(srfi 13)" :package "STR")
(str:string-pad "42" 6 #\0)                   ; => "000042"
```

- Each exported procedure becomes a function, converting per section 2.
  `:convert nil` binds the procedures themselves instead.
- Exported variables become symbol macros that read the current value.
- Syntax exports are skipped; `library-exports` lists them.

To read `pkg:name` in the same file that creates the package, call
`use-library` inside `eval-when (:compile-toplevel :load-toplevel
:execute)`, as `examples/mixed-system/report.lisp` does.

### 4.2 Errors

An uncaught Scheme `raise` is a Lisp error of type
`ps-r7rs::uncaught-raise`, printed with the condition's message and
irritants. Lisp code can `handler-case` it.

## 5. Scheme sources in ASDF systems

```lisp
(defsystem :my-app
  :defsystem-depends-on (:pseudoscheme/asdf)
  :depends-on (:alexandria)
  :components ((:r7rs-library "lib/stats")     ; lib/stats.sld
               (:r7rs-file "setup")            ; setup.scm, run at load time
               (:file "main")))                ; Lisp using both
```

- `:r7rs-library` / `:r6rs-library`: a file of library definitions.
  Loading the component puts the library's root directory on
  `*library-path*` and installs the libraries.
- `:r7rs-file`, `:r6rs-file`, `:r5rs-file`: a source file that is loaded
  (run).

Defining a library again replaces it, so reloading the system (or
re-evaluating a `define-library` at the REPL) picks up changes. This is
also how a Scheme library reaches Lisp users who never install a Scheme
package manager: ship it as an ASDF system.

Compiling the component does nothing yet: there are no compiled Scheme
libraries (ROADMAP), so each load expands the sources again.

## 6. Scheme libraries from Scheme package managers

Akku and snow-fort lay libraries out the way `*library-path*` expects,
so `(r7rs:add-library-directory ".akku/lib/")` (or `-L .akku/lib` on the
command line) is all it takes. `tests/run-library-corpus.lisp`
measures how many load.

## 7. Not done yet

- **CLOS from Scheme.** Calling generic functions and making instances
  works (they're functions). *Defining* classes and methods wants a
  `(pseudoscheme clos)`: `define-class`, `define-generic`,
  `define-method`.
- **Lisp macros** with expression-only arguments (`incf`, `when`,
  `with-open-file`'s body) could be imported as "foreign syntax", by
  translating the subforms and then macroexpanding.
- **Compiled libraries** (ROADMAP).
- **Continuations** captured in Scheme called from Lisp can't re-enter
  through the Lisp frames above them. Continuations are escape-only
  everywhere today (docs/continuations.md).
- **Threads.** psyntax's state is global and unlocked: one expanding
  thread at a time.
