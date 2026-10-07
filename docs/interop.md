# Scheme and Common Lisp, both ways

Pseudoscheme compiles Scheme *to* Lisp, so most of the bridge is just
"use the same objects". This document covers what is shared, what gets
converted at the boundary and why, the API in each direction, and how
libraries are found and distributed. The code is in `src/interop.lisp`,
`src/interop/lisp.sls`, `src/api.lisp` and `src/asdf.lisp`. Tests are in
`tests/run-interop-tests.lisp`, and programs using real Common Lisp and C
libraries in `tests/programs/` (`make test-programs`); examples are in
`examples/`.

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
| a Scheme procedure given to Lisp, as an argument or a result | wrapped: its `#f` → `NIL` |
| a wrapper crossing back | unwrapped (so `cl:equal` reaches `make-hash-table` as `#'equal`) |
| a Lisp error, caught by a Scheme handler | an R6RS condition (`error-object?`, with the Lisp message) |
| that condition passed back to Lisp | the Lisp condition again: `(cl:typep e sqlite:sqlite-error)` works |

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

`(cl <package>)` exports every external symbol of the package, under
its Lisp name in Scheme spelling (`REMOVE-IF` becomes `remove-if`).
Prefixing the import keeps these names apart from Scheme's own and reads
like Lisp. `only`, `except`, `rename` and `prefix` all work. A name like
`(cl foo bar)` refers to the package `FOO/BAR`, the naming that
package-inferred systems use.

- **Functions** are wrapped per section 2. Generic functions and struct
  accessors are functions too.
- **Variables** (specials and constants) are identifier macros over
  `symbol-value`. Reading `cl:*print-base*` gives its current value,
  `(set! cl:*print-base* 16)` assigns it, and `lisp-let` binds it
  dynamically (3.4).
- **Macros and special operators** are Scheme macros whose calls are
  compiled as Lisp; see 3.5.
- **Every other symbol** (types, classes, lambda-list keywords...) is
  syntax for the symbol itself, so `(cl:typep x cl:integer)` needs no
  quote.

A library is built when first imported: a fraction of a second for all
of `COMMON-LISP`. After that, a `(cl ...)` import costs nothing.

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
dependency in an ASDF system (3.6).

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
| `(lisp form ...)` | Lisp code in Scheme: a `progn`, compiled as Lisp (3.5) |

### 3.5 Lisp macros in Scheme

```scheme
(define numbers '(1 2 3 4 5 6))
(cl:loop for n in numbers when (even? n) collect (square n))   ; => (4 16 36)
(cl:destructuring-bind (a (b c) &key d) '(1 (2 3) #:d 4) (list a b c d))
(cl:handler-case (cl:parse-integer "12x") (cl:parse-error () 'bad))
(it:iter (for x in '(1 2 3)) (collect (* x 10)))               ; ITERATE
(cl:defclass circle () ((radius #:initarg #:radius #:reader radius)))
(lisp (radius (make-instance 'circle :radius 2)))              ; => 2
```

Inside a Lisp macro call, **the code is Lisp, with Scheme's variables
and procedures visible**. The transformer walks the form and decides what
each identifier is:

| identifier | becomes |
|---|---|
| a Scheme variable or procedure (`numbers`, `even?`, `square`) | a placeholder: the form is compiled, once, as a Lisp function of the placeholders, and the expansion calls it with the variables' values. A placeholder in operator position calls its value. |
| an export of a `(cl ...)` library (`cl:car`, `cl:parse-error`, `cl:*print-base*`, a nested `cl:when`) | its Lisp symbol |
| Scheme syntax with a Lisp counterpart: `lambda`, `if`, `let`, `let*`, `quote`, `set!` (`setq`), `begin` (`progn`), `and`, `or`, `when`, `unless`, `cond`, `case`, `do`, `else` (`t`) | that Lisp operator |
| other Scheme syntax | an error |
| unbound (`for`, `in`, `collect`, and `x`, which `loop` binds) | the symbol of that name in the macro's package, else in `COMMON-LISP`, else the Scheme symbol itself; `:name` is a keyword |

Quoted data stays Scheme data, except that `(cl ...)` exports inside it
become their Lisp symbols. Values cross by the section 2 rules, and so
does the result. Because unbound names become the Scheme symbols, a
class or function defined from Scheme is named by Scheme's symbol:
`'circle` is the class above, and `(lisp-function 'radius)` is its
reader.

`(lisp form ...)`, from `(pseudoscheme lisp)`, is the same mechanism
with `progn`: Lisp code anywhere in Scheme.

A program or library is expanded before any of it runs, so each Lisp
form's compile-time effects happen when it is expanded, as
`compile-file` does them: after `(cl:defvar *depth* 0)` a later
`(cl:let ((*depth* 5)) ...)` binds it dynamically, and a `cl:defmacro`,
`cl:defstruct` or CFFI's `defcstruct` is known to the Lisp forms after
it. (A compile-time part that mentions a Scheme variable can't run
early, and is left to run time.)

Limits. A Scheme variable can be read and called, but not assigned, from
inside a Lisp form, since its value is passed in. A Scheme name is
Scheme's there even where Lisp code means something else: a CFFI struct
slot named `min` is Scheme's `min`, so give it another name. Local macros
(`let-syntax`) aren't recognized as syntax there. Each macro use is
compiled when it's expanded, which takes milliseconds per use.

### 3.6 Distributing Scheme code that uses Lisp

Make it an ASDF system. Its `:depends-on` lists the Lisp libraries, so
whatever installs systems for the user resolves them like any other
dependency: Quicklisp, ocicl, qlot, CLPM, or a source registry. Scheme
sources are components of the system (section 5).

## 4. Lisp calling Scheme

`(asdf:load-system :r7rs)` loads the API. `:r6rs` and `:r5rs` are the
same system, `pseudoscheme/api`.

### 4.1 Importing, defining, and libraries written in Lisp

```lisp
(r7rs:import (only (srfi 1) fold filter iota)
             (rename (only (srfi 1) delete-duplicates) (delete-duplicates dedup))
             (prefix (srfi 13) str-)
             (srfi 26))                         ; cut: a Scheme macro

(fold #'+ 0 '(1 2 3))                           ; => 6
(filter #'evenp (iota 10))                      ; => (0 2 4 6 8)
(mapcar (cut * 2 <>) '(1 2 3))                  ; => (2 4 6)

(r7rs:define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
(mapcar #'fact '(1 2 3 4))                      ; => (1 2 6 24)

(r7rs:define-library (demo geometry)
  (export distance)
  (import (scheme base) (scheme inexact))
  (begin (define (distance x y) (sqrt (+ (* x x) (* y y))))))
(r7rs:import (demo geometry))
(distance 3 4)                                  ; => 5
```

- **`r7rs:import`** takes R7RS import sets and brings their bindings
  into the *current package*. Procedures become functions, converting by
  section 2; variables become symbol macros; syntax becomes Lisp macros
  (4.2). A name that would redefine a symbol the package inherits (most
  often from `COMMON-LISP`: `find`, `remove`, `delete-duplicates`...) is
  an error, so use `only`, `except`, `prefix` or `rename`. The import
  happens at compile time too, so the names can be used in the same file.
- **`r7rs:define`** defines at the Scheme REPL and binds the Lisp
  function (or symbol macro) of the same name. Redefining a name that
  Scheme imports, such as `(r7rs:define (positive? x) ...)`, shadows the
  import at the REPL.
- **`r7rs:define-library`** installs a library, at compile time too.
- **Scheme written as Lisp data** (these macros, `r7rs:scheme`) reads
  CL's `FOO` as Scheme's `foo`. `r7rs:false` and `r7rs:true` stand for
  `#f` and `#t`.

`r6rs:import`, `r6rs:define`, `r6rs:library` and `r5rs:define` are the
same for the other standards.

`r7rs:use-library` is the alternative to `import`: it puts a library's
bindings into a package of their own, named after the library
(`srfi-1:fold`), or the one given by `:package`. To read `pkg:name` in
the same file that creates the package, call it inside `eval-when
(:compile-toplevel :load-toplevel :execute)`, as
`examples/mixed-system/report.lisp` does.

### 4.2 Scheme macros in Lisp

A Scheme macro brought in by `import` or `use-library` is a Lisp macro.
Its arguments are **Scheme code written in Lisp syntax, with Lisp's
meanings where they exist**:
- a Lisp lexical variable is that variable;
- a Lisp function name is that function, wrapped as a `(cl ...)` export
  would be: a global function, or a local one from `flet` or `labels`;
- a call of a name that is neither, and that Scheme doesn't bind, calls
  the global Lisp function of that name when it runs, as in Lisp: so a
  `defun` whose body uses a Scheme macro can call itself. Pass such a
  function as a value with `#'name`;
- quoted data is Lisp data, and `NIL` is the empty list;
- other symbols are Scheme's: syntax, Scheme-only procedures, and the
  variables the macro binds.

The expansion runs psyntax, hygienically, on a Scheme `lambda` over the
Lisp variables, translates the result to Lisp, and calls it. The result
is ordinary Lisp code, compiled with the file.

```lisp
(let ((n 10)) (mapcar (cut + n <>) '(1 2 3)))  ; => (11 12 13)
(receive (q r) (floor 17 5) (list q r))        ; SRFI 8; => (3 2)
(macroexpand-1 '(cut list 1 <>))               ; plain Lisp
```

Limits:
- Lisp variables are passed by value, so the macro can't assign them.
- The expansion refers to the running Pseudoscheme image, so code
  compiled with these macros must be loaded into an image where
  Pseudoscheme is booted, the same way the Scheme libraries themselves
  must be.

### 4.3 The rest of the API

Each of `R7RS`, `R6RS` and `R5RS` has:

| | |
|---|---|
| `eval source` | Scheme text, or a Lisp datum. Values come back as they are (`#f` is `FALSE`). |
| `scheme form ...` | macro: evaluate forms at the REPL top level; results converted (`#f` → NIL) |
| `load file`, `repl` | |
| `expand source` | the core Scheme an expression expands to |
| `translate source` | the Lisp code it compiles to |
| `procedure name &key library convert` | a Scheme procedure as a Lisp function |
| `read-from-string`, `write-to-string` | the Scheme reader and writer |
| `true-p`, `false`, `true`, `verbatim` | |

`R7RS` and `R6RS` also have `use-library`, `library-exports`,
`*library-path*` and `add-library-directory`. These names shadow CL's,
so use them package-qualified (`r7rs:eval`).

### 4.4 Errors

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
command line) is all it takes. docs/libraries.md walks through
installing and using packages with each. `tests/run-library-corpus.lisp`
measures how many load.

## 7. Not done yet

- **CLOS-style definitions in Scheme syntax.** `cl:defclass` and
  `cl:defmethod` work through 3.5, with Lisp syntax. A `(pseudoscheme
  clos)` with Scheme-style forms (`define-class`, `define-method`
  bodies as Scheme) could follow.
- **Compiled libraries** (ROADMAP). These would also make code that uses
  Scheme macros from Lisp loadable from fasls into a fresh image.
- **Continuations** captured in Scheme called from Lisp can escape
  through the Lisp frames above them but not be re-entered through
  them: a Scheme procedure handed to Lisp (`lisp-facing`, so any
  procedure passed to a `(cl ...)` function and `use-library`'s
  functions) runs in a barrier frame, and re-entering a continuation
  captured under one is an error (docs/continuations.md).
- **Threads.** psyntax's state is global and unlocked: one expanding
  thread at a time.
