# Chez Scheme on Pseudoscheme: `--chez`

`pseudoscheme --chez prog.ss` runs programs written for Chez Scheme 10.
Scripts run at Chez's top level, R6RS programs and libraries can import
`(chezscheme)`, and so can the libraries written for Chez. Run as
`scheme-script` (a link to bin/pseudoscheme, as Chez's is), the program
is `--chez`.

The test program is [e](https://github.com/paveluv/e), a terminal
editor written in Chez: about 60 libraries, an FFI layer over libc,
threads, fork/exec, ptys, and 63 test scripts. **33** of those scripts
pass here, run as e runs them (its `tests/*.ss`, from the repository's
root). The rest are listed under "What e still needs" below.

All of it lives in `src/chez/`:

| file | what |
|---|---|
| chez.lisp | the `(chezscheme)` library, Chez's top level, library files that define their library at top level, the extensions Chez makes to R6RS procedures |
| host.lisp | primitives in Lisp: boxes, fxvectors, weak tables, threads, mutexes and conditions, `format`, Chez-style parameters, fd ports, nonblocking reads, file options |
| ffi.lisp | `load-shared-object`, `foreign-procedure`, foreign memory, on CFFI |
| chezscheme.scm, lists.scm, numbers.scm, hashtables.scm, threads.scm, conditions.scm, top-level.scm, ffi.scm | the library's body, in Scheme |

`(chezscheme)` exports 1178 of the 1715 names Chez 10.4.1's does (803
before this work), with no extra names of its own. The comparison is
`(library-exports '(chezscheme))` in Chez against psyntax's
`library-export-bindings` here.

## Chez's own tests: the mats

Chez Scheme's test suite, its "mats" (`mats/*.ms` at cisco/ChezScheme
e3b13636), is vendored in vendor/chez-mats and run by `make test-chez`
(tests/run-chez-mats.sh). Each file runs in its own `--chez` process.
tests/chez-mat.ss stands in for Chez's mat.ss and reports one line per
clause:

| result | meaning |
|---|---|
| pass | true, or raised the error it should |
| fail | false, or raised no error where it should have |
| error | raised an unexpected condition |
| unbound | referred to a name `(chezscheme)` doesn't export |
| unsup | needs what isn't planned here: Chez's compiler and its files, the inspector, `$` system procedures |
| crash | hung or killed its process, and was skipped on a retry |

The files about the compiler or C (cp0, cptypes, foreign, ftype,
profile, windows) aren't counted in the totals. The latest run:

| | pass | fail | error | unbound | unsup | crash |
|---|---|---|---|---|---|---|
| **counted files** | **12573** | **1277** | **792** | **1550** | **6663** | **26** |
| first run | 7892 | 941 | 714 | 1798 | 4726 | 24 |

The first run stopped reading nine files early, at reader syntax we
didn't have. Once those files read through, more of their clauses
counted in every column, failures included. By file:

| file | pass | fail | error | unbound | unsup | crash |
|---|---|---|---|---|---|---|
| 3 | 468 | 58 | 0 | 4 | 150 | 0 |
| 4 | 153 | 17 | 17 | 1 | 89 | 7 |
| 5_1 | 382 | 0 | 3 | 16 | 4 | 0 |
| 5_2 | 187 | 18 | 1 | 4 | 3 | 7 |
| 5_3 | 98 | 0 | 0 | 6 | 0 | 0 |
| 5_4 | 780 | 68 | 3 | 19 | 12 | 0 |
| 5_5 | 425 | 62 | 0 | 9 | 54 | 1 |
| 5_6 | 383 | 88 | 9 | 43 | 197 | 2 |
| 5_7 | 37 | 17 | 3 | 2 | 1 | 0 |
| 5_8 | 19 | 3 | 0 | 0 | 0 | 0 |
| 6 | 2597 | 121 | 44 | 52 | 62 | 0 |
| 7 | 172 | 45 | 134 | 89 | 575 | 0 |
| 8 | 684 | 154 | 242 | 217 | 919 | 3 |
| bytevector | 863 | 50 | 12 | 13 | 1673 | 3 |
| cfl | 59 | 0 | 0 | 123 | 81 | 0 |
| cp0 | 7 | 0 | 1 | 44 | 244 | 0 |
| cptypes | 3 | 0 | 0 | 22 | 539 | 0 |
| date | 75 | 35 | 1 | 2 | 159 | 0 |
| enum | 41 | 3 | 0 | 2 | 0 | 0 |
| examples | 4 | 2 | 54 | 318 | 0 | 0 |
| exceptions | 12 | 0 | 0 | 2 | 71 | 0 |
| fl | 737 | 14 | 0 | 16 | 125 | 0 |
| foreign | 21 | 1 | 4 | 4 | 28 | 0 |
| format | 960 | 212 | 14 | 0 | 26 | 0 |
| ftype | 18 | 0 | 0 | 79 | 1115 | 0 |
| fx | 1122 | 18 | 19 | 49 | 597 | 0 |
| hash | 665 | 145 | 1 | 12 | 443 | 0 |
| ieee | 479 | 22 | 1 | 14 | 4 | 0 |
| io | 248 | 35 | 64 | 101 | 445 | 0 |
| misc | 357 | 28 | 16 | 137 | 309 | 3 |
| oop | 70 | 0 | 97 | 75 | 0 | 0 |
| primvars | 69 | 11 | 0 | 9 | 43 | 0 |
| profile | 7 | 6 | 1 | 13 | 181 | 0 |
| record | 348 | 34 | 51 | 211 | 539 | 0 |
| thread | 19 | 0 | 2 | 4 | 70 | 0 |
| unix | 60 | 17 | 4 | 0 | 12 | 0 |
| windows | 15 | 0 | 0 | 0 | 0 | 0 |

What the fixes since the first run were:

- **The reader.** It now handles:
  - datum labels referred to before they are defined (`'#1=(a #2#)`
    with `#2=` later in the same datum);
  - `#!eof` inside a list;
  - `#i#32r+21`, any radix after an exactness prefix;
  - binary and octal exponents (`#b1.1e101`), and an exponent after
    a ratio (`1/5e4`);
  - R4RS's `98##` digit placeholders;
  - stencil vectors (`#5vs(a b)`);
  - the character names `#\bel`, `#\nul`, `#\vt`, `#\nel`
    and `#\ls`;
  - tokens like `2.a`, read as symbols.

  In `--chez` mode, `#!r6rs` restricts the rest of the port to R6RS
  syntax, `(` and `[` must be closed by their own kind of bracket,
  and absurd vector lengths are errors. Number syntax is ASCII only, so
  `٦` is no longer read as 6.
- **Top-level `module`.** Internal modules already worked. At top level, a
  module is expanded as a library of its own that imports what the top
  level has. An anonymous module's exports are imported at once; a
  named one's when `(import name)` runs.
- **Arities Chez extends:**
  - comparisons (`char=?`, `string<?`, ..., `fx=`, `fl<`) take one
    argument;
  - `fx+`, `fx-` and `fx*` take any number;
  - `delete-file` takes `error?`, `file-exists?` takes `follow?`;
  - `load` takes an eval procedure.

  The translator still open-codes the standard arities: an extended
  primitive is closed only for calls whose argument counts it doesn't
  share with the built-in (`psx::*extended-primitives*`).
- **New exports:**
  - stencil vectors;
  - the `r6rs:` versions of the comparisons and of `fx+`, `fx-` and
    `fx*`;
  - `char-name`;
  - file times as `time-utc` times, from `stat` (`file-access-time`,
    ...);
  - the 24-, 40-, 48- and 56-bit bytevector accessors;
  - `flonum->fixnum`, `flsingle`, `immutable-vector-append`,
    `immutable-vector-copy`, `vector-cas!` and `pretty-format`.
- **The harness** loads oop.ss before oop.ms, as Chez's makefile does.
  It counts clauses that reach `$` system procedures as unsup, and it
  kills a hung clause's process rather than the shell around it.

The largest groups left:
- 7 and 8: clauses that need the files of an earlier `compile-file`
  or `compile-whole-library` (missing here; the harness can't tell
  them from the clauses' text), `define-property`, library
  introspection, `eval-when` and `meta`.
- format: `~e`, `~f`, `~$` and `~:r` (ordinals), where CL's `format`
  writes numbers differently from Chez's.
- hash: weak and ephemeron tables, which are ordinary tables here.
- record: R6RS record-syntax details, `#[...]` record literals, and
  `record-equal-procedure` and `record-hash-procedure`.
- oop: oop.ss builds `define-class` on `$make-record-type`, a system
  procedure, so its library doesn't load.
- examples: Chez's example programs, which the mats expect to find and
  load.

## Why Chez is the near one

- **The expander is already Chez's.** psyntax (vendor/psyntax) is
  Ghuloum and Dybvig's portable version of the expander Chez is built
  on, so `module`, `import`, `alias`, `define-property` and
  `syntax-case` behave as Chez's do.
- **R6RS is complete** (all 8902 of Racket's R6RS tests), and Chez's
  language is R6RS plus extensions.
- **A top level with another base library already existed**: R5RS's.
  Chez's is the same thing with `(chezscheme)`.

So `(chezscheme)` is implemented here, as the R6RS libraries were.
Hosting Chez's own `s/*.ss` would have meant emulating its compiler's
internals.

## How each problem was solved

### The top level

A Chez script runs in the interaction environment: every `(chezscheme)`
name visible, definitions redefinable, `import` allowed anywhere. That
is a psyntax interaction library, `(pseudoscheme chez interaction)`,
whose source library is `(chezscheme)`. `eval-at-chez-top-level`
(chez.lisp) binds psyntax's two parameters and evaluates:

```lisp
(with-psyntax-parameter ("psyntax:interaction-library-name" (mapcar #'ssym library))
  (with-psyntax-parameter ("psyntax:interaction-source-name" (list (ssym "chezscheme")))
    (eval-top-level-forms form)))
```

A top-level `begin` is evaluated one form at a time, as Chez does. Each
form is expanded after the ones before it have run, and a library is
invoked only when a form that runs refers to it. e's documentation
test checks exactly that: a type registered by a library the test never
touches must not be there.

### Libraries a macro makes

Every one of e's library files looks like this:

```scheme
(import (only (foundation edoc) elibrary))
(elibrary (foundation string)
  (export ...)
  (import ...)
  ...)
```

`elibrary` is a macro that expands into a `library` form, which Chez
allows at top level. Three changes made that work:

- psyntax accepts `library` as a top-level form (`top-level-library-expander`
  in expander.ss). The name, exports and imports are taken as data. The
  body keeps its syntax, so identifiers the macro put there refer to
  what they did where the macro was defined.
- When `(foundation string)` is imported and its file doesn't begin with
  a `library` form, a library loader (`load-library-file-at-top-level`)
  loads the file at a fresh Chez top level. Whatever library that
  defines is the one imported.
- `meta define`, which e's `edoc` uses to check documentation as it
  expands, defines a variable at expansion time (`chi-meta-definition`).
  A library that uses it isn't put in the compiled-library cache, since
  its expansion depends on state the cache doesn't keep.

### A body's `import` of a library

Chez's `import` in a body takes library import sets as well as modules.
e uses it this way:

```scheme
(eval `(let () (import (only ,lib init!)) (init!)) (interaction-environment))
```

psyntax's body `import` now binds each imported name in the body's rib,
in the context of the `import` keyword, and records the library to be
invoked.

### Shared bindings: Chez's extensions to R6RS procedures

In Chez, `(rnrs)` and `(chezscheme)` export the same `dynamic-wind`,
`eval` and `call-with-output-file`. Those procedures accept Chez's
extra arguments, and a program may import both libraries. Here too the
extensions are made to the R6RS procedures themselves
(`install-chez-extensions`):

```scheme
(dynamic-wind #t before thunk after)          ; Chez's critical? flag
(call-with-output-file path proc 'replace)    ; file options
(eval '(+ 1 2))                               ; one argument: the top level running
```

Without the extra arguments they behave as before. `eval` is psyntax's
own; it now passes a missing or non-psyntax environment to a hook
(`eval-hook`), which evaluates at the Chez top level or the REPL.

### Threads and parameters

`fork-thread` makes a thread with bordeaux-threads' first API. Its
second API ends a thread at the first condition the thread signals,
warnings included. The new thread takes the creator's standard ports
and a **snapshot of every parameter's value**: Chez's parameters are
per thread. A parameter's value is global unless the thread has a table
of its own (`*thread-parameters*`, src/r7rs/rts.lisp):

```scheme
(define p (make-parameter 1))
(p 2)                                   ; Chez: calling with a value sets it
(thread-join (fork-thread (lambda () (p 9) (p))))   ; the thread sees 2, sets 9
(p)                                     ; => 2 here still
```

Mutexes are recursive in Chez, and bordeaux-threads' recursive locks
aren't implemented on SBCL, so the recursion is counted by hand around
a plain lock. `condition-wait` releases the whole count while it waits.

### The FFI

The FFI is the foreign-function layer the Chez and Guile modes share
(src/ffi.lisp, system `pseudoscheme/cffi`). `foreign-procedure` compiles
a Lisp function that calls the entry point through
`cffi:foreign-funcall-pointer`, with Chez's types translated:
`int`, `unsigned`, `uptr`, `void*`, `double`, `boolean`, `string`,
`u8*` (a bytevector's data, pinned for the call), and so on. Pointers
are integers, as in Chez. `foreign-ref`, `foreign-set!` and
`foreign-alloc` work on those addresses. `foreign-callable` compiles a
CFFI callback; its code object is its entry point, and it is never
freed. e's `(sys sys)` uses this to
call `fork`, `execvp`, `pipe`, `poll`, `waitpid` and `openpty`. Its
process tests run real children through it.

### Ports on descriptors, and reading what is there

`open-fd-input-port` wraps a descriptor in an SBCL fd-stream.
`get-bytevector-some!` returns what's available: on a blocking port it
waits for the first byte only, and on a port made nonblocking with
`set-port-nonblocking!` it returns 0 when nothing is ready. SBCL's
`listen` can't tell an empty pipe from its end, so a zero-timeout poll
of the descriptor decides that (`read-some-bytes`, src/r6rs/ports.lisp).
Without this, e's `write-process!` deadlocked: it drains the child's
output while writing its input.

### `format` and error messages

Chez's `format` follows Common Lisp's, so CL's `format` does the work.
Objects are printed as Scheme prints them through a pprint-dispatch
table, which calls Scheme's writer for everything but integers and
ratios. An uncaught error is reported as Chez reports it:

```
Exception in car: 5 is not a pair
Exception in foo: bad thing with irritant 42
Exception occurred with non-condition value 42
```

### Reader syntax

`#&x` (boxes), `#vu8(...)`, `#vfx(...)`, `#vfl(...)`, `#5vs(a b)`
(stencil vectors), `#3(a b)` (length-prefixed vectors, filled out with
the last element), `#%car`, `#12r-3b`, `#{name unique}` and `#!eof` are
read. Boxes, fxvectors and stencil vectors are written back the same
way. The mats section above lists the rest, along with what `#!r6rs`
turns off.

## What e still needs

From the last run of all 63 scripts (five at a time, each under a
five-minute limit; `vttest-drive` needs a terminal and isn't counted,
and `roots.ss` is a helper the others include):

| cause | scripts |
|---|---|
| passing (33) | blame, buffet, copy, datum, delta-log, digest, edit-conflict, edit-position, format, head-sync, layers, local, log, mark, markdown, markdown-anchor, mode, navigation, paint, popup, reference, render, scope, string, style, surface, text, trash, tty, two-actors, undo, warm, window-link |
| engines (`make-engine`) | kernel, policy |
| annotations (source positions from the reader) | expression; expression-keys times out |
| run Chez itself (`scheme` on the path) | extensions, journal, lint |
| canonical paths (`/tmp` is `/private/tmp` on macOS) | file, markdown-view, wire, finder |
| over five minutes (they drive a pty or wait on workers) | copy-clipboard, interactive, startup-visit, terminal-process, wiring |
| library invocation is less lazy than Chez's | edoc |
| Chez's compile-on-import notifications | evaluation |
| threads' timing (a worker's deadline passes) | actor, startup |
| not yet diagnosed | app, edit-state, https, keys, mx, reload, sandbox, store, terminal, typing |

The main causes:

- **Engines** (`make-engine`, and timer interrupts with `set-timer`).
  e fuels evaluation with them (kernel, policy). Chez builds engines
  from a tick counter, a timer interrupt and `call/cc`. The same can
  be done here, because full continuations already mark every place
  a call might capture. A tick check at procedure entry, compiled only
  in `--chez` mode, would call the timer handler, which captures the
  continuation and returns to the engine's caller. Not done yet.
- **Annotations**: `get-datum/annotations` with source positions, which
  e's expression navigation reads Scheme files with. The reader keeps
  no positions; it needs a second, annotating reader.
- **Tests that run Chez itself** (`scheme` on the path) to compare
  against: extensions, journal, lint.
- **Paths through /tmp**: macOS's `/tmp` is `/private/tmp`, and e
  compares canonical paths (`markdown-view`, `file`, `wire`), which
  needs `realpath` semantics in path canonicalization.

## Behaviour that still differs

| | Chez 10.4.1 | Pseudoscheme |
|---|---|---|
| `(greatest-fixnum)` | 2⁶⁰ − 1 | 2⁶² − 1 (SBCL's fixnums, allowed by R6RS) |
| `(write 1e21)` | `1e21` | `1.0e21` |
| `(open-output-file f)`, `f` existing | an error (Chez's default option) | truncates, as R6RS code here expects; with an option, Chez's behaviour |
| library invocation | lazy, when a variable is first referenced | when an expression that refers to it runs |
| printer parameters (`print-radix`, `print-length`, ...) | honoured | accepted, not yet consulted by the writer |

## Not planned

Of the 597 names still missing, most are Chez's internals: `$`-system
procedures, compiler knobs (`cp0-*`, `enable-*`), boot files, fasl,
profiling and cost centers, the inspector, the 56 port-buffer
accessors, phantom bytevectors. The ones that matter
to programs, in rough order:

1. engines and timer interrupts (above);
2. ftypes (`define-ftype`, `ftype-ref`), on CFFI's structs;
3. annotations and source objects;
4. `define-record` and `record-case`, the old record syntax;
5. `meta-cond`, `extend-syntax`, `define-structure`;
6. continuation marks (Chez 10's), which would share prompts' frames
   (docs/continuations.md).
