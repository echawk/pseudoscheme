# Guile on Pseudoscheme: `--guile`

Status: **running.** `pseudoscheme --guile prog.scm` loads GNU Guile 3.0's
own `ice-9/boot-9.scm` and `ice-9/psyntax-pp.scm`, unmodified, from the
Guile installation on the machine. They run on libguile's primitives
written in Lisp and a Tree-IL compiler. After that, `define-module`,
`use-modules` and Guile's own modules are Guile's own code: `(ice-9 match)`,
`(ice-9 format)`, `(ice-9 pretty-print)`, `(ice-9 regex)`, `(srfi srfi-1)`
and the rest. On Guile's own test suite (vendored,
`vendor/guile-test-suite/`), **41,993 tests pass** so far. See "Where it
stands" below. Emacs Lisp has started: Guile's elisp compiler runs, and
`(compile '(defun ...) #:from 'elisp)` works. It is future work.

The sections after "Where it stands" are the plan as written before the
work started, with each stage marked as it now is.

## Summary

Do for Guile what docs/racket.md proposes for Racket: **implement the
layer that Guile's own Scheme code runs on, then run that code
unchanged.**

1. **libguile's primitives in Lisp.** These are the procedures Guile
   implements in C. Most have equivalents in the R6RS/R7RS runtime and
   the SRFIs we already have.
2. **A Tree-IL compiler.** Guile's expander (its own psyntax) produces
   Tree-IL, Guile's small core language. We would translate it to the
   core Scheme our translator already compiles to Lisp.
3. **Guile's own `boot-9.scm`, `psyntax-pp.scm`, and the modules in
   `ice-9/`, `srfi/`, `system/` and `rnrs/`**, loaded from an installed
   Guile's source tree and running on 1 and 2. Guile's module system,
   `define-module`, `use-modules`, its macros and its libraries would
   then be Guile's own code, not imitations of it.

The hard parts were **delimited continuations**, because Guile's
exceptions, `catch`/`throw`, parameters and REPL are built on prompts
(now done);
**Guile's top-level semantics**, where each module is a mutable
top-level environment rather than an R6RS library; and, for Guix,
**`(system foreign)`** (the FFI) and **compile time**.

## How Guile is layered

```
  your code, Guix, guile-json, ...         Scheme
  ice-9/*, srfi/*, system/*, oop/goops     Scheme (about 950 of libguile's C procedures referenced)
  ice-9/boot-9.scm                         Scheme, 4,800 lines: module system, exceptions,
                                           define-module, use-modules, load paths, records
  ice-9/psyntax-pp.scm                     Guile's psyntax (Dybvig's, adapted), pre-expanded;
                                           expands to Tree-IL
  ---------------------------------------------------------------------------------------------
  libguile (C)                             about 1,100 documented procedures, the evaluator,
                                           structs and vtables, variables and obarrays,
                                           fluids and dynamic states, prompts, ports, GC
  compiler (language tree-il -> cps -> bytecode), the VM, .go files
```

- `boot-9.scm` starts by defining a few procedures, then loads
  `psyntax-pp` (`(primitive-load-path "ice-9/psyntax-pp")`). From that
  point psyntax is `macroexpand`, and the rest of boot-9 (and everything
  after it) is expanded by it.
- Guile's psyntax builds its output with Tree-IL constructors
  (`build-call` is `make-call`, `build-lexical-reference` is
  `make-lexical-ref`, and so on). It uses 18 of them: `void`, `const`,
  `primitive-ref`, `lexical-ref`/`-set`, `toplevel-ref`/`-set`/`-define`,
  `module-ref`/`-set`, `call`, `primcall`, `conditional`, `seq`,
  `lambda`, `lambda-case`, `let` and `letrec`. Later compiler passes
  add `fix`, `let-values`, `prompt`, `abort` and `dynlet`, but the
  expander doesn't emit them.
- By the token scan, boot-9 refers to about 208 of the documented C
  procedures (74 of them R7RS names). With the common modules (`match`,
  `format`, `rdelim`, `popen`, `ftw`, `regex`, `optargs`, `posix`, the
  textual and binary ports, SRFI 1) it is about 350. All of Guile's
  bundled Scheme modules together refer to about 950. Many are
  internals that the documentation doesn't list (`%get-pre-modules-obarray`,
  `make-struct/no-tail`), so the real set needs a proper inventory.
- Guile's psyntax is the same lineage as ours (vendor/psyntax), but we
  would run **Guile's** copy for Guile code. It knows Guile's modules,
  `syntax-parameterize`, `define-syntax-rule`, its source-property
  conventions, and how Guile resolves top-level identifiers, none of
  which ours models.

## The plan

### Stage 0: the reader and the flag (done)

`--guile` selects a Guile top level and reader. The reader extensions:
- `#:kw` keywords: these already read as Lisp keywords, and Guile's
  keywords are disjoint objects, so they can stay Lisp keywords;
- `#!...!#` block comments (script headers);
- `#{odd symbol}#`;
- `#true`/`#false`, which R7RS already has;
- `#vu8(...)`, `#*101` bit vectors and `#,`/`#,@`;
- **`read-hash-extend`**, the user-defined `#` syntax. Guix's
  G-expressions are `#~`, `#$`, `#$@` and `#+`.

### Stage 1: libguile's primitives (largely done; see "Where it stands")

Written in Lisp, as the R6RS layer is (src/r6rs/), and grouped as
libguile is:
- **Already ours**, behind different names: lists (SRFI 1 is built
  into Guile), strings and SRFI 13/14, characters, numbers, symbols,
  bytevectors (R6RS), vectors, ports, `dynamic-wind`, hash functions.
- **Guile's own data:** hash tables (`make-hash-table`, `hash-ref`,
  `hashq-set!`, `hash-for-each`, and weak-key and weak-value tables);
  association lists (`assq-ref`, `acons`, `assoc-set!`); `1+`, `1-`;
  `logbit?`; `procedure-property` and `procedure-with-setter`;
  undefined and unspecified values.
- **Structs and vtables** (`make-vtable`, `make-struct/no-tail`,
  layouts), which Guile's records, SRFI 9, R6RS records and GOOPS are
  all built on, and applicable structs. These would be Lisp structs
  carrying a vtable pointer.
- **Variables and modules' C half:** first-class `variable` objects,
  obarrays, `current-module`, `module-variable`, `%get-pre-modules-obarray`,
  `make-syntax-transformer`, and `macroexpand` hooks.
- **Fluids and dynamic states** (`make-fluid`, `fluid-ref`,
  `with-fluids`, `with-dynamic-state`), which Guile's parameters are
  built on. They are Lisp special variables, per thread.
- **Prompts:** `make-prompt-tag`, `call-with-prompt`,
  `abort-to-prompt`, and `%exception-handler`. See stage 4.
- **Evaluation:** `primitive-eval`, `primitive-load`,
  `primitive-load-path` and `%load-path`. These call stage 2's compiler
  one top-level form at a time.

### Stage 2: a Tree-IL compiler (done)

Guile's psyntax calls the Tree-IL constructors. Ours would build core
Scheme forms (the `lambda`/`if`/`set!`/`letrec`/`begin` language that
`host-eval` in src/psyntax.lisp translates), and `primitive-eval` would
translate and compile them.
- `lexical-ref`, `lambda`, `let`, `letrec`, `seq`, `conditional`,
  `call` and `const` map directly.
- `lambda-case` (`case-lambda` with optional and keyword arguments, as
  `define*` and `lambda*` expand to) becomes a dispatcher over the
  argument list. Keywords are Lisp keywords, so CL's own keyword
  parsing can do some of the work.
- `toplevel-ref`/`-set`/`-define` and `module-ref`/`-set` go through
  **variable objects**: a reference resolves its module's variable once
  (the first time it runs, or at link time) and then reads the box.
  Guile's top levels are mutable, so a module can be redefined at the
  REPL and code that uses it sees the change. That has to stay true.
- `primcall` and `primitive-ref` name libguile primitives, and become
  direct calls to the stage 1 functions. The common ones (`car`,
  `vector-ref`, arithmetic) can be open-coded as the translator already
  does for R7RS.
- Each top-level form is compiled when it is evaluated, as `host-eval`
  does now. SRFI 257 showed what that costs when a program evaluates
  thousands of small forms: src/psyntax.lisp now shares compiled lambdas
  between forms of the same shape. Guile code leans on that path too.

### Stage 3: boot-9 and the module system (done, with a compiled-module cache)

Load `ice-9/boot-9.scm` from an installed Guile: it loads
`psyntax-pp.scm`, then defines the module system, load paths,
`define-module`, `use-modules`, autoloads, `#:select`, `#:renamer`,
`#:prefix`, `define-public` and the rest. After that,
`(use-modules (ice-9 match))` finds `ice-9/match.scm` on `%load-path`
and loads it like any other module.
- Guile's sources are LGPL-3.0-or-later. We would load them from the
  user's Guile installation, or from a Guile source tree they point us
  at, rather than vendor them.
- **Compiled modules.** Guile compiles each module to a `.go` file
  once. Expanding and compiling boot-9 and `ice-9` from source on every
  start would take seconds, so compiled modules must go into the
  library cache (src/library-cache.lisp), keyed by the module file and
  its dependencies.

### Stage 4: delimited continuations (done)

Guile 3's exceptions, `with-exception-handler`, `catch`/`throw`,
`raise-exception`, `false-if-exception`, the REPL's error recovery, and
much of `(ice-9 control)` and fibers are built on `call-with-prompt` and
`abort-to-prompt`. They are native now, on the frames the full
continuations already keep (src/continuations.lisp, "Delimited
continuations"):

- A prompt is a frame with a `catch`. An abort copies the frames above
  the prompt and throws to it, so an abort whose handler ignores the
  continuation costs a copy of those frames and a Lisp `throw`.
- Calling the continuation rebuilds the copied frames on top of the
  caller's stack, the same way a re-entered full continuation is
  rebuilt, without a throw to a base. It can be called any number of
  times, and `dynamic-wind`s inside it run their befores again.
- `(pseudoscheme control)` (src/control.sls) adds Guile's `%`,
  `shift`/`reset`, `call/ec` and `let/ec`.
- Guile code should run with full continuations, the default. With
  `--continuations=escape`, aborts work but their continuations can't
  be called.

### Stage 5: ports and POSIX (done, but sockets)

- **Ports:** Guile's ports include custom ports, soft ports, string
  and bytevector ports, `(ice-9 rdelim)`, `(ice-9 textual-ports)` and
  `(ice-9 binary-ports)`. Most map onto the R6RS port layer
  (src/r6rs/ports.lisp), which is built on Gray streams.
- **POSIX** (`(ice-9 posix)` is mostly C): `stat`, `opendir`, `mkdir`,
  `chdir`, `getenv`/`setenv`, `environ`, pipes, `system*`, `fork`,
  `execl`, `waitpid` and sockets. These go onto `sb-posix` (or osicat
  and iolib to be portable). `fork` in a threaded Lisp is risky:
  `(ice-9 popen)` should be implemented with `uiop:launch-program` or
  `posix_spawn` instead of `fork` plus `exec`.
- `(ice-9 regex)` is POSIX extended regular expressions. CL-PPCRE is
  Perl-flavoured, so the syntax needs a translation layer, or calls to
  libc's `regcomp`/`regexec` through CFFI, which matches Guile exactly.

### Stage 6: `(system foreign)` on CFFI (done)

Guile libraries bind C with `(system foreign)`: `pointer->procedure`,
`procedure->pointer`, `make-c-struct`/`parse-c-struct`,
`bytevector->pointer` and `pointer->bytevector`, and
`(system foreign-library)`'s `load-foreign-library` and
`foreign-library-function`. Guile's two modules load unmodified. Their
C halves (`scm_init_foreign`, `scm_init_system_foreign_library`) are
src/guile/foreign.lisp, on the foreign-function layer the Chez mode
uses too (src/ffi.lisp, system `pseudoscheme/cffi`).
- `pointer->procedure` takes the C types at run time. SBCL's compiler is
  there at run time, so each call compiles a CFFI call stub, without
  libffi. `procedure->pointer` compiles a CFFI callback the same way.
- **`bytevector->pointer`.** Guile's GC doesn't move bytevectors, so
  the pointer stays valid; SBCL's does move them. A pointer made from
  a bytevector is therefore a place in it, not an address: whenever it
  is passed to C, the bytevector is pinned for the call, so C reads and
  writes the bytevector itself. `qsort` on a bytevector, through a
  Scheme comparison procedure, sorts it in place. C must not keep such
  a pointer after the call returns.
- `pointer->bytevector` of C's memory copies it: a Lisp vector can't be
  laid over memory it didn't allocate, so writes to the copy don't reach
  C.
- Structs and complex numbers are read and written in memory
  (`make-c-struct`, `parse-c-struct`), but not passed to or returned
  from C by value: CFFI does that only through libffi.
- Pointers to the same address are `eq?`, as `%null-pointer` and
  `(make-pointer 0)` must be.
- This is what Guix needs from C: guile-gcrypt (libgcrypt), guile-git
  (libgit2, through bytestructures), guile-sqlite3, guile-zlib,
  guile-lzlib and guile-zstd all use `(system foreign)`.
  tests/programs/c-libraries.scm already shows zlib and libc called
  through CFFI.
- C extensions written against libguile's C API (guile-gnutls,
  guile-avahi, guile-ssh) can't be loaded at all. They would need
  replacements: cl+ssl for TLS, say, behind the same Scheme interface.

### Stage 7: GOOPS (done)

GOOPS (`(oop goops)`) is Guile's CLOS-like object system, written in
Scheme over C primitives for structs, applicable structs and method
dispatch caches. There are two routes:
- run `oop/goops.scm` itself, on stage 1's structs; or
- map GOOPS onto CLOS: `define-class`, `define-generic` and
  `define-method` become `defclass`, `defgeneric` and `defmethod`. That
  would make GOOPS objects real CLOS instances that Lisp code can
  specialize on (docs/interop.md's "Scheme-syntax CLOS" item, ROADMAP
  4). It would lose GOOPS's own MOP where the two differ.

How much Guix's client side depends on GOOPS is for the inventory
("First steps") to say. Unless it says otherwise, this can wait.

## Guix

Guix is the stress test: hundreds of modules, tens of thousands of
package definitions, heavy macro use, the FFI, and a protocol to a
daemon. What its **client side** (the `guix` command, `(guix packages)`,
`(gnu packages ...)`) needs beyond stages 0–6:
- **`(guix records)`**: `define-record-type*`, a large `syntax-case`
  macro with `this-record`, thunked and delayed fields, and
  inheritance. It is a good test of stages 2–3, since Guile's psyntax
  runs it and only the output has to be right.
- **G-expressions**: `#~`/`#$`/`#+` reader syntax (stage 0's
  `read-hash-extend`) and the `(guix gexp)` macros.
- **The store protocol**: `(guix store)` talks to `guix-daemon` (a
  separate C++ program, which we don't touch) over a Unix socket. It is
  pure Scheme on bytevectors and binary ports, plus hashing through
  guile-gcrypt (stage 6, or reimplemented on Ironclad).
- **Monads**: `(guix monads)` is plain Scheme.
- **Scale**: `(gnu packages ...)` is several hundred modules. Loading
  them cold through the expander and SBCL would take minutes, so stage
  3's compiled-module cache is required.
- **Builders**: the code that runs inside builds (`(guix build utils)`
  and the build systems) is executed by the daemon with the Guile from
  the store. It would keep running on real Guile, and we only need to
  produce the same derivations.

A concrete milestone: for a package such as `hello`, compute the same
derivation file name (`/gnu/store/...-hello-<version>.drv`) that
`guix build -d hello` prints, talking to a real `guix-daemon`. That proves package
evaluation, records, G-expressions, monads, the store protocol and
hashing all match Guile byte for byte.

## Alternatives considered

- **A compatibility library on our own psyntax** (a `(guile)` library
  with `define-module` as a macro over R6RS libraries, `use-modules`,
  hash tables, `assq-ref` and the like). It is cheaper to start, and
  enough for scripts that use a little of Guile. But Guile's module
  reflection (`resolve-module`, `module-ref`, `module-define!`,
  `current-module` passed to `eval`), its mutable top levels, and its
  psyntax's extensions would be imitations that diverge, and every
  library would find a new divergence. It could still be a stepping
  stone before stage 3, as long as nothing is built on it that stage 3
  would have to undo.
- **Running Guile's compiled `.go` files** (ELF files of VM bytecode)
  with a VM written in Lisp. That would mean emulating Guile's VM
  instead of compiling, which loses the reason to be on SBCL.
- **Compiling from CPS**, Guile's lower intermediate language. That is
  more work than Tree-IL for little gain, since our translator already
  wants something close to Tree-IL.

## Risks and costs

- **libguile's breadth.** Hundreds of primitives, each with Guile's
  exact error behaviour. Guile code relies on errors (`catch #t`), and
  sometimes on messages and keys (`'wrong-type-arg`, `'system-error`).
- **Prompts' cost.** Every `with-exception-handler` installs a prompt.
  If prompts are slow, everything is. The escape-only fast path in
  stage 4 is essential.
- **Compile time.** SBCL's compiler is slow next to Guile's bytecode
  compiler, and Guile's top-level-at-a-time evaluation produces many
  small forms. The shared-lambda cache (src/psyntax.lisp) helps with
  repeated shapes, but not with genuinely new code. The compiled-module
  cache, and possibly interpreting run-once top-level forms, matter
  more here than anywhere else.
- **Licensing.** Guile is LGPL-3.0-or-later and Guix is
  GPL-3.0-or-later. Loading their sources at run time is fine. Vendoring
  them, or shipping their compiled forms in our image, needs care.
- **Threads.** Guix uses threads (`par-for-each`, the substitute
  machinery) and Guile's fluids are per-thread. Parameters can now be
  per thread (a thread's own table of values, which Chez's
  `fork-thread` threads get; src/r7rs/rts.lisp); Guile's threads would
  get the same. ROADMAP section 4's expansion lock is still needed.

## Where it stands

### How it is built

All of it is in `src/guile/`, system `pseudoscheme/guile`:

| file | what |
|---|---|
| reader.lisp | Guile's reader: `#:kw`, `#{odd}#`, `#!...!#`, `#nil`, `#*101`, `#vu8(...)`, arrays, `read-hash-extend` |
| runtime.lisp | structs and vtables (applicable structs are funcallable instances, so they are procedures), variables, Guile's hash tables (one table for `hashq-`/`hashv-`/`hash-`, handles shared with the table), fluids, syntax objects, macros, procedure properties, hooks, `scm-error` |
| compile.lisp | Tree-IL to Pseudoscheme's core Scheme, and a pre-expander standing in for libguile's `expand.c` until psyntax is loaded |
| boot.lisp | the module system's C half (`module-variable` and friends over boot-9's module records), `primitive-eval`, `primitive-load`, the root module's bindings, errors, the REPL |
| ports.lisp | Guile's ports: binary and textual at once, any encoding, Guile's custom ports, and the C halves of `(ice-9 ports)`, `(ice-9 binary-ports)`, `(ice-9 custom-ports)`, `(rnrs io ports)`, rdelim and rw |
| write.lisp | `write` and `display` as libguile's print.c writes, following the print options |
| goops.lisp | GOOPS's C half: `class-of` for every object, classes for records and libguile's types, primitive generics |
| arrays.lisp | Guile's arrays: shared views of a root vector, typed (SRFI 4 vectors are tagged bytevectors) |
| cache.lisp | the compiled-module cache: Guile's `.go` files, as fasls |
| primitives.lisp, threads.lisp, regex.lisp, foreign.lisp, i18n.lisp | the rest of libguile so far, and the other C halves that Guile's modules install with `load-extension` (threads, regular expressions, `(system foreign)`, locales, popen, SRFI 60, ...) |
| modules/ | `(language tree-il spec)`, Guile's own being written for its VM: written afresh, not an edited copy |

How each problem was solved:

- **Guile's sources are never copied or changed.** They are LGPL. They are
  read from the installation (`guile -c '(display (%package-data-dir))'`,
  the usual prefixes, or `GUILE_SOURCE_DIR`). `src/guile/modules/` comes
  first on `%load-path`, for the replacement.
- **Tree-IL to core Scheme.** psyntax builds Tree-IL with the
  constructors in `%expanded-vtables`. `compile-tree-il` turns each node
  into the core forms `host-eval` (src/psyntax.lisp) already compiles:
  - lexical variables are renamed apart (`~1`, `~2`, ...);
  - top-level and `@`/`@@` references go through a *site*, which caches
    the variable once it has been looked up, as Guile's compiled code does;
  - `lambda-case`, with `#:optional`, `#:key` and several clauses,
    becomes a dispatch on the argument count followed by ordered binding;
  - a primcall becomes the host's procedure where it behaves the same,
    and is otherwise a call of the `(guile)` binding.
  `(language tree-il)`'s record nodes (`<prompt>`, `<abort>`,
  `<let-values>`, `<fix>`) compile too.
- **Before psyntax exists**, `macroexpand` is a pre-expander written in
  Lisp. It knows the core forms that boot-9's first page and
  `psyntax-pp.scm` use, and builds the same Tree-IL. `psyntax-pp.scm` is
  one top-level `let` around a `letrec*` of 400 procedures, too big for
  one SBCL code object. The lets around it become top-level definitions,
  and the `letrec*` is hoisted the way psyntax's own image is
  (`hoist-definitions`).
- **The C half of the module system** reads boot-9's module records by
  field position, as `modules.h` does. Duplicate imports are resolved by
  boot-9's own duplicate handlers.
- **boot-9's `dynamic-wind`, `with-fluid*`, `apply`, `call/cc` and
  `call-with-prompt`** are written on libguile's internal `wind`/`unwind`
  and `push-fluid`. Their definitions are replaced, as they are made, by
  Pseudoscheme's own continuation-aware procedures (`*boot-overrides*`).
- **Fluids** are the R7RS layer's parameter states, so they are
  per-thread and survive re-entered continuations. They keep the stack of
  outer values that `fluid-ref*` reads, which boot-9's `raise-exception`
  walks. The current-port fluids hold Lisp's `*standard-output*` and the
  other stream variables, so Pseudoscheme's own I/O follows
  `(current-output-port)`.
- **Errors.** A Lisp error inside Guile code is raised as a Guile
  exception (`wrong-type-arg`, `unbound-variable`, `misc-error`, ...)
  where it happens, so `catch`, `with-exception-handler` and
  `false-if-exception` see it. Each entry point runs inside one
  continuation base, so prompts set up by a `catch` around `eval` are
  visible to the code evaluated.
- **C extensions.** `load-extension` runs a Lisp version of the C init
  function where there is one (ports, fports, ioext, threads, control,
  i18n, bytevectors, rdelim, rw). After that, every variable the module
  exports and nothing has defined gets Pseudoscheme's procedure of the
  same name, if it has one. That is how `(rnrs bytevectors)`, SRFI 4 and
  others get their C procedures.
- **Ports** are Gray streams over a byte backend (a file descriptor, a
  string or bytevector buffer, a custom port's procedures, a Lisp
  stream). Like Guile's, each is binary and textual at once, with read
  and write buffers, an encoding and conversion strategy that can be
  changed at any time (UTF-8, UTF-16, UTF-32, Latin-1, ASCII and other
  single-byte ones), unread characters going back into the read buffer,
  line and column, seeking and truncation. Guile's own `(ice-9
  binary-ports)`, `(ice-9 textual-ports)` and `(ice-9 custom-ports)` load
  on that unmodified. Lisp's `*standard-output*` and friends are wrapped
  as ports while Guile code runs, so output captured in Lisp still is.
- **The writer** is libguile's: `#{odd symbol}#` (or `|...|` with the
  `r7rs-symbols` print option), Guile's string escapes and character
  names, `#<eof>`, `#<procedure name (_ _)>`, `#<output: file 1>`.
  Floats are printed with the fewest digits in any radix (Burger and
  Dybvig, with Guile's choice of when to use an exponent).
- **GOOPS** is Guile's own `oop/goops.scm`. Classes are vtables and
  instances structs, as in Guile 3; the C half supplies `class-of`,
  classes for records and libguile's types, `%init-layout!`,
  `%modify-instance`, and primitive generics: a primitive given a
  method (`+`, `write`, `equal?`) dispatches to its generic when it fails
  on its arguments.
- **Arrays** are views of a root vector (an offset, and bounds and a
  stride per dimension), so `make-shared-array`, `transpose-array`,
  slices and lower bounds work; a zero-based vector is its own array.
  SRFI 4 vectors are bytevectors tagged with their element type, as in
  Guile 3.
- **Regular expressions** are libc's `regcomp`/`regexec` through CFFI.
  That is POSIX, as Guile's are. macOS's `regcomp` rejects empty
  alternatives, `(|x)`, which glibc accepts; on macOS they become `(x)?`.
- **`#nil`**, Emacs Lisp's nil, is a distinct object. Guile's `not`,
  `null?` and `boolean?` know it, and a conditional's test goes through
  a `#nil` check unless it is a predicate's call or a comparison.
- **Booting into the image.** The command-line program boots boot-9 when
  it is built, so `--guile` starts in under a second.
- **The compiled-module cache.** A module loaded from the load path is
  expanded (as Guile's `compile-file` expands, in `c` mode), translated
  and compiled once; its code is then written with `compile-file` to a
  fasl under `~/.cache/pseudoscheme/guile/`, keyed by the file, its date
  and the Pseudoscheme it was compiled by. The next load is the fasl's:
  twelve large modules (GOOPS, SRFI 19, the web, PEG, ...) load in 6 s
  instead of 49. The literals in compiled code (modules, syntax objects,
  variable sites) have load forms; a file whose code holds anything else
  isn't cached. `PSEUDOSCHEME_GUILE_CACHE=0` turns it off. Files loaded
  with `load` are evaluated as read, as Guile's evaluator does.

### Guile's test suite

`make test-guile` runs each of the 186 files in
`vendor/guile-test-suite/tests` in its own process, 6 at a time, each
under 5 minutes (tests/run-guile-tests.sh). It reports Guile's own
result kinds. With the module cache filled the whole suite takes about
eight minutes.

| | first run | now |
|---|---|---|
| pass | 5,982 | 41,993 |
| fail | 191 | 73 |
| error (an exception where a result was expected) | 465 | 100 |
| unresolved / unsupported / untested / xfail | 14 / 12 / 1 / 3 | 94 / 15 / 7 / 4 |
| files that crashed or hit the time limit | 32 | 1 |

(That run lacked the `threads` feature, provided again since: srfi-18
adds 61 passes, and threads.test hits the time limit.)

The biggest files pass entirely or nearly: `numbers` (28,987 of
28,991), `srfi-1` (1,902), `regexp` (1,089), `srfi-67` (902), `r4rs`
(538), `arrays` (564), `srfi-13` (533 of 534), `syntax` (251),
`bytevectors` (238 of 239), `goops` (237), `srfi-60` (168), `peval`
(163, all), `tree-il` (151), `srfi-4` (148), `optargs` (120, all),
`reader` (115), `foreign` (79), `print`, `format`, `weaks`,
`parameters`, `r6rs-enums`, `r6rs-arithmetic-fixnums`, `sort`, `popen`,
`ports`.

What fails now, by cause:

- **Guile's VM** (docs/guile-vm.md) runs Guile's bytecode: `rtl`,
  `rtl-compilation` and `dwarf` pass. Left: frames and stacks
  (`coverage`, `statprof`, part of `eval`), `types`, continuations
  captured in bytecode.
- **libguile's internals**: immutable literal strings, copy-on-write
  shared substrings (`substring/shared` copies), guardians (SBCL can't
  give back a collected object), GC behaviour (`strings`, `guardians`,
  `gc`).
- **Emacs Lisp and ECMAScript** (`elisp*`, `ecmascript`): future work.
- **A long tail** of exact error keys and messages, and smaller missing
  pieces (sockets, `sandbox`, source properties, some POSIX).

### Differences from Guile that remain

| | Guile 3.0 | here |
|---|---|---|
| compiling | Tree-IL → CPS → bytecode (`.go` files cached) | Tree-IL → core Scheme → SBCL native code (fasls cached) |
| `(system vm ...)`, `(language cps)`, the optimizer | present | Guile's passes run when compiling to `cps` or `bytecode`; bytecode can't be loaded. To `value` (`compile`'s default), Tree-IL goes to native code as it is |
| GOOPS | present | present (`goops.test`: all 237) |
| `(system foreign)` | present | present on CFFI (`foreign.test`: 79 pass, 0 fail). Structs aren't passed by value; `pointer->bytevector` of C's memory copies it; the deprecated `dynamic-link`/`dynamic-func` are absent |
| `bytevector-slice` | shares the bytes | copies them |
| port buffers, `(ice-9 suspendable-ports)` | Guile's | ports have their own buffers, which `(ice-9 ports internal)` doesn't expose: suspendable ports are absent |
| stacks, frames, backtraces | Guile's VM's | none: `make-stack` is #f |
| locales | the C library's | the C library's (newlocale); case mapping is SBCL's Unicode |
| stack overflow handlers | a limit in words | the control stack's own limit |
| recursion depth | memory | about 60,000 non-tail calls in a thread other than the main one: each binds `*fstack*` (full continuations), and SBCL's binding stack is of fixed size. threads.test's `par-map` of 10,000 elements nests that many futures in a worker thread, and doesn't finish |

### Emacs Lisp (future work)

Guile's elisp front end (`language/elisp/`) loads and runs unmodified,
through the replacement `(language tree-il spec)`, whose native compiler
goes from Tree-IL to `value`:

```scheme
(compile '(progn (defun sq (x) (* x x)) (sq 7)) #:from 'elisp #:to 'value)  ; => 49
(compile '(if nil 1 2) #:from 'elisp #:to 'value)                           ; => 2
```

Loading `language/elisp/boot.el` stops at the first missing piece
(`*random-state*`, now defined; the next one is unknown). Still to do:

- `#nil` as the end of a list in the host's list procedures (only
  `apply` accepts it so far);
- `#nil` and `#t` as Scheme's when elisp values reach Scheme code;
- `elisp.test`, `elisp-compiler.test` and `elisp-reader.test`, which
  currently crash.

### Next steps

1. The long tail of the suite: exact error keys, sockets, the sandbox,
   source properties.
2. Running Guile's lowerer (peval) before native compilation, which
   needs its primcalls for dynamic extents (`push-fluid`, prompts)
   compiled structurally.
3. A Scheme library putting the FFI layer (src/ffi.lisp) in front of
   R5RS, R6RS and R7RS programs.
4. Then Emacs Lisp, and Guix's client side.

The earlier `(guile)` R6RS library (src/guile/guile.lisp, guile.scm), which
imitates Guile's everyday procedures for R6RS programs, remains. It is
separate from all of this.
