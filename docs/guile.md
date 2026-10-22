# Guile on Quasischeme: `--guile`

Status: **running.** `quasischeme --guile prog.scm` loads GNU Guile 3.0's
own `ice-9/boot-9.scm` and `ice-9/psyntax-pp.scm`, unmodified, from the
Guile installation on the machine. They run on libguile's primitives
written in Lisp and a Tree-IL compiler. After that, `define-module`,
`use-modules` and Guile's own modules are Guile's own code: `(ice-9 match)`,
`(ice-9 format)`, `(ice-9 pretty-print)`, `(ice-9 regex)`, `(srfi srfi-1)`
and the rest. On Guile's own test suite (vendored,
`vendor/guile-test-suite/`), **42,702 tests pass** so far. See "Where it
stands" below. Emacs Lisp runs too: `,language elisp` at the REPL, and
Guile's three elisp test files pass in full (below).

After `--guile` the command line is Guile's own (`guile --help`): FILE,
`-s`, `-c`, `-l`, `-e`, `-L`, `-C`, `-x`, `-q`, `--use-srfi`, `--` and
the rest. Guile's own `compile-shell-switches`, from the installation's
`(ice-9 command-line)`, turns it into the expression to run, as `guile`
does. Quasischeme's own options go before `--guile`; run as `guile` (a
link to bin/quasischeme), the program is `--guile`.

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
  built on. A fluid's value is per thread. A dynamic state is a table of
  every fluid's value, which `with-dynamic-state` makes the thread's for
  its extent; thread-local fluids (`%exception-handler` among them) stay
  out of it, as in Guile.
- **Atomic boxes** (`(ice-9 atomic)`), by compare-and-swap.
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
- `(quasischeme control)` (src/control.sls) adds Guile's `%`,
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
uses too (src/ffi.lisp, system `quasischeme/cffi`).
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

### A first trial (2026-10-12, on macOS)

Guix from its Git repository, with guile-json, guile-gcrypt,
guile-sqlite3, guile-git and guile-bytestructures from theirs. Their
`.in` files were filled in by hand, since configure wasn't run. They are
found through `GUILE_LOAD_PATH`. guile-zlib wasn't found under its old
address, and is only needed for compression.
- **What loads:**
  - `(guix utils)`, `(guix records)`, `(guix gexp)`, `(guix store)`,
    `(guix packages)` and `(guix build-system gnu)`;
  - then, from `(gnu packages base)`, dozens of package modules: perl,
    python, golang and rust, in about 6½ minutes cold.
- **Where it stops:** at Guix's own Linux and Hurd assumptions.
  - `rust.scm` asks for the `i686-linux-gnu` platform.
  - Platforms are discovered by listing `guix/platforms/` with
    `scandir*`, which calls glibc's `readdir64` and decodes Linux's
    `struct dirent`. Neither exists on macOS.
  - Going further needs Linux (a container), not more of Guile.
- **Fixed in Guile mode along the way:**
  - `GUILE_LOAD_PATH` and `GUILE_LOAD_COMPILED_PATH` are read at start-up.
  - `%load-compiled-path` holds the module cache's directory. Guix
    expects it not to be empty.
  - `dynamic-link`, `dynamic-func`, `dynamic-pointer`, `dynamic-call` and
    `dynamic-object?` are libguile's wrappers of `(system foreign-library)`.
  - `dlsym` falls back to our own `strverscmp`, which macOS's libc lacks
    and `(guix utils)` uses.
  - A file whose forms total more than 1.5M conses isn't put in the module
    cache: one COMPILE-FILE of `(guix build syscalls)` (2M) exhausted
    8 GB, though compiling its forms as it loads takes 28 s.

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

All of it is in `src/guile/`, system `quasischeme/guile`:

| file | what |
|---|---|
| reader.lisp | Guile's reader: `#:kw`, `#{odd}#`, `#!...!#`, `#nil`, `#*101`, `#vu8(...)`, arrays, `read-hash-extend` |
| runtime.lisp | structs and vtables (applicable structs are funcallable instances, so they are procedures), variables, Guile's hash tables (one table for `hashq-`/`hashv-`/`hash-`, handles shared with the table), fluids, syntax objects, macros, procedure properties, hooks, `scm-error` |
| compile.lisp | Tree-IL to Quasischeme's core Scheme, and a pre-expander standing in for libguile's `expand.c` until psyntax is loaded |
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
  Quasischeme's own continuation-aware procedures (`*boot-overrides*`).
- **Fluids** are the R7RS layer's parameter states, so they are
  per-thread and survive re-entered continuations. They keep the stack of
  outer values that `fluid-ref*` reads, which boot-9's `raise-exception`
  walks. The current-port fluids hold Lisp's `*standard-output*` and the
  other stream variables, so Quasischeme's own I/O follows
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
  exports and nothing has defined gets Quasischeme's procedure of the
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
  fasl under `~/.cache/quasischeme/guile/`, keyed by the file, its date
  and the Quasischeme it was compiled by. The next load is the fasl's:
  twelve large modules (GOOPS, SRFI 19, the web, PEG, ...) load in 6 s
  instead of 49. The literals in compiled code (modules, syntax objects,
  variable sites) have load forms; a file whose code holds anything else
  isn't cached. `QUASISCHEME_GUILE_CACHE=0` turns it off. Files loaded
  with `load` are evaluated as read, as Guile's evaluator does.

### Guile's test suite

`make test-guile` runs each of the 186 files in
`vendor/guile-test-suite/tests` in its own process, 6 at a time, each
under 5 minutes (tests/run-guile-tests.sh). It reports Guile's own
result kinds. With the module cache filled the whole suite takes about
eight minutes.

| | first run | now |
|---|---|---|
| pass | 5,982 | 43,278 |
| fail | 191 | 4 |
| error (an exception where a result was expected) | 465 | 1 |
| unresolved / unsupported / untested / xfail | 14 / 12 / 1 / 3 | 89 / 10 / 15 / 6 |
| files that crashed or hit the time limit | 32 | 0 (with the module cache filled) |

The biggest files pass entirely or nearly: `numbers` (all 28,991),
`srfi-1` (1,902), `regexp` (1,092), `srfi-67` (902), `r4rs` (538),
`arrays` (564), `srfi-13` (534), `syntax` (254), `bytevectors`
(239), `goops` (237), `web-http` (234), `srfi-60` (168), `peval`
(163), `tree-il` (154), `srfi-4` (148), `srfi-207` (136), `control`
(127), `optargs` (120), `reader` (115), `srfi-105` (107), `foreign`,
`print`, `format`, `weaks`, `parameters`, `r6rs-enums`,
`r6rs-arithmetic-fixnums`, `sort`, `popen`, `ports`, `web-server`,
`net-db`, `00-socket` (32; the rest need `fork`), `coverage` (11),
`statprof`, `procprop`, `session`, `hooks`, `signals`, `unicode`, `posix`,
`elisp` (87), `elisp-compiler` (99), `ecmascript` (32).

What fails now, by cause:

- **Native code, not bytecode**: code a test file defines, and boot-9,
  are SBCL code here, so the VM's hooks don't see them (2 of
  `coverage`).
- **Conservative stack scanning**: a dead object whose last reference
  is a stale word in a live frame of the stack stays reachable, so a
  guardian doesn't return it (`weaks`' "weak-key dies", and, depending on
  how the frames fall, `fluids`' "fluids are GC'd" and `threads`' "mutex
  with owner not retained"). SBCL reports such a root with no frame
  around it, but a broken frame walk looks the same, so it can't be
  ignored safely.
- **The environment**: locales not installed here (`i18n`, `time`,
  `web-uri`, `encoding-escapes`, `regexp`), `fork` (`00-socket`): these
  are unresolved, not failures.

Fixed since the last count: `types` (all 47: a `(system base types)`
written for Quasischeme decodes `object-address`'s bits, which are now
Guile's for immediates and a stable number for heap objects),
`strings` (all 136: `substring/shared` remembers where it came from, and
a root mutator's change is copied to the strings sharing characters),
and `suspendable-ports` (514 pass: a port's buffers are Guile's own
`#(bytevector cur end has-eof? position)` vectors, shared with
`(ice-9 suspendable-ports)`).

### Differences from Guile that remain

| | Guile 3.0 | here |
|---|---|---|
| compiling | Tree-IL → CPS → bytecode (`.go` files cached) | Tree-IL → core Scheme → SBCL native code (fasls cached) |
| `(system vm ...)`, `(language cps)`, the optimizer | present | Guile's passes run when compiling to `cps` or `bytecode`; bytecode can't be loaded. To `value` (`compile`'s default), Tree-IL goes to native code as it is |
| GOOPS | present | present (`goops.test`: all 237) |
| `(system foreign)` | present | present on CFFI (`foreign.test`: 79 pass, 0 fail). Structs aren't passed by value; `pointer->bytevector` of C's memory copies it |
| `bytevector-slice` | shares the bytes | copies them |
| port buffers, `(ice-9 suspendable-ports)` | Guile's | ports have their own buffers, which `(ice-9 ports internal)` doesn't expose: suspendable ports are absent |
| stacks, frames, backtraces | Guile's VM's | frames from the VM and from native code (stacks.lisp); the VM's next and apply hooks (`(system vm coverage)`, statprof) run while the trace level is above zero |
| locales | the C library's | the C library's (newlocale); case mapping is Unicode's simple mappings, a character at a time, as Guile's |
| stack overflow handlers | a limit in words | the control stack's own limit |
| recursion depth | memory | the control stack: 500 MB in the main thread (as run by the tests), 256 MB in a thread Guile makes |

### Speed

A program file given to `--guile` is read and evaluated form by form in
`(guile-user)`: psyntax (Guile's own) expands each form to Tree-IL, which
compile.lisp turns to core Scheme and SBCL compiles to native code. Guile's
evaluator, its compiler to bytecode and the VM are not involved; the VM runs
only `.go` files that are loaded.

What the native code costs, against `--r7rs` on the same program (the
r7rs-benchmarks, `bench/run.sh quasischeme-guile`):

- **Top-level references.** A call of a top-level procedure, `(fib ...)`,
  reads its variable each time (a *site*, cached after the first lookup;
  inline in the caller), because Guile lets any module variable be
  redefined; `--r7rs` calls a procedure it has just defined directly. A call
  through a variable is also a point where a continuation may be captured, so
  it gets a frame, which a known procedure that can't capture doesn't.
- **Calls of a procedure by its own name.** The continuation transformation
  (src/continuations.lisp, `GUARD-SELF-CALLS`) compiles a top-level
  definition that calls itself through its variable, and calls nothing else
  that can capture, three times: `F`, the body with each such call made "if
  the variable still holds this procedure, call `F` directly, without a
  frame"; `M`, the body as written; and the procedure defined, which runs `F`
  while its variable holds it and `M` once it doesn't (an old reference
  called after a redefinition, a wrapper calling what it wraps). Each call
  still reads the variable, so a redefinition is seen at the next call, as
  in Guile. The one difference: if, while `F` runs, something it calls
  without capturing (a primitive such as `variable-set!`, another thread)
  gives the variable a new value, `F` calls that value in a barrier frame,
  and a continuation captured in it is an error to re-enter after the call
  returns. A procedure that `set!`s or defines its own name isn't compiled
  so. This is not Guile's letrectify (below): `(guile-user)`, where a script
  runs, is not declarative, so Guile itself calls through the variable
  there.
- **Root primitives.** Guile's compiler turns `(+ a b)`, `(car x)`, `(< a b)`
  and the like, when the name is the `(guile)` module's variable, into
  primcalls (its `resolve-primitives` pass), which are open-coded. The native
  compiler does the same (`*RESOLVABLE-PRIMITIVES*` in compile.lisp), at
  compile time and by variable, as Guile does, so a module that defines its
  own `+` keeps it. Module references, `(@@ (guile) memv)`, which boot-9's
  macros (`case`) expand to, are resolved too, as in Guile's pass, but only
  if the module is loaded already. `memq` and `memv` of a short constant
  list are tests in line, as Guile's peval makes them in a test.
  Comparisons and `-` of two arguments go through helpers (`%guile-<` and so
  on) with a fixnum case inlined by a compiler macro, and a wrong type is
  Guile's error from the root procedure. Before this every `+` and `<` was a
  call of a rest-argument procedure, which made fib 20 times slower than it
  need be. Out of line, the helpers do flonums (and a flonum with a fixnum)
  with the host's arithmetic too, before the root procedure, which costs a
  lookup by name and a list of the arguments: that made `fibfp` 20 times
  faster (70 s to 3.4 s) and `mbrot` 7 (37 s to 5.2 s), both now faster
  than in Guile 3.0.11.
- **`map` and `for-each`** are boot-9's, in Scheme, checking the list for
  circularity as they go. A procedure and one or two proper lists of one
  length (so every error but not the common case) go to the host's
  frame-aware `map`, which is in order and re-entrant, 4 to 5 times faster.
- **Allocation, GC and the rest** are those of every mode.

On the r7rs-benchmarks (`bench/run.sh quasischeme-guile`), `fib`, `tak`,
`lattice` and `nqueens` now run 1.3 to 1.6 times as long as with `--r7rs`
(they ran 2 to 4 times as long). Left over are the reads of the variable
that guard each call of a procedure by its own name, and the `#nil` checks
of conditionals.

Guile's own passes, against what the native compiler does:

| Guile pass | here |
|---|---|
| resolve-primitives | the same, by variable at compile time (above) |
| peval | `memq`/`memv` of constant lists only; no inlining of procedures |
| letrectify (declarative modules: a module's single, unassigned definitions are lexical within the compilation unit) | not done: each top-level form is compiled apart. A call of another procedure of a declarative module, already defined and unable to capture, needs no frame (`SAFE-SITE-P`) |
| fix-letrec, contification | psyntax's letrec of lambdas is a `LABELS`, which SBCL compiles as local functions and loops |
| closure conversion, type inference, unboxing (CPS) | SBCL's |

#### Native code or bytecode

Guile code can also be compiled by Guile's own compiler to bytecode and
run on the VM (docs/guile-vm.md): a `.go` file, from the installed Guile
or from `compile-file` here. Measured on arm64 (2026-10-17, a busy machine,
so within 20% or so), the r7rs-benchmarks programs as `bench/run.sh
quasischeme-guile` runs them, natively, against the same file compiled by
`guild compile -O2` and loaded with `load-compiled`, run time only, in
seconds:

| | native | VM, `vop` | VM, `jit` | VM, `interpret` | Guile 3.0.11 |
|---|---|---|---|---|---|
| fib | 4.4 | 13.1 | | | 4.9 |
| tak | 2.8 | 7.4 | 19.4 | 154 | 4.7 |
| nqueens | 10.1 | 24.9 | | | 14.6 |
| deriv | 6.7 | 22.2 | | | 9.4 |
| destruc | 3.0 | 8.6 | | | 4.4 |
| browse | 7.1 | 15.4 | | | 4.8 |
| sum | 3.4 | 6.9 | | | 5.8 |
| nboyer | 1.9 | 8.6 | | | 2.3 |
| string | 1.0 | 1.9 | | | 0.6 |
| earley | 1.6 | 7.3 | | | 4.8 |
| lattice | 9.8 | 46.7 | | | 8.5 |
| fibfp | 3.4 (70 before the flonum paths above) | 21.9 | | | 10.9 |
| mbrot | 5.2 (37 before) | 35.2 | | | 22.1 |

In another run of fib, a busier one, `jit` took 2.9 times as long as
`vop`, and `interpret` over 11 times (killed at 300 s).

Guile-idiomatic loops (milliseconds; the same file natively and as a `.go`):

| | native | VM, `vop` | VM, `jit` | Guile 3.0.11 |
|---|---|---|---|---|
| closures (3M counters) | 97 | 281 | 911 | 95 |
| records (3M SRFI 9 records) | 371 | 856 | 2,038 | 106 |
| hash tables (1M `hash-set!`, `hash-ref`) | 431 | 709 | 892 | 872 |
| strings (300k `string-append`, `string-split`, ...) | 312 | 520 | 638 | 152 |
| `(ice-9 match)` (1M evaluations of a small expression) | 613 | 357 | 1,492 | 141 |
| lists (SRFI 1 `iota`, `filter`, `fold`, `map`, `assv`) | 1,130 | 1,630 | 1,994 | 3,257 |

Native code is 1.5 to 5 times faster than the fastest VM backend, except
for `(ice-9 match)` (where Guile itself is 4 times faster than either:
its compiler contifies the failure continuations `match` makes). In the
VM's translated code (sb-sprof: 98% of fib's samples are in the region's
function), every local is a slot of the stack vector, read and written in
memory, a return goes through the region's dispatch on the IP, and a call
to another image or to Lisp leaves the translation.

Loading, with nothing to compile (19 modules, SRFI 1, 9, 19, 26, 41, 43,
64, 69, `(ice-9 match)`, `format`, `regex`, `peg` and the rest):

| | from the cache's fasls, native | from the installed `.go` files, on the VM (`QUASISCHEME_GUILE_GO=all`) |
|---|---|---|
| all 19 | 57 ms | 400 to 530 ms |
| `(srfi srfi-19)` alone | 19 ms | 454 ms |
| the 19, GOOPS and `(web uri)` | 77 ms | 18 s, in a second run too (nothing of it is kept between processes) |

About 350 ms of the VM's is two `guile` processes it asks for tables
(the intrinsics, the directory of `.go` files), each forked from an 8 GB
SBCL; there is one more for each symbol whose hash compiled code reads
(a `case` on symbols), the first time. Compiling from source:
the 21 modules with GOOPS and `(web uri)` take 16.5 s natively, cache off
(21.7 s writing the cache); compiling `srfi-19.scm` to bytecode here, with
Guile's compiler on the VM (`QUASISCHEME_GUILE_COMPILER=vm`), didn't finish
in 11 minutes, where `guild` takes 0.3 s.

So native code with the fasl cache is the faster both to run and to load,
and it is what LilyPond uses: its `scm/*.scm` load through
`primitive-load-path`, so the cache holds them. LilyPond on a small score
(tests/lilypond/time.sh, in the container, the cache filled) takes 8 to
10 s, against 1 to 1.4 s on real Guile; the first run, filling the cache,
about 55 s. Of a run of 7.8 s (`--verbose`, timestamped), loading
`scm/*.scm` is 0.25 s; the font database 0.8 s; the `ly/*-init.ly` files
1.3 s; interpreting the music 0.6 s; and opening the Emmentaler fonts 0.55 s
for each of the seven sizes, 3.9 s, the most of it. None of that is the
code's speed, native or bytecode.

#### LilyPond's start-up (2026-10-18)

Where a warm run's time went (tests/lilypond/time.sh in the container),
and what changed:

- **Top-level forms that make no procedure** are evaluated by SBCL's
  interpreter, not compiled (`TRIVIAL-CORE-P`, boot.lisp): a call of a
  global with quoted arguments, a quoted list, a `define` of a value. Of
  the 1,170 forms LilyPond evaluates as it starts (from `ly/*.ly` and
  `#(...)`), 945 are such, and SBCL's `compile` took 0.5 ms for the
  smallest, 0.2 s for one list of 1,400 conses (the drum pitch table);
  the interpreter takes microseconds. A form with a lambda (the 220 music
  functions `define-music-function` makes) is still compiled, since its
  procedures may run many times. This took a warm run from 3.5-4.2 s to
  2.2-2.5 s (real Guile: 0.8 to 1.1 s). `*INTERPRET-TRIVIAL-FORMS*` turns
  it off.
- **The cold run** (the cache empty, 31 to 35 s here; the first measure of
  this section, 46 s): of the 33 s, SBCL's `compile-file` of the 4,400
  forms of Guile's and LilyPond's modules is about half (15 s; 60% of
  that in the 166 forms of 2,000 conses or more, 56 ms each); evaluating
  macro transformers as psyntax expands (2,600 of them, each compiled
  again in the form's chunk) a seventh; expanding, loading the chunks and
  the rest of the start-up the remainder. Not much is left to cut
  without making the code slower: a cheaper SBCL policy for the cached
  code (`compilation-speed 3`, `speed 0`, `debug 0`) saves 5%, and
  without constraint propagation 6%, both measured as CPU time in a run
  alone on the machine, and both cost the Guile modules' speed for ever
  after. The chunks are small (1,800 of them under 30 conses take 1.5 s in
  all): the per-chunk cost of `compile-file` is not it.
- **A cache that doesn't last.** LilyPond sets `XDG_CACHE_HOME` to its
  installation, so the cache is written there: in the container, to the
  image's layer, which is gone after each `run.sh`, so every
  measurement's first run was cold. tests/lilypond/ now keeps the cache in
  `/root/.cache` (`COLD=1` empties it, to time filling it), and
  `GUILE-CACHE-PARENT` (cache.lisp) falls back to `~/.cache/` when the
  XDG directory can't be written, as LilyPond's installation can't be by
  a user. The cache's key (the build signature) holds the dates of all
  the files Quasischeme is built from, those of Chez mode among them
  (Guile mode's library uses `(chezscheme)`), so an edit to any of them
  makes the next run cold; a rebuild from unchanged files doesn't.

### Emacs Lisp

Guile's elisp front end (`language/elisp/`) runs unmodified. It compiles
through the replacement `(language tree-il spec)`, whose native compiler
goes from Tree-IL to `value`. `,language elisp` at Guile's REPL loads
`boot.el`, all 136 of its forms:

```
scheme@(guile-user)> ,language elisp
Happy hacking with Emacs Lisp!  To switch back, type `,L scheme'.
elisp@(guile-user)> (defun sq (x) (* x x))
$1 = sq
elisp@(guile-user)> (mapcar (lambda (x) (sq x)) (cons 1 (cons 2 nil)))
$2 = (1 4)
```

`elisp.test` (87), `elisp-compiler.test` (99) and `elisp-reader.test`
(20) all pass.

Emacs Lisp's nil is Guile's `#nil`, so the lists it conses end in `#nil`,
and Guile's list procedures take that as the end of a list
(src/guile/elisp.lisp):
- **Defined in Scheme** (boot-9's, SRFI-1's): they get it from `null?`
  and `pair?`.
- **Loops that stop at either end**: `length`, `list?`, `reverse`, the
  `mem` and `ass` procedures and `last-pair`, both as root bindings and
  where the compiler calls them directly. They're as fast as the
  originals, and those that return a tail return the list's own.
- **Converted first**: `map`, `for-each`, `append`, `list->vector`,
  `reverse!`, `delete!` and the like get a list ending in `()` in place
  of one ending in `#nil`. The destructive ones therefore return a new
  list rather than changing the argument.
- **`sort`, `merge` and `append!`** keep `#nil` as Guile does.

The writer prints `(1 . #nil)` as `(1)`, as Guile's does.

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
