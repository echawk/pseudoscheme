# Guile on Pseudoscheme: a `--guile` mode, and someday Guix

Status: a plan, with two pieces done. **Delimited continuations** are
native (`call-with-prompt`, `abort-to-prompt`, composable continuations;
docs/continuations.md, "Delimited continuations"), and **`(guile)`**, a
library of Guile's everyday procedures on top of them, works for R6RS
programs (see "Started" below).

The goal: `pseudoscheme --guile prog.scm` runs programs written for GNU
Guile, with `(use-modules (ice-9 match) (srfi srfi-1) ...)` and the
libraries people install for Guile. Guile has a large body of real code
behind it, and the largest is GNU Guix. Running Guix's client side
(evaluating package definitions, computing derivations, talking to the
daemon) would be a long-term milestone that tests everything below it.
As with Scheme today (docs/interop.md), Guile code would then also be
callable from Common Lisp.

The figures below come from Guile 3.0.11's installed sources
(`/opt/homebrew/share/guile/3.0`). The primitive counts are token
scans, not a real analysis, and so are approximate.

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

### Stage 0: the reader and the flag

`--guile` selects a Guile top level and reader. The reader extensions:
- `#:kw` keywords: these already read as Lisp keywords, and Guile's
  keywords are disjoint objects, so they can stay Lisp keywords;
- `#!...!#` block comments (script headers);
- `#{odd symbol}#`;
- `#true`/`#false`, which R7RS already has;
- `#vu8(...)`, `#*101` bit vectors and `#,`/`#,@`;
- **`read-hash-extend`**, the user-defined `#` syntax. Guix's
  G-expressions are `#~`, `#$`, `#$@` and `#+`.

### Stage 1: libguile's primitives

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

### Stage 2: a Tree-IL compiler

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

### Stage 3: boot-9 and the module system

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

### Stage 5: ports and POSIX

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

### Stage 6: `(system foreign)` on CFFI

Guile libraries bind C with `(system foreign)`: `dynamic-link`,
`dynamic-func`, `pointer->procedure`, `procedure->pointer`,
`make-c-struct`/`parse-c-struct`, `bytevector->pointer` and
`pointer->bytevector`. CFFI covers all of it.
- `pointer->procedure` takes the C types at run time. We have SBCL's
  compiler at run time, so it can compile a CFFI call stub for each
  signature and cache it, without libffi.
- **`bytevector->pointer` is the catch.** Guile's GC doesn't move
  bytevectors, so the pointer stays valid. SBCL's does move them, and
  `with-pointer-to-vector-data` pins a vector only for its dynamic
  extent. Bytevectors that reach C would have to be allocated where the
  GC doesn't move them (static-vectors), or pinned for as long as C may
  hold the pointer.
- This is what Guix needs from C: guile-gcrypt (libgcrypt), guile-git
  (libgit2, through bytestructures), guile-sqlite3, guile-zlib,
  guile-lzlib and guile-zstd all use `(system foreign)`.
  tests/programs/c-libraries.scm already shows zlib and libc called
  through CFFI.
- C extensions written against libguile's C API (guile-gnutls,
  guile-avahi, guile-ssh) can't be loaded at all. They would need
  replacements: cl+ssl for TLS, say, behind the same Scheme interface.

### Stage 7: GOOPS

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

## Started: `(guile)`

`(import (guile))` gives an R6RS program Guile's default environment as
far as it goes so far: all of R6RS, plus

- prompts: `call-with-prompt`, `abort-to-prompt`, `make-prompt-tag`,
  `default-prompt-tag`, `%`, `call/ec`, `let/ec`;
- `catch`, `throw`, `with-throw-handler` and `false-if-exception`, on
  R6RS conditions. `(catch #t ...)` also catches R6RS errors, as
  `misc-error` (Guile gives some of them other keys, such as
  `wrong-type-arg`);
- `define-syntax-rule`, `1+`, `1-`, `const`, `compose`, `negate`;
- Guile's hash tables (`make-hash-table`, `hash-ref`, `hash-set!`,
  `hash-fold`, ...), association lists (`assoc-ref`, `acons`, ...), and
  SRFI 1's and SRFI 13's list and string procedures;
- `format` (Chez's, i.e. Common Lisp's directives, as Guile's
  `(ice-9 format)` mostly follows) and `simple-format`, `pk`.

It is src/guile/guile.scm, made into a library by src/guile/guile.lisp
when first imported, which is how it reuses `(chezscheme)`'s `format`
and the SRFIs without slowing every boot. This is the *imitation* path
the Summary argues against for the long run, since Guile's own modules
should eventually run unchanged. But it lets Guile-flavoured R6RS code
run now, and each procedure here maps onto a libguile primitive that
stage 1 needs anyway. Not yet: keywords (`#:key`, which the reader
needs), `define*`/`lambda*`, `define-module`/`use-modules`, fluids.

## First steps

1. **An inventory.** Run Guile's psyntax on Guile's own module sources
   *in Guile*, record every `primcall`, `primitive-ref` and
   `toplevel-ref` that resolves to a C primitive, and count them by
   module. That turns this document's token scan into a real worklist,
   ordered by what boot-9 needs first.
2. **Tree-IL by hand.** Construct Tree-IL for small programs in Guile,
   write it out as data, and compile it with a first Tree-IL-to-core
   translator here, before psyntax is involved.
3. **Boot to the first `define-module`.** Load `boot-9.scm` up to and
   including `psyntax-pp`, with stubs for everything that isn't used
   yet, until `(define-module (test) #:use-module (ice-9 match))` works.
4. **Then** `ice-9` modules one by one, with Guile's own test suite
   (`test-suite/tests/*.test` in Guile's source tree) as the measure,
   as chibi's and Racket's suites are for R7RS and R6RS here.
