# Racket on Quasischeme: a `--racket` mode

Status: **running, on strategy B** (October 2026).
`quasischeme --racket prog.rkt` runs `#lang racket/base` and
`#lang racket` programs on Racket's own expander. Strategy A, Racket
CS's layers on the Chez mode (the plan this document describes), has
its pieces in src/racket/cs/ but doesn't get past booting rumble yet; strategy C, its hybrid,
waits on A. See "Where it stands", next, then the design as it was
written, which the strategies section compares.

## Where it stands

Strategy B (docs/racket-linklet-compiler.md): our compiler from
linklets to Lisp, with Racket's primitives written natively in Lisp.

- **Racket's own layers, extracted by Racket's own recipe.**
  `utils/racket/setup.sh` fetches Racket's source at the pinned version;
  `utils/racket/extract.sh` runs the expander's `bootstrap-run.rkt`, as
  each layer's `main.zuo` does, with a `racket` of that version, to
  flatten a layer into one S-expression linklet
  (`build/external/racket-derived/LAYER.rktl`): the expander (8.7 MB),
  thread, io and regexp. The expander and the regexp layer run here;
  thread and io are extracted but not used, our natives standing in
  for them. Without a Racket to extract with, the expander is read from
  the copy in Racket BC's `startup.inc` instead
  (src/racket/linklet/sources.lisp).
- **The compiler** (src/racket/linklet/compiler.lisp) lowers each
  linklet to core Scheme, which the translator and SBCL compile:
  `letrec` checks, `with-continuation-mark` on our continuation marks,
  boxes only for assigned variables, direct calls for a top-level
  procedure's calls of itself and for `letrec`s of lambdas. Linklets
  too big for SBCL to compile in one piece are split.
- **The primitives** (src/racket/native/, about 20 files): structs and
  properties, chaperones and impersonators, ports and `rktio`, the
  printer, hash tables, `equal?`, exceptions and error messages,
  parameters, continuation marks and prompts, threads, events and
  custodians (SBCL threads with a baton, preempted every 10 ms),
  `#%foreign` on CFFI, `#%network`, extflonum stubs, places, futures
  and will executors as stand-ins. Regexps are Racket's own regexp
  layer; the old cl-ppcre natives remain as the fallback
  (`QUASISCHEME_RACKET_REGEXP=native`).
- **Modules** load from the collections in Racket's source tree, and
  from collection links; `.zo` files are written and read, and each
  compiled linklet is cached as a fasl (src/racket/cache.lisp). With a
  warm cache, `#lang racket/base` hello starts in about 1.1 s and
  `#lang racket` hello in about 3.4 s (0.5 s for `racket/base` from the
  saved image, below). A cold cache takes minutes (a minute for
  `racket/base`), and one run fills it: a `.zo` carries its linklets'
  cache keys, so the run that reads one finds the machine code the run
  that wrote it compiled.
- **Saved image.** The first run saves an executable of the program
  with the expander instantiated, in the Racket cache (190 MB, kept
  only for the current build of `bin/quasischeme`), and each run
  after execs it; `QUASISCHEME_RACKET_CORE=0` turns it off.
- **Tests** (tests/racket/run.sh linklet): 6 hand-written linklet
  programs, 14 expressions through the expander, and 18 `#lang
  racket/base` programs in tests/racket/programs/, each checked
  against what Racket 9.3 prints.
- **Racket's own test suite** (`pkgs/racket-test-core`,
  tests/racket/run-suite.sh, one file per process, against real
  Racket's run as the reference): in the last full run, before the
  regexp layer and the fixes for the crashes it found, 38 of the 91
  files reported results (14 of them with no failures), 95,933 tests
  ran and 52 files crashed or timed out; real Racket runs about 1.15
  million tests in 89 of them. Most of that run's failures were
  `rx.rktl`'s, cl-ppcre's differences from Racket's regexps, which
  the regexp layer replaces.

What's missing: a faster cold start (compiling the linklets with SBCL
is most of it; a cheaper policy didn't help, and `(debug 0)` breaks
procedure arities), real places and futures, the thread and io layers themselves, and the
crashes Racket's suite still finds. `--racket` is strategy B,
`--racket=linklet`; `--racket=cs` (A) reports "not implemented yet".

The goal: `quasischeme --racket prog.rkt` runs Racket programs with
`racket/base` and the packages on the package server, by reusing as much
of Racket's own implementation as possible. Racket code then becomes one
more thing a Common Lisp program can call, as Scheme is today
(docs/interop.md).

## The REPL

`bin/quasischeme --racket` with no file starts Racket's REPL as `racket`
with no arguments does (strategy B only). src/racket/linklet/repl.lisp
follows Racket CS's main.sps: print the banner (`Welcome to Racket v9.3
[cs].`), `namespace-require` the init library `racket/init` (`racket`,
`racket/enter` and `racket/help`, with `current-print` set to
`pretty-print`), load `racket/interactive` (the user's init file, and
xrepl if it is installed), then run `read-eval-print-loop` from
collects/racket/repl.rkt: Racket's own, with Racket's `current-prompt-read`,
`current-read-interaction`, `current-eval` and `current-print`. Errors
print through Racket's handlers and return to the prompt; end of file
leaves, writing a newline, as `racket` does.

- **rktio.** The terminal comes from rktio, Racket's portable OS layer,
  built from racket/src/rktio/ as a shared library:
  `sh utils/racket/rktio.sh` (run by `setup.sh`) runs rktio's own
  `configure --enable-standalone` and `make` into
  build/external/rktio/. src/racket/native/rktio.lisp binds it with
  CFFI, loading it on first use: `rktio_init`, `rktio_std_fd`,
  `rktio_fd_is_terminal`, `rktio_poll_read_ready`, `rktio_read` and
  `rktio_write`. `rktio_init` blocks SIGCHLD in the calling thread (a
  signal thread reaps children once a process starts), which SBCL won't
  run with, so the binding unblocks it again.
- **Console ports.** In the REPL `current-input-port`,
  `current-output-port` and `current-error-port` are ports on rktio's
  std fds, as the io layer makes them: custom ports whose reads poll
  (`rktio_poll_read_ready`, handing back an alarm event while nothing
  is there, so other threads run) and read, and whose writes call
  `rktio_write`. `terminal-port?` is `rktio_fd_is_terminal` of the
  port's fd (for the console ports, and the host's streams on the same
  descriptors); ports.lisp's stub said false.
- **Line editor.** At a terminal (rktio's answer, stdin and stdout both,
  and `TERM` not `dumb`) the REPL's line editor (src/repl/, docs/repl.md)
  supplies the text: `current-prompt-read` asks it for a line, history
  and completion (from `namespace-mapped-symbols`) included, and Racket's
  reader finds the data in it, one at a time, so that `1 2` evaluates
  both. Otherwise the default `current-prompt-read` runs on the console
  input port. `,language racket` from another REPL runs the same loop.
- **Not yet.** `racket -i FILE` (requiring the file first), `-e` before
  a REPL, and the break key (Ctrl-C ends the line being edited, not a
  running program); the other strategies.

## Summary

**Our Chez mode plays the part of the Chez Scheme that Racket CS is
built on.** Racket CS's own layers then run on it unchanged:
- rumble, the core runtime;
- the schemified thread, io, regexp, schemify and expander layers;
- the linklet layer.

Only the bottom layer is ours:
- `--chez` itself (docs/chez.md);
- the additions Racket's fork of Chez Scheme makes, which rumble uses;
- one replaced hook, the step that compiles a schemified linklet into
  machine code.

There is one pipeline, for Racket's runtime and for user code alike:

```
  .rkt --expander--> linklet --schemify--> core Scheme --(the hook)--> SBCL
```

The hook is the only choice we make:
1. **First:** the route that already works. Schemified code is treated
   as Chez source: psyntax, then the translator, then SBCL. Racket CS's
   schemified regexp and schemify linklets already ran this way on the
   Chez mode (contrib/racket-linklets).
2. **Then the fast path.** Schemify's output is already fully expanded
   core Scheme, so psyntax has nothing to do. It goes straight to the
   translator's core-form entry (`translate-core`, src/psyntax.lisp).
   That gives "linklets to Common Lisp" without a linklet compiler of
   our own. Schemify keeps doing the Racket-specific lowering we would
   otherwise rewrite:
   - `letrec` checks;
   - known-procedure and known-constant information across linklets;
   - recognizing struct types, so accessors inline;
   - finding mutated variables, the only ones that need boxes;
   - lifting closures.

This supersedes this document's earlier plan: a linklet-to-Lisp compiler
plus primitive instances written in Lisp. That plan is under
"Alternatives considered", and the reason for the change is under "Why
Racket CS's layers, not our own".

## How Racket is layered

Racket CS (racket/src/cs/README.txt in Racket's source) is built in
layers:

| layer | written in | does |
|---|---|---|
| Chez Scheme | C, Scheme | the compiler and the machine |
| rumble | Chez Scheme | the core runtime: continuation marks, prompts, structs and properties, impersonators, hash tables, ... |
| thread, io, regexp | Racket | green threads, ports and files (on the C library rktio), regexps |
| schemify | Racket | turns a linklet into a Chez Scheme `lambda` |
| linklet | Chez Scheme | `compile-linklet`, `instantiate-linklet`, ... on top of schemify and Chez's `compile` |
| expander | Racket | the macro expander, module system, reader, `eval`/`compile`/`expand` |
| collects | Racket | `racket/base` and everything else |

The thread, io, regexp, schemify and expander layers are written in
Racket but are each *extracted* into a single linklet with no
dependencies other than primitives, which is how Racket CS bootstraps
them on Chez Scheme. A different backend can do the same.

Where each extracted layer is found in the pinned release:
- **schemified**: racket/src/cs/schemified/ has all of them as
  schemify's output. Strategies A and C run these.
- **raw linklets**: racket/src/bc/src/startup.inc has only the
  expander, flattened, as C string literals. Racket BC compiles it with
  its own linklet compiler. Strategy B reads it from there
  (src/racket/linklet/sources.lisp), so it needs no Racket to extract
  the expander. BC has its thread, io and regexp layers in C, so for
  those B still needs racket/src/expander/extract run on a Racket.

## Why Racket CS's layers, not our own

The earlier plan targeted linklets and wrote everything under them
ourselves: a linklet compiler and the primitive instances in Lisp,
about 1,650 primitives in all. Its argument against schemify's output
was that the output calls rumble's internals:
- variable objects, `#%app` variants, record operations, `$value`;
- Chez's `#3%` unsafe primitives;
- continuation attachments and engines.

So running it seemed to mean reimplementing rumble. That argument
assumed rumble would be ours. If rumble is Racket's own, running on
`--chez`, the objection goes away.

The comparison:

| | our own layers (earlier plan) | Racket CS's layers (this plan) |
|---|---|---|
| what we write | a linklet compiler; about 1,650 primitives done to Racket's specification: structs and properties, impersonators, continuation marks, ports, threads | the Chez fork's additions that rumble uses (dozens, mostly control); one compile hook; start-up |
| what stays Racket's | the expander and the collects | rumble, thread, io, regexp, schemify, the linklet layer, the expander, the collects |
| fidelity | as good as our reimplementation of `#%kernel` | Racket's own code, so Racket's own behaviour |
| following new Racket releases | re-check about 1,650 primitives | re-check the fork's additions and the hook |
| speed | depends on our linklet compiler's inference | schemify's inference, which Racket CS ships |

There is also one reason that is about the project. The Guile mode runs
Guile's own boot-9, psyntax and modules on libguile's primitives
written in Lisp (docs/guile.md). The Chez mode runs Chez's own mats.
Running Racket CS's own runtime on the Chez mode is the same move, one
layer further down.

## Three strategies, and which should be fastest

The strategies are kept separate, each buildable and selectable on its
own (decision 8), so that they can be measured side by side rather than
argued about:

| | strategy | lowering | runtime underneath | document |
|---|---|---|---|---|
| **A** | Racket CS's layers | schemify | rumble itself, on `--chez` and the Chez fork's additions | this one |
| **B** | our linklet compiler | our passes 1 to 4 | rumble's exports written natively in Lisp | docs/racket-linklet-compiler.md |
| **C** | hybrid | schemify | rumble, with its hot procedures replaced by native Lisp versions of the same interface | this section |

**Assessment (October 2026; a judgement, not yet measured).**

*Where A and B are equal.* User code and the layers above rumble (the
expander, io, thread, regexp) end up in the same place either way: core
Scheme, through the translator, to SBCL. For straight-line computation,
what differs is only the quality of the lowering. Schemify's analysis
is mature: known calls, struct recognition, boxing only mutated
variables, `letrec` handling. Strategy B's pass 3 would start out behind
it and have to catch up. So on such code A is ahead early, and at best
the two tie later.

*Where B should win: the runtime under the code.* In A, rumble runs
through the Chez mode, which adds a layer of emulation exactly where
Racket code spends its time:
- **Structs.** rumble's structs are Chez records, which are a struct
  holding a vector here: two allocations and an extra load per field
  (docs/optimizations.md, idea 2). B would make each Racket struct type
  an SBCL struct of its own.
- **`parameterize`, exception handlers, `with-handlers`.** rumble builds
  continuation marks on the fork's continuation attachments. A emulates
  attachments on top of the Chez mode's marks, which are themselves on
  our frames: two layers where B has one.
- **Applicable structs, keyword procedures, impersonators, contracts.**
  rumble uses the fork's wrapper procedures. In B these are SBCL
  funcallable instances, which dispatch directly.
- **Chez's optimizer.** rumble is written to be compiled by it:
  `define-inline`, cp0's inlining, unchecked code at `optimize-level
  3`. The Chez mode has no cp0, so rumble's small hot helpers become real
  calls.
- **Hash tables.** rumble's are written for Chez; B's would be built on
  SBCL's.

Idiomatic Racket (contracts, keyword arguments, `for` over generic
sequences, structs, deep `parameterize`) leans on exactly these. On such
programs B could plausibly be 1.5 to 2 times faster than A. **B has the
higher ceiling; A runs well sooner.**

*C should approach B's speed with A's cost.* schemify's output calls
rumble's procedures by name. So after A runs, a profile of `racket/base`
loading and of bench/'s programs as Racket modules picks out rumble's
hot procedures, and each can be replaced by a native Lisp version with
the same interface. The likely first candidates are struct operations,
attachments and marks, wrapper procedures, and hash tables. C keeps
schemify's analysis and gets a native runtime where it matters, with
no linklet compiler of our own.

*Two improvements help A and C regardless*, and the Chez and R6RS modes
too:
- records as one object (docs/optimizations.md, idea 2);
- a cp0-style inlining pass in the Chez mode, which recovers what rumble
  expects from Chez's optimizer (part of idea 6).

**Measuring.** The first comparison is milestone 2 in each document:
Racket's regexp linklet running `rx.rktl`'s 6,866 checks,
- under A: schemify on rumble itself;
- under B: our compiler on our native runtime;
- under C: once rumble's regexp-relevant hot spots are replaced.

After milestone 5 (`racket/base`), the comparison is the time to load
and compile `racket/base`, and bench/'s programs as Racket modules.

## Decisions

1. **The Chez mode is the platform.** Racket CS's Scheme sources
   (`racket/src/cs/`) are compiled and run by `--chez`, and nothing else
   of Chez Scheme is needed. Everything Racket's fork of Chez adds that
   rumble uses is added to `--chez` (src/racket/fork, below), not
   imitated in Racket's code.
2. **Racket's sources come from a pinned release**, fetched by a script,
   never vendored, and never searched for on the machine:
   - `utils/racket/setup.sh` makes a shallow clone of the
     racket/racket repository at a release tag (`git clone --depth 1
     --branch vX.Y`) into `build/external/racket/`, which is gitignored;
   - `utils/racket/teardown.sh` removes it;
   - the tag is set in one place, the setup script, and moving to a new
     release is a deliberate change.
3. **An installed Racket is used only through its executable**, and only
   where the design needs it: the collection paths, through `racket -e
   '(display (find-system-path (quote collects-dir)))'`, or through
   `PLTCOLLECTS`. The installed `.zo` files are Chez machine code, which
   we can't use, so they are never read.
4. **One compile hook, in two stages:**
   - stage A: schemified code goes through psyntax, the translator and
     SBCL, as today;
   - stage B: schemified code goes straight to `translate-core`;
   - stage A stays available behind a switch, to compare against.
5. **The runtime is built into the image.** rumble and the schemified
   layers are compiled once, when the image is built, as the R6RS and
   Chez layers are now. Loading the expander from source at every
   start-up would take minutes.
6. **Compiled user code is cached** in two parts (see "Compiled
   code"):
   - the `.zo` holds the schemified S-expression;
   - the library cache holds the SBCL fasl.
7. **contrib/racket-linklets retires.** Its racket-lite stand-in for
   rumble is replaced by rumble itself. What remains of it moves to
   `src/racket/` and `tests/racket/`. Nothing more goes into contrib/.
8. **The strategies stay separate and selectable** ("Three
   strategies"), so that they can be compared on the same programs:
   - `--racket=cs` (A), `--racket=linklet` (B) and `--racket=hybrid`
     (C), or `QUASISCHEME_RACKET_STRATEGY`;
   - A and C share rumble and schemify; C only adds overrides;
   - B shares everything above rumble.

   Each is built and tested on its own, and none is merged into
   another.

## Structure

The skeleton exists (October 2026): every file below is in place and
`--racket=NAME` selects a strategy, but each reports what isn't
implemented yet.

```
utils/racket/
  setup.sh              shallow clone of racket/racket at v9.3 (commit pinned) -> build/external/racket/
  teardown.sh           removes it
src/racket/             (ASDF system quasischeme/racket)
  racket.lisp           shared: the strategy registry, Racket's sources and collections, entry points
  instances.lisp        shared: the primitive instances (#%kernel, ...)
  cache.lisp            shared: compiled linklets, the .zo format, the fasl cache
  native/               the native runtime: B's rumble, C's overrides (DEFNATIVE, with rumble's names)
    native.lisp           DEFNATIVE and its table
    structs.lisp          struct types and properties
    procedures.lisp       arity, wrappers, impersonators and chaperones
    control.lisp          marks, prompts, composable continuations, #%paramz
    hash.lisp             mutable and immutable (persistent) hash tables
    exn.lisp              the exn hierarchy, raise
    rktio.lisp            Racket's OS layer, built from source and bound with CFFI
  cs/                   A: Racket CS's layers on --chez
    fork.lisp, fork.scm   the Chez fork's additions (and milestone 0's inventory)
    layers.lisp           the layers in build order
    hook.lisp             the compile hook: stage A and stage B
    boot.lisp             our main.sps
  linklet/              B: our linklet compiler (docs/racket-linklet-compiler.md)
    api.lisp              #%linklet: instances, variables, the interface
    compiler.lisp         passes 1 to 4
    boot.lisp
  hybrid/               C: A plus native overrides
    overrides.lisp
  strategies.lisp       the three, registered
tests/racket/
  run.sh                each strategy on the same steps: hand-written linklets, rx.rktl, the test suite
  linklets/*.rktl       hand-written linklets and their expected values
```

In the command line (contrib/cli/cli.lisp):
- `--racket[=cs|linklet|hybrid]` selects the mode and the strategy;
- `QUASISCHEME_RACKET_STRATEGY` selects the strategy otherwise;
- `--precompile COLLECTION` will compile a collection into the cache,
  as `--precompile-srfi` does for the SRFIs.

## Build time

`make racket` (needs `utils/racket/setup.sh` to have been run):
1. Load `--chez`, then src/racket/fork.
2. Compile the layers in Racket CS's order:
   - rumble (`racket/src/cs/rumble.sls` and its `rumble/*.ss`);
   - the schemified thread, io, regexp, schemify and expander layers;
   - the linklet layer (`racket/src/cs/linklet.sls`), with
     src/racket/backend's hook.

   Each is compiled as a whole library (`compile-whole-library`, Chez
   mode). The expander's file is very large, so hoisting definitions
   (`hoist-definitions`, src/psyntax.lisp) splits it into pieces SBCL
   can compile.
3. Save the result into the image, so that start-up only instantiates.

## Run time

`quasischeme --racket prog.rkt`:
1. Initialize rumble. Instantiate the primitive instances (`#%kernel`,
   `#%unsafe`, `#%flfxnum`, `#%paramz`, `#%foreign`, …, built by rumble
   and the layers above it as in Racket CS), then the io, thread and
   expander instances.
2. Set the collection paths from `PLTCOLLECTS` or the installed `racket`
   executable (decision 3). Set `use-compiled-file-paths` to our own
   `compiled/quasischeme/` directories, so that we never read or
   overwrite Racket's.
3. Run `(dynamic-require 'racket/base)`, then the program. A module with
   no compiled form of ours is compiled from its `.rkt` source and
   cached. Racket's own compilation manager decides when, as `raco make`
   would.

The first run of a collection is slow. `racket/base` is a few hundred
modules, and SBCL compiles much more slowly than Chez. So:
- `quasischeme --racket --precompile racket/base` compiles them ahead
  of time;
- stage B (decision 4) is the main lever on this cost.

## Compiled code

Racket CS writes a compiled linklet into a `.zo` as fasl'd Chez code
objects. SBCL code doesn't belong in Racket's byte stream, so:
- **A `.zo` holds each linklet's schemified S-expression.** This is
  machine-independent, close to what `racket -M` produces. Racket's
  compilation manager, its dependency hashes and `raco make` work
  unchanged.
- **The SBCL fasl for a linklet is in the library cache**
  (src/library-cache.lisp, `~/.cache/quasischeme/`). Its key is a hash
  of the S-expression, the build signature and the continuations mode,
  as the cache's keys are now.
- **Loading a linklet** finds its fasl by hash, or else translates and
  compiles it and writes the fasl.
- `(system-type 'vm)` answers `quasischeme`, so code that checks for
  `chez-scheme` doesn't take paths meant for Racket CS's machine code.

## The Chez fork's additions

**Where they go.** `--chez` stays faithful to upstream Chez Scheme:
- if upstream Chez also has a name rumble uses (stencil vectors and
  `$record-ref` are in Chez 10, for example), it is implemented in
  src/chez/, as upstream defines it;
- if only Racket's fork has it (continuation attachments,
  `$app/no-inline`, wrapper procedures, …), it is defined in
  src/racket/cs/ (fork.scm), in an environment only `--racket` uses.
  It never goes into the Chez mode itself.

`sbcl --script tests/racket/coverage.lisp --fork` is milestone 0's
inventory. It lists the Chez internals that rumble and the schemified
layers use and `--chez` lacks, 70 of 203 at v9.3. With upstream Chez's
source (`sh utils/chez/setup.sh`, cisco/ChezScheme at e3b13636, the
mats' commit), it also says which side each one goes on:
- **63 are in upstream Chez 10.** Racket's fork upstreamed most of its
  additions: stencil vectors, continuation attachments,
  `$app/no-inline`, the record internals. So most of strategy A's
  groundwork is `--chez` covering more of upstream Chez's internals,
  which belongs in src/chez/.
- **About 7 are Racket's alone:** `$intmap-has-key?`, `$intmap-ref-key`,
  `fxop`, and some names fork.lisp lists by hand. A few of the 63, such
  as `foreign-procedure`, are syntax, which the inventory's
  `top-level-bound?` test can't see, so they aren't really missing.

This decides feasibility; milestone 0's inventory gives the exact list.
What is known, from the regexp and schemify experiments (about 61 of
Chez's internal `$` names, mostly continuation attachments):

| addition | what rumble uses it for | on --chez |
|---|---|---|
| continuation attachments (`call-setting-continuation-attachment`, `call-getting-continuation-attachment`, `call-consuming-continuation-attachment`, …) | continuation marks, so `parameterize`, exception handlers, `with-handlers` | onto the continuation marks the Chez mode has (ac9fb00) |
| engines and timer interrupts | green threads (the thread layer), `break` | the Chez mode's engines (ac9fb00) |
| wrapper procedures (`make-wrapper-procedure`, `$app`, arity masks) | impersonators and chaperones, keyword procedures, `procedure-reduce-arity` | new: funcallable instances, as Guile mode's applicable structs are |
| record internals (`$record-ref`, sealed and opaque types, record-type properties) | structs, struct properties | mostly present since the `oop.ms` work |
| weak and ephemeron hash tables, guardians, phantom bytevectors | weak boxes, wills, memory accounting | on SBCL's weak tables and finalizers |
| `foreign-procedure`, ftypes | `ffi/unsafe` | onto CFFI, as Guile mode's `(system foreign)` is |
| `fasl-read`, `fasl-write` | serialization of compiled code | not needed: see "Compiled code" |

rumble is compiled at `optimize-level 3` in Racket CS, and may rely on
unchecked behaviour that `--chez`'s checks reject. The inventory should
find such places too.

## Linklets

These facts about the interface hold whichever compile hook is used.

A linklet is the unit the expander hands to the backend. Its language is
fully expanded core Racket:

```
(linklet [[imported-id ...] ...]   ; one list per imported instance
         [exported-id ...]
  defn-or-expr ...)

defn-or-expr = (define-values (id ...) expr) | expr
expr = id | (quote datum) | (lambda formals expr) | (case-lambda [formals expr] ...)
     | (if expr expr expr) | (begin expr ...+) | (begin0 expr expr ...)
     | (let-values ([(id ...) expr] ...) expr) | (letrec-values ...)
     | (set! id expr) | (with-continuation-mark expr expr expr)
     | (#%variable-reference ...) | (expr expr ...)
```

Primitives (`car`, `zero?`, `make-struct-type`) are free identifiers;
everything else comes from imported instances. The documented API is
`racket/linklet` (Racket reference, "Linklets and the Core Compiler"):
`compile-linklet`, `instantiate-linklet`, `make-instance`,
`instance-variable-value`, and so on. These 29 functions are the
`#%linklet` primitive instance. **They are the whole interface between
the expander and a backend**: the expander calls `compile-linklet` on
each linklet it produces.

What a module compiles to, checked with Racket 9.3 (CS). This compiles
a module to machine-independent form and decodes it with
`compiler/zo-parse`:

```racket
#lang racket/base
(provide fact greet)
(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))
(define (greet #:name [name "world"]) (string-append "hello " name))
(define-syntax-rule (twice e) (begin e e))
(twice (void))
```

The result is a linklet *directory* holding a *bundle* per module and
submodule. The bundle's keys are phases (`0`, `1`) and metadata
(`decl`, `stx-data`, `stx`, `data`, `pre`, `side-effects`, `max-phase`,
`name`, `vm`). Its phase-0 linklet:

```scheme
(linklet
 ((.get-syntax-literal!) (.set-transformer!) (make-optional-keyword-procedure))
 (greet3.1 greet.2 greet.1 fact)
 (void)
 (define-values (fact)
   (lambda (arg_12) (if (zero? arg_12) 1 (* arg_12 (fact (sub1 arg_12))))))
 (define-values (greet.1)
   (lambda (arg_13) (let-values (((loc_14) arg_13)) (let-values () (string-append "hello " loc_14)))))
 ...
 (define-values (greet3.1)
   (make-optional-keyword-procedure
    (lambda (arg_19 arg_20) ...)
    (case-lambda ((arg_23 arg_24) (greet.2 arg_23 arg_24)))
    null '(#:name) (case-lambda (() (greet.2 null null)))))
 (void) (void) (void))
```

That is a language the translator already nearly handles. The phase-1
linklet holds `twice`'s transformer. The `decl` linklet, which describes
requires and provides, calls the expander's own deserializers
(`deserialize-module-path-indexes`, `syntax-module-path-index-shift`,
...). So even *running* compiled modules needs the expander's module
runtime, which is one more reason to host the real expander rather than
reimplement a loader.

The primitive instances, counted in Racket 9.3 with
`(primitive-table '#%kernel)` etc. In this design rumble and the layers
above it provide them, as in Racket CS. The counts show what the earlier
plan would have had to write, and the notes are what it would have done:

| instance | names | notes |
|---|---|---|
| `#%kernel` | 1070 | the bulk: lists, numbers, strings, structs, hashes, ports, continuations, threads, ... |
| `#%unsafe` | 224 | unchecked versions; can start as the checked ones |
| `#%flfxnum` | 81 | `fl+`, `fx+`, flvectors, ... |
| `#%extfl` | 45 | extflonums; Racket CS doesn't support them either, so they can be stubs |
| `#%foreign` | 95 | the FFI; CFFI later, stubs first |
| `#%network` | 44 | TCP/UDP; our SRFI 106 code is a start |
| `#%linklet` | 29 | the backend interface above |
| `#%terminal` | 21 | for the expeditor REPL; can stub |
| `#%futures` | 19 | can run futures sequentially |
| `#%place` | 14 | can be single-place |
| `#%paramz` | 10 | parameterization internals |

## Milestones

Each ends in something that runs, and in a test. These are strategy
A's. Strategy B has reached the equivalents of 2 (the regexp layer),
4 (the expander evaluates in a `#%kernel` namespace), 5 (`racket/base`
loads, cached) and 7 (Racket's suite runs, file by file); A stops at
1, its boot step.

0. **Inventory.** `utils/racket/setup.sh` fetches the pinned tag. A
   script lists every identifier that rumble and the schemified layers
   use and `--chez` doesn't define. Then the table above becomes a work
   list.
   - **Settled when the skeleton was made:** the release tree has the
     schemified layers checked in (`racket/src/cs/schemified/`:
     `expander.scm` 5.3 MB, `io.scm` 1.8 MB, `schemify.scm` 3.6 MB,
     `thread.scm`, `regexp.scm`, `known.scm`). So A and C need no Racket
     to generate them.
   - There is no Chez-only bootstrap directory under `racket/src/cs/`.
     The bootstrap scripts (`racket/src/expander/bootstrap-run.rkt`,
     `racket/src/thread/bootstrap.rkt`, …) are Racket programs, which
     matters only if the schemified files ever have to be regenerated.
1. **rumble loads** on `--chez`, with the fork's additions.
2. **regexp and schemify run on rumble itself**, not racket-lite, and the
   6,866 checks of Racket's `rx.rktl` pass again.
3. **thread and io run**, on their own tests and demos from Racket's
   source tree.
4. **The expander instance evaluates** `(+ 1 2)` in a `#%kernel`
   namespace, and the REPL's `,language racket` reaches it.
5. **`racket/base` loads** from the collects, with the cache and
   `--precompile`.
6. **Stage B**, measured against stage A on the time to compile
   `racket/base` and on bench/'s programs as Racket modules.
7. **Racket's test suite**, `pkgs/racket-test-core`, run by
   tests/racket/run.sh and tallied like the Chez mats.

## Risks and costs

- **Compile time.** The expander compiles once, when the image is built.
  User modules rely on the cache and stage B. If SBCL is still too slow
  on schemify's output, the fallback is a cheaper policy for code run
  once (docs/optimizations.md, idea 8).
- **Following the fork.** Racket's Chez fork changes between releases.
  Pinning one tag (decision 2) keeps this to one deliberate update at a
  time.
- **Unchecked code.** rumble is written for `optimize-level 3` (above).
- **Control.** Continuation attachments carry every
  `parameterize` and exception handler, so their cost on our frames
  (docs/continuations.md) matters for all Racket code, not just code
  that captures continuations.
- **Size of the image.** The expander and the layers under it are large
  compiled code. `--racket` may want an image of its own, built by
  `make racket`, rather than growing bin/quasischeme for every mode.

## Alternatives considered

- **Our own layers** (this document's earlier plan): a linklet-to-Lisp
  compiler and the primitive instances written in Lisp, with Racket's
  expander on top. docs/racket-linklet-compiler.md designs it in detail,
  as the fallback if schemify can't be used. It was rejected for the reasons in "Why Racket CS's
  layers, not our own". Its linklet compiler survives as an option for
  the hook, if schemify's output ever fits SBCL badly. Its design notes
  stay useful, since they are what schemify does: known procedures,
  struct inlining, mutated variables, `letrec` checks.
- **Ahead of time only.** Use an installed Racket to compile modules to
  machine-independent linklets, and run them with a small loader of our
  own and no expander. But the `decl` and `stx-data` linklets need the
  expander's runtime (see "Linklets"), and there would be no `eval` or
  run-time macros.
- **Translating Racket ourselves**, with psyntax or a new front end. Not
  viable: Racket's macro system (scope sets, phases, submodules, `#lang`
  readers, `syntax-parse`) is the expander, and a reimplementation would
  always be behind it.
- **Talking to a Racket subprocess.** Libraries become reachable quickly,
  but nothing is shared: no common heap, and data copied both ways.

Prior art: Racket BC and Racket CS are the two backends of the linklet
interface. Pycket (Racket on RPython) moved, in its later versions, to
loading the expander as a linklet and implementing the primitive
instances, as I understand its history. That is the earlier plan's
shape. This plan differs in reusing Racket CS's runtime as well, which
only works because the host can be made to look like Chez Scheme.
