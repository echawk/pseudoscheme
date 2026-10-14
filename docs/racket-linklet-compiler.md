# A linklet-to-Common-Lisp compiler (the fallback for `--racket`)

Status: **design only**, for the case docs/racket.md's plan can't
cover. That plan runs Racket CS's own layers on the Chez mode and
compiles what schemify produces. This document is what to build
instead if schemify can't be used:
- rumble can't be made to run on `--chez`, so schemify's output has
  nothing to call;
- or schemify's output turns out to be a poor fit for SBCL;
- or we want `--racket` without the Chez fork layer at all.

Then we compile **linklets** ourselves, and write the runtime they call.
Racket's expander, and as much of Racket's other layers as possible,
still run unchanged on top.

## What a linklet compiler has to do

The expander hands the backend one linklet at a time, through
`compile-linklet`. A linklet is fully expanded core Racket. docs/racket.md,
"Linklets", has the grammar and a worked example:

```
(linklet [[imported-id ...] ...]  [exported-id ...]  defn-or-expr ...)

defn-or-expr = (define-values (id ...) expr) | expr
expr = id | (quote datum) | (lambda formals expr) | (case-lambda [formals expr] ...)
     | (if expr expr expr) | (begin expr ...+) | (begin0 expr expr ...)
     | (let-values ([(id ...) expr] ...) expr) | (letrec-values ...)
     | (set! id expr) | (with-continuation-mark expr expr expr)
     | (#%variable-reference) | (#%variable-reference id) | (expr expr ...)
```

Beyond the grammar, the compiler must handle:
- **Import and export shapes.** An import or export may be renamed
  (`[external internal]`).
- **Instances.** A linklet is instantiated against import instances. It
  either makes a new instance or adds its definitions to a target
  instance; the second is how the top level and `eval` work.
- **Variables.** Exported and imported variables are shared *variable
  objects*, which can be set and can be constant
  (`variable-reference-constant?`).
- **Correlated objects.** Source locations wrap expressions (`correlated?`,
  `datum->correlated`). They give procedure names and error locations,
  and are stripped otherwise.
- **`compile-linklet`'s options:**
  - `serializable`: the result must be writable to a `.zo`;
  - `unsafe`: checks may go;
  - `static`: variables can be resolved when compiled, because the
    linklet will only be instantiated once;
  - `use-prompt`: each top-level form runs under the default
    continuation prompt;
  - `uninterned-literal`, `quick`.
- **Cross-linklet information.** `compile-linklet` takes `import-keys`
  and a `get-import` callback, and returns new keys. This is how a
  backend learns about the linklets it imports: that a variable is a
  procedure of known arity, a constant, or a struct accessor.
- **The rest of `#%linklet`'s 29 functions:**
  - `instantiate-linklet`, `make-instance`, `instance-variable-value`,
    `instance-set-variable-value!`, `instance-describe-variable!`;
  - `linklet-import-variables`, `linklet-export-variables`;
  - `recompile-linklet`, `eval-linklet`;
  - `read-linklet-bundle-hash`, `write-linklet-bundle-hash` (the `.zo`
    format);
  - `variable-reference->instance`, `variable-reference-constant?`;
  - the `correlated` operations;
  - `primitive-table`, `primitive->compiled-primitive`,
    `primitive-in-category?`.

## What we already have

The pieces a linklet compiler would reuse, from strongest to weakest
fit:

| need | what exists | where |
|---|---|---|
| core Scheme to CL | the translator: `lambda` (with rest arguments), `if`, `set!`, `define`, `quote`, `begin`, `letrec` and calls; open-coded primitives; inline fixnum and flonum arithmetic | `translate-core`, src/psyntax.lisp; src/builtin.scm |
| a body of definitions as direct calls | definitions become one `labels` (`definitions-as-letrec`); big bodies are hoisted to top-level definitions (`hoist-definitions`) | src/psyntax.lisp |
| R6RS `letrec` checks | `letrec-checks` and `%letrec-check` | src/psyntax.lisp |
| **a compiler from another core language to ours** | the Tree-IL compiler. Tree-IL is Guile's equivalent of linklets: it renames lexicals apart, turns module variables into cached sites, makes `lambda-case` (optional, keyword and several clauses) a rest-list dispatcher, and maps primcalls to host procedures. 909 lines; the closest model for this compiler | src/guile/compile.lisp |
| full and composable continuations, prompts, `dynamic-wind` | the frame transformation (`cc-transform`) on core Scheme, so anything the compiler emits gets it free; the safe-procedure analysis keeps ordinary code fast | src/continuations.lisp, docs/continuations.md |
| continuation marks | the Chez mode's (`with-continuation-mark`, `continuation-marks`) | src/chez/ (ac9fb00) |
| engines (preemptive green threads) | the Chez mode's | src/chez/ (ac9fb00) |
| applicable objects | funcallable instances: Guile mode's applicable structs, its `vm-program` | src/guile/runtime.lisp, src/guile/vm.lisp |
| records | R6RS records, Chez's record types (rtds that are records, sealed and opaque, `$record` internals), Guile's structs and vtables | src/r6rs/records.lisp, src/chez/, src/guile/runtime.lisp |
| numbers | the full tower: exact rationals and complexes, IEEE flonums, `fl`/`fx` operations | src/numbers.lisp, src/r6rs/arithmetic.lisp |
| strings, bytevectors, characters, Unicode | yes, and read-only strings (Guile mode) | src/r6rs/, src/guile/ |
| hash tables | `eq`/`eqv`/`equal` and weak tables | src/r6rs/hashtables.lisp, Guile's `ghash` |
| ports | R6RS binary and textual ports, custom ports | src/r6rs/ports.lisp |
| an FFI | CFFI, as Guile mode's `(system foreign)` uses it | src/guile/foreign.lisp |
| compiled-code caching | the library cache: fasls keyed by a hash of the form and a build signature | src/library-cache.lisp |
| whole-unit compilation | `compile-whole-library`, Chez mode | src/chez/top-level.scm |

So the compiler front half is the new part. The back half (translator,
continuations, SBCL) exists, and the Tree-IL compiler shows the shape.

## The compiler

A Lisp module, `src/racket/linklet/`, modelled on src/guile/compile.lisp:

```
linklet S-expression (with correlated objects)
  1 decorrelate     keep names and locations in a side table; plain S-expressions after
  2 rename          lexicals renamed apart (as Tree-IL's ~1, ~2): no capture of host names
  3 analyse         per variable: defined, imported, exported, mutated (set!), referenced
                    before its definition, known value (lambda and arity, constant,
                    struct-type piece)
  4 lower           core Scheme: the forms below
  5 translate       translate-core, with the continuation transformation; then SBCL
```

Passes 1 to 4 are new, perhaps 1,500 to 3,000 lines going by the
Tree-IL compiler's 909 lines. Pass 3 is where the speed is.

### Lowering, form by form

| linklet form | core Scheme it becomes | notes |
|---|---|---|
| `(define-values (x) e)`, internal | `define`, so it joins the body's `labels` | most definitions |
| `(define-values (x) e)`, exported | an instance-variable set, plus a direct binding for uses inside the linklet | exports are variable objects shared with importers |
| `(define-values (x y ...) e)` | `call-with-values` into the definitions | rare outside the expander |
| imported `x` | a reference to its variable object, resolved once per instantiation (Tree-IL's sites); a constant known through `get-import` is used directly | |
| `lambda` | `lambda` | a name from the correlated object's `inferred-name` |
| `case-lambda` | a rest-list dispatcher on the argument count, as Tree-IL's `lambda-case`; one clause is a plain `lambda` | later: CL `&optional` where the clauses nest |
| `let-values` with one value per binding | `let` (`(lambda (x ...) body)`) | the common case: let-values from the expander is mostly this |
| `let-values`, several values | `call-with-values` | |
| `letrec-values` | `letrec`, with `letrec-checks` | Racket raises on use before definition, as R6RS does |
| `begin0` | a host `%begin0`, which keeps the first form's values and runs the rest (`multiple-value-prog1` in CL) | |
| `with-continuation-mark` | the Chez mode's marks; a mark in tail position replaces the frame's mark | the semantics `parameterize` and exception handlers need |
| `(#%variable-reference)` | an object holding the instance, and the variable for `(#%variable-reference x)` | `variable-reference-constant?`, `variable-reference->instance` |
| `quote` | `quote`; literals a `.zo` can't hold (paths, regexps, hash tables, prefab structs) are rebuilt when instantiated, for `serializable` | |
| `set!` on an exported or imported variable | a set of the variable object | also invalidates "constant" |
| application of a known non-procedure, wrong arity | Racket's error (`application: not a procedure`, `arity mismatch`) | message formats from the reference |

Each top-level form gets the default prompt under `use-prompt`. The
whole body is instantiated in an instance's environment; `static`
linklets resolve their variables at compile time.

### Analysis (pass 3): what makes it fast

Schemify's design is the model. We would reimplement it, not copy it:
- **Known procedures.** A variable bound once to a `lambda` or
  `case-lambda` is called directly, with its arity checked at compile
  time. The `labels` from `definitions-as-letrec` already does this
  within a linklet; across linklets, the knowledge travels through
  `import-keys` and `get-import`.
- **Struct types.** The `(define-values (struct:s make-s s? s-a set-s-a!)
  (let-values (((struct: make- ? -ref -set!) (make-struct-type ...))) ...))`
  shape the expander emits for every `struct` is recognized. Then
  constructors, predicates and accessors become inline operations on a
  known layout (docs/optimizations.md, idea 2) rather than calls to
  closures.
- **Mutated variables.** Only variables that are `set!` need boxes. SBCL
  already boxes only closed-over assigned variables, so this mostly
  matters for exported ones.
- **Constants.** A variable never assigned and bound to a literal is
  folded.
- **Safety for continuations.** `cc-transform`'s safe-procedure analysis
  runs on the lowered code. It needs the cross-linklet knowledge too,
  or every imported call is a capture site (docs/optimizations.md,
  idea 3).
- **Unsafe.** The `unsafe` option, and `#%unsafe` primitives, compile
  under `(safety 0)` (docs/optimizations.md, idea 1).

### What the compiled form is

As in docs/racket.md, "Compiled code":
- for `serializable`, the `.zo` holds the linklet S-expression, after
  passes 1 to 3, with the analysis's results;
- the SBCL fasl is in the library cache, keyed by its hash.

`write-linklet-bundle-hash` and `read-linklet-bundle-hash` write and
read our format. Racket's other code only sees bundles and hashes.

## The runtime it calls

This is the bulk of the work.

**The layers above rumble are linklets too.** Racket's thread, io and
regexp layers, and the expander, are extracted into single linklets
that depend only on primitives. Our compiler compiles them like any
other linklet. So the runtime we write is **what rumble provides to
those layers**, not all of `#%kernel`. A large part of `#%kernel` comes
from them, written in Racket:
- ports and files (io);
- threads, `sync` and events, custodians, semaphores (thread);
- regexps (regexp);
- `eval`, `namespace-*` and modules (expander).

What remains for us, grouped:

| area | needed | from what we have |
|---|---|---|
| pairs and lists | immutable pairs; `mcons` mutable pairs as a separate type; `list?` | CL conses (Racket can't mutate them anyway); mpairs, a struct |
| numbers | `#%kernel`'s arithmetic, `#%flfxnum`, `exact->inexact` and friends; no single-flonums or extflonums | the numeric tower; `#%extfl` as stubs, as Racket CS has |
| strings, bytes, characters | mutable and immutable strings and byte strings, `string->immutable-string` | read-only strings (Guile mode); bytevectors |
| symbols and keywords | interned, uninterned and unreadable symbols; keywords as their own type | Pseudoscheme's symbols; CL keywords (as Guile mode does) |
| structs | `make-struct-type` with properties, guards, inspectors, prefab structs, `prop:procedure` (applicable structs) | new, on funcallable instances; record machinery to borrow from |
| impersonators and chaperones | of procedures, structs, vectors, boxes, hash tables | new: wrapper procedures, and checks in the accessors |
| hash tables | mutable `equal`/`eqv`/`eq` and weak; **immutable hash tables** with Racket's persistent semantics; `equal-hash` | the mutable ones exist; a persistent map (a HAMT) is new |
| control | continuation marks, prompts with tags, composable continuations, `dynamic-wind`, `call-with-continuation-barrier`, `#%paramz` | marks (Chez mode), continuations and prompts (docs/continuations.md) |
| errors | the `exn` struct hierarchy, `raise` with barriers, `error-display-handler` | new structs; Guile and R6RS conditions show the pattern |
| engines and timers | the thread layer's engines, `current-inexact-milliseconds`, interrupts | the Chez mode's engines |
| memory | weak boxes, ephemerons, wills, `collect-garbage`, phantom bytes | SBCL weak pointers and finalizers |
| the operating system | **rktio**, the C library io calls for files, sockets, processes and polling | built from Racket's source (`racket/src/rktio/`) as a shared library by the setup script, called through CFFI |
| FFI | `#%foreign`, which `ffi/unsafe` is written on | CFFI (Guile mode's `(system foreign)` shows how) |
| places and futures | `#%place`, `#%futures` | single place; futures run sequentially, as the old plan's table says |

rktio deserves a note. It is Racket's portable OS layer, in C, under the
Racket license (MIT/Apache). Building it as a shared library and binding
it with CFFI is the cheapest way to get io's exact behaviour.
Reimplementing it on SBCL's `sb-posix` is the alternative.

## Testing

- **Hand-written linklets first.** The `fact`/`greet` linklet in
  docs/racket.md, then each form in "Lowering", compared with Racket.
  `racket -e` with `racket/linklet`'s `compile-linklet` and
  `instantiate-linklet` gives the expected results.
- **Differential.** A linklet's results here against `racket -M`'s, its
  machine-independent interpretation, over the extracted layers' own
  tests.
- **The layers, in order:** regexp (`rx.rktl`, already used by
  contrib/racket-linklets), thread, io, then the expander instantiating.
- **Racket's test suite**, `pkgs/racket-test-core`, with a tally, as in
  docs/racket.md, milestone 7.

## Milestones

1. Passes 1 to 4 and `#%linklet`, enough for hand-written linklets with a
   handful of primitives.
2. The regexp linklet compiled by our compiler, on our primitives, and
   `rx.rktl` passing. This is the same test contrib/racket-linklets
   passes through schemify, so it compares the two routes directly.
3. Structs with properties, and the exn hierarchy.
4. Engines and the thread linklet.
5. rktio and the io linklet.
6. The expander linklet instantiates; `(eval '(+ 1 2))`.
7. `racket/base`, with the cache.
8. Pass 3's cross-linklet knowledge, measured on `racket/base`'s compile
   time and on bench/.

## Status

Milestone 1 runs; parts of milestone 3 and the control row are done.

- **The compiler** (src/racket/linklet/compiler.lisp):
  - passes 1 and 3 exist. Pass 3 records what each top-level name is
    and which ones are `set!`. Known values and early use are still
    to do.
  - Passes 2 and 4 are one walk: lowering renames lexicals apart as it
    goes. It handles every form in the table except quoted literals
    that need rebuilding.
  - A compiled linklet is a core lambda of its variable objects,
    imports first and then exports. Its body is a `letrec*` of the
    definitions.
- **`#%linklet`** (src/racket/linklet/api.lisp): instances, variables,
  correlated objects, `compile-linklet` and `instantiate-linklet`, with
  and without a target. Bundles, directories and `.zo` reading and
  writing are stubs.
- **The native runtime** (src/racket/native/):
  - structs: properties with supers and guards, auto fields, immutable
    fields, and `prop:procedure` both ways, through funcallable
    instances;
  - the exn hierarchy, `raise`, `error`, and Lisp errors raised as
    Racket exceptions;
  - continuation marks, prompts with tags, parameters and
    parameterizations;
  - mutable hash tables, and immutable ones on a hash array mapped
    trie.
  - The host's procedures are used wherever Racket's mean the same
    (`*host-shared*` in native.lisp).
- **Tests:** `sh tests/racket/run.sh linklet` runs the linklets in
  tests/racket/linklets/ and compares each with its `.expected` file.
  A file there may also hold `(define-linklet NAME (linklet ...))`,
  which the harness instantiates for later linklets to import from
  instance NAME.
- **Coverage:** `sbcl --script tests/racket/coverage.lisp [--missing]`
  counts how much of each of Racket CS's primitive tables the instances
  have. It reads the tables from the pinned source, which is the
  checklist. It also lists the primitives whose arity mask differs from
  the table's.

- **The expander instantiates** (milestone 6's first half), with
  `sbcl --dynamic-space-size 4GB --script tests/racket/coverage.lisp --instantiate`:
  - it reads BC's `startup.inc` and lowers it in 0.2 s, and compiles it
    form by form in about 25 s;
  - all 2,004 top-level forms run, through the expander's own
    `namespace-init!`;
  - the instances lack 118 of the primitives it refers to. Every procedure
    in Racket CS's tables that we lack is installed as a stub, which
    raises "not implemented yet" when it is called, not when it is
    referenced.

- **The expander evaluates.** `bin/pseudoscheme --racket=linklet -e
  '...'` reads each form with Racket's datum syntax and evaluates it
  through Racket's expander, in a namespace with `#%kernel`. That is the
  `boot`, `namespace-require` and `eval` that Racket CS's main.sps
  does. tests/racket/expander.rktd holds the expressions and values
  tests/racket/run.sh checks: 10 of 10 pass.
- **The compiled expander is cached** (src/racket/cache.lisp): a 21 MB
  fasl under `~/.cache/pseudoscheme/racket/`. It is keyed by
  startup.inc's text, the build signature and src/racket/'s file dates.
  The first process compiles it (about 30 s); after that,
  `--racket=linklet -p '(+ 1 2)'` takes 0.4 s.

- **`racket/base` loads and runs**, from the pinned source's
  `racket/collects`. Each module is expanded from source by Racket's
  expander and compiled by our compiler, and racket/base takes about a
  minute. Keyword arguments, `for` loops, structs, `with-handlers`,
  hash tables and regexps work (tests/racket/base/). The natives that
  stand in for the io and regexp layers are in src/racket/native/:
  - `ports.lisp`;
  - `paths.lisp`;
  - `printer.lisp`;
  - `regexp.lisp`, on CL-PPCRE.

- **Programs run**: `bin/pseudoscheme --racket=linklet prog.rkt` runs a
  module as `racket prog.rkt` does, its `#lang`, its body and its `main`
  submodule (tests/racket/programs/). Every linklet's compiled code is
  cached, keyed by its text, so a `#lang racket/base` program starts in
  about 18 s, which is the expansion of racket/base. The first run after
  src/racket/ changes compiles everything again (about 100 s).

- **.zo files.** A module file is compiled serializable as it loads, and
  its `.zo` is written under a cache root (`current-compiled-file-roots`,
  `~/.cache/pseudoscheme/racket/zo-SIGNATURE/`), never into Racket's tree.
  The expander's own load handler finds it next time. Our
  `write-linklet-bundle-hash` and `read-linklet-bundle-hash` write and
  read a bundle as Racket data; the expander's machine-independent
  bundles it writes itself. With both caches warm, a `#lang
  racket/base` program runs in 1.3 s.
  `PSEUDOSCHEME_RACKET_ZO=0` turns `.zo` writing off.

Next:
- milestone 2, the regexp linklet;
- the rest of `#%kernel` that the extracted layers use, in the order the
  coverage report's `--missing` list makes plain.

## How it compares with docs/racket.md's plan

This is strategy B of docs/racket.md's "Three strategies"; that section
has the performance assessment.


| | Racket CS's layers (docs/racket.md) | this compiler |
|---|---|---|
| new compiler code | none; one hook | passes 1 to 4 (about 1,500 to 3,000 lines) |
| runtime | rumble itself, plus the Chez fork's additions | rumble's exports, ours: structs, impersonators, persistent hashes, control, exns, rktio bindings |
| what decides speed | schemify | pass 3 |
| what decides fidelity | Racket's code | how closely our rumble follows Racket's |

The two share everything above rumble, and the two hooks are
interchangeable. If both get built, the regexp linklet compiled either
way (milestone 2 here, milestone 2 there) is the natural differential
test between them.
