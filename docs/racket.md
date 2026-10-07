# Racket on Pseudoscheme: a `--racket` mode

Status: investigation and plan; nothing implemented yet.

The goal: `pseudoscheme --racket prog.rkt` runs Racket programs, with
`racket/base` and the libraries on the package server, by reusing as
much of Racket's own implementation as possible. Racket code would then
be one more thing a Common Lisp program can call, like Scheme today
(docs/interop.md).

## Summary

Make Pseudoscheme a **third backend for Racket's linklet layer**, next
to Racket CS (Chez Scheme) and Racket BC (C). Concretely:

1. a compiler from **linklets** (Racket's small core language) to Common
   Lisp;
2. the **primitive instances** that linklets refer to (`#%kernel`,
   `#%unsafe`, `#%paramz`, ...), written in Lisp, mostly on top of the
   R6RS/R7RS runtime we already have;
3. **Racket's own expander**, loaded as one big linklet, running on 1
   and 2. It provides the module system, `#lang`, the reader, macros,
   `eval` and `compile`. Everything from `racket/base` up is then
   Racket's own code, unchanged.

The output of `schemify` is the wrong layer to target (see "Why
linklets"). The hard runtime problems are continuation marks and
prompts (Racket's `parameterize`, exception handlers and `with-handlers`
are built on them), structs with properties, and ports. Full
re-entrant continuations exist now (docs/continuations.md); marks and
prompts would go on the frames they already push.

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

## Linklets

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
`(primitive-table '#%kernel)` etc.:

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

## Why linklets, not schemify output

The todo suggested either linklets or schemify's output. Linklets are
the better target:

- **They're the documented, stable boundary.** Both Racket backends
  consume them, and the expander produces them. Schemify is an internal
  pass of the CS backend.
- **Schemify's output is Chez- and rumble-specific.** It calls rumble
  internals (variable objects, `#%app` variants, record operations,
  `$value`, Chez's `#3%` unsafe primitives) and assumes Chez's
  `define-record-type`, `call/cc`, attachments and engines. Running it
  would mean reimplementing rumble's internal API, which is larger and
  less specified than the primitive instances.
- **We need `compile-linklet` anyway.** Once the expander is hosted, it
  produces linklets at run time (`eval`, macros, REPL). Whatever we
  consume ahead of time must also be compilable then.

Schemify is still worth reading, for what our linklet compiler should
do: inferring known procedures and constants across linklets,
recognizing struct-type definitions so that accessors can be inlined,
finding mutated variables (only those need boxes), checking `letrec`
for use before definition, and lifting closures.

## The plan

### Stage 1: a linklet compiler

`src/racket/linklet.lisp`: `compile-linklet` from the S-expression form
to a Lisp function, compiled with `compile`.

- **Shape.** A linklet becomes `(lambda (self import-1 ... import-n) ...)`.
  Imports are read from the import instances into local variables when
  the linklet is instantiated, or through `variable` cells when they
  may be mutated.
- **Definitions.** Exported variables live in cells owned by the
  instance (`instance-variable-value` reads them). A definition that is
  never `set!` and is defined once can be referenced directly inside
  the linklet. Internal definitions become `let`s/`labels`.
- **Forms.** `case-lambda` becomes one `&rest` function that dispatches
  on argument count (or a funcallable object carrying its arity, for
  `procedure-arity`). `begin0` is `multiple-value-prog1`, `let-values`
  is `multiple-value-call`/`multiple-value-bind`.
  `with-continuation-mark` is described under stage 2.
- **Reuse.** psyntax's output is the same kind of language, and the
  translator (src/*.scm) already compiles it to Lisp: lambdas, `if`,
  `letrec`, calls, proper handling of `set!`. The linklet compiler can
  be a front end to the translator's back end rather than new code.
  The open-coding of primitives (the translator's built-ins,
  src/builtin.scm, and the host's inline primitives,
  `*inline-primitives*` in src/psyntax.lisp) carries over.

Test it on linklets dumped from a real Racket: a `boot/racket/` script,
like the one used above, that compiles modules with
`current-compile-target-machine` set to `#f` and writes their linklets
as S-expressions. That's also a test corpus for stage 2.

### Stage 2: the primitive instances

`src/racket/kernel.lisp` and friends. A coverage check like
tests/check-r7rs-exports.py, listing `(primitive-table '#%kernel)`
against what's defined, keeps track of progress. Much is already there
in the R6RS/R7RS runtime: numbers (src/numbers.lisp), characters and
strings with Unicode, bytevectors (byte strings), hash tables, records,
conditions. What's new:

- **Continuation marks and prompts.** These are used everywhere:
  `parameterize` (`#%paramz`), exception handlers
  (`call-with-exception-handler` is a mark), `with-handlers` (an escape
  plus marks), error reporting (`continuation-mark-set->context`),
  `dynamic-wind`, the default prompt around each module body and REPL
  interaction, `call-with-continuation-prompt` and
  `abort-current-continuation`.
  - In direct style, marks can be a special variable holding a list of
    mark frames. `with-continuation-mark` pushes a frame, or replaces
    the key in the current one when it's in tail position with respect
    to another `with-continuation-mark` in the same frame. The linklet
    compiler can see that, as schemify does.
  - A dynamic binding isn't a tail call, so `(let loop () (wcm k v
    (loop)))` would grow the stack where Racket runs it in constant
    space. Code like this is rare outside the runtime, but it exists.
  - Prompts and aborts, and escaping continuations
    (`call-with-escape-continuation`, which is most `call/cc` use in
    practice) map onto `catch`/`throw`, as an escape-only `call/cc`
    does today (one the analysis proves is only used to escape, or any
    with `--continuations=escape`).
  - **Composable and re-entrant continuations** (`racket/control`,
    `racket/generator`, the web server's `send/suspend`). Re-entrant
    ones exist: the generalized-stack-inspection transformation of
    docs/continuations.md is the default. Composable ones exist only in
    SRFI 226's library implementation on top of `call/cc`; natively, a
    composable continuation would copy the frames up to a prompt frame
    and rebuild them on top of the current stack, and marks would be a
    slot in each frame (docs/continuations.md, "Further work").
- **Structs.** `make-struct-type` with properties, guards, inspectors,
  prefab structs, and `prop:procedure`: applicable structs. Keyword
  procedures (`make-optional-keyword-procedure` above) are applicable
  structs. On CL, a funcallable instance (closer-mop's
  `funcallable-standard-class`) is an object that is also a function,
  so Racket's applicable structs stay callable from Lisp. Our R6RS
  records (src/r6rs/records.lisp) are the starting point.
- **Impersonators and chaperones** (contracts use them): wrapper
  objects that the struct, vector, hash and procedure primitives look
  through.
- **Hash tables.** Racket's immutable hashes (`hash-set`) need a
  persistent map. rumble's is a HAMT written in Scheme, which could go
  through our translator. Mutable and weak tables need
  `hash-iterate-first`/`-next`, iteration by position, which CL's
  tables don't offer directly.
- **Ports.** Racket's `io` layer is written for rktio, a C library. It's
  simpler to implement the port primitives on our ports (Gray streams,
  src/r6rs/ports.lisp): peeking, line counting, special values, custom
  ports (`make-input-port` with its callbacks), progress events.
- **Threads.** Racket's threads are green threads that the `thread`
  layer schedules with engines (preemption via continuations and timer
  interrupts). Two options: map them onto Lisp threads (simplest, but
  the runtime's shared state then needs locking; see ROADMAP.md 4,
  thread safety), or host the `thread` layer on full continuations,
  which exist (SRFI 226's own threads already run that way). Start
  with Lisp threads.
- **Keywords.** Racket's keywords can be Lisp keywords: `#:name` reads
  as `:NAME` today (docs/interop.md), so a Racket keyword argument is
  already the Lisp spelling.
- **Strings and pairs.** Racket pairs are immutable and `mcons` pairs
  are a separate type, so CL conses are fine. Racket strings can be
  immutable (literals, `string->immutable-string`), which CL can't
  mark. A weak set of immutable strings, or a wrapper type, is needed
  for `immutable?`.
- Custodians, will executors, logging, security guards and plumbers
  can start nearly empty.

### Stage 3: host the expander

Build the expander linklet from Racket's source
(`racket/src/expander`, extracted into one linklet the way Racket CS's
build does), with an installed Racket of the same version. This is the
same pattern as boot/psyntax, where Chez builds psyntax's seed.
Instantiate it with our primitive instances and `#%linklet`. Then we
have Racket's `read-syntax`, `namespace-require`, `expand`, `compile`
and `eval`, and `#lang` works.

`racket/base` and its dependencies are a few hundred modules. Expanding
them from source on every start is far too slow (it's slow even on
Racket CS), so compiled linklets must be cached. That builds on the
existing compiled-library cache (src/library-cache.lisp), which keeps
Lisp fasls keyed by a library's form, a build signature and the ids of
the libraries it was expanded against; for Racket the key would be the
module's source hash, the versions of the modules it depends on, and
the expander's version, the way Racket keys `compiled/*.zo`. Two ways
to fill it:

- compile from source with the hosted expander (always possible);
- or, as a shortcut, load machine-independent `.zo` files that a real
  Racket of the same version wrote (`racket -M`, `--compile-any`; with
  `PLTCOMPILEDROOTS` pointing somewhere other than the installation's
  own `compiled/` directories), and hand their linklets to our
  `compile-linklet`/`recompile-linklet`. That skips our expansion of
  the collections entirely.

The thread, io and regexp layers could later be hosted the same way.
regexp is the easiest of the three and would save reimplementing
Racket's regexp syntax.

### Stage 4: the command line and the bridge

- `pseudoscheme --racket prog.rkt`, and a Racket REPL.
- From Lisp: `(racket:require 'racket/list)`, Racket procedures called as
  functions; keyword procedures called with Lisp keywords.
- From Racket: a `(require (cl <package>))`-style bridge, in the spirit
  of the Scheme one.
- `raco pkg`: packages are directories of modules. Installing with a
  real Racket's `raco pkg install` and pointing our collection paths at
  the result is enough at first.

## Alternatives considered

- **Ahead of time only.** Use an installed Racket to compile modules to
  machine-independent linklets, translate those to Lisp, and run them
  with a small module loader of our own, with no expander at run time.
  It's a smaller first step, but the `decl` and `stx-data` linklets
  depend on the expander's runtime, so the loader would grow into a
  partial reimplementation of the expander's module system. There would
  be no `eval` or run-time macros either. It's worth doing only as the
  cache-filling shortcut of stage 3.
- **Translate Racket ourselves**, with psyntax or a new front end. Not
  viable: Racket's macro system (scope sets, phases, submodules, `#lang`
  readers, `syntax-parse`) is the expander, and reimplementing it would
  be a project of the expander's size that's always behind.
- **Talk to a Racket subprocess.** Libraries become reachable quickly,
  but nothing is shared: no common heap, and data is copied in both
  directions. Useful as a stopgap, not as `--racket`.

Prior art: Racket BC and Racket CS are the two backends of this
interface. Pycket, Racket on RPython, moved to the same approach in its
later versions (loading the expander as a linklet and implementing the
primitive instances), as I understand its history. That suggests the
interface is workable for a third party.

## Risks and costs

- **Size.** About 1,650 primitives, plus continuation marks, structs,
  impersonators and ports done to Racket's specification. This is
  larger than the R6RS and R7RS runtimes put together. It can be
  incremental: each primitive can be checked against Racket's own test
  suite (`pkgs/racket-test-core`).
- **Version coupling.** The expander linklet, the primitive tables and
  the `.zo` format change between Racket versions. Pin one version (9.3
  is installed here) and move deliberately.
- **Control.** Re-entrant continuations exist, so the risk is now
  continuation marks and prompts in the runtime, and composable
  continuations on the existing frames. Without them, `parameterize`,
  exception handling, `racket/control` and the web server's
  `send/suspend` won't run as Racket's own code expects.
- **Speed.** Contracts, keyword procedures and generic sequences (`for`
  over non-specialized sequences) are expensive without schemify-style
  optimizations, so the linklet compiler's inference matters.

## First steps

1. `boot/racket/dump-linklets.rkt`: compile a module and its
   dependencies machine-independently and write their linklets as
   S-expressions (the snippet above, generalized).
2. The linklet compiler and `#%linklet`, tested on hand-written linklets
   and on `fact`/`greet`, with the few primitives they use.
3. Continuation marks and prompts in the runtime, on the frames full
   continuations push (docs/continuations.md, "Further work").
4. A primitive-coverage script, then `#%kernel` until the expander
   linklet instantiates, then until `racket/base` instantiates.
