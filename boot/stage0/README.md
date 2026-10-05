# stage0: psyntax on a Scheme without R6RS

psyntax, Pseudoscheme's expander, is written as R6RS libraries that use
`syntax-case`, so its first image has to come from a Scheme that can run
those sources. `boot/psyntax.sh` gets one from Chez Scheme, which has
R6RS natively. stage0 gets one from any R7RS-small Scheme instead:

```sh
boot/psyntax.sh --seed=stage0                       # on Gauche
STAGE0_HOST=chibi boot/psyntax.sh --seed=stage0     # on Chibi
make bootstrap-psyntax SEED=stage0
```

or by hand, in a directory holding psyntax's sources (`psyntax/*.ss` and
`psyntax-buildscript.ss`, copied from `vendor/psyntax/`):

```sh
gosh .../boot/stage0/hosts/gauche.scm               # writes psyntax-pseudoscheme.pp
chibi-scheme .../boot/stage0/hosts/chibi.scm
```

## How it works

`stage0.scm` is portable R7RS-small. It reads psyntax's six libraries
that the build script imports (`config`, `compat`, `internal`,
`builders`, `library-manager`, `expander`) and the build script,
expands them into plain Scheme, and evaluates the result in the host.
psyntax is then running in the host, and its build script expands
psyntax's sources, writing `psyntax-pseudoscheme.pp`: the seed.

- **Libraries are flattened.** Each library's definitions become
  top-level definitions named `library:name`, and references are
  resolved through the library's imports, as R6RS would. Only one name
  clashes between them (`make-collection`), and the prefixes keep all of
  them apart from the host's own.
- **Macros are expanded by stage0**, which knows only what psyntax's
  sources need: `syntax-rules` (builders.ss, config.ss, the build
  script's `define-prims`, two in expander.ss), and five procedural
  macros, rewritten as transformers in `stage0.scm`: `parameterize` and
  `define-record` (compat.ss), `stx-error` and `syntax-match`
  (expander.ss), and `no-source`, an identifier macro for `#f`.
- **Hygiene is by renaming.** Every symbol a macro introduces becomes a
  fresh alias that resolves where the macro was defined, and every local
  variable gets a fresh name, so nothing an expansion introduces can be
  captured or capture. That is enough for these sources; stage0 is not a
  general expander.
- **R6RS procedures** an R7RS host lacks, or has with other arguments
  (`error`, the hashtables, `for-all`, `fold-left`, `list-sort`, ...),
  are defined in `stage0.scm` as `r6:name`.
- **psyntax's host primitives**, its `(psyntax system $bootstrap)`
  library, are `s0:gensym` (interned `g$s0$N`), `s0:eval-core` and so
  on. The code psyntax evaluates while building refers only to global
  locations (the build script maps each primitive to one), so
  `s0:eval-core` translates its free variables into lookups in a table.

A host adapter (`hosts/*.scm`) defines `s0:host-eval` and an eq hash
table, loads `stage0.scm` and calls `(s0:build)`.

## Why the seed doesn't matter

Pseudoscheme rebuilds psyntax from the seed, then from its own result,
until the image reproduces itself, and names the image's gensyms in
order of appearance. So nothing of the seed survives into the result:
the seeds from Chez, Gauche and Chibi all end in the same image, byte
for byte. `make bootstrap-check` and CI check that Chez's and Gauche's
do, and match what's checked in.

Two seeds from unrelated expanders ending in the same image is also
evidence that neither put anything into it that the sources don't say:
David A. Wheeler's "diverse double-compiling", the countermeasure to
Ken Thompson's trusting-trust attack.

## Adding a host

An adapter defines, before loading `stage0.scm`:

- `(s0:host-eval form)`, evaluating at top level, in the environment
  `stage0.scm` is loaded into;
- `(s0:make-table)`, `(s0:table-ref table key default)`,
  `(s0:table-set! table key value)`, `(s0:table-delete! table key)` and
  `(s0:table-keys table)`: a table keyed by `eq?`;
- `(s0:host-delete-file name)`;

then calls `(s0:build)`. Add the host to `stage0_command` in
`boot/psyntax.sh`. The host's `read` must be case-sensitive (R7RS's
is). Brackets in psyntax's sources appear only in comments.
