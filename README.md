# Pseudoscheme

Pseudoscheme is Jonathan Rees' Scheme-to-Common-Lisp translator and
evaluator (originally 1991-1994, targeting CMU CL, Symbolics, VAX Lisp
and LispWorks). Scheme source is classified, alpha-converted, and
translated into Common Lisp, which is then compiled or `eval`'d by the
host Lisp. It loads and runs cleanly under modern SBCL via ASDF.

## Loading

```lisp
(push #P"/path/to/pseudoscheme-asdf/src/" asdf:*central-registry*)
(asdf:load-system :pseudoscheme)
```

`ps:scheme-eval`, `ps:scheme-load`, and `ps:scheme-compile-file` are
the main entry points; `ps:scheme-user-environment` is the default
environment to evaluate or load Scheme code into.

## System layout

- `pseudoscheme/rts` -- the core run-time (booleans, pairs, numeric
  tower primitives, the reader/writer bridge to the host Lisp reader).
- `pseudoscheme/translator` -- the self-hosted Scheme-to-CL translator
  itself (classifier, alpha-converter, `syntax-rules` expander, CL code
  generator). Written in Scheme (the `.scm` files below); loaded
  pre-translated (see **Bootstrap artifacts**).
- `pseudoscheme/evaluator` -- `scheme-eval`/`scheme-load`/REPL support
  built on the two systems above.
- `pseudoscheme` -- the above three, for convenience.
- `pseudoscheme/reader` -- a proper, self-hosted Scheme reader/writer
  (`read.scm`/`write.scm`, adapted from Scheme48's). Loading it
  switches `ps:*scheme-read*`/`*scheme-write*`/`*scheme-display*` away
  from the default CL-reader bridge to this one -- see **Which reader
  to use** below.
- `pseudoscheme/r5rs` -- the R5RS-conformant surface. The runtime
  structure built from `revised^4-scheme-interface` (`ssig.scm`)
  already includes R5RS's additions over R4RS (`values`,
  `dynamic-wind`, `eval`, string ports, ...), so this system is the
  core stack plus `pseudoscheme/reader`, under its R5RS name;
  `tests/run-r5rs-tests.lisp` is what actually gates the R5RS claim.
  R6RS and R7RS-small surfaces are planned as further
  `pseudoscheme/r6rs` / `pseudoscheme/r7rs` systems but aren't
  implemented yet.
- `pseudoscheme/bootstrap` -- regenerates the translator's own `.pso`
  bootstrap files (and `pseudoscheme/reader`'s); see below.

## Which reader to use

Pseudoscheme has two Scheme readers, for two different jobs:

- **The CL-reader bridge** (`ps:scheme-read-using-commonlisp-reader`,
  in `readwrite.lisp`) is the default, installed as part of
  `pseudoscheme/rts`. It reads Scheme source using the host's own
  Common Lisp reader, which means CL package-qualified symbols like
  `ps-lisp:setf` read correctly without any special escaping -- used
  throughout the translator's own sources (`builtin.scm`, `p-utils.scm`,
  `generate.scm`, ...). But it inherits CL's reader syntax exactly, so
  it can't parse the `...` ellipsis identifier (`syntax-rules` uses it
  everywhere; CL treats a run of dots as an error) or Scheme string
  escapes like `\n` (CL reads a backslash as "take the next character
  literally", not as introducing a control character). **This is the
  reader `pseudoscheme/bootstrap` always uses**, regardless of what else
  is loaded (see below) -- regenerating the translator's own `.pso`
  files depends on it.
- **The dedicated reader** (`pseudoscheme/reader`, `read.scm`/`write.scm`)
  is a real Scheme reader/writer and is the right choice for evaluating
  or loading ordinary R5RS/R6RS/R7RS Scheme source: `...`, standard
  string escapes, `#| |#` block comments, `#;` datum comments, and more
  named characters all work, and `write`/`display` show a symbol in the
  case it was actually written in (`'Martin` prints as `Martin`, not
  `MARTIN`) even though, like the rest of the system, identifiers are
  still folded to a canonical case for matching purposes (see
  `read.scm`'s comments for why). It does *not* handle CL
  package-qualified symbols -- `:` has no special meaning in R5RS/R7RS
  symbol syntax (e.g. `:::` is a valid ordinary identifier, used as a
  custom ellipsis marker in some R7RS code) and giving it CL semantics
  here would break that. `pseudoscheme/r5rs` loads this automatically.

Don't load `pseudoscheme/reader` in the same image where you're about
to call `pseudoscheme-bootstrap:regenerate-pso-files` and expect it to
retranslate the *translator's* sources correctly -- those need the
CL-reader bridge's package-qualification support. `regenerate-pso-files`
and `regenerate-closed-pso` both force the CL-reader bridge back on for
the duration of the call regardless of which reader is globally active,
so this is only a concern if you're reaching into the translator
manually rather than through `pseudoscheme-bootstrap`.

## Bootstrap artifacts (`.pso` files)

The translator (`alpha.scm`, `derive.scm`, `generate.scm`, etc., listed
in `translator.files`) is itself Scheme, and is normally loaded
pre-translated to Common Lisp -- that's what the `.pso` files next to
each `.scm` file are. This lets `pseudoscheme/translator` load without
already having a working Pseudoscheme to translate it with (the
classic bootstrapping problem). They're checked in deliberately, not
build output to delete.

Whenever a translator `.scm` file changes, its `.pso` is stale and
must be regenerated by asking the (already-loaded, working) translator
to translate itself again:

```lisp
(asdf:load-system :pseudoscheme)
(asdf:load-system :pseudoscheme/bootstrap)
(pseudoscheme-bootstrap:regenerate-pso-files)   ; retranslates every .scm in translator.files
```

`closed.pso` is different: it isn't translated from a `.scm` source at
all. It's synthesized by `write-closed-definitions` from whatever is
*currently* in `builtin.scm`'s integration table, as real top-level
function definitions for every procedure in
`revised^4-scheme-interface`. So it goes stale whenever `builtin.scm`
changes which CL function backs a Scheme procedure, or how its result
gets coerced to a Scheme boolean -- even though no line of
`closed.pso` itself changed. Regenerate it with:

```lisp
(pseudoscheme-bootstrap:regenerate-closed-pso)
```

`pseudoscheme/reader`'s own `read.pso`/`write.pso` aren't part of
`translator.files` either (they're ordinary runtime code, not part of
the translator), but they do need to be retranslated the same way
whenever `read.scm`/`write.scm` change:

```lisp
(pseudoscheme-bootstrap:regenerate-reader-writer)
```

**After regenerating any `.pso` file**, always:
1. Delete your SBCL fasl cache for this project
   (`~/.cache/common-lisp/sbcl-*/.../pseudoscheme-asdf/`) or wait a
   full second before reloading. ASDF compares source/fasl
   modification times with one-second resolution, so a regenerate
   immediately followed by a reload in rapid succession can be
   mistaken for "already up to date" and silently load the stale fasl.
2. Reload `:pseudoscheme` from a **fresh** Lisp image.
3. Re-run `tests/run-r5rs-tests.lisp` before trusting the result.

## Running the R5RS test suite

```sh
sbcl --script tests/run-r5rs-tests.lisp
```

This loads `pseudoscheme/r5rs` (which pulls in `pseudoscheme/reader`)
and runs chibi's `tests/chibi/r5rs-tests.scm` against it, one top-level
form at a time (so a single failing or erroring test doesn't abort the
run). As of this writing: **183 of 188 tests pass.** The remaining 5
are all genuine, individually-documented gaps (below), not reader
issues -- the whole file now reads correctly.

## Known gaps

- `symbol->string` now preserves case too (`(symbol->string 'Martin)`
  => `"Martin"`) by consulting the same original-spelling stash
  `write`/`display` use (`read.scm`'s `record-original-spelling!`).
  Getting this right required separating it from how the translator's
  *own* internals name the real CL symbols/packages they create for
  Scheme variables and structures -- those now call
  `ps-lisp:symbol-name` directly (`classify.scm`'s `name->string`,
  `module.scm`, `emit.scm`, `reify.scm`, `p-utils.scm`), rather than
  going through `symbol->string` and picking up its case preference by
  accident, which previously renamed every top-level `define` to its
  as-typed (sometimes lower-case) spelling instead of the canonical one
  the rest of the system expects, and broke variable lookup entirely.
- **Only one-shot (escape) continuations work.** `call-with-current-
  continuation` is implemented with a CL `block`/`return-from` (see
  `builtin.scm`), which can only unwind back out to an enclosing call
  -- it can't re-enter a `dynamic-wind` after that call has already
  returned, the classic "re-invoke a captured continuation later"
  pattern (1 known failure). This is an architectural property of how
  call/cc is implemented on top of Common Lisp here, not a quick fix.
- **`syntax-rules` doesn't support a custom ellipsis identifier**
  (R7RS's `(syntax-rules ::: () ...)` form, letting a macro use `:::`
  or another symbol instead of `...`) **or segment patterns with fixed
  trailing arguments** (R7RS's `(args ... penultimate ultimate)`,
  matching a variadic segment followed by required final arguments) --
  3 of the known failures. Both are `rules.scm` (the translator's own
  `syntax-rules` expander) feature gaps, not reader issues; fixing them
  means editing translator.files source, with the regenerate-and-retest
  workflow above.
- Obscure R5RS corner cases around `unquote`/`unquote-splicing`/`...`
  being shadowable as ordinary local variable names without disturbing
  their syntactic role in `quasiquote`/`syntax-rules` are not handled.

## Vendored syntax-case

`syntax-case.tar.gz` at the repo root is Dybvig & Hieb's reference
`syntax-case` implementation (targeting Chez Scheme). The plan is to
port its `hooks.ss` to Pseudoscheme's own expander/eval hooks and
expose it as an optional `pseudoscheme/syntax-case` library -- for code
that wants non-pattern hygienic macros -- without disturbing the
`syntax-rules`-based expander (`rules.scm`) that `pseudoscheme/r5rs`
and friends use by default. Not yet started.
