# syntax-case under Pseudoscheme: status

This directory is Dybvig & Hieb's reference `syntax-case` implementation
(see `ReadMe`/`Notes` for the original, targeting Chez Scheme), plus
Pseudoscheme-specific files:

- `hooks-pseudoscheme.ss` -- the port of `hooks.ss` (the file the
  original `ReadMe` says is "the only file you have to change"), and
  the handful of Chez built-ins the sources assume.
- `extras-pseudoscheme.ss` -- `when` and `unless`, which `macro-defs.ss`
  uses but defines nowhere (Chez has them built in).

and one edit to a vendored file: `macro-defs.ss`'s `delay` support code
used Chez's `[ ]` brackets, which the Scheme reader here (rightly, for
R5RS) doesn't read; they are now parentheses.

Everything is loaded by `src/syntax-case.lisp` (system
`pseudoscheme/syntax-case`); see the top-level README's "syntax-case"
section for how to use it and `tests/run-syntax-case-tests.lisp` for
what's covered.

## Load order

As in the original `loadpp.ss`: `compat.ss`, `hooks-pseudoscheme.ss`
(with the CL-reader bridge, because it uses bare `ps-lisp:foo`
symbols), `output.ss`, `init.ss`, `expand.pp` (the pre-expanded form of
`expand.ss`, which is written in terms of `syntax-case` itself), then --
what an earlier attempt at this left out, and which is why top-level
`define-syntax` appeared to fail with `invalid syntax NIL` --
**`macro-defs.ss`**. `syntax-rules`, `with-syntax`, `or`, `cond`, `case`,
`do`, `quasiquote`, ... are not built into the expander: `macro-defs.ss`
defines them using `syntax-case`, and `(define-syntax m (syntax-rules
...))` fails until `syntax-rules` itself exists.

## What had to be fixed to get there

All in this directory or `src/`:

- `symbol->string` (in `src/rts.lisp`) crashed on *uninterned* symbols
  -- exactly what hygienic renaming makes -- because it assumed every
  symbol has a package to qualify with.
- Chez built-ins the sources use but Scheme doesn't have: `list*`,
  `gensym`, `top-level-bound?` (all in `hooks-pseudoscheme.ss`).
- `get-global-definition-hook` must return Scheme `#f`, not Lisp `NIL`
  (which is the empty list here), for "not defined".
- The environment macro transformers are evaluated in is now a hook
  (`expander-environment-hook`), so code expanding inside a library sees
  that library's bindings.

## Known limits of this expander (they are the 1992 design's)

- No identifier macros: a macro keyword used as a plain identifier is
  "invalid context for identifier", so `identifier-syntax` /
  `make-variable-transformer` can't be real.
- `syntax-rules` (from `macro-defs.ss`) is R5RS-level: no patterns after
  an ellipsis, no custom ellipsis, no `x ... ...`.
- Macros are global (one table), not scoped to environments or
  libraries.

## The design question, answered for now

Does a vendored `syntax-case` become Pseudoscheme's `define-syntax`, or
live alongside? **Alongside**: forms must go through `sc:sc-eval` /
`sc:sc-load`. The expander's output (core forms only) is fed to the
ordinary translator, so nothing in the translator changed. What it
costs, and ways to remove the cost, is in `ROADMAP.md`
("syntax-case and the two macro worlds").
