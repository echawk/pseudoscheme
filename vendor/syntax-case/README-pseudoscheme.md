# syntax-case under Pseudoscheme: status

This directory is Dybvig & Hieb's reference `syntax-case` implementation
(see `ReadMe`/`Notes` for the original, targeting Chez Scheme), plus
one added file, `hooks-pseudoscheme.ss`, a Pseudoscheme port of
`hooks.ss` (the file the original `ReadMe` says is "the only file you
have to change" to port this to a new Scheme).

## What's confirmed working

Loading `compat.ss`, `hooks-pseudoscheme.ss`, `output.ss`, `init.ss`,
and `expand.pp` (in that order -- `expand.pp` is the pre-expanded form
of `expand.ss`, needed because `expand.ss` uses `syntax-case` to define
itself) brings up a working `expand-syntax`. Calling it directly
(bypassing Pseudoscheme's own `define-syntax`/`syntax-rules` for now --
see below) correctly expands and hygienically renames core forms:

```
(quote x)        => (quote x)
(lambda (x) x)   => (lambda (#:|x|) #:|x|)        ; hygienic rename
(if 1 2 3)       => (if (quote 1) (quote 2) (quote 3))
(let ((x 1)) x)  => ((lambda (#:|x|) #:|x|) (quote 1))
```

`install-global-transformer` and `syntax-dispatch` are both live
(non-placeholder) after loading, so the macro engine itself is fully
initialized.

One real bug was found and fixed in `hooks-pseudoscheme.ss` along the
way: `get-global-definition-hook` must return Pseudoscheme's actual
`#f` when a macro isn't defined, not Common Lisp's bare `NIL` -- NIL is
a legitimate Scheme value here (the empty list; see `core.lisp`'s
`true?`), so `expand.ss`'s `(or (get-global-definition-hook sym)
'(global-unbound))` silently did the wrong thing until this was wrapped
in `true?`.

## What's not working yet

Top-level `(define-syntax my-or (syntax-rules () ...))`, expanded
through `expand-syntax` the same way as above, currently fails with
`invalid syntax NIL`. Core-form expansion (above) works, so this is
specific to how `define-syntax`/`syntax-rules` processing interacts
with the hooks here -- not yet root-caused. Next step for picking this
back up: trace `global-extend` and the `syntax-rules` macro's own
expansion (both in `expand.ss`/`expand.pp`) to see what's producing or
receiving `NIL` where a form was expected.

Separately, `macro-defs.ss` (which redefines standard forms like `cond`,
`let*`, `case`, `do`, `quasiquote` *using* `syntax-case` itself, mostly
as a self-test per the original `loadpp.ss`'s comment) wasn't loaded in
this round of testing -- it tries to globally redefine keywords that
Pseudoscheme's own classifier already intercepts specially
(`define-syntax` is a special operator there, not a redefinable
procedure), which is a separate integration question from whether
`syntax-case` itself works: does a vendored `syntax-case` become
Pseudoscheme's `define-syntax`, or does it live alongside as an
independent, explicitly-invoked library? That's a real design decision
for whoever picks this up, not yet made.

## Loading it yourself

`ps:scheme-load` assumes a `.scm` extension and forwards any other
keyword arguments straight to `cl:load` (which doesn't know
`:source-type`), so the simplest way to load these `.ss` files is to
read and evaluate them form-by-form directly, switching readers for
`hooks-pseudoscheme.ss` only:

```lisp
(asdf:load-system :pseudoscheme/r5rs)

(defun load-scheme-forms (path reader)
  (let ((ps:*scheme-read* reader))
    (with-open-file (in path)
      (loop (let ((form (funcall ps:*scheme-read* in)))
              (when (eq form ps:eof-object) (return))
              (ps:scheme-eval form ps:scheme-user-environment))))))

(let ((cl-bridge #'ps:scheme-read-using-commonlisp-reader)
      (dir #P"vendor/syntax-case/"))
  (load-scheme-forms (merge-pathnames "compat.ss" dir) ps:*scheme-read*)
  (load-scheme-forms (merge-pathnames "hooks-pseudoscheme.ss" dir) cl-bridge)
  (load-scheme-forms (merge-pathnames "output.ss" dir) ps:*scheme-read*)
  (load-scheme-forms (merge-pathnames "init.ss" dir) ps:*scheme-read*)
  (load-scheme-forms (merge-pathnames "expand.pp" dir) ps:*scheme-read*))

(funcall (symbol-value (intern "EXPAND-SYNTAX" "SCHEME")) '(lambda (x) x))
```
