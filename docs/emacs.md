# Emacs: Scheme on Quasischeme through SLY or SLIME

Quasischeme runs inside Common Lisp, so Emacs talks to it the way it
talks to any Lisp: through SLY (Slynk) or SLIME (Swank). There's no new
server or protocol.

- `bin/quasischeme --lisp` starts the Lisp's own REPL with all of
  Quasischeme loaded. SLY or SLIME starts it as an inferior Lisp,
  loads Slynk or Swank into it, and connects, as for any SBCL.
- `contrib/emacs/quasischeme.el` then talks Scheme over that
  connection. It calls the functions of the `QUASISCHEME-IDE` package
  (src/repl/ide.lisp) with `sly-eval` or `slime-eval`, whichever is
  connected: evaluate text in a language, complete, arglists, describe,
  expand, load, and the REPL's meta-commands.

Every one of Quasischeme's languages works this way: R7RS, R6RS, R5RS,
Chez Scheme, Guile and the older reports. Each buffer has its language.

## Setting up

Build the command first (`make -C contrib/cli` makes bin/quasischeme).
Then, with SLY:

```elisp
(add-to-list 'load-path "/path/to/quasischeme/contrib/emacs")
(setq quasischeme-program "/path/to/quasischeme/bin/quasischeme")
(require 'sly-quasischeme)
```

`M-x sly-quasischeme` starts it. To have `M-- M-x sly` offer it among
your other Lisps:

```elisp
(add-to-list 'sly-lisp-implementations (sly-quasischeme-implementation))
```

With SLIME it's the same with `slime-` names:

```elisp
(add-to-list 'load-path "/path/to/quasischeme/contrib/emacs")
(setq quasischeme-program "/path/to/quasischeme/bin/quasischeme")
(require 'slime-quasischeme)
;; M-x slime-quasischeme, or:
(add-to-list 'slime-lisp-implementations (slime-quasischeme-implementation))
```

`sly-quasischeme` and `slime-quasischeme` add `quasischeme-mode` to
`scheme-mode-hook`. To have it in Scheme buffers whether or not you
started Quasischeme that way:

```elisp
(add-hook 'scheme-mode-hook #'quasischeme-mode)
```

A connection you already have to a Lisp that has Quasischeme loaded
(`(asdf:load-system :quasischeme-cli)`, or just `:quasischeme/repl`)
works as well: the commands only need the `QUASISCHEME-IDE` package
there.

## Scheme buffers: `quasischeme-mode`

A buffer's language comes from its file: `.sls` and `.sps` are R6RS,
`.ss` Chez Scheme, `.sld` R7RS. A `.scm` file is Guile's if it has a
`define-module` or `use-modules` form, R6RS's if it starts with `#!r6rs`
or a `library` form, and otherwise `quasischeme-default-language`
(`"r7rs"`). The language shows in the mode line, `PS[r7rs]`. `C-c C-v`
changes it; `quasischeme-language-alist` changes the guessing.

| key | command |
|---|---|
| `C-x C-e` | evaluate the form before point; values in the echo area, output in `*quasischeme output*` |
| `C-M-x`, `C-c C-c` | evaluate the top-level form around point |
| `C-c C-r` | evaluate the region |
| `C-c C-l`, `C-c C-k` | load the buffer's file |
| `C-c C-z` | the REPL for the buffer's language |
| `C-c C-d d` | describe the name at point: a procedure and its parameters, syntax, a variable and its value |
| `C-c C-d a` | apropos: the bound names containing a string |
| `C-c RET` | expand the form at point's macros (`*quasischeme expansion*`) |
| `C-c C-b` | the backtrace of the last error |
| `C-c ,` | any REPL meta-command: `trace fact`, `time (fib 25)`, `disassemble car` ... |
| `C-c C-v` | change the buffer's language |

`completion-at-point` (`M-TAB`, or your completion UI) completes from
the names bound at the language's top level. Eldoc shows the parameters
of the procedure being called at point.

## The REPL: `quasischeme-repl`

`C-c C-z`, or `M-x quasischeme-repl`, opens `*quasischeme-repl[r7rs]*`
(one per language). It's a Scheme buffer: RET sends the input once its
parentheses close, and starts an indented line before that. `M-p` and
`M-n` move through the history, and Tab completes. Errors show their
message; `C-c C-b` shows the backtrace.

The REPL's meta-commands work here as at the terminal (docs/repl.md):
`,describe`, `,expand`, `,time`, `,trace`, `,untrace`, `,disassemble`,
`,bt`, `,apropos`, `,load`, `,pp`, `,help`. `,language chez` turns the
buffer into a Chez Scheme REPL.

## SLY's and SLIME's own REPLs

Their REPLs are the Lisp's: Common Lisp, with Quasischeme's Lisp API
(`(r7rs:eval "(+ 1 2)")`, `r7rs:import` and so on; docs/interop.md).
SLY's and SLIME's debuggers, inspectors and cross-referencing work on the
Lisp side as usual. A Scheme error from `quasischeme-mode` doesn't open
the debugger: it's caught and reported, and `C-c C-b` shows its
backtrace.

## How it works

`quasischeme.el` calls the `QUASISCHEME-IDE` package's functions,
which take a language's name and strings and return strings, lists and
plists:

| function | |
|---|---|
| `(languages)` | `((name title) ...)` |
| `(eval-string lang text)` | `(:values (written ...) :output text :error message-or-nil)` |
| `(completions lang prefix)` | the bound names beginning with `prefix` |
| `(arglist lang name)` | `"(name param ...)"` or `nil` |
| `(describe-name lang name)`, `(apropos-names lang string)` | text, names |
| `(expand-string lang text)` | the expansion, written |
| `(load-file lang path)` | as `eval-string` |
| `(command lang ",trace f")` | a meta-command's output |
| `(backtrace)` | the last error's backtrace |

Any other client of Swank or Slynk can call them too. The languages are the REPL's (src/repl/), so a
language added there is one here too.
