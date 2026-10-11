# Emacs: Scheme on Pseudoscheme through SLY or SLIME

Pseudoscheme runs inside Common Lisp, so Emacs talks to it the way it
talks to any Lisp: through SLY (Slynk) or SLIME (Swank). There's no new
server or protocol.

- `bin/pseudoscheme --lisp` starts the Lisp's own REPL with all of
  Pseudoscheme loaded. SLY or SLIME starts it as an inferior Lisp,
  loads Slynk or Swank into it, and connects, as for any SBCL.
- `contrib/emacs/pseudoscheme.el` then talks Scheme over that
  connection. It calls the functions of the `PSEUDOSCHEME-IDE` package
  (src/repl/ide.lisp) with `sly-eval` or `slime-eval`, whichever is
  connected: evaluate text in a language, complete, arglists, describe,
  expand, load, and the REPL's meta-commands.

Every one of Pseudoscheme's languages works this way: R7RS, R6RS, R5RS,
Chez Scheme, Guile and the older reports. Each buffer has its language.

## Setting up

Build the command first (`make -C contrib/cli` makes bin/pseudoscheme).
Then, with SLY:

```elisp
(add-to-list 'load-path "/path/to/pseudoscheme/contrib/emacs")
(setq pseudoscheme-program "/path/to/pseudoscheme/bin/pseudoscheme")
(require 'sly-pseudoscheme)
```

`M-x sly-pseudoscheme` starts it. To have `M-- M-x sly` offer it among
your other Lisps:

```elisp
(add-to-list 'sly-lisp-implementations (sly-pseudoscheme-implementation))
```

With SLIME it's the same with `slime-` names:

```elisp
(add-to-list 'load-path "/path/to/pseudoscheme/contrib/emacs")
(setq pseudoscheme-program "/path/to/pseudoscheme/bin/pseudoscheme")
(require 'slime-pseudoscheme)
;; M-x slime-pseudoscheme, or:
(add-to-list 'slime-lisp-implementations (slime-pseudoscheme-implementation))
```

`sly-pseudoscheme` and `slime-pseudoscheme` add `pseudoscheme-mode` to
`scheme-mode-hook`. To have it in Scheme buffers whether or not you
started Pseudoscheme that way:

```elisp
(add-hook 'scheme-mode-hook #'pseudoscheme-mode)
```

A connection you already have to a Lisp that has Pseudoscheme loaded
(`(asdf:load-system :pseudoscheme-cli)`, or just `:pseudoscheme/repl`)
works as well: the commands only need the `PSEUDOSCHEME-IDE` package
there.

## Scheme buffers: `pseudoscheme-mode`

A buffer's language comes from its file: `.sls` and `.sps` are R6RS,
`.ss` Chez Scheme, `.sld` R7RS. A `.scm` file is Guile's if it has a
`define-module` or `use-modules` form, R6RS's if it starts with `#!r6rs`
or a `library` form, and otherwise `pseudoscheme-default-language`
(`"r7rs"`). The language shows in the mode line, `PS[r7rs]`. `C-c C-v`
changes it; `pseudoscheme-language-alist` changes the guessing.

| key | command |
|---|---|
| `C-x C-e` | evaluate the form before point; values in the echo area, output in `*pseudoscheme output*` |
| `C-M-x`, `C-c C-c` | evaluate the top-level form around point |
| `C-c C-r` | evaluate the region |
| `C-c C-l`, `C-c C-k` | load the buffer's file |
| `C-c C-z` | the REPL for the buffer's language |
| `C-c C-d d` | describe the name at point: a procedure and its parameters, syntax, a variable and its value |
| `C-c C-d a` | apropos: the bound names containing a string |
| `C-c RET` | expand the form at point's macros (`*pseudoscheme expansion*`) |
| `C-c C-b` | the backtrace of the last error |
| `C-c ,` | any REPL meta-command: `trace fact`, `time (fib 25)`, `disassemble car` ... |
| `C-c C-v` | change the buffer's language |

`completion-at-point` (`M-TAB`, or your completion UI) completes from
the names bound at the language's top level. Eldoc shows the parameters
of the procedure being called at point.

## The REPL: `pseudoscheme-repl`

`C-c C-z`, or `M-x pseudoscheme-repl`, opens `*pseudoscheme-repl[r7rs]*`
(one per language). It's a Scheme buffer: RET sends the input once its
parentheses close, and starts an indented line before that. `M-p` and
`M-n` move through the history, and Tab completes. Errors show their
message; `C-c C-b` shows the backtrace.

The REPL's meta-commands work here as at the terminal (docs/repl.md):
`,describe`, `,expand`, `,time`, `,trace`, `,untrace`, `,disassemble`,
`,bt`, `,apropos`, `,load`, `,pp`, `,help`. `,language chez` turns the
buffer into a Chez Scheme REPL.

## SLY's and SLIME's own REPLs

Their REPLs are the Lisp's: Common Lisp, with Pseudoscheme's Lisp API
(`(r7rs:eval "(+ 1 2)")`, `r7rs:import` and so on; docs/interop.md).
SLY's and SLIME's debuggers, inspectors and cross-referencing work on the
Lisp side as usual. A Scheme error from `pseudoscheme-mode` doesn't open
the debugger: it's caught and reported, and `C-c C-b` shows its
backtrace.

## How it works

`pseudoscheme.el` calls the `PSEUDOSCHEME-IDE` package's functions,
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
