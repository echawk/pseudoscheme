# The REPL

`bin/quasischeme` with no program starts a REPL in its mode's language
(`--r7rs` by default, `--r6rs`, `--r5rs`, `--chez`, `--guile`, `--r4rs`
...). Every mode shares one REPL (src/repl/, the `quasischeme/repl`
system). Guile's mode runs Guile's own REPL instead, with the same line
editor in front of it (below), and so does Racket's (`--racket`, below).

```
$ bin/quasischeme
Quasischeme 1.0 (r7rs) on SBCL 2.6.9
Enter ,help for help.
r7rs> (define (fact n)
        (if (= n 0) 1 (* n (fact (- n 1)))))
r7rs> ,trace fact
r7rs> (fact 2)
(fact 2)
  (fact 1)
    (fact 0)
    => 1
  => 1
=> 2
2
r7rs> ,language chez
;; Chez Scheme 10
> (fx+ 1 2 3)
6
```

## The line editor

At a terminal, input is read by a line editor written in Lisp
(src/repl/editor.lisp, over raw mode through sb-posix; no readline). It
handles:

- **Multi-line input.** Enter evaluates once the parentheses close.
  Before that, it starts an indented line: under the first argument, or
  two columns in for a defining or binding form. Alt-Enter or Ctrl-J
  always inserts a newline, and Tab at the start of a line reindents it.
- **Highlighting.** Parentheses are colored by depth ("rainbow"), and
  the pair around the cursor is underlined. Strings, comments,
  characters, numbers, booleans and syntactic keywords have colors of
  their own, and so do printed values. An unmatched closer is shown in
  red.
- **Completion.** Tab completes the names bound at the top level, and
  after a comma, the meta-commands. When the candidates have nothing
  more in common, Tab lists them.
- **History.** Up and Down, or Ctrl-P and Ctrl-N, move through the
  history; Up and Down move between lines first within a multi-line
  input. Ctrl-R searches backward as you type. Each language keeps its
  history in `$XDG_STATE_HOME/quasischeme/history-<language>`
  (`~/.local/state/...`).
- **Emacs's editing keys:**

  | keys | action |
  |---|---|
  | Ctrl-A, Ctrl-E | start and end of line |
  | Ctrl-B, Ctrl-F | back and forward a character |
  | Alt-B, Alt-F | back and forward a word |
  | Ctrl-K, Ctrl-U | kill to the end or start of the line |
  | Ctrl-W, Alt-D | kill a word back or forward |
  | Ctrl-Y | yank |
  | Ctrl-T | transpose |
  | Ctrl-_ | undo |
  | Ctrl-L | clear the screen |
  | Ctrl-C | cancel the input |
  | Ctrl-D | delete a character; on an empty input, quit |

- **Pasting.** Bracketed paste is on, so a pasted block is inserted
  whole rather than run line by line.

Without a terminal (input from a pipe, `TERM=dumb`), the REPL prompts
and reads lines as before, so scripts that feed it input are unaffected.

## Meta-commands

A line that starts with a comma and a command's name is a meta-command.
Where a command exists in Guile's REPL, it has Guile's name. `,help`
lists them all:

| command | |
|---|---|
| `,help [cmd]` `,h` | the commands, or one command's help |
| `,quit` `,q` | leave |
| `,language [name]` `,L` `,mode` | list the languages, or change to one: r7rs, r6rs, r5rs, chez, guile, r4rs, ieee, r3rs, r2rs, rrs |
| `,load file` `,l` | load a file in the current language |
| `,describe name` `,d` | what a name is bound to: a procedure and its parameters, syntax, a variable and its value |
| `,apropos string` `,a` | the bound names containing a string |
| `,expand expr` `,exp` | the expression with its macros expanded, in the source's names |
| `,time expr` `,t` | evaluate, then show real time, run time and bytes allocated |
| `,pp expr` | evaluate and pretty-print |
| `,trace name ...` `,tr` | trace calls of top-level procedures (with no names: list them) |
| `,untrace [name ...]` | stop tracing |
| `,disassemble expr` `,x` | the native code of a procedure |
| `,backtrace [all]` `,bt` | the last error's backtrace; `all` keeps the evaluator's and the REPL's own frames |
| `,error-info` `,e` | the last error again, with its condition type |
| `,values` `,v` | the values printed so far, numbered |
| `,option [name [value]]` `,o` | show or set options (below) |
| `,history [n]` | the last inputs |
| `,cd dir`, `,pwd` | the current directory |
| `,shell command` `,!` | run a shell command |
| `,clear` | clear the screen |

`,expand` shows psyntax's core forms with the source's names: gensyms
keep the names they were made from. Here `loop` and `i` are the
program's own:

```
r7rs> ,expand (let loop ((i 0)) (if (< i 3) (loop (+ i 1))))
(letrec ((loop (lambda (i) (if (< i '3) (loop (+ i '1)) (void))))) (loop '0))
```

## Options

`,option name value` sets one (`#t`/`#f`, `on`/`off`, or a number):

| option | default | |
|---|---|---|
| `color` | `#t` | highlighting and colored messages |
| `rainbow` | `#t` | color parentheses by depth |
| `paredit` | `#f` | insert closing parentheses and quotes in pairs; delete them in pairs |
| `indent` | `#t` | indent continuation lines |
| `history-length` | 1000 | entries kept in the history file |
| `print-numbered` | `#f` | print values as Guile does, `$1 = ...` |

## Guile

`--guile` (or `,language guile`) runs Guile's own REPL,
`(system repl repl)`, from Guile's sources, so it has all of Guile's
meta-commands: `,describe`, `,apropos`, `,expand`, `,optimize`, `,time`,
`,profile`, `,bt` and `,locals` after an error, `,module`, `,import`,
`,in`, `,show`, `,option`, and `,language elisp` for Emacs Lisp
(docs/guile.md). At a terminal it reads through the same
line editor, as Guile's own `(ice-9 readline)` would. Its current input
port is filled from the editor, and its `repl-reader` passes the prompt
along. Completion covers the current module and the modules it uses,
and after a comma, Guile's meta-commands. The pieces are in
src/repl/guile.lisp:

- `,mode NAME` switches to another of Quasischeme's languages; Guile's
  `,language` is about Guile's own languages.
- `,describe` falls back, without documentation, to a procedure's
  arguments or a value's type.
- `,disassemble` (`,x`) shows the VM's bytecode for a program loaded
  from a `.go` file, and native code otherwise.
- The `interp` option is on: REPL input is evaluated by
  `primitive-eval` rather than compiled. Compiling each input natively
  would take SBCL about half a second.

## Racket

`--racket` (or `,language racket`) runs Racket's own REPL,
`read-eval-print-loop`, with `racket/init` required as `racket` with no
arguments does (docs/racket.md, "The REPL"). The banner and the prompt are
Racket's, `print` is `pretty-print` as `racket/init` sets it, and there are
no meta-commands: a comma is Racket's `unquote`, and `(exit)` or end of
file leaves. At a terminal, which rktio decides (librktio built by
utils/racket/rktio.sh; without it, the way the other languages decide),
the line editor above supplies each input: `current-prompt-read` asks
src/repl/racket.lisp for the text, and Racket's reader finds the data
in it. History is kept for Racket separately, and Tab completes the names
bound in the current namespace.

## Not yet

- Backtraces name Lisp's frames. A Scheme procedure defined at the
  REPL shows as a `(LAMBDA ...)`, since psyntax's lambdas share compiled
  code by shape (`canonical-lambda`, src/psyntax.lisp).
- Guile's `,trace` fails: it traces through the VM's hooks, and
  natively compiled procedures don't run them.
- The Lisp API's `r7rs:repl` and friends (for use inside a Lisp REPL)
  are still the plain loop of src/api.lisp.
