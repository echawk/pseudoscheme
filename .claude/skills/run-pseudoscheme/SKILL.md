---
name: run-pseudoscheme
description: Build, run, drive and smoke-test pseudoscheme (bin/pseudoscheme) — evaluate Scheme in r7rs/r6rs/r5rs/chez/guile/racket modes, feed the REPL, load the systems from source in SBCL, run one test suite. Use when asked to run, start, build, test, try or check a change in pseudoscheme.
---

# Run pseudoscheme

A Scheme-to-Common-Lisp compiler on SBCL. The agent handle is
`.claude/skills/run-pseudoscheme/driver.sh` (POSIX sh): every command
is hard-killed after `LIMIT` seconds (default 60). Paths are relative
to the repo root. Verified on macOS/arm64, SBCL 2.6.9.

## Prerequisites

`sbcl` on PATH (Homebrew: `brew install sbcl`). Quicklisp in
`~/quicklisp` is used if present. `expect` (in macOS) only for pty REPL work.

## Build

```sh
sh .claude/skills/run-pseudoscheme/driver.sh build   # make -C contrib/cli; ~40 s cold, ~12 s warm
```

Rebuild after any `src/` change: `bin/pseudoscheme` is a saved image and
does not see source edits. Build log: `$TMPDIR/pseudoscheme-build.log`.

## Run (agent path)

```sh
D=.claude/skills/run-pseudoscheme/driver.sh
sh $D smoke                                   # PASS/FAIL per mode: r7rs r6rs r5rs r4rs chez guile racket repl
sh $D eval chez '(fx* 6 7)'                   # => 42; MODE = r7rs r6rs r5rs r4rs r3rs r2rs rrs ieee chez guile racket
sh $D eval r6rs '(vector-map + #(1 2) #(10 20))'
sh $D run r7rs prog.scm                       # a program file (racket: a #lang file)
LIMIT=120 sh $D run r7rs --compile prog.scm -o prog   # standalone executable
printf '(define x 10)\n(+ x 1)\n(car 5)\n,q\n,q\n' | sh $D repl guile   # REPL from stdin
sh $D lisp poke.lisp                          # ASDF-load :r7rs from source, then load poke.lisp
sh $D suite interop                           # tests/run-interop-tests.lisp (cache off) => "116 of 116"
```

`lisp` is the path for changes to `src/` internals without rebuilding the
image; in `poke.lisp` use `r7rs:eval`, `r7rs:expand`, `r7rs:translate`,
`r7rs:define`, e.g. `(print (r7rs:eval "(import (scheme base)) (exact-integer-sqrt 17)"))`.

For line-editor / colour work the REPL needs a pty (it only edits when
stdin and stdout are terminals and `TERM` isn't `dumb`): use `expect`
(`spawn bin/pseudoscheme --guile`, `expect "> "`, `send "(+ 1 2)\r"`).

## Run (human path)

`bin/pseudoscheme [--MODE]` opens an interactive REPL (`,help`, `,q`).

## Test

Run only the suites a change touches (`sh $D suite NAME`; names from
`tests/run-*-tests.lisp`, e.g. `r7rs`, `r6rs`, `continuation`,
`syntax-case`). Raise `LIMIT` for big suites. CLI smoke tests:
`make -C contrib/cli test` (rebuilds first; 44 tests; give `test.sh` an absolute path if run directly).

## Gotchas

- R7RS *program files* need `(import (scheme base) (scheme write))`;
  `-p` and the REPL import for you. Without it: `expander: unbound identifier (SCHEME::DISPLAY ...)`.
- The first `--racket` run after a rebuild takes ~17 s (its compiled
  cache is rebuilt); later runs ~1 s. A cold `smoke` may need `LIMIT=120`.
- Guile's REPL: `,q` at an error level (`[1]>`) only pops the level;
  send `,q` twice. Piped output runs prompts and values together on a line.
- Chez mode prints errors as `Exception: not a pair 5`; Guile as
  `ERROR: Wrong type argument: 5`. Grep accordingly.
- No `timeout(1)` on macOS and SBCL ignores SIGALRM, so the driver kills
  with a background `sleep; kill -9`. Exit 137 + `driver: killed after Ns` = hit the cap.

## Troubleshooting

- `repl` prints only the banner and first prompt: stdin didn't reach the
  child. sh gives `&` jobs `/dev/null` for stdin; the driver redirects
  explicitly (`<&3`) — keep that if you edit `capped`.
- A command piped to `tail` hangs for the full `LIMIT`: the kill-watcher
  inherited the pipe; its output must go to `/dev/null` (as in `capped`).
