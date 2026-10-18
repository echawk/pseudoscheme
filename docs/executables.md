# Standalone executables

```sh
bin/pseudoscheme [MODE-FLAG] [-L DIR] [-D FEATURE] --compile FILE [-o OUT]
./OUT ARGUMENT ...
```

`MODE-FLAG` is any of the mode flags (`--r7rs`, the default, `--r6rs`,
`--r5rs`, `--r4rs`, `--ieee`, `--r3rs`, `--r2rs`, `--rrs`, `--chez`,
`--guile`, `--racket`).  OUT is FILE without its extension if `-o` is not
given.  SRFI 138's `compile-r7rs [-I dir] [-A dir] [-D feature] [-o out] FILE`
(a link to `bin/pseudoscheme`) is the same as `--compile` in R7RS mode.

The executable is an image of `bin/pseudoscheme` with FILE's work done at
build time, so it starts in about 0.05 s (0.15 s for a Racket program of
`#lang racket`).  It needs no repository, no caches, no source files and
not `bin/pseudoscheme`, and it ignores `~/.sbclrc`.  Its arguments are the
program's: `(command-line)` (R6RS, R7RS, Chez; the first element is the
executable's name), Guile's `(program-arguments)`, Racket's
`(current-command-line-arguments)`.  Its exit status, the flushing of
output and the reporting of errors are those of running the program
(`bin/pseudoscheme FILE`); an uncaught error exits with 70 (Guile and
Racket: 1), `(exit n)` with n.  Control-C exits with 130.

| mode | at build time | at start-up |
| --- | --- | --- |
| R7RS, R6RS | the program's own libraries installed (expanded, compiled); the program expanded and compiled | the libraries it imports are invoked, in order; the program runs |
| R5RS and the older reports, Chez | FILE's forms expanded one by one, each after the ones before it, at the mode's top level, and compiled | the libraries they import are invoked; the forms run |
| Guile | the program's modules loaded (`use-modules`, `define-module`, ...: their bodies run at build time); the forms expanded and compiled natively, those that define modules and macros also run, as `compile-file` does | the compiled forms run in `(guile-user)` |
| Racket | the expander instantiated; FILE's module declared (compiled) with the modules it requires; the collection modules among them (`racket/base`, `racket/list`, ...) instantiated | the module runs, then its `main` submodule |

Differences from running the program:

- Top-level modes (R5RS, the older reports, Chez) expand each form without
  running the ones before it, so a macro whose transformer calls a
  procedure that the program defines at top level does not work; put such
  a procedure in a library.
- A Guile module's body (a `display` at its top level) runs once, at build
  time, not at start-up.  Guile's installed sources and `GUILE_LOAD_PATH`
  are needed only if the program loads a module it did not name in
  `use-modules` forms at its top level (say, from `(resolve-module ...)`
  or a `use-modules` inside a procedure).
- A Racket program that loads a collection module at run time that it did
  not require at its top level (`dynamic-require`, `eval` of a `require`)
  needs the Racket collections (`build/external/racket`) where they were
  at build time.  A Racket build takes as long as the first run of the
  program does: a few seconds for `racket/base`, minutes the first time for
  `#lang racket`.  `--racket=cs` is not supported.
- `-L` and `GUILE_LOAD_PATH` apply to the build only.

## Sizes and the runtime options

The executable is 140 MB (Racket: 200 MB for `racket/base`, 500 MB for
`#lang racket`).  It keeps the heap limit of `bin/pseudoscheme` (8 GB, see
`contrib/cli/Makefile`) and its 256 MB main-thread control stack: SBCL
saves the thread stack size with the image (UIOP saves the runtime's
options), and by the time of the build `limit-thread-stacks` had lowered it
for threads to 32 MB, so `build-executable` sets it back to the main
thread's own size before saving, and the executable lowers it for its
threads again at start-up.  `PSEUDOSCHEME_THREAD_STACK_SIZE` works in the
executable.  Racket's `current-directory` is set from the real one at
start-up (a saved image's is the build's).

Tests: `contrib/cli/test.sh` builds an executable per mode, removes its
source, runs it from another directory with no cache and no home, and
checks its arguments, output and status.
