# Renaming Pseudoscheme to Quasischeme (plan for 1.0)

At 1.0 the project becomes **Quasischeme**. The name still credits
Jonathan Rees' Pseudoscheme, which this code grew from, but marks it as
a different system. 1.0 means:

- every Revised Report, R1RS through R7RS;
- Guile and Chez;
- all the SRFIs;
- Emacs through SLY/Slynk (and SLIME/Swank);
- possibly an LSP server for other editors.

There are no outside users, so nothing old is kept for compatibility:
no package nicknames or system aliases, and no `PSEUDOSCHEME_*` variables
read as a fallback.

The rename is one mechanical commit, made by `utils/rename/rename.sh`
(section 0), followed by regeneration and the full test run. This
document lists everything it touches. The counts in section 1 are from
2026-10-11, at commit 95dd7fd; section 0 has 2026-10-17's.

## 0. The scripts (utils/rename/)

```sh
sh utils/rename/rename.sh            # dry run: the files it would rewrite and move
sh utils/rename/rename.sh --apply    # rewrite tracked text files, git mv the named ones
sh utils/rename/check.sh             # what's left of the old name, for review
sh utils/rename/out-of-repo.sh       # prints section 5's steps for the user; runs nothing
```

- `rename.sh --apply` refuses to run with uncommitted changes. It rewrites
  only tracked text files (`git grep -lI`), skipping the paths in
  `utils/rename/allowlist.txt` (Rees' original files and this
  document), with section 4's perl rules, which also cover the newer
  `PSR` (`PSEUDOSCHEME-RACKET`) prefix. It keeps "Pseudoscheme" before
  "2.", "1." or "was " (history prose), then `git mv`s every file named
  for the project and `src/*.pso` to `src/*.qso`.
- Then: `check.sh`, the history prose by hand (README's opening
  paragraph, section 3), section 2's Emacs and version items, the
  out-of-repo steps, and section 6's tests.
- **Trial run (2026-10-17, at e8a8b3f plus the scripts):** in a scratch
  worktree, `--apply` rewrote 495 files and moved 40 (17 named, 23
  `.pso`); `check.sh` left one line, a ROADMAP.md sentence of history
  to review; the renamed tree builds `bin/quasischeme`, which runs
  programs, and R7RS's suite passes on it, 978 of 978. Two rules were
  found missing on the way and are in: files whose only mention is a
  string designator (`(in-package "PS")`), and the bare `"pso"` file
  type (src/core.lisp, the .asd, boot/check.lisp).
- **Since 2026-10-11** the name also appears in:
  - the packages `PSEUDOSCHEME-RACKET` (`PSR`) and
    `PSEUDOSCHEME-LIBGUILE`;
  - 35 `PSEUDOSCHEME_*` environment variables, among them the Racket
    mode's (`RACKET_STRATEGY`, `RACKET_CORE`, `RACKET_REGEXP`, ...), the
    libguile ones (`LIBGUILE_PROFILE`, `LIBGUILE_TRACE`, ...) and
    `GUILE_GO`; `rename.sh` renames them all with the rest
    (`git grep -ohI -E 'PSEUDOSCHEME_[A-Z_]+' | sort -u` lists them);
  - the benchmarks' per-mode files (`Pseudoscheme{R6RS,R5RS,Chez,Guile,Racket}-*lude.scm`)
    and the `bench` script's system names (`pseudoscheme-r6rs` and so on);
  - the container's machine, image and volumes (`utils/container/config.sh`):
    tear the container down with the old names *before* applying the
    rename, and set it up again after (`out-of-repo.sh` says so).
- The C side of libguile (`psg_*` functions in src/libguile/*.c) is
  internal; the rules leave it, as nothing outside sees it.

## 1. Size

| What | Count |
|---|---|
| tracked files | 2,053 |
| files mentioning the name (any case) | 370 |
| `pseudoscheme` / `PSEUDOSCHEME` / `Pseudoscheme` | 1,057 / 469 / 476 |
| `ps:` / `ps::` package prefixes | about 1,670 / 42 |
| `ps-lisp:` | 745 |
| `psx:` / `psx::` | 291 / 108 |
| `ps-r7rs:` / `ps-r6rs:` | 122 / 93 |
| `psl:` / `psg:` / `psffi:` | 50 / 43 / 26 |
| `(pseudoscheme ...)` Scheme library names | about 150 |
| `:pseudoscheme` / `'pseudoscheme` (systems, features) | 130 / 54 |

By directory:

| Directory | References |
|---|---|
| src | 919 |
| contrib | 298 |
| top level | 166 |
| vendor (our psyntax changes) | 137 |
| docs | 107 |
| tests | 85 |
| boot | 62 |
| bench | 35 |
| .github | 21 |
| examples | 16 |

## 2. The mapping

### Common Lisp packages (names and nicknames)

| Now | 1.0 |
|---|---|
| `PS` (nicknames `PS-LISP`, `PSEUDOSCHEME`) | `QS` (nicknames `QS-LISP`, `QUASISCHEME`) |
| `PSEUDOSCHEME-PSYNTAX` / `PSX` | `QUASISCHEME-PSYNTAX` / `QSX` |
| `PSEUDOSCHEME-ENVIRONMENTS` / `PSL` | `QUASISCHEME-ENVIRONMENTS` / `QSL` |
| `PSEUDOSCHEME-FFI` / `PSFFI` | `QUASISCHEME-FFI` / `QSFFI` |
| `PSEUDOSCHEME-GUILE` / `PSG` | `QUASISCHEME-GUILE` / `QSG` |
| `PSEUDOSCHEME-R6RS` / `PS-R6RS` | `QUASISCHEME-R6RS` / `QS-R6RS` |
| `PSEUDOSCHEME-R7RS` / `PS-R7RS` | `QUASISCHEME-R7RS` / `QS-R7RS` |
| `PSEUDOSCHEME-API`, `-ASDF`, `-INTEROP`, `-IDE`, `-REPL`, `-CLI`, `-BOOTSTRAP`, `-BOOT-CHECK` | `QUASISCHEME-...` |
| `PS-REPL` (one reference) | `QS-REPL` |

`SCHEME`, `REVISED^4-SCHEME`, `SCHEME-TRANSLATOR` and the `chez:`/`guile`
packages don't contain the name, so they stay. Before committing, check
that `QS`, `QSX`, `QSL`, `QSG` and `QSFFI` aren't already used by
anything in Quicklisp or ocicl that we load. Search `~/quicklisp`,
`~/ocicl` and `(list-all-packages)` in a loaded image.

### ASDF systems

| Now | 1.0 |
|---|---|
| `pseudoscheme.asd` | `quasischeme.asd` |
| `pseudoscheme`, `pseudoscheme/rts`, `/translator`, `/evaluator`, `/reader`, `/r5rs`, `/environments`, `/cffi`, `/r6rs`, `/api`, `/asdf`, `/r7rs-runtime`, `/r7rs`, `/bootstrap`, `/guile`, `/repl` | `quasischeme...` with the same suffixes |
| `contrib/cli/pseudoscheme-cli.asd`, system `pseudoscheme-cli` | `quasischeme-cli.asd`, `quasischeme-cli` |

`r5rs`, `r6rs` and `r7rs` stay. The `:defsystem-depends-on
("pseudoscheme/asdf")` in examples/mixed-system changes with the others.

### Commands

- `bin/pseudoscheme` becomes `bin/quasischeme`.
- Add the short names from `todo`:
  - `qs`, the REPL and script runner, the same as `quasischeme`;
  - `qsc`, the compiler to Common Lisp or fasl, with `quasischeme --compile`-style options.
- Both are symlinks or two-line scripts made by `make -C contrib/cli`, the
  way `scheme-r7rs`, `compile-r7rs` and the others already are.
- The existing `scheme-*` and `compile-r7rs` names stay.
- The `todo` item about naming emitted executables `guile` or `chez` is separate from this rename.

### Environment variables

Rename all twelve variables from `PSEUDOSCHEME_*` to `QUASISCHEME_*`:

- `LIBRARY_PATH`, `LIBRARY_CACHE`, `LIBRARY_CACHE_DIRECTORY`;
- `THREAD_STACK_SIZE`;
- `GUILE_COMPILER`, `GUILE_VM_BACKEND`, `GUILE_JIT`, `GUILE_JIT_THRESHOLD`, `GUILE_COMPILE_VALUE`, `GUILE_CACHE`, `GUILE_CACHE_DIRECTORY`;
- the `PSEUDOSCHEME_SURELY_UNSET_VARIABLE` test.

The bench script's `PSEUDOSCHEME=` variable becomes `QUASISCHEME=`.

### What Scheme code sees

- **Library names:** `(pseudoscheme lisp)`, `(pseudoscheme host)`,
  `(pseudoscheme control)`, `(pseudoscheme reports ...)`, `(pseudoscheme
  interaction)`, `(pseudoscheme clos)`, `(pseudoscheme chez ...)` and the
  others become `(quasischeme ...)`. This includes library files on disk under
  src/ whose directory is named `pseudoscheme`, if there are any. Check with
  `git ls-files | grep /pseudoscheme/`.
- **Feature identifier:** `cond-expand` / `features` change `pseudoscheme` to
  `quasischeme`. This is in src/pseudoscheme-features.scm and the R7RS features list.
- **Library file suffix:** `foo.pseudoscheme.sls` becomes `foo.quasischeme.sls`
  (docs/libraries.md, and the library search code).
- **Cache directory:** `~/.cache/pseudoscheme/{libraries,guile}` becomes
  `~/.cache/quasischeme/...`. Old caches are simply orphaned; delete them by hand.

### Files to `git mv`

| Now | 1.0 |
|---|---|
| `pseudoscheme.asd` | `quasischeme.asd` |
| `contrib/cli/pseudoscheme-cli.asd` | `contrib/cli/quasischeme-cli.asd` |
| `contrib/emacs/pseudoscheme.el`, `sly-pseudoscheme.el`, `slime-pseudoscheme.el` | `quasischeme.el`, `sly-quasischeme.el`, `slime-quasischeme.el` |
| `src/pseudoscheme-features.scm`, `src/pseudoscheme-record.scm` | `src/quasischeme-features.scm`, `src/quasischeme-record.scm` |
| `vendor/psyntax/README-pseudoscheme.md`, `psyntax-pseudoscheme.pp` | `README-quasischeme.md`, `psyntax-quasischeme.pp` |
| `bench/r7rs-benchmarks/src/Pseudoscheme-postlude.scm` | `Quasischeme-postlude.scm` (the bench script builds this name from the system name) |
| the 23 `src/*.pso` | `src/*.qso` (decided: the extension follows the name; update `.pso` in the Makefile, boot/, the .asd, CI's cache key and docs) |

### Emacs

- All 52 `pseudoscheme-*` symbols become `quasischeme-*`: commands, defcustoms,
  faces, `pseudoscheme-mode`, `M-x pseudoscheme` and `provide`/`require` features.
- In `sly-pseudoscheme` and `slime-pseudoscheme`, the contrib names and the
  `--lisp` back-end lookup change too.
- Update docs/emacs.md's setup snippets.
- Users' own `init.el` files are outside the repo and need the same rename.

### Version

- `3.0` becomes `1.0` in `version.sexp` and in the 16 `:version` lines of
  the .asd files. Also check the CLI's `--version` output and the Slynk
  `connection-info`.
- 3.0 continued Rees' numbering (he released up to 2.13). 1.0 starts the new name's numbering.

## 3. What must *not* change

Mentions of Rees' original program are history, so leave them alone:

- **README:** the opening paragraph says what Pseudoscheme was. Rewrite the
  introduction as "Quasischeme grew from Jonathan Rees' Pseudoscheme
  (versions up to 2.13, 1991-1994, ...)".
- **The original's files:**
  - `LICENSE`, `:author "Jonathan Rees"`, and the `Copyright (c) 1991 Jonathan Rees` file headers;
  - `src/pseudo.lisp`, his 1991 loader script (`/zu/jar/pseudo/...`); keep it as it is, or delete it, since nothing loads it;
  - `src/jar-defrecord.scm` and other files that are Rees' or Kelsey's.
- **Names that only look alike:**
  - `psyntax`, Ghuloum and Dybvig's expander: it is not "ps" plus "yntax";
  - `old-revised-reports/` and `rnrs-pdfs/` (untracked);
  - `bench/r7rs-benchmarks` lines about other systems.
- **Git history and commit messages.**

Write these exceptions in as an allowlist, so that the check at the end can
tell they're deliberate.

## 4. Doing it with sed

Order matters: replace the longest patterns first, and match on word boundaries.
Run it on tracked text files only (`git grep -lI`), never on .fasl
files, caches or the untracked files. On macOS, use `gsed` or `perl -pi`
for `\b`. Below is a sketch in perl, which behaves the same on macOS and
Linux:

```sh
files=$(git grep -lI -i -E 'pseudo|\bps(x|l|g|ffi|-lisp|-r[67]rs|-repl)?::?' -- . \
        ':!LICENSE' ':!src/pseudo.lisp')
perl -pi -e '
  s/PSEUDOSCHEME/QUASISCHEME/g;
  s/Pseudoscheme(?! (?:2\.|was|1\.))/Quasischeme/g;   # leave historical prose; review by hand
  s/pseudoscheme/quasischeme/g;
  s/\b(ps|PS)-(lisp|LISP|r6rs|R6RS|r7rs|R7RS|repl|REPL)\b/ ($1 eq "ps" ? "qs" : "QS")."-$2" /ge;
  s/(?<![\w-])(psx|psl|psg|psffi)(::?)/ "qs".substr($1,2).$2 /ge;
  s/(?<![\w-])(PSX|PSL|PSG|PSFFI)\b/ "QS".substr($1,2) /ge;
  s/(?<![\w*%-])ps(::?)(?=[\w*%+&<>=!?\/-])/qs$1/g;
  s/(?<![\w*%-])PS(::?)(?=[\w*%+&<>=!?\/-])/QS$1/g;
  s/"PS"/"QS"/g;
  s/\.pso\b/.qso/g;
  s/\bps-(symbol|package|exports)\b/qs-$1/g;            # internal helpers, for consistency
' $files
```

Also check for these:

- A blanket replacement of `pseudo` would hit `pseudo.lisp`, the word "pseudo-"
  in prose, and similar. Only `pseudoscheme` itself is replaced.
- No `ps:` occurs inside a word today. `steps:`, `maps:` and the like
  all have a letter before them, so the lookbehind leaves them alone. Look
  for prose `PS:` afterwards. There is none today.
- `(in-package "PS")`, `(find-package "PS")` and `(intern ... "PS")`
  contain string package designators. The `"PS"` rule covers them.
  Afterwards, search for `"PS-` and `:ps-` to catch the rest.
- The generated files (`src/*.pso`, `src/spack.lisp`,
  `vendor/psyntax/psyntax-*.pp`) contain package names too. Either
  sed them like source and check that a re-bootstrap reproduces them
  byte for byte, or regenerate them from scratch:
  `SCHEME=chez boot/bootstrap.sh`, then `make bootstrap-psyntax` and `make bootstrap-check`.

Then `git mv` the files in section 2. Fix up their `load`/`:file`
references and the Makefiles. Finally, the .el `provide` names must match the new file names.

## 5. Outside the repo (done by hand, at rename time)

- **GitHub:** rename `echawk/pseudoscheme` to `echawk/quasischeme`. GitHub
  redirects the old URL. Then `git remote set-url echawk ...`.
- **The checkout:** rename `~/schemes/pseudoscheme` to `~/schemes/quasischeme`.
  Claude Code keys its project memory on the path. Copy
  `~/.claude/projects/-Users-ethan-schemes-pseudoscheme/` to
  `-Users-ethan-schemes-quasischeme/`, and keep the old one: earlier sessions'
  transcripts there are still referenced.
- **Caches:** delete `~/.cache/pseudoscheme`, and any ASDF fasl cache
  under `~/.cache/common-lisp/` for the old paths.
- **Emacs config:** update `load-path` and `require` lines.

## 6. Checking it

1. `make test`, `make test-escape`, `make test-cli`, `make test-srfi` and `make test-programs`.
2. The Guile suite (docs/guile.md) and the Chez mats (`tests/run-chez-mats.sh`).
   The totals should be unchanged: no test depends on the name.
3. In batch Emacs, run the SLY and SLIME round trips (docs/emacs.md).
4. CI: `.github/actions/setup-lisp` has a cache key and a `quickload`
   that both name the system.
5. Check what's left with `git grep -n -i pseudo`. It should list only the
   allowlist in section 3.

Do it in one commit, "Rename to Quasischeme; version 1.0", after
the other 1.0 work is done. A rename in the middle of other work makes
every later diff harder to read.
