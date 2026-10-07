# Programs: Scheme and Common Lisp, with real libraries

Each program here is a non-trivial use of the bridge (docs/interop.md),
using Common Lisp libraries from Quicklisp and C libraries through CFFI,
and checks its own results. `make test-programs` builds
`bin/pseudoscheme` and runs them all; `sh tests/run-program-tests.sh
bin/pseudoscheme kanren.lisp` runs some. Quicklisp must be installed
(`~/quicklisp/setup.lisp`); it fetches missing libraries the first time.

| program | what it does | libraries |
|---|---|---|
| `c-libraries.scm` | Scheme calls C: compresses bytevectors with zlib (checked against CRC-32 and Adler-32 written in Scheme), sorts with `qsort` and a Scheme comparison as the C callback (and escapes out of it with a continuation), fills `struct tm` for `timegm` and `strftime`, and calls a varargs function | CFFI, zlib, libc |
| `kanren.lisp` | Lisp writes relations (`defun appendo ...`) with a miniKanren's hygienic macros (`fresh`, `conde`, `run*`), which are written in Scheme (`lib/kanren/micro.sld`); checks hygiene against Lisp decoys and user variables, and checks the answers against cl-kanren | cl-kanren |
| `log-report.scm` | parses an access log with `register-groups-bind`, does date arithmetic on local-time timestamps kept in Scheme records, summarizes with Lisp hash tables and `cl:sort` keyed by record accessors, writes JSON with YASON's streaming macros and CSV with cl-csv into Scheme string ports, reads both back, and signs the report with an HMAC | CL-PPCRE, local-time, YASON, cl-csv, Ironclad |
| `sqlite-bank.scm` | a bank in SQLite: transfers in `with-transaction` (a Lisp macro around Scheme code) roll back on a CHECK constraint failure, a Scheme `raise`, or a continuation escape; 200 pseudo-random transfers are checked against a model of the bank in Scheme; blobs are bytevectors | cl-sqlite, SQLite |
| `connect-four.lisp` | the game, with rules and alpha-beta search in Scheme (`lib/games/connect-four.sld`) and the rest in Lisp: CLOS players, the game loop, a seeded Mersenne Twister, colored output, and an evaluation in Lisp that the Scheme search calls at its leaves | mt19937, cl-ansi-text |

The `.scm` programs run on the command line, with `lib/` on the library
path. The `.lisp` programs run in SBCL and load `harness.lisp`, which
loads Quicklisp and Pseudoscheme and reports like SRFI 64.

Behavior at the boundary that the programs show:
- A Lisp function that answers false with `NIL`, but whose name doesn't
  mark it as a predicate (`timestamp<`, `step-statement`,
  `constant-time-equal`), returns `()` to Scheme, and `()` is true in
  Scheme. `lisp-true?` reads such a result the way Lisp does.
- JSON's `false` and `null` both parse as `NIL`, which is `()`, unless
  YASON is told otherwise; Scheme's `#f` and `()` both encode as `null`.
- Strings that Lisp libraries build with fill pointers (YASON's) aren't
  Scheme strings: Scheme strings are simple strings. Copy them
  (`cl:coerce` to `simple-string`).
