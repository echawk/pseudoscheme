# SRFIs

The SRFI libraries that ship with Pseudoscheme: every final SRFI that
hasn't been withdrawn, as of SRFI 274. Each `N.sld` is an R7RS
`define-library` for `(srfi N)` (a few are R6RS `library` forms). psyntax finds them with no setup:
`psx:*system-library-path*` roots every name that starts with `srfi` here.
That root is searched after the user's `psx:*library-path*`, so a project
can still supply its own SRFIs, for instance Akku's chez-srfi.

Other names resolve to these libraries as well (`src/r7rs/front.lisp`,
`srfi-alias-form`):

- The R6RS spelling of SRFI 97, `(srfi :1)` and `(srfi :1 lists)`. A name
  with a trailing identifier gets a synthesized library that re-exports
  everything in `(srfi :1)`.
- SRFI 261's portable `(srfi srfi-1)`.
- The R7RS-large names that have an implementation here (Red and
  Tangerine editions): `(scheme list)`, `(scheme hash-table)`,
  `(scheme charset)`, `(scheme vector)`, `(scheme sort)`,
  `(scheme comparator)`, `(scheme generator)`, `(scheme stream)`,
  `(scheme box)`, `(scheme bitwise)`, `(scheme fixnum)`,
  `(scheme division)`, `(scheme set)`, `(scheme rlist)`,
  `(scheme text)`, `(scheme mapping)`, `(scheme mapping hash)`,
  `(scheme regex)`, `(scheme flonum)`, `(scheme list-queue)`,
  `(scheme lseq)`, `(scheme ideque)` and `(scheme vector u8)` through
  `(scheme vector c128)`.

Each `N.sld` here also makes `srfi-N` a feature for `cond-expand` (SRFI
0), as do the SRFIs built into the reader, the expander and the command
line, which have no library: 0, 22, 30, 46, 62, 97, 105, 138 and 169
(`psl:*scheme-features*`, src/environments.lisp). Some others are mostly
built in, their libraries holding what procedures they have: the reader
syntax of 10, 49, 58, 88, 107, 108, 109, 110, 119, 163, 207 and 270
(`src/read.scm`, `src/quasi.lisp`), and the expander forms of 139, 149,
212, 213 and 251 (`vendor/psyntax`).

`make precompile-srfi` compiles all of them into the compiled-library
cache ahead of time (`bin/pseudoscheme --precompile-srfi` for one
continuation mode; `pseudoscheme-api:precompile-libraries` from Lisp).

`tests/N.scm` tests SRFI N, an R7RS program that exits with 0 when its
tests pass; `make test-srfi` runs them all, in order, a line each
(`tests/run-srfi-tests.sh`; `make test-srfi SRFIS="1 13"` for some). The
older SRFIs' tests are in `tests/run-srfi-system-tests.lisp` and the
library corpus.

Some SRFIs are built on Common Lisp libraries, imported as `(cl
<package>)` libraries (docs/interop.md): 18 (bordeaux-threads), 106
(usocket), 115 (cl-ppcre), 170 (osicat, which needs a C compiler to
build) and 229 (closer-mop). The Lisp system is loaded, through
Quicklisp when it's present, the first time the SRFI is imported, so
nothing is loaded into an image that doesn't use them, and none of them
is a dependency of Pseudoscheme's ASDF systems.

Where possible, a library uses the SRFI's reference implementation,
unmodified, from `reference/`, and pulls it in with `include`. Each
`.sld` notes any change it needed at the top. Two conventions hold
throughout:

- `(srfi private shim)` supplies what the older, Scheme 48-flavoured
  reference code expects of its host: `check-arg`, `:optional`,
  `let-optionals*` (including the scsh forms), `receive`,
  `%char->latin1` and `%latin1->char`.
- When R7RS's own binding already meets the SRFI's specification, the
  library exports *that binding* with `(rename r7:x x)`. Examples are SRFI
  1's `map`, `member` and `make-list`, and SRFI 133's `vector-map`. That
  way the common `(import (scheme base) (srfi 1))` raises no conflict.
  Procedures whose contracts really differ keep their SRFI definitions, so
  a program has to `except` R7RS's versions:
  - SRFI 13's `string-map` and `string-for-each`;
  - SRFI 43's `vector-map`, `vector-for-each` and `vector-copy`;
  - SRFI 61's `cond`.

## Provenance and licences

| SRFI | Source | Licence |
|---|---|---|
| 0, 6, 9, 11, 16, 23, 34, 39, 87, 98, 244 | Re-export R7RS's bindings | — |
| 1 | Olin Shivers' reference implementation | MIT-style (Shivers) |
| 2, 8, 26, 28, 31, 61, 145 | Written for Pseudoscheme (26 follows its reference implementation) | Pseudoscheme's |
| 4 | Written for Pseudoscheme: CL specialized vectors (src/r6rs/numeric-vectors.lisp), with reader and writer syntax | Pseudoscheme's |
| 5 | Andy Gaynor's syntax-rules implementation (2003 revision), from the SRFI document | SRFI copyright notice |
| 7 | Richard Kelsey's process-program, from the SRFI document; program runs the result as an R7RS program | MIT |
| 10, 88, 105, 107, 108, 109, 169 | Written for Pseudoscheme: reader syntax (`src/read.scm`, `src/quasi.lisp`), with the libraries' procedures | Pseudoscheme's |
| 13 | Olin Shivers' reference implementation (minor changes, see `13.sld`) | MIT Scheme licence + scsh BSD |
| 14 | Written for Pseudoscheme: range-vector char-sets over all of Unicode, the standard sets by general category from cl-unicode; the interface after Olin Shivers' reference implementation | Pseudoscheme's |
| 17 | Lars Thomas Hansen's Twobit sample implementation and the document's getter-with-setter (adapted, see `17.sld`) | MIT (Per Bothner) |
| 18 | Written for Pseudoscheme, on bordeaux-threads | Pseudoscheme's; bordeaux-threads MIT |
| 19 | Will Fitzgerald's reference implementation (five changes, see `19.sld`); the local time zone from CL's `decode-universal-time` | MIT |
| 21 | Written for Pseudoscheme, on SRFI 18 | Pseudoscheme's |
| 22, 138, 176, 193 | Written for Pseudoscheme: the command-line program (`contrib/cli`), with libraries for 176 and 193 | Pseudoscheme's |
| 25 | Jussi Piitulainen's reference implementation | MIT |
| 27 | Sebastian Egner's reference implementation (integer MRG32k3a) | MIT |
| 29 | Scott G. Miller's reference implementation, from the SRFI document; locale from LC_ALL/LC_MESSAGES/LANG | MIT |
| 35, 36 | Written for Pseudoscheme on R6RS conditions: the types are R6RS record types, and SRFI 36's I/O types are R6RS's where they match (see `36.sld`) | Pseudoscheme's |
| 37 | Anthony Carrico's reference implementation | 3-clause BSD |
| 38 | Ray Dillinger's reference write-with-shared-structure; read-with-shared-structure is R7RS read | MIT |
| 41 | Philip L. Bewig's R6RS reference implementation, with the library names changed | MIT |
| 42 | Sebastian Egner's reference implementation | MIT |
| 43 | Taylor Campbell's reference implementation, corrected by Will Clinger | Public domain |
| 44 | Antero Mejr's R7RS implementation (contrib); SRFI 256 records on R6RS records (see `44.sld`) | MIT |
| 45 | `lazy` and `eager` defined on R7RS's `delay-force` and `make-promise` | — |
| 47 | Re-exports SRFI 63; prototypes written for Pseudoscheme | MIT (Jaffer); Pseudoscheme's |
| 48 | Kenneth A Dickey's reference implementation, revised by Hamayama | MIT |
| 49 | The reader of `sugar.scm`, the implementation in the SRFI document (Egil Möller), adapted (see `49.sld`) | MIT |
| 51, 54 | Joo ChurlSoo's implementations, from the SRFI documents | MIT |
| 55 | Written for Pseudoscheme (require-extension as import at the REPL) | Pseudoscheme's |
| 57 | André van Tonder's reference implementation; 7 macros replaced (see `57.sld`) | MIT |
| 58, 163 | Written for Pseudoscheme: array literals in the reader, made SRFI 164 arrays | Pseudoscheme's |
| 59 | Aubrey Jaffer's implementation from the SRFI document (SLIB), included with include-ci | MIT |
| 60 | Written for Pseudoscheme, on SRFI 151 | Pseudoscheme's |
| 63 | Aubrey Jaffer's SLIB array.scm, from the SRFI document; SLIB records on R6RS records | SLIB permissive notice + MIT |
| 64 | Taylan Kammer's implementation (after Per Bothner's) | MIT |
| 66 | Written for Pseudoscheme on bytevectors (SRFI 4's u8vectors) and R6RS bytevector=?/bytevector-copy! | Pseudoscheme's |
| 67 | Sebastian Egner and Jens Axel Søgaard's reference implementation | MIT |
| 69 | Written for Pseudoscheme on R6RS hashtables, with the interface of Panu Kalliokoski's reference implementation | Pseudoscheme's |
| 70 | Written for Pseudoscheme from the SRFI document: R7RS's arithmetic re-exported; exact-floor etc., rational gcd/lcm and expt of exact 0 added | Pseudoscheme's |
| 71 | Sebastian Egner's reference implementation (the generic part) | MIT |
| 72 | Written for Pseudoscheme, approximately, on psyntax's syntax-case | Pseudoscheme's |
| 74 | Written for Pseudoscheme on R6RS bytevectors | Pseudoscheme's |
| 78 | Sebastian Egner's reference implementation | MIT |
| 89 | Marc Feeley's implementation from the SRFI document, its expander ported from define-macro to syntax-case | MIT |
| 90 | Written for Pseudoscheme on SRFI 69, after Marc Feeley's implementation in the SRFI document | MIT; Pseudoscheme's |
| 94 | Aubrey Jaffer's implementation from the SRFI document, copied into `94.sld` with the self-referential redefinitions renamed | MIT |
| 95 | SLIB's `sort.scm` (Richard O'Keefe, Aubrey Jaffer), with one fix (see `95.sld`) | Public domain |
| 96 | Written for Pseudoscheme, after the specification | Pseudoscheme's |
| 99 | William D Clinger's reference implementation, on R6RS records (2 changes, see `99/records/syntactic.sld`) | MIT |
| 100 | Joo ChurlSoo's implementation (define-syntax part; object registration added) | MIT |
| 101 | David Van Horn's R6RS reference implementation, with the library name changed | MIT |
| 106 | Written for Pseudoscheme, on usocket | Pseudoscheme's; usocket MIT |
| 110 | David A. Wheeler and Alan Manuel K. Gloria's `kernel.scm` (two changes, see `110.sld`) | MIT |
| 111 | Re-exports SRFI 195's boxes | — |
| 112 | Written for Pseudoscheme, on CL's software-type, machine-type, ... | Pseudoscheme's |
| 113 | John Cowan's reference implementation, on SRFIs 69 and 128 | MIT |
| 115 | Written for Pseudoscheme, on cl-ppcre; the match-iteration procedures follow Alex Shinn's chibi-scheme implementation | Pseudoscheme's; cl-ppcre BSD; chibi BSD |
| 116 | John Cowan's reference implementation (two changes, see `116.sld`) | MIT |
| 117 | John Cowan's reference implementation, with R7RS shims from chibi-scheme | MIT; 3-clause BSD |
| 118 | Written for Pseudoscheme: an approximation by macros (strings are fixed-size; see `118.sld`) | Pseudoscheme's |
| 119 | Arne Babenhauserheide's `wisp-scheme.scm`, with Guile's procedures supplied (see `119.sld`) | MIT |
| 120 | Takashi Kato's sample implementation (four changes, see `120.sld`) | 2-clause BSD |
| 121 | Re-exports SRFI 158, which extends it | — |
| 123 | Taylan Kammer's reference implementation, on (srfi 17) and R6RS records (field-name wrappers, see `123.sld`) | MIT |
| 125 | William D Clinger's reference implementation, on `(rnrs hashtables)` in place of SRFI 126 | Clinger (permissive) |
| 126 | Taylan Kammer's reference implementation, with weak tables on trivial-garbage (see `126.sld`) | MIT |
| 127 | John Cowan's reference implementation | MIT |
| 128 | John Cowan's reference implementation | MIT |
| 129 | John Cowan's sample implementation (string-titlecase); char-title-case? and char-titlecase are R6RS's | MIT |
| 130 | William D Clinger's implementation, on SRFI 13 | MIT |
| 131 | Marc Nieper-Wißkirchen's implementation on SRFI 136 | MIT |
| 132 | Olin Shivers / John Cowan / Will Clinger | MIT |
| 133 | Taylor Campbell / Will Clinger / John Cowan | Public domain or MIT |
| 134 | Shiro Kawai's implementation, stream version by Wolfgang Corcoran-Mathe | MIT |
| 135 | William D Clinger's sample implementation (kernel0: texts are strings) | MIT |
| 136 | Marc Nieper-Wißkirchen's sample implementation, on SRFI 137 | MIT |
| 137 | John Cowan and Marc Nieper-Wißkirchen's implementation | MIT |
| 139, 149, 212, 213, 251 | Written for Pseudoscheme, in psyntax's expander (`vendor/psyntax`) | Pseudoscheme's; psyntax's |
| 140 | Written for Pseudoscheme: William D Clinger's SRFI 135 sample implementation on a string kernel (see `140.sld`); test suite by Per Bothner and Clinger | MIT; Pseudoscheme's |
| 141 | Taylor R. Campbell's reference implementation | 2-clause BSD |
| 143 | John Cowan's reference implementation (two changes, see `143.sld`) | MIT |
| 144 | William D Clinger's sample implementation, on `(rnrs arithmetic flonums)`; the exponent, sign and adjacency procedures on CL's `decode-float` and float-features (see `144.sld`) | MIT |
| 146 | Marc Nieper-Wißkirchen's mappings (red-black trees); Arthur A. Gleckler's hashmaps (HAMTs) | MIT |
| 147 | Marc Nieper-Wißkirchen's sample implementation, adapted to psyntax (see `private/srfi-147-implementation.sld`) | MIT |
| 148 | Marc Nieper-Wißkirchen's sample implementation, split into three libraries | MIT |
| 150 | Written for Pseudoscheme (syntax-case, on R6RS records), after Marc Nieper-Wißkirchen's sample implementation | Pseudoscheme's |
| 151 | John Cowan's reference implementation; bitwise-33 by Olin Shivers, bitwise-60 by Aubrey Jaffer | MIT; public domain; SLIB licence |
| 152 | John Cowan's sample implementation, after Olin Shivers' SRFI 13 | MIT; MIT Scheme licence + scsh BSD |
| 153 | John Cowan's sample implementation, on SRFIs 128 and 146 | MIT |
| 156 | Panicz Maciej Godek's implementation, from the SRFI document | MIT |
| 158 | Shiro Kawai / John Cowan / Thomas Gilray's sample implementation | MIT |
| 160 | John Cowan's reference implementation, expanded per type by its `atexpander.sh`; `(srfi 160 base)` on SRFI 4's host procedures | MIT |
| 161 | Marc Nieper-Wißkirchen's sample implementation | MIT |
| 162 | John Cowan's sample implementation, on (srfi 128), which it re-exports | MIT |
| 164 | Written for Pseudoscheme from the SRFI document (Kawa's implementation is Java) | Pseudoscheme's |
| 165 | Marc Nieper-Wißkirchen's sample implementation | MIT |
| 166 | Alex Shinn's chibi-scheme implementation, on SRFI 165 | 3-clause BSD |
| 167 | Amirouche Boubekki's sample implementation (in-memory engine) | MIT |
| 168 | Amirouche Boubekki's sample implementation (one misplaced parenthesis fixed) | MIT |
| 170 | Written for Pseudoscheme, on osicat | Pseudoscheme's; osicat MIT |
| 171 | Linus Björnstam's reference implementation | MIT |
| 172 | John Cowan's sample libraries, renamed | MIT |
| 173 | Amirouche Boubekki's sample implementation | MIT |
| 174 | John Cowan's sample implementation | MIT |
| 175 | Lassi Kortela's reference implementation | MIT |
| 178 | Wolfgang Corcoran-Mathe's sample implementation | MIT |
| 179 | Alex Shinn's chibi-scheme implementation (two fixes, see `179.sld`) | 3-clause BSD |
| 180 | Amirouche Boubekki's sample implementation; its chibi helpers rewritten on SRFI 115 | MIT (test files MIT; jsonl samples BSD) |
| 181, 192 | Written for Pseudoscheme on its R6RS ports; tests from Shiro Kawai's sample | Pseudoscheme's; tests MIT |
| 185 | The portable implementation in the SRFI document (Per Bothner, John Cowan) | MIT |
| 188 | Re-exports R6RS's let-syntax and letrec-syntax | — |
| 189 | Wolfgang Corcoran-Mathe's reference implementation | MIT |
| 190 | Written for Pseudoscheme after the SRFI document's sample, with a parameter in place of a syntax parameter | Pseudoscheme's (sample MIT) |
| 194 | Sample implementation by Arvydas Silanskas, Bradley Lucier, Linas Vepštas and John Cowan | MIT |
| 195 | Marc Nieper-Wißkirchen's sample implementation | MIT |
| 196 | John Cowan / Wolfgang Corcoran-Mathe | MIT |
| 197 | Adam R. Nelson's `syntax-rules` implementation | MIT |
| 201 | Panicz Maciej Godek's Guile sample implementation (adapted), on Alex Shinn's match | MIT; match public domain |
| 202 | Panicz Maciej Godek's Guile sample implementation, on Alex Shinn's match | MIT |
| 203 | Vasilij Schneidermann's SVG implementation (the SRFI repository's contrib), with changes; Rogers image CC0 | BSD-3-Clause, CC0 |
| 206 | Written for Pseudoscheme (partial); (srfi 206 all) after the SRFI's "poor man's" implementation | MIT |
| 207 | Wolfgang Corcoran-Mathe's sample implementation | MIT |
| 208, 254, 258, 260 | Written for Pseudoscheme (258 and 260 after their sample implementations) | Pseudoscheme's |
| 209 | Wolfgang Corcoran-Mathe's sample implementation | MIT |
| 210 | Marc Nieper-Wißkirchen's reference implementation | MIT |
| 211 | Written for Pseudoscheme, on psyntax's syntax-case | Pseudoscheme's |
| 214 | Adam Nelson's sample implementation | MIT |
| 215 | Göran Weinholt's sample implementation | MIT |
| 216 | Vladimir Nikishkin's sample implementation (runtime fixed) | MIT |
| 217 | Wolfgang Corcoran-Mathe's sample implementation | MIT |
| 219 | Lassi Kortela's implementation | MIT |
| 221 | Arvydas Silanskas's sample implementation | MIT |
| 222 | John Cowan and Arvydas Silanskas's sample implementation | MIT |
| 223 | Daphne Preston-Kendal's implementation (the algorithm after Python's `bisect`) | MIT |
| 224 | Wolfgang Corcoran-Mathe's sample implementation | MIT |
| 225 | Arvydas Silanskas's sample implementation (one change, see `225.sld`) | MIT |
| 226 | Marc Nieper-Wißkirchen's sample implementation, its Chez Scheme parts replaced (see `226.sld`) | MIT |
| 227 | Daphne Preston-Kendal's R7RS implementation | MIT |
| 228 | Daphne Preston-Kendal's implementation | MIT |
| 229 | Written for Pseudoscheme, on closer-mop funcallable instances | Pseudoscheme's; closer-mop MIT |
| 230 | Marc Nieper-Wißkirchen's sample implementation, on SRFI 18 | MIT |
| 231 | Alex Shinn's portable implementation from chibi-scheme (the SRFI's own is Gambit-only) | 3-clause BSD |
| 232 | Wolfgang Corcoran-Mathe's sample implementation (its import form removed) | MIT |
| 233 | Arvydas Silanskas's sample implementation | MIT |
| 234 | Sample implementation by Shiro Kawai, John Cowan and Arne Babenhauserheide | MIT |
| 235 | John Cowan and Arvydas Silanskas's implementation | MIT |
| 236 | Marc Nieper-Wißkirchen's implementation, from the SRFI document | MIT |
| 237 | Marc Nieper-Wißkirchen's sample implementation, on R6RS records (changes in `237/records.sld`) | MIT |
| 238 | Written for Pseudoscheme after Lassi Kortela's sample implementation | MIT |
| 239 | Marc Nieper-Wißkirchen's syntax-case sample implementation | MIT |
| 240 | Marc Nieper-Wißkirchen's sample implementation, on SRFI 237 | MIT |
| 241 | Marc Nieper-Wißkirchen's R6RS sample implementation, converted to define-library | MIT |
| 242 | Marc Nieper-Wißkirchen's reference implementation, converted to R7RS libraries, with an SRFI 213 emulation | MIT |
| 247 | Marc Nieper-Wißkirchen's R6RS sample implementation, converted to define-library (one change, see `247.sld`) | MIT |
| 248 | Written for Pseudoscheme; `guard` from Marc Nieper-Wißkirchen's sample implementation | Pseudoscheme's; MIT |
| 250 | Daphne Preston-Kendal's sample implementation | MIT |
| 252 | Antero Mejr's sample implementation (library renamed) | MIT |
| 253 | Artyom Bologov's sample implementation (impl.scm, unmodified) | MIT |
| 255 | Wolfgang Corcoran-Mathe and Marc Nieper-Wißkirchen's R6RS sample implementation, converted to define-library | MIT |
| 257 | Sergei Egorov's R7RS sample libraries (main, misc, box; not rx) | MIT |
| 259 | Daphne Preston-Kendal's sample implementation, on SRFI 229 | MIT |
| 261 | The library system's names (`src/r7rs/front.lisp`) | Pseudoscheme's |
| 263 | Daniel Ziltener's reference implementation, with fixes | MIT |
| 264 | Sergei Egorov's sample implementation, on SRFI 115 | MIT |
| 270 | Peter McGoron's `write-hexadecimal-float`; hexadecimal floats in `string->number` | MIT; Pseudoscheme's |
| 271 | Wolfgang Corcoran-Mathe's sample implementation; xoshiro256++ moved from Gauche classes to R6RS custom ports | MIT |
| 274 | Peter McGoron's sample implementation | MIT |

Where an upstream licence file was a blank MIT template, the
`reference/srfi-N/LICENSE` here names the copyright holder from the SRFI
document or the file headers.

The MIT Scheme licence behind SRFIs 13 and 14 is permissive and not
copyleft. It does ask that improvements be sent back to MIT and that
MIT's name stay out of advertising. Every copied file keeps its
copyright notice.

## Limitations

Each library's header comment says what it doesn't do; the main points:

- Some libraries replace standard names, as their SRFIs intend, so a
  program imports them with `except` or `prefix`: SRFI 71's `let`,
  `let*` and `letrec`; SRFI 219's `define`; SRFI 101's list procedures;
  SRFI 152's string procedures (which also clash with SRFIs 13 and 130);
  SRFI 5's `let`; SRFI 17's and 123's `set!`; SRFI 201's `lambda`,
  `define`, `let`, `let*` and `or`; the `define-record-type` of SRFIs 57,
  99, 131, 136, 150, 237 and 240; SRFI 70's `gcd`, `lcm` and `expt`;
  SRFI 94's `quotient`, `remainder`, `modulo`, `abs`, `make-rectangular`
  and `make-polar`; SRFI 241's `quasiquote`; SRFI 147's `define-syntax`
  and the other macro forms; SRFI 248's `guard`; SRFI 226's `call/cc`,
  `dynamic-wind`, `parameterize`, `raise` and the rest; SRFI 140's
  `make-string`, `string-copy`, `list->string` and `string-map`; SRFI
  44's collection names. Others clash with each other: `(rnrs)` and SRFI
  60 on `bitwise-if` and `bit-count`; SRFIs 125 and 126 on `string-hash`
  and `string-ci-hash`; SRFIs 25, 47, 63, 164, 179 and 231 on
  `make-array`, `array-ref` and the like; SRFI 209 with `(rnrs enums)`;
  SRFI 250 with SRFIs 69 and 125; SRFI 129's `string-titlecase` with
  `(rnrs unicode)`'s; SRFI 202's `and-let*` with SRFI 2's.
- SRFI 18's threads share `parameterize`'s bindings (ROADMAP, 4), and
  must not expand code while another thread does. SRFI 21's priorities
  and quanta are recorded, not scheduled. SRFI 226's threads are its
  own, switched only when they yield, block or end.
- With `--continuations=escape`, SRFI 158's `make-coroutine-generator`
  (and `make-for-each-generator`) runs its producer to completion on the
  first call and buffers what it yields, so an infinite coroutine hangs;
  SRFI 225's default `dict->generator` likewise collects its entries up
  front. SRFI 248's prompts are global, not per thread, and an ordinary
  continuation captured inside one and called during a reinstatement of
  it returns as the reinstatement does.
- SRFI 115 matches leftmost-first by backtracking (cl-ppcre), not
  leftmost-longest; look-behind must have a fixed length.
- SRFI 197's `_ ...` placeholder doesn't work (ROADMAP, 1).
- SRFI 35's names clash with `(rnrs conditions)`'s (`condition`,
  `define-condition-type`, the `&` types), and SRFI 36's
  `i/o-read-error?` and `i/o-write-error?` with `(rnrs io ports)`'s.
  Opening a missing file raises only `&i/o-filename-error`, not a more
  specific type.
- SRFI 231's `array-freeze!` can't make storage already shared by
  another array immutable.
- Reader syntax: SRFI 88's `foo:` keywords need `#!srfi-88` first (a name
  ending in a colon is otherwise a symbol, as R7RS has it); SRFI 10's
  `#,(tag ...)` constructors apply to what is read after they are
  defined (a program is read whole before it runs), and without one
  `#,` is R6RS's unsyntax; SRFIs 49, 110 and 119 read a file after
  `#!srfi-49`, `#!sweet` or `#!wisp`; SRFI 58's and 163's array literals
  are SRFI 164 arrays, their element types not kept; SRFI 109's format
  specifiers aren't supported, and SRFI 107's namespaces aren't resolved.
- SRFI 72 is approximate: there is one environment for all phases, so
  `begin-for-syntax` is `begin`. SRFI 213's properties aren't kept with a
  compiled library. SRFI 139's `syntax-parameterize` accepts any keyword.
- SRFI 118's `string-append!` and `string-replace!` are macros that
  assign their variable (strings are fixed-size); SRFI 140's istrings
  aren't enforced immutable.
- SRFI 55 works at the REPL only; SRFI 96's `slib:load-compiled` is an
  error; SRFI 203 draws SVG; SRFI 206's `define-auxiliary-syntax` makes a
  new keyword each time; SRFI 211 lacks `with-ellipsis`; SRFI 257 lacks
  `(srfi 257 rx)`; SRFI 167 is in-memory only.
