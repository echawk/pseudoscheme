# SRFIs

The SRFI libraries that ship with Pseudoscheme. Each `N.sld` is an R7RS
`define-library` for `(srfi N)`. psyntax finds them with no setup:
`psx:*system-library-path*` roots every name that starts with `srfi` here.
That root is searched after the user's `psx:*library-path*`, so a project
can still supply its own SRFIs, for instance Akku's chez-srfi.

Other names resolve to these libraries as well (`src/r7rs/front.lisp`,
`srfi-alias-form`):

- The R6RS spelling of SRFI 97, `(srfi :1)` and `(srfi :1 lists)`. A name
  with a trailing identifier gets a synthesized library that re-exports
  everything in `(srfi :1)`.
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
0), as do the SRFIs built into the reader and expander: 30, 46, 62 and
97 (`psl:*scheme-features*`, src/environments.lisp).

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
| 0, 6, 9, 11, 16, 23, 34, 39, 87, 98 | Re-export R7RS's bindings | — |
| 1 | Olin Shivers' reference implementation | MIT-style (Shivers) |
| 2, 8, 26, 28, 31, 61, 145 | Written for Pseudoscheme (26 follows its reference implementation) | Pseudoscheme's |
| 4 | Written for Pseudoscheme: CL specialized vectors (src/r6rs/numeric-vectors.lisp), with reader and writer syntax | Pseudoscheme's |
| 13 | Olin Shivers' reference implementation (minor changes, see `13.sld`) | MIT Scheme licence + scsh BSD |
| 14 | Written for Pseudoscheme: range-vector char-sets over all of Unicode, the standard sets by general category from cl-unicode; the interface after Olin Shivers' reference implementation | Pseudoscheme's |
| 18 | Written for Pseudoscheme, on bordeaux-threads | Pseudoscheme's; bordeaux-threads MIT |
| 19 | Will Fitzgerald's reference implementation (five changes, see `19.sld`); the local time zone from CL's `decode-universal-time` | MIT |
| 27 | Sebastian Egner's reference implementation (integer MRG32k3a) | MIT |
| 35, 36 | Written for Pseudoscheme on R6RS conditions: the types are R6RS record types, and SRFI 36's I/O types are R6RS's where they match (see `36.sld`) | Pseudoscheme's |
| 37 | Anthony Carrico's reference implementation | 3-clause BSD |
| 41 | Philip L. Bewig's R6RS reference implementation, with the library names changed | MIT |
| 42 | Sebastian Egner's reference implementation | MIT |
| 43 | Taylor Campbell's reference implementation, corrected by Will Clinger | Public domain |
| 45 | `lazy` and `eager` defined on R7RS's `delay-force` and `make-promise` | — |
| 48 | Kenneth A Dickey's reference implementation, revised by Hamayama | MIT |
| 51, 54 | Joo ChurlSoo's implementations, from the SRFI documents | MIT |
| 60 | Written for Pseudoscheme, on SRFI 151 | Pseudoscheme's |
| 64 | Taylan Kammer's implementation (after Per Bothner's) | MIT |
| 69 | Written for Pseudoscheme on R6RS hashtables, with the interface of Panu Kalliokoski's reference implementation | Pseudoscheme's |
| 71 | Sebastian Egner's reference implementation (the generic part) | MIT |
| 78 | Sebastian Egner's reference implementation | MIT |
| 95 | SLIB's `sort.scm` (Richard O'Keefe, Aubrey Jaffer), with one fix (see `95.sld`) | Public domain |
| 101 | David Van Horn's R6RS reference implementation, with the library name changed | MIT |
| 106 | Written for Pseudoscheme, on usocket | Pseudoscheme's; usocket MIT |
| 111 | Re-exports SRFI 195's boxes | — |
| 113 | John Cowan's reference implementation, on SRFIs 69 and 128 | MIT |
| 115 | Written for Pseudoscheme, on cl-ppcre; the match-iteration procedures follow Alex Shinn's chibi-scheme implementation | Pseudoscheme's; cl-ppcre BSD; chibi BSD |
| 117 | John Cowan's reference implementation, with R7RS shims from chibi-scheme | MIT; 3-clause BSD |
| 121 | Re-exports SRFI 158, which extends it | — |
| 125 | William D Clinger's reference implementation, on `(rnrs hashtables)` in place of SRFI 126 | Clinger (permissive) |
| 126 | Taylan Kammer's reference implementation, with weak tables on trivial-garbage (see `126.sld`) | MIT |
| 127 | John Cowan's reference implementation | MIT |
| 128 | John Cowan's reference implementation | MIT |
| 130 | William D Clinger's implementation, on SRFI 13 | MIT |
| 132 | Olin Shivers / John Cowan / Will Clinger | MIT |
| 133 | Taylor Campbell / Will Clinger / John Cowan | Public domain or MIT |
| 134 | Shiro Kawai's implementation, stream version by Wolfgang Corcoran-Mathe | MIT |
| 135 | William D Clinger's sample implementation (kernel0: texts are strings) | MIT |
| 141 | Taylor R. Campbell's reference implementation | 2-clause BSD |
| 143 | John Cowan's reference implementation (two changes, see `143.sld`) | MIT |
| 144 | William D Clinger's sample implementation, on `(rnrs arithmetic flonums)`; the exponent, sign and adjacency procedures on CL's `decode-float` and float-features (see `144.sld`) | MIT |
| 146 | Marc Nieper-Wißkirchen's mappings (red-black trees); Arthur A. Gleckler's hashmaps (HAMTs) | MIT |
| 151 | John Cowan's reference implementation; bitwise-33 by Olin Shivers, bitwise-60 by Aubrey Jaffer | MIT; public domain; SLIB licence |
| 152 | John Cowan's sample implementation, after Olin Shivers' SRFI 13 | MIT; MIT Scheme licence + scsh BSD |
| 156 | Panicz Maciej Godek's implementation, from the SRFI document | MIT |
| 158 | Shiro Kawai / John Cowan / Thomas Gilray's sample implementation | MIT |
| 160 | John Cowan's reference implementation, expanded per type by its `atexpander.sh`; `(srfi 160 base)` on SRFI 4's host procedures | MIT |
| 170 | Written for Pseudoscheme, on osicat | Pseudoscheme's; osicat MIT |
| 175 | Lassi Kortela's reference implementation | MIT |
| 189 | Wolfgang Corcoran-Mathe's reference implementation | MIT |
| 195 | Marc Nieper-Wißkirchen's sample implementation | MIT |
| 196 | John Cowan / Wolfgang Corcoran-Mathe | MIT |
| 197 | Adam R. Nelson's `syntax-rules` implementation | MIT |
| 210 | Marc Nieper-Wißkirchen's reference implementation | MIT |
| 219 | Lassi Kortela's implementation | MIT |
| 223 | Daphne Preston-Kendal's implementation (the algorithm after Python's `bisect`) | MIT |
| 225 | Arvydas Silanskas's sample implementation (one change, see `225.sld`) | MIT |
| 227 | Daphne Preston-Kendal's R7RS implementation | MIT |
| 228 | Daphne Preston-Kendal's implementation | MIT |
| 229 | Written for Pseudoscheme, on closer-mop funcallable instances | Pseudoscheme's; closer-mop MIT |
| 231 | Alex Shinn's portable implementation from chibi-scheme (the SRFI's own is Gambit-only) | 3-clause BSD |
| 235 | John Cowan and Arvydas Silanskas's implementation | MIT |
| 236 | Marc Nieper-Wißkirchen's implementation, from the SRFI document | MIT |

Where an upstream licence file was a blank MIT template, the
`reference/srfi-N/LICENSE` here names the copyright holder from the SRFI
document or the file headers.

The MIT Scheme licence behind SRFIs 13 and 14 is permissive and not
copyleft. It does ask that improvements be sent back to MIT and that
MIT's name stay out of advertising. Every copied file keeps its
copyright notice.

## Limitations

- Continuations are escape-only, so the `make-coroutine-generator` in
  SRFI 158 (and `make-for-each-generator` on top of it) runs its producer
  to completion on the first call and buffers what it yields. An infinite
  coroutine hangs.
- Some libraries replace standard names, as their SRFIs intend, so a
  program imports them with `except` or `prefix`: SRFI 71's `let`,
  `let*` and `letrec`; SRFI 219's `define`; SRFI 101's list procedures;
  SRFI 152's string procedures (which also clash with SRFIs 13 and 130).
  `(rnrs)` and SRFI 60 clash on `bitwise-if` and `bit-count`; SRFIs 125
  and 126 on `string-hash` and `string-ci-hash`.
- SRFI 18's threads share `parameterize`'s bindings (ROADMAP, 4), and
  must not expand code while another thread does.
- SRFI 115 matches leftmost-first by backtracking (cl-ppcre), not
  leftmost-longest; look-behind must have a fixed length.
- SRFI 197's `_ ...` placeholder doesn't work, because `_` isn't a
  `syntax-rules` wildcard yet (ROADMAP, 1).
- SRFI 35's names clash with `(rnrs conditions)`'s (`condition`, `define-condition-type`, the `&` types), and SRFI 36's `i/o-read-error?` and `i/o-write-error?` with `(rnrs io ports)`'s. Opening a missing file raises only `&i/o-filename-error`, not a more specific type.
- SRFI 225's default `dict->generator` collects the entries up front
  rather than lazily (it was a coroutine).
- SRFI 231's `array-freeze!` can't make storage already shared by
  another array immutable.
