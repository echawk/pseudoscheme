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
- The R7RS-large names that have an implementation here: `(scheme list)`,
  `(scheme hash-table)`, `(scheme charset)`, `(scheme vector)`,
  `(scheme sort)`, `(scheme comparator)`, `(scheme generator)`,
  `(scheme stream)`, `(scheme box)`, `(scheme bitwise)`, `(scheme fixnum)`
  and `(scheme division)`.

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
| 2, 8, 26, 28, 31, 61, 111, 145 | Written for Pseudoscheme (26 follows its reference implementation) | Pseudoscheme's |
| 6, 9, 11, 16, 23, 39, 87, 98 | Re-export R7RS's bindings | — |
| 45 | `lazy` and `eager` defined on R7RS's `delay-force` and `make-promise` | — |
| 1 | Olin Shivers' reference implementation | MIT-style (Shivers) |
| 13 | Olin Shivers' reference implementation (minor changes, see `13.sld`) | MIT Scheme licence + scsh BSD |
| 14 | Olin Shivers / Brian D. Carlstrom's reference implementation (one change). Char-sets cover Latin-1 only. | MIT Scheme licence + scsh BSD |
| 19 | Will Fitzgerald's reference implementation (four changes, see `19.sld`). The local time zone is assumed to be UTC. | MIT |
| 27 | Sebastian Egner's reference implementation (integer MRG32k3a) | MIT |
| 41 | Philip L. Bewig's R6RS reference implementation, with the library names changed | MIT |
| 43 | Taylor Campbell's reference implementation, corrected by Will Clinger | Public domain |
| 64 | Taylan Kammer's implementation (after Per Bothner's) | MIT |
| 69 | Written for Pseudoscheme on R6RS hashtables, with the interface of Panu Kalliokoski's reference implementation | Pseudoscheme's |
| 125 | William D Clinger's reference implementation, on `(rnrs hashtables)` in place of SRFI 126 | Clinger (permissive) |
| 128 | John Cowan's reference implementation | MIT |
| 130 | William D Clinger's implementation, on SRFI 13 | MIT |
| 132 | Olin Shivers / John Cowan / Will Clinger | MIT |
| 133 | Taylor Campbell / Will Clinger / John Cowan | Public domain or MIT |
| 141 | Taylor R. Campbell's reference implementation | 2-clause BSD |
| 143 | John Cowan's reference implementation (two changes, see `143.sld`) | MIT |
| 151 | John Cowan's reference implementation; bitwise-33 by Olin Shivers, bitwise-60 by Aubrey Jaffer | MIT; public domain; SLIB licence |
| 158 | Shiro Kawai / John Cowan / Thomas Gilray's sample implementation | MIT |

The MIT Scheme licence behind SRFIs 13 and 14 is permissive and not
copyleft. It does ask that improvements be sent back to MIT and that
MIT's name stay out of advertising. Every copied file keeps its
copyright notice.

## Limitations

- Continuations are escape-only, so the `make-coroutine-generator` in
  SRFI 158 (and `make-for-each-generator` on top of it) runs its producer
  to completion on the first call and buffers what it yields. An infinite
  coroutine hangs.
- SRFI 14's char-sets are limited to Latin-1.
