# Results

October 2026, on an Apple M4 MacBook Air (4 performance and 6
efficiency cores) running macOS. These are single runs of
`r7rs-benchmarks` with its standard inputs. Times are in seconds, and
the figure in parentheses is the ratio to Chez Scheme. Lower is better.

| implementation | version | geometric mean vs Chez | benchmarks completed |
|---|---|---|---|
| Chez Scheme (with akku-r7rs; `--optimize-level 2`) | 10.4.1 | 1.0× | 57/57 |
| **Pseudoscheme** (SBCL 2.6.9) | 3.0 | **1.44×** | **57/57** |
| Pseudoscheme, `--continuations=full` | 3.0 | 1.55× | 57/57 |
| Guile | 3.0.11 | 2.8× | 56/57 |
| Gauche | 0.9.15 | 9.2× | 57/57 |
| Chibi (a bytecode interpreter) | 0.12.0 | 35.7× | 49/57 |

Pseudoscheme is at or ahead of Chez on 14 benchmarks (`tail`,
`chudnovsky`, `pi`, `earley`, `fft`, `mbrot`, `mperm`, `ntakl`,
`parsing`, `cat`, ...), within 1.5× on 32 and within 2× on 41. Its worst ratios are
`ack` (3.9×), `fibc` (3.7×), `divrec` (3.6×) and `browse` (3.5×), which
come down to SBCL's own call and allocation costs. Full continuations
(re-entrant, from generalized stack inspection: docs/continuations.md)
cost 7.7% over escape-only as a geometric mean; most benchmarks run at
the same speed, and the cost is in programs that call unknown
procedures in tight loops (`lattice`, `graphs`, `quicksort`, `conform`,
`matrix`: 1.6–1.8×).

**Caveats.**
- Chez, Guile, Gauche and Chibi are from an earlier run, in which the
  implementations ran three at a time (Pseudoscheme, Chez and Guile;
  then Chibi and Gauche), so that each could have a performance core.
  Pseudoscheme's figures here are from a later run of its own.
- The times cover each benchmark's own loop. Pseudoscheme's expansion
  and SBCL compilation are excluded, as are other implementations'
  compile steps.
- Racket wasn't run: it needs its `r7rs` package, which isn't installed
  here.
- Chibi's failures are its own crashes and timeouts under the 300 s
  limit.
- ecraven's published results (<https://ecraven.github.io/r7rs-benchmarks/>)
  cover more implementations on different hardware.

Reproduce with `bench/run.sh pseudoscheme chez-akku guile gauche-local chibi`,
then `python3 bench/summarize.py`.

## What made the difference

From 3.0× Chez's time to 1.44× (earlier run on the same machine, then
this one), in order of effect:

- **Definitions as `letrec`** (src/psyntax.lisp, `definitions-as-letrec`).
  psyntax expands a body's definitions as `letrec*`: variables bound to
  `#f`, then assigned. Assigned and closed over, each was boxed by SBCL,
  and every call to a program's procedure was a `funcall` through the
  box. A variable assigned once to a lambda is now bound by a `letrec`,
  which the translator makes a `labels` function, called directly; one
  that aliases another such procedure or a primitive is replaced by it.
  Big programs, whose procedures would make one code object too big for
  SBCL, get top-level definitions instead (`hoist-definitions`).
- **Inline boolean tests.** core.lisp declared `truep` and `true?` inline
  with `proclaim`, which works only at load time, so every Scheme `if`
  whose test wasn't already a Lisp boolean was a call.
- **`(void)` as a constant**: psyntax's unspecified value, from every
  one-armed `if`, `when`, `unless` and `cond` without `else`, was a call.
- **Inline fixnum and flonum arithmetic** for two-argument `+`, `-` and
  `*` (src/numbers.lisp), like the comparisons'.
- **Primitives open-coded again**: the R6RS layer had replaced `memq`
  and `memv` with closure-calling versions, and R7RS's character
  comparisons with an n-ary wrapper around the built-in n-ary ones;
  `case` on symbols uses `memq`; `map` is no longer kept from being
  open-coded.
- **R7RS records** define procedures of the program's own instead of
  closures from the procedural record API.
- **`equal?`**: a typed first pass, and union-find for big or cyclic
  data.

Biggest gains: `lattice` 23.2 → 2.9 s, `parsing` 11.2 → 1.4 s, `takl`
11.1 → 2.5 s, `wc` 7.5 → 1.1 s, `sum` 8.9 → 2.0 s, `gcbench` 4.7 →
1.0 s, `conform` 9.0 → 2.5 s.

## All benchmarks

Times in seconds; in parentheses, the ratio to Chez. "pseudoscheme" is
escape-only continuations (the default), "full" the same build with
`--continuations=full`, both from the run of the summary above; the other
columns are from the earlier run.

| benchmark | chez | pseudoscheme | full | chibi | gauche | guile |
|---|---|---|---|---|---|---|
| browse | 0.65 (1.0x) | 2.23 (3.4x) | 2.23 (3.4x) | crashed | 10.87 (16.9x) | 2.27 (3.5x) |
| deriv | 0.51 (1.0x) | 1.05 (2.1x) | 1.05 (2.1x) | 39.76 (78.5x) | 13.83 (27.3x) | 3.41 (6.7x) |
| destruc | 0.91 (1.0x) | 1.68 (1.9x) | 1.67 (1.8x) | 35.32 (38.9x) | 14.27 (15.7x) | 1.61 (1.8x) |
| diviter | 0.93 (1.0x) | 1.17 (1.3x) | 1.19 (1.3x) | 21.53 (23.3x) | 8.64 (9.3x) | 3.24 (3.5x) |
| divrec | 1.02 (1.0x) | 3.71 (3.6x) | 3.66 (3.6x) | 21.05 (20.5x) | 10.54 (10.3x) | 5.24 (5.1x) |
| puzzle | 1.40 (1.0x) | 2.78 (2.0x) | 2.80 (2.0x) | 101.89 (72.9x) | 17.21 (12.3x) | 4.02 (2.9x) |
| triangl | 1.27 (1.0x) | 1.63 (1.3x) | 1.63 (1.3x) | 35.96 (28.3x) | 12.23 (9.6x) | 3.37 (2.7x) |
| tak | 0.74 (1.0x) | 1.42 (1.9x) | 1.42 (1.9x) | 25.37 (34.3x) | 11.12 (15.0x) | 2.11 (2.9x) |
| takl | 1.60 (1.0x) | 2.46 (1.5x) | 2.44 (1.5x) | 202.89 (126.5x) | 42.16 (26.3x) | 2.83 (1.8x) |
| ntakl | 1.54 (1.0x) | 1.29 (0.8x) | 1.25 (0.8x) | 39.87 (25.9x) | 35.92 (23.3x) | 2.94 (1.9x) |
| cpstak | 1.29 (1.0x) | 3.00 (2.3x) | 2.95 (2.3x) | 90.09 (69.6x) | 42.30 (32.7x) | 7.77 (6.0x) |
| ctak | 0.39 (1.0x) | 0.85 (2.2x) | 1.15 (2.9x) | 234.71 (603.2x) | 8.49 (21.8x) | 39.82 (102.3x) |
| fib | 1.75 (1.0x) | 2.98 (1.7x) | 2.85 (1.6x) | 27.82 (15.9x) | 25.10 (14.3x) | 5.09 (2.9x) |
| fibc | 0.26 (1.0x) | 0.98 (3.8x) | 1.18 (4.6x) | 49.01 (186.9x) | 8.00 (30.5x) | 25.49 (97.2x) |
| fibfp | 1.25 (1.0x) | 1.26 (1.0x) | 1.23 (1.0x) | 16.34 (13.0x) | 5.53 (4.4x) | 6.70 (5.3x) |
| sum | 1.78 (1.0x) | 2.01 (1.1x) | 2.00 (1.1x) | 33.52 (18.8x) | 20.19 (11.3x) | 2.46 (1.4x) |
| sumfp | 1.92 (1.0x) | 2.61 (1.4x) | 2.62 (1.4x) | 30.58 (16.0x) | 6.47 (3.4x) | 11.68 (6.1x) |
| fft | 1.41 (1.0x) | 1.01 (0.7x) | 1.03 (0.7x) | 23.29 (16.5x) | 4.94 (3.5x) | 2.87 (2.0x) |
| mbrot | 4.91 (1.0x) | 3.48 (0.7x) | 3.46 (0.7x) | 76.44 (15.6x) | 10.51 (2.1x) | 12.14 (2.5x) |
| mbrotZ | 2.50 (1.0x) | 11.95 (4.8x) | 11.93 (4.8x) | 164.98 (65.9x) | 15.27 (6.1x) | 10.35 (4.1x) |
| nucleic | 1.02 (1.0x) | 1.51 (1.5x) | 1.54 (1.5x) | 31.63 (31.1x) | 8.71 (8.6x) | 2.92 (2.9x) |
| pi | 0.24 (1.0x) | 0.11 (0.5x) | 0.11 (0.5x) | crashed | 1.96 (8.2x) | 0.05 (0.2x) |
| pnpoly | 2.92 (1.0x) | 3.38 (1.2x) | 3.44 (1.2x) | 95.57 (32.7x) | 13.18 (4.5x) | 7.67 (2.6x) |
| ray | 1.35 (1.0x) | 1.73 (1.3x) | 1.69 (1.3x) | 84.46 (62.8x) | 10.48 (7.8x) | 3.58 (2.7x) |
| simplex | 1.29 (1.0x) | 1.52 (1.2x) | 1.54 (1.2x) | 72.78 (56.5x) | 19.13 (14.9x) | 3.94 (3.1x) |
| ack | 1.37 (1.0x) | 5.37 (3.9x) | 4.99 (3.6x) | 23.25 (17.0x) | 42.15 (30.8x) | 5.61 (4.1x) |
| array1 | 2.71 (1.0x) | 2.60 (1.0x) | 2.71 (1.0x) | 21.55 (7.9x) | 13.85 (5.1x) | 4.03 (1.5x) |
| string | 0.74 (1.0x) | 0.96 (1.3x) | 0.96 (1.3x) | 2.91 (3.9x) | 0.46 (0.6x) | 0.10 (0.1x) |
| sum1 | 0.82 (1.0x) | 1.43 (1.7x) | 1.71 (2.1x) | 31.87 (38.9x) | 0.64 (0.8x) | 2.67 (3.3x) |
| cat | 1.56 (1.0x) | 1.36 (0.9x) | 1.51 (1.0x) | 25.31 (16.2x) | 7.22 (4.6x) | 8.97 (5.7x) |
| tail | 1.71 (1.0x) | 0.38 (0.2x) | 0.38 (0.2x) | 3.39 (2.0x) | 2.42 (1.4x) | 4.00 (2.3x) |
| wc | 0.87 (1.0x) | 1.10 (1.3x) | 1.49 (1.7x) | 166.40 (191.2x) | 11.34 (13.0x) | 5.40 (6.2x) |
| read1 | 1.19 (1.0x) | 1.28 (1.1x) | 1.23 (1.0x) | 89.59 (75.2x) | 1.24 (1.0x) | 3.31 (2.8x) |
| compiler | 0.89 (1.0x) | 1.99 (2.2x) | 2.24 (2.5x) | 43.96 (49.4x) | 13.47 (15.1x) | 1.59 (1.8x) |
| conform | 1.14 (1.0x) | 2.52 (2.2x) | 4.33 (3.8x) | 59.70 (52.5x) | 22.90 (20.2x) | 3.61 (3.2x) |
| dynamic | 1.56 (1.0x) | 2.01 (1.3x) | 2.11 (1.4x) | 97.16 (62.5x) | 9.62 (6.2x) | 3.75 (2.4x) |
| earley | 2.20 (1.0x) | 1.19 (0.5x) | 1.18 (0.5x) | crashed | 6.43 (2.9x) | 4.41 (2.0x) |
| graphs | 1.19 (1.0x) | 1.88 (1.6x) | 3.43 (2.9x) | crashed | 49.79 (41.9x) | 6.54 (5.5x) |
| lattice | 1.72 (1.0x) | 2.94 (1.7x) | 5.37 (3.1x) | crashed | 35.43 (20.7x) | 7.34 (4.3x) |
| matrix | 0.60 (1.0x) | 1.33 (2.2x) | 2.20 (3.7x) | 85.53 (141.8x) | 18.94 (31.4x) | 3.08 (5.1x) |
| maze | 0.45 (1.0x) | 1.12 (2.5x) | 1.09 (2.4x) | 33.43 (74.4x) | 11.16 (24.9x) | 1.51 (3.4x) |
| mazefun | 1.14 (1.0x) | 1.34 (1.2x) | 1.38 (1.2x) | 43.22 (37.8x) | 21.96 (19.2x) | 3.42 (3.0x) |
| nqueens | 3.50 (1.0x) | 4.60 (1.3x) | 4.46 (1.3x) | 633.75 (181.1x) | 39.62 (11.3x) | 6.33 (1.8x) |
| paraffins | 3.80 (1.0x) | 3.88 (1.0x) | 2.66 (0.7x) | crashed | 4.29 (1.1x) | 2.41 (0.6x) |
| parsing | 1.64 (1.0x) | 1.44 (0.9x) | 1.94 (1.2x) | crashed | 67.55 (41.2x) | 4.79 (2.9x) |
| peval | 1.04 (1.0x) | 1.97 (1.9x) | 2.10 (2.0x) | 70.53 (67.5x) | 21.25 (20.3x) | 4.31 (4.1x) |
| primes | 0.52 (1.0x) | 1.26 (2.4x) | 1.24 (2.4x) | 7.68 (14.8x) | 6.90 (13.3x) | 1.68 (3.2x) |
| quicksort | 2.33 (1.0x) | 3.41 (1.5x) | 5.90 (2.5x) | 157.72 (67.6x) | 21.93 (9.4x) | 4.44 (1.9x) |
| scheme | 1.38 (1.0x) | 2.82 (2.0x) | 4.32 (3.1x) | 45.68 (33.1x) | 16.02 (11.6x) | 5.07 (3.7x) |
| slatex | 5.48 (1.0x) | 12.86 (2.3x) | 13.11 (2.4x) | 53.22 (9.7x) | 11.74 (2.1x) | 10.56 (1.9x) |
| chudnovsky | 0.12 (1.0x) | 0.05 (0.4x) | 0.05 (0.4x) | 60.80 (495.0x) | 0.48 (3.9x) | 0.07 (0.5x) |
| nboyer | 1.18 (1.0x) | 1.18 (1.0x) | 1.09 (0.9x) | 13.64 (11.6x) | 10.35 (8.8x) | 1.66 (1.4x) |
| sboyer | 0.50 (1.0x) | 0.77 (1.5x) | 0.69 (1.4x) | 13.17 (26.2x) | 9.54 (19.0x) | 1.64 (3.3x) |
| gcbench | 0.46 (1.0x) | 0.96 (2.1x) | 0.93 (2.0x) | crashed | 11.41 (25.0x) | 0.85 (1.9x) |
| mperm | 5.82 (1.0x) | 4.37 (0.8x) | 4.35 (0.7x) | 74.09 (12.7x) | 16.70 (2.9x) | 6.15 (1.1x) |
| equal | 0.45 (1.0x) | 0.49 (1.1x) | 0.47 (1.1x) | 17.63 (39.2x) | 6.12 (13.6x) | crashed |
| bv2string | 0.56 (1.0x) | 2.27 (4.1x) | 2.59 (4.6x) | 3.62 (6.4x) | 5.24 (9.3x) | 0.90 (1.6x) |
| **geometric mean vs chez** | 1.0x | 1.44x | 1.55x | 35.7x | 9.2x | 2.8x |
| **completed** | 57/57 | 57/57 | 57/57 | 49/57 | 57/57 | 56/57 |
