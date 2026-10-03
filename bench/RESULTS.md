# Results

October 2026, on an Apple M4 MacBook Air (4 performance and 6
efficiency cores) running macOS. These are single runs of
`r7rs-benchmarks` with its standard inputs. Times are in seconds, and
the figure in parentheses is the ratio to Chez Scheme. Lower is better.

| implementation | version | geometric mean vs Chez | benchmarks completed |
|---|---|---|---|
| Chez Scheme (with akku-r7rs; `--optimize-level 2`) | 10.4.1 | 1.0× | 57/57 |
| Guile | 3.0.11 | 2.8× | 56/57 |
| **Pseudoscheme** (SBCL 2.6.9) | 0.2 | **3.0×** | **57/57** |
| Gauche | 0.9.15 | 9.2× | 57/57 |
| Chibi (a bytecode interpreter) | 0.12.0 | 35.7× | 49/57 |

Pseudoscheme is in Guile's range. It is at or ahead of Chez on:
- `tail`, `chudnovsky`, `pi` and `paraffins`: bignums and I/O, where
  SBCL's runtime is strong;
- `cat`, `mperm`, `earley` and `read1`.

Its worst ratios are `lattice` (13.5×), `gcbench` (10.2×), `wc` (8.7×),
`ack` (8.6×), `conform` (7.9×), `mbrotZ` (7.5×) and `takl` (6.9×).

**Caveats.**
- The implementations ran three at a time (Pseudoscheme, Chez and Guile;
  then Chibi and Gauche), so that each could have a performance core.
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

The previous build ran the same programs much slower. That build
translated psyntax's references to primitives, `(primitive +)`, into
calls through a lookup function. The host environment also called its
copies of the standard procedures out of line, and number comparisons
went through a fully generic function. Three changes fixed this:

- `(primitive x)` became a plain reference to `x` (`psx::open-primitives`).
- The host environment now shares R5RS's own bindings, which the
  translator open-codes, so `(+ a b)` is CL's `+`.
- Two-argument numeric comparisons have an inline fast path for fixnums
  and doubles (`src/numbers.lisp`).

`car`, `cdr` and the `cXr`s are open-coded with an inline pair check, so
`(car '())` is still an error.

| benchmark | previous build | now | speedup |
|---|---|---|---|
| fib | 49.21 | 6.35 | 7.7× |
| tak | 22.84 | 3.13 | 7.3× |
| nqueens | 61.19 | 8.75 | 7.0× |
| quicksort | 28.09 | 6.90 | 4.1× |
| mbrot | 19.78 | 5.10 | 3.9× |
| earley | 4.99 | 1.36 | 3.7× |
| browse | 7.90 | 3.03 | 2.6× |
| deriv | 3.87 | 1.60 | 2.4× |

## All benchmarks

| benchmark | chez | pseudoscheme | chibi | gauche | guile |
|---|---|---|---|---|---|
| browse | 0.65 (1.0x) | 3.39 (5.3x) | crashed | 10.87 (16.9x) | 2.27 (3.5x) |
| deriv | 0.51 (1.0x) | 2.18 (4.3x) | 39.76 (78.5x) | 13.83 (27.3x) | 3.41 (6.7x) |
| destruc | 0.91 (1.0x) | 2.69 (3.0x) | 35.32 (38.9x) | 14.27 (15.7x) | 1.61 (1.8x) |
| diviter | 0.93 (1.0x) | 1.67 (1.8x) | 21.53 (23.3x) | 8.64 (9.3x) | 3.24 (3.5x) |
| divrec | 1.02 (1.0x) | 4.27 (4.2x) | 21.05 (20.5x) | 10.54 (10.3x) | 5.24 (5.1x) |
| puzzle | 1.40 (1.0x) | 6.74 (4.8x) | 101.89 (72.9x) | 17.21 (12.3x) | 4.02 (2.9x) |
| triangl | 1.27 (1.0x) | 4.22 (3.3x) | 35.96 (28.3x) | 12.23 (9.6x) | 3.37 (2.7x) |
| tak | 0.74 (1.0x) | 3.47 (4.7x) | 25.37 (34.3x) | 11.12 (15.0x) | 2.11 (2.9x) |
| takl | 1.60 (1.0x) | 11.13 (6.9x) | 202.89 (126.5x) | 42.16 (26.3x) | 2.83 (1.8x) |
| ntakl | 1.54 (1.0x) | 2.71 (1.8x) | 39.87 (25.9x) | 35.92 (23.3x) | 2.94 (1.9x) |
| cpstak | 1.29 (1.0x) | 7.69 (5.9x) | 90.09 (69.6x) | 42.30 (32.7x) | 7.77 (6.0x) |
| ctak | 0.39 (1.0x) | 1.25 (3.2x) | 234.71 (603.2x) | 8.49 (21.8x) | 39.82 (102.3x) |
| fib | 1.75 (1.0x) | 7.02 (4.0x) | 27.82 (15.9x) | 25.10 (14.3x) | 5.09 (2.9x) |
| fibc | 0.26 (1.0x) | 1.50 (5.7x) | 49.01 (186.9x) | 8.00 (30.5x) | 25.49 (97.2x) |
| fibfp | 1.25 (1.0x) | 2.48 (2.0x) | 16.34 (13.0x) | 5.53 (4.4x) | 6.70 (5.3x) |
| sum | 1.78 (1.0x) | 8.86 (5.0x) | 33.52 (18.8x) | 20.19 (11.3x) | 2.46 (1.4x) |
| sumfp | 1.92 (1.0x) | 4.43 (2.3x) | 30.58 (16.0x) | 6.47 (3.4x) | 11.68 (6.1x) |
| fft | 1.41 (1.0x) | 1.88 (1.3x) | 23.29 (16.5x) | 4.94 (3.5x) | 2.87 (2.0x) |
| mbrot | 4.91 (1.0x) | 6.73 (1.4x) | 76.44 (15.6x) | 10.51 (2.1x) | 12.14 (2.5x) |
| mbrotZ | 2.50 (1.0x) | 18.74 (7.5x) | 164.98 (65.9x) | 15.27 (6.1x) | 10.35 (4.1x) |
| nucleic | 1.02 (1.0x) | 4.69 (4.6x) | 31.63 (31.1x) | 8.71 (8.6x) | 2.92 (2.9x) |
| pi | 0.24 (1.0x) | 0.14 (0.6x) | crashed | 1.96 (8.2x) | 0.05 (0.2x) |
| pnpoly | 2.92 (1.0x) | 7.32 (2.5x) | 95.57 (32.7x) | 13.18 (4.5x) | 7.67 (2.6x) |
| ray | 1.35 (1.0x) | 5.28 (3.9x) | 84.46 (62.8x) | 10.48 (7.8x) | 3.58 (2.7x) |
| simplex | 1.29 (1.0x) | 4.85 (3.8x) | 72.78 (56.5x) | 19.13 (14.9x) | 3.94 (3.1x) |
| ack | 1.37 (1.0x) | 11.73 (8.6x) | 23.25 (17.0x) | 42.15 (30.8x) | 5.61 (4.1x) |
| array1 | 2.71 (1.0x) | 3.26 (1.2x) | 21.55 (7.9x) | 13.85 (5.1x) | 4.03 (1.5x) |
| string | 0.74 (1.0x) | 1.59 (2.2x) | 2.91 (3.9x) | 0.46 (0.6x) | 0.10 (0.1x) |
| sum1 | 0.82 (1.0x) | 1.49 (1.8x) | 31.87 (38.9x) | 0.64 (0.8x) | 2.67 (3.3x) |
| cat | 1.56 (1.0x) | 1.57 (1.0x) | 25.31 (16.2x) | 7.22 (4.6x) | 8.97 (5.7x) |
| tail | 1.71 (1.0x) | 0.42 (0.2x) | 3.39 (2.0x) | 2.42 (1.4x) | 4.00 (2.3x) |
| wc | 0.87 (1.0x) | 7.54 (8.7x) | 166.40 (191.2x) | 11.34 (13.0x) | 5.40 (6.2x) |
| read1 | 1.19 (1.0x) | 1.30 (1.1x) | 89.59 (75.2x) | 1.24 (1.0x) | 3.31 (2.8x) |
| compiler | 0.89 (1.0x) | 2.85 (3.2x) | 43.96 (49.4x) | 13.47 (15.1x) | 1.59 (1.8x) |
| conform | 1.14 (1.0x) | 8.99 (7.9x) | 59.70 (52.5x) | 22.90 (20.2x) | 3.61 (3.2x) |
| dynamic | 1.56 (1.0x) | 4.45 (2.9x) | 97.16 (62.5x) | 9.62 (6.2x) | 3.75 (2.4x) |
| earley | 2.20 (1.0x) | 2.32 (1.1x) | crashed | 6.43 (2.9x) | 4.41 (2.0x) |
| graphs | 1.19 (1.0x) | 5.75 (4.8x) | crashed | 49.79 (41.9x) | 6.54 (5.5x) |
| lattice | 1.72 (1.0x) | 23.20 (13.5x) | crashed | 35.43 (20.7x) | 7.34 (4.3x) |
| matrix | 0.60 (1.0x) | 2.92 (4.8x) | 85.53 (141.8x) | 18.94 (31.4x) | 3.08 (5.1x) |
| maze | 0.45 (1.0x) | 2.18 (4.8x) | 33.43 (74.4x) | 11.16 (24.9x) | 1.51 (3.4x) |
| mazefun | 1.14 (1.0x) | 3.89 (3.4x) | 43.22 (37.8x) | 21.96 (19.2x) | 3.42 (3.0x) |
| nqueens | 3.50 (1.0x) | 9.59 (2.7x) | 633.75 (181.1x) | 39.62 (11.3x) | 6.33 (1.8x) |
| paraffins | 3.80 (1.0x) | 3.38 (0.9x) | crashed | 4.29 (1.1x) | 2.41 (0.6x) |
| parsing | 1.64 (1.0x) | 11.15 (6.8x) | crashed | 67.55 (41.2x) | 4.79 (2.9x) |
| peval | 1.04 (1.0x) | 6.10 (5.8x) | 70.53 (67.5x) | 21.25 (20.3x) | 4.31 (4.1x) |
| primes | 0.52 (1.0x) | 1.55 (3.0x) | 7.68 (14.8x) | 6.90 (13.3x) | 1.68 (3.2x) |
| quicksort | 2.33 (1.0x) | 8.81 (3.8x) | 157.72 (67.6x) | 21.93 (9.4x) | 4.44 (1.9x) |
| scheme | 1.38 (1.0x) | 5.52 (4.0x) | 45.68 (33.1x) | 16.02 (11.6x) | 5.07 (3.7x) |
| slatex | 5.48 (1.0x) | 7.52 (1.4x) | 53.22 (9.7x) | 11.74 (2.1x) | 10.56 (1.9x) |
| chudnovsky | 0.12 (1.0x) | 0.06 (0.5x) | 60.80 (495.0x) | 0.48 (3.9x) | 0.07 (0.5x) |
| nboyer | 1.18 (1.0x) | 2.43 (2.1x) | 13.64 (11.6x) | 10.35 (8.8x) | 1.66 (1.4x) |
| sboyer | 0.50 (1.0x) | 1.82 (3.6x) | 13.17 (26.2x) | 9.54 (19.0x) | 1.64 (3.3x) |
| gcbench | 0.46 (1.0x) | 4.66 (10.2x) | crashed | 11.41 (25.0x) | 0.85 (1.9x) |
| mperm | 5.82 (1.0x) | 6.08 (1.0x) | 74.09 (12.7x) | 16.70 (2.9x) | 6.15 (1.1x) |
| equal | 0.45 (1.0x) | 1.71 (3.8x) | 17.63 (39.2x) | 6.12 (13.6x) | crashed |
| bv2string | 0.56 (1.0x) | 2.56 (4.6x) | 3.62 (6.4x) | 5.24 (9.3x) | 0.90 (1.6x) |
| **geometric mean vs chez** | 1.0x | 3.0x | 35.7x | 9.2x | 2.8x |
| **completed** | 57/57 | 57/57 | 49/57 | 57/57 | 56/57 |
