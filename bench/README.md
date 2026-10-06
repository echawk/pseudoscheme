# Benchmarks

`r7rs-benchmarks/` is a vendored copy of ecraven's
[r7rs-benchmarks](https://github.com/ecraven/r7rs-benchmarks) (commit
`85f6acd`, February 2026). It is the suite behind the usual
cross-implementation comparison at
<https://ecraven.github.io/r7rs-benchmarks/>. The benchmarks are
Larceny's R7RS benchmarks, "taken with kind permission from the Larceny
project, based on the Gabriel and Gambit benchmarks". Clinger's `bench`
driver is included.

**Provenance and licences.** The upstream repository has no licence
file. Most benchmarks are public domain or carry their authors' notices,
which are kept intact: Olin Shivers, William D Clinger, Al Petrofsky.
`alexpander.scm` is dual-licensed under a BSD-style licence or the GPL,
and is used here under the BSD-style terms. Only `src/`, `inputs/`, the
`bench` driver, `Makefile` and `README.org` are copied. ecraven's
published results, graphs and HTML are not.

**Local changes**, each marked `PSEUDOSCHEME:` in `bench`:

- `pseudoscheme`, run as `bin/pseudoscheme prog.scm < input`; its
  implementation name is defined in `src/Pseudoscheme-postlude.scm`.
- `chez-akku`: Chez Scheme with Akku's `akku-r7rs` libraries on
  `CHEZ_LIBDIRS`, and no prelude. Chez 10 has no R7RS libraries of its
  own, and allows only one `import` per program.
- `gauche-local`: Gauche without SLIB. The stock postlude imports SLIB
  only to get the version string.
- `BENCH_TEMP` sets where the assembled programs go (default
  `/tmp/r7rs-benchmarks`), and `CHEZ_LIBDIRS` replaces a hard-coded
  path.

## Running

On GitHub, the Benchmarks workflow (`.github/workflows/bench.yml`,
started by hand from the Actions tab) runs every benchmark with full
continuations (the default) and escape-only, and tables both in its
summary. Locally:

```sh
make -C contrib/cli                               # build bin/pseudoscheme
bench/run.sh                                      # Pseudoscheme, every benchmark
bench/run.sh pseudoscheme chez-akku guile -- fib tak nqueens
python3 bench/summarize.py                        # table of results.* so far
```

Each benchmark is one program: the benchmark, `common.scm` (the timing
harness) and a postlude. Its input is read from stdin. The time reported
is that of the benchmark's own loop, measured with `current-jiffy`, so
the time Pseudoscheme spends expanding, translating and compiling the
program with SBCL is not included. That time *is* charged against
`CPU_LIMIT` (300 s by default), however.

## Results

See `RESULTS.md`.
