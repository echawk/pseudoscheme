#!/bin/sh
# Run the r7rs-benchmarks suite (vendored in r7rs-benchmarks/) on
# Pseudoscheme and/or other Schemes, then summarize.
#
#   bench/run.sh [system ...] [-- benchmark ...]
#
#   bench/run.sh                              pseudoscheme, all benchmarks
#   bench/run.sh pseudoscheme chez-akku guile -- fib tak nqueens
#
# Systems are the harness's names (see r7rs-benchmarks/bench), plus:
#   pseudoscheme   bin/pseudoscheme (build it first: make -C contrib/cli)
#   chez-akku      Chez Scheme with Akku's akku-r7rs libraries; set
#                  CHEZ_LIBDIRS to a .akku/lib containing them
#   gauche-local   Gauche without SLIB
# Environment: CPU_LIMIT (seconds per benchmark, default 300), BENCH_TEMP
# (scratch directory for the assembled programs, default /tmp/...).
#
# Results accumulate in r7rs-benchmarks/results.<System>; delete those
# files to start over.  bench/summarize.py prints the comparison table.

here=$(cd "$(dirname "$0")" && pwd)
systems=""
while [ $# -gt 0 ] && [ "$1" != "--" ]; do systems="$systems $1"; shift; done
[ "$1" = "--" ] && shift
benchmarks="${*:-all}"
systems="${systems:-pseudoscheme}"

cd "$here/r7rs-benchmarks" || exit 1
for s in $systems; do
    ./bench "$s" "$benchmarks"
done
python3 "$here/summarize.py" "$here/r7rs-benchmarks"
