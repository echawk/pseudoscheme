#!/bin/sh
# Run the r7rs-benchmarks suite (vendored in r7rs-benchmarks/) on
# Quasischeme and/or other Schemes, then summarize.
#
#   bench/run.sh [system ...] [-- benchmark ...]
#
#   bench/run.sh                              quasischeme, all benchmarks
#   bench/run.sh quasischeme chez-akku guile -- fib tak nqueens
#   bench/run.sh modes -- fib tak     each of Quasischeme's modes
#
# Systems are the harness's names (see r7rs-benchmarks/bench), plus:
#   quasischeme   bin/quasischeme (build it first: make -C contrib/cli)
#   chez-akku      Chez Scheme with Akku's akku-r7rs libraries; set
#                  CHEZ_LIBDIRS to a .akku/lib containing them
#   gauche-local   Gauche without SLIB
#   quasischeme-r6rs, -r5rs, -chez, -guile, -racket
#                  bin/quasischeme's other modes on the same programs;
#                  -racket and -r5rs with a prelude of the R7RS names
#                  they lack, and -r5rs timed from outside (R5RS has no
#                  clock), so its times include startup and compilation
#   modes          quasischeme and all five of those
#   racket-base    Racket, racket/base with -racket's prelude
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
systems="${systems:-quasischeme}"
modes="quasischeme quasischeme-r6rs quasischeme-r5rs quasischeme-chez quasischeme-guile quasischeme-racket"
expanded=""
for s in $systems; do
    if [ "$s" = modes ]; then expanded="$expanded $modes"; else expanded="$expanded $s"; fi
done
systems=$expanded

cd "$here/r7rs-benchmarks" || exit 1
for s in $systems; do
    ./bench "$s" "$benchmarks"
done
python3 "$here/summarize.py" "$here/r7rs-benchmarks"
