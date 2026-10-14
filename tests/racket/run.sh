#!/bin/sh
# --racket's tests (docs/racket.md, milestones), for each strategy:
#
#   sh tests/racket/run.sh [STRATEGY ...]      (default: cs linklet hybrid)
#
# Each strategy runs the same steps, so the three can be compared:
#   linklets   the hand-written linklets in tests/racket/linklets/: each
#              NAME.rktl holds linklets, and NAME.expected what
#              instantiating them writes (which Racket's own
#              compile-linklet agrees with)
#   regexp     Racket's rx.rktl checks on the regexp layer (milestone 2)
#   suite      pkgs/racket-test-core, one file per process (milestone 7)
# The linklets step needs no Racket source; the others need
# sh utils/racket/setup.sh first.  Status: strategy linklet runs the
# linklets step; the rest report "not implemented yet".
cd "$(dirname "$0")/../.." || exit 1
OUT=${OUT:-${TMPDIR:-/tmp}/pseudoscheme-racket-tests}
mkdir -p "$OUT"
[ $# -gt 0 ] || set -- cs linklet hybrid
status=0
for strategy in "$@"; do
  pass=0 total=0
  for f in tests/racket/linklets/*.rktl; do
    name=$(basename "$f" .rktl)
    out=$OUT/$strategy-$name.out
    total=$((total + 1))
    bin/pseudoscheme --racket="$strategy" "$f" > "$out" 2>&1
    if cmp -s "$out" "tests/racket/linklets/$name.expected"; then
      pass=$((pass + 1))
    else
      echo "$strategy $name: $(tail -1 "$out")"
      status=1
    fi
  done
  echo "$strategy: $pass of $total linklet tests passed"
done
exit $status
