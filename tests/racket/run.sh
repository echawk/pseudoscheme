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
#   expander   core forms evaluated through Racket's expander (-e), in a
#              #%kernel namespace: tests/racket/expander.rktd holds each
#              expression and its expected printed value
#   regexp     Racket's rx.rktl checks on the regexp layer (milestone 2)
#   suite      pkgs/racket-test-core, one file per process (milestone 7)
# The linklets step needs no Racket source; the others need
# sh utils/racket/setup.sh first.  Status: strategy linklet runs the
# linklets and expander steps; the rest report "not implemented yet".
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
  # the expander: every expression in one process (it compiles once)
  program="(begin"
  while IFS='	' read -r expr expected; do
    program="$program (write $expr) (newline)"
  done < tests/racket/expander.rktd
  program="$program)"
  out=$OUT/$strategy-expander.out
  bin/pseudoscheme --racket="$strategy" -e "$program" > "$out" 2>&1
  cut -f2 tests/racket/expander.rktd > "$OUT/$strategy-expander.expected"
  n=$(wc -l < tests/racket/expander.rktd)
  if cmp -s "$out" "$OUT/$strategy-expander.expected"; then
    echo "$strategy: $((n)) of $((n)) expander tests passed"
  else
    echo "$strategy: expander tests failed: $(diff "$out" "$OUT/$strategy-expander.expected" | head -3 | tr '\n' ' ')"
    status=1
  fi
done
exit $status
