#!/bin/sh
# Guile's test suite (vendor/guile-test-suite/tests) on the Guile mode:
# each file in its own SBCL (tests/run-guile-test.lisp), JOBS at a time,
# each under LIMIT seconds; then the totals.  Arguments, if any, name
# the files to run (e.g. "hash alist").
#
# Usage: sh tests/run-guile-tests.sh [name ...]
cd "$(dirname "$0")/.." || exit 1
SBCL=${SBCL:-sbcl}
JOBS=${JOBS:-6}
LIMIT=${LIMIT:-300}
OUT=${OUT:-/tmp/quasischeme-guile-tests}
# How Guile's compiler runs (src/guile/boot.lisp, *GUILE-COMPILER*):
# natively, unless QUASISCHEME_GUILE_COMPILER says otherwise; the files
# that test Guile's VM itself (coverage, statprof, ...) need compile's
# code to run on the VM, so they get Guile's compiler, from its sources
VM_TESTS="compiler coverage session statprof"
export SBCL LIMIT OUT VM_TESTS

if [ "$1" = "--one" ]; then
  f=$2; name=$(basename "$f" .test)
  mode=${QUASISCHEME_GUILE_COMPILER:-native}
  case " $VM_TESTS " in *" $name "*) mode=${QUASISCHEME_GUILE_COMPILER:-vm-source} ;; esac
  QUASISCHEME_GUILE_COMPILER=$mode $SBCL --dynamic-space-size 4GB --control-stack-size 500MB --script tests/run-guile-test.lisp "$f" \
    > "$OUT/$name.out" 2> "$OUT/$name.err" & pid=$!
  ( sleep "$LIMIT"; kill -9 $pid 2>/dev/null ) > /dev/null 2>&1 & w=$!
  wait $pid 2>/dev/null
  kill $w 2>/dev/null
  line=$(grep "^$name" "$OUT/$name.out" | tail -1)
  [ -n "$line" ] || line="$name crashed=1"
  echo "$line"
  exit 0
fi

mkdir -p "$OUT"
if [ $# -gt 0 ]; then
  files=$(for n in "$@"; do echo "vendor/guile-test-suite/tests/$n.test"; done)
else
  files=$(ls vendor/guile-test-suite/tests/*.test)
fi
# compile once, before the parallel runs
$SBCL --dynamic-space-size 4GB --non-interactive \
  --eval '(require :asdf)' \
  --eval '(let ((s (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file s) (load s)))' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system :quasischeme/guile))' \
  > "$OUT/build.log" 2>&1
echo "$files" | xargs -P "$JOBS" -n 1 sh tests/run-guile-tests.sh --one | sort > "$OUT/results.txt"
cat "$OUT/results.txt"
awk '{ for (i = 2; i <= NF; i++) { split($i, kv, "="); if (kv[2] ~ /^[0-9]+$/) t[kv[1]] += kv[2] } }
     END { printf "Guile test suite:"; for (k in t) printf " %s=%d", k, t[k]; printf "\n" }' "$OUT/results.txt"
