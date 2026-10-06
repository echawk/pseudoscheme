#!/bin/sh
# The bundled SRFIs' tests, in numeric order, a line for each as it runs:
# each src/srfi/tests/N.scm is an R7RS program that exits 0 when its
# tests pass (SRFI 64, mostly).
#
#   sh tests/run-srfi-tests.sh [PSEUDOSCHEME [N ...]]
#
# PSEUDOSCHEME is the command-line program (default bin/pseudoscheme);
# N ... the SRFIs to test (default: all).  Each test runs in a scratch
# directory of its own (SRFI 64 writes a log file in the current one),
# at most $SRFI_TEST_TIMEOUT seconds (default 120); a failure's output
# is kept there, as N.out.

root=$(cd "$(dirname "$0")/.." && pwd)
ps=${1:-$root/bin/pseudoscheme}
case $ps in /*) ;; *) ps=$(pwd)/$ps ;; esac
[ $# -gt 0 ] && shift
limit=${SRFI_TEST_TIMEOUT:-120}
work=$(mktemp -d "${TMPDIR:-/tmp}/pseudoscheme-srfi-tests.XXXXXX")

if [ -t 1 ]; then
  green=$(printf '\033[32m'); red=$(printf '\033[31m'); dim=$(printf '\033[2m'); off=$(printf '\033[0m')
else
  green=; red=; dim=; off=
fi

if [ $# -gt 0 ]; then
  numbers="$*"
else
  numbers=$(ls "$root"/src/srfi/tests/*.scm | sed 's|.*/||; s|\.scm$||' | sort -n)
fi
total=$(echo $numbers | wc -w | tr -d ' ')

now() { perl -MTime::HiRes=time -e 'printf "%.1f", time' 2>/dev/null || date +%s; }

i=0; pass=0; fail=0; failed=""; start_all=$(now)
for n in $numbers; do
  i=$((i+1))
  printf '%s[%3d/%d]%s SRFI %-4s ' "$dim" "$i" "$total" "$off" "$n"
  if [ ! -f "$root/src/srfi/tests/$n.scm" ]; then
    fail=$((fail+1)); failed="$failed $n"
    printf '%sFAIL%s  no src/srfi/tests/%s.scm\n' "$red" "$off" "$n"
    continue
  fi
  start=$(now)
  # the test in the background, and a watchdog that kills it
  (cd "$work" && exec "$ps" "$root/src/srfi/tests/$n.scm") > "$work/$n.out" 2>&1 &
  pid=$!
  (sleep "$limit"; kill -9 $pid 2>/dev/null) > /dev/null 2>&1 &
  watchdog=$!
  wait $pid; code=$?
  kill $watchdog 2>/dev/null; wait $watchdog 2>/dev/null
  seconds=$(echo "$(now) $start" | awk '{printf "%.1f", $1 - $2}')
  # SRFI 64's tally, when the test printed one
  passes=$(grep -a '^# of expected passes\|^Passes:' "$work/$n.out" | tail -1 | awk '{print $NF}')
  fails=$(grep -a '^# of unexpected failures\|^Failures:' "$work/$n.out" | tail -1 | awk '{print $NF}')
  tally=${passes:+$passes passed}${fails:+, $fails failed}
  if [ $code = 0 ]; then
    pass=$((pass+1))
    printf '%sok%s    %s%5ss  %s%s\n' "$green" "$off" "$dim" "$seconds" "$tally" "$off"
  else
    fail=$((fail+1)); failed="$failed $n"
    case $code in 137) why="killed after ${limit}s" ;; *) why="exit $code" ;; esac
    printf '%sFAIL%s  %s%5ss  %s%s  (%s; %s)\n' "$red" "$off" "$dim" "$seconds" "$tally" "$off" "$why" "$work/$n.out"
  fi
done

seconds=$(echo "$(now) $start_all" | awk '{printf "%.0f", $1 - $2}')
echo
if [ $fail = 0 ]; then
  printf '%sSRFIs: all %d test programs passed%s (%ss)\n' "$green" "$pass" "$off" "$seconds"
else
  printf '%sSRFIs: %d of %d test programs passed; failed:%s%s (%ss)\n' "$red" "$pass" "$((pass+fail))" "$failed" "$off" "$seconds"
fi
[ $fail = 0 ] && rm -rf "$work"
[ $fail = 0 ]
