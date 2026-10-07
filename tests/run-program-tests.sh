#!/bin/sh
# The programs in tests/programs/: Scheme and Common Lisp using each
# other, and real Common Lisp libraries (from Quicklisp, which must be
# installed; it fetches what's missing) and C libraries (through CFFI).
# Each program checks itself and exits 0 when its checks pass: a .scm
# file runs on the command-line Scheme, with tests/programs/lib/ on the
# library path; a .lisp file in SBCL, loading tests/programs/harness.lisp.
#
#   sh tests/run-program-tests.sh [PSEUDOSCHEME [NAME ...]]
#
# PSEUDOSCHEME is the command-line program (default bin/pseudoscheme);
# NAME ... the programs to run (default: all), as file names without the
# directory (c-libraries.scm).  Each runs in a scratch directory of its
# own, at most $PROGRAM_TEST_TIMEOUT seconds (default 300); a failure's
# output is kept there, as NAME.out.

root=$(cd "$(dirname "$0")/.." && pwd)
ps=${1:-$root/bin/pseudoscheme}
case $ps in /*) ;; *) ps=$(pwd)/$ps ;; esac
[ $# -gt 0 ] && shift
sbcl=${SBCL:-sbcl}
limit=${PROGRAM_TEST_TIMEOUT:-300}
dir=$root/tests/programs
work=$(mktemp -d "${TMPDIR:-/tmp}/pseudoscheme-program-tests.XXXXXX")

if [ -t 1 ]; then
  green=$(printf '\033[32m'); red=$(printf '\033[31m'); dim=$(printf '\033[2m'); off=$(printf '\033[0m')
else
  green=; red=; dim=; off=
fi

if [ $# -gt 0 ]; then
  names="$*"
else
  names=$(cd "$dir" && ls *.scm *.lisp | grep -v '^harness\.lisp$')
fi
total=$(echo $names | wc -w | tr -d ' ')

now() { perl -MTime::HiRes=time -e 'printf "%.1f", time' 2>/dev/null || date +%s; }

i=0; pass=0; fail=0; failed=""; start_all=$(now)
for name in $names; do
  i=$((i+1))
  printf '%s[%d/%d]%s %-22s ' "$dim" "$i" "$total" "$off" "$name"
  case $name in
    *.scm) set -- "$ps" --quicklisp -L "$dir/lib" "$dir/$name" ;;
    *.lisp) set -- "$sbcl" --dynamic-space-size 4GB --control-stack-size 500MB --script "$dir/$name" ;;
    *) set -- false ;;
  esac
  if [ ! -f "$dir/$name" ]; then
    fail=$((fail+1)); failed="$failed $name"
    printf '%sFAIL%s  no tests/programs/%s\n' "$red" "$off" "$name"
    continue
  fi
  start=$(now)
  # the program in the background, and a watchdog that kills it
  (cd "$work" && exec "$@") > "$work/$name.out" 2>&1 &
  pid=$!
  (sleep "$limit"; kill -9 $pid 2>/dev/null) > /dev/null 2>&1 &
  watchdog=$!
  wait $pid; code=$?
  kill $watchdog 2>/dev/null; wait $watchdog 2>/dev/null
  seconds=$(echo "$(now) $start" | awk '{printf "%.1f", $1 - $2}')
  # the tally SRFI 64 (or the harness) printed
  passes=$(grep -a '^# of expected passes\|^Passes:' "$work/$name.out" | tail -1 | awk '{print $NF}')
  fails=$(grep -a '^# of unexpected failures\|^Failures:' "$work/$name.out" | tail -1 | awk '{print $NF}')
  tally=${passes:+$passes passed}${fails:+, $fails failed}
  if [ $code = 0 ]; then
    pass=$((pass+1))
    printf '%sok%s    %s%5ss  %s%s\n' "$green" "$off" "$dim" "$seconds" "$tally" "$off"
  else
    fail=$((fail+1)); failed="$failed $name"
    case $code in 137) why="killed after ${limit}s" ;; *) why="exit $code" ;; esac
    printf '%sFAIL%s  %s%5ss  %s%s  (%s; %s)\n' "$red" "$off" "$dim" "$seconds" "$tally" "$off" "$why" "$work/$name.out"
  fi
done

seconds=$(echo "$(now) $start_all" | awk '{printf "%.0f", $1 - $2}')
echo
if [ $fail = 0 ]; then
  printf '%sPrograms: all %d passed%s (%ss)\n' "$green" "$pass" "$off" "$seconds"
else
  printf '%sPrograms: %d of %d passed; failed:%s%s (%ss)\n' "$red" "$pass" "$((pass+fail))" "$failed" "$off" "$seconds"
fi
[ $fail = 0 ] && rm -rf "$work"
[ $fail = 0 ]
