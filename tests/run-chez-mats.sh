#!/bin/sh
# Chez Scheme's own mats (vendor/chez-mats/*.ms) on the --chez mode:
# each file in its own process (tests/chez-mat.ss supplies the `mat`
# macro), JOBS at a time, each under LIMIT seconds; then per-file and
# total counts.  Arguments, if any, name the files to run (e.g. "hash 5_3").
#
# Clauses that need what Pseudoscheme won't have (`#%$` primitives,
# compile-file, the inspector, ...) count as "unsup", not as failures;
# "unbound" is a clause that referred to a name not bound here (a missing
# export).  Whole files that are about the compiler or C (cp0, cptypes,
# foreign, ftype, profile, windows) are listed as unsupported too.
#
# Usage: sh tests/run-chez-mats.sh [name ...]
#   PS=bin/pseudoscheme  JOBS=6  LIMIT=120  OUT=/tmp/pseudoscheme-chez-mats
cd "$(dirname "$0")/.." || exit 1
ROOT=$(pwd)
PS=${PS:-$ROOT/bin/pseudoscheme}
JOBS=${JOBS:-6}
LIMIT=${LIMIT:-120}
OUT=${OUT:-/tmp/pseudoscheme-chez-mats}
UNSUPPORTED_FILES="cp0 cptypes foreign ftype profile windows"
mkdir -p "$OUT" || exit 1
OUT=$(cd "$OUT" && pwd)
# separate-eval's Scheme: Pseudoscheme run as `chez`, its Chez mode
ln -sf "$PS" "$OUT/chez"
CHEZ_MAT_SCHEME="$OUT/chez --no-userinit"
export PS LIMIT OUT ROOT CHEZ_MAT_SCHEME

if [ "$1" = "--one" ]; then
  name=$2
  work="$OUT/work-$name"
  rm -rf "$work"; mkdir -p "$work"
  for f in "$ROOT"/vendor/chez-mats/*; do ln -s "$f" "$work/"; done
  # Chez's example programs, and their object files, as Chez's mats
  # makefile leaves them (an object file here is the source, compile-file's)
  for f in "$ROOT"/vendor/chez-examples/*.ss; do
    b=$(basename "$f" .ss)
    [ -e "$work/$b.ss" ] || ln -s "$f" "$work/$b.ss"
    [ -e "$work/$b.so" ] || ln -s "$f" "$work/$b.so"
  done
  skip=
  : > "$OUT/$name.out"
  : > "$OUT/$name.err"
  # a mat file's own library, as oop.ss is oop.ms's, is loaded first
  files="$name.ms"
  [ -f "$ROOT/vendor/chez-mats/$name.ss" ] && files="$name.ss $name.ms"
  attempt=0
  while [ $attempt -lt 8 ]; do
    attempt=$((attempt + 1))
    # exec, so that the kill below reaches pseudoscheme, not just a subshell
    ( cd "$work" && CHEZ_MAT_SKIP="$skip" exec "$PS" --chez "$ROOT/tests/chez-mat.ss" $files \
        > "$OUT/$name.out.$attempt" 2>> "$OUT/$name.err" < /dev/null ) & pid=$!
    ( sleep "$LIMIT"; kill -9 $pid 2>/dev/null ) > /dev/null 2>&1 & w=$!
    wait $pid 2>/dev/null
    kill $w 2>/dev/null
    cp "$OUT/$name.out.$attempt" "$OUT/$name.out"
    grep -q '^MATDONE' "$OUT/$name.out.$attempt" && break
    # killed or crashed: the clause that started and never reported is
    # skipped (reported as crash) in the next attempt
    last=$(grep '^MAT ' "$OUT/$name.out.$attempt" | tail -1)
    case $last in
      "MAT start "*) set -- $last; skip="$skip $3:$4" ;;
      *) break ;;
    esac
  done
  rm -f "$OUT/$name.out".[0-9]*
  rm -rf "$work"
  done=ok; grep -q '^MATDONE' "$OUT/$name.out" || done=incomplete
  c() { grep -c "^MAT $1 " "$OUT/$name.out"; }
  echo "$name pass=$(c pass) fail=$(c fail) error=$(c error) unbound=$(c unbound) unsup=$(c unsup) crash=$(c crash) $done"
  exit 0
fi

mkdir -p "$OUT"
if [ $# -gt 0 ]; then
  names="$*"
else
  names=$(cd vendor/chez-mats && ls *.ms | sed 's/\.ms$//')
fi
echo "$names" | tr ' ' '\n' | xargs -P "$JOBS" -n 1 sh tests/run-chez-mats.sh --one | sort > "$OUT/results.txt"
cat "$OUT/results.txt"
awk -v unsup="$UNSUPPORTED_FILES" '
  BEGIN { n = split(unsup, u, " "); for (i = 1; i <= n; i++) U[u[i]] = 1 }
  { name = $1
    for (i = 2; i <= NF; i++) { split($i, kv, "="); if (kv[2] ~ /^[0-9]+$/) {
        if (name in U) us[kv[1]] += kv[2]; else t[kv[1]] += kv[2] } }
    if ($NF == "incomplete") inc = inc " " name }
  END { printf "Chez mats:"; for (k in t) printf " %s=%d", k, t[k]
        printf "\n(unsupported files, not counted:"; for (k in us) printf " %s=%d", k, us[k]
        printf ")\nincomplete (crashed or timed out):%s\n", inc }' "$OUT/results.txt"
