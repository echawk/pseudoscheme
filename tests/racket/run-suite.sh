#!/bin/sh
# Racket's own test suite, pkgs/racket-test-core/tests/racket (milestone 7
# of docs/racket.md), on --racket=linklet, tallied like the Chez mats:
#
#   sh tests/racket/run-suite.sh [NAME ...]     (NAME: basic, list, ...)
#
# Each file runs in its own process, loaded into a namespace with racket
# required (as all.rktl's sandbox loads its files; some of
# core-tests.rktl's need more than racket/base too).  Its testing.rktl reports how many tests it performed and
# the errors; a file that prints no report crashed or ran past LIMIT
# seconds (default 600).  J files run at a time (default 2).
#
# QUASISCHEME names the binary to run (default: bin/quasischeme), so a
# long run can go on while bin/ is rebuilt; it then has a compiled-file
# cache of its own (build/suite-cache), since each build prunes the
# others' generations of the shared one.  REF=1 runs the installed racket on the same files instead, for the
# reference counts (some tests depend on the machine).  Results go to
# $OUT/suite-{ours,racket}/: NAME.out per file and summary.txt.
# Needs sh utils/racket/setup.sh first.
cd "$(dirname "$0")/../.." || exit 1
ROOT=$PWD
T=$ROOT/build/external/racket/pkgs/racket-test-core/tests/racket
[ -d "$T" ] || { echo "No Racket source: run utils/racket/setup.sh first." >&2; exit 1; }
OUT=${OUT:-${TMPDIR:-/tmp}/quasischeme-racket-tests}
LIMIT=${LIMIT:-600}
J=${J:-2}
if [ -n "$REF" ]; then who=racket; else who=ours; fi
DIR=$OUT/suite-$who
mkdir -p "$DIR"
if [ -n "$QUASISCHEME" ] && [ -z "$QUASISCHEME_LIBRARY_CACHE_DIRECTORY" ]; then
  export QUASISCHEME_LIBRARY_CACHE_DIRECTORY=$ROOT/build/suite-cache/libraries
  mkdir -p "$QUASISCHEME_LIBRARY_CACHE_DIRECTORY"
fi

# core-tests.rktl's files, then all.rktl's
CORE="basic unicode rx read print macro syntax letrec procs stx linklet module
submodule stxparam number unsafe foreign-inline object struct struct-derived
thread parallel logger sync place deep contmark prompt will namespac expobs
collects modprot chaperone param port file udp path jitinline optimize name
srcloc error"
MORE="for list math vector hash function dict fixnum flonum extflonum string
treelist fasl async-channel pathlib filelib portlib set date cmdline stream
sequence generator pretty control serialize sandbox shared resource syntaxlibs
subprocess bytes trace trait promise future moddep boundmap-test id-table-test
id-set-test cm module-reader version foreign-test cstruct uni-norm"
[ $# -gt 0 ] || set -- $CORE $MORE

run_one() {
  name=$1
  w=$DIR/$name.rkt
  cat > "$w" <<EOF
#lang racket/base
(parameterize ([current-namespace (make-base-empty-namespace)])
  (namespace-require 'racket)
  (load "$T/$name.rktl"))
EOF
  if [ -n "$REF" ]; then set -- racket "$w"; else set -- "${QUASISCHEME:-$ROOT/bin/quasischeme}" --racket=linklet "$w"; fi
  (cd "$T" && exec "$@") > "$DIR/$name.out" 2>&1 &
  pid=$!
  ( sleep "$LIMIT"; kill -9 $pid 2>/dev/null ) &
  watcher=$!
  wait $pid
  kill $watcher 2>/dev/null
  # the last report is the file's total
  performed=$(grep -o 'Performed [0-9]* expression tests' "$DIR/$name.out" | tail -1 | grep -o '[0-9][0-9]*')
  errors=$(grep -o 'Errors were \[[0-9]*\]' "$DIR/$name.out" | tail -1 | grep -o '[0-9][0-9]*')
  if [ -z "$performed" ]; then
    echo "$name crashed: $(grep -v '^ ' "$DIR/$name.out" | tail -1 | cut -c1-150)"
  else
    echo "$name $performed ${errors:-0}"
  fi
}

: > "$DIR/results.txt"
running=0
for name in "$@"; do
  run_one "$name" >> "$DIR/results.txt" &
  running=$((running + 1))
  if [ "$running" -ge "$J" ]; then wait; running=0; fi
done
wait
sort "$DIR/results.txt" > "$DIR/summary.txt"
awk '$2 == "crashed:" { crashed++; next }
     { files++; tests += $2; failed += $3; if ($3 == 0) clean++ }
     END { printf "%s: %d files reported (%d with no failures), %d crashed or timed out; %d tests, %d failed\n",
                  "'"$who"'", files, clean, crashed, tests, failed }' "$DIR/summary.txt" | tee -a "$DIR/summary.txt"
