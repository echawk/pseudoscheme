#!/bin/sh
# LilyPond's start-up and a small score, timed, on real Guile and on our
# libguile (docs/libguile.md), in the Linux container:
#
#   sh utils/container/run.sh lilypond-time 'sh tests/lilypond/time.sh'
#
# Each of ours is run RUNS times (default 2): the first may fill the
# compiled-module cache, the others load from it.  Settings for ours
# (PSEUDOSCHEME_GUILE_*) pass through.  With VERBOSE=1, ours also runs
# once with LilyPond's --verbose, its lines timestamped (verbose.log in
# $LOGS), to show where the start-up goes; with TRACE=1 as well, with
# each call into our libguile listed (PSEUDOSCHEME_LIBGUILE_TRACE).
cd "$(dirname "$0")/../.." || exit 1
LOGS=${LOGS:-/logs}
LILYPOND=/opt/lilypond/bin/lilypond
[ -x "$LILYPOND" ] || { echo "LilyPond isn't in the image (utils/container/Containerfile: LILYPOND_SHA256)"; exit 1; }
make -C src/libguile BUILD="$PWD/build/libguile" > "$LOGS/libguile-build.log" 2>&1 \
  || { echo "libguile: build failed (libguile-build.log)"; exit 1; }
make -C src/libguile core BUILD="$PWD/build/libguile" > "$LOGS/core-build.log" 2>&1 \
  || { echo "core: build failed (core-build.log)"; exit 1; }
top=$PWD
mkdir -p "$LOGS/time"
cp tests/lilypond/demo.ly "$LOGS/time/"
cd "$LOGS/time" || exit 1
now () { perl -MTime::HiRes=time -e 'printf "%.2f", time'; }
ours () {
  LD_LIBRARY_PATH=$top/build/libguile PSEUDOSCHEME_GUILE_CORE=$top/build/libguile/pseudoscheme-guile.core \
    timeout "${STEP_LIMIT:-900}" "$LILYPOND" "$@"
}
t0=$(now); LD_LIBRARY_PATH=/opt/guile/lib "$LILYPOND" demo.ly > reference.log 2>&1; s=$?; t1=$(now)
echo "reference (real Guile): exit $s, $(awk "BEGIN{print $t1 - $t0}") s"
t0=$(now); LD_LIBRARY_PATH=/opt/guile/lib "$LILYPOND" --version > /dev/null 2>&1; t1=$(now)
echo "reference --version: $(awk "BEGIN{print $t1 - $t0}") s"
i=1
while [ $i -le "${RUNS:-2}" ]; do
  t0=$(now); ours demo.ly > ours-$i.log 2>&1; s=$?; t1=$(now)
  echo "ours, run $i: exit $s, $(awk "BEGIN{print $t1 - $t0}") s"
  i=$((i + 1))
done
t0=$(now); ours --version > /dev/null 2>&1; t1=$(now)
echo "ours --version: $(awk "BEGIN{print $t1 - $t0}") s"
if [ -n "$VERBOSE" ]; then
  PSEUDOSCHEME_LIBGUILE_TRACE=${TRACE:-0} ours --verbose demo.ly 2>&1 | perl -MTime::HiRes=time -ne 'BEGIN{$t0=time} printf "%7.2f %s", time-$t0, $_' > verbose.log
  echo "verbose.log: $(wc -l < verbose.log) lines"
fi
