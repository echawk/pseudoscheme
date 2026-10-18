#!/bin/sh
# LilyPond's start-up and a small score, timed, on real Guile and on our
# libguile (docs/libguile.md), in the Linux container:
#
#   sh utils/container/run.sh lilypond-time 'sh tests/lilypond/time.sh'
#
# Each of ours is run RUNS times (default 2): the first may fill the
# compiled-module cache, the others load from it.  LilyPond points
# XDG_CACHE_HOME into its installation, which the container forgets
# between runs, so the cache is in /root/.cache (PSEUDOSCHEME_GUILE_CACHE_DIRECTORY
# if set): only a change to the sources makes the first run slow.  With
# COLD=1 it starts empty, to time filling it.  Settings for ours
# (PSEUDOSCHEME_GUILE_*) pass through.  With VERBOSE=1, ours also runs
# once with LilyPond's --verbose, its lines timestamped (verbose.log in
# $LOGS), to show where the start-up goes; with TRACE=1 as well, with
# each call into our libguile listed (PSEUDOSCHEME_LIBGUILE_TRACE).  With
# PROFILE=1, one more run prints the time spent in each libguile function
# (PSEUDOSCHEME_LIBGUILE_PROFILE).
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
PSEUDOSCHEME_GUILE_CACHE_DIRECTORY=${PSEUDOSCHEME_GUILE_CACHE_DIRECTORY:-$HOME/.cache/pseudoscheme/lilypond-guile}
PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY=${PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY:-$HOME/.cache/pseudoscheme/lilypond-libraries}
export PSEUDOSCHEME_GUILE_CACHE_DIRECTORY PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY
[ -n "$COLD" ] && rm -rf "$PSEUDOSCHEME_GUILE_CACHE_DIRECTORY" "$PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY"
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
if [ -n "$PROFILE" ]; then
  SBCL_HOME=${SBCL_HOME:-/usr/local/lib/sbcl} PSEUDOSCHEME_LIBGUILE_PROFILE=${PROFILE_MODE:-1} ours demo.ly > profile.log 2>&1
  grep -v '^\(Processing\|Parsing\|Interpreting\|Preprocessing\|Finding\|Fitting\|Drawing\|Converting\|Success\|Layout\)' profile.log | tail -n +1 | head -${PROFILE_LINES:-200}
fi
