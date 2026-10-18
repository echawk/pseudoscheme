#!/bin/sh
# LilyPond on our libguile (docs/libguile.md), in the Linux container:
#
#   sh utils/container/run.sh lilypond 'sh tests/lilypond/run.sh'
#
# The image's LilyPond (/opt/lilypond, built against Guile 3.0.11) is run
# on a tiny score:
#   1. `reference`: on real Guile; it makes demo.pdf.
#   2. `ours`: the same binary with LD_LIBRARY_PATH on our libguile
#      (build/libguile), which starts Lisp on pseudoscheme-guile.core.
# Each step's output is in $LOGS (/logs), as is how far ours got.  Ours
# compiles Guile's and LilyPond's modules the first time (some 35 s) into
# a cache in /root/.cache, as LilyPond's own XDG_CACHE_HOME is in its
# installation, which the container forgets.
cd "$(dirname "$0")/../.." || exit 1
LOGS=${LOGS:-/logs}
LILYPOND=/opt/lilypond/bin/lilypond
[ -x "$LILYPOND" ] || { echo "LilyPond isn't in the image (utils/container/Containerfile: LILYPOND_SHA256)"; exit 1; }

make -C src/libguile BUILD="$PWD/build/libguile" > "$LOGS/libguile-build.log" 2>&1 \
  || { echo "libguile: build failed (libguile-build.log)"; exit 1; }
cp src/libguile/missing.lisp "$LOGS/"
make -C src/libguile core BUILD="$PWD/build/libguile" > "$LOGS/core-build.log" 2>&1 \
  || { echo "core: build failed (core-build.log)"; exit 1; }

PSEUDOSCHEME_GUILE_CACHE_DIRECTORY=${PSEUDOSCHEME_GUILE_CACHE_DIRECTORY:-$HOME/.cache/pseudoscheme/lilypond-guile}
PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY=${PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY:-$HOME/.cache/pseudoscheme/lilypond-libraries}
export PSEUDOSCHEME_GUILE_CACHE_DIRECTORY PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY
mkdir -p "$LOGS/reference" "$LOGS/ours"
cp tests/lilypond/demo.ly "$LOGS/reference/" && cp tests/lilypond/demo.ly "$LOGS/ours/"

echo "reference: real Guile"
(cd "$LOGS/reference" && LD_LIBRARY_PATH=/opt/guile/lib "$LILYPOND" demo.ly) > "$LOGS/reference.log" 2>&1
echo "  exit $?; $(ls "$LOGS/reference"/*.pdf 2>/dev/null | wc -l) pdf"

echo "ours: LD_LIBRARY_PATH=build/libguile"
(cd "$LOGS/ours" && PSEUDOSCHEME_LIBGUILE_TRACE=${TRACE:-0} PSEUDOSCHEME_LIBGUILE_TRACE_AFTER=${TRACE_AFTER:-0} LD_LIBRARY_PATH=$OLDPWD/build/libguile \
   PSEUDOSCHEME_GUILE_CORE=$OLDPWD/build/libguile/pseudoscheme-guile.core \
   timeout "${STEP_LIMIT:-600}" "$LILYPOND" demo.ly) > "$LOGS/ours.log" 2>&1
status=$?
echo "  exit $status; $(ls "$LOGS/ours"/*.pdf 2>/dev/null | wc -l) pdf"
tail -5 "$LOGS/ours.log"
# the same page: ghostscript's rendering of each PDF, compared
if [ "$status" = 0 ] && [ -f "$LOGS/ours/demo.pdf" ]; then
  for d in reference ours; do
    gs -q -dNOPAUSE -dBATCH -sDEVICE=pgmraw -r50 -sOutputFile="$LOGS/$d/demo.pgm" "$LOGS/$d/demo.pdf" > /dev/null 2>&1
  done
  if cmp -s "$LOGS/reference/demo.pgm" "$LOGS/ours/demo.pgm"; then
    echo "  the page is the same as with real Guile's"
  else
    echo "  the page differs from real Guile's (demo.pgm in each)"
    status=1
  fi
fi
[ "$status" = 0 ]
