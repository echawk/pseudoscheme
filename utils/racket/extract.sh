#!/bin/sh
# Extract Racket's expander as one standalone S-expression linklet, the
# way Racket's own build does for Racket CS (racket/src/expander/README.txt
# and main.zuo, target compiled/expander.rktl): racket/src/expander/
# expanded and flattened by bootstrap-run.rkt.  --racket=linklet reads
# the result (src/racket/linklet/sources.lisp) in place of the copy in
# BC's startup.inc.
#
# Usage: sh utils/racket/extract.sh
# Needs sh utils/racket/setup.sh first, and a `racket` of the pinned
# version (RACKET names another) to run the expander's source: the
# README requires "essentially the same version" as the target.  Writes
# build/external/racket-derived/expander.rktl, with the log beside it.
set -e
cd "$(dirname "$0")/../.."
RACKET=${RACKET:-racket}
TREE=$PWD/build/external/racket
E=$TREE/racket/src/expander
OUT=$PWD/build/external/racket-derived
[ -d "$E" ] || { echo "No Racket source in $TREE: run utils/racket/setup.sh first." >&2; exit 1; }
v() { sed -n "s/^#define MZSCHEME_VERSION_$1 \([0-9]*\)$/\1/p" "$TREE/racket/src/version/racket_version.h"; }
want=$(v X).$(v Y)
[ "$(v W)" = 0 ] || want=$want.$(v Z).$(v W)
[ "$(v W)" != 0 ] || [ "$(v Z)" = 0 ] || want=$want.$(v Z)
have=$("$RACKET" -e '(display (version))')
if [ -n "$want" ] && [ "$want" != "$have" ]; then
  echo "$RACKET is Racket $have; the source is $want.  Set RACKET to a Racket $want." >&2
  exit 1
fi
mkdir -p "$OUT/cache-src"
echo "Extracting the expander with Racket $have (a few minutes)..."
# main.zuo's arguments for the standalone expander.rktl
"$RACKET" "$E/bootstrap-run.rkt" \
  -t "$E/main.rkt" \
  -c "$OUT/cache-src" \
  --depends "$OUT/expander-dep.rktd" \
  ++knot read "$E/read/api.rkt" \
  ++knot read "$E/read/primitive-parameter.rkt" \
  ++knot read "$E/read/readtable-parameter.rkt" \
  ++knot read "$E/read/readtable.rkt" \
  ++knot read "$E/read/number.rkt" \
  ++direct linklet ++direct kernel \
  ++disallow make-optional-keyword-procedure \
  --no-global \
  ++global-ok core-forms \
  ++global-ok core-primitives \
  ++global-ok built-in-symbols \
  ++global-ok default-read-handler \
  ++global-ok current-previously-unbound \
  ++global-ok 'keep-source-locations?' \
  ++global-ok rx:path-list \
  ++global-ok load-configure-expand \
  -k "$TREE/racket" \
  -s -x \
  -o "$OUT/expander.rktl.new" > "$OUT/extract.log" 2>&1 \
  || { echo "Extraction failed: see $OUT/extract.log" >&2; exit 1; }
mv "$OUT/expander.rktl.new" "$OUT/expander.rktl"
echo "Wrote $OUT/expander.rktl ($(wc -c < "$OUT/expander.rktl") bytes)."
