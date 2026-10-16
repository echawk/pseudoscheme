#!/bin/sh
# Extract one of Racket CS's layers as a standalone S-expression linklet,
# the way Racket's own build does (each layer's main.zuo, its NAME.rktl
# target; racket/src/expander/README.txt): the layer's source expanded
# and flattened by the expander's bootstrap-run.rkt.  --racket=linklet
# reads the expander from it (src/racket/linklet/sources.lisp) in place
# of the copy in BC's startup.inc.
#
# Usage: sh utils/racket/extract.sh [LAYER ...]
#   LAYER: expander (the default), thread, io or regexp
# Needs sh utils/racket/setup.sh first, and a `racket` of the pinned
# version (RACKET names another) to run the source: the README requires
# "essentially the same version" as the target.  Writes
# build/external/racket-derived/LAYER.rktl, with LAYER.log beside it.
set -e
# the arguments below hold a ? and are split unquoted: no globbing
set -f
cd "$(dirname "$0")/../.."
RACKET=${RACKET:-racket}
TREE=$PWD/build/external/racket
SRC=$TREE/racket/src
E=$SRC/expander
OUT=$PWD/build/external/racket-derived
[ -d "$E" ] || { echo "No Racket source in $TREE: run utils/racket/setup.sh first." >&2; exit 1; }
v() { sed -n "s/^#define MZSCHEME_VERSION_$1 \([0-9]*\)$/\1/p" "$SRC/version/racket_version.h"; }
want=$(v X).$(v Y)
[ "$(v W)" = 0 ] || want=$want.$(v Z).$(v W)
[ "$(v W)" != 0 ] || [ "$(v Z)" = 0 ] || want=$want.$(v Z)
have=$("$RACKET" -e '(display (version))')
if [ "$want" != "$have" ]; then
  echo "$RACKET is Racket $have; the source is $want.  Set RACKET to a Racket $want." >&2
  exit 1
fi
[ $# -gt 0 ] || set -- expander
mkdir -p "$OUT/cache-src"

# each layer's arguments, as its main.zuo gives them
layer_args() {
  case $1 in
    expander)
      echo ++knot read "$E/read/api.rkt" ++knot read "$E/read/primitive-parameter.rkt" \
           ++knot read "$E/read/readtable-parameter.rkt" ++knot read "$E/read/readtable.rkt" \
           ++knot read "$E/read/number.rkt" \
           ++direct linklet ++direct kernel ++disallow make-optional-keyword-procedure \
           --no-global ++global-ok core-forms ++global-ok core-primitives \
           ++global-ok built-in-symbols ++global-ok default-read-handler \
           ++global-ok current-previously-unbound ++global-ok 'keep-source-locations?' \
           ++global-ok rx:path-list ++global-ok load-configure-expand ;;
    thread)
      echo ++direct pthread ++disallow error --no-global \
           ++global-ok the-sandman ++global-ok post-shutdown-action \
           ++global-ok get-subprocesses-time ++global-ok pre-poll-callbacks \
           ++global-ok queued-shutdowns ++global-ok compute-memory-sizes \
           ++global-ok 'computed-memory-sizes?' ++global-ok custodians-with-limits \
           ++global-ok make-place-ports+fds ++global-ok pthread-count \
           ++global-ok 'logging-future-events?' ++global-ok log-future-event \
           ++global-ok 'logging-place-events?' ++global-ok log-place-event \
           ++global-ok thread-engine-for-roots ++global-ok TICKS ;;
    io)
      echo ++direct thread ++direct pthread --no-global \
           ++global-ok installed-read ++global-ok installed-read-syntax \
           ++global-ok installed-read-accept-reader ++global-ok installed-read-accept-lang \
           ++global-ok maybe-raise-missing-module ++global-ok 'string->number?' \
           ++global-ok 'printable-regexp?' ++global-ok do-global-print \
           ++global-ok simplify-path/dl ++global-ok exec-file ++global-ok run-file \
           ++global-ok collects-dir ++global-ok config-dir ++global-ok addon-dir \
           ++global-ok host-collects-dir ++global-ok host-config-dir \
           ++global-ok host-addon-dir ++global-ok 'saved-error-value->string' ;;
    regexp)
      echo --no-global ;;
    *) echo "unknown layer: $1 (expander, thread, io or regexp)" >&2; return 1 ;;
  esac
}

for layer in "$@"; do
  args=$(layer_args "$layer") || exit 1
  # the expander's standalone flattening keeps its primitives' names (-k)
  echo "Extracting $layer with Racket $have (a few minutes)..."
  # shellcheck disable=SC2086
  "$RACKET" "$E/bootstrap-run.rkt" \
    -t "$SRC/$layer/main.rkt" \
    -c "$OUT/cache-src" \
    --depends "$OUT/$layer-dep.rktd" \
    $args \
    -k "$TREE/racket" \
    -s -x \
    -o "$OUT/$layer.rktl.new" > "$OUT/$layer.log" 2>&1 \
    || { echo "Extracting $layer failed: see $OUT/$layer.log" >&2; exit 1; }
  mv "$OUT/$layer.rktl.new" "$OUT/$layer.rktl"
  echo "Wrote $OUT/$layer.rktl ($(wc -c < "$OUT/$layer.rktl") bytes)."
done
