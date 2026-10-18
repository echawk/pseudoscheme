#!/bin/sh
# Fetch Racket's source for --racket (docs/racket.md): a shallow clone of
# racket/racket at the pinned release tag, into build/external/racket/
# (gitignored).  Nothing else of the machine's is used; an installed
# Racket is consulted only through its executable, at run time.
#
# Usage: sh utils/racket/setup.sh [--typed]
# --typed also runs utils/racket/packages.sh (Typed Racket's packages, into
# build/external/racket-pkgs/).
# RACKET_TAG overrides the pinned tag, for trying a newer release.
set -e
TYPED=
for a; do [ "$a" = --typed ] && TYPED=1; done
cd "$(dirname "$0")/../.."
# librktio, for the REPL's terminal (and what io asks rktio): a C compiler
# is all it takes; without one the REPL falls back to the host's streams.
typed() {
  [ -z "$TYPED" ] || sh utils/racket/packages.sh
}
rktio() {
  sh utils/racket/rktio.sh || echo "(librktio not built: run utils/racket/rktio.sh when there is a C compiler)" >&2
}
TAG=${RACKET_TAG:-v9.3}
# the commit the tag names, checked after the clone
COMMIT=${RACKET_COMMIT:-bbf7dea831df95922db23aaa51e13eed17dcf881}
DIR=build/external/racket
if [ -d "$DIR/.git" ]; then
  have=$(git -C "$DIR" rev-parse HEAD)
  [ "$have" = "$COMMIT" ] && { echo "Racket $TAG is already in $DIR."; rktio; typed; exit 0; }
  echo "$DIR holds $have, not $TAG; run utils/racket/teardown.sh first." >&2
  exit 1
fi
mkdir -p build/external
git clone --depth 1 --branch "$TAG" https://github.com/racket/racket "$DIR"
have=$(git -C "$DIR" rev-parse HEAD)
if [ -z "$RACKET_TAG" ] && [ "$have" != "$COMMIT" ]; then
  echo "$TAG is $have, not the pinned $COMMIT: the tag moved?" >&2
  exit 1
fi
echo "Racket $TAG ($have) is in $DIR."
rktio
typed
