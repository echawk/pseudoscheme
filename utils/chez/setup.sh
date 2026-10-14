#!/bin/sh
# Fetch upstream Chez Scheme's source, cisco/ChezScheme at the commit
# vendor/chez-mats/ was taken from, into build/external/chezscheme/
# (gitignored): for telling what upstream Chez has from what only
# Racket's fork has (src/racket/cs/fork.lisp, docs/racket.md "The Chez
# fork's additions").  Only its source is read; nothing is built.
#
# Usage: sh utils/chez/setup.sh
set -e
cd "$(dirname "$0")/../.."
COMMIT=${CHEZ_COMMIT:-e3b13636820b13e0feb82b2fb944b60fab4b6e80}
DIR=build/external/chezscheme
if [ -d "$DIR/.git" ]; then
  have=$(git -C "$DIR" rev-parse HEAD)
  [ "$have" = "$COMMIT" ] && { echo "Chez Scheme $COMMIT is already in $DIR."; exit 0; }
  echo "$DIR holds $have, not $COMMIT; run utils/chez/teardown.sh first." >&2
  exit 1
fi
mkdir -p "$DIR"
git -C "$DIR" init -q
git -C "$DIR" remote add origin https://github.com/cisco/ChezScheme
git -C "$DIR" fetch -q --depth 1 origin "$COMMIT"
git -C "$DIR" checkout -q FETCH_HEAD
echo "Chez Scheme $COMMIT is in $DIR."
