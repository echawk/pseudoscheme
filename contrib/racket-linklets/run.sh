#!/bin/sh
# Run Racket CS's schemified linklets on Pseudoscheme's Chez mode
# (README.md): Racket's own regexp tests on its regexp linklet, then
# its schemify linklet schemifying a linklet, and that run here too.
#
# Usage: contrib/racket-linklets/run.sh RACKET-SOURCE-TREE
#   (a checkout of racket/racket; its racket/src/cs/schemified/ holds
#   the schemified linklets)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
racket=$(cd "${1:?usage: run.sh RACKET-SOURCE-TREE}" && pwd)
cs=$racket/racket/src/cs
chez=${CHEZ:-$root/bin/chez}
work=$(mktemp -d /tmp/racket-linklets-XXXXXX)
trap 'rm -rf "$work"' EXIT
for f in racket-lite.sls load-linklet.ss rx-tests.ss schemify-demo.ss; do ln -s "$here/$f" "$work/"; done
for f in regexp known schemify; do ln -s "$cs/schemified/$f.scm" "$work/"; done
ln -s "$cs/primitive" "$work/primitive"
cd "$work"
python3 "$here/extract-rx-tests.py" "$racket/pkgs/racket-test-core/tests/racket/rx.rktl"
echo "== Racket's regexp tests on its schemified regexp linklet"
"$chez" --script load-linklet.ss regexp.scm '(load "rx-tests.ss")'
echo "== Racket's schemify linklet, schemifying a linklet here"
"$chez" --script load-linklet.ss regexp.scm '(load-linklet "known.scm")' '(load-linklet "schemify.scm")' '(load "schemify-demo.ss")'
