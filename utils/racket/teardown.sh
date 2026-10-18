#!/bin/sh
# Undo utils/racket/setup.sh: remove build/external/racket/, build/external/racket-pkgs/ and build/external/rktio/.
cd "$(dirname "$0")/../.." || exit 1
rm -rf build/external/racket build/external/rktio build/external/racket-pkgs
echo "Removed build/external/racket, build/external/racket-pkgs and build/external/rktio."
