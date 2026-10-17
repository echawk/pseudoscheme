#!/bin/sh
# Undo utils/racket/setup.sh: remove build/external/racket/ and build/external/rktio/.
cd "$(dirname "$0")/../.." || exit 1
rm -rf build/external/racket build/external/rktio
echo "Removed build/external/racket and build/external/rktio."
