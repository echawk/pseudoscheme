#!/bin/sh
# Undo utils/racket/setup.sh: remove build/external/racket/.
cd "$(dirname "$0")/../.." || exit 1
rm -rf build/external/racket
echo "Removed build/external/racket."
