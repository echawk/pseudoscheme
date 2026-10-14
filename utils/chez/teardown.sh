#!/bin/sh
# Undo utils/chez/setup.sh: remove build/external/chezscheme/.
cd "$(dirname "$0")/../.." || exit 1
rm -rf build/external/chezscheme
echo "Removed build/external/chezscheme."
