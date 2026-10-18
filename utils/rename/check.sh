#!/bin/sh
# What the rename left: every remaining mention of the old name outside
# utils/rename/allowlist.txt's paths, and package prefixes that look
# like the old ones.  Expect only history prose (README's opening and
# the like); review each line.
cd "$(dirname "$0")/../.."
excludes=$(grep -v '^#' utils/rename/allowlist.txt | sed 's/^/:!/')
echo "== mentions of the old name"
# shellcheck disable=SC2086
git grep -nI -i pseudoscheme -- . $excludes
echo "== old package prefixes"
# shellcheck disable=SC2086
git grep -nI -E '"PS"|"PS-|(^|[^A-Za-z0-9_*%-])(ps|psr|psx|psl|psg|psffi|ps-lisp|ps-r[67]rs)::?[A-Za-z*%]' -- . $excludes | head -50
echo "== .pso"
git ls-files 'src/*.pso'
