#!/bin/sh
# Rename Pseudoscheme to Quasischeme (docs/rename-quasischeme.md).
#
#   sh utils/rename/rename.sh            dry run: what would change
#   sh utils/rename/rename.sh --apply    rewrite tracked files, git mv
#
# Rewrites tracked text files only (never caches, fasls or untracked
# files), skipping utils/rename/allowlist.txt's paths; then moves the
# files whose names hold the old name, and src/*.pso to src/*.qso.
# Prose about Rees' original (README's opening, "Pseudoscheme 2.13" and
# the like) is left for review by hand: check.sh lists what remains.
set -e
cd "$(dirname "$0")/../.."
apply=no
[ "$1" = --apply ] && apply=yes
[ -z "$(git status --porcelain --untracked-files=no)" ] || [ $apply = no ] \
  || { echo "commit or stash your changes first" >&2; exit 1; }
excludes=$(grep -v '^#' utils/rename/allowlist.txt | sed 's/^/:!/')
# shellcheck disable=SC2086
files=$(git grep -lI -i -E '"PS"|"PS-|pseudo|(^|[^A-Za-z0-9_*%-])ps(r|x|l|g|ffi|-lisp|-r[67]rs|-repl)?::?|\.pso\b|"pso"' -- . $excludes)
echo "$(echo "$files" | wc -l | tr -d ' ') files to rewrite"
if [ $apply = no ]; then
  echo "$files" | head -20; echo "..."
else
  # longest patterns first; word boundaries by lookarounds
  echo "$files" | xargs perl -pi -e '
    s/PSEUDOSCHEME/QUASISCHEME/g;
    s/Pseudoscheme(?= (?:2\.\d|was |1\.\d))/PSEUDO_HISTORY/g;   # keep history
    s/Pseudoscheme/Quasischeme/g;
    s/PSEUDO_HISTORY/Pseudoscheme/g;
    s/pseudoscheme/quasischeme/g;
    s/(?<![\w*%-])(ps|PS)-(lisp|LISP|r6rs|R6RS|r7rs|R7RS|repl|REPL)\b/ ($1 eq "ps" ? "qs" : "QS")."-$2" /ge;
    s/(?<![\w*%-])(psr|psx|psl|psg|psffi)(::?)/ "qs".substr($1,2).$2 /ge;
    s/(?<![\w*%-])(PSR|PSX|PSL|PSG|PSFFI)\b/ "QS".substr($1,2) /ge;
    s/(?<![\w*%-])ps(::?)(?=[\w*%+&<>=!?\/-])/qs$1/g;
    s/(?<![\w*%-])PS(::?)(?=[\w*%+&<>=!?\/-])/QS$1/g;
    s/"PS"/"QS"/g;
    s/\.pso\b/.qso/g;
    s/"pso"/"qso"/g;
  '
fi
# files named for it, and the .pso files
git ls-files | grep -i pseudoscheme | while read -r f; do
  new=$(echo "$f" | sed 's/pseudoscheme/quasischeme/g; s/Pseudoscheme/Quasischeme/g')
  echo "git mv $f $new"
  # a renamed directory (.claude/skills/run-pseudoscheme/) must exist first
  if [ $apply = yes ]; then mkdir -p "$(dirname "$new")"; git mv "$f" "$new"; fi
done
git ls-files 'src/*.pso' | while read -r f; do
  echo "git mv $f ${f%.pso}.qso"
  if [ $apply = yes ]; then git mv "$f" "${f%.pso}.qso"; fi
done
if [ $apply = yes ]; then echo "Now: sh utils/rename/check.sh, then the tests (docs/rename-quasischeme.md, section 6).";  fi
exit 0
