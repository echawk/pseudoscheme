#!/bin/sh
# Fetch extra Racket packages (Typed Racket and what it needs) for --racket:
# shallow clones of the packages' github.com/racket repositories at the
# tag of the pinned release, into build/external/racket-pkgs/REPO/
# (gitignored).  Packages that Racket's own repository holds
# (build/external/racket/pkgs) are skipped.  Dependencies are resolved
# transitively from each package's info.rkt `deps`.
#
# Usage: sh utils/racket/packages.sh [PACKAGE...]
# Default: typed-racket-lib (typed/racket and typed/racket/base are in it;
# typed-racket-more adds typed/net, typed/db, ... and pulls in much more).
# RACKET_TAG overrides the tag (default v9.3), as in setup.sh.
# Makes the collections visible to --racket through
# build/external/racket/racket/etc/config.rktd (see the end of this file);
# the roots are also written, as a PLTCOLLECTS value, to
# build/external/racket-pkgs/pltcollects.
set -e
cd "$(dirname "$0")/../.."
TAG=${RACKET_TAG:-v9.3}
CORE=build/external/racket/pkgs
OUT=build/external/racket-pkgs
[ -d "$CORE" ] || { echo "run utils/racket/setup.sh first" >&2; exit 1; }
mkdir -p "$OUT"
[ $# -gt 0 ] || set -- typed-racket-lib

# The repository holding a package, when not named after it (minus -lib,
# -doc, -test, ...)
repo_of() {
  case "$1" in
    source-syntax|typed-racket*) echo typed-racket ;;
    srfi*) echo srfi ;;
    scheme-lib) echo scheme-lib ;;
    mzscheme-lib) echo mzscheme ;;
    *) echo "$1" | sed -e 's/-lib$//' -e 's/-doc$//' -e 's/-test$//' ;;
  esac
}

# The dependencies named by a package's info.rkt: the strings of its
# `(define deps ...)` form, less keyword arguments' values
deps_of() {
  awk '
    /^\(define (build-deps|deps)/ { on = ($2 == "deps"); depth = 0 }
    on {
      line = $0; sub(/;.*/, "", line)
      gsub(/#:(version|platform) +("[^"]*"|\(.*\)|[^ )]+)/, "", line)
      gsub(/#:version +"[^"]*"/, "", line)
      n = split(line, ch, "")
      for (i = 1; i <= n; i++) { if (ch[i]=="(") depth++; else if (ch[i]==")") depth-- }
      while (match(line, /"[^"]*"/)) {
        print substr(line, RSTART + 1, RLENGTH - 2); line = substr(line, RSTART + RLENGTH)
      }
      if (depth <= 0 && NR > 0 && $0 ~ /\)/) on = 0
    }' "$1"
}

fetch_repo() {
  r=$1
  [ -d "$OUT/$r/.git" ] && return 0
  echo "fetching racket/$r at $TAG"
  git clone -q --depth 1 --branch "$TAG" "https://github.com/racket/$r" "$OUT/$r" 2>/dev/null \
    || { rm -rf "$OUT/$r"; echo "no racket/$r at $TAG (package $2 unmapped: add it to repo_of)" >&2; exit 1; }
  echo "  $(git -C "$OUT/$r" rev-parse HEAD)"
}

seen=" "
roots=
queue="$*"
while [ -n "$queue" ]; do
  set -- $queue; p=$1; shift; queue="$*"
  case "$seen" in *" $p "*) continue ;; esac
  seen="$seen$p "
  [ -f "$CORE/$p/info.rkt" ] && continue
  # not under github.com/racket, and unneeded by typed/racket/base (only
  # typed/net/cookies uses it): skipped rather than unpinned
  case "$p" in net-cookies*) echo "skipping $p (not a racket/ repository)"; continue ;; esac
  r=$(repo_of "$p")
  fetch_repo "$r" "$p"
  # a repository holds packages in subdirectories, or is one itself
  d=$OUT/$r/$p
  [ -f "$d/info.rkt" ] || d=$OUT/$r
  [ -f "$d/info.rkt" ] || { echo "$p isn't in racket/$r" >&2; exit 1; }
  if grep -q "(define collection 'multi)" "$d/info.rkt"; then
    roots="$roots:$(pwd)/$d"
  else
    # a single-collection package: its collection is named by the package,
    # so a directory of links makes it visible beside the others
    # (`(define collection "syntax")`: source-syntax adds a file to syntax/)
    c=$(sed -n 's/^(define collection "\(.*\)").*/\1/p' "$d/info.rkt")
    mkdir -p "$OUT/single/$p"
    ln -sfn "$(pwd)/$d" "$OUT/single/$p/${c:-$p}"
    case "$roots" in *"$(pwd)/$OUT/single/$p"*) ;; *) roots="$roots:$(pwd)/$OUT/single/$p" ;; esac
  fi
  queue="$queue $(deps_of "$d/info.rkt" | tr '\n' ' ')"
done

printf '%s\n' "$(pwd)/build/external/racket/racket/collects$roots" > "$OUT/pltcollects"
# --racket reads PLTCOLLECTS only with the user-specific search paths on,
# which it turns off.  The source tree's config file (racket/etc/config.rktd,
# found as `config-dir`) is read either way, so it names the new roots.
ETC=build/external/racket/racket/etc
mkdir -p "$ETC"
dirs=$(echo "$roots" | tr ':' '\n' | sed -e '/^$/d' -e 's/.*/"&"/' | tr '\n' ' ')
printf '#hash((collects-search-dirs . (#f %s)))\n' "$dirs" > "$ETC/config.rktd"
echo "wrote $ETC/config.rktd"
