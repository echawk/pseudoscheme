#!/usr/bin/env bash
# Regenerate vendor/psyntax/psyntax-pseudoscheme.pp, psyntax's expanded
# image, from psyntax's sources.  See boot/README.md.
#
# Usage: boot/psyntax.sh [--no-install]
#
#   1. Seed: an image built from the sources by a Scheme with native R6RS
#      libraries and syntax-case, which runs psyntax-buildscript.ss
#      directly.  Chez Scheme does (boot/psyntax/chez/).
#      PSYNTAX_SEED=<file.pp> uses that image instead: one built from
#      these sources, e.g. a previous vendor/psyntax/psyntax-pseudoscheme.pp.
#      (Not vendor/psyntax/pre-built/psyntax-scheme48.pp: the sources
#      have outgrown it; it lacks the lisp-keyword? primitive compat.ss
#      imports.)
#   2. Pseudoscheme, loaded from src/ (so run boot/bootstrap.sh first
#      for .pso files made from source), rebuilds psyntax with the seed,
#      then with its own result, until it reproduces itself
#      (boot/psyntax.lisp).
#   3. That image is copied to vendor/psyntax/, unless --no-install.
#
# Work happens in boot/build/psyntax/, with logs alongside; the result
# is boot/build/psyntax/psyntax-pseudoscheme.pp.

set -eu

cd "$(dirname "$0")/.."

SBCL=${SBCL:-sbcl}
CHEZ=${CHEZ:-chez}
build=boot/build/psyntax

install=yes
for arg in "$@"; do
    case "$arg" in
	--no-install) install=no ;;
	*) echo "usage: $0 [--no-install]" >&2; exit 2 ;;
    esac
done

mkdir -p "$build"

# A copy of the sources to run the build script in: it reads psyntax/*.ss
# and writes psyntax-pseudoscheme.pp in the current directory.
copy_sources() {
    rm -rf "$1"
    mkdir -p "$1"
    cp -R vendor/psyntax/psyntax vendor/psyntax/psyntax-buildscript.ss "$1/"
}

seed=${PSYNTAX_SEED:-}
if [ -z "$seed" ]; then
    command -v "$CHEZ" >/dev/null 2>&1 || {
	echo "psyntax: no Chez Scheme ($CHEZ) to build the seed image with;" >&2
	echo "  set CHEZ, or PSYNTAX_SEED to an image built from these sources" >&2
	exit 1
    }
    dir=$build/chez
    printf 'Building a psyntax seed image with Chez Scheme ... '
    copy_sources "$dir"
    libdirs="$PWD/$dir:$PWD/boot/psyntax/chez"
    if ! (cd "$dir" && "$CHEZ" --libdirs "$libdirs" --program psyntax-buildscript.ss) \
	 >"$build/chez.log" 2>&1 || [ ! -s "$dir/psyntax-pseudoscheme.pp" ]; then
	echo "failed; see $build/chez.log"
	exit 1
    fi
    echo ok
    seed=$dir/psyntax-pseudoscheme.pp
fi

printf 'Rebuilding psyntax on Pseudoscheme until it reproduces itself ... '
if ! "$SBCL" --dynamic-space-size 4GB --control-stack-size 500MB \
     --script boot/psyntax.lisp "$seed" >"$build/pseudoscheme.log" 2>&1; then
    echo "failed; see $build/pseudoscheme.log"
    exit 1
fi
image=$(sed -n 's/^Stage [0-9]* reproduced stage [0-9]*: //p' "$build/pseudoscheme.log")
cp "$image" "$build/psyntax-pseudoscheme.pp"
echo "ok ($image)"

if [ "$install" = yes ]; then
    cp "$image" vendor/psyntax/psyntax-pseudoscheme.pp
    echo "Installed it as vendor/psyntax/psyntax-pseudoscheme.pp."
fi
