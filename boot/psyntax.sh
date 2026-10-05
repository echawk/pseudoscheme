#!/usr/bin/env bash
# Regenerate vendor/psyntax/psyntax-pseudoscheme.pp, psyntax's expanded
# image, from psyntax's sources.  See boot/README.md.
#
# Usage: boot/psyntax.sh [--no-install] [--seed=chez|stage0]
#
#   1. Seed: an image built from the sources by another Scheme, which
#      runs psyntax-buildscript.ss:
#      --seed=chez (the default): Chez Scheme, which has R6RS libraries
#        and syntax-case natively and loads psyntax's sources as they are
#        (boot/psyntax/chez/);
#      --seed=stage0: a Scheme without R6RS, R5RS or R7RS-small,
#        $STAGE0_HOST (chibi, the default; gauche; scheme48), through
#        boot/stage0/, which flattens psyntax's libraries and expands
#        their macros itself.
#      The seeds differ, but step 2 rebuilds until the image reproduces
#      itself, so every seed must end in the same image.
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

STAGE0_HOST=${STAGE0_HOST:-chibi}

install=yes
seed_kind=chez
for arg in "$@"; do
    case "$arg" in
	--no-install) install=no ;;
	--seed=chez) seed_kind=chez ;;
	--seed=stage0) seed_kind=stage0 ;;
	*) echo "usage: $0 [--no-install] [--seed=chez|stage0]" >&2; exit 2 ;;
    esac
done

# Run in the seed's work directory, which holds a copy of boot/stage0/
# as stage0/.
stage0_command() {
    case "$1" in
	chibi)    echo "chibi-scheme stage0/hosts/chibi.scm" ;;
	gauche)   echo "gosh stage0/hosts/gauche.scm" ;;
	scheme48) echo "scheme48 -h 0 < stage0/hosts/scheme48.scm" ;;
	*) echo "STAGE0_HOST=$1: unknown (try chibi, gauche or scheme48)" >&2; exit 2 ;;
    esac
}

mkdir -p "$build"

# A copy of the sources to run the build script in: it reads psyntax/*.ss
# and writes psyntax-pseudoscheme.pp in the current directory.
copy_sources() {
    rm -rf "$1"
    mkdir -p "$1"
    cp -R vendor/psyntax/psyntax vendor/psyntax/psyntax-buildscript.ss "$1/"
}

seed=${PSYNTAX_SEED:-}
if [ -z "$seed" ] && [ "$seed_kind" = stage0 ]; then
    command=$(stage0_command "$STAGE0_HOST")
    command -v "${command%% *}" >/dev/null 2>&1 || {
	echo "psyntax: no ${command%% *} for STAGE0_HOST=$STAGE0_HOST" >&2
	exit 1
    }
    dir=$build/stage0-$STAGE0_HOST
    printf 'Building a psyntax seed image with stage0 on %s ... ' "$STAGE0_HOST"
    copy_sources "$dir"
    cp -R boot/stage0 "$dir/stage0"
    if ! (cd "$dir" && sh -c "$command") >"$build/stage0-$STAGE0_HOST.log" 2>&1 \
	 || [ ! -s "$dir/psyntax-pseudoscheme.pp" ]; then
	echo "failed; see $build/stage0-$STAGE0_HOST.log"
	exit 1
    fi
    echo ok
    seed=$dir/psyntax-pseudoscheme.pp
fi
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
