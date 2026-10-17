#!/bin/sh
# Build librktio, Racket's portable operating-system layer (docs/racket.md,
# "The REPL"), as a shared library from racket/src/rktio/ in the tree
# utils/racket/setup.sh fetched: rktio's own configure (--enable-standalone,
# which makes a shared library possible) and make, in build/external/rktio/
# (gitignored).  src/racket/native/rktio.lisp binds it with CFFI.
#
# Usage: sh utils/racket/rktio.sh
# Needs a C compiler and make.
set -e
cd "$(dirname "$0")/../.."
SRC=$PWD/build/external/racket/racket/src/rktio
OUT=build/external/rktio
[ -f "$SRC/configure" ] || { echo "No Racket source in build/external/racket: run utils/racket/setup.sh first." >&2; exit 1; }
case $(uname -s) in Darwin) SFX=dylib ;; *) SFX=so ;; esac
mkdir -p "$OUT"
cd "$OUT"
[ -f Makefile ] || "$SRC/configure" --enable-standalone > configure.log 2>&1 \
  || { tail -20 configure.log >&2; exit 1; }
make "librktio.$SFX" > make.log 2>&1 || { tail -20 make.log >&2; exit 1; }
echo "librktio.$SFX is in build/external/rktio."
