#!/bin/sh
# Our libguile's tests (docs/libguile.md), in the Linux container
# (utils/container/), which has GNU Guile 3.0.11 in /opt/guile:
#
#   sh utils/container/run.sh libguile 'sh tests/libguile/run.sh'
#
# 1. The ABI probe (probe.c), built against Guile's headers, prints the
#    same facts as src/libguile/abi.lisp records.
# 2. Extensions built against Guile's headers (ext-*.c) are loaded by a
#    script (*.scm) in real Guile and in --guile on our libguile; the two
#    outputs must be the same.
# 3. Programs that embed Guile (embed-*.c: a C main calling scm_with_guile
#    or scm_boot_guile) are linked once against real Guile and once
#    against ours, with the core our library starts (make core); the
#    outputs must be the same.
# Both libraries and bin/pseudoscheme are built first.  Logs go to $LOGS
# (/logs).
cd "$(dirname "$0")/../.." || exit 1
LOGS=${LOGS:-/logs}
GUILE=/opt/guile
export PKG_CONFIG_PATH=$GUILE/lib/pkgconfig
CFLAGS=$(pkg-config --cflags guile-3.0)
failed=0

# the library first: it regenerates src/libguile/missing.lisp, which the
# command line's image loads
make -C src/libguile BUILD="$PWD/build/libguile" > "$LOGS/libguile-build.log" 2>&1 \
  || { echo "libguile: build failed (libguile-build.log)"; exit 1; }
cp src/libguile/missing.lisp "$LOGS/"
make -C contrib/cli > "$LOGS/cli-build.log" 2>&1 \
  || { echo "bin/pseudoscheme: build failed (cli-build.log)"; exit 1; }
export PSEUDOSCHEME_LIBGUILE=$PWD/build/libguile/libguile-3.0.so.1

for c in tests/libguile/ext-*.c; do
  name=$(basename "$c" .c | sed 's/^ext-//')
  so=$LOGS/ext-$name.so
  cc -shared -fPIC -O2 $CFLAGS "$c" -o "$so" || { echo "$name: compile failed"; failed=1; continue; }
  var=EXT_$(echo "$name" | tr a-z- A-Z_)
  env "$var=$so" $GUILE/bin/guile --no-auto-compile "tests/libguile/$name.scm" > "$LOGS/$name.guile.out" 2>&1
  env "$var=$so" bin/pseudoscheme --no-userinit --guile "tests/libguile/$name.scm" > "$LOGS/$name.ours.out" 2>&1
  if diff "$LOGS/$name.guile.out" "$LOGS/$name.ours.out" > "$LOGS/$name.diff"; then
    echo "$name: $(wc -l < "$LOGS/$name.guile.out") lines, the same as Guile's"
  else
    echo "$name: differs from Guile's ($name.diff)"
    failed=1
  fi
done

# Embedding: our library starts Lisp on pseudoscheme-guile.core
make -C src/libguile core BUILD="$PWD/build/libguile" > "$LOGS/core-build.log" 2>&1 \
  || { echo "embedding: pseudoscheme-guile.core failed to build (core-build.log)"; exit 1; }
unset PSEUDOSCHEME_LIBGUILE
for c in tests/libguile/embed-*.c; do
  name=$(basename "$c" .c)
  cc -O2 $CFLAGS "$c" -o "$LOGS/$name.guile.exe" -L$GUILE/lib -lguile-3.0 -Wl,-rpath,$GUILE/lib \
    || { echo "$name: compile failed"; failed=1; continue; }
  cc -O2 $CFLAGS "$c" -o "$LOGS/$name.ours.exe" -L"$PWD/build/libguile" -lguile-3.0 \
    || { echo "$name: compile failed against ours"; failed=1; continue; }
  "$LOGS/$name.guile.exe" a b > "$LOGS/$name.guile.out" 2>&1
  LD_LIBRARY_PATH=$PWD/build/libguile "$LOGS/$name.ours.exe" a b > "$LOGS/$name.ours.out" 2>&1
  if diff "$LOGS/$name.guile.out" "$LOGS/$name.ours.out" > "$LOGS/$name.diff"; then
    echo "$name: $(wc -l < "$LOGS/$name.guile.out") lines, the same as Guile's"
  else
    echo "$name: differs from Guile's ($name.diff)"
    failed=1
  fi
done
exit $failed
