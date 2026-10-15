#!/bin/sh
# GNU Guix on --guile, in the Linux container (utils/container/):
#
#   sh utils/container/run.sh guix 'sh tests/guix/run.sh STEP ...'
#
# Steps, each logging to /logs/STEP.log (build/container/logs/guix/ on
# the host) and appending a line to /logs/summary.txt:
#   build       build bin/pseudoscheme and our libguile
#   reference   real Guix (the store's Guile 3.0.9): `guix build -d
#               --no-grafts -e PACKAGE', and `guix build -e PACKAGE' from
#               substitutes
#   modules     load each of Guix's modules on --guile, one process each
#               (MODULES="guix/gexp.scm gnu/packages/base.scm" for some)
#   derivation  `guix build -d --no-grafts -e PACKAGE' on --guile,
#               compared with the reference's .drv
#   hello       `guix build -e PACKAGE' on --guile, then run hello
#   tests       Guix's own test suite (its tests/*.scm), each file on
#               --guile (TESTS="base32 records" for some), and on real
#               Guile for comparison
# guix-daemon is started first, if it isn't running.  LIMIT (seconds,
# default 600) bounds each process.
#
# PACKAGE is "(@ (gnu packages base) hello)": by name (`guix build
# hello'), Guix looks through every package module, all 585 of them, for
# want of a package cache in this profile, which cold takes hours here;
# by module, it loads only hello's dependencies.  Real Guix computes the
# same derivation either way.
#
# Guix's modules are compiled the first time they load, into the cache
# volume, and a killed run keeps what it finished.  The cache is keyed
# by the build signature (src/library-cache.lisp), which changes when a
# file under src/guile/ (and the other directories it lists) changes:
# then the first run compiles everything again, which takes longer than
# the default LIMIT for derivation and hello.
cd "$(dirname "$0")/../.." || exit 1
LOGS=${LOGS:-/logs}
LIMIT=${LIMIT:-600}
PROFILE=/root/.config/guix/current
GUIX_SITE=$PROFILE/share/guile/site/3.0
GUIX_SOURCE=/opt/src/guix-1.5.0
export GUIX_SCRIPT=$PROFILE/bin/guix
export GUILE_LOAD_PATH=$GUIX_SITE
mkdir -p "$LOGS"

note() { echo "$*" | tee -a "$LOGS/summary.txt"; }

# Run with a time limit; the exit status is 137 if it was killed
limit() {
  "$@" & pid=$!
  ( sleep "$LIMIT"; kill -9 $pid 2>/dev/null ) > /dev/null 2>&1 & w=$!
  wait $pid; st=$?
  kill $w 2>/dev/null
  return $st
}

start_daemon() {
  if ! pgrep -x guix-daemon > /dev/null; then
    guix-daemon --build-users-group=guixbuild > "$LOGS/guix-daemon.log" 2>&1 &
    for i in 1 2 3 4 5 6 7 8 9 10; do
      [ -S /var/guix/daemon-socket/socket ] && break
      sleep 1
    done
  fi
}

# Run with the limit, logging to LOG with the command, its exit status
# and how long it took
logged() {
  log=$1; shift
  echo "# $*" > "$log"
  start=$(date +%s)
  limit "$@" >> "$log" 2>&1
  st=$?
  echo "# exit $st, $(( $(date +%s) - start ))s" >> "$log"
  return $st
}

# Escape-only continuations: with full ones (the default elsewhere), the
# continuation transformation makes a package definition's code about ten
# times bigger (each call of a record constructor is a capture site), and
# compiling Guix's package modules takes hours; guix build doesn't re-enter
# a continuation.  CONTINUATIONS=full for the other.
CONTINUATIONS=${CONTINUATIONS:-escape}
PS="bin/pseudoscheme --no-userinit --continuations=$CONTINUATIONS --guile"
PACKAGE=${PACKAGE:-"(@ (gnu packages base) hello)"}

step_build() {
  # an 8 GB heap (of the machine's 12): hello's dependencies, some 525
  # of Guix's modules, peak at about 3 GB in use; with 11 GB, SBCL let
  # garbage grow until the kernel killed it for want of memory
  make -C contrib/cli HEAP=${HEAP:-8GB} > "$LOGS/build.log" 2>&1
  note "build: exit $?"
  # our libguile, for Guix's C extensions (guile-gnutls, ...): docs/libguile.md
  make -C src/libguile > "$LOGS/libguile-build.log" 2>&1
  note "build libguile: exit $?"
}

step_reference() {
  start_daemon
  logged "$LOGS/reference.log" guix build -d --no-grafts -e "$PACKAGE"
  drv=$(grep '\.drv$' "$LOGS/reference.log" | tail -1)
  echo "$drv" > "$LOGS/reference.drv"
  note "reference: guix build -d --no-grafts -e '$PACKAGE': ${drv:-failed}"
  logged "$LOGS/reference-build.log" guix build -e "$PACKAGE"
  out=$(grep '^/gnu/store/.*-hello-' "$LOGS/reference-build.log" | tail -1)
  if [ -n "$out" ] && "$out/bin/hello" > /dev/null 2>&1; then
    note "reference: guix build: $out ($("$out/bin/hello"))"
  else
    note "reference: guix build: failed"
  fi
}

step_modules() {
  mkdir -p "$LOGS/modules"
  if [ -n "$MODULES" ]; then
    files=$MODULES
  else
    files=$(cd "$GUIX_SITE" && find guix gnu -name '*.scm' | sort)
  fi
  : > "$LOGS/modules.txt"
  for f in $files; do
    m=$(echo "$f" | sed 's/\.scm$//; s|/| |g')
    log="$LOGS/modules/$(echo "$f" | tr / _).log"
    start=$(date +%s)
    limit $PS -e "(use-modules ($m))" > "$log" 2>&1
    st=$?
    echo "$f $st $(( $(date +%s) - start ))s" >> "$LOGS/modules.txt"
  done
  n=$(wc -l < "$LOGS/modules.txt")
  ok=$(awk '$2 == 0' "$LOGS/modules.txt" | wc -l)
  killed=$(awk '$2 == 137' "$LOGS/modules.txt" | wc -l)
  note "modules: $ok of $n load ($killed over ${LIMIT}s); modules.txt lists each"
}

step_derivation() {
  start_daemon
  GUIX_TRACE_LOADS=1 logged "$LOGS/derivation.log" $PS tests/guix/guix.scm build -d --no-grafts -e "$PACKAGE"
  drv=$(grep '\.drv$' "$LOGS/derivation.log" | tail -1)
  ref=$(cat "$LOGS/reference.drv" 2>/dev/null)
  if [ -z "$drv" ]; then
    note "derivation: failed (derivation.log)"
  elif [ "$drv" = "$ref" ]; then
    note "derivation: $drv, the same as real Guix's"
  else
    note "derivation: $drv; real Guix: ${ref:-not run}"
  fi
}

step_hello() {
  start_daemon
  logged "$LOGS/hello.log" $PS tests/guix/guix.scm build -e "$PACKAGE"
  out=$(grep '^/gnu/store/.*-hello-' "$LOGS/hello.log" | tail -1)
  if [ -n "$out" ] && "$out/bin/hello" > /dev/null 2>&1; then
    note "hello: $out ($("$out/bin/hello"))"
  else
    note "hello: failed (hello.log)"
  fi
}

# One of Guix's test files, on GUILE (a command), in the test environment
# Guix's own test-env sets up: a store of its own, and a daemon on it
run_test() {
  guile=$1 name=$2 log=$3
  tmp=$(mktemp -d)
  mkdir -p "$tmp/store" "$tmp/var" "$tmp/etc"
  (
    cd "$tmp" || exit 1
    export NIX_STORE_DIR=$tmp/store GUIX_STATE_DIRECTORY=$tmp/var \
           GUIX_LOG_DIRECTORY=$tmp/var/log GUIX_CONFIGURATION_DIRECTORY=$tmp/etc \
           GUIX_DAEMON_SOCKET=$tmp/var/daemon-socket/socket \
           GUILE_LOAD_PATH=$GUIX_SOURCE:$GUIX_SITE \
           srcdir=$GUIX_SOURCE/tests top_srcdir=$GUIX_SOURCE top_builddir=$GUIX_SOURCE
    mkdir -p "$tmp/var/daemon-socket"
    guix-daemon --disable-chroot --listen="$GUIX_DAEMON_SOCKET" \
      --substitute-urls="" > "$tmp/daemon.log" 2>&1 &
    d=$!
    sleep 1
    limit $guile "$GUIX_SOURCE/tests/$name.scm"
    echo "# exit $?"
    kill $d 2>/dev/null
  ) > "$log" 2>&1
  rm -rf "$tmp"
}

step_tests() {
  mkdir -p "$LOGS/tests"
  if [ -n "$TESTS" ]; then
    names=$TESTS
  else
    names=$(cd "$GUIX_SOURCE/tests" && ls *.scm | sed 's/\.scm$//')
  fi
  guile=$(ls -d /gnu/store/*-guile-3.0.9/bin/guile | head -1)
  : > "$LOGS/tests.txt"
  for t in $names; do
    run_test "$guile --no-auto-compile" "$t" "$LOGS/tests/$t.guile.log"
    run_test "$PS" "$t" "$LOGS/tests/$t.pseudoscheme.log"
    count() { grep -c "^$2:" "$1"; }
    printf '%s guile: pass=%s fail=%s %s; pseudoscheme: pass=%s fail=%s %s\n' "$t" \
      "$(grep -c '^PASS:' "$LOGS/tests/$t.guile.log")" "$(grep -c '^FAIL:' "$LOGS/tests/$t.guile.log")" \
      "$(tail -1 "$LOGS/tests/$t.guile.log")" \
      "$(grep -c '^PASS:' "$LOGS/tests/$t.pseudoscheme.log")" "$(grep -c '^FAIL:' "$LOGS/tests/$t.pseudoscheme.log")" \
      "$(tail -1 "$LOGS/tests/$t.pseudoscheme.log")" >> "$LOGS/tests.txt"
  done
  note "tests: $(wc -l < "$LOGS/tests.txt") files; tests.txt compares real Guile and --guile"
}

[ $# -gt 0 ] || set -- build reference modules derivation hello tests
for s in "$@"; do "step_$s"; done
