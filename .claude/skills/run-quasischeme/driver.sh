#!/bin/sh
# Agent driver for quasischeme.  Run from the repo root:
#   sh .claude/skills/run-quasischeme/driver.sh COMMAND [ARGS]
#
#   build                  rebuild bin/quasischeme (make -C contrib/cli)
#   smoke                  one check per mode (r7rs r6rs r5rs r4rs chez guile racket), PASS/FAIL each
#   eval MODE EXPR         print EXPR's value in MODE (r7rs r6rs r5rs chez guile ...)
#   run MODE FILE [ARGS]   run a program file (racket: a #lang file)
#   repl [MODE]            the REPL, fed from stdin: printf '(+ 1 2)\n,q\n' | driver.sh repl chez
#   lisp FILE.lisp         load the systems from source with ASDF (no image), then FILE.lisp
#   suite NAME [ARGS]      one core suite: tests/run-NAME-tests.lisp (r7rs, interop, continuation ...)
#
# Every command is hard-killed after LIMIT seconds (default 60; SBCL
# ignores SIGALRM, and macOS has no timeout(1)).
set -u
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
PS=$ROOT/bin/quasischeme
LIMIT=${LIMIT:-60}

# capped CMD...: run CMD, kill -9 it after LIMIT s.  The watcher's
# output goes to /dev/null, or a pipe on our output stays open LIMIT s.
# sh gives a background job /dev/null for stdin unless it's redirected
# explicitly, hence <&3 (else `repl` sees no input).
capped() {
  exec 3<&0
  "$@" <&3 &
  pid=$!
  ( sleep "$LIMIT"; kill -9 "$pid" ) >/dev/null 2>&1 &
  w=$!
  wait "$pid"; rc=$?
  { kill "$w"; wait "$w"; } 2>/dev/null
  [ $rc = 137 ] && echo "driver: killed after ${LIMIT}s" >&2
  return $rc
}

mode_flag() { [ "$1" = r7rs ] && echo "" || echo "--$1"; }

cmd=${1:-help}; [ $# -gt 0 ] && shift
case $cmd in
  build)
    LIMIT=${LIMIT_BUILD:-900} capped make -C "$ROOT/contrib/cli" >"${TMPDIR:-/tmp}/quasischeme-build.log" 2>&1 \
      && echo "built $PS" || { tail -20 "${TMPDIR:-/tmp}/quasischeme-build.log"; exit 1; } ;;
  eval)
    m=$1; shift; capped "$PS" $(mode_flag "$m") -p "$1" ;;
  run)
    m=$1; shift; capped "$PS" $(mode_flag "$m") "$@" ;;
  repl)
    capped "$PS" $(mode_flag "${1:-r7rs}") ;;
  lisp)
    f=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
    tmp=${TMPDIR:-/tmp}/quasischeme-driver.$$.lisp
    cat >"$tmp" <<EOF
(require :asdf)
(let ((s (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file s) (load s)))
(push #p"$ROOT/" asdf:*central-registry*)
(asdf:load-system :r7rs)
(load #p"$f")
EOF
    capped sbcl --dynamic-space-size 4GB --control-stack-size 500MB --script "$tmp"; rc=$?
    rm -f "$tmp"; exit $rc ;;
  suite)
    n=$1; shift
    cd "$ROOT" && QUASISCHEME_LIBRARY_CACHE=0 capped sbcl --dynamic-space-size 4GB \
      --control-stack-size 500MB --script "tests/run-$n-tests.lisp" "$@" ;;
  smoke)
    tmp=${TMPDIR:-/tmp}/quasischeme-smoke.$$; mkdir -p "$tmp"; fail=0
    printf '(display (expt 2 100))\n' >"$tmp/a.ss"
    printf '#lang racket/base\n(displayln (for/list ([i 3]) (* i i)))\n' >"$tmp/a.rkt"
    chk() { # chk NAME EXPECTED CMD...
      n=$1; e=$2; shift 2; got=$(capped "$@" 2>&1)
      if [ "$got" = "$e" ]; then echo "PASS $n"; else echo "FAIL $n: expected [$e] got [$got]"; fail=1; fi
    }
    chk r7rs  6        "$PS" -p '(+ 1 2 3)'
    chk r6rs  "(1 2 3)" "$PS" --r6rs -p '(list-sort < (list 3 1 2))'
    chk r5rs  "#t"     "$PS" --r5rs -p "(eq? 'abc 'ABC)"
    chk r4rs  "2"      "$PS" --r3rs -p "(if '() 1 2)"
    chk chez  1267650600228229401496703205376 "$PS" --chez "$tmp/a.ss"
    chk guile '"a,b"'  "$PS" --guile -p '(string-join (list "a" "b") ",")'
    chk racket "(0 1 4)" "$PS" --racket "$tmp/a.rkt"
    chk repl  "$(printf 'r7rs> 3\nr7rs> ')" sh -c "printf '(+ 1 2)\n,q\n' | '$PS' | tail -n +2"
    rm -rf "$tmp"; exit $fail ;;
  *) sed -n '2,15p' "$0"; exit 2 ;;
esac
