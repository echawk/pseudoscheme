#!/bin/sh
# Smoke tests for the command-line program: sh test.sh path/to/quasischeme
PS=${1:-../../bin/quasischeme}
fail=0; n=0
check() {  # check NAME EXPECTED COMMAND...
  name=$1; expected=$2; shift 2; n=$((n+1))
  got=$("$@" 2>&1)
  if [ "$got" = "$expected" ]; then :; else
    fail=$((fail+1)); printf 'FAIL %s\n  expected: %s\n  got:      %s\n' "$name" "$expected" "$got"
  fi
}
tmp=${TMPDIR:-/tmp}/quasischeme-test.$$; mkdir -p $tmp/lib/my
trap 'rm -rf $tmp' EXIT

check "r7rs -p" "6" $PS -p '(+ 1 2 3)'
check "r6rs -p" "(1 2 3)" $PS --r6rs -p '(list-sort < (list 3 1 2))'
check "r5rs folds case" "#t" $PS --r5rs -p "(eq? 'abc 'ABC)"
check "r4rs: no values" "ERR" sh -c "$PS --r4rs -p '(values 1)' >/dev/null 2>&1 && echo OK || echo ERR"
check "r3rs: the empty list is false" "2" $PS --r3rs -p "(if '() 1 2)"
check "r2rs: #!true and 1+" "(#t 5)" $PS --r2rs -p "(list #!true (1+ 4))"
check "rrs: block and aset" "5" $PS --rrs -p "(block (define x 1) (aset' x 5) x)"
check "r7rs is case-sensitive" "#f" $PS -p "(eq? 'abc 'ABC)"

# R5RS's load and eval in the interaction environment are at R5RS's
# top level, not the R7RS REPL's; a loaded definition can even shadow
# a standard procedure (as J-Bob's does)
cat > $tmp/defs5.scm <<'S'
(define s.car car)
(define (car x) (if (pair? x) (s.car x) 'NOPE))
(define (foo) 'foo)
S
cat > $tmp/load5.scm <<'S'
(load "TMP/defs5.scm")
(define xx 42)
(display (list (foo) (car 5) (eval 'xx (interaction-environment))))
S
sed -i.bak "s|TMP|$tmp|" $tmp/load5.scm
check "r5rs load and eval at R5RS's top level" "(foo nope 42)" $PS --r5rs $tmp/load5.scm

cat > $tmp/hello.scm <<'S'
(import (scheme base) (scheme write) (scheme process-context))
(display "hello ")
(display (cdr (command-line)))
(newline)
(exit 3)
S
check "r7rs program, command-line" "hello (a b)" $PS $tmp/hello.scm a b
$PS $tmp/hello.scm >/dev/null 2>&1; code=$?; n=$((n+1))
[ $code = 3 ] || { fail=$((fail+1)); echo "FAIL exit status: expected 3, got $code"; }

cat > $tmp/lib/my/lib.sld <<'S'
(define-library (my lib)
  (export twice)
  (import (scheme base))
  (begin (define (twice x) (* 2 x))))
S
cat > $tmp/uselib.scm <<'S'
(import (scheme base) (scheme write) (my lib))
(display (twice 21)) (newline)
S
check "r7rs library from -L" "42" $PS -L $tmp/lib $tmp/uselib.scm
check "QUASISCHEME_LIBRARY_PATH" "42" env QUASISCHEME_LIBRARY_PATH=/nonexistent:$tmp/lib $PS $tmp/uselib.scm
mkdir -p $tmp/proj/.akku/lib/my $tmp/proj/src && cp $tmp/lib/my/lib.sld $tmp/proj/.akku/lib/my/
check "--akku finds .akku/lib above" "42" sh -c "cd $tmp/proj/src && $PS --akku $tmp/uselib.scm"

cat > $tmp/lib/my/lib6.sls <<'S'
(library (my lib6)
  (export swap!)
  (import (rnrs))
  (define-syntax swap!
    (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp))))))
S
cat > $tmp/prog6.sps <<'S'
#!r6rs
(import (rnrs) (my lib6))
(define tmp 1) (define other 2)
(swap! tmp other)
(display (list tmp other)) (newline)
S
check "r6rs program + library from -L" "(2 1)" $PS --r6rs -L $tmp/lib $tmp/prog6.sps

reenter="(let ((k #f) (n 0)) (let ((v (call/cc (lambda (c) (set! k c) 0)))) (set! n (+ n 1)) (if (< n 3) (k n) (list v n (+ v n)))))"
check "continuations re-enter (full, the default)" "(2 3 5)" $PS -p "$reenter"
check "--continuations=full" "(2 3 5)" $PS --continuations=full -p "$reenter"
check "--continuations=escape can't re-enter" "no" sh -c "$PS --continuations=escape -p '$reenter' >/dev/null 2>&1 && echo yes || echo no"
check "--r5rs re-enters" "3" $PS --r5rs -p "(let ((k #f) (n 0)) (call-with-current-continuation (lambda (c) (set! k c))) (set! n (+ n 1)) (if (< n 3) (k #f) n))"

mkdir -p $tmp/home
cat > $tmp/home/.sbclrc <<'S'
(defun cl-user::rc-loaded-p () 42)
S
check "the init file is loaded" "42" env HOME=$tmp/home $PS -p '(begin (import (quasischeme lisp)) ((lisp-function "rc-loaded-p" "cl-user")))'
check "--no-userinit skips it" "no" env HOME=$tmp/home sh -c "$PS --no-userinit -p '(begin (import (quasischeme lisp)) ((lisp-function \"rc-loaded-p\" \"cl-user\")))' >/dev/null 2>&1 && echo yes || echo no"
printf '(error "broken rc")\n' > $tmp/home/.sbclrc
check "an error in it is reported, and the program runs" "3" env HOME=$tmp/home sh -c "$PS -p '(+ 1 2)' 2>/dev/null"

check "error exit" "Error: not a pair 5" sh -c "$PS -e '(car 5)' 2>&1 | head -1"
check "repl" "3" sh -c "printf '(+ 1 2)\n,q\n' | $PS | grep -v '^Quasischeme' | tr -d '> r7s' "

check "(cl common-lisp) from Scheme" "(1 2 3)" $PS -p "(begin (import (prefix (cl common-lisp) cl:)) (cl:sort (list 3 1 2) cl:<))"
check "#:keyword arguments" "((a . 1) (b . 2))" $PS -p "(begin (import (prefix (cl common-lisp) cl:)) (cl:sort (list '(b . 2) '(a . 1)) cl:< #:key cl:cdr))"
check "-l loads a Lisp system" "#t" $PS -l uiop -p "(begin (import (quasischeme lisp)) (procedure? (lisp-function \"getenv\" \"uiop\")))"

check "-V: SRFI 176" '(command "quasischeme")' sh -c "$PS -V | head -1"
check "(command-line) without a program: SRFI 193" '("")' $PS -p '(command-line)'

# SRFI 22: links named for the script interpreters run FILE's main
mkdir -p $tmp/ib
for i in scheme-r5rs scheme-r6rs scheme-r7rs; do ln -s "$(cd "$(dirname "$PS")" && pwd)/$(basename "$PS")" $tmp/ib/$i; done
printf '#! /usr/bin/env scheme-r5rs\n(define (main args) (display args) 3)\n' > $tmp/s5
printf '#! /usr/bin/env scheme-r6rs\n(import (rnrs))\n(define (main args) (display (cdr args)) 0)\n' > $tmp/s6
printf '#! /usr/bin/env scheme-r7rs\n(import (scheme base))\n(define (main args) "no")\n' > $tmp/s7
check "SRFI 22: scheme-r5rs" "($tmp/s5 a b) 3" sh -c "$tmp/ib/scheme-r5rs $tmp/s5 a b; echo ' '\$?"
check "SRFI 22: scheme-r6rs" "(x) 0" sh -c "$tmp/ib/scheme-r6rs $tmp/s6 x; echo ' '\$?"
check "SRFI 22: main's value isn't a status" "70" sh -c "$tmp/ib/scheme-r7rs $tmp/s7 2>/dev/null; echo \$?"

# SRFI 138: compile-r7rs writes an executable
ln -s "$(cd "$(dirname "$PS")" && pwd)/$(basename "$PS")" $tmp/ib/compile-r7rs
printf '(import (scheme base) (scheme write) (my lib))\n(cond-expand (shout (display (twice 21))))\n' > $tmp/c.scm
check "SRFI 138: compile-r7rs" "42" sh -c "$tmp/ib/compile-r7rs -A $tmp/lib -D shout -o $tmp/c $tmp/c.scm >/dev/null && $tmp/c"

# --compile: an executable in each mode, run from another directory with
# no source, no cache and no home directory; it takes its own arguments,
# exits with the program's status, and reports errors as the run does
mkdir -p $tmp/exe $tmp/exe/nowhere
printf '(import (scheme base) (scheme write) (scheme process-context) (my lib))\n(write (cdr (command-line))) (display (twice 21)) (newline)\n(exit 3)\n' > $tmp/exe/p7.scm
printf '#!r6rs\n(import (rnrs) (rnrs programs))\n(write (cdr (command-line))) (newline)\n(exit 4)\n' > $tmp/exe/p6.sps
printf "(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))\n(write (list (f 20) 'ABC)) (newline)\n(car 5)\n" > $tmp/exe/p5.scm
printf '(import (chezscheme))\n(printf "~a ~a\\n" (cdr (command-line)) (fx+ 1 2))\n(exit 5)\n' > $tmp/exe/pc.ss
printf '(use-modules (ice-9 format))\n(format #t "~a~%%" (cdr (program-arguments)))\n(exit 6)\n' > $tmp/exe/pg.scm
printf '#lang racket/base\n(write (current-command-line-arguments)) (newline)\n(exit 7)\n' > $tmp/exe/pr.rkt
exe_check() {  # exe_check NAME EXPECTED-OUTPUT EXPECTED-STATUS SOURCE MODE-FLAG...
  name=$1; want=$2; wantcode=$3; src=$4; shift 4; n=$((n+1))
  out=$tmp/exe/${src%.*}.out
  if ! $PS "$@" -L $tmp/lib --compile $tmp/exe/$src -o $out >$tmp/exe/build.log 2>&1; then
    fail=$((fail+1)); printf 'FAIL %s: did not build\n  %s\n' "$name" "$(tail -3 $tmp/exe/build.log)"; return
  fi
  mv $tmp/exe/$src $tmp/exe/$src.moved
  got=$(cd $tmp/exe/nowhere && HOME=$tmp/exe/nowhere QUASISCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/exe/nowhere/cache $out a "b c" --help 2>&1); code=$?
  mv $tmp/exe/$src.moved $tmp/exe/$src
  if [ "$got" != "$want" ] || [ $code != $wantcode ]; then
    fail=$((fail+1)); printf 'FAIL %s\n  expected: %s (status %s)\n  got:      %s (status %s)\n' "$name" "$want" "$wantcode" "$got" "$code"
  fi
}
exe_check "--compile r7rs" '("a" "b c" "--help")42' 3 p7.scm
exe_check "--compile r6rs" '("a" "b c" "--help")' 4 p6.sps --r6rs
exe_check "--compile r5rs: error status" "(2432902008176640000 abc)
Error: not a pair 5" 70 p5.scm --r5rs
exe_check "--compile r3rs" "(2432902008176640000 abc)
Error: not a pair 5" 70 p5.scm --r3rs
exe_check "--compile chez" '(a b c --help) 3' 5 pc.ss --chez
if $PS --guile -p '(+ 1 2)' >/dev/null 2>&1; then
  exe_check "--compile guile" '(a b c --help)' 6 pg.scm --guile
fi
if $PS --racket -p '(+ 1 2)' >/dev/null 2>&1; then
  exe_check "--compile racket" '#("a" "b c" "--help")' 7 pr.rkt --racket
fi

# --precompile DIR: its libraries compiled into the cache
mkdir -p $tmp/cache
check "--precompile" "1 of 1 compiled" sh -c "QUASISCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/cache $PS --precompile $tmp/lib | grep -o '1 of 1 compiled'"
check "a precompiled library loads" "42" env QUASISCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/cache $PS -L $tmp/lib $tmp/uselib.scm

# Chez's command: compile-program writes a fasl, which runs as the program
ln -s "$(cd "$(dirname "$PS")" && pwd)/$(basename "$PS")" $tmp/ib/chez
printf '(import (chezscheme) (my lib))\n(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))\n(printf "~a ~a ~a\\n" (f 20) (twice 4) (cdr (command-line)))\n' > $tmp/p.ss
check "chez: compile-program" "# FASL" sh -c "echo '(compile-program \"$tmp/p.ss\")' | QUASISCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/cache $tmp/ib/chez -q --libdirs $tmp/lib >/dev/null && sed -n 1p $tmp/p.so | cut -c1-6"
check "chez: a compiled program runs" "2432902008176640000 8 (a b)" env QUASISCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/cache $tmp/ib/chez --libdirs $tmp/lib --program $tmp/p.so a b

echo "$((n-fail)) of $n CLI tests passed"
[ $fail = 0 ]
