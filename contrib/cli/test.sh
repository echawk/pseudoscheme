#!/bin/sh
# Smoke tests for the command-line program: sh test.sh path/to/pseudoscheme
PS=${1:-../../bin/pseudoscheme}
fail=0; n=0
check() {  # check NAME EXPECTED COMMAND...
  name=$1; expected=$2; shift 2; n=$((n+1))
  got=$("$@" 2>&1)
  if [ "$got" = "$expected" ]; then :; else
    fail=$((fail+1)); printf 'FAIL %s\n  expected: %s\n  got:      %s\n' "$name" "$expected" "$got"
  fi
}
tmp=${TMPDIR:-/tmp}/pseudoscheme-test.$$; mkdir -p $tmp/lib/my
trap 'rm -rf $tmp' EXIT

check "r7rs -p" "6" $PS -p '(+ 1 2 3)'
check "r6rs -p" "(1 2 3)" $PS --r6rs -p '(list-sort < (list 3 1 2))'
check "r5rs folds case" "#t" $PS --r5rs -p "(eq? 'abc 'ABC)"
check "r7rs is case-sensitive" "#f" $PS -p "(eq? 'abc 'ABC)"

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
check "PSEUDOSCHEME_LIBRARY_PATH" "42" env PSEUDOSCHEME_LIBRARY_PATH=/nonexistent:$tmp/lib $PS $tmp/uselib.scm
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
check "the init file is loaded" "42" env HOME=$tmp/home $PS -p '(begin (import (pseudoscheme lisp)) ((lisp-function "rc-loaded-p" "cl-user")))'
check "--no-userinit skips it" "no" env HOME=$tmp/home sh -c "$PS --no-userinit -p '(begin (import (pseudoscheme lisp)) ((lisp-function \"rc-loaded-p\" \"cl-user\")))' >/dev/null 2>&1 && echo yes || echo no"
printf '(error "broken rc")\n' > $tmp/home/.sbclrc
check "an error in it is reported, and the program runs" "3" env HOME=$tmp/home sh -c "$PS -p '(+ 1 2)' 2>/dev/null"

check "error exit" "Error: not a pair 5" sh -c "$PS -e '(car 5)' 2>&1 | head -1"
check "repl" "3" sh -c "printf '(+ 1 2)\n,q\n' | $PS | grep -v '^Pseudoscheme' | tr -d '> r7s' "

check "(cl common-lisp) from Scheme" "(1 2 3)" $PS -p "(begin (import (prefix (cl common-lisp) cl:)) (cl:sort (list 3 1 2) cl:<))"
check "#:keyword arguments" "((a . 1) (b . 2))" $PS -p "(begin (import (prefix (cl common-lisp) cl:)) (cl:sort (list '(b . 2) '(a . 1)) cl:< #:key cl:cdr))"
check "-l loads a Lisp system" "#t" $PS -l uiop -p "(begin (import (pseudoscheme lisp)) (procedure? (lisp-function \"getenv\" \"uiop\")))"

check "-V: SRFI 176" '(command "pseudoscheme")' sh -c "$PS -V | head -1"
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

# --precompile DIR: its libraries compiled into the cache
mkdir -p $tmp/cache
check "--precompile" "1 of 1 compiled" sh -c "PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/cache $PS --precompile $tmp/lib | grep -o '1 of 1 compiled'"
check "a precompiled library loads" "42" env PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY=$tmp/cache $PS -L $tmp/lib $tmp/uselib.scm

echo "$((n-fail)) of $n CLI tests passed"
[ $fail = 0 ]
