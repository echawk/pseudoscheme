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

check "error exit" "Error: not a pair 5" sh -c "$PS -e '(car 5)' 2>&1 | head -1"
check "repl" "3" sh -c "printf '(+ 1 2)\n,q\n' | $PS | grep -v '^Pseudoscheme' | tr -d '> r7s' "

check "(cl common-lisp) from Scheme" "(1 2 3)" $PS -p "(begin (import (prefix (cl common-lisp) cl:)) (cl:sort (list 3 1 2) cl:<))"
check "#:keyword arguments" "((a . 1) (b . 2))" $PS -p "(begin (import (prefix (cl common-lisp) cl:)) (cl:sort (list '(b . 2) '(a . 1)) cl:< #:key cl:cdr))"
check "-l loads a Lisp system" "#t" $PS -l uiop -p "(begin (import (pseudoscheme lisp)) (procedure? (lisp-function \"getenv\" \"uiop\")))"

echo "$((n-fail)) of $n CLI tests passed"
[ $fail = 0 ]
