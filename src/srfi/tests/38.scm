;;; Tests for SRFI 38, after chibi-scheme's lib/srfi/38/test.sld.  The
;;; reference writer numbers labels from 1, so the expected strings
;;; here do too.
(import (scheme base) (scheme process-context) (scheme write) (scheme cxr)
        (srfi 64) (srfi 1) (srfi 38))

(define (read-from-string str)
  (read-with-shared-structure (open-input-string str)))

(define (write-to-string x)
  (let ((out (open-output-string)))
    (write-with-shared-structure x out)
    (get-output-string out)))

(define-syntax test-io
  (syntax-rules ()
    ((_ str-expr expr)
     (let ((str str-expr) (value expr))
       (test-equal str (write-to-string value))
       (test-equal str (write-to-string (read-from-string str)))))))

(test-begin "srfi-38")

(test-io "(1)" (list 1))
(test-io "(1 2)" (list 1 2))
(test-io "(1 . 2)" (cons 1 2))
(test-io "#1=(1 . #1#)" (circular-list 1))
(test-io "#1=(1 2 . #1#)" (circular-list 1 2))
(test-io "(1 . #1=(2 . #1#))" (cons 1 (circular-list 2)))
(test-io "#1=(1 #1# 3)"
         (let ((x (list 1 2 3))) (set-car! (cdr x) x) x))
(test-io "(#1=(1 #1# 3))"
         (let ((x (list 1 2 3))) (set-car! (cdr x) x) (list x)))
(test-io "(#1=(1 #1# 3) #1#)"
         (let ((x (list 1 2 3))) (set-car! (cdr x) x) (list x x)))
(test-io "(#1=(1 . #1#) #2=(1 . #2#))"
         (list (circular-list 1) (circular-list 1)))
(test-io "(#1=(1 . 2) #2=(1 . 2) #3=(3 . 4) #1# #2# #3#)"
         (let ((a (cons 1 2)) (b (cons 1 2)) (c (cons 3 4)))
           (list a b c a b c)))
(test-io "((1 . #1=#(2)) #1#)"
         (let ((vec (vector 2))) (list (cons 1 vec) vec)))
(test-io "((1 . #1=#(2 #1#)) #1#)"
         (let ((vec (vector 2 #f)))
           (vector-set! vec 1 vec)
           (list (cons 1 vec) vec)))
(test-io "#1=#(#1#)" (let ((x (vector 1))) (vector-set! x 0 x) x))
(test-io "#1=#(1 #1#)" (let ((x (vector 1 2))) (vector-set! x 1 x) x))
(test-io "#1=#(#1# 2 #1#)"
         (let ((x (vector 1 2 3)))
           (vector-set! x 0 x)
           (vector-set! x 2 x)
           x))
(test-io "#\\newline" #\newline)

;; Shared strings are labelled, as the SRFI requires; empty ones aren't.
(test-io "(#1=\"abc\" #1# #1#)"
         (let ((str (string #\a #\b #\c))) (list str str str)))
(test-io "(\"\" \"\")" (let ((s (string))) (list s s)))
(test-io "(\"a\\\"b\" \"a\\\"b\")" (list (string #\a #\" #\b) (string #\a #\" #\b)))

;; The example from the SRFI document.
(test-equal "#1=(val1 . #1#)"
  (let ((a (cons 'val1 'val2)))
    (set-cdr! a a)
    (write-to-string a)))

;; Reading preserves sharing.
(let ((x (read-from-string "(#5=\"ab\" #5# #6=(1 2) #6#)")))
  (test-assert (eq? (car x) (cadr x)))
  (test-assert (eq? (caddr x) (cadddr x))))
(let ((x (read-from-string "#0=(a . #0#)")))
  (test-assert (eq? x (cdr x))))
(let ((x (read-from-string "#2=#(1 #3=(2) #3# #2#)")))
  (test-assert (eq? x (vector-ref x 3)))
  (test-assert (eq? (vector-ref x 1) (vector-ref x 2))))

(test-eq '+.! (read-from-string "+.!"))
(test-eqv 255 (read-from-string "#xff"))
(test-eqv 5 (read-from-string "#e5.0"))
(test-eqv 15 (read-from-string "#e#xf"))
(test-eqv (expt 10 100) (read-from-string "#e1e100"))
(test-assert (eof-object? (read-from-string "")))

;; write/ss and read/ss are the same procedures.
(test-eq write-with-shared-structure write/ss)
(test-eq read-with-shared-structure read/ss)
(test-equal "#1=(x . #1#)"
  (let ((out (open-output-string)) (c (circular-list 'x)))
    (write/ss c out 'optarg)
    (get-output-string out)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-38")
  (exit (if (zero? failures) 0 1)))
