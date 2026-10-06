;;; Tests for SRFI 17, from the examples in the SRFI document.
(import (except (scheme base) set!) (scheme process-context)
        (srfi 64) (srfi 17))
(test-begin "srfi-17")

(define x 1)
(set! x 2)
(test-equal "plain variable" 2 x)

(let ((y 1))
  (set! y (+ y 10))
  (test-equal "local variable" 11 y))

(let ((p (list 1 2 3)))
  (set! (car p) 'a)
  (set! (cdr (cdr p)) '(c))
  (test-equal "car and cdr" '(a 2 c) p)
  (set! (cadr p) 'b)
  (test-equal "cadr" '(a b c) p))

(let ((p (list (list 1 2) 3)))
  (set! (caar p) 'x)
  (set! (cdar p) '(y))
  (test-equal "caar, cdar" '((x y) 3) p)
  (set! (cddr (list 1 2 3)) '())
  (test-assert "cddr" #t))

(let ((v (vector 1 2 3)))
  (set! (vector-ref v 1) 'two)
  (test-equal "vector-ref" '#(1 two 3) v))

(let ((s (make-string 3 #\a)))
  (set! (string-ref s 2) #\z)
  (test-equal "string-ref" "aaz" s))

(let ((b (bytevector 1 2 3)))
  (set! (bytevector-u8-ref b 0) 9)
  (test-equal "bytevector-u8-ref" (bytevector 9 2 3) b))

(test-eq "setter of car" set-car! (setter car))
(test-eq "setter of vector-ref" vector-set! (setter vector-ref))

;; A user-defined getter/setter pair, registered with (set! (setter ...)).
(define (my-car p) (car p))
(set! (setter my-car) set-car!)
(let ((p (list 1 2)))
  (set! (my-car p) 'first)
  (test-equal "set! (setter proc)" '(first 2) p))

;; getter-with-setter
(define kar (getter-with-setter (lambda (p) (car p))
                                (lambda (p v) (set-car! p v))))
(let ((p (list 1 2)))
  (test-equal "getter-with-setter gets" 1 (kar p))
  (set! (kar p) 10)
  (test-equal "getter-with-setter sets" '(10 2) p))

;; A setter taking several arguments.
(define (matrix-ref m i j) (vector-ref (vector-ref m i) j))
(define (matrix-set! m i j v) (vector-set! (vector-ref m i) j v))
(set! (setter matrix-ref) matrix-set!)
(let ((m (vector (vector 1 2) (vector 3 4))))
  (set! (matrix-ref m 1 0) 'z)
  (test-equal "several index arguments" 'z (matrix-ref m 1 0)))

;; The setter of setter is set-setter!.
(define (g) 'g)
((setter setter) g (lambda (v) (set! x v)))
(set! (g) 42)
(test-equal "(setter setter)" 42 x)

;; Arguments are evaluated once each.
(let ((count 0) (v (vector 0 0)))
  (set! (vector-ref (begin (set! count (+ count 1)) v) 0) 5)
  (test-equal "operands evaluated once" '(1 . #(5 0)) (cons count v)))

(test-error "no setter" #t (setter (lambda (z) z)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-17")
  (exit (if (zero? failures) 0 1)))
