;;; Tests for SRFI 47, from the examples in the SRFI document.
(import (except (scheme base) equal?) (scheme process-context)
        (srfi 64) (srfi 47))

(test-begin "srfi-47")

(test-assert (array? (make-array '#(1) 2 2)))
(test-assert (array? (vector 1 2)))
(test-assert (array? "abc"))
(test-assert (not (array? 'a)))

(test-assert (equal? 'a 'a))
(test-assert (equal? '(a) '(a)))
(test-assert (equal? '(a (b) c) '(a (b) c)))
(test-assert (equal? "abc" "abc"))
(test-assert (equal? 2 2))
(test-assert (equal? (make-vector 5 'a) (make-vector 5 'a)))
(test-assert (equal? (make-array (au32 4) 5 3) (make-array (au32 4) 5 3)))
(test-assert (not (equal? (make-array (au32 4) 5 3) (make-array (au32 5) 5 3))))

(test-eq 'foo (array-ref (make-array '#(foo) 2 3) 1 2))
(test-equal '(2 3) (array-dimensions (make-array '#(foo) 2 3)))
(test-equal '(3 5) (array-dimensions (make-array '#() 3 5)))
(test-eqv 2 (array-rank (make-array '#() 3 5)))
(test-eqv 0 (array-rank 'foo))

(define fred (make-array '#(#f) 8 8))
(define freds-diagonal
  (make-shared-array fred (lambda (i) (list i i)) 8))
(array-set! freds-diagonal 'foo 3)
(test-eq 'foo (array-ref fred 3 3))
(define freds-center
  (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j))) 2 2))
(test-eq 'foo (array-ref freds-center 0 0))

(test-assert (array-in-bounds? fred 7 7))
(test-assert (not (array-in-bounds? fred 8 7)))
(test-assert (not (array-in-bounds? freds-center 2 0)))

;; Prototypes
(test-eqv 255 (array-ref (make-array (au8 255) 2) 1))
(test-error (au8 256))
(test-eqv -128 (array-ref (make-array (as8 -128) 2) 1))
(test-error (as8 128))
(test-eqv (- (expt 2 63)) (array-ref (make-array (as64 (- (expt 2 63))) 1) 0))
(test-eqv 1.5 (array-ref (make-array (ar64 1.5) 1) 0))
(test-eqv #t (array-ref (make-array (at1 #t) 1) 0))
(test-error (at1 0))
(test-assert (array? (make-array (ac32) 2 2)))
(test-error (ar32 1 2))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-47")
  (exit (if (zero? failures) 0 1)))
