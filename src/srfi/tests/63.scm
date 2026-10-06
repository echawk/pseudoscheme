;;; Tests for SRFI 63, from the examples in the SRFI document.
(import (except (scheme base) equal?) (scheme process-context)
        (srfi 64) (srfi 63))

(test-begin "srfi-63")

(test-assert (array? (make-array '#(1) 2 2)))
(test-assert (array? (vector 1 2)))
(test-assert (array? "abc"))
(test-assert (not (array? 'a)))
(test-assert (not (array? '(1 2))))

;; equal?
(test-assert (equal? 'a 'a))
(test-assert (equal? '(a) '(a)))
(test-assert (equal? '(a (b) c) '(a (b) c)))
(test-assert (equal? "abc" "abc"))
(test-assert (equal? 2 2))
(test-assert (equal? (make-vector 5 'a) (make-vector 5 'a)))
(test-assert (equal? (make-array (A:fixN32b 4) 5 3)
                     (make-array (A:fixN32b 4) 5 3)))
(test-assert (equal? (make-array '#(foo) 3 3) (make-array '#(foo) 3 3)))
(test-assert (not (equal? (make-array '#(foo) 3 3) (make-array '#(bar) 3 3))))
(test-assert (not (equal? (make-array '#(foo) 3 3) (make-array '#(foo) 9))))
(test-assert (equal? (bytevector 1 2) (bytevector 1 2)))
(test-assert (not (equal? "abc" "abd")))

;; array-rank, array-dimensions
(test-equal '(3 5) (array-dimensions (make-array '#() 3 5)))
(test-eqv 2 (array-rank (make-array '#() 3 5)))
(test-eqv 1 (array-rank (vector 1 2 3)))
(test-eqv 0 (array-rank 'foo))
(test-equal '(3) (array-dimensions "abc"))

;; make-array fills with the prototype's first element
(test-equal '((foo foo foo) (foo foo foo)) (array->list (make-array '#(foo) 2 3)))
(test-equal '(#\x #\x #\x) (array->list (make-array "x" 3)))

;; make-shared-array
(define fred (make-array '#(#f) 8 8))
(define freds-diagonal
  (make-shared-array fred (lambda (i) (list i i)) 8))
(array-set! freds-diagonal 'foo 3)
(test-eq 'foo (array-ref fred 3 3))
(define freds-center
  (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j))) 2 2))
(test-eq 'foo (array-ref freds-center 0 0))
(array-set! freds-center 'bar 1 1)
(test-eq 'bar (array-ref fred 4 4))
(test-eq 'bar (array-ref freds-diagonal 4))

;; list->array, array->list
(define m (list->array 2 '#() '((1 2) (3 4))))
(test-equal '(2 2) (array-dimensions m))
(test-eqv 3 (array-ref m 1 0))
(test-equal '((1 2) (3 4)) (array->list m))
(test-eqv 3 (array->list (list->array 0 '#() 3)))
(test-equal '((ho ho ho) (ho oh oh))
  (array->list (list->array 2 '#() '((ho ho ho) (ho oh oh)))))

;; vector->array, array->vector
(test-equal '((1 2) (3 4)) (array->list (vector->array #(1 2 3 4) #() 2 2)))
(test-eqv 3 (array->list (vector->array '#(3) '#())))
(test-equal #(1 2 3 4) (array->vector (list->array 2 '#() '((1 2) (3 4)))))
(test-equal #(ho) (array->vector (list->array 0 '#() 'ho)))
(test-equal #(1 3 2 4)
  (array->vector (make-shared-array m (lambda (i j) (list j i)) 2 2)))

;; array-in-bounds?, array-ref, array-set!
(test-assert (array-in-bounds? m 1 1))
(test-assert (not (array-in-bounds? m 2 0)))
(test-assert (not (array-in-bounds? m 0)))
(test-assert (not (array-in-bounds? m -1 0)))
(array-set! m 'x 0 1)
(test-eq 'x (array-ref m 0 1))
(test-eqv 2 (array-ref (vector 1 2 3) 1))
(test-eqv #\b (array-ref "abc" 1))

;; prototypes
(test-assert (vector? (A:floR64b)))
(test-equal '((1.5 1.5)) (array->list (make-array (A:floR64b 1.5) 1 2)))
(test-equal '(#t #t) (array->list (make-array (A:bool #t) 2)))
(test-eqv 255 (array-ref (make-array (A:fixN8b 255) 2) 0))
(test-error (A:fixN8b 256))
(test-error (A:fixN8b -1))
(test-eqv -128 (array-ref (make-array (A:fixZ8b -128) 1) 0))
(test-error (A:bool 1))
(test-error (A:floR32b 'x))
(test-error (array-ref m 5 5))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-63")
  (exit (if (zero? failures) 0 1)))
