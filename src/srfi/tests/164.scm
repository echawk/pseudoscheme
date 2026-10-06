;;; Tests for SRFI 164, from the examples in the SRFI document.  Kawa's
;;; range syntax is replaced by vectors or SRFI 196 ranges.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 164)
        (only (srfi 196) numeric-range iota-range))

(test-begin "srfi-164")

(define (rows a)                        ; a rank-2 array as a list of lists
  (let loop ((i (- (array-end a 0) 1)) (acc '()))
    (if (< i (array-start a 0))
        acc
        (loop (- i 1)
              (cons (let inner ((j (- (array-end a 1) 1)) (row '()))
                      (if (< j (array-start a 1))
                          row
                          (inner (- j 1) (cons (array-ref a i j) row))))
                    acc)))))

;; array?
(test-assert (array? (make-array #(2 2))))
(test-assert (array? (vector 1 2)))
(test-assert (not (array? '(1 2))))

;; shapes
(test-equal #(0 2 1 4) (array->vector (shape 0 2 1 4)))
(test-eqv 2 (array-rank (shape 0 2 1 4)))
(test-eqv 2 (array-end (shape 0 2 1 4) 0))
(test-equal '((0 2) (0 3)) (rows (->shape #(2 3))))
(test-equal '((1 3) (1 4)) (rows (->shape #((1 3) (1 4)))))
(test-equal '((0 2) (0 3)) (rows (->shape #(2 (0 3)))))
(test-equal '((1 3) (1 4)) (rows (->shape (vector (numeric-range 1 3) (iota-range 3 1)))))
(test-equal '((1 3) (1 4)) (rows (->shape (shape 1 3 1 4))))
(test-equal '((1 3) (1 4)) (rows (array-shape (make-array (shape 1 3 1 4)))))
(test-error (shape 1 2 3))
(test-error (shape 2 1))
(test-eqv 2 (array-rank (make-array (shape 1 2 3 4))))
(test-eqv 0 (array-rank (make-array (shape))))
(test-eqv 1 (array-size (make-array (shape))))
(test-eqv 12 (array-size (make-array #((1 4) (0 4)))))
(test-eqv 1 (array-start (make-array #((1 4) (0 4))) 0))
(test-eqv 4 (array-end (make-array #((1 4) (0 4))) 1))
(test-eqv 3 (array-size (vector 1 2 3)))

;; make-array cycles through its values
(test-equal '((1 2 3 4) (5 1 2 3)) (rows (make-array #(2 4) 1 2 3 4 5)))
(test-equal '((7 7) (7 7)) (rows (make-array #(2 2) 7)))

;; array, array-ref
(test-eq 'cuatro
  (array-ref (array #(2 3) 'uno 'dos 'tres 'cuatro 'cinco 'seis) 1 0))
(test-equal '(3 1 4)
  (let ((a (array (shape 4 7 1 2) 3 1 4)))
    (list (array-ref a 4 1)
          (array-ref a (vector 5 1))
          (array-ref a (array (shape 0 2) 6 1)))))
(test-error (array #(2) 1 2 3))
(test-error (array-ref (array #(2) 1 2) 2))

;; array-set!
(test-equal "huuhkaja"
  (let ((a (make-array (shape 4 5 4 5 4 5))))
    (array-set! a 4 4 4 "huuhkaja")
    (array-ref a 4 4 4)))
(let ((a (make-array #(2 2) 0)))
  (array-set! a (vector 1 0) 'x)
  (test-eq 'x (array-ref a 1 0)))

;; build-array (the shape #2a((10 12) (0 3)) is written with shape)
(define built
  (build-array (shape 10 12 0 3)
               (lambda (ind)
                 (let ((x (vector-ref ind 0)) (y (vector-ref ind 1)))
                   (- x y)))))
(test-equal '((10 9 8) (11 10 9)) (rows built))
(test-error (array-set! built 10 0 'x))

(define (make-sparse-array shape default-value)
  (let ((vals '()))
    (build-array shape
                 (lambda (I)
                   (let ((v (assoc I vals)))
                     (if v (cdr v) default-value)))
                 (lambda (I newval)
                   (let ((v (assoc I vals)))
                     (if v
                         (set-cdr! v newval)
                         (set! vals (cons (cons I newval) vals))))))))
(let ((sp (make-sparse-array #(1000 1000) 0)))
  (array-set! sp 500 7 'x)
  (test-eq 'x (array-ref sp 500 7))
  (test-eqv 0 (array-ref sp 7 500)))

;; index-array
(test-equal '((0 1 2 3) (4 5 6 7)) (rows (index-array (shape 1 3 2 6))))
(test-eqv 2 (array-start (index-array (shape 1 3 2 6)) 1))
(test-error (array-set! (index-array #(2)) 0 1))

;; array-index-ref
(define arr (array (shape 1 4 0 4) 10 11 12 13 20 21 22 23 30 31 32 33))
(test-eqv 23 (array-index-ref arr 2 3))
(test-equal #(23 21) (array-index-ref arr 2 #(3 1)))
(test-equal '((23 21 23) (13 11 13)) (rows (array-index-ref arr #(2 1) #(3 1 3))))
(test-equal '((11 12 13) (21 22 23))
  (rows (array-index-ref arr (numeric-range 1 3) (numeric-range 1 4))))
(let ((r (array-index-ref arr #(2 1) (array (shape 0 2 0 2) 3 1 3 2))))
  (test-eqv 3 (array-rank r))
  (test-equal '(23 21 23 22 13 11 13 12) (vector->list (array-flatten r))))
(test-equal #(20 21 22 23) (array-index-ref arr 2 (numeric-range 0 4)))
(test-equal #(23 22 21 20) (array-index-ref arr 2 (numeric-range 3 -1 -1)))
(test-equal '((13) (23) (33))
  (rows (array-index-ref arr (numeric-range 1 4) #(3))))
(test-equal '((13 13 13 13 13) (23 23 23 23 23) (33 33 33 33 33))
  (rows (array-index-ref arr (numeric-range 1 4) (make-vector 5 3))))
(test-error (array-index-ref arr #(0) #(0)))
;; the result is a fresh copy
(let ((r (array-index-ref arr #(1 2) #(0 1))))
  (array-set! arr 1 0 'changed)
  (test-eqv 10 (array-ref r 0 0))
  (array-set! arr 1 0 10))

;; array-copy!, array-fill!
(let ((a (make-array #(2 2) 0)))
  (array-copy! a (array #(2 2) 1 2 3 4))
  (test-equal '((1 2) (3 4)) (rows a))
  (array-fill! a 'z)
  (test-equal '((z z) (z z)) (rows a))
  (test-error (array-copy! a (make-array #(2 3) 0))))
(let ((a (make-array #(3 3) 0)))
  (array-fill! (array-index-share a 1 (numeric-range 0 3)) 9)
  (test-equal '((0 0 0) (9 9 9) (0 0 0)) (rows a)))

;; array-transform
(define tr
  (array-transform arr (shape 0 3 1 3 0 2)
    (lambda (ix) (let ((i (vector-ref ix 0)) (j (vector-ref ix 1)) (k (vector-ref ix 2)))
                   (vector (+ i 1) (+ (* 2 (- j 1)) k))))))
(test-equal '(10 11 12 13 20 21 22 23 30 31 32 33) (vector->list (array-flatten tr)))
(array-set! tr 0 1 0 'a)
(test-eq 'a (array-ref arr 1 0))
(array-set! arr 1 0 10)

;; array-index-share is a view
(let ((v (array-index-share arr #(1 3) #(0 3))))
  (test-equal '((10 13) (30 33)) (rows v))
  (array-set! v 1 1 'x)
  (test-eq 'x (array-ref arr 3 3))
  (array-set! arr 3 3 33))
(let ((e (array-index-share arr 2 2)))
  (test-eqv 0 (array-rank e))
  (test-eqv 22 (array-ref e))
  (array-set! e 'y)
  (test-eq 'y (array-ref arr 2 2))
  (array-set! arr 2 2 22))

;; array-reshape
(let* ((v (vector 1 2 3 4 5 6))
       (m (array-reshape v #(2 3))))
  (test-equal '((1 2 3) (4 5 6)) (rows m))
  (array-set! m 1 1 'five)
  (test-eq 'five (vector-ref v 4))
  (test-assert (eq? v (array->vector (array-reshape v #(3 2))))))
(test-error (array-reshape (vector 1 2 3) #(2 2)))
(let* ((t (share-array (array #(2 3) 1 2 3 4 5 6) #(3 2)
                       (lambda (i j) (values j i))))
       (r (array-reshape t #(6))))
  (test-equal '(1 4 2 5 3 6) (vector->list (array-flatten r))))

;; share-array
(define i_4
  (let* ((i (make-array (shape 0 4 0 4) 0))
         (d (share-array i (shape 0 4) (lambda (k) (values k k)))))
    (do ((k 0 (+ k 1)))
        ((= k 4))
      (array-set! d k 1))
    i))
(test-equal '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1)) (rows i_4))
(test-equal '((1.0 2.0 3.0) (4.0 5.0 6.0))
  (rows (share-array (vector 1.0 2.0 3.0 4.0 5.0 6.0) (shape 0 2 0 3)
                     (lambda (i j) (+ (* 3 i) j))))) ; the SRFI has (* 2 i), a typo
(test-equal '((1 4) (2 5) (3 6))
  (rows (share-array (array #(2 3) 1 2 3 4 5 6) #(3 2)
                     (lambda (i j) (values j i)))))
(test-equal '((22 23) (32 33))
  (rows (share-array arr (shape 0 2 0 2) (lambda (i j) (values (+ i 2) (+ j 2))))))
(test-error (share-array arr (shape 0 4 0 4) (lambda (i j) (values i j))))
(test-equal '(9 8)
  (let ((s (share-array (build-array #(10) (lambda (iv) (vector-ref iv 0)))
                        #(2) (lambda (i) (- 9 i)))))
    (list (array-ref s 0) (array-ref s 1))))

;; array-flatten, array->vector
(test-equal #(10 11 12 13 20 21 22 23 30 31 32 33) (array-flatten arr))
(let* ((a (array #(2 2) 1 2 3 4))
       (f (array-flatten a)))
  (vector-set! f 0 'x)
  (test-eqv 1 (array-ref a 0 0)))
(let* ((a (array #(2 2) 1 2 3 4))
       (v (array->vector a)))
  (test-equal #(1 2 3 4) v)
  (vector-set! v 0 'x)
  (test-eq 'x (array-ref a 0 0)))
(let* ((a (share-array (array #(2 2) 1 2 3 4) #(2 2) (lambda (i j) (values j i))))
       (v (array->vector a)))
  (test-equal '(1 3 2 4) (list (array-ref v 0) (array-ref v 1) (array-ref v 2) (array-ref v 3)))
  (array-set! v 1 'y)
  (test-eq 'y (array-ref a 0 1)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-164")
  (exit (if (zero? failures) 0 1)))
