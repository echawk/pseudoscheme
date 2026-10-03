;;; (demo stats): a little statistics library, in R7RS Scheme, using a
;;; SRFI and Common Lisp's SORT.

(define-library (demo stats)
  (export mean variance standard-deviation median summary)
  (import (scheme base) (scheme inexact)
          (only (srfi 1) fold)
          (prefix (only (cl common-lisp) sort) cl:))
  (begin
    (define (mean xs)
      (/ (fold + 0 xs) (length xs)))

    (define (variance xs)
      (let ((m (mean xs)))
        (/ (fold (lambda (x acc) (+ acc (square (- x m)))) 0 xs)
           (length xs))))

    (define (standard-deviation xs)
      (sqrt (variance xs)))

    (define (median xs)
      (let* ((v (list->vector (cl:sort (list-copy xs) <)))
             (n (vector-length v)))
        (if (odd? n)
            (vector-ref v (quotient n 2))
            (/ (+ (vector-ref v (- (quotient n 2) 1))
                  (vector-ref v (quotient n 2)))
               2))))

    ;; An association list, which is just as convenient in Lisp.
    (define (summary xs)
      (list (cons 'mean (mean xs))
            (cons 'median (median xs))
            (cons 'standard-deviation (standard-deviation xs))))))
