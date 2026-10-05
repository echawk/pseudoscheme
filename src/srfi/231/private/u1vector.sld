;;; (srfi 231 private u1vector): the u1vector procedures SRFI 231's
;;; u1-storage-class needs, which chibi's (srfi 160 base) provides as an
;;; extension and Pseudoscheme's does not.  Written for Pseudoscheme: a
;;; u1vector is a record around a bytevector holding one element (0 or
;;; 1) per byte -- simple rather than compact.
(define-library (srfi 231 private u1vector)
  (export make-u1vector u1vector u1? u1vector? u1vector-ref u1vector-set!
          u1vector-length u1vector->list list->u1vector)
  (import (scheme base))
  (begin
    (define-record-type u1vector-type
      (bytes->u1vector bytes)
      u1vector?
      (bytes u1vector-bytes))

    (define (u1? x) (or (eqv? x 0) (eqv? x 1)))

    (define (check-u1 who x)
      (unless (u1? x) (error (string-append who ": not a u1") x)))

    (define (make-u1vector len . fill)
      (let ((fill (if (pair? fill) (car fill) 0)))
        (check-u1 "make-u1vector" fill)
        (bytes->u1vector (make-bytevector len fill))))

    (define (list->u1vector list)
      (for-each (lambda (x) (check-u1 "list->u1vector" x)) list)
      (bytes->u1vector (apply bytevector list)))

    (define (u1vector . elements) (list->u1vector elements))

    (define (u1vector-length v) (bytevector-length (u1vector-bytes v)))

    (define (u1vector-ref v i) (bytevector-u8-ref (u1vector-bytes v) i))

    (define (u1vector-set! v i x)
      (check-u1 "u1vector-set!" x)
      (bytevector-u8-set! (u1vector-bytes v) i x))

    (define (u1vector->list v . range)
      (let* ((b (u1vector-bytes v))
             (start (if (pair? range) (car range) 0))
             (end (if (and (pair? range) (pair? (cdr range)))
                      (cadr range)
                      (bytevector-length b))))
        (let loop ((i (- end 1)) (acc '()))
          (if (< i start)
              acc
              (loop (- i 1) (cons (bytevector-u8-ref b i) acc))))))))
