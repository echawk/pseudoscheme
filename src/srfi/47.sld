;;; SRFI 47: array.  The SRFI's implementation is SLIB's array.scm,
;;; the same code that implements SRFI 63, which supersedes this one; so
;;; this library re-exports (srfi 63)'s procedures (see 63.sld, and
;;; reference/srfi-63/ for the code and its licence).  SRFI 47's
;;; uniform-array prototypes, ac64 through at1, are written here, after
;;; the pattern of SRFI 63's A:... prototypes: each checks its optional
;;; argument and, since no uniform array types are provided, returns a
;;; vector, which the SRFI permits ("resorting finally to vector").
;;;
;;; equal? (which compares arrays) clashes with R7RS's, as in SRFI 63:
;;; (import (except (scheme base) equal?) (srfi 47)).
(define-library (srfi 47)
  (export array? equal? make-array make-shared-array array-rank
          array-dimensions array-in-bounds? array-ref array-set!
          ac64 ac32 ar64 ar32 as64 as32 as16 as8 au64 au32 au16 au8 at1)
  (import (except (scheme base) equal?)
          (srfi 63))
  (begin
    (define (prototype name ok?)
      (lambda args
        (cond ((null? args) (vector))
              ((and (null? (cdr args)) (ok? (car args))) (vector (car args)))
              ((null? (cdr args)) (error "incompatible type" name (car args)))
              (else (error "wrong number of arguments" name args)))))

    ;; Exact integers that fit in N bytes; signed when N is negative.
    (define (bytes n)
      (let ((bits (* 8 (abs n))))
        (lambda (x)
          (and (exact-integer? x)
               (if (negative? n)
                   (and (<= (- (expt 2 (- bits 1))) x)
                        (< x (expt 2 (- bits 1))))
                   (and (<= 0 x) (< x (expt 2 bits))))))))

    (define ac64 (prototype 'ac64 complex?))
    (define ac32 (prototype 'ac32 complex?))
    (define ar64 (prototype 'ar64 real?))
    (define ar32 (prototype 'ar32 real?))
    (define as64 (prototype 'as64 (bytes -8)))
    (define as32 (prototype 'as32 (bytes -4)))
    (define as16 (prototype 'as16 (bytes -2)))
    (define as8 (prototype 'as8 (bytes -1)))
    (define au64 (prototype 'au64 (bytes 8)))
    (define au32 (prototype 'au32 (bytes 4)))
    (define au16 (prototype 'au16 (bytes 2)))
    (define au8 (prototype 'au8 (bytes 1)))
    (define at1 (prototype 'at1 boolean?))))
