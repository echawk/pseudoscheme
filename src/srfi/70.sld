;;; SRFI 70: numbers.  Written for Pseudoscheme from the SRFI document
;;; (there is no reference implementation).  SRFI 70 restates R5RS
;;; section 6.2 with IEEE infinities, which Pseudoscheme's arithmetic
;;; already has (+inf.0, -inf.0; +nan.0 is what SRFI 70 writes 0/0), so
;;; almost every binding here is R7RS's own, re-exported.  Defined here:
;;;
;;;  - exact-floor, exact-ceiling, exact-truncate, exact-round, which
;;;    SRFI 70 adds;
;;;  - gcd and lcm, extended to exact rationals:
;;;    (gcd 1/6 1/4) => 1/12, (lcm 1/6 1/4) => 1/2;
;;;  - expt, where an exact zero raised to a power with negative real
;;;    part is +inf.0 rather than an error, and to a power with positive
;;;    real part is 0 (0.0 for an inexact power);
;;;  - exact->inexact and inexact->exact, the R5RS names, which are
;;;    R7RS's inexact and exact.
;;;
;;; gcd, lcm and expt therefore clash with (scheme base)'s; import one
;;; or the other with except.  SRFI 70's 0/0 notation for a NaN is not
;;; read; Pseudoscheme reads and writes +nan.0.
(define-library (srfi 70)
  (export
   number? complex? real? rational? integer?
   exact? inexact? = < > <= >=
   finite? infinite? zero? positive? negative? odd? even?
   max min + * - / abs quotient remainder modulo
   gcd lcm numerator denominator
   floor ceiling truncate round
   exact-floor exact-ceiling exact-truncate exact-round
   rationalize exp log sin cos tan asin acos atan sqrt expt
   make-rectangular make-polar real-part imag-part magnitude angle
   exact->inexact inexact->exact
   number->string string->number)
  (import (rename (scheme base)
                  (gcd r7:gcd) (lcm r7:lcm) (expt r7:expt))
          (scheme inexact)
          (scheme complex))
  (begin
    (define (exact-floor x) (exact (floor x)))
    (define (exact-ceiling x) (exact (ceiling x)))
    (define (exact-truncate x) (exact (truncate x)))
    (define (exact-round x) (exact (round x)))

    (define exact->inexact inexact)
    (define inexact->exact exact)

    ;; gcd of rationals: gcd of the numerators over lcm of the
    ;; denominators; lcm the other way round.
    (define (rational-op int-op num-op den-op)
      (lambda args
        (if (every-integer? args)
            (apply int-op args)
            (let loop ((args (cdr args)) (acc (car args)))
              (if (null? args)
                  (abs acc)
                  (let* ((x (car args))
                         (inexact-result? (or (inexact? x) (inexact? acc)))
                         (a (exact acc))
                         (b (exact x))
                         (r (/ (num-op (numerator a) (numerator b))
                               (den-op (denominator a) (denominator b)))))
                    (loop (cdr args) (if inexact-result? (inexact r) r))))))))
    (define (every-integer? xs)
      (or (null? xs) (and (integer? (car xs)) (every-integer? (cdr xs)))))
    (define gcd (rational-op r7:gcd r7:gcd r7:lcm))
    (define lcm (rational-op r7:lcm r7:lcm r7:gcd))

    (define (expt z1 z2)
      (if (and (exact? z1) (zero? z1))
          (let ((r (real-part z2)))
            (cond ((zero? z2) (if (exact? z2) 1 1.0))
                  ((positive? r) (if (exact? z2) 0 0.0))
                  ((negative? r) +inf.0)
                  (else (r7:expt z1 z2))))
          (r7:expt z1 z2)))))
