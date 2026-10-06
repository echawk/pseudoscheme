;;; SRFI 208: NaN procedures.  Written for Pseudoscheme: inexact reals are
;;; IEEE doubles, and a NaN's sign, quiet bit and payload are its bits
;;; (float-features' DOUBLE-FLOAT-BITS and BITS-DOUBLE-FLOAT): bit 63 the
;;; sign, bits 62-52 all ones, bit 51 quiet, bits 50-0 the payload.  The
;;; SRFI's sample implementation is C, with a Chibi interface.
(define-library (srfi 208)
  (export make-nan nan-negative? nan-quiet? nan-payload nan=?)
  (import (scheme base) (scheme inexact)
          (prefix (cl float-features) ff:))
  (begin
    (define payload-bits 51)

    (define (nan-bits x)
      (unless (and (real? x) (nan? x)) (error "not a NaN" x))
      (ff:double-float-bits x))

    (define (make-nan negative? quiet? payload . float)
      (unless (and (exact-integer? payload) (>= payload 0) (< payload (expt 2 payload-bits)))
        (error "make-nan: payload out of range" payload))
      (when (and (not quiet?) (zero? payload))
        (error "make-nan: a signalling NaN needs a nonzero payload"))
      (let ((bits (+ (if negative? (expt 2 63) 0)
                     (* #x7FF (expt 2 52))
                     (if quiet? (expt 2 51) 0)
                     payload)))
        (ff:bits-double-float bits)))

    (define (nan-negative? nan) (>= (nan-bits nan) (expt 2 63)))
    (define (nan-quiet? nan) (odd? (quotient (nan-bits nan) (expt 2 51))))
    (define (nan-payload nan) (remainder (nan-bits nan) (expt 2 payload-bits)))
    (define (nan=? a b) (= (nan-bits a) (nan-bits b)))))
