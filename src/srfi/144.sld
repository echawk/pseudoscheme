;;; SRFI 144: flonums.  William D Clinger's portable sample
;;; implementation (reference/srfi-144/, MIT licence, unmodified), on
;;; R6RS's (rnrs arithmetic flonums), which Pseudoscheme implements
;;; natively.  A flonum is a Common Lisp double-float.
;;;
;;; The sample implementation finds a float's exponent and neighbours
;;; with logarithms and search loops.  The procedures below that start
;;; with % replace those with exact Common Lisp operations on the
;;; float (decode-float, float-sign, and float-features' access to the
;;; IEEE bits), and are exported under the SRFI's names; the sample
;;; implementation's own definitions stay private.  This also corrects
;;; its flnormalized? and fldenormalized?, which put the boundary at
;;; 1/fl-greatest instead of the least normalized double.
;;;
;;; There is no fused multiply-add: fl+* is the sample implementation's
;;; (exact when the result is finite), and fl-fast-fl+* is #f.
(define-library (srfi 144)
  (export

   ;; Mathematical Constants

   fl-e
   fl-1/e
   fl-e-2
   fl-e-pi/4
   fl-log2-e
   fl-log10-e
   fl-log-2
   fl-1/log-2
   fl-log-3
   fl-log-pi
   fl-log-10
   fl-1/log-10
   fl-pi
   fl-1/pi
   fl-2pi
   fl-pi/2
   fl-pi/4
   fl-2/sqrt-pi
   fl-pi-squared
   fl-degree
   fl-2/pi
;  fl-2/sqrt-pi    ; FIXME: duplicate
   fl-sqrt-2
   fl-sqrt-3
   fl-sqrt-5
   fl-sqrt-10
   fl-1/sqrt-2
   fl-cbrt-2
   fl-cbrt-3
   fl-4thrt-2
   fl-phi
   fl-log-phi
   fl-1/log-phi
   fl-euler
   fl-e-euler
   fl-sin-1
   fl-cos-1
   fl-gamma-1/2
   fl-gamma-1/3
   fl-gamma-2/3

   ;; Implementation Constants

   fl-greatest
   fl-least
   fl-epsilon
   fl-fast-fl+*
   fl-integer-exponent-zero
   fl-integer-exponent-nan

   ;; Constructors

   flonum
   (rename %fladjacent fladjacent)
   flcopysign
   make-flonum

   ;; Accessors

   flinteger-fraction
   (rename %flexponent flexponent)
   (rename %flinteger-exponent flinteger-exponent)
   (rename %flnormalized-fraction-exponent flnormalized-fraction-exponent)
   (rename %flsign-bit flsign-bit)

   ;; Predicates

   flonum?
   fl=?
   fl<?
   fl>?
   fl<=?
   fl>=?
   flunordered?
   flmax
   flmin
   flinteger?
   flzero?
   flpositive?
   flnegative?
   flodd?
   fleven?
   flfinite?
   flinfinite?
   flnan?
   (rename %flnormalized? flnormalized?)
   (rename %fldenormalized? fldenormalized?)

   ;; Arithmetic

   fl+
   fl*
   fl+*
   fl-
   fl/
   flabs
   flabsdiff
   flposdiff
   flsgn
   flnumerator
   fldenominator
   flfloor
   flceiling
   flround
   fltruncate

   ;; Exponents and logarithsm

   flexp
   flexp2
   flexp-1
   flsquare
   flsqrt
   flcbrt
   flhypot
   flexpt
   fllog
   fllog1+
   fllog2
   fllog10
   make-fllog-base

   ;; Trigonometric functions

   flsin
   flcos
   fltan
   flasin
   flacos
   flatan
   flsinh
   flcosh
   fltanh
   flasinh
   flacosh
   flatanh

   ;; Integer division

   flquotient
   flremainder
   flremquo

   ;; Special functions

   flgamma
   flloggamma
   flfirst-bessel
   flsecond-bessel
   flerf
   flerfc
   )

  (import (scheme base)
          (scheme write)
          (scheme inexact)
          (except (rnrs arithmetic flonums)
                  flmax flmin flnumerator fldenominator)
          (prefix (only (cl common-lisp) decode-float
                        least-positive-normalized-double-float
                        most-positive-fixnum ash)
                  cl:)
          (prefix (only (cl float-features) double-float-bits
                        bits-double-float)
                  ff:)
          (prefix (only (rnrs arithmetic flonums)
                        flnumerator fldenominator)
                  r6rs:))
  (include "reference/srfi-144/144.constants.scm")
  (include "reference/srfi-144/144.body0.scm")
  (include "reference/srfi-144/144.body.scm")
  (include "reference/srfi-144/144.special.scm")
  (begin
    (define (%flsign-bit x)
      (check-flonum! 'flsign-bit x)
      (cl:ash (ff:double-float-bits x) -63))

    (define (%flnormalized? x)
      (check-flonum! 'flnormalized? x)
      (let ((x (flabs x)))
        (and (flfinite? x)
             (fl<=? cl:least-positive-normalized-double-float x))))

    (define (%fldenormalized? x)
      (check-flonum! 'fldenormalized? x)
      (let ((x (flabs x)))
        (and (fl<? 0.0 x)
             (fl<? x cl:least-positive-normalized-double-float))))

    ;; C's nextafter: the IEEE bits, as a sign and a magnitude, step by
    ;; one toward y.
    (define (%fladjacent x y)
      (check-flonum! 'fladjacent x)
      (check-flonum! 'fladjacent y)
      (cond ((flnan? x) x)
            ((flnan? y) y)
            ((fl=? x y) y)
            ((flzero? x) (if (fl<? x y) fl-least (fl- fl-least)))
            (else
             (let ((bits (ff:double-float-bits x)))
               (ff:bits-double-float
                (if (eq? (fl<? x y) (fl<? 0.0 x)) (+ bits 1) (- bits 1)))))))

    ;; C's logb.
    (define (%flexponent x)
      (check-flonum! 'flexponent x)
      (cond ((flnan? x) x)
            ((flinfinite? x) +inf.0)
            ((flzero? x) -inf.0)
            (else (inexact (%exponent x)))))

    ;; The exponent of a finite non-zero x, for a significand in [1, 2).
    (define (%exponent x)
      (call-with-values (lambda () (cl:decode-float x))
        (lambda (significand exponent sign) (- exponent 1))))

    ;; C's ilogb.
    (define (%flinteger-exponent x)
      (check-flonum! 'flinteger-exponent x)
      (cond ((flnan? x) fl-integer-exponent-nan)
            ((flinfinite? x) cl:most-positive-fixnum)
            ((flzero? x) fl-integer-exponent-zero)
            (else (%exponent x))))

    ;; C's frexp, for finite non-zero x; the unspecified cases are the
    ;; sample implementation's.
    (define (%flnormalized-fraction-exponent x)
      (check-flonum! 'flnormalized-fraction-exponent x)
      (if (and (flfinite? x) (not (flzero? x)))
          (call-with-values (lambda () (cl:decode-float x))
            (lambda (significand exponent sign)
              (values (fl* sign significand) exponent)))
          (flnormalized-fraction-exponent x)))

    (define c-functions-are-available #f)
         (define fl-fast-fl+* #f)
         (define (fma x y z) (error "fma not defined"))
         (define (jn n x) (error "jn not defined"))
         (define (yn n x) (error "yn not defined"))))
