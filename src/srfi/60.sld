;;; SRFI 60: integers as bits.  Written for Pseudoscheme on (srfi 151)
;;; (the SRFI's own implementation, SLIB's logical.scm, computes in
;;; 4-bit nibbles); the SRFI 33 names that SRFI 151 shares with SRFI 60
;;; under the same contract are SRFI 151's very bindings.  The
;;; bits-as-booleans procedures put the most significant bit first, as
;;; SRFI 60 specifies (SRFI 151's bits->list is the other way round).
(define-library (srfi 60)
  (export logand logior logxor lognot logtest logcount log2-binary-factors
          logbit? ash copy-bit-field rotate-bit-field reverse-bit-field
          bitwise-and bitwise-ior bitwise-xor bitwise-not
          bitwise-if bitwise-merge any-bits-set? bit-count
          integer-length first-set-bit bit-set? copy-bit bit-field
          arithmetic-shift
          integer->list list->integer booleans->integer)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 151))
  (begin
    (define logand bitwise-and)
    (define logior bitwise-ior)
    (define logxor bitwise-xor)
    (define lognot bitwise-not)
    (define bitwise-merge bitwise-if)
    (define (any-bits-set? j k) (not (zero? (bitwise-and j k))))
    (define logtest any-bits-set?)
    (define logcount bit-count)
    (define log2-binary-factors first-set-bit)
    (define logbit? bit-set?)
    (define ash arithmetic-shift)
    (define (copy-bit-field to from start end)
      (bit-field-replace to from start end))
    (define (rotate-bit-field n count start end)
      (bit-field-rotate n count start end))
    (define (reverse-bit-field n start end)
      (bit-field-reverse n start end))
    (define integer->list
      (case-lambda
        ((k) (integer->list k (integer-length k)))
        ((k len)
         (do ((i 0 (+ i 1))
              (k k (arithmetic-shift k -1))
              (bits '() (cons (odd? k) bits)))
             ((= i len) bits)))))
    (define (list->integer bools)
      (do ((bools bools (cdr bools))
           (n 0 (+ (* 2 n) (if (car bools) 1 0))))
          ((null? bools) n)))
    (define (booleans->integer . bools) (list->integer bools))))
