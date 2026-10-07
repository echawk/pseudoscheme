;;; -*- Mode: Scheme -*-
;;;; (chezscheme): numbers beyond R6RS, as Chez Scheme 10 has them.  Part
;;;; of the library's body (src/chez/chez.lisp).  Fixnums are SBCL's,
;;;; wider than Chez's (62 bits against 61), as R6RS allows.

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (-1+ n) (- n 1))

(define (nonnegative? x) (>= x 0))
(define (nonpositive? x) (<= x 0))

(define (most-positive-fixnum) (greatest-fixnum))
(define (most-negative-fixnum) (least-fixnum))

(define (bignum? x) (and (exact-integer? x) (not (fixnum? x))))
(define (ratnum? x) (and (rational? x) (exact? x) (not (integer? x))))
(define (exact-integer? x) (and (integer? x) (exact? x)))
(define (cflonum? x) (and (complex? x) (inexact? x)))

;;; Bits, on exact integers of any size

(define (logand . ns) (apply bitwise-and ns))
(define (logior . ns) (apply bitwise-ior ns))
(define logor logior)
(define (logxor . ns) (apply bitwise-xor ns))
(define (lognot n) (bitwise-not n))
(define (logbit? i n) (bitwise-bit-set? n i))
(define (logtest a b) (not (zero? (bitwise-and a b))))
(define (logbit0 i n) (bitwise-and n (bitwise-not (bitwise-arithmetic-shift-left 1 i))))
(define (logbit1 i n) (bitwise-ior n (bitwise-arithmetic-shift-left 1 i)))
(define (ash n k) (bitwise-arithmetic-shift n k))
(define (integer-length n) (bitwise-length n))

(define (isqrt n) (let-values (((s r) (exact-integer-sqrt n))) s))

(define (expt-mod base e m)
  (let loop ((b (modulo base m)) (e e) (acc 1))
    (cond ((= e 0) (modulo acc m))
          ((odd? e) (loop (modulo (* b b) m) (quotient e 2) (modulo (* acc b) m)))
          (else (loop (modulo (* b b) m) (quotient e 2) acc)))))

;;; Fixnums: Chez's names for R6RS's, and its extras

(define (chain-compare op)
  (lambda (a b . more)
    (let loop ((a a) (b b) (more more))
      (and (op a b)
           (or (null? more) (loop b (car more) (cdr more)))))))
(define fx= (chain-compare fx=?))
(define fx< (chain-compare fx<?))
(define fx> (chain-compare fx>?))
(define fx<= (chain-compare fx<=?))
(define fx>= (chain-compare fx>=?))
(define (fx/ a b) (fxquotient a b))
(define (fxnonnegative? n) (fx>=? n 0))
(define (fxnonpositive? n) (fx<=? n 0))
(define (fxlogbit? i n) (fxbit-set? n i))
(define (fxlogtest a b) (not (fxzero? (fxand a b))))
(define (fxlognot n) (fxnot n))
(define fxlogior fxior)
(define (fxsrl n k) (bitwise-arithmetic-shift-right (bitwise-and n (greatest-fixnum)) k))
(define (fxpopcount n) (fxbit-count n))

(define fl= (chain-compare fl=?))
(define fl< (chain-compare fl<?))
(define fl> (chain-compare fl>?))
(define fl<= (chain-compare fl<=?))
(define fl>= (chain-compare fl>=?))
(define (flnonnegative? x) (fl>=? x 0.0))
(define (flnonpositive? x) (fl<=? x 0.0))

;;; Random numbers: (random n) is an exact integer below an exact N, or
;;; an inexact real below an inexact one

(define random %chez:random)
(define random-seed %chez:random-seed)

;;; Hyperbolic functions

(define sinh %chez:sinh)
(define cosh %chez:cosh)
(define tanh %chez:tanh)
(define asinh %chez:asinh)
(define acosh %chez:acosh)
(define atanh %chez:atanh)
