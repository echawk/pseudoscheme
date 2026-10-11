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

;;; cfl operations: on flonums and inexact complex numbers

(define (check-cflonum who x)
  (unless (cflonum? x) (assertion-violation who "not a cflonum" x)))
(define (check-cflonums who xs) (for-each (lambda (x) (check-cflonum who x)) xs))
(define (cfl-real-part x) (check-cflonum 'cfl-real-part x) (inexact (real-part x)))
(define (cfl-imag-part x) (check-cflonum 'cfl-imag-part x) (if (flonum? x) 0.0 (imag-part x)))
(define (cfl-conjugate x)
  (check-cflonum 'cfl-conjugate x)
  (if (flonum? x) x (make-rectangular (real-part x) (fl- (imag-part x)))))
(define (cfl-magnitude-squared x)
  (check-cflonum 'cfl-magnitude-squared x)
  (let ((r (cfl-real-part x)) (i (cfl-imag-part x)))
    (fl+ (fl* r r) (fl* i i))))
(define (fl-make-rectangular x y)
  (unless (flonum? x) (assertion-violation 'fl-make-rectangular "not a flonum" x))
  (unless (flonum? y) (assertion-violation 'fl-make-rectangular "not a flonum" y))
  (make-rectangular x y))
(define (cfl+ . xs) (check-cflonums 'cfl+ xs) (if (null? xs) 0.0 (apply + xs)))
(define (cfl* . xs) (check-cflonums 'cfl* xs) (if (null? xs) 1.0 (apply * xs)))
(define (cfl- x . xs) (check-cflonums 'cfl- (cons x xs)) (apply - x xs))
(define (cfl/ x . xs) (check-cflonums 'cfl/ (cons x xs)) (apply / x xs))
(define (cfl= x . xs) (check-cflonums 'cfl= (cons x xs)) (apply = x x xs))

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
  (lambda (a . more)
    (if (null? more)
        (begin (op a a) #t)             ;checks a's type
        (let loop ((a a) (b (car more)) (more (cdr more)))
          (and (op a b)
               (or (null? more) (loop b (car more) (cdr more))))))))
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
(define fxpopcount32 %chez:fxpopcount32)
(define fxpopcount16 %chez:fxpopcount16)
(define fxlogbit0 %chez:fxlogbit0)
(define fxlogbit1 %chez:fxlogbit1)
(define fx+/wraparound %chez:fx+/wraparound)
(define fx-/wraparound %chez:fx-/wraparound)
(define fx*/wraparound %chez:fx*/wraparound)
(define fxsll/wraparound %chez:fxsll/wraparound)
(define flbit-field %chez:flbit-field)

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
