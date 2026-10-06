;;; Tests for SRFI 270: the sample implementation's test.scm (its fixed
;;; cases; the random round trips are done here with write-hexadecimal-float
;;; on a sample of doubles), and the SRFI's examples.
(import (scheme base) (scheme read) (scheme write) (scheme complex) (scheme process-context)
        (srfi 64) (srfi 270))
(define (r s) (read (open-input-string s)))
(define (hex x) (let ((p (open-output-string))) (write-hexadecimal-float x p) (get-output-string p)))
(test-begin "srfi-270")

(test-eqv 9 (r "#e#x1.2p3"))
(test-eqv 4608 (r "#e#x9p9"))
(test-eqv 65279/128 (r "#e#xFE.FFp1"))
(test-eqv -5/32 (r "#e#x-0.Ap-2"))
(test-eqv 25/8+32i (r "#e#x1.9p1+10p1i"))
(test-eqv 3.141592653589793116 (r "#x1.921fb54442d18pd+1"))
(test-eqv 65279/256 (r "#e#xFE.FF"))
(test-eqv (make-polar 32 130799/1024) (r "#x1p5@1.FEEFp6"))
(test-eqv +63/2i (r "#e#x+3.Fp3i"))
(test-eqv 63/2+i (r "#e#x3.Fp3+i"))

;; examples
(test-eqv 4608.0 #x9p9)
(test-eqv 9.0 #x1.2p3)
(test-eqv -0.15625 #x-0.Ap-2)
(test-assert (inexact? #x1.8))
(test-eqv 1.5 #x1.8)
(test-eqv 255 #xFF)
(test-eqv 0.5 (string->number "1p-1" 16))
(test-eqv 2.0 (string->number "#x.8p2"))
(test-eqv #f (string->number "1.8p" 16))
(test-eqv #f (string->number "1.g" 16))

;; write-hexadecimal-float
(test-equal "1.8p3" (hex 12.0))
(test-equal "1.p0" (hex 1.0))
(test-equal "-1.p-1" (hex -0.5))
(test-equal "0p0" (hex 0.0))
(test-equal "-0p0" (hex -0.0))
(test-equal "+inf.0" (hex +inf.0))
(test-equal "1.8p0+1.p1i" (hex 1.5+2.0i))
(test-assert "round trips"
  (let loop ((xs (list 1.0 -2.5 3.141592653589793 1e-310 6.02e23 -7.25e-100
                       4.9406564584124654e-324 1.7976931348623157e308)))
    (or (null? xs)
        (and (eqv? (car xs) (string->number (hex (car xs)) 16))
             (loop (cdr xs))))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-270")
  (exit (if (zero? failures) 0 1)))
