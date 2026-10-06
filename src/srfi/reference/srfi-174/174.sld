(define-library (srfi 174)

(import (scheme base))

(export timespec timespec? timespec-seconds timespec-nanoseconds
        timespec=? timespec<? timespec-hash
        timespec->inexact inexact->timespec)
(begin

(define-record-type <timespec>
  (timespec seconds nanoseconds)
  timespec?
  (seconds timespec-seconds)
  (nanoseconds timespec-nanoseconds))

;; sample implementation using pairs
; (define timespec cons)
; (define timespec-seconds car)
; (define timespec-nanoseconds cdr)
;
; (define (timespec? obj)
;   (and (pair? obj)
;        (exact-integer? (car obj))
;        (exact-integer? (cdr obj))
;        (<= 0 (cdr obj) (- #e1e9 1))))

(define (timespec=? a b)
  (and (= (timespec-seconds a) (timespec-seconds b))
       (= (timespec-nanoseconds a) (timespec-nanoseconds b))))

(define (timespec<? a b)
  (let ((asecs (timespec-seconds a))
        (bsecs (timespec-seconds b))
        (ansecs (timespec-nanoseconds a))
        (bnsecs (timespec-nanoseconds b)))
    (cond
      ((< asecs bsecs) #t)
      ((> asecs bsecs) #f)
      ((negative? asecs) (> ansecs bnsecs))
      (else (< ansecs bnsecs)))))

(define (timespec-hash a)
  (+ (* (abs (timespec-seconds a)) #e1e9) (timespec-nanoseconds a)))

(define (timespec->inexact timespec)
  (let ((secs (timespec-seconds timespec))
        (nsecs (timespec-nanoseconds timespec)))
    (if (negative? secs)
      (- secs (/ nsecs #i1e9))
      (+ secs (/ nsecs #i1e9)))))

(define (inexact->timespec inex)
  (let* ((quo (exact (truncate inex)))
         (rem (exact (- inex quo))))
    (timespec quo (exact (truncate (* (abs rem) #e1e9))))))
))
