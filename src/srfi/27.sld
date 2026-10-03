;;; SRFI 27: sources of random bits.  Sebastian Egner's reference
;;; implementation (reference/srfi-27/), the integer-only variant
;;; (mrg32k3a-a.scm + mrg32k3a.scm), unmodified; what its Scheme 48
;;; package file (srfi-27-a.scm) supplied -- the record type and a clock
;;; -- is defined here.
(define-library (srfi 27)
  (export random-integer random-real default-random-source
          make-random-source random-source? random-source-state-ref
          random-source-state-set! random-source-randomize!
          random-source-pseudo-randomize! random-source-make-integers
          random-source-make-reals)
  (import (scheme base) (scheme time)
          (only (scheme r5rs) exact->inexact inexact->exact))
  (begin
    (define-record-type :random-source
      (:random-source-make state-ref state-set! randomize! pseudo-randomize!
                           make-integers make-reals)
      :random-source?
      (state-ref :random-source-state-ref)
      (state-set! :random-source-state-set!)
      (randomize! :random-source-randomize!)
      (pseudo-randomize! :random-source-pseudo-randomize!)
      (make-integers :random-source-make-integers)
      (make-reals :random-source-make-reals))
    (define (:random-source-current-time)
      (+ (exact (floor (current-second))) (current-jiffy))))
  (include "reference/srfi-27/mrg32k3a-a.scm" "reference/srfi-27/mrg32k3a.scm"))
