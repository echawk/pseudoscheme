;;; Tests for SRFI 137: the SRFI's own test suite
;;; (reference/srfi-137/test.sld), as a program, plus a few more.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 137))

(define (run-tests)
  (test-equal "Type payload"
              'reia
              (let-values
                  (((reia-payload
                     make-reia reia?
                     reia-ref
                     make-reia-subtype)
                    (make-type 'reia)))
                (reia-payload)))

  (test-assert "Disjoint procedures"
               (let-values
                   (((reia-payload1 . reia1*)
                     (make-type 'reia))
                    ((reia-payload2 . reia2*)
                     (make-type 'reia)))
                 (not (eq? reia-payload1 reia-payload2))))

  (test-begin "Type predicates and subtypes")

  (let*-values
      (((reia-payload
         make-reia
         reia?
         reia-ref
         make-reia-subtype)
        (make-type 'reia))
       ((daughter-payload
         make-daughter
         daughter?
         daughter-ref
         make-daughter-subtype)
        (make-reia-subtype 'daughter))
       ((son-payload
         make-son
         son?
         son-ref
         make-son-subtype)
        (make-reia-subtype 'son))
       ((grand-daughter-payload
         make-grand-daughter
         grand-daughter?
         grand-daughter-ref
         make-grand-daughter-subtype)
        (make-daughter-subtype 'grand-daughter)))
    (test-assert "Instance fulfills predicate"
                 (reia? (make-reia #f)))
    (test-assert "Instance of subtype fulfills predicate"
                 (reia? (make-daughter #f)))
    (test-assert "Instance of supertype does not fulfill predicate"
                 (not (daughter? (make-reia #f))))
    (test-assert "Instance of peertype does not fulfill predicate"
                 (not (son? (make-daughter #f))))
    (test-assert "Instance of indirect subtype fulfills predicate"
                 (reia? (make-grand-daughter #f)))
    ;; Not in the SRFI's suite.
    (test-equal "Subtype payload" 'daughter (daughter-payload))
    (test-equal "Subtype accessor on a sub-subtype instance" 'gd
                (daughter-ref (make-grand-daughter 'gd)))
    (test-error "Accessor on an instance of another type" #t
                (daughter-ref (make-son 1)))
    (test-assert "Predicate on a non-instance" (not (reia? 'reia))))

  (test-end)

  (test-equal "Instance payload"
              'payload
              (let-values
                  (((reia-payload
                     make-reia
                     reia?
                     reia-ref
                     make-reia-subtype)
                    (make-type 'reia)))
                (reia-ref (make-reia 'payload)))))

(test-begin "srfi-137")
(run-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-137")
  (exit (if (zero? failures) 0 1)))
