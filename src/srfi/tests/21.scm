;;; Tests for SRFI 21: the SRFI's examples of what it adds to SRFI 18,
;;; and some of SRFI 18's.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 21))
(test-begin "srfi-21")

(thread-base-priority-set! (current-thread) 12.3)
(test-eqv 12.3 (thread-base-priority (current-thread)))
(thread-priority-boost-set! (current-thread) 2.5)
(test-eqv 2.5 (thread-priority-boost (current-thread)))
(thread-quantum-set! (current-thread) 1.5)
(test-eqv 1.5 (thread-quantum (current-thread)))
(thread-quantum-set! (current-thread) 0)
(test-eqv .01 (thread-quantum (current-thread)))
(test-error (thread-priority-boost-set! (current-thread) -1))

;; a new thread has its creator's
(define t (make-thread (lambda () (thread-base-priority (current-thread))) 'worker))
(test-eqv 12.3 (thread-base-priority t))
(thread-base-priority-set! t 7)
(test-eqv 7 (thread-base-priority t))
(test-eqv 12.3 (thread-base-priority (current-thread)))
(test-eq 'worker (thread-name t))
(thread-start! t)
(test-eqv 7 (thread-join! t))

;; SRFI 18's, re-exported
(define m (make-mutex))
(test-eq 'not-abandoned (mutex-state m))
(mutex-lock! m)
(test-eq (current-thread) (mutex-state m))
(mutex-unlock! m)
(test-eqv 5 (thread-join! (thread-start! (make-thread (lambda () (+ 2 3))))))
(test-assert (time? (current-time)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-21")
  (exit (if (zero? failures) 0 1)))
