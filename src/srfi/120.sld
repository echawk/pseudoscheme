;;; SRFI 120: Timer APIs.  Takashi Kato's sample implementation
;;; (reference/srfi-120/timer.sld; 2-clause BSD licence), its body
;;; included from timer-body.scm, on SRFIs 18, 19 and 69.  Changes:
;;;
;;; - The sample imports SRFI 114 for three procedures, defined here
;;;   instead: make-comparator, comparator-compare, comparator-equal?
;;;   (and make-comparison=/<).
;;; - SRFI 18's current-time and time? are left out: the sample uses
;;;   SRFI 19's, which share the names.  Its timeouts are SRFI 19 times,
;;;   so mutex-unlock! is wrapped to take them.
;;; - Its task<? compared tasks' times with time=?, so the queue wasn't
;;;   ordered; it uses time<?.
;;; - Its wait-cv relocked the mutex only if the wait didn't time out,
;;;   so the worker went on without it and then unlocked it, an error
;;;   that ended the worker thread; it relocks either way.
;;; - timer-reschedule! of a task while it ran raised an error (the task
;;;   isn't in the queue then); the worker queues it at its new time
;;;   when it has run.
(define-library (srfi 120)
  (export make-timer timer?
          timer-cancel!
          timer-schedule! timer-reschedule!
          timer-task-remove! timer-task-exists?
          make-timer-delta timer-delta?)
  (import (scheme base) (scheme case-lambda)
          (rename (except (srfi 18) current-time time?)
                  (mutex-unlock! srfi-18:mutex-unlock!))
          (srfi 19) (srfi 69))
  (begin
    ;; the part of SRFI 114 the sample uses
    (define (make-comparator type-test equality compare hash)
      (vector type-test equality compare hash))
    (define (comparator-equal? c a b) ((vector-ref c 1) a b))
    (define (comparator-compare c a b) ((vector-ref c 2) a b))
    (define (make-comparison=/< = <)
      (lambda (a b) (cond ((= a b) 0) ((< a b) -1) (else 1))))

    ;; The sample's timeouts are SRFI 19 times; SRFI 18's are its own
    ;; times or seconds from now.
    (define (seconds-until t)
      (let ((d (time-difference t (current-time))))
        (max 0 (+ (time-second d) (/ (time-nanosecond d) 1e9)))))
    (define mutex-unlock!
      (case-lambda
        ((m) (srfi-18:mutex-unlock! m))
        ((m c) (srfi-18:mutex-unlock! m c))
        ((m c timeout)
         (srfi-18:mutex-unlock! m c (if (time? timeout) (seconds-until timeout) timeout))))))
  (include "reference/srfi-120/timer-body.scm"))
