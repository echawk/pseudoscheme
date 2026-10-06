;;; SRFI 21: Real-time multithreading support.  Written for Pseudoscheme:
;;; SRFI 18's threads (src/srfi/18.sld, on the host's threads), with
;;; SRFI 21's base priority, priority boost and quantum.
;;;
;;; Pseudoscheme's threads are the operating system's, whose scheduler
;;; knows nothing of these: each thread has them, a new one its creator's,
;;; and they can be read and set, but they don't affect scheduling, and
;;; there is no priority inheritance.  A quantum of 0 reads as .01, the
;;; smallest the SRFI's example shows.
(define-library (srfi 21)
  (export current-thread thread? (rename make-thread/21 make-thread) thread-name
          thread-specific thread-specific-set! thread-start!
          thread-yield! thread-sleep! thread-terminate! thread-join!
          thread-base-priority thread-base-priority-set!
          thread-priority-boost thread-priority-boost-set!
          thread-quantum thread-quantum-set!
          mutex? make-mutex mutex-name mutex-specific mutex-specific-set!
          mutex-state mutex-lock! mutex-unlock!
          condition-variable? make-condition-variable
          condition-variable-name condition-variable-specific
          condition-variable-specific-set! condition-variable-signal!
          condition-variable-broadcast!
          current-time time? time->seconds seconds->time
          current-exception-handler
          join-timeout-exception? abandoned-mutex-exception?
          terminated-thread-exception? uncaught-exception?
          uncaught-exception-reason)
  (import (scheme base) (scheme case-lambda) (srfi 18) (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    ;; thread -> #(base-priority priority-boost quantum), weakly
    (define attributes
      (lisp-eval-string "(make-hash-table :test 'eq :weakness :key :synchronized t)"))
    (define %ref (lisp-eval-string "(lambda (table key) (gethash key table ps:false))"))
    (define %set! (lisp-eval-string "(lambda (table key value) (setf (gethash key table) value) ps:unspecific)"))

    (define (thread-attributes thread)
      (unless (thread? thread) (error "not a thread" thread))
      (or (%ref attributes thread)
          (let ((v (vector 0 0 .01)))
            (%set! attributes thread v)
            v)))

    (define make-thread/21
      (case-lambda
        ((thunk) (make-thread/21 thunk #f))
        ((thunk name)
         (let ((thread (if name (make-thread thunk name) (make-thread thunk))))
           (%set! attributes thread (vector-copy (thread-attributes (current-thread))))
           thread))))

    (define (check-real who x nonnegative)
      (unless (and (real? x) (or (not nonnegative) (>= x 0)))
        (error (string-append who ": not a valid value") x)))

    (define (thread-base-priority thread) (vector-ref (thread-attributes thread) 0))
    (define (thread-base-priority-set! thread priority)
      (check-real "thread-base-priority-set!" priority #f)
      (vector-set! (thread-attributes thread) 0 priority))
    (define (thread-priority-boost thread) (vector-ref (thread-attributes thread) 1))
    (define (thread-priority-boost-set! thread boost)
      (check-real "thread-priority-boost-set!" boost #t)
      (vector-set! (thread-attributes thread) 1 boost))
    (define (thread-quantum thread) (vector-ref (thread-attributes thread) 2))
    (define (thread-quantum-set! thread quantum)
      (check-real "thread-quantum-set!" quantum #t)
      (vector-set! (thread-attributes thread) 2 (if (zero? quantum) .01 quantum)))))
