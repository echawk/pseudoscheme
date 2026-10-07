;;; -*- Mode: Scheme -*-
;;;; (chezscheme): threads, mutexes and condition variables, as Chez
;;;; Scheme's threaded builds have them, on bordeaux-threads (the host's
;;;; chez: procedures, src/chez/host.lisp).  Part of the library's body
;;;; (src/chez/chez.lisp).
;;;;
;;;; Not as Chez: thread parameters are ordinary parameters, shared by
;;;; every thread (parameterize assigns the one value, ROADMAP.md 4), and
;;;; psyntax must not expand in two threads at once.

(define fork-thread %chez:fork-thread)
(define thread? %chez:thread?)
(define thread-join %chez:thread-join)
(define get-thread-id %chez:get-thread-id)

(define make-mutex
  (case-lambda (() (%chez:make-mutex #f)) ((name) (%chez:make-mutex name))))
(define mutex? %chez:mutex?)
(define mutex-name %chez:mutex-name)
(define mutex-acquire
  (case-lambda ((m) (%chez:mutex-acquire m #t)) ((m block?) (%chez:mutex-acquire m block?))))
(define mutex-release %chez:mutex-release)

(define-syntax with-mutex
  (syntax-rules ()
    ((_ m body1 body2 ...)
     (let ((mutex m))
       (dynamic-wind
         (lambda () (mutex-acquire mutex))
         (lambda () body1 body2 ...)
         (lambda () (mutex-release mutex)))))))

(define make-condition
  (case-lambda (() (%chez:make-condition #f)) ((name) (%chez:make-condition name))))
(define thread-condition? %chez:condition?)
(define condition-wait
  (case-lambda
    ((c m) (%chez:condition-wait c m #f))
    ((c m timeout) (%chez:condition-wait c m (and timeout (time->seconds timeout))))))
(define condition-signal %chez:condition-signal)
(define condition-broadcast %chez:condition-broadcast)

(define make-thread-parameter make-parameter)

(define (time->seconds t)
  (+ (time-second t) (/ (time-nanosecond t) 1000000000)))

;; (sleep t): T a time object of type time-duration
(define (sleep t) (%chez:sleep-seconds (time->seconds t)))

(define (threaded?) #t)
