#lang racket/base
;; A continuation captured in dynamic-wind's before thunk, re-entered
;; from the after thunk, runs the body again inside the winder
;; (src/continuations.lisp, full-dynamic-wind).  The .expected file is
;; what Racket 9.3 prints.
(define k #f)
(define n 0)
(define log '())
(dynamic-wind
 (lambda () (call/cc (lambda (c) (set! k c))) (set! log (cons 'pre log)))
 (lambda () (set! log (cons 'body log)))
 (lambda ()
   (set! log (cons 'post log))
   (when (< n 1) (set! n (+ n 1)) (k #f))))
(displayln (reverse log))
