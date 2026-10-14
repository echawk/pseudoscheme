#lang racket/base
;; Arities: case-lambda's, procedure-reduce-arity's, and arity errors
(define (show . xs) (for-each (lambda (x) (write x) (display " ")) xs) (newline))
(define f (case-lambda [(x) 1] [(x y z) 3] [(x y z w . r) 'more]))
(show (procedure-arity-mask f) (procedure-arity-includes? f 2) (procedure-arity-includes? f 3)
      (procedure-arity-includes? f 10))
(show (f 1) (f 1 2 3) (f 1 2 3 4 5))
(define g (procedure-reduce-arity (lambda args (length args)) 2))
(show (g 'a 'b) (procedure-arity g) (procedure-arity-mask g))
(define (arity-error thunk) (with-handlers ([exn:fail:contract:arity? (lambda (e) 'arity)]) (thunk)))
(define h (if (zero? (random 1)) (lambda (x) x) #f))
(show (arity-error (lambda () (g 1))) (arity-error (lambda () (f 1 2))) (arity-error (lambda () (h))))
(show (procedure-arity (lambda (x [y 1]) x)) (procedure-arity car) (procedure-arity-mask cons))
