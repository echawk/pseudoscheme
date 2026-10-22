#lang racket/base
(require (prefix-in cl: cl/common-lisp))
(define (squares xs) (cl:loop for x in xs collect (* x x)))
(displayln (squares '(1 2 3)))
(displayln (cl:remove-if-not even? '(1 2 3 4)))
(set! cl:*print-base* 16)
(displayln (cl:format #f "~a" 255))
