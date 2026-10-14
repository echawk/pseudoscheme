#lang racket/base
;; A module run as racket runs a file: its body, then its main submodule.
(define (greet name) (string-append "hello, " name))
(displayln (greet "world"))
(module+ main
  (printf "~a squares: ~a\n" 5 (for/list ([i 5]) (* i i))))
