#lang racket/base
(require racket/pretty)
(define (nest n) (if (zero? n) '(leaf "a string" 12345 symbol) (list 'node n (nest (sub1 n)) (nest (sub1 n)))))
(pretty-print (nest 4))
(pretty-write '(define (f x) (let loop ([i 0] [acc '()]) (if (< i x) (loop (add1 i) (cons (* i i) acc)) (reverse acc)))))
(parameterize ([pretty-print-columns 40]) (pretty-print (vector 1 2 3 (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p))))
