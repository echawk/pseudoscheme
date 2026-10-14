#lang racket/base
;; Libraries beyond racket/base: racket/list, racket/string, racket/match.
(require racket/list racket/string racket/match)
(displayln (take (range 10) 3))
(displayln (string-join (map string-upcase (string-split "a b c")) "-"))
(displayln (match '(1 (2 3) 4)
             [(list a (list b c) d) (+ a b c d)]))
(displayln (match (vector 'point 3 4)
             [(vector 'point x y) (sqrt (+ (* x x) (* y y)))]))
(displayln (remove-duplicates '(1 2 1 3 2)))
(displayln (append* '((1 2) (3) (4 5))))
