#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2021).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (rnrs (6))
	(srfi :239))

(define type-of
  (lambda (x)
    (list-case x
      [(_ . _) 'pair]
      [() 'null]
      [_ 'atom])))

(assert (eq? 'pair (type-of '(a . b))))
(assert (eq? 'null (type-of '())))
(assert (eq? 'atom (type-of 'x)))

(define fold
  (lambda (proc seed ls)
    (let f ([acc seed] [ls ls])
      (list-case ls
        [(h . t) (f (proc h acc) t)]
        [() acc]
        [_ (assertion-violation 'fold "not a list" ls)]))))

(assert (equal? '(3 2 1 . 0)
                (fold cons 0 '(1 2 3))))

(assert (guard (exc
                [(assertion-violation? exc) #t]
                [else #f])
          (fold cons 0 '(1 2 . 3))))

(assert (list-case '(1 . 2)
          [_ #f]
          [(_ . _) #t]))

(assert (guard (exc
                [(assertion-violation? exc) #t]
                [else #f])
          (list-case 0
            [(_ . _) #f]
            [() #f])))
