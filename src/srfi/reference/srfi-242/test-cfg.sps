#!r6rs

;; © 2022 Marc Nieper-Wißkirchen.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including
;; the next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (rnrs)
        (srfi :242))

(assert (equal? 1
                (cfg (halt)
                  1)))

(assert (equal? 2
                (cfg (finally (x) 2 (halt))
                  x)))

(assert (equal? '(1 . 2)
                (let ([x 2])
                  (cfg (finally (x) (cons 1 x)
                         (halt))
                    x))))

(assert (equal? '(3 . 3)
                (cfg
                    (labels ([l (finally (x) 3 (halt))])
                      (finally (x) (cons x x)
                        (call l)))
                  x)))

(assert (equal? '(4 . 5)
                (cfg (execute
                         (lambda (e)
                           (e 4))
                       [(x) (finally (x) (cons x 5) (halt))])
                  x)))

(assert (equal? '(6 . 7)
                (let ([x 6])
                  (cfg (execute
                           (lambda (e)
                             (e (cons x 7)))
                         [(x) (finally (x) x (halt))])
                    x))))

(assert (equal? 7
                (let ([x 7])
                  (cfg (execute
                           (lambda (e1 e2)
                             (e1))
                         [() (finally (x) x (halt))]
                         [(x) (finally (x) x (halt))])
                    x))))

(assert (equal? '(8 . 10)
                (let ([x 8])
                  (cfg (labels ([l (finally (x) (cons x y) (halt))])
                         (execute (lambda (e1 e2)
                                    (e2 9 10))
                           [(y) (call l)]
                           [(x y) (call l)]))
                    x))))

(assert (equal? '(12 . 13)
                (cfg (execute (lambda (e1)
                                (e1 11))
                       [(x) (finally (x) (cons x 13)
                              (finally (x) (+ x 1) (halt)))])
                  x)))

(assert (equal? 14
                (cfg (permute ([l (call l)])
                       (finally (x) 14 (halt)))
                  x)))

(assert (equal? '(15 . 15)
                (let ([x 15])
                  (cfg (permute ([l (execute
                                        (lambda (e)
                                          (e x))
                                      [(y) (call l)])])
                         (finally (x) (cons y x)
                           (halt)))
                    x))))

(assert (equal? '(15 . 16)
                (let ([x 15])
                  (cfg (permute ([l (execute
                                        (lambda (e)
                                          (e 16))
                                      [(x) (call l)])])
                         (permute ([l (execute
                                          (lambda (e)
                                            (e x))
                                        [(y) (call l)])])
                           (finally (x) (cons y x)
                             (halt))))
                    x))))

(assert (equal? '(20 (0 . 17) (17 18 20))
                (let ([x 17]
                      [z 0])
                  (cfg
                      (permute ([l1 (execute (lambda (e)
                                               (e 21))
                                      [(z) (call l1)])])
                        (permute ([l1 (permute ([l2 (execute
                                                        (lambda (e)
                                                          (e 18))
                                                      [(x) (call l2)])])
                                        (permute ([l2 (execute
                                                          (lambda (e)
                                                            (e x 20))
                                                        [(y z) (call l2)])])
                                          (execute (lambda (e)
                                                     (e (list y x z)))
                                            [(x) (call l1)])))])
                          (permute ([l1 (execute
                                            (lambda (e)
                                              (e (cons z x)))
                                          [(y) (call l1)])])
                            (finally (x) (list z y x)
                              (halt)))))
                    x))))

(assert (equal? 21
                (cfg (labels ([l (finally (x) x (halt))])
                       (permute ([l (execute
                                        (lambda (e)
                                          (e 21))
                                      [(x) (call l)])])
                         (call l)))
                  x)))

(assert (equal? '(21 . 22)
                (cfg (label* [(l (finally (x) 21 (halt)))]
                       (label* [(l (finally (x) (cons x 22)
                                     (call l)))]
                         (call l)))
                  x)))

(assert (equal? '(23 24)
                (let ([x 23])
                  (cfg (label* [(l (permute ([l (execute (lambda (e)
                                                           (e x))
                                                  [(y) (call l)])])
                                     (finally (x) (list y x) (halt))))]
                         (permute ([l (execute (lambda (e)
                                                 (e 24))
                                        [(x) (call l)])])
                           (call l)))
                    x))))

(assert (equal? '(25 . 26)
                (let ([x 25])
                  (cfg (permute ([l (execute
                                        (lambda (e)
                                          (e 26))
                                      [(x) (call l)])]
                                 [l (execute
                                        (lambda (e)
                                          (e x))
                                      [(y) (call l)])])
                         (finally (x) (cons y x)
                           (halt)))
                    x))))

(define-cfg-syntax simple-bind
  (lambda (stx)
    (syntax-case stx ()
      [(_ [(id init) ...] cfg)
       (for-all identifier? #'(id ...))
       #'(execute (lambda (e)
                    (e init ...))
           [(id ...) cfg])])))

(assert (equal? 27
                (let ([x 26])
                  (cfg (simple-bind [(x (fx+ x 1))] (finally (x) x (halt)))
                    x))))

(assert (equal? '30
                (cfg (simple-bind [(x 10)] (finally (y) x (finally (x) (+ x 20) (halt))))
                  y)))

(assert (equal? '(49 (1 . 28) (3 . (1 . 28)) (5 . (1 . 28)))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   [(l (simple-bind [(x (cons 1 x))] (call l)))]
                                 (simple-bind [(z (cons 3 x))] (call l)))])
                         (simple-bind [(u (cons 5 x))]
                               (finally (x) (list 49 x z u) (halt))))
                    x))))

(assert (equal? '((1 . 28) (3 . (1 . 28)) (5 . (1 . 28)))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   ([l (permute
                                           [(l (simple-bind [(x (cons 1 x))] (call l)))]
                                         (simple-bind [(z (cons 3 x))] (call l)))])
                                 (simple-bind [(u (cons 5 x))] (call l)))])
                         (finally (x) (list x z u) (halt)))
                    x))))

(assert (equal? '50
                (cfg (permute
                         ([l (simple-bind [(z 50)] (call l))]
                          [l (call l)])
                       (finally (x) z (halt)))
                  x)))

(assert (equal? '((1 . 28) (2 . 28) (3 . (1 . 28)) (4 . 28) (5 . (1 . 28)) (6 . 28))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   ([l (permute
                                           [(l (simple-bind [(x (cons 1 x))] (call l)))
                                            (l (simple-bind [(y (cons 2 x))] (call l)))]
                                         (simple-bind [(z (cons 3 x))] (call l)))]
                                    [l (simple-bind [(w (cons 4 x))] (call l))])
                                 (simple-bind [(u (cons 5 x))] (call l)))]
                            [l (simple-bind [(v (cons 6 x))] (call l))])
                         (finally (x) (list x y z w u v) (halt)))
                    x))))

(assert (equal? '(2 30)
                (cfg (simple-bind ([x 1]) (labels ([l (permute ([p (simple-bind ([y 30]) (call p))]
                                                         [p (execute
                                                                (lambda (e1 e2)
                                                                  (if (eqv? x 1)
                                                                      (e1 2)
                                                                      (e2)))
                                                              [(x) (call l)]
                                                              [() (call p)])])
                                                 (finally (x) (list x y) (halt)))])
                                     (call l)))
                  x)))

;;; Permute and finally variables.

(assert (equal? 45
                (let ([x 35])
                  (cfg (permute ([p (finally (x) (+ 10 x) (call p))]
                                 [p (finally (x) (+ 10 x) (call p))])
                         (halt))
                    x))))

;;; Bind

(assert (equal? '((1 2) 4)
                (cfg (simple-bind ([x 1]) (bind ([x (values x 2)] [(y) (+ x 3)])
                                     (finally (x) (list x y) (halt))))
                  x)))

;;; Label*

(assert (equal? 1
                (cfg (label* ([p (finally (x) 1 (halt))]
                              [p (call p)])
                       (call p))
                  x)))

;;; Examples from specification

(define ex1
  (lambda (n*)
    (cfg (labels
             [(f (execute
                     (lambda (e1 e2)
                       (if (null? n*)
                           (e2)
                           (e1 (car n*) (cdr n*))))
                   [(n n*)
                    (execute
                        (lambda (e1 e2)
                          (if (odd? n)
                              (e2 (+ o 1))
                              (e1 (+ e 1))))
                      [(e) (call f)]
                      [(o) (call f)])]
                   [()
                    (finally (e o) (values e o) (halt))]))]
           (execute (lambda (e) (e n* 0 0)) [(n* e o) (call f)]))
      (values e o))))

(define ex2
  (lambda (n*)
    (cfg (labels [(f (execute
                         (lambda (e1 e2)
                           (if (null? n*)
                               (e2)
                               (e1 (car n*) (cdr n*))))
                       [(n n*)
                        (execute
                            (lambda (e1 e2)
                              (if (odd? n)
                                  (e2)
                                  (e1)))
                          [()
                           (finally (e*) (cons n e*) (call f))]
                          [()
                           (finally (o*) (cons n o*) (call f))])]
                       [()
                        (finally (e* o*) (values '() '()) (halt))]))]
           (execute (lambda (e) (e n*)) [(n*) (call f)]))
      (values e* o*))))

(assert (equal? '(2 1)
                (call-with-values
                    (lambda ()
                      (ex1 '(2 1 4)))
                  list)))

(assert (equal? '((2 4) (1))
                (call-with-values
                    (lambda ()
                      (ex2 '(2 1 4)))
                  list)))

;;; Label definitions

(define-cfg-label p)

(assert (equal? 40
                (let-syntax ([k (syntax-rules ()
                                  [(k d)
                                   (cfg (labels ([p (finally (x) 40 (halt))])
                                          (call d))
                                     x)])])
                  (k p))))

(define-syntax permuting
  (lambda (stx)
    (syntax-case stx ()
      [(_ cfg-term ... result-expr)
       #'(cfg (permute ([p cfg-term] ...)
                (finally (res) result-expr (halt)))
           res)])))

(assert (equal? 99
                (permuting (simple-bind ([x 99]) (call p)) x)))

;;; More examples from spec

(assert (equal? '(2 3)
                (let ([x 1])
                  (cfg (finally (x y) (values (+ x 1) (+ x 2))
                         (halt))
                    (list x y)))))

(assert (equal? 2
                (let ([x 1])
                  (cfg (execute (lambda (e)
                                  (e (+ x 1)))
                         [(x) (finally (res) x (halt))])
                    res))))

(assert (equal? 'odd
                (let ([x 1])
                  (cfg (execute (lambda (e1 e2)
                                  (if (even? x) (e1) (e2 'odd)))
                         [() (finally (res) 'even (halt))]
                         [(a) (finally (res) a (halt))])
                    res))))

(assert (equal? 'outer
                (let ([a 'outer]
                      [x 1])
                  (cfg (finally (res) a
                         (execute (lambda (e1 e2)
                                    (if (even? x) (e1) (e2 'odd)))
                           [() (finally (res) 'even (halt))]
                           [(a) (halt)]))
                    res))))

(assert (equal? 'outer
                (let ([res 'outer]
                      [x 1])
                  (cfg (execute (lambda (e1 e2)
                                  (if (even? x) (e1) (e2 'odd)))
                         [() (halt)]
                         [(a) (finally (res) a (halt))])
                    res))))

(assert (equal? 720
                (cfg (labels ([f (execute
                                        (lambda (e1 e2)
                                          (if (> x 6)
                                              (e1)
                                              (e2 (+ x 1) (* a x))))
                                      [() (finally (res) a (halt))]
                                      [(x a) (call f)])])
                          (simple-bind [(x 1) (a 1)] (call f)))
                     res)))

(cfg (labels ([f (execute
                     (lambda (e1 e2)
                       (if (> x 6)
                                              (e1)
                                              (e2 (+ x 1) (* a x))))
                   [() (finally (res) a (halt))]
                   [(x a) (call f)])])
       (simple-bind [(x 1) (a 1)] (call f)))
  res)

(define-cfg-syntax return
  (lambda (stx)
    (syntax-case stx ()
      [(_ return-var ...)
       (for-all identifier? #'(return-var ...))
       #'(finally (return-var ...) (values return-var ...) (halt))])))

(assert (equal? 1
                (cfg (simple-bind ([x 1]) (return x))
                  x)))

(assert (equal? '(outer outer)
                (let ([x 'outer] [y 'outer])
                  (cfg (label* ([c (permute ([p (finally (y) 'inner
                                                  (simple-bind ([a x]) (call p)))])
                                     (finally (a) a (halt)))])
                         (permute [(p (finally (b) y
                                        (simple-bind ([x 'inner]) (call p))))]
                           (call c)))
                    (list a b)))))

(let ([x 'outer] [y 'outer])
  (cfg (label* ([c (permute ([p (finally (y) 'inner
                                  (simple-bind ([a x]) (call p)))])
                     (finally (a) a (halt)))])
         (permute [(p (finally (b) y
                        (simple-bind ([x 'inner]) (call p))))]
           (call c)))
    (list a b)))

;;; Loop example

(define-cfg-syntax loop
  (lambda (stx)
    (syntax-case stx ()
      [(_ n-expr lp-lbl loop-cfg-term body-cfg-term)
       (identifier? #'lp-lbl)
       #'(bind ([(n) n-expr])
           (labels ([lp-lbl
                     (execute
                         (lambda (loop done)
                           (if (zero? n)
                               (done)
                               (loop (- n 1))))
                       [(n) loop-cfg-term]
                       [() body-cfg-term])])
             (call lp-lbl)))])))

(assert (equal? 20
                (cfg
                    (bind ([(n) 0])
                      (loop 10 next
                            (bind ([(n) (+ n 2)])
                              (call next))
                            (finally (n) n (halt))))
                  n)))

(define-cfg-label next)

(define-cfg-syntax loop2
  (lambda (stx)
    (syntax-case stx ()
      [(_ n-expr loop-cfg-term body-cfg-term)
       #'(bind ([(n) n-expr])
           (labels ([next
                     (execute
                         (lambda (loop done)
                           (if (zero? n)
                               (done)
                               (loop (- n 1))))
                       [(n) loop-cfg-term]
                       [() body-cfg-term])])
             (call next)))])))

(assert (equal? 20
                (cfg
                    (bind ([(n) 0])
                      (loop2 10
                            (bind ([(n) (+ n 2)])
                              (call next))
                            (finally (n) n (halt))))
                  n)))

;;; Examples from the spec

(assert (equal? '(2 5)
                (let ([x 1] [y 2])
                  (cfg
                      (finally (y) (+ x 3)
                        (execute
                            (lambda (e)
                              (set! x y)
                              (e))
                          [() (halt)]))
                    (list x y)))))

(assert (equal? '(1 (2 3))
                (cfg
                    (finally (x . y) (values 1 2 3)
                      (halt))
                  (list x y))))

(assert (equal? 4
                (let ([x 1] [y 2])
                  (cfg
                      (execute
                          (lambda (e1 e2)
                            (if (odd? y) (e1 5) (e2)))
                        [(x) (finally (y) (+ x 2) (halt))]
                        [() (finally (y) (+ x 3) (halt))])
                    y))))

(assert (equal? '(2 1)
                (let ([x 1])
                  (cfg
                      (permute ([p (bind ([(x) 2])
                                         (call p))]
                                [p (bind ([(y) x])
                                         (call p))])
                        (finally (z) (list x y) (halt)))
                    z))))

;; Local Variables:
;; mode: scheme
;; End:
