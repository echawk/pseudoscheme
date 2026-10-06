;;; Tests for SRFI 201, from the sample implementation's examples
;;; (reference/srfi-201/examples.scm).  The examples that expect
;;; expansion errors aren't run (a program can't recover from those).
(import (except (scheme base) lambda define let let* or)
        (scheme process-context)
        (only (srfi 1) filter)
        (srfi 64) (srfi 201))

(define (results thunk) (call-with-values thunk list))

(define (examples)
  ;; let: multiple values and destructuring.
  (test-equal '(1 2)
    (let ((`(,x) `(,y) (values '(1) '(2))))
      (list x y)))
  (test-equal "extra values are ignored" '(1 2)
    (let ((x y (values 1 2 3)))
      (list x y)))
  (test-equal "values keyword" '(1 2)
    (let (((values x y) (values 1 2)))
      (list x y)))
  (test-error "values keyword: no extra values" #t
    (let (((values x y) (values 1 2 3)))
      (list x y)))
  (test-equal "values keyword, rest" '((1 2) (3))
    (results (lambda ()
               (let (((values x y . z) (values 1 2 3)))
                 (values (list x y) z)))))
  (test-equal "named let" 10
    (let loop ((x 0))
      (if (>= x 10) x (loop (+ x 1)))))
  (test-equal "named let, destructuring" 10
    (let loop ((`(,x) '(0)))
      (if (>= x 10) x (loop `(,(+ x 1))))))
  (test-equal "named let, multiple values" '(5 6)
    (results (lambda ()
               (let loop ((`(,x) y (values '(1) 2)))
                 (if (>= (+ x y) 10)
                     (values x y)
                     (loop `(,(+ x 1)) (+ y 1)))))))
  (test-equal "named let, values keyword" '(5 6)
    (results (lambda ()
               (let loop (((values x `(,y)) (values 1 '(2))))
                 (if (>= (+ x y) 10)
                     (values x y)
                     (loop (+ x 1) `(,(+ y 1))))))))
  (test-equal "let, several destructuring bindings" '(1 2 3)
    (let ((`(,a ,b) '(1 2)) (c 3)) (list a b c)))
  (test-error "let, failed destructuring" #t
    (let ((`(,a ,b) '(1 2 3))) a))

  ;; let*
  (test-equal '(1 2 4 5)
    (let* ((`(,x) y (values '(1) 2 3))
           (z `(,w) (values 4 '(5))))
      (list x y z w)))
  (test-equal "let*, values keyword" '(1 2 4 5)
    (let* (((values `(,x) y . _) (values '(1) 2 3))
           ((values z `(,w)) (values 4 '(5))))
      (list x y z w)))
  (test-equal "let*, plain" 3 (let* ((a 1) (b (+ a 1))) (+ a b)))
  (test-equal "let*, sequential destructuring" '(1 2 3)
    (let* ((`(,a . ,r) '(1 2 3)) (`(,b ,c) r)) (list a b c)))

  ;; lambda
  (test-equal '(3 7 11)
    (map (lambda (`(,l . ,r)) (+ l r))
         '((1 . 2) (3 . 4) (5 . 6))))
  (test-equal "body-less lambda" '((3 4 5) (8 9 0))
    (filter (lambda (`(,_ ,_ ,_)))
            '((1 2) (3 4 5) (6 7) (8 9 0))))
  (test-assert "(lambda _)" ((lambda _) 1 2 3))
  (test-error "lambda, failed destructuring" #t
    ((lambda (`(,x . ,y)) 'whatever) '()))
  (test-equal "plain lambda" '(1 (2 3))
    ((lambda (a . r) (list a r)) 1 2 3))
  (test-equal "lambda with no parameters" 'ok ((lambda () 'ok)))

  ;; define
  (test-equal "curried define" '(1 2 3 4)
    (let ()
      (define ((f `(,a ,b)) `(,c ,d))
        `(,a ,b ,c ,d))
      ((f '(1 2)) '(3 4))))
  (test-assert "body-less curried define"
    (let ()
      (define ((f `(,a ,b)) `(,c ,d)))
      (and ((f '(1 2)) '(3 4))
           (not ((f 5) '(6 7))))))
  (test-equal "plain define" 6
    (let ()
      (define (g x) (* 2 x))
      (define h 3)
      (g h)))

  ;; or
  (test-equal '(6 7 8) (results (lambda () (or (values #f 5) (values 6 7 8)))))
  (test-equal '(#t 5) (results (lambda () (or (values #t 5) (values 6 7 8)))))
  (test-equal '(4 5) (results (lambda () (or (values 4 5) (values 6 7 8)))))
  (test-equal #f (or))
  (test-equal 3 (or #f 3)))

(test-begin "srfi-201")
(examples)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-201")
  (exit (if (zero? failures) 0 1)))
