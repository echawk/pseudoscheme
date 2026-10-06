;;; Tests for SRFI 188: the sample implementation's test suite
;;; (reference/srfi-188/188-test.sld), unchanged but for the framing.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 188))

(test-begin "srfi-188")

(test-equal 1
  (let ((x #t))
    (splicing-let-syntax ()
      (define x 1)
      #f)
    x))

(test-equal 2
  (let ((x #t))
    (splicing-let-syntax ()
      (define x 2))
    x))

(test-equal 3
  (let ((x #t))
    (splicing-let-syntax
        ((f (syntax-rules ()
              ((f) x))))
      (define x 3)
      (f))))

(test-equal 4
  (let ((x #t))
    (splicing-letrec-syntax ()
      (define x 4)
      #f)
    x))

(test-equal 5
  (let ((x #t))
    (splicing-letrec-syntax ()
      (define x 5))
    x))

(test-equal 6
  (let ((x #t))
    (splicing-letrec-syntax
        ((f (syntax-rules ()
              ((f) x))))
      (define x 6)
      (f))))

(test-equal 'now
  (splicing-let-syntax ((given-that (syntax-rules ()
                                      ((given-that test stmt1 stmt2 ...)
                                       (if test
                                           (begin stmt1
                                                  stmt2 ...))))))
    (let ((if #t))
      (given-that if (set! if 'now))
      if)))

(test-equal 'outer
  (let ((x 'outer))
    (splicing-let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m)))))

(test-equal 7
  (splicing-letrec-syntax
      ((my-or (syntax-rules ()
                ((my-or) #f)
                ((my-or e) e)
                ((my-or e1 e2 ...)
                 (let ((temp e1))
                   (if temp
                       temp
                       (my-or e2 ...)))))))
    (let ((x #f)
          (y 7)
          (temp 8)
          (let odd?)
          (if even?))
      (my-or x
             (let temp)
             (if y)
             y))))

(test-equal 'let-syntax
  (let ((x 'let-syntax))
    (let-syntax ()
      (define x 'splicing-let-syntax)
      #f)
    x))

(test-equal 'splicing-let-syntax
  (let ((x 'let-syntax))
    (splicing-let-syntax ()
      (define x 'splicing-let-syntax)
      #f)
    x))

;; At a program's top level too
(splicing-let-syntax ((def (syntax-rules () ((_ n v) (define n v)))))
  (def top-level-a 10)
  (def top-level-b 20))
(test-equal 30 (+ top-level-a top-level-b))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-188")
  (exit (if (zero? failures) 0 1)))
