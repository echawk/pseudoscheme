;;; Tests for SRFI 190: the SRFI's test file (reference/srfi-190/test.scm)
;;; and the examples in the SRFI document.
(import (scheme base) (scheme process-context)
        (srfi 158) (srfi 64) (srfi 190))

(test-begin "srfi-190")

(test-group "Coroutine Generators"
  (define g
    (coroutine-generator
      (do ((i 0 (+ i 1)))
          ((<= 3 i))
        (yield i))))

  (define-coroutine-generator (h n)
    (do ((i 0 (+ i 1)))
        ((<= 3 i))
      (yield i)))

  (test-equal '(0 1 2) (generator->list g))
  (test-equal '(0 1 2) (generator->list (h 3))))

;; From the SRFI document
(test-equal '(0 1 4)
  (generator->list
   (let ((yield-square (lambda (yield i) (yield (* i i)))))
     (coroutine-generator
       (do ((i 0 (+ i 1)))
           ((<= 3 i))
         (yield-square yield i))))))

(test-equal '(0 1 4)
  (generator->list
   (let-syntax ((yield-square (syntax-rules () ((_ i) (yield (* i i))))))
     (coroutine-generator
       (do ((i 0 (+ i 1)))
           ((<= 3 i))
         (yield-square i))))))

(define-coroutine-generator (g5 n)
  (do ((i 0 (+ i 1)))
      ((<= n i))
    (yield i)))
(test-equal '(0 1 2 3 4) (generator->list (g5 5)))

(define-coroutine-generator abc
  (yield 'a) (yield 'b) (yield 'c))
(test-equal '(a b c) (generator->list abc))
(test-assert (eof-object? (abc)))

;; Nested generators: each yield belongs to the innermost one.
(test-equal '((1 10) (2 20))
  (let ((outer
         (coroutine-generator
           (let ((inner (coroutine-generator (yield 10) (yield 20))))
             (yield (list 1 (inner)))
             (yield (list 2 (inner)))))))
    (generator->list outer)))

;; Interleaved, re-entered generators.
(test-equal '(0 100 1 101 2 102)
  (let ((a (g5 3))
        (b (coroutine-generator (yield 100) (yield 101) (yield 102))))
    (let loop ((acc '()))
      (let ((x (a)))
        (if (eof-object? x)
            (reverse acc)
            (loop (cons (b) (cons x acc))))))))

;; yield outside a coroutine generator is an error
(test-error #t (yield 1))

;; Body with internal definitions
(test-equal '(6)
  (generator->list
   (coroutine-generator
     (define x 6)
     (yield x))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-190")
  (exit (if (zero? failures) 0 1)))
