;;; Tests for SRFI 216: the sample's SRFI 78 conformance test
;;; (reference/srfi-216/srfi-216-tests.scm), in SRFI 64.  The parallel
;;; test checks that each run leaves 2 or 3; "sleep-a-little" waits 50 ms
;;; rather than a second.
(import (scheme base) (scheme lazy) (scheme time) (scheme process-context)
        (srfi 64) (srfi 216)
        (only (srfi 27) random-integer))

(define (sleep-a-little)
  (define starting-time (current-second))
  (let loop ()
    (if (< (- (current-second) starting-time) 0.05)
        (loop))))

(test-begin "srfi-216")

;;; runtime
(test-assert (exact-integer? (runtime)))
(test-assert (> (let* ((first-value (runtime))
                       (second-value (begin (sleep-a-little) (runtime))))
                  (- second-value first-value))
                0))
;; microseconds: 50 ms is about 50000
(test-assert (let* ((a (runtime)) (b (begin (sleep-a-little) (runtime))))
               (< 40000 (- b a) 5000000)))

;;; random
(test-assert (> (random 100) -1))
(test-assert (< (random 100) 100))
(test-assert (exact-integer? (random 100)))
(test-assert (not (exact-integer? (random 100.0))))
(test-assert (< (random 1.5) 1.5))

;;; parallel-execute
(define (my-wait n)
  (if (= n 0)
      #t
      (my-wait (- n 1))))

(define testval 1)
(do ((i 0 (+ i 1)))
    ((= i 5) #f)
  (parallel-execute
   (lambda ()
     (my-wait (random-integer 100))
     (set! testval 2))
   (lambda ()
     (my-wait (random-integer 100))
     (set! testval 3)))
  (test-assert (or (= testval 2) (= testval 3))))

(define x 10)
(parallel-execute (lambda () (set! x (+ x 1))) (lambda () (set! x (+ x 1))))
(test-assert (memv x '(11 12)))

;;; test-and-set!
(define cell (list #f))
(test-equal #f (test-and-set! cell))
(test-equal #t (car cell))
(set-car! cell (list #t))
(test-equal #t (test-and-set! cell))

;;; booleans and nil
(test-equal #f (if false #t #f))
(test-equal #t (if true #t #f))
(test-equal '() nil)

;;; streams
(test-equal #t (stream-null? the-empty-stream))
(test-equal #f (stream-null? (cons-stream 'a 'b)))
(test-equal 'a (car (cons-stream 'a 'b)))
(test-assert (promise? (cdr (cons-stream 'a 'b))))
(test-equal 'b (force (cdr (cons-stream 'a 'b))))
(define (integers-from n) (cons-stream n (integers-from (+ n 1))))
(define (stream-ref s n) (if (= n 0) (car s) (stream-ref (force (cdr s)) (- n 1))))
(test-equal 105 (stream-ref (integers-from 5) 100))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-216")
  (exit (if (zero? failures) 0 1)))
