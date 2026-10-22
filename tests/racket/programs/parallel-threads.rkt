#lang racket/base
;; threads in a parallel-thread pool: results, thread-wait, closing
(define pool (make-parallel-thread-pool 4))
(displayln (parallel-thread-pool? pool))
(displayln (parallel-thread-pool? 'x))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define results (make-vector 4 #f))
(define threads
  (for/list ([i (in-range 4)])
    (thread (lambda () (vector-set! results i (fib (+ 20 i)))) #:pool pool)))
(for-each thread-wait threads)
(displayln results)
(displayln (map thread-dead? threads))

;; a pool thread that blocks goes on as an ordinary thread
(define s (make-semaphore))
(define box1 (box #f))
(define t (thread (lambda () (semaphore-wait s) (set-box! box1 'woke)) #:pool pool))
(sleep 0.05)
(semaphore-post s)
(thread-wait t)
(displayln (unbox box1))

;; shared state under a lock-free-safe structure
(define h (make-hasheqv))
(define ts
  (for/list ([i (in-range 4)])
    (thread (lambda () (for ([j (in-range 1000)]) (hash-set! h (+ (* i 1000) j) j))) #:pool pool)))
(for-each thread-wait ts)
(displayln (hash-count h))

(parallel-thread-pool-close pool)
(displayln (with-handlers ([exn:fail? (lambda (e) 'closed)])
             (thread void #:pool pool)))
