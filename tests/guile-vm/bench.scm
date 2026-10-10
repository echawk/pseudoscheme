;;; Benchmarks of Guile's VM backends (tests/guile-vm/bench.sh): compiled
;;; by the installed Guile, run here.  The file's value is the procedures.
(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (count-loop n) (let lp ((i 0) (acc 0)) (if (= i n) acc (lp (+ i 1) (+ acc i)))))
(define (tak x y z) (if (not (< y x)) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
(define (vec-sum n)
  (let ((v (make-vector n 1)))
    (let lp ((i 0) (s 0)) (if (= i n) s (lp (+ i 1) (+ s (vector-ref v i)))))))
(list (list "fib 30" fib 30) (list "loop 10^6" count-loop 1000000)
      (list "tak 24 16 8" tak 24 16 8) (list "vector sum 10^6" vec-sum 1000000))
