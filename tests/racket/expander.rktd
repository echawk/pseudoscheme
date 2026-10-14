(+ 1 2)	3
(let-values ([(x) 5]) (* x x))	25
(if #f 1 2)	2
((lambda args args) 1 2)	(1 2)
(call-with-values (lambda () (values 1 2)) cons)	(1 . 2)
(letrec-values ([(f) (lambda (n) (if (zero? n) 1 (* n (f (sub1 n)))))]) (f 10))	3628800
(quote (a #(b) "c"))	(a #(b) "c")
(with-continuation-mark 'k 1 (continuation-mark-set-first #f 'k))	1
(begin0 1 2)	1
((case-lambda [(a) a] [(a b) b]) 1 2)	2
