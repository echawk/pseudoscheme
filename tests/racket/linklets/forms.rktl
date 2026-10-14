;;; The core forms other than lambda: let-values, letrec-values, begin0,
;;; set!, define-values of several values, case-lambda's dispatch.
(linklet
 ()
 ()
 (define-values (q r) (values 7 2))
 (define-values (counter) 0)
 (define-values (bump!) (lambda () (set! counter (add1 counter)) counter))
 (define-values (f)
   (case-lambda ((a) (list 'one a))
                ((a b) (list 'two a b))
                ((a b . more) (list 'many a b more))))
 (list q r
       (let-values (((a b) (values 1 2)) ((c) 3)) (list a b c))
       (let-values (((x) 1)) (let-values (((x) 2) ((y) x)) (list x y)))
       (letrec-values (((even? odd?)
                        (values (lambda (n) (if (zero? n) #t (odd? (sub1 n))))
                                (lambda (n) (if (zero? n) #f (even? (sub1 n)))))))
         (list (even? 10) (odd? 7)))
       (begin0 (bump!) (bump!) (bump!))
       counter
       (call-with-values (lambda () (begin0 (values 1 2) (bump!))) list)
       (f 1) (f 1 2) (f 1 2 3 4)))
