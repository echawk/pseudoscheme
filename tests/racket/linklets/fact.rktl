;;; A hand-written linklet (docs/racket.md, "Linklets"), and what
;;; instantiating it gives: (fact 5) => 120, (greet) => "hello world".
(linklet
 ()
 (fact greet)
 (define-values (fact)
   (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n))))))
 (define-values (greet)
   (case-lambda (() (string-append "hello " "world"))
                ((name) (string-append "hello " name))))
 (list (fact 5) (greet)))
