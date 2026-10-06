;; Combinatorics

(define-syntax em-0
  (em-syntax-rules ()
    ((em-0)
     '())))

(define-syntax em-1
  (em-syntax-rules ()
    ((em-1)
     '(0))))

(define-syntax em-2
  (em-syntax-rules ()
    ((em-2)
     '(0 1))))

(define-syntax em-3
  (em-syntax-rules ()
    ((em-3)
     '(0 1 2))))

(define-syntax em-4
  (em-syntax-rules ()
    ((em-4)
     '(0 1 2 3))))

(define-syntax em-5
  (em-syntax-rules ()
    ((em-5)
     '(0 1 2 3 4))))

(define-syntax em-6
  (em-syntax-rules ()
    ((em-6)
     '(0 1 2 3 4 5))))

(define-syntax em-7
  (em-syntax-rules ()
    ((em-7)
     '(0 1 2 3 4 5 6))))

(define-syntax em-8
  (em-syntax-rules ()
    ((em-8)
     '(0 1 2 3 4 5 6 7))))

(define-syntax em-9
  (em-syntax-rules ()
    ((em-9)
     '(0 1 2 3 4 5 6 7 8))))

(define-syntax em-10
  (em-syntax-rules ()
    ((em-10)
     '(0 1 2 3 4 5 6 7 8 9))))

(define-syntax em=
  (em-syntax-rules ()
    ((em= '_)
     '#t)
    ((em= '() '())
     '#t)
    ((em= '(h . t) '())
     '#f)
    ((em= '() '(h . t))
     '#f)
    ((em= '(h . t) '(u . v))
     (em= 't 'v))
    ((em= 'list1 'list2 'list ...)
     (em-and (em= 'list1 'list2)
	     (em= 'list1 'list ...)))))

(define-syntax em<
  (em-syntax-rules ()
    ((em<)
     '#t)
    ((em< 'list)
     '#t)
    ((em< '_ '())
     '#f)
    ((em< '() '_)
     '#t)
    ((em< '(t . h) '(u . v))
     (em< 'h 'v))
    ((em< 'list1 'list2 'list ...)
     (em-and (em< 'list1 'list2)
	     (em< 'list2 'list ...)))))

(define-syntax em<=
  (em-syntax-rules ()
    ((em<=)
     '#t)
    ((em<= 'list)
     '#t)
    ((em<= '() '_)
     '#t)
    ((em<= '_ '())
     '#f)
    ((em<= '(t . h) '(u . v))
     (em<= 'h 'v))
    ((em<= 'list1 'list2 'list ...)
     (em-and (em<= 'list1 'list2)
	     (em<= 'list2 'list ...)))))

(define-syntax em>
  (em-syntax-rules ()
    ((em> 'list ...)
     (em-apply 'em< (em-reverse '(list ...))))))

(define-syntax em>=
  (em-syntax-rules ()
    ((em>= 'list ...)
     (em-apply 'em<= (em-reverse '(list ...))))))

(define-syntax em-zero? em-null?)

(define-syntax em-even?
  (em-syntax-rules ()
    ((em-even? '())
     '#t)
    ((em-even? '(a b . c))
     (em-even? 'c))
    ((em-even? '_)
     '#f)))

(define-syntax em-odd?
  (em-syntax-rules ()
    ((em-odd? 'list)
     (em-not (em-even? 'list)))))

(define-syntax em+ em-append)

(define-syntax em-
  (em-syntax-rules ()
    ((em- 'list)
     'list)
    ((em- 'list '())
     'list)
    ((em- '(a ... b) '(u . v))
     (em- '(a ...) 'v))
    ((em- 'list1 'list2 'list ...)
     (em- (em- 'list1 'list2) 'list ...))))

(define-syntax em*
  (em-syntax-rules ()
    ((em*)
     '(()))
    ((em* 'list1 'list2 ...)
     (em*-aux 'list1 (em* 'list2 ...)))))

(define-syntax em*-aux
  (em-syntax-rules ()
    ((em*-aux '(x ...) 'list)
     (em-append (em-map (em-cut 'em-cons 'x <>) 'list) ...))))

(define-syntax em-quotient
  (em-syntax-rules ()
    ((em-quotient 'list 'k)
     (em-if (em>= 'list 'k)
	    (em-cons (em-car 'list)
		     (em-quotient (em-list-tail 'list 'k) 'k))
	    '()))))

(define-syntax em-remainder
  (em-syntax-rules ()
    ((em-quotient 'list 'k)
     (em-if (em>= 'list 'k)
	    (em-remainder (em-list-tail 'list 'k) 'k)
	    'list))))

(define-syntax em-binom
  (em-syntax-rules ()
    ((em-binom 'list '())
     '(()))
    ((em-binom '() '(h . t))
     '())
    ((em-binom '(u . v) '(h . t))
     (em-append (em-map (em-cut 'em-cons u <>) (em-binom 'v 't))
		(em-binom 'v '(h . t))))))

(define-syntax em-fact
  (em-syntax-rules ()
    ((em-fact '())
     '(()))
    ((em-fact 'list)
     (em-append-map 'em-fact-cons*
		    'list (em-map 'em-fact (em-fact-del 'list))))))

(define-syntax em-fact-del
  (em-syntax-rules ()
    ((em-fact-del '())
     '())
    ((em-fact-del '(h . t))
     `(t ,@(em-map (em-cut 'em-cons 'h <>) (em-fact-del 't))))))

(define-syntax em-fact-cons*
  (em-syntax-rules ()
    ((em-fact-cons* 'a '((l ...) ...))
     '((a l ...) ...))))
 
