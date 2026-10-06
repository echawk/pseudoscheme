;; Filtering

(define-syntax em-filter
  (em-syntax-rules ()
    ((em-filter 'pred '())
     '())
    ((em-filter 'pred '(h . t))
     (em-if (pred 'h)
	    (em-cons 'h (em-filter 'pred 't))
	    (em-filter 'pred 't)))))

(define-syntax em-remove
  (em-syntax-rules ()
    ((em-remove 'pred '())
     '())
    ((em-remove 'pred '(h . t))
     (em-if (pred 'h)
	    (em-remove 'pred 't)
	    (em-cons 'h (em-remove 'pred 't))))))

;; Searching

(define-syntax em-find
  (em-syntax-rules ()
    ((em-find 'pred '())
     '())
    ((em-find 'pred '(h . t))
     (em-if (pred 'h)
	    'h
	    (em-find 'pred 't)))))

(define-syntax em-find-tail
  (em-syntax-rules ()
    ((em-find-tail 'pred '())
     '#f)
    ((em-find-tail 'pred '(h . t))
     (em-if (pred 'h)
	    '(h . t)
	    (em-find-tail 'pred 't)))))

(define-syntax em-take-while
  (em-syntax-rules ()
    ((em-take-while 'pred '())
     '())
    ((em-take-while 'pred '(h . t))
     (em-if (pred 'h)
	    (em-cons 'h (em-take-while 'pred 't))
	    '()))))

(define-syntax em-drop-while
  (em-syntax-rules ()
    ((em-drop-while 'pred '())
     '())
    ((em-drop-while 'pred '(h . t))
     (em-if (pred 'h)
	    (em-drop-while 'pred 't)
	    '(h . t)))))

(define-syntax em-any
  (em-syntax-rules ()
    ((em-any 'pred '(h . t) ...)
     (em-or (pred 'h ...) (em-any 'pred 't ...)))
    ((em-any 'pred '_ ...)
     '#f)))

(define-syntax em-every
  (em-syntax-rules ()
    ((em-every 'pred '() ...)
     '#t)
    ((em-every 'pred '(a b . x) ...)
     (em-and (pred 'a ...) (em-every 'pred '(b . x) ...)))
    ((em-every 'pred '(h . t) ...)
     (pred 'h ...))))

(define-syntax em-member
  (em-syntax-rules ()
    ((em-member 'obj 'list 'compare)
     (em-find-tail (em-cut 'compare 'obj <>) 'list))
    ((em-member 'obj 'list)
     (em-member 'obj 'list 'em-equal?))))

;; Association lists

(define-syntax em-assoc
  (em-syntax-rules ()
    ((em-assoc 'key '() 'compare)
     '#f)
    ((em-assoc 'key '((k . v) . t) 'compare)
     (em-if (compare 'key 'k)
	    '(k . v)
	    (em-assoc 'key 't 'compare)))
    ((em-assoc 'key 'alist)
     (em-assoc 'key 'alist 'em-equal?))))

(define-syntax em-alist-delete
  (em-syntax-rules ()
    ((em-alist-delete 'key '() 'compare)
     '())
    ((em-alist-delete 'key '((k . v) . t) 'compare)
     (em-if (compare 'key 'k)
	    (em-alist-delete 'key 't 'compare)
	    (em-cons '(k . v) (em-alist-delete 'key 't 'compare))))
    ((em-alist-delete 'key 'alist)
     (em-alist-delete 'key 'alist 'em-equal?))))

;; Set operations

(define-syntax em-set<=
  (em-syntax-rules ()
    ((em-set<= 'compare '())
     '#t)
    ((em-set<= 'compare 'list)
     '#t)
    ((em-set<= 'compare '() 'list)
     '#t)
    ((em-set<= 'compare '(h . t) 'list)
     (em-and (em-member 'h 'list 'compare)
	     (em-set<= 'compare 't 'list)))
    ((em-set<= 'compare 'list1 'list2 'list ...)
     (em-and (em-set<= 'compare 'list1 'list2)
	     (em-set<= 'compare 'list2 'list ...)))))

(define-syntax em-set=
  (em-syntax-rules ()
    ((em-set= 'compare 'list)
     '#t)
    ((em-set= 'compare 'list1 list2)
     (em-and (em-set<= 'compare 'list1 'list2)
	     (em-set<= 'compare 'list2 'list1)))
    ((em-set= 'compare 'list1 'list2 'list ...)
     (em-and (em-set= 'list1 'list2)
	     (em-set= 'list1 'list ...)))))

(define-syntax em-set-adjoin
  (em-syntax-rules ()
    ((em-set-adjoin 'compare 'list)
     'list)
    ((em-set-adjoin 'compare 'list 'element1 'element2 ...)
     (em-if (em-member 'element1 'list 'compare)
	    (em-set-adjoin 'compare 'list 'element2 ...)
	    (em-set-adjoin 'compare (em-cons 'element1 'list) 'element2 ...)))))

(define-syntax em-set-union
  (em-syntax-rules ()
    ((em-set-union 'compare 'list ...)
     (em-apply 'em-set-adjoin 'compare '() (em-append 'list ...)))))

(define-syntax em-set-intersection
  (em-syntax-rules ()
    ((em-set-intersection 'compare 'list)
     'list)
    ((em-set-intersection 'compare 'list1 'list2)
     (em-filter (em-cut 'em-member <> 'list2 'compare) 'list1))
    ((em-set-intersection 'compare 'list1 'list2 'list ...)
     (em-set-intersection 'compare (em-set-intersection 'list1 'list2) 'list ...))))

(define-syntax em-set-difference
  (em-syntax-rules ()
    ((em-set-difference 'compare 'list)
     'list)
    ((em-set-difference 'compare 'list1 'list2)
     (em-remove (em-cut 'em-member <> 'list2 'compare) 'list1))
    ((em-set-difference 'compare 'list1 'list2 'list ...)
     (em-set-difference 'compare (em-set-difference 'list1 'list2) 'list ...))))

(define-syntax em-set-xor
  (em-syntax-rules ()
    ((em-set-xor 'compare 'list1 'list2)
     (em-set-union 'compare
		   (em-set-difference 'compare 'list1 'list2)
		   (em-set-difference 'compare 'list2 'list1)))))

