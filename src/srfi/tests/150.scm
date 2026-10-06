;;; Tests for SRFI 150: the SRFI's own test suite
;;; (reference/srfi-150/test.sld), as a program, plus a few more.
(import (except (scheme base) define-record-type) (scheme process-context)
        (srfi 64) (srfi 150)
        (only (rnrs records inspection) record-type-name record-rtd))

(define (run-tests)
      (test-begin "SRFI 150")

      (test-group "Simple"

		  (define-record-type <pare>
		    (kons x y)
		    pare?
		    (x kar set-kar!)
		    (y kdr))

		  (test-assert (pare? (kons 1 2)))
		  (test-assert (not (pare? (cons 1 2))))
		  (test-eqv 1 (kar (kons 1 2)))
		  (test-eqv 2 (kdr (kons 1 2)))
		  (test-eqv 3 (let ((k (kons 1 2)))
				(set-kar! k 3)
				(kar k))))

      (test-group "Inheritance"

		  (define-record-type <parent>
		    (make-parent x)
		    parent?
		    (x parent-field parent-set-field!))

		  (define-record-type (<child> <parent>)
		    (make-child x y)
		    child?
		    (y child-field child-set-field!))

		  (test-assert (parent? (make-child 1 2)))
		  (test-assert (child? (make-child 1 2)))
		  (test-assert (not (child? (make-parent 1))))
		  (test-eqv 1 (parent-field (make-child 1 2)))
		  (test-eqv 2 (child-field (make-child 1 2)))
		  (test-eqv 3 (let ((c (make-child 1 2)))
				(parent-set-field! c 3)
				(parent-field c)))
		  (test-eqv 3 (let ((c (make-child 1 2)))
				(child-set-field! c 3)
				(child-field c))))

      (test-group "Implicit constructor arguments"

		  (define-record-type <parent>
		    (make-parent)
		    parent?
		    (x parent-field))

		  (define-record-type (<child> <parent>)
		    make-child
		    child?
		    (y child-field))

		  (test-eqv 1 (parent-field (make-child 1 2)))
		  (test-eqv 2 (child-field (make-child 1 2))))

      (test-group "Shadowing of parent fields"

		  (define-record-type <parent>
		    (make-parent x)
		    parent?
		    (x parent-field parent-set-field!))

		  (define-record-type (<child> <parent>)
		    (%make-child x)
		    child?
		    (x child-field))

		  (define (make-child x)
		    (let ((c (%make-child x)))
		      (parent-set-field! c 'undefined)
		      c))
		  
		  (test-eqv 1 (child-field (make-child 1)))
		  (test-eqv 'undefined (parent-field (make-child 1))))

      (test-group "Field referral through accessors"

		  (define-record-type <record>
		    (make-record x y get-z)
		    record?
		    (x get-x)
		    (y x)
		    (z get-z))

		  (test-eqv 1 (get-x (make-record 1 2 3)))
		  (test-eqv 2 (x (make-record 1 2 3)))
		  (test-eqv 3 (get-z (make-record 1 2 3))))

      (test-group "Hygiene 1"

		  (define a #f)
		  
		  (define-syntax def
		    (syntax-rules ()
		      ((def b make-record get-a get-b)
		       (define-record-type <record>
			 (make-record a b)
			 record?
			 (a get-a)
			 (b get-b)))))
		  
		  (def a make-record get-a get-b)

		  (test-eqv 1 (get-a (make-record 1 2)))
		  (test-eqv 2 (get-b (make-record 1 2))))

      (test-group "Hygiene 2"

		  (define x #f)
		  
		  (define-record-type <parent>
		    (make-parent x)
		    parent?
		    (x parent-get))

		  (define-syntax define-child
		    (syntax-rules ()
		      ((define-child make-child child-get parent-field)
		       (define-record-type (<child> <parent>)
			 (make-child parent-field x)
			 child?
			 (x child-get)))))

		  (define-child make-child child-get x)

		  (test-eqv 1 (parent-get (make-child 1 2)))
		  (test-eqv 2 (child-get (make-child 1 2))))
      
      (test-group "Alex Shinn's example"

		  (define-syntax define-tuple-type
		    (syntax-rules ()
		      ((define-tuple-type name make pred x-ref (defaults ...))
		       (deftuple name (make) pred x-ref (defaults ...) (defaults ...) ()))))

		  (define-syntax deftuple
		    (syntax-rules ()
		      ((deftuple name (make args ...) pred x-ref defaults (default . rest)
			 (fields ...))
		       (deftuple name (make args ... tmp) pred x-ref  defaults rest
			 (fields ... (tmp tmp))))
		      ((deftuple name (make args ...) pred x-ref (defaults ...) ()
			 ((field-name get) ...))
		       (begin
			 (define-record-type name (make-tmp args ...) pred
			   (field-name get) ...)
			 (define (make . o)
			   (if (pair? o) (apply make-tmp o) (make-tmp defaults ...)))
			 (define x-ref
			   (let ((accessors (vector get ...)))
			     (lambda (x i)
			       ((vector-ref accessors i) x))))))))

		  (define-tuple-type point make-point point? point-ref (0 0))

		  (let ((pt (make-point)))
		    (test-equal '(0 0) (list (point-ref pt 0) (point-ref pt 1))))
		  (let ((pt (make-point 1 2)))
		    (test-equal '(1 2) (list (point-ref pt 0) (point-ref pt 1)))))
      
      (test-end))

;; Top-level definitions.
(define-record-type <tl-p> (make-tl-p x) tl-p? (x tl-p-x))
(define-record-type (<tl-c> <tl-p>) (make-tl-c x y) tl-c? (y tl-c-y))

(define (more-tests)
  (test-equal "top-level definitions" '(1 2 #t)
    (let ((c (make-tl-c 1 2))) (list (tl-p-x c) (tl-c-y c) (tl-p? c))))
  (test-group "Constant field names, #f constructor and predicate"
    (define-record-type <r> (make-r "a" 2) r? ("a" r-a) (2 r-2 set-r-2!))
    (define-record-type <s> #f #f (x s-x))
    (define r (make-r 1 2))
    (set-r-2! r 20)
    (test-equal '(1 20) (list (r-a r) (r-2 r)))
    (test-eq '<r> (record-type-name <r>))
    (test-eq <r> (record-rtd r))
    (test-assert (not (r? 5))))
  (test-group "Bare constructor name with a parent"
    (define-record-type <p> make-p p? (a p-a))
    (define-record-type (<c> <p>) make-c c? (b c-b) (c c-c))
    (test-equal '(1 2 3) (let ((c (make-c 1 2 3))) (list (p-a c) (c-b c) (c-c c))))
    (test-assert (p? (make-c 1 2 3))))
  (test-group "Uninitialized fields, and an explicit #f parent"
    (define-record-type (<q> #f) (make-q b) q? (a q-a) (b q-b))
    (test-equal '(#f 2) (let ((q (make-q 2))) (list (q-a q) (q-b q))))))

(test-begin "srfi-150")
(run-tests)
(more-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-150")
  (exit (if (zero? failures) 0 1)))
