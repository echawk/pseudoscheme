;;; Tests for SRFI 131: the SRFI's own test suite (reference/srfi-131/test.sld,
;;; from the SRFI 136 repository), as a program.
(import (except (scheme base) define-record-type) (scheme process-context)
        (srfi 64) (srfi 131)
        (rename (srfi 136) (define-record-type define-record-type/136)))

(define (run-tests)
      (test-begin "ERR5RS Record Syntax (reduced) on top of SRFI 136")

      (test-assert "Predicate"
		   (let ()
		     (define-record-type <record>
		       (make-record)
		       record?)
		     (record? (make-record))))
      
      (test-assert "Disjoint type"
		   (let ()
		     (define-record-type <record>
		       (make-record)
		       record?)
		     (not (vector? (make-record)))))

      (test-equal "Record fields"
		  '(1 2 3)
		  (let ()
		    (define-record-type <record>
		      (make-record foo baz)
		      record?
		      (foo foo)
		      (bar bar set-bar!)
		      (baz baz))
		    (define record (make-record 1 3))
		    (set-bar! record 2)
		    (list (foo record) (bar record) (baz record))))

      (test-equal "Subtypes"
		  '(#t #f)
		  (let ()
		    (define-record-type <parent>
		      (make-parent)
		      parent?)
		    (define-record-type (<child> <parent>)
		      (make-child)
		      child?)
		    (list (parent? (make-child)) (child? (make-parent)))))

      (test-equal "Inheritance of constructor"
		  '(1 2)
		  (let ()
		    (define-record-type <parent>
		      (make-parent foo)
		      parent?
		      (foo foo))
		    (define-record-type (<child> <parent>)
		      (make-child foo bar)
		      child?
		      (bar bar))
		    (define child (make-child 1 2))
		    (list (foo child) (bar child))))

      (test-equal "Default constructors"
		  1
		  (let ()
		    (define-record-type <parent>
		      (make-parent foo)
		      #f
		      (foo foo))
		    (define-record-type (<child> <parent>)
		      make-child
		      child?)
		    (define child (make-child 1))
		    (foo child)))

      (test-assert "Missing parent"
		   (let ()
		     (define-record-type (<record> #f)
		       (make-record)
		       record?)
		     (record? (make-record))))

      (test-equal "Interoperability with SRFI 136"
		  '(#t #t 1 2)
		  (let ()
		    (define-record-type/136 <parent>
		      (make-parent foo)
		      parent?
		      (foo foo))
		    (define-record-type (<child> <parent>)
		      (make-child foo bar)
		      child?
		      (bar bar))
		    (define record (make-child 1 2))
		    (list (parent? record)
			  (child? record)
			  (foo record)
			  (bar record))))
      
      (test-end))

(test-begin "srfi-131")
(run-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-131")
  (exit (if (zero? failures) 0 1)))
