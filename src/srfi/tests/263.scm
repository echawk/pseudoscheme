;;; Tests for SRFI 263: the reference implementation's test.scm
;;; (reference/srfi-263/test.scm) in SRFI 64, plus the SRFI document's
;;; examples.  Changes to the reference tests: full-ancestor-list counts
;;; the mirrored object's ancestors (1 and 2, where the reference
;;; implementation counted its mirror's, 2 and 3); add1 is (+ val 1);
;;; the syntax tests use the SRFI's method form ((name self resend) body).
(import (scheme base) (scheme process-context) (srfi 64)
        (srfi 263) (srfi 263 syntax))

(define testmethod
  (lambda (self resend) 'success))

(define (raises? thunk)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e) (k #t))
      (lambda () (thunk) #f)))))

(test-begin "srfi-263")

;;; Basic functionality
(test-assert (null? ((*the-root-object* 'mirror) 'immediate-ancestor-list)))
(test-equal 9 (length ((*the-root-object* 'mirror) 'immediate-message-alist)))

(let ((class (*the-root-object* 'derive)))
  (test-assert (eq? *the-root-object* (car ((class 'mirror) 'immediate-ancestor-list))))
  (class 'set-method-slot! 'testmethod testmethod)
  (test-eq 'success (class 'testmethod))
  (class 'set-value-slot! 'val 'set-val! 10)
  (test-eqv 10 (class 'val))
  (class 'set-val! 20)
  (test-eqv 20 (class 'val))
  (test-equal 5 (length ((class 'mirror) 'immediate-message-alist)))
  (class 'set-value-slot! 'val 40)
  (test-eqv 40 (class 'val))
  (test-equal 4 (length ((class 'mirror) 'immediate-message-alist)))
  ;; Deleting the setter keeps the getter
  (class 'set-value-slot! 'val 'set-val! 10)
  (class 'delete-slot! 'set-val!)
  (test-equal 4 (length ((class 'mirror) 'immediate-message-alist)))
  ;; Deleting the getter also deletes the setter
  (class 'set-value-slot! 'val 'set-val! 10)
  (class 'delete-slot! 'val)
  (test-equal 3 (length ((class 'mirror) 'immediate-message-alist))))

;;; Inheritance
(let* ((firstlevel (*the-root-object* 'derive))
       (secondlevel (firstlevel 'derive)))
  (firstlevel 'set-method-slot! 'testmethod testmethod)
  (test-eq 'success (secondlevel 'testmethod))
  (firstlevel 'set-value-slot! 'val 'set-val! 10)
  (test-eqv 10 (secondlevel 'val))
  (secondlevel 'set-val! 20)
  (test-eqv 10 (firstlevel 'val))
  (test-eqv 20 (secondlevel 'val))
  (firstlevel 'set-value-slot! 'val #f 30)
  (test-eqv 30 (firstlevel 'val))
  (test-eqv 20 (secondlevel 'val))
  (test-equal 1 (length ((firstlevel 'mirror) 'full-ancestor-list)))
  (test-equal 2 (length ((secondlevel 'mirror) 'full-ancestor-list)))
  (test-assert ((secondlevel 'mirror) 'has-ancestor *the-root-object*))
  (test-assert ((secondlevel 'mirror) 'has-ancestor firstlevel))
  (test-assert (not ((firstlevel 'mirror) 'has-ancestor secondlevel)))
  (test-assert (slot? (car ((secondlevel 'mirror) 'full-slot-list))))
  (test-assert (memq 'val (map slot-getter ((secondlevel 'mirror) 'full-slot-list))))
  (test-assert (memq 'testmethod (map slot-getter ((secondlevel 'mirror) 'full-slot-list)))))

;;; Multiple inheritance
(let* ((adderclass (*the-root-object* 'derive))
       (squareclass (*the-root-object* 'derive))
       (mathclass (squareclass 'derive)))
  (adderclass 'set-method-slot! 'add1
              (lambda (self resend val)
                (+ val 1)))
  (squareclass 'set-method-slot! 'square
               (lambda (self resend val)
                 (* val val)))
  (mathclass 'set-parent-slot! 'adder adderclass)
  (test-equal 10 (adderclass 'add1 9))
  (test-equal 9 (squareclass 'square 3))
  (test-equal 9 (mathclass 'add1 8))
  (test-equal 16 (mathclass 'square 4))
  (test-assert (raises? (lambda () (adderclass 'sub1 10))))
  (adderclass 'set-method-slot! 'reset (lambda (self resend x) 5))
  (squareclass 'set-method-slot! 'reset (lambda (self resend x) 5))
  (test-assert (raises? (lambda () (mathclass 'reset 1))))
  (test-equal 2 (length ((mathclass 'mirror) 'immediate-ancestor-list)))
  (mathclass 'delete-slot! 'adder)
  (test-equal 1 (length ((mathclass 'mirror) 'immediate-ancestor-list))))

;;; message-not-understood can be overridden
(let ((o (*the-root-object* 'derive)))
  (o 'set-method-slot! 'message-not-understood
     (lambda (self resend message args) (list 'unknown message args)))
  (test-equal '(unknown frob (1 2)) (o 'frob 1 2)))

;;; resend
(let* ((base (*the-root-object* 'derive))
       (derived (base 'derive)))
  (base 'set-method-slot! 'describe (lambda (self resend) 'base))
  (derived 'set-method-slot! 'describe
           (lambda (self resend) (list 'derived (resend #f))))
  (test-equal '(derived base) (derived 'describe)))

;;; copy
(let* ((o (*the-root-object* 'derive)))
  (o 'set-value-slot! 'x 'set-x! 1)
  (let ((c (o 'copy)))
    (test-eqv 1 (c 'x))
    (c 'set-x! 2)
    (test-eqv 2 (c 'x))
    (test-eqv 1 (o 'x))))

;;; Private messages
(let ((secret (list 'secret))
      (o (*the-root-object* 'derive)))
  (o 'set-value-slot! secret 42)
  (test-eqv 42 (o secret))
  (test-assert (raises? (lambda () (o (list 'secret))))))

;;; Syntax
(define-object testobject (*the-root-object*)
  ((testmethod self resend) 'success)
  (val 10)
  (testval set-testval! 50))

(test-eq 'success (testobject 'testmethod))
(test-eqv 10 (testobject 'val))
(test-eqv 50 (testobject 'testval))
(testobject 'set-testval! 20)
(test-eqv 20 (testobject 'testval))

(define-method (testobject methodslot self resend a b)
  (+ a b))
(test-eqv 50 (testobject 'methodslot 20 30))
(set-method! (testobject methodslot2 self resend a)
  (* a 2))
(test-eqv 6 (testobject 'methodslot2 3))

;; The SRFI document's example
(define-object o (*the-root-object*)
  (constant set-constant! 5)
  ((add self resend summand)
   (+ summand (self 'constant))))
(test-equal 15 (o 'add 10))
(o 'set-constant! 7)
(test-equal 17 (o 'add 10))
(define-method (o average self resend a b) (/ (+ a b) 2))
(test-equal 3 (o 'average 2 4))

(define other (derive-object (*the-root-object* (helper o)) (k 1)))
(test-equal 12 (other 'add 5))
(test-equal 1 (other 'k))
(define copy (copy-object (o)))
(test-equal 17 (copy 'add 10))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-263")
  (exit (if (zero? failures) 0 1)))
