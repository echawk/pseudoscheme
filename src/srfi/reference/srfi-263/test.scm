; SPDX-FileCopyrightText: 2026 Daniel Ziltener
;
; SPDX-License-Identifier: MIT

(include "srfi-263.scm")
(import (srfi 263))

(define testmethod
  (lambda (self resend) 'success))

;;; Basic Functionality

(assert (null? ((*the-root-object* 'mirror) 'immediate-ancestor-list)))
(assert (= 9 (length ((*the-root-object* 'mirror) 'immediate-message-alist))))

(let ((class (*the-root-object* 'derive)))
  (assert (eq? *the-root-object* (car ((class 'mirror) 'immediate-ancestor-list))))

  (class 'set-method-slot! 'testmethod testmethod)
  (assert (eq? 'success (class 'testmethod)))

  (class 'set-value-slot! 'val 'set-val! 10)
  (assert (eq? 10 (class 'val)))
  (class 'set-val! 20)
  (assert (eq? 20 (class 'val)))
  (assert (= 5 (length ((class 'mirror) 'immediate-message-alist))))
  (class 'set-value-slot! 'val 40)
  (assert (eq? 40 (class 'val)))
  (assert (= 4 (length ((class 'mirror) 'immediate-message-alist))))
  ;; Deleting the setter keeps the getter
  (class 'set-value-slot! 'val 'set-val! 10)
  (class 'delete-slot! 'set-val!)
  (assert (= 4 (length ((class 'mirror) 'immediate-message-alist))))
  ;; Deleting the getter also deletes the setter
  (class 'set-value-slot! 'val 'set-val! 10)
  (class 'delete-slot! 'val)
  (assert (= 3 (length ((class 'mirror) 'immediate-message-alist))))
  )

;;; Inheritance

(let* ((firstlevel (*the-root-object* 'derive))
       (secondlevel (firstlevel 'derive)))
  (firstlevel 'set-method-slot! 'testmethod testmethod)
  (assert (eq? 'success (secondlevel 'testmethod)))
  (firstlevel 'set-value-slot! 'val 'set-val! 10)
  (assert (eq? 10 (secondlevel 'val)))
  (secondlevel 'set-val! 20)
  (assert (eq? 10 (firstlevel 'val)))
  (assert (eq? 20 (secondlevel 'val)))
  (firstlevel 'set-value-slot! 'val #f 30)
  (assert (eq? 30 (firstlevel 'val)))
  (assert (eq? 20 (secondlevel 'val)))

  (assert (= 2 (length ((firstlevel 'mirror) 'full-ancestor-list))))
  (assert (= 3 (length ((secondlevel 'mirror) 'full-ancestor-list)))))

;;;; Multiple Inheritance

(let* ((adderclass (*the-root-object* 'derive))
       (squareclass (*the-root-object* 'derive))
       (mathclass (squareclass 'derive)))
  (adderclass 'set-method-slot! 'add1
              (lambda (self resend val)
                (add1 val)))
  (squareclass 'set-method-slot! 'square
               (lambda (self resend val)
                 (* val val)))
  (mathclass 'set-parent-slot! 'adder adderclass)
  (assert (= 10 (adderclass 'add1 9)))
  (assert (= 9 (squareclass 'square 3)))
  (assert (= 9 (mathclass 'add1 8)))

  (assert
   (call-with-current-continuation
    (lambda (cont)
      (with-exception-handler
          (lambda (exc) (cont #t))
        (lambda () (adderclass 'sub1 10)
           (cont #f))))
    ))

  (adderclass 'set-method-slot! 'reset (lambda (self resend x) 5))
  (squareclass 'set-method-slot! 'reset (lambda (self resend x) 5))

  (assert
   (call-with-current-continuation
    (lambda (cont)
      (with-exception-handler
          (lambda (exc) (cont #t))
        (lambda () (mathclass 'reset 1)
           (cont #f))))))

  (assert (= 2 (length ((mathclass 'mirror) 'immediate-ancestor-list))))
  (mathclass 'delete-slot! 'adder)
  (assert (= 1 (length ((mathclass 'mirror) 'immediate-ancestor-list))))
  )

;;; Syntax
(include "srfi-263-syntax.impl.scm")

(define-object testobject (*the-root-object*)
  (testmethod (self resend) 'success)
  (val 10)
  (testval set-testval! 50))

(assert (eq? 'success (testobject 'testmethod)))
(assert (eq? 10 (testobject 'val)))
(assert (eq? 50 (testobject 'testval)))
(testobject 'set-testval! 20)
(assert (eq? 20 (testobject 'testval)))

(set-method! (testobject methodslot self resend a b)
  (+ a b))
(assert (eq? 50 (testobject 'methodslot 20 30)))
