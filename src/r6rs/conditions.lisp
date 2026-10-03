; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs conditions (6)) and (rnrs exceptions (6)), library report
;;;; chapter 7, plus the i/o condition types of 8.1 and the base library's
;;;; ERROR and ASSERTION-VIOLATION (language report 11.14).
;;;;
;;;; Simple conditions are records whose type descends from &condition;
;;;; a compound condition is a COMPOUND-CONDITION holding a list of them.
;;;; psyntax's (record-type-descriptor &foo) and DEFINE-CONDITION-TYPE
;;;; refer to host globals &foo-rtd and &foo-rcd, defined here.

(in-package "PSEUDOSCHEME-R6RS")

(defstruct (compound-condition (:constructor %make-compound (components)))
  components)

(defmethod print-object ((c compound-condition) stream)
  (print-unreadable-object (c stream)
    (format stream "Condition~{ ~A~}"
	    (mapcar (lambda (s) (scheme-name (rtd-name (record-rtd s))))
		    (compound-condition-components c)))))

(defvar *condition-rtd* nil)

(defun simple-condition-p (x)
  (and (record-p x) *condition-rtd* (rtd-descends-p (record-rtd x) *condition-rtd*)))

(defun condition-p* (x)
  (or (simple-condition-p x) (compound-condition-p x)))

(defun components (c)
  (cond ((compound-condition-p c) (compound-condition-components c))
	((simple-condition-p c) (list c))
	(t '())))

(defprim "condition" (&rest conditions)
  (let ((all (loop for c in conditions
		   do (unless (condition-p* c)
			(ps:scheme-error "condition: not a condition: ~S" c))
		   append (components c))))
    (if (= (length all) 1) (car all) (%make-compound all))))

(defprim "simple-conditions" (c)
  (unless (condition-p* c) (ps:scheme-error "simple-conditions: not a condition: ~S" c))
  (copy-list (components c)))

(defprim "condition?" (x) (ps:true? (condition-p* x)))

(defun condition-predicate* (rtd)
  (lambda (x)
    (ps:true? (some (lambda (s) (rtd-descends-p (record-rtd s) rtd)) (components x)))))

(defprim "condition-predicate" (rtd)
  (check-rtd "condition-predicate" rtd)
  (condition-predicate* rtd))

(defun condition-accessor* (rtd proc)
  (lambda (c)
    (let ((s (find-if (lambda (s) (rtd-descends-p (record-rtd s) rtd)) (components c))))
      (unless s (ps:scheme-error "condition accessor: ~S has no ~A component" c (rtd-name rtd)))
      (funcall proc s))))

(defprim "condition-accessor" (rtd proc)
  (check-rtd "condition-accessor" rtd)
  (condition-accessor* rtd proc))

;;; Standard condition types (7.3, 8.1, 11.3 of the library report).
;;; Each entry: name, parent, fields, constructor, predicate, accessors.

(defvar *condition-types* (make-hash-table :test 'equal)
  "\"&name\" -> rtd")

(defun scheme-sym (string) (ps:intern-scheme-symbol string))

(defun define-condition-type* (name parent fields constructor predicate accessors)
  (let* ((parent-rtd (and parent (gethash parent *condition-types*)))
	 (rtd (funcall (prim "make-record-type-descriptor")
		       (scheme-sym name) (or parent-rtd ps:false) ps:false ps:false ps:false
		       (map 'simple-vector (lambda (f) (list (scheme-sym "immutable") (scheme-sym f)))
			    fields)))
	 (rcd (funcall (prim "make-record-constructor-descriptor") rtd ps:false ps:false)))
    (setf (gethash name *condition-types*) rtd)
    (register-primitive (format nil "~A-rtd" name) rtd)
    (register-primitive (format nil "~A-rcd" name) rcd)
    (when constructor
      ;; All fields, inherited ones first: (make-i/o-encoding-error
      ;; port char), (make-i/o-file-protection-error filename), ...
      (register-primitive constructor (funcall (prim "record-constructor") rcd)))
    (when predicate
      (register-primitive predicate (condition-predicate* rtd)))
    (loop for accessor in accessors
	  for k from 0
	  do (register-primitive accessor
				 (condition-accessor* rtd (funcall (prim "record-accessor") rtd k))))
    rtd))

(defun prim (name)
  (or (cdr (assoc name *primitives* :test #'string=))
      (error "no primitive ~A" name)))

(defun register-primitive (name value)
  (let ((cell (assoc name *primitives* :test #'string=)))
    (if cell (setf (cdr cell) value)
	(setq *primitives* (nconc *primitives* (list (cons name value)))))))

(defparameter *standard-condition-types*
  '(("&condition" nil () nil nil ())
    ("&message" "&condition" ("message") "make-message-condition" "message-condition?" ("condition-message"))
    ("&warning" "&condition" () "make-warning" "warning?" ())
    ("&serious" "&condition" () "make-serious-condition" "serious-condition?" ())
    ("&error" "&serious" () "make-error" "error?" ())
    ("&violation" "&serious" () "make-violation" "violation?" ())
    ("&assertion" "&violation" () "make-assertion-violation" "assertion-violation?" ())
    ("&irritants" "&condition" ("irritants") "make-irritants-condition" "irritants-condition?" ("condition-irritants"))
    ("&who" "&condition" ("who") "make-who-condition" "who-condition?" ("condition-who"))
    ("&non-continuable" "&violation" () "make-non-continuable-violation" "non-continuable-violation?" ())
    ("&implementation-restriction" "&violation" () "make-implementation-restriction-violation" "implementation-restriction-violation?" ())
    ("&lexical" "&violation" () "make-lexical-violation" "lexical-violation?" ())
    ("&syntax" "&violation" ("form" "subform") "make-syntax-violation" "syntax-violation?" ("syntax-violation-form" "syntax-violation-subform"))
    ("&undefined" "&violation" () "make-undefined-violation" "undefined-violation?" ())
    ;; 8.1 I/O condition types
    ("&i/o" "&error" () "make-i/o-error" "i/o-error?" ())
    ("&i/o-read" "&i/o" () "make-i/o-read-error" "i/o-read-error?" ())
    ("&i/o-write" "&i/o" () "make-i/o-write-error" "i/o-write-error?" ())
    ("&i/o-invalid-position" "&i/o" ("position") "make-i/o-invalid-position-error" "i/o-invalid-position-error?" ("i/o-error-position"))
    ("&i/o-filename" "&i/o" ("filename") "make-i/o-filename-error" "i/o-filename-error?" ("i/o-error-filename"))
    ("&i/o-file-protection" "&i/o-filename" () "make-i/o-file-protection-error" "i/o-file-protection-error?" ())
    ("&i/o-file-is-read-only" "&i/o-file-protection" () "make-i/o-file-is-read-only-error" "i/o-file-is-read-only-error?" ())
    ("&i/o-file-already-exists" "&i/o-filename" () "make-i/o-file-already-exists-error" "i/o-file-already-exists-error?" ())
    ("&i/o-file-does-not-exist" "&i/o-filename" () "make-i/o-file-does-not-exist-error" "i/o-file-does-not-exist-error?" ())
    ("&i/o-port" "&i/o" ("port") "make-i/o-port-error" "i/o-port-error?" ("i/o-error-port"))
    ("&i/o-decoding" "&i/o-port" () "make-i/o-decoding-error" "i/o-decoding-error?" ())
    ("&i/o-encoding" "&i/o-port" ("char") "make-i/o-encoding-error" "i/o-encoding-error?" ("i/o-encoding-error-char"))
    ;; 11.3 (rnrs arithmetic flonums)
    ("&no-infinities" "&implementation-restriction" () "make-no-infinities-violation" "no-infinities-violation?" ())
    ("&no-nans" "&implementation-restriction" () "make-no-nans-violation" "no-nans-violation?" ())))

(defun install-condition-types ()
  (clrhash *condition-types*)
  (dolist (spec *standard-condition-types*)
    (apply #'define-condition-type* spec))
  (setq *condition-rtd* (gethash "&condition" *condition-types*)))

;;; ------------------------------------------------------------------
;;; Raising (rnrs exceptions; language report 11.14)

(defun make-standard-condition (kind who message irritants)
  (apply (prim "condition")
	 (append (list (funcall (prim kind)))
		 (if (and who (not (eq who ps:false)))
		     (list (funcall (prim "make-who-condition") who))
		     '())
		 (list (funcall (prim "make-message-condition") message)
		       (funcall (prim "make-irritants-condition") irritants)))))

(defun r6rs-assertion-violation (who message &rest irritants)
  (ps-r7rs:raise-object (make-standard-condition "make-assertion-violation" who message irritants) nil))

(defprim "error" (who &optional (message "" message-p) &rest irritants)
  ;; Tolerate a lone argument, which some of psyntax's own calls pass.
  (if message-p
      (ps-r7rs:raise-object (make-standard-condition "make-error" who message irritants) nil)
      (ps-r7rs:raise-object (make-standard-condition "make-error" ps:false who '()) nil)))

(defprim "assertion-violation" (who message &rest irritants)
  (apply #'r6rs-assertion-violation who message irritants))

(defprim "syntax-violation" (who message form &optional (subform ps:false))
  (ps-r7rs:raise-object
   (funcall (prim "condition")
	    (funcall (prim "make-syntax-violation") form subform)
	    (if (eq who ps:false)
		(funcall (prim "make-message-condition") message)
		(funcall (prim "condition")
			 (funcall (prim "make-who-condition") who)
			 (funcall (prim "make-message-condition") message))))
   nil))

(defun foreign-condition (c)
  "An R6RS condition standing for the Lisp condition C, so handlers in
Scheme see something CONDITION? and MESSAGE-CONDITION? are true of."
  (multiple-value-bind (message irritants) (foreign-condition-message c)
    (apply (prim "condition")
	   (append
	    (typecase c
	      (file-error
	       (list (funcall (prim "make-i/o-filename-error")
			      (namestring (or (file-error-pathname c) "")))))
	      (reader-error (list (funcall (prim "make-lexical-violation"))))
	      (t (list (funcall (prim "make-assertion-violation")))))
	    (list (funcall (prim "make-message-condition") message)
		  (funcall (prim "make-irritants-condition") irritants))))))

(defun foreign-condition-message (c)
  "The message and irritants a Scheme handler sees for Lisp condition C.
Applying a non-procedure shows up in Lisp as calling an undefined
function (#f is a symbol) or as a type error expecting a function."
  (flet ((plain () (values (remove #\Newline (princ-to-string c)) '())))
    (typecase c
      (undefined-function
       (if (eq (cell-error-name c) ps:false)
	   (values "attempt to apply non-procedure" (list ps:false))
	   (plain)))
      (type-error
       (cond ((subtypep 'function (type-error-expected-type c))
	      (values "attempt to apply non-procedure" (list (type-error-datum c))))
	     ;; car/cdr of a non-pair (builtin.scm checks with (the cons x))
	     ((member (type-error-expected-type c) '(cons list))
	      (values "not a pair" (list (type-error-datum c))))
	     (t (plain))))
      ;; THROW to a dead tag: see PS:CALL-WITH-ESCAPE
      (control-error
       (values "continuation invoked after its extent ended (continuations are escape-only)" '()))
      (t (plain)))))

(defun describe-condition (c stream)
  "Print condition C the way a REPL reports an uncaught one:
  error in WHO: MESSAGE IRRITANT ...   [&type ...]"
  (when (condition-p* c)
    (let ((who (component-of c "&who"))
	  (message (component-of c "&message"))
	  (irritants (component-of c "&irritants"))
	  (syntax (component-of c "&syntax")))
      (format stream "~:[~;~:*~A: ~]~A~{ ~S~}~@[~%  in form: ~S~]~%  [~{~A~^ ~}]"
	      (and who (let ((w (svref (record-values who) 0)))
			 (if (symbolp w) (ps:scheme-symbol-name w) w)))
	      (if message (svref (record-values message) 0) "")
	      (if irritants (svref (record-values irritants) 0) '())
	      (and syntax (svref (record-values syntax) 0))
	      (mapcar (lambda (s) (ps:scheme-symbol-name (rtd-name (record-rtd s)))) (components c))))
    t))

(defun install-exception-hooks ()
  (setq ps-r7rs:*condition-describer* #'describe-condition)
  (setq ps-r7rs:*foreign-condition-converter* #'foreign-condition
	ps-r7rs:*non-continuable-condition*
	(lambda (obj)
	  (funcall (prim "condition")
		   (funcall (prim "make-non-continuable-violation"))
		   (funcall (prim "make-message-condition") "handler returned from non-continuable raise")
		   (funcall (prim "make-irritants-condition") (list obj))))))

;;; R7RS's views of the same objects (R7RS 6.11), so R7RS code running
;;; in the R6RS host sees one exception model.

(defprim "error-object?" (x) (ps:true? (condition-p* x)))
(defun component-of (x type)
  (let ((rtd (gethash type *condition-types*)))
    (find-if (lambda (s) (rtd-descends-p (record-rtd s) rtd)) (components x))))

(defprim "error-object-message" (x)
  (let ((s (component-of x "&message")))
    (if s (svref (record-values s) 0) "")))
(defprim "error-object-irritants" (x)
  (let ((s (component-of x "&irritants")))
    (if s (svref (record-values s) 0) '())))
(defprim "file-error?" (x) (ps:true? (component-of x "&i/o-filename")))
(defprim "read-error?" (x)
  (ps:true? (or (component-of x "&lexical") (component-of x "&i/o-read"))))

(install-condition-types)
