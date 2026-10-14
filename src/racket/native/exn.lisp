; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's exceptions, natively: the exn struct types of #%kernel,
;;;; the uncaught-exception handler, and Lisp errors as Racket
;;;; exceptions.
;;;;
;;;; The exn types are structs made by make-struct-type (structs.lisp);
;;;; each is #%kernel's struct:NAME, its constructor NAME, its predicate
;;;; NAME? and its own fields' accessors NAME-FIELD.
;;;;
;;;; A Lisp error -- a host primitive's (car of a non-pair), or
;;;; RACKET-ERROR's -- becomes a Racket exception where it's signalled:
;;;; WITH-RACKET-ERRORS's handler raises an exn:fail (exn:fail:contract
;;;; for a contract violation or a type error, ...:divide-by-zero for
;;;; division by zero) with Racket's raise, so that with-handlers sees
;;;; it.  Racket's messages are "who: message".

(in-package "PSEUDOSCHEME-RACKET")

(defparameter *exn-types*
  '(("exn" nil ("message" "continuation-marks"))
    ("exn:fail" "exn" ())
    ("exn:fail:contract" "exn:fail" ())
    ("exn:fail:contract:arity" "exn:fail:contract" ())
    ("exn:fail:contract:divide-by-zero" "exn:fail:contract" ())
    ("exn:fail:contract:non-fixnum-result" "exn:fail:contract" ())
    ("exn:fail:contract:continuation" "exn:fail:contract" ())
    ("exn:fail:contract:variable" "exn:fail:contract" ("id"))
    ("exn:fail:read" "exn:fail" ("srclocs"))
    ("exn:fail:read:eof" "exn:fail:read" ())
    ("exn:fail:read:non-char" "exn:fail:read" ())
    ("exn:fail:filesystem" "exn:fail" ())
    ("exn:fail:filesystem:exists" "exn:fail:filesystem" ())
    ("exn:fail:filesystem:version" "exn:fail:filesystem" ())
    ("exn:fail:filesystem:errno" "exn:fail:filesystem" ("errno"))
    ("exn:fail:network" "exn:fail" ())
    ("exn:fail:network:errno" "exn:fail:network" ("errno"))
    ("exn:fail:out-of-memory" "exn:fail" ())
    ("exn:fail:unsupported" "exn:fail" ())
    ("exn:fail:user" "exn:fail" ())
    ("exn:break" "exn" ("continuation"))
    ("exn:break:hang-up" "exn:break" ())
    ("exn:break:terminate" "exn:break" ()))
  "#%kernel's exn types: name, parent, own fields.")

(defvar *exn-struct-types* (make-hash-table :test 'equal) "Name -> struct type.")
(defvar *exn-constructors* (make-hash-table :test 'equal) "Name -> constructor.")

(defun make-struct-type* (&rest args)
  (apply (native-function (gethash "make-struct-type" *natives*)) args))

(defun define-exn-types ()
  (loop for (name parent fields) in *exn-types*
	do (multiple-value-bind (type constructor predicate accessor)
	       (make-struct-type* (rsym name) (if parent (gethash parent *exn-struct-types*) ps:false)
				  (length fields) 0 ps:false '() ps:false ps:false
				  (loop for i below (length fields) collect i))
	     (setf (gethash name *exn-struct-types*) type
		   (gethash name *exn-constructors*) constructor)
	     (define-primitive "#%kernel" (format nil "struct:~A" name) type)
	     (define-primitive "#%kernel" name constructor)
	     (define-primitive "#%kernel" (format nil "~A?" name) predicate)
	     (loop for field in fields
		   for i from 0
		   do (define-primitive "#%kernel" (format nil "~A-~A" name field)
			(let ((i i)) (lambda (x) (funcall accessor x i))))))))

(define-exn-types)

(defun make-exn (type message &rest more-fields)
  "An exn of TYPE (its name, as \"exn:fail\"), with the current marks."
  (apply (gethash type *exn-constructors*)
	 (coerce message 'simple-string)
	 (ps-r6rs::make-mark-set (current-marks))
	 more-fields))

(defun exn-message (exn)
  (svref (struct-fields exn) 0))

(defun exn-p (x)
  (and (struct-instance-p x) (subtype-p (struct-of-type x) (gethash "exn" *exn-struct-types*))))

;;; ------------------------------------------------------------------
;;; Uncaught exceptions

(defvar *uncaught* nil "Whether an exception went uncaught (the exit status).")

(defun default-uncaught-exception-handler (v)
  "Racket's: the message (or the value, written) on the error port, then
an abort to the default prompt."
  (setq *uncaught* t)
  (finish-output *standard-output*)
  (if (exn-p v)
      (format *error-output* "~A~%" (exn-message v))
      (format *error-output* "uncaught exception: ~A~%"
	      (with-output-to-string (s) (funcall (psx:host-ref "write") v s))))
  (finish-output *error-output*)
  (psx::abort-to-prompt *default-prompt-tag* (lambda () *void*)))

(setq *uncaught-exception-handler* #'default-uncaught-exception-handler)

(defvar *uncaught-exception-handler-parameter*
  (make-parameter* #'default-uncaught-exception-handler))

(define-primitive "#%kernel" "uncaught-exception-handler" *uncaught-exception-handler-parameter*)

(setq *uncaught-exception-handler*
      (lambda (v) (funcall (funcall *uncaught-exception-handler-parameter*) v)))

(defnative "error" (first &rest rest) ()
  ;; (error sym) / (error msg v ...) / (error src format v ...)
  (do-raise
   (make-exn "exn:fail"
	     (cond ((and (symbolp first) (null rest)) (format nil "error: ~A" (rname first)))
		   ((symbolp first)
		    (format nil "~A: ~A" (rname first)
			    (apply #'racket-format (car rest) (cdr rest))))
		   (t (with-output-to-string (s)
			(write-string first s)
			(dolist (v rest)
			  (write-char #\Space s)
			  (funcall (psx:host-ref "write") v s))))))))

(defun racket-format (control &rest args)
  "Racket's format directives: ~a ~s ~v ~e ~% ~n ~~."
  (with-output-to-string (out)
    (loop with i = 0
	  while (< i (length control))
	  do (let ((c (char control i)))
	       (if (and (char= c #\~) (< (1+ i) (length control)))
		   (let ((d (char-downcase (char control (1+ i)))))
		     (case d
		       ((#\a) (funcall (psx:host-ref "display") (pop args) out))
		       ((#\s #\v #\e) (funcall (psx:host-ref "write") (pop args) out))
		       ((#\% #\n) (terpri out))
		       (#\~ (write-char #\~ out))
		       (t (write-char c out) (write-char d out)))
		     (incf i 2))
		   (progn (write-char c out) (incf i)))))))

(defnative "raise-argument-error" (who expected value &rest more) ()
  (declare (ignore more))
  (do-raise (make-exn "exn:fail:contract"
		      (format nil "~A: contract violation~%  expected: ~A~%  given: ~A"
			      (rname who) expected
			      (with-output-to-string (s) (funcall (psx:host-ref "write") value s))))))

;;; ------------------------------------------------------------------
;;; Lisp errors as Racket exceptions

(defun racket-type-name (type)
  "The Racket predicate for Lisp type TYPE, as Racket's messages name it."
  (let ((name (cond ((member type '(cons list)) "pair?")
		    ((member type '(number real)) "number?")
		    ((member type '(integer fixnum)) "exact-integer?")
		    ((member type '(float double-float)) "flonum?")
		    ((member type '(string simple-string)) "string?")
		    ((member type '(character)) "char?")
		    ((member type '(symbol)) "symbol?")
		    ((member type '(function)) "procedure?")
		    ((member type '(vector simple-vector)) "vector?")
		    ((equal type '(simple-array (unsigned-byte 8) (*))) "bytes?"))))
    (or name (string-downcase (princ-to-string type)))))

(defun lisp-error-exn (e)
  (typecase e
    (racket-error (make-exn "exn:fail:contract" (princ-to-string e)))
    (division-by-zero (make-exn "exn:fail:contract:divide-by-zero" "/: division by zero"))
    (type-error (make-exn "exn:fail:contract"
			  (format nil "contract violation~%  expected: ~A~%  given: ~A"
				  (racket-type-name (type-error-expected-type e))
				  (with-output-to-string (s)
				    (funcall (psx:host-ref "write") (type-error-datum e) s)))))
    (t (make-exn "exn:fail" (princ-to-string e)))))

(defun note-lisp-error (e)
  "With PSEUDOSCHEME_RACKET_BACKTRACE set, Lisp error E and where it was
signalled, on the error output: for finding what a Racket program ran
into that isn't written yet."
  (when (uiop:getenv "PSEUDOSCHEME_RACKET_BACKTRACE")
    (format *error-output* "~&;; Lisp error: ~A~%" e)
    (let ((*print-length* 5) (*print-level* 3))
      (sb-debug:print-backtrace :count 25 :stream *error-output*))))

(defmacro with-racket-errors (&body body)
  "BODY, its Lisp errors raised as Racket exceptions."
  `(handler-bind ((error (lambda (e)
			   (note-lisp-error e)
			   (unless (typep e 'not-yet)
			     (do-raise (lisp-error-exn e))))))
     ,@body))
