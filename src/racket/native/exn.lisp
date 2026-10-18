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
;;;
;;; As in rumble's: the uncaught-exception handler gives the message to
;;; the error display handler (which writes it to the error port, with
;;; the exception's source locations), then calls the error escape
;;; handler (which aborts to the default prompt).  Both are parameters
;;; (handlers.lisp has the other error parameters).

(defvar *uncaught* nil "Whether an exception went uncaught (the exit status).")

(defun exn-of-type-p (v type)
  "Whether V is an exn of the exn type named TYPE (or one of its subtypes)."
  (and (exn-p v) (subtype-p (struct-of-type v) (gethash type *exn-struct-types*))))

(defun default-error-display-handler (message v)
  "Racket's: MESSAGE on the error port, then where the exception came
from, if it says (there is no context to show: no traces of the calls)."
  (let ((out (current-port :error)))
    (port-write-string message out)
    (when (and (exn-p v) (not (exn-of-type-p v "exn:fail:user")))
      (port-write-string (error-display-locations v) out))
    (port-write-string (string #\Newline) out)
    *void*))

(defun default-error-escape-handler ()
  "Racket's: abort to the default prompt."
  (psx::abort-to-prompt *default-prompt-tag* (lambda () *void*)))

(defvar *error-display-handler* (make-parameter* #'default-error-display-handler))
(defvar *error-escape-handler* (make-parameter* #'default-error-escape-handler))

(defun default-uncaught-exception-handler (v)
  "Racket's: the message (or the value, written) to the error display
handler, then the error escape handler."
  (setq *uncaught* t)
  (finish-output *standard-output*)
  (let ((message (if (exn-p v)
		     (exn-message v)
		     (concatenate 'string "uncaught exception: " (error-value->string v)))))
    (funcall (funcall *error-display-handler*) message v))
  (finish-output *error-output*)
  (funcall (funcall *error-escape-handler*))
  ;; in case the escape handler doesn't escape
  (default-error-escape-handler))

(setq *uncaught-exception-handler* #'default-uncaught-exception-handler)

(defvar *uncaught-exception-handler-parameter*
  (make-parameter* #'default-uncaught-exception-handler))

(define-primitive "#%kernel" "uncaught-exception-handler" *uncaught-exception-handler-parameter*)

(setq *uncaught-exception-handler*
      (lambda (v) (funcall (funcall *uncaught-exception-handler-parameter*) v)))

(defun error-message-from (first rest)
  "The message of (error FIRST REST ...) and raise-user-error: (error
sym), (error sym format v ...) or (error string v ...)."
  (cond ((racket-symbol-p first)
	 (cond ((null rest) (format nil "error: ~A" (rname first)))
	       ((stringp (car rest))
		(format nil "~A: ~A" (rname first) (apply #'racket-format (car rest) (cdr rest))))
	       (t (arg-error "error" "string?" (car rest)))))
	((stringp first)
	 (with-output-to-string (s)
	   (write-string first s)
	   (dolist (v rest)
	     (write-char #\Space s)
	     (write-string (error-value->string v) s))))
	(t (arg-error "error" "(or/c symbol? string?)" first))))

(defnative "error" (first &rest rest) ()
  (do-raise (make-exn "exn:fail" (error-message-from first rest))))

(defun checked-format (who control args)
  "CONTROL formatted with ARGS for WHO (format, printf, ...): Racket's
checks of the pattern string and of the number of arguments first."
  (unless (stringp control) (contract-error who "string?" control))
  (let ((needed 0)
	(shown (lambda ()
		 (if args
		     (format nil "; arguments were:~{ ~A~}" (mapcar #'error-value->string args))
		     ""))))
    (loop with i = 0 while (< i (length control))
	  do (if (char/= (char control i) #\~)
		 (incf i)
		 (let ((d (and (< (1+ i) (length control)) (char control (1+ i)))))
		   (when (and d (char= d #\.) (< (+ i 2) (length control))) (incf i) (setq d (char control (1+ i))))
		   (cond ((null d)
			  (racket-error who "ill-formed pattern string~%  explanation: cannot end in `~~`~A" (funcall shown)))
			 ((find d "aAsSvVeEcCbBoOxX") (incf needed))
			 ((find d "n%~") nil)
			 ((member d '(#\Space #\Tab #\Newline #\Return #\Page))
			  ;; ~ and whitespace: skips it, and the whitespace after
			  (loop while (and (< (+ i 2) (length control)) (member (char control (+ i 2)) '(#\Space #\Tab #\Newline #\Return #\Page)))
				do (incf i)))
			 (t (racket-error who "ill-formed pattern string~%  explanation: tag `~~~A` not allowed~A"
					  d (funcall shown))))
		   (incf i 2))))
    (unless (= needed (length args))
      (racket-error who "format string requires ~D arguments, given ~D~A" needed (length args)
		    (if args (funcall shown) "")))
    (apply #'racket-format control args)))

(defun racket-format (control &rest args)
  "Racket's format directives: ~a ~s ~v ~e ~% ~n ~~."
  (with-output-to-string (out)
    (loop with i = 0
	  while (< i (length control))
	  do (let ((c (char control i)))
	       (if (and (char= c #\~) (< (1+ i) (length control)))
		   (let ((d (char-downcase (char control (1+ i)))))
		     ;; ~.a ~.s ~.v: truncated to error-print-width in Racket;
		     ;; whole here
		     (when (and (char= d #\.) (< (+ i 2) (length control)))
		       (incf i)
		       (setq d (char-downcase (char control (1+ i)))))
		     (case d
		       ;; Racket's printer (printer.lisp): ~v prints as print does
		       ((#\a) (print-value (pop args) out :display))
		       ((#\s) (print-value (pop args) out :write))
		       ((#\e) (write-string (error-value->string (pop args)) out))
		       ((#\v) (print-quoted (pop args) out))
		       ((#\c) (write-char (pop args) out))
		       ((#\b) (write (pop args) :stream out :base 2 :radix nil))
		       ((#\o) (write (pop args) :stream out :base 8 :radix nil))
		       ((#\x) (write-string (string-downcase (write-to-string (pop args) :base 16 :radix nil)) out))
		       ((#\% #\n) (terpri out))
		       (#\~ (write-char #\~ out))
		       (t (write-char c out) (write-char d out)))
		     (incf i 2))
		   (progn (write-char c out) (incf i)))))))

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
    (racket-error (make-exn (or (racket-error-type e) "exn:fail:contract") (princ-to-string e)))
    ;; SBCL's: a function called with the wrong number of arguments
    (sb-int:simple-program-error
     (if (search "invalid number of arguments" (princ-to-string e))
	 (make-exn "exn:fail:contract:arity"
		   (format nil "#<procedure>: arity mismatch;~% the expected number of arguments does not match the given number~@[~%  given: ~A~]"
			   (car (simple-condition-format-arguments e))))
	 (make-exn "exn:fail" (princ-to-string e))))
    (division-by-zero (make-exn "exn:fail:contract:divide-by-zero" "/: division by zero"))
    (sb-int:invalid-array-index-error
     (let* ((array (sb-kernel::invalid-array-index-error-array e))
	    (kind (typecase array (string "string") ((array (unsigned-byte 8) (*)) "bytes") (t "vector")))
	    (shown (with-output-to-string (s) (print-quoted array s))))
       (make-exn "exn:fail:contract"
		 (if (zerop (length array))
		     (format nil "index is out of range for empty ~A~%  index: ~A~%  ~A: ~A"
			     kind (type-error-datum e) kind shown)
		     (format nil "index is out of range~%  index: ~A~%  valid range: [0, ~D]~%  ~A: ~A"
			     (type-error-datum e) (1- (length array)) kind shown)))))
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
