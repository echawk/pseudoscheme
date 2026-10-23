; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Errors, handlers and the small parameters, natively: the raisers
;;;; (raise-argument-error and its family), the error parameters and
;;;; handlers, loggers with receivers, equal-always? and the hash codes
;;;; that agree with it, srcloc->string, procedure-rename, composable
;;;; continuations, and the parameters that say how Racket runs (read-,
;;;; compile-, current-command-line-arguments, ...).
;;;;
;;;; The messages are those of rumble's error.ss and the io layer's
;;;; logger (racket/src/io/logger/): a raiser builds its message, passes
;;;; it through the current-error-message-adjuster, and raises an exn
;;;; with Racket's raise.  Values in messages go through the
;;;; error-value->string-handler, which truncates to error-print-width.
;;;;
;;;; This file replaces what basics.lisp, equal.lisp, control.lisp (the
;;;; loggers) and more.lisp (exit, continuation?) had for these names: it
;;;; loads after them, and a native defined later wins.
;;;; Not yet: the print-graph parameter (printer.lisp has the others);
;;;; log receivers on stderr (the root logger has none); marks
;;;; remembered by a continuation (continuation-marks gives the current
;;;; ones for one the Chez mode's hook hasn't noted).

(in-package "QUASISCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Small helpers

(defun racket-symbol-p (x)
  "Whether X is a Racket symbol (the host's symbol?, which says so with a Scheme boolean)."
  (let ((r (funcall (qsx:host-ref "symbol?") x)))
    (and r (truthy r))))

(defun who-string (who)
  "WHO, a symbol or a string, as the start of a message."
  (cond ((stringp who) who)
	((racket-symbol-p who) (rname who))
	(t "#<unknown>")))

(defvar *primitive-realm* (rsym "racket/primitive"))
(defvar *default-realm* (rsym "racket"))

(defun accepts-arity-p (f k)
  "Whether F is a procedure that accepts K arguments."
  (and (functionp f)
       (let ((mask (ignore-errors (arity-mask f))))
	 (or (null mask) (logbitp k mask)))))

(defun object-name-of (f)
  (funcall (native-function (gethash "object-name" *natives*)) f))

(defun immutable-copy (s)
  (if (immutable-p s) s (immutable! (copy-seq s))))

(defmacro define-param (variable name value &optional guard)
  "VARIABLE, a parameter named NAME that has VALUE, also #%kernel's NAME."
  `(progn (defvar ,variable (make-parameter* ,value ,guard (rsym ,name)))
	  (define-primitive "#%kernel" ,name ,variable)))

(defun guard-check (who test expected)
  "A parameter guard that accepts what TEST accepts, and else says it
wanted EXPECTED."
  (lambda (v)
    (unless (funcall test v) (arg-error who expected v))
    v))

(defun guard-arity (who k)
  (lambda (v)
    (unless (accepts-arity-p v k)
      (arg-error who (format nil "(procedure-arity-includes/c ~D)" k) v))
    v))

(defun nonnegative-integer-p (v) (and (integerp v) (>= v 0)))

;;; A parameter whose first value is made when it's first asked for (a
;;; custodian or thread group that the thread layer has the type of)
(defvar *lazy-unset* (make-symbol "unset"))

(defun make-lazy-parameter (name make &optional guard)
  (let* ((data (make-parameter-data (list *lazy-unset*) guard (rsym name)))
	 (p (lambda (&optional (new nil new-p))
	      (let ((cell (parameter-cell data)))
		(cond (new-p (setf (car cell) (if guard (funcall guard new) new)) *void*)
		      (t (when (eq (car cell) *lazy-unset*) (setf (car cell) (funcall make)))
			 (car cell)))))))
    (setf (gethash p *parameters*) data)
    (define-primitive "#%kernel" name p)
    p))

(defstruct (stand-in (:constructor make-stand-in (kind))) kind)
(defmethod print-object ((x stand-in) stream) (format stream "#<~A>" (stand-in-kind x)))

(defun native-or-stand-in (name kind)
  "What the native NAME makes if there is one (the thread layer's), else a
stand-in of KIND."
  (let ((n (gethash name *natives*)))
    (if n (funcall (native-function n)) (make-stand-in kind))))

;;; ------------------------------------------------------------------
;;; Error messages: error-value->string, the adjuster

(defun truncated-string (s len)
  "S, if it fits in LEN characters, else its start and ..., LEN in all."
  (if (> (length s) len)
      (concatenate 'string (subseq s 0 (max 0 (- len 3))) "...")
      s))

(defclass bounded-output (trivial-gray-streams:fundamental-character-output-stream)
  ((out :initform (make-string-output-stream) :reader bounded-out)
   (left :initarg :left :accessor bounded-left))
  (:documentation "A string output stream that gives up past a limit."))

(defmethod trivial-gray-streams:stream-write-char ((s bounded-output) c)
  (when (minusp (decf (bounded-left s))) (throw 'bounded-output nil))
  (write-char c (bounded-out s)))

(defun bounded-print (v len)
  "V as print writes it, cut off a little past LEN characters, so that a
cyclic value is printed only as far as is shown."
  (let ((s (make-instance 'bounded-output :left (+ len 1))))
    (catch 'bounded-output (print-quoted v s))
    (get-output-stream-string (bounded-out s))))

(defun default-error-value->string-handler (v len)
  "Racket's: V as print writes it, in at most LEN characters."
  (truncated-string (bounded-print v len) len))

(define-param *error-value->string-handler* "error-value->string-handler"
  #'default-error-value->string-handler
  (guard-arity "error-value->string-handler" 2))

(defun error-value->string (v)
  "V as the current error-value->string-handler shows it in an error message."
  (let ((s (funcall (funcall *error-value->string-handler*) v (funcall *error-print-width*))))
    (cond ((stringp s) s)
	  ((typep s '(simple-array (unsigned-byte 8) (*)))
	   (sb-ext:octets-to-string s :external-format :utf-8))
	  (t "..."))))

(setf (parameter-data-guard (gethash *error-print-width* *parameters*))
      (guard-check "error-print-width" (lambda (v) (and (integerp v) (>= v 3)))
		   "(and/c exact-integer? (>=/c 3))"))

(defun default-error-message-adjuster (mode)
  "Racket's default current-error-message-adjuster: no change, to every
kind of adjustment."
  (unless (racket-symbol-p mode) (arg-error "default-error-message-adjuster" "symbol?" mode))
  (let ((m (rname mode)))
    (cond ((string= m "name") (lambda (name realm) (values name realm)))
	  ((string= m "contract") (lambda (string realm) (values string realm)))
	  ((string= m "message")
	   (lambda (from from-realm msg msg-realm) (values from from-realm msg msg-realm)))
	  (t qs:false))))

(define-param *current-error-message-adjuster* "current-error-message-adjuster"
  #'default-error-message-adjuster
  (guard-arity "current-error-message-adjuster" 1))

(defvar *error-message-adjuster-key* (make-symbol "error-message"))
(define-primitive "#%kernel" "error-message-adjuster-key" *error-message-adjuster-key*)

(defun adjuster-procedures ()
  "The adjusters in effect: the marks' (innermost first) that are
procedures of one argument, then the parameter's; none, if only the
default one."
  (let ((procs (loop for m in (current-marks)
		     when (and (eq (cadr m) *error-message-adjuster-key*) (accepts-arity-p (cddr m) 1))
		       collect (cddr m)))
	(parameter (funcall *current-error-message-adjuster*)))
    (if (and (null procs) (eq parameter #'default-error-message-adjuster))
	'()
	(append procs (list parameter)))))

(defun apply-adjuster (adjuster vals modes)
  "VALS, adjusted by ADJUSTER in the first of MODES it has an adjustment
for.  A mode is (name argument-count applies? applier): APPLIES? says
whether it applies to VALS, and APPLIER gets the adjustment and VALS."
  (dolist (mode modes vals)
    (destructuring-bind (name n applies? applier) mode
      (let ((adjustment (and (funcall applies? vals) (funcall adjuster (rsym name)))))
	(when (and adjustment (truthy adjustment))
	  (unless (accepts-arity-p adjustment n)
	    (contract-violation "current-error-message-adjuster" *primitive-realm* "result"
				(format nil "(or/c (procedure-arity-includes/c ~D) #f)" n)
				nil adjustment nil))
	  (return (funcall applier adjustment vals)))))))

(defun adjusted-message (name name-realm msg msg-realm)
  "error-message->adjusted-string's: NAME (a symbol, or #f) and MSG after
the adjusters, as the message \"name: msg\"."
  (let ((vals (vector name name-realm msg msg-realm))
	(who "current-error-message-adjuster for ")
	(adjusters (adjuster-procedures)))
    (when adjusters
      (let ((modes
	      (list (list "message" 4 (lambda (v) (declare (ignore v)) t)
			  (lambda (proc v)
			    (let ((results (multiple-value-list
					    (funcall proc (svref v 0) (svref v 1) (svref v 2) (svref v 3)))))
			      (unless (= (length results) 4)
				(result-arity-error (concatenate 'string who "message") 4 qs:false results))
			      (destructuring-bind (name name-realm msg msg-realm) results
				(unless (or (not (truthy name)) (racket-symbol-p name))
				  (result-error (concatenate 'string who "message") "(or/c symbol? #f)" name))
				(unless (racket-symbol-p name-realm)
				  (result-error (concatenate 'string who "message") "symbol?" name-realm))
				(unless (stringp msg)
				  (result-error (concatenate 'string who "message") "string?" msg))
				(unless (racket-symbol-p msg-realm)
				  (result-error (concatenate 'string who "message") "symbol?" msg-realm))
				(vector name name-realm msg msg-realm)))))
		    (list "name" 2 (lambda (v) (truthy (svref v 0)))
			  (lambda (proc v)
			    (let ((results (multiple-value-list (funcall proc (svref v 0) (svref v 1)))))
			      (unless (= (length results) 2)
				(result-arity-error (concatenate 'string who "name") 2 qs:false results))
			      (destructuring-bind (name name-realm) results
				(unless (or (not (truthy name)) (racket-symbol-p name))
				  (result-error (concatenate 'string who "name") "(or/c symbol? #f)" name))
				(unless (racket-symbol-p name-realm)
				  (result-error (concatenate 'string who "name") "symbol?" name-realm))
				(vector name name-realm (svref v 2) (svref v 3)))))))))
	(dolist (adjuster adjusters)
	  (setq vals (apply-adjuster adjuster vals modes)))))
    (let ((name (svref vals 0)) (msg (svref vals 2)))
      (if (truthy name)
	  (concatenate 'string (who-string name) ": " msg)
	  msg))))

(defun adjusted-contract (contract realm)
  "error-contract->adjusted-string's: CONTRACT after the adjusters."
  (let ((vals (cons contract realm))
	(adjusters (adjuster-procedures)))
    (when adjusters
      (let ((modes
	      (list (list "contract" 2 (lambda (v) (declare (ignore v)) t)
			  (lambda (proc v)
			    (let ((results (multiple-value-list (funcall proc (car v) (cdr v)))))
			      (unless (= (length results) 2)
				(result-arity-error "current-error-message-adjuster for contract" 2
							  qs:false results))
			      (destructuring-bind (ctc realm) results
				(unless (stringp ctc)
				  (result-error "current-error-message-adjuster for contract" "string?" ctc))
				(unless (racket-symbol-p realm)
				  (result-error "current-error-message-adjuster for contract" "symbol?" realm))
				(cons ctc realm))))))))
	(dolist (adjuster adjusters)
	  (setq vals (apply-adjuster adjuster vals modes)))))
    (car vals)))

(defnative "error-message->adjusted-string" (name name-realm msg msg-realm) ()
  (unless (or (not (truthy name)) (racket-symbol-p name))
    (arg-error "error-message->adjusted-string" "(or/c symbol? #f)" name))
  (unless (racket-symbol-p name-realm) (arg-error "error-message->adjusted-string" "symbol?" name-realm))
  (unless (stringp msg) (arg-error "error-message->adjusted-string" "string?" msg))
  (unless (racket-symbol-p msg-realm) (arg-error "error-message->adjusted-string" "symbol?" msg-realm))
  (adjusted-message name name-realm msg msg-realm))

(defnative "error-contract->adjusted-string" (contract realm) ()
  (unless (stringp contract) (arg-error "error-contract->adjusted-string" "string?" contract))
  (unless (racket-symbol-p realm) (arg-error "error-contract->adjusted-string" "symbol?" realm))
  (adjusted-contract contract realm))

;;; ------------------------------------------------------------------
;;; The raisers

(defun raise-exn (type message)
  (do-raise (make-exn type message)))

(defun insert-indentation (s indentation)
  (with-output-to-string (out)
    (loop for c across s
	  do (write-char c out)
	     (when (char= c #\Newline) (write-string indentation out)))))

(defun multi-line-p (s)
  (and (find #\Newline s) (char/= (char s 0) #\Newline)))

(defun reindent (s amount)
  "S, with its lines after the first indented by AMOUNT spaces."
  (if (multi-line-p s)
      (insert-indentation s (make-string amount :initial-element #\Space))
      s))

(defun reindent/newline (s)
  "S, if it's one line; else on lines of its own, indented by 3."
  (if (multi-line-p s)
      (concatenate 'string (string #\Newline) "   " (insert-indentation s "   "))
      s))

(defun nth-string (n)
  (format nil "~D~A" n
	  (if (member (mod n 100) '(11 12 13))
	      "th"
	      (case (mod n 10) (1 "st") (2 "nd") (3 "rd") (t "th")))))

(defun contract-violation (name realm tag expected pos arg args)
  "Raise exn:fail:contract: NAME's contract violation, EXPECTED a
contract.  TAG is \"given\" or \"result\"; ARG the bad value, or, with
POS (its position), the first of the arguments, ARGS the rest."
  (let* ((all (cons arg args))
	 (bad (if pos (nth pos all) arg)))
    (raise-exn "exn:fail:contract"
	       (adjusted-message
		name realm
		(with-output-to-string (s)
		  (format s "contract violation~%  expected: ~A"
			  (reindent (adjusted-contract expected realm) (length "  expected: ")))
		  (format s "~%  ~A: ~A" tag (reindent/newline (error-value->string bad)))
		  (when (and pos args)
		    (format s "~%  argument position: ~A~%  other arguments...:" (nth-string (1+ pos)))
		    (loop for a in all
			  for i from 0
			  unless (= i pos)
			    do (format s "~%   ~A" (reindent (error-value->string a) 3)))))
		realm))))

(defun arg-error (who expected value)
  "WHO's argument-contract violation: VALUE isn't what EXPECTED says."
  (contract-violation (if (stringp who) (rsym who) who) *primitive-realm* "given" expected nil value nil))

(defun result-error (who expected value)
  (contract-violation (if (stringp who) (rsym who) who) *primitive-realm* "result" expected nil value nil))

(defun do-raise-argument-error (e-who tag who realm what pos arg args)
  "raise-argument-error's and raise-result-error's, once their arguments
are sorted out."
  (unless (racket-symbol-p who) (arg-error e-who "symbol?" who))
  (unless (racket-symbol-p realm) (arg-error e-who "symbol?" realm))
  (unless (stringp what) (arg-error e-who "string?" what))
  (when pos
    (unless (nonnegative-integer-p pos) (arg-error e-who "exact-nonnegative-integer?" pos))
    (unless (<= pos (length args))
      (racket-error (who-string e-who) "position ~D is out of range for ~D arguments" pos (1+ (length args)))))
  (contract-violation who realm tag what pos arg args))

(defun raise-argument-error-from (e-who tag who realm what a more)
  "The raisers' common part: A is the argument if MORE is empty; else
the position of the bad one among MORE's arguments."
  (if more
      (do-raise-argument-error e-who tag who realm what a (car more) (cdr more))
      (do-raise-argument-error e-who tag who realm what nil a nil)))

(defnative "raise-argument-error" (who what a &rest more) ()
  (raise-argument-error-from "raise-argument-error" "given" who *default-realm* what a more))
(defnative "raise-argument-error*" (who realm what a &rest more) ()
  (raise-argument-error-from "raise-argument-error*" "given" who realm what a more))
(defnative "raise-result-error" (who what a &rest more) ()
  (raise-argument-error-from "raise-result-error" "result" who *default-realm* what a more))
(defnative "raise-result-error*" (who realm what a &rest more) ()
  (raise-argument-error-from "raise-result-error*" "result" who realm what a more))

(defun raise-arguments-error-from (e-who who realm what more)
  (unless (racket-symbol-p who) (arg-error e-who "symbol?" who))
  (unless (racket-symbol-p realm) (arg-error e-who "symbol?" realm))
  (unless (stringp what) (arg-error e-who "string?" what))
  (raise-exn "exn:fail:contract"
	     (adjusted-message
	      who realm
	      (with-output-to-string (s)
		(write-string what s)
		(loop while more
		      do (let ((field (pop more)))
			   (unless (stringp field) (arg-error e-who "string?" field))
			   (when (null more)
			     (raise-arguments-error-from e-who (rsym e-who) *primitive-realm*
							 "missing value after field string" (list "string" field)))
			   (let ((v (pop more)))
			     (format s "~%  ~A: ~A" field
				     (reindent/newline (if (unquoted-ps-p v) (unquoted-ps-value v) (error-value->string v))))))))
	      realm)))

(defnative "raise-arguments-error" (who what &rest more) ()
  (raise-arguments-error-from "raise-arguments-error" who *default-realm* what more))
(defnative "raise-arguments-error*" (who realm what &rest more) ()
  (raise-arguments-error-from "raise-arguments-error*" who realm what more))

(defun raise-range-error-from (e-who in-who realm type-description index-prefix index in-value
			       lower-bound upper-bound alt-lower-bound)
  (unless (racket-symbol-p in-who) (arg-error e-who "symbol?" in-who))
  (unless (stringp type-description) (arg-error e-who "string?" type-description))
  (unless (stringp index-prefix) (arg-error e-who "string?" index-prefix))
  (unless (integerp index) (arg-error e-who "exact-integer?" index))
  (unless (integerp lower-bound) (arg-error e-who "exact-integer?" lower-bound))
  (unless (integerp upper-bound) (arg-error e-who "exact-integer?" upper-bound))
  (unless (or (not (truthy alt-lower-bound)) (integerp alt-lower-bound))
    (arg-error e-who "(or/c exact-integer? #f)" alt-lower-bound))
  (let ((alt (and (truthy alt-lower-bound) alt-lower-bound)))
    (raise-exn "exn:fail:contract"
	       (adjusted-message
		in-who realm
		(concatenate
		 'string index-prefix "index is "
		 (if (< upper-bound lower-bound)
		     (format nil "out of range for empty ~A~%  index: ~D" type-description index)
		     (format nil "~A  valid range: [~D, ~D]~%  ~A: ~A"
			     (if (and alt (>= index alt) (< index upper-bound))
				 (format nil "smaller than starting index~%  ~Aindex: ~D~%  starting index: ~D~%"
					 index-prefix index lower-bound)
				 (format nil "out of range~%  ~Aindex: ~D~%" index-prefix index))
			     (or alt lower-bound) upper-bound type-description
			     (reindent/newline (error-value->string in-value)))))
		realm))))

(defnative "raise-range-error" (in-who type-description index-prefix index in-value
				       lower-bound upper-bound &optional (alt-lower-bound qs:false)) ()
  (raise-range-error-from "raise-range-error" in-who *default-realm* type-description index-prefix
			  index in-value lower-bound upper-bound alt-lower-bound))
(defnative "raise-range-error*" (in-who realm type-description index-prefix index in-value
					lower-bound upper-bound &optional (alt-lower-bound qs:false)) ()
  (raise-range-error-from "raise-range-error*" in-who realm type-description index-prefix
			  index in-value lower-bound upper-bound alt-lower-bound))

;;; Arity errors

(defun arguments-context-string (args)
  (with-output-to-string (s)
    (when args
      (format s "~%  arguments...:")
      (dolist (a args)
	(format s "~%   ~A" (reindent (error-value->string a) 3))))))

(defun arity-at-least-p (x)
  (truthy (funcall (third *arity-at-least*) x)))

(defun expected-arity-string (arity)
  (cond ((integerp arity) (format nil "  expected: ~D~%" arity))
	((arity-at-least-p arity)
	 (format nil "  expected: at least ~D~%" (funcall (fourth *arity-at-least*) arity 0)))
	(t "")))

(defun raise-arity-error-from (who name realm arity mask? args)
  (unless (or (racket-symbol-p name) (functionp name))
    (arg-error who "(or/c symbol? procedure?)" name))
  (unless (racket-symbol-p realm) (arg-error who "symbol?" realm))
  (if mask?
      (unless (integerp arity) (arg-error who "exact-integer?" arity))
      (unless (truthy (funcall (native-function (gethash "procedure-arity?" *natives*)) arity))
	(arg-error who "procedure-arity?" arity)))
  (let ((arity (if mask? (mask->arity arity) arity)))
    (raise-exn "exn:fail:contract:arity"
	       (adjusted-message
		(if (functionp name) (object-name-of name) name)
		(if (functionp name) *default-realm* realm)
		(format nil "arity mismatch;~% the expected number of arguments does not match the given number~%~A  given: ~D~A"
			(expected-arity-string arity) (length args) (arguments-context-string args))
		*primitive-realm*))))

(defnative "raise-arity-error" (name arity &rest args) ()
  (raise-arity-error-from "raise-arity-error" name *default-realm* arity nil args))
(defnative "raise-arity-error*" (name realm arity &rest args) ()
  (raise-arity-error-from "raise-arity-error*" name realm arity nil args))
(defnative "raise-arity-mask-error" (name mask &rest args) ()
  (raise-arity-error-from "raise-arity-mask-error" name *default-realm* mask t args))
(defnative "raise-arity-mask-error*" (name realm mask &rest args) ()
  (raise-arity-error-from "raise-arity-mask-error*" name realm mask t args))

(defun result-arity-error (who-in num-expected where args)
  "A result arity error with the default realm (WHO-IN a string here is
the adjuster's name for itself)."
  (raise-result-arity-error-from "raise-result-arity-error"
				 (if (stringp who-in) (rsym who-in) who-in)
				 *default-realm* num-expected where args))

(defun raise-result-arity-error-from (e-who who-in realm num-expected where args)
  (unless (or (not (truthy who-in)) (racket-symbol-p who-in)) (arg-error e-who "(or/c symbol? #f)" who-in))
  (unless (nonnegative-integer-p num-expected) (arg-error e-who "exact-nonnegative-integer?" num-expected))
  (unless (or (not (truthy where)) (stringp where)) (arg-error e-who "(or/c string? #f)" where))
  (raise-exn "exn:fail:contract:arity"
	     (adjusted-message
	      who-in realm
	      (format nil "result arity mismatch;~% expected number of values not received~%  expected: ~D~%  received: ~D~A~A"
		      num-expected (length args) (if (truthy where) where "") (arguments-context-string args))
	      *primitive-realm*)))

(defnative "raise-result-arity-error" (who-in num-expected where &rest args) ()
  (raise-result-arity-error-from "raise-result-arity-error" who-in *default-realm* num-expected where args))
(defnative "raise-result-arity-error*" (who-in realm num-expected where &rest args) ()
  (raise-result-arity-error-from "raise-result-arity-error*" who-in realm num-expected where args))

;;; Mismatch, type and user errors

(defnative "raise-mismatch-error" (in-who what v &rest more) ()
  (unless (racket-symbol-p in-who) (arg-error "raise-mismatch-error" "symbol?" in-who))
  (unless (stringp what) (arg-error "raise-mismatch-error" "string?" what))
  (raise-exn "exn:fail:contract"
	     (adjusted-message
	      in-who *default-realm*
	      (with-output-to-string (s)
		(write-string what s)
		(dolist (x (cons v more)) (write-string (error-value->string x) s)))
	      *default-realm*)))

(defnative "raise-type-error" (who what a &rest more) ()
  (let* ((pos (and more a))
	 (arg (if more (car more) a))
	 (args (and more (cdr more)))
	 (all (cons arg args)))
    (unless (racket-symbol-p who) (arg-error "raise-argument-error" "symbol?" who))
    (unless (stringp what) (arg-error "raise-argument-error" "string?" what))
    (when pos
      (unless (nonnegative-integer-p pos) (arg-error "raise-argument-error" "exact-nonnegative-integer?" pos))
      (unless (<= pos (length args))
	(racket-error "raise-type-error" "position ~D is out of range for ~D arguments" pos (length all))))
    (raise-exn "exn:fail:contract"
	       (adjusted-message
		who *default-realm*
		(with-output-to-string (s)
		  (format s "expected argument of type <~A>; given: ~A" what
			  (error-value->string (if pos (nth pos all) arg)))
		  (when (and pos args)
		    (write-string "; other arguments:" s)
		    (loop for x in all
			  for i from 0
			  unless (= i pos) do (format s " ~A" (error-value->string x)))))
		*default-realm*))))

(defnative "raise-user-error" (first &rest rest) ()
  (do-raise (make-exn "exn:fail:user" (error-message-from first rest))))

;;; ------------------------------------------------------------------
;;; Error and exit parameters

(define-param *error-print-context-length* "error-print-context-length"
  (let ((n (let ((s (uiop:getenv "PLT_ERROR_PRINT_CONTEXT_LENGTH")))
	     (and s (parse-integer s :junk-allowed t)))))
    (if (and n (>= n 0)) n 16))
  (guard-check "error-print-context-length" #'nonnegative-integer-p "exact-nonnegative-integer?"))
(define-param *error-print-source-location* "error-print-source-location" qs:true #'boolean-guard)

;;; The handlers (exn.lisp has the display and escape handlers' defaults)
(define-primitive "#%kernel" "error-display-handler" *error-display-handler*)
(define-primitive "#%kernel" "error-escape-handler" *error-escape-handler*)
(let ((display-guard (guard-arity "error-display-handler" 2))
      (escape-guard (guard-arity "error-escape-handler" 0)))
  (setf (parameter-data-guard (gethash *error-display-handler* *parameters*)) display-guard
	(parameter-data-name (gethash *error-display-handler* *parameters*)) (rsym "error-display-handler")
	(parameter-data-guard (gethash *error-escape-handler* *parameters*)) escape-guard
	(parameter-data-name (gethash *error-escape-handler* *parameters*)) (rsym "error-escape-handler")))

(defun default-error-syntax->srcloc-handler (v) (declare (ignore v)) qs:false)
(defun default-error-syntax->name-handler (v) (declare (ignore v)) qs:false)
(defun default-error-syntax->string-handler (v len)
  (truncated-string (with-output-to-string (s) (print-value v s :write)) len))
(defun default-error-module-path->string-handler (v len)
  (truncated-string (with-output-to-string (s) (print-value v s :write)) len))

(define-param *error-syntax->srcloc-handler* "error-syntax->srcloc-handler"
  #'default-error-syntax->srcloc-handler (guard-arity "error-syntax->srcloc-handler" 1))
(define-param *error-syntax->string-handler* "error-syntax->string-handler"
  #'default-error-syntax->string-handler (guard-arity "error-syntax->string-handler" 2))
(define-param *error-syntax->name-handler* "error-syntax->name-handler"
  #'default-error-syntax->name-handler (guard-arity "error-syntax->name-handler" 1))
(define-param *error-module-path->string-handler* "error-module-path->string-handler"
  #'default-error-module-path->string-handler (guard-arity "error-module-path->string-handler" 2))

;;; exit: through the exit handler, which by default quits
(defun default-exit-handler (v)
  "Racket's: quit, with status V if it's an integer from 1 to 255, else 0."
  (finish-output)
  (uiop:quit (if (and (integerp v) (<= 1 v 255)) v 0)))

(define-param *exit-handler* "exit-handler" #'default-exit-handler (guard-arity "exit-handler" 1))

(defnative "exit" (&optional (v qs:true)) ()
  (funcall (funcall *exit-handler*) v)
  *void*)

(defun default-executable-yield-handler (v) (declare (ignore v)) *void*)
(define-param *executable-yield-handler* "executable-yield-handler"
  #'default-executable-yield-handler (guard-arity "executable-yield-handler" 1))

;;; ------------------------------------------------------------------
;;; srcloc->string, and exn:srclocs

(defun srcloc-p (x)
  (and (struct-instance-p x) (subtype-p (struct-of-type x) *srcloc-type*)))

(defun srcloc-string (s)
  "srcloc->string's: S's source, line and column or position; #f if it has
no source."
  (let* ((fields (struct-fields-copy s))
	 (source (svref fields 0)) (line (svref fields 1)) (column (svref fields 2))
	 (position (svref fields 3)))
    (if (not (truthy source))
	qs:false
	(let ((where (cond ((stringp source) source)
			   ((rpath-p source) (rpath-string source))
			   ((racket-symbol-p source) (rname source))
			   (t (with-output-to-string (o) (print-value source o :display))))))
	  (cond ((and (truthy line) (truthy column)) (format nil "~A:~D:~D" where line column))
		((truthy position) (format nil "~A::~D" where position))
		(t where))))))

(defnative "srcloc->string" (s) ()
  (unless (srcloc-p s) (arg-error "srcloc->string" "srcloc?" s))
  (srcloc-string s))

(defun exn-srclocs-procedure (x)
  "X's prop:exn:srclocs procedure; the srclocs field of an exn:fail:read."
  (and (struct-instance-p x)
       (multiple-value-bind (v found) (property-value x (primitive "prop:exn:srclocs"))
	 (cond ((and found (functionp v)) v)
	       ((exn-of-type-p x "exn:fail:read") (primitive "exn:fail:read-srclocs"))))))

(defun exn-srclocs (x)
  (let ((f (exn-srclocs-procedure x)))
    (and f (funcall f x))))

(defun error-display-locations (v)
  "What the error display handler adds after the message of exn V: its
source locations, if it has any and error-print-context-length isn't 0."
  (let ((locs (and (plusp (funcall *error-print-context-length*)) (exn-srclocs v))))
    (if (null locs)
	""
	(let* ((locs (if (and (or (exn-of-type-p v "exn:fail:read") (exn-of-type-p v "exn:fail:contract:variable"))
			      (truthy (funcall *error-print-source-location*)))
			 (cdr locs)
			 locs))
	       (strings (loop for l in locs
			      for s = (and (srcloc-p l) (srcloc-string l))
			      when (and s (truthy s)) collect s)))
	  (with-output-to-string (o)
	    (when strings
	      (format o "~%  location...:")
	      (dolist (s strings) (format o "~%   ~A" s))))))))

(defnative "exn:srclocs?" (x) (:leaf t) (bool (exn-srclocs-procedure x)))
(defnative "exn:srclocs-accessor" (x) (:leaf t)
  (or (exn-srclocs-procedure x) (arg-error "exn:srclocs-accessor" "exn:srclocs?" x)))

;;; ------------------------------------------------------------------
;;; Properties, procedures and values that say what they are

(defnative "true-object?" (x) (:leaf t) (bool (eq x qs:true)))

(defnative "unquoted-printing-string" (s) (:leaf t)
  (unless (stringp s) (arg-error "unquoted-printing-string" "string?" s))
  (make-unquoted-ps s))
(defnative "unquoted-printing-string?" (x) (:leaf t) (bool (unquoted-ps-p x)))
(defnative "unquoted-printing-string-value" (x) (:leaf t)
  (unless (unquoted-ps-p x) (arg-error "unquoted-printing-string-value" "unquoted-printing-string?" x))
  (unquoted-ps-value x))

(defun property-of (x property-name)
  "X's value for the struct-type property PROPERTY-NAME, and whether it has one."
  (if (struct-instance-p x)
      (property-value x (primitive property-name))
      (values nil nil)))

(defnative "custom-write?" (x) (:leaf t) (bool (nth-value 1 (property-of x "prop:custom-write"))))
(defnative "custom-write-accessor" (x) (:leaf t)
  (multiple-value-bind (v found) (property-of x "prop:custom-write")
    (if found v (arg-error "custom-write-accessor" "custom-write?" x))))
(defnative "custom-print-quotable?" (x) (:leaf t)
  (bool (nth-value 1 (property-of x "prop:custom-print-quotable"))))
(defnative "custom-print-quotable-accessor" (x) (:leaf t)
  (multiple-value-bind (v found) (property-of x "prop:custom-print-quotable")
    (if found v (arg-error "custom-print-quotable-accessor" "custom-print-quotable?" x))))

(defnative "procedure-struct-type?" (type) (:leaf t)
  (unless (struct-type-p type) (arg-error "procedure-struct-type?" "struct-type?" type))
  (bool (applicable-procedure type)))

;;; The predicate and accessor procedures that make-struct-type-property
;;; and make-impersonator-property return, kept to tell them apart
(defvar *property-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A property's predicate or accessor procedure -> (kind . property), kind :predicate or :accessor.")

(defmacro record-property-procedures (name)
  "Redefine the native NAME (it returns a property, its predicate and its
accessor) to remember the predicate and the accessor."
  `(let* ((old (gethash ,name *natives*))
	  (old-function (native-function old)))
     (defnative ,name (&rest args) (:instance (native-instance old) :rumble (native-rumble old))
       (multiple-value-bind (property predicate accessor) (apply old-function args)
	 (setf (gethash predicate *property-procedures*) (cons :predicate property)
	       (gethash accessor *property-procedures*) (cons :accessor property))
	 (values property predicate accessor)))
     (setf (gethash (native-function (gethash ,name *natives*)) *arity-masks*) (arity-mask old-function))))

(record-property-procedures "make-struct-type-property")
(record-property-procedures "make-impersonator-property")

(defun property-procedure-p (f kind &optional property)
  (let ((e (and (functionp f) (gethash f *property-procedures*))))
    (and e (eq (car e) kind)
	 (or (null property) (eq (cdr e) property))
	 (if (struct-type-property-p (cdr e)) :struct :impersonator))))

(defnative "struct-type-property-accessor-procedure?" (f) (:leaf t)
  (bool (eq (property-procedure-p f :accessor) :struct)))
(defnative "struct-type-property-predicate-procedure?" (f &optional (property qs:false)) (:leaf t)
  (bool (eq (property-procedure-p f :predicate (and (truthy property) property)) :struct)))
(defnative "impersonator-property-accessor-procedure?" (f) (:leaf t)
  (bool (eq (property-procedure-p f :accessor) :impersonator)))
(defnative "impersonator-property-predicate-procedure?" (f) (:leaf t)
  (bool (eq (property-procedure-p f :predicate) :impersonator)))

;;; An impersonator property that a procedure chaperone's wrapper may be
;;; given with (cons mark-key value): not acted on here
(define-primitive "#%kernel" "impersonator-prop:application-mark"
  (make-impersonator-property* (rsym "application-mark")))

;;; procedure-rename: a procedure of PROC's arity that has NAME for its
;;; name.  Its Lisp name is a symbol of that name in a package of its own
;;; (object-name makes it lower case); *RENAMED-PROCEDURES* has the exact
;;; name.
(defvar *renamed-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A procedure procedure-rename made -> its name, a symbol.")
(defvar *rename-makers* (make-hash-table :test 'equal :synchronized t))
(defvar *names-package* (or (find-package "QUASISCHEME-RACKET-NAMES")
			    (make-package "QUASISCHEME-RACKET-NAMES" :use '())))

(defun rename-maker (name)
  "A function of a procedure, its arity mask and name for messages, that
makes the procedure NAME names."
  (or (gethash name *rename-makers*)
      (setf (gethash name *rename-makers*)
	    (let ((symbol (intern name *names-package*)))
	      (compile nil `(lambda (proc mask who expected)
			      (sb-int:named-lambda ,symbol (&rest args)
				(if (logbitp (length args) mask)
				    (apply proc args)
				    (racket-arity-error who (length args) expected)))))))))

(defnative "procedure-rename" (proc name &optional realm) (:rumble "procedure-rename")
  (declare (ignore realm))
  (unless (functionp proc) (arg-error "procedure-rename" "procedure?" proc))
  (unless (racket-symbol-p name) (arg-error "procedure-rename" "symbol?" name))
  (let* ((mask (or (ignore-errors (arity-mask proc)) -1))
	 (arity (mask->arity mask))
	 (new (funcall (rename-maker (rname name)) proc mask (rname name) (and (integerp arity) arity))))
    (setf (gethash new *arity-masks*) mask
	  (gethash new *renamed-procedures*) name)
    new))

;;; ------------------------------------------------------------------
;;; Continuations: marks, composable continuations

(defvar *composable-continuations* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The procedures call-with-composable-continuation makes.")

(defnative "continuation-marks" (k &optional prompt-tag) ()
  (declare (ignore prompt-tag))
  (cond ((not (truthy k)) (qs-r6rs::make-mark-set '()))
	((rthread-p k) (qs-r6rs::make-mark-set (if (eq k *main-thread*) (current-marks) '())))
	((and (functionp k) (or (qsx::continuation-procedure-p k) (gethash k *composable-continuations*)))
	 (multiple-value-bind (marks found) (gethash k qs-r6rs::*continuation-marks*)
	   (qs-r6rs::make-mark-set (if found marks (current-marks)))))
	(t (arg-error "continuation-marks" "(or/c continuation? thread? #f)" k))))

(defnative "continuation-mark-set->context" (marks) (:leaf t)
  (unless (qs-r6rs::mark-set-p marks)
    (arg-error "continuation-mark-set->context" "continuation-mark-set?" marks))
  ;; no record of the calls: the context is empty
  '())

(defnative "continuation-mark-set->iterator" (marks keys &optional (none qs:false) prompt-tag) (:leaf t)
  (declare (ignore prompt-tag))
  (let ((keys (coerce keys 'list)))
    (labels ((iterator (frames)
	       (lambda ()
		 (loop for rest on frames
		       for entries = (cdr (car rest))
		       when (some (lambda (k) (assoc k entries :test #'eq)) keys)
			 do (return (values (coerce (mapcar (lambda (k)
							      (let ((e (assoc k entries :test #'eq)))
								(if e (cdr e) none)))
							    keys)
						    'simple-vector)
					    (iterator (cdr rest))))
		       finally (return (values qs:false (iterator '())))))))
      (iterator (qs-r6rs::mark-frames (marks-of marks))))))

(defun capture-composable-continuation (tag)
  "The continuation from the call of this native up to the nearest prompt
tagged TAG, as a procedure that calls it on top of its own caller (as
the aborts abort-current-continuation makes do, without the throw)."
  (let ((prompt (qsx::find-prompt tag)))
    (unless prompt
      (raise-exn "exn:fail:contract:continuation"
		 (format nil "call-with-composable-continuation: no corresponding prompt in the continuation~%  tag: ~A"
			 (with-output-to-string (s) (print-value tag s :display)))))
    (let* ((frames (loop with inside = '()
			 for frame in qsx::*fstack*
			 until (eq frame prompt)
			 collect (prog1 (qsx::fresh-frame frame inside) (push frame inside))))
	   (winders (ldiff qsx::*winders* (svref prompt 4)))
	   ;; its marks (a parameterization among them: a generator's), which
	   ;; its frames, rebuilt, have on top of the caller's, as Racket's
	   (outside (assoc tag *prompt-marks*))
	   (marks (if outside (ldiff (current-marks) (cdr outside)) '()))
	   (k (lambda (&rest values)
		(unless qsx::*full-continuations*
		  (racket-error "continuation" "a composable continuation can't be called without full continuations"))
		(let ((qs-r6rs::*marks* (append marks (current-marks))))
		  (qsx::compose-continuation frames winders values)))))
      (setf (gethash k *composable-continuations*) t)
      k)))

(defnative "call-with-composable-continuation" (proc &optional (tag *default-prompt-tag*)) ()
  (unless (accepts-arity-p proc 1)
    (arg-error "call-with-composable-continuation" "(procedure-arity-includes/c 1)" proc))
  (unless (prompt-tag-p tag)
    (arg-error "call-with-composable-continuation" "continuation-prompt-tag?" tag))
  (funcall proc (capture-composable-continuation tag)))

(defnative "continuation?" (x) (:leaf t)
  (bool (and (functionp x) (or (qsx::continuation-procedure-p x) (gethash x *composable-continuations*)))))

(defnative "call-in-continuation" (k proc) ()
  (unless (and (functionp k) (or (qsx::continuation-procedure-p k) (gethash k *composable-continuations*)))
    (arg-error "call-in-continuation" "continuation?" k))
  (unless (accepts-arity-p proc 0)
    (arg-error "call-in-continuation" "(procedure-arity-includes/c 0)" proc))
  ;; PROC's results go to K; Racket calls PROC in K's context, whose
  ;; marks and parameterization this doesn't have
  (multiple-value-call k (funcall proc)))

;;; ------------------------------------------------------------------
;;; Loggers and log receivers (racket/src/io/logger/)
;;;
;;; A logger has a topic, a parent and the filters its messages pass to
;;; the parent; a receiver has the filters it wants: an improper list
;;; ((topic . level) ... . default-level).  A message goes to each
;;; receiver of the logger that wants its level for its topic, then
;;; the same for the parent's, while the logger's filters let it through.

(defstruct (rlogger (:constructor make-rlogger (topic parent filters root)))
  topic parent filters
  (receivers '())			; newest first
  root)					; a cons shared by a tree of loggers: its car is the
					; log-level-evt's semaphore, if one has been made

(defmethod print-object ((x rlogger) stream) (write-string "#<logger>" stream))

;;; a receiver is an evt: sync takes its first message (control.lisp's
;;; EVT-READY calls LOG-RECEIVER-POLL for kind :log-receiver)
(defstruct (log-receiver (:include revt (kind :log-receiver)) (:constructor make-log-receiver* (filters)))
  filters
  (queue '()))

(defmethod print-object ((x log-receiver) stream) (write-string "#<log-receiver>" stream))

(defun log-receiver-poll (receiver)
  "RECEIVER's first message, and T; or NIL and NIL if it has none."
  (if (log-receiver-queue receiver)
      (values (pop (log-receiver-queue receiver)) t)
      (values nil nil)))

(defparameter *log-levels* '("none" "fatal" "error" "warning" "info" "debug"))

(defun log-level-value (level)
  (and (racket-symbol-p level) (position (rname level) *log-levels* :test #'string=)))

(defun check-log-level (who level)
  (unless (log-level-value level)
    (arg-error who "(or/c 'none 'fatal 'error 'warning 'info 'debug)" level)))

(defun log-level>= (a b) (>= (log-level-value a) (log-level-value b)))
(defun log-level-max (a b) (if (< (log-level-value a) (log-level-value b)) b a))
(defun log-level-min (a b) (if (< (log-level-value a) (log-level-value b)) a b))
(defun user-log-level (level) (if (string= (rname level) "none") qs:false level))

(defun parse-log-filters (who args default)
  "ARGS (level topic level topic ... [level]) as filters ending in DEFAULT."
  (let ((accum '()))
    (loop
      (when (null args) (return (append accum default)))
      (let ((level (pop args)))
	(check-log-level who level)
	(when (null args) (return (append accum level)))
	(let ((topic (pop args)))
	  (unless (or (not (truthy topic)) (racket-symbol-p topic))
	    (arg-error who "(or/c #f symbol?)" topic))
	  (if (truthy topic)
	      (push (cons topic level) accum)
	      (setq default level)))))))

(defun filters-level-for-topic (filters topic)
  (loop for f = filters then (cdr f)
	while (consp f)
	when (eq (caar f) topic) return (cdar f)
	finally (return f)))

(defun filters-max-level (filters)
  (let ((best (rsym "none")))
    (loop for f = filters then (cdr f)
	  while (consp f)
	  do (setq best (log-level-max best (cdar f)))
	  finally (return (log-level-max best f)))))

(defun logger-wanted-level (logger topic)
  "The most verbose level that a receiver of LOGGER or of its ancestors,
through their filters, wants of messages about TOPIC (any, if #f)."
  (let ((topic (and (truthy topic) topic))
	(ceil (rsym "debug")) (top (rsym "none"))
	(topic-ceil (rsym "debug")) (topic-top (rsym "none"))
	(logger logger))
    (loop
      (dolist (r (rlogger-receivers logger))
	(setq top (log-level-max top (log-level-min (filters-max-level (log-receiver-filters r)) ceil)))
	(when topic
	  (setq topic-top (log-level-max topic-top
					 (log-level-min (filters-level-for-topic (log-receiver-filters r) topic)
							topic-ceil)))))
      (let ((parent (rlogger-parent logger)))
	(if (and parent (or (log-level>= ceil top) (and topic (log-level>= ceil topic-top))))
	    (let ((filters (rlogger-filters logger)))
	      (setq ceil (log-level-min ceil (filters-max-level filters))
		    topic-ceil (if topic
				   (log-level-min topic-ceil (filters-level-for-topic filters topic))
				   topic-ceil)
		    logger parent))
	    (return (if topic topic-top top)))))))

(defun check-logger (who x)
  (unless (rlogger-p x) (arg-error who "logger?" x)))

(defun check-log-topic (who topic)
  (unless (or (not (truthy topic)) (racket-symbol-p topic))
    (arg-error who "(or/c symbol? #f)" topic)))

(defnative "logger?" (x) (:leaf t) (bool (rlogger-p x)))
(defnative "logger-name" (logger) (:leaf t)
  (check-logger "logger-name" logger)
  (rlogger-topic logger))

(defnative "make-logger" (&optional (topic qs:false) (parent qs:false) &rest filters) (:leaf t)
  (unless (or (not (truthy topic)) (racket-symbol-p topic)) (arg-error "make-logger" "(or/c symbol? #f)" topic))
  (unless (or (not (truthy parent)) (rlogger-p parent)) (arg-error "make-logger" "(or/c logger? #f)" parent))
  (let ((parent (and (truthy parent) parent)))
    (make-rlogger topic parent (parse-log-filters "make-logger" filters (rsym "debug"))
		  (if parent (rlogger-root parent) (list nil)))))

;;; the root logger, for current-logger: it has no receivers, and a
;;; logger is what its guard asks for
(let ((data (gethash *current-logger* *parameters*)))
  (setf (car (parameter-data-cell data)) (make-rlogger qs:false nil (rsym "none") (list nil))
	(parameter-data-guard data) (guard-check "current-logger" #'rlogger-p "logger?")))

(defun add-log-receiver (logger receiver)
  (push receiver (rlogger-receivers logger))
  ;; wanted levels may have changed: wake the log-level-evts
  (let ((root (rlogger-root logger)))
    (when (car root)
      (incf (rsemaphore-count (car root)))
      (setf (car root) nil))))

(defnative "make-log-receiver" (logger level &rest args) ()
  (check-logger "make-log-receiver" logger)
  (let ((receiver (make-log-receiver* (parse-log-filters "make-log-receiver" (cons level args) (rsym "none")))))
    (add-log-receiver logger receiver)
    receiver))
(defnative "log-receiver?" (x) (:leaf t) (bool (log-receiver-p x)))

(defnative "log-level?" (logger level &optional (topic qs:false)) (:leaf t)
  (check-logger "log-level?" logger)
  (check-log-level "log-level?" level)
  (check-log-topic "log-level?" topic)
  (bool (and (string/= (rname level) "none")
	     (log-level>= (logger-wanted-level logger topic) level))))

(defnative "log-max-level" (logger &optional (topic qs:false)) (:leaf t)
  (check-logger "log-max-level" logger)
  (check-log-topic "log-max-level" topic)
  (user-log-level (logger-wanted-level logger topic)))

(defnative "log-all-levels" (logger) (:leaf t)
  (check-logger "log-all-levels" logger)
  ;; the topics receivers have filters for, and the default level
  (let ((topics '()) (default (rsym "none")) (max-default (rsym "debug")) (current logger))
    (loop
      (let ((new-default (rsym "none")))
	(dolist (r (rlogger-receivers current))
	  (loop for f = (log-receiver-filters r) then (cdr f)
		while (consp f)
		do (pushnew (caar f) topics :test #'eq)
		finally (setq new-default (log-level-max new-default f))))
	(setq default (log-level-max (log-level-min new-default max-default) default))
	(let ((parent (rlogger-parent current)))
	  (unless parent (return))
	  (setq max-default (log-level-min max-default
					   (filters-level-for-topic (rlogger-filters current) qs:false))
		current parent))))
    (list* (user-log-level default) qs:false
	   (loop for topic in topics
		 append (list (user-log-level (logger-wanted-level logger topic)) topic)))))

(defnative "log-level-evt" (logger) (:leaf t)
  (check-logger "log-level-evt" logger)
  (let ((root (rlogger-root logger)))
    (unless (car root) (setf (car root) (make-rsemaphore 0)))
    (make-revt :peek (car root))))

(defun send-log-message (logger level topic message data prefix?)
  "Deliver a message to the receivers that want it."
  (let ((msg nil))
    (when (log-level>= (logger-wanted-level logger qs:false) level)
      (loop while logger
	    do (dolist (r (rlogger-receivers logger))
		 (when (log-level>= (filters-level-for-topic (log-receiver-filters r) topic) level)
		   (unless msg
		     (setq msg (immutable! (vector level
						   (immutable! (if (and (truthy prefix?) (truthy topic))
								   (concatenate 'string (rname topic) ": " message)
								   (copy-seq message)))
						   data topic))))
		   (setf (log-receiver-queue r) (nconc (log-receiver-queue r) (list msg)))))
	       (let ((parent (rlogger-parent logger)))
		 (setq logger (and parent
				   (log-level>= (filters-level-for-topic (rlogger-filters logger) topic) level)
				   parent)))))))

(defun do-log-message (logger level topic message data prefix?)
  (check-logger "log-message" logger)
  (check-log-level "log-message" level)
  (unless (or (not (truthy topic)) (racket-symbol-p topic))
    (arg-error "log-message" "(or/c #f symbol?)" topic))
  (unless (stringp message) (arg-error "log-message" "string?" message))
  (unless (string= (rname level) "none")
    (send-log-message logger level topic message data prefix?))
  *void*)

(defnative "log-message" (logger level &rest more) ()
  ;; (logger level message), (logger level topic/message message/data),
  ;; (... data/prefix?), or all six arguments; a string third argument is
  ;; the message, with the logger's own name as the topic
  (let ((n (+ 2 (length more)))
	(logger-topic (if (rlogger-p logger) (rlogger-topic logger) qs:false)))
    (flet ((bad-topic (x)
	     (check-logger "log-message" logger)
	     (check-log-level "log-message" level)
	     (arg-error "log-message" "(or/c string? symbol?)" x)))
      (case n
	(3 (do-log-message logger level logger-topic (first more) qs:false qs:true))
	(4 (let ((tm (first more)) (md (second more)))
	     (cond ((stringp tm) (do-log-message logger level logger-topic tm md qs:true))
		   ((or (not (truthy tm)) (racket-symbol-p tm)) (do-log-message logger level tm md qs:false qs:true))
		   (t (bad-topic tm)))))
	(5 (let ((tm (first more)) (md (second more)) (dp (third more)))
	     (cond ((stringp tm) (do-log-message logger level logger-topic tm md dp))
		   ((or (not (truthy tm)) (racket-symbol-p tm)) (do-log-message logger level tm md dp qs:true))
		   (t (bad-topic tm)))))
	(6 (apply #'do-log-message logger level more))
	(t (racket-arity-error "log-message" n))))))

;;; ------------------------------------------------------------------
;;; equal-always? and the hash codes
;;;
;;; equal-always? is equal? but for mutable values (strings, byte
;;; strings, vectors, boxes, mutable pairs, mutable hash tables, structs
;;; with mutable fields), which it compares by identity.  EQUAL-CORE is
;;; both, with a procedure for the parts: equal?/recur and the
;;; prop:equal+hash procedures get Racket's own.

(defun struct-fully-immutable-p (type)
  (loop for ty = type then (struct-type-parent ty)
	while ty
	always (>= (length (remove-duplicates (struct-type-immutables ty))) (struct-type-field-count ty))))

(defun equal-core (a b recur always)
  "Whether A and B are equal? (equal-always? if ALWAYS), their parts by RECUR."
  (flet ((both-immutable-p () (or (not always) (and (immutable-p a) (immutable-p b)))))
    (cond ((eq a b) t)
	  ((consp a) (and (consp b) (funcall recur (car a) (car b)) (funcall recur (cdr a) (cdr b))))
	  ((numberp a) (racket-eqv a b))
	  ((characterp a) (eql a b))
	  ((stringp a) (and (stringp b) (both-immutable-p) (string= a b)))
	  ((typep a '(simple-array (unsigned-byte 8) (*)))
	   (and (typep b '(simple-array (unsigned-byte 8) (*))) (both-immutable-p) (equalp a b)))
	  ((simple-vector-p a)
	   (and (simple-vector-p b) (both-immutable-p) (= (length a) (length b))
		(every (lambda (x y) (funcall recur x y)) a b)))
	  ((rbox-p a) (and (rbox-p b) (both-immutable-p) (funcall recur (rbox-value a) (rbox-value b))))
	  ((mpair-p a)
	   (and (not always) (mpair-p b)
		(funcall recur (mpair-car a) (mpair-car b)) (funcall recur (mpair-cdr a) (mpair-cdr b))))
	  ((rpath-p a) (and (rpath-p b) (string= (rpath-string a) (rpath-string b))))
	  ;; stencil vectors, fxvectors and flvectors: mutable, so by identity for equal-always?
	  ((typep a 'stencil)
	   (and (not always) (typep b 'stencil) (= (stencil-mask a) (stencil-mask b))
		(every (lambda (x y) (funcall recur x y)) (stencil-items a) (stencil-items b))))
	  ((or (typep a 'fxvector) (typep a '(simple-array double-float (*))))
	   (and (not always) (equal* a b)))
	  ((or (ihash-p a) (rhash-p a))
	   (and (or (not always) (ihash-p a))
		(or (ihash-p b) (rhash-p b))
		(eq (hash-kind a) (hash-kind b))
		(eq (ihash-p a) (ihash-p b))
		(= (length (entries a)) (length (entries b)))
		(loop for (k . v) across (entries a)
		      always (multiple-value-bind (w found) (lookup* b k)
			       (and found (funcall recur v w))))))
	  ((struct-instance-p a)
	   (and (struct-instance-p b)
		(eq (struct-of-type a) (struct-of-type b))
		(let ((eqp (equal+hash-procedures a))
		      (type (struct-of-type a)))
		  (cond (eqp (truthy (funcall eqp a b (lambda (x y) (bool (funcall recur x y))))))
			((and (transparent-p type) (or (not always) (struct-fully-immutable-p type)))
			 (let ((n (struct-field-count a))) (and (= n (struct-field-count b)) (loop for i below n always (funcall recur (struct-field a i) (struct-field b i))))))
			(t nil)))))
	  (t nil))))

(defun equal-always* (a b)
  (labels ((recur (x y) (equal-core x y #'recur t)))
    (recur a b)))

(defnative "equal-always?" (a b) () (bool (equal-always* a b)))
(defnative "equal?/recur" (a b recur) ()
  (bool (equal-core a b (lambda (x y) (truthy (funcall recur x y))) nil)))
(defnative "equal-always?/recur" (a b recur) ()
  (bool (equal-core a b (lambda (x y) (truthy (funcall recur x y))) t)))

(defun equal-hash-core (x recur always)
  "X's equal-hash-code (equal-always-hash-code if ALWAYS), with the
hashes of its parts by RECUR."
  (flet ((mutable-p () (and always (not (immutable-p x)))))
    (cond ((consp x) (mix (funcall recur (car x)) (funcall recur (cdr x))))
	  ((or (stringp x) (typep x '(simple-array (unsigned-byte 8) (*))))
	   (if (mutable-p) (eq-hash x) (equal-hash* x)))
	  ((simple-vector-p x)
	   (if (mutable-p)
	       (eq-hash x)
	       (let ((h (length x)))
		 (loop for e across x do (setq h (mix h (funcall recur e))))
		 h)))
	  ((rbox-p x) (if (mutable-p) (eq-hash x) (mix 7 (funcall recur (rbox-value x)))))
	  ((typep x 'stencil)
	   (if always
	       (eq-hash x)
	       (let ((h (mix 17 (stencil-mask x))))
		 (loop for e across (stencil-items x) do (setq h (mix h (funcall recur e))))
		 h)))
	  ((or (typep x 'fxvector) (typep x '(simple-array double-float (*))))
	   (if always (eq-hash x) (equal-hash* x)))
	  ((mpair-p x)
	   (if always
	       (eq-hash x)
	       (mix (funcall recur (mpair-car x)) (funcall recur (mpair-cdr x)))))
	  ((or (ihash-p x) (rhash-p x))
	   (if (and always (rhash-p x)) (eq-hash x) (mix 13 (length (entries x)))))
	  ((struct-instance-p x)
	   (multiple-value-bind (eqp hashp) (equal+hash-procedures x)
	     (declare (ignore eqp))
	     (let ((type (struct-of-type x)))
	       (cond (hashp (logand (funcall hashp x recur) most-positive-fixnum))
		     ((and (transparent-p type) (or (not always) (struct-fully-immutable-p type)))
		      (let ((h (eq-hash type)))
			(loop for i below (struct-field-count x) do (setq h (mix h (funcall recur (struct-field x i)))))
			h))
		     (t (eq-hash x))))))
	  (t (equal-hash* x)))))

(defun full-equal-hash (x always &optional (depth 0))
  (if (> depth 16)
      0
      (equal-hash-core x (lambda (y) (full-equal-hash y always (1+ depth))) always)))

(defun secondary-hash (h)
  (logand (mix 1000003 h) most-positive-fixnum))

(defnative "equal-always-hash-code" (x) () (full-equal-hash x t))
(defnative "equal-secondary-hash-code" (x) () (secondary-hash (full-equal-hash x nil)))
(defnative "equal-always-secondary-hash-code" (x) () (secondary-hash (full-equal-hash x t)))
(defnative "equal-hash-code/recur" (x recur) () (equal-hash-core x recur nil))
(defnative "equal-always-hash-code/recur" (x recur) () (equal-hash-core x recur t))

;;; ------------------------------------------------------------------
;;; Parameters that say how Racket runs

(define-param *compile-allow-set!-undefined* "compile-allow-set!-undefined" qs:false #'boolean-guard)
(define-param *compile-context-preservation-enabled* "compile-context-preservation-enabled"
  qs:false #'boolean-guard)
(define-param *compile-enforce-module-constants* "compile-enforce-module-constants" qs:true #'boolean-guard)
(define-param *load-on-demand-enabled* "load-on-demand-enabled" qs:true #'boolean-guard)
(define-param *current-force-delete-permissions* "current-force-delete-permissions" qs:true #'boolean-guard)

(defun target-machine-symbol () (rsym *vm-name*))

(defnative "compile-target-machine?" (machine) (:leaf t)
  (unless (racket-symbol-p machine) (arg-error "compile-target-machine?" "symbol?" machine))
  (bool (eq machine (target-machine-symbol))))

(make-lazy-parameter "current-compile-target-machine" #'target-machine-symbol
		     (lambda (v)
		       (unless (or (not (truthy v)) (and (racket-symbol-p v) (eq v (target-machine-symbol))))
			 (arg-error "current-compile-target-machine"
				    "(or/c #f (and/c symbol? compile-target-machine?))" v))
		       v))
(define-param *current-compile-realm* "current-compile-realm" (rsym "racket")
  (guard-check "current-compile-realm" #'racket-symbol-p "symbol?"))

(define-param *read-accept-bar-quote* "read-accept-bar-quote" qs:true #'boolean-guard)
(define-param *read-case-sensitive* "read-case-sensitive" qs:true #'boolean-guard)
(define-param *read-on-demand-source* "read-on-demand-source" qs:false
  (guard-check "read-on-demand-source"
	       (lambda (v) (or (not (truthy v)) (eq v qs:true) (rpath-p v) (stringp v)))
	       "(or/c #f #t (and/c path-string? complete-path?))"))

(define-param *current-command-line-arguments* "current-command-line-arguments" (immutable! (vector))
  (lambda (v)
    (unless (and (simple-vector-p v) (every #'stringp v))
      (arg-error "current-command-line-arguments" "(vectorof string?)" v))
    (immutable! (map 'simple-vector #'immutable-copy v))))

(define-param *current-locale* "current-locale" (immutable! (copy-seq ""))
  (lambda (v)
    (unless (or (not (truthy v)) (stringp v)) (arg-error "current-locale" "(or/c #f string?)" v))
    (if (truthy v) (immutable-copy v) v)))

(define-param *current-thread-initial-stack-size* "current-thread-initial-stack-size" 64
  (guard-check "current-thread-initial-stack-size" (lambda (v) (and (integerp v) (plusp v)))
	       "exact-positive-integer?"))

(define-param *current-subprocess-custodian-mode* "current-subprocess-custodian-mode" qs:false
  (guard-check "current-subprocess-custodian-mode"
	       (lambda (v) (or (not (truthy v))
			       (and (racket-symbol-p v) (member (rname v) '("kill" "interrupt") :test #'string=))))
	       "(or/c #f 'kill 'interrupt)"))
(define-param *current-subprocess-keep-file-descriptors* "current-subprocess-keep-file-descriptors"
  (rsym "inherited")
  (guard-check "current-subprocess-keep-file-descriptors"
	       (lambda (v) (or (null v)
			       (and (racket-symbol-p v) (member (rname v) '("all" "inherited") :test #'string=))))
	       "(or/c '() 'uninherited 'all)"))

(defnative "current-drive" () (:leaf t) (make-rpath "/"))

;;; The custodian, thread group and security guard are the thread
;;; layer's and the security layer's: what they make, once, else a
;;; stand-in of the right name
(make-lazy-parameter "current-custodian" (lambda () (native-or-stand-in "make-custodian" "custodian")))
(make-lazy-parameter "current-thread-group"
		     (lambda () (native-or-stand-in "make-thread-group" "thread-group")))
(make-lazy-parameter "current-security-guard"
		     (lambda () (native-or-stand-in "make-security-guard" "security-guard")))
(define-param *current-evt-pseudo-random-generator* "current-evt-pseudo-random-generator"
  (funcall (native-function (gethash "make-pseudo-random-generator" *natives*)))
  (guard-check "current-evt-pseudo-random-generator" #'prng-p "pseudo-random-generator?"))

;;; The interaction hooks: what the REPL asks for a datum
(defun default-get-interaction-input-port () (current-port :input))
(defun default-get-interaction-evt () *never-evt*)

(define-param *current-get-interaction-input-port* "current-get-interaction-input-port"
  #'default-get-interaction-input-port (guard-arity "current-get-interaction-input-port" 0))
(define-param *current-get-interaction-evt* "current-get-interaction-evt"
  #'default-get-interaction-evt (guard-arity "current-get-interaction-evt" 0))

(defun default-read-interaction (source in)
  "Racket's: read-syntax from IN (the expander's, when it has set its own)."
  (multiple-value-bind (read-syntax found) (primitive "read-syntax")
    (if found
	(funcall read-syntax source in)
	(racket-error "default-read-interaction" "read-syntax isn't available"))))

(define-param *current-read-interaction* "current-read-interaction"
  #'default-read-interaction (guard-arity "current-read-interaction" 2))

(defun default-prompt-read ()
  "Racket's: the prompt, then a datum from the interaction input port."
  (port-write-string "> " (current-port :output))
  (let ((in (funcall (funcall *current-get-interaction-input-port*))))
    (funcall (funcall *current-read-interaction*) (object-name-of in) in)))

(define-param *current-prompt-read* "current-prompt-read"
  #'default-prompt-read (guard-arity "current-prompt-read" 0))

(defun default-load-extension (path symbol)
  (declare (ignore path symbol))
  (raise-exn "exn:fail:unsupported" "default-load-extension: extensions are not supported"))

(define-param *current-load-extension* "current-load-extension"
  #'default-load-extension (guard-arity "current-load-extension" 2))

;;; ------------------------------------------------------------------
;;; Statistics, and printing's default handler

(defnative "vector-set-performance-stats!" (vec &optional (thread qs:false)) (:leaf t)
  (unless (and (simple-vector-p vec) (not (immutable-p vec)))
    (arg-error "vector-set-performance-stats!" "(and/c vector? (not/c immutable?))" vec))
  (unless (or (not (truthy thread)) (rthread-p thread))
    (arg-error "vector-set-performance-stats!" "(or/c thread? #f)" thread))
  (flet ((set-stat (i v) (when (< i (length vec)) (setf (svref vec i) v))))
    (if (truthy thread)
	;; running, dead, blocked, continuation size
	(progn (set-stat 0 qs:true) (set-stat 1 qs:false) (set-stat 2 qs:false) (set-stat 3 0))
	(progn
	  (set-stat 0 (floor (get-internal-run-time) (/ internal-time-units-per-second 1000)))
	  (set-stat 1 (floor (get-internal-real-time) (/ internal-time-units-per-second 1000)))
	  (set-stat 2 (floor sb-ext:*gc-run-time* (/ internal-time-units-per-second 1000)))
	  (set-stat 3 0)
	  (set-stat 4 0) (set-stat 5 0) (set-stat 6 1) (set-stat 7 0) (set-stat 8 0) (set-stat 9 0)
	  (set-stat 10 0)
	  (set-stat 11 (sb-kernel:dynamic-usage)))))
  *void*)

(defnative "dump-memory-stats" (&rest args) (:leaf t)
  ;; the object counts are Chez's: just the memory in use
  (declare (ignore args))
  (format *error-output* "Current memory use: ~D~%" (sb-kernel:dynamic-usage))
  *void*)

(defnative "default-global-port-print-handler" (v port &optional depth) ()
  (declare (ignore depth))
  (write-with-mode v port 0 :print))
