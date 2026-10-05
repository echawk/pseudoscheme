; -*- Mode: Lisp; Syntax: Common-Lisp; Package: REVISED^4-SCHEME; -*-
; File rts.lisp / Copyright (c) 1991 Jonathan Rees / See file COPYING

;;;; Revised^4 Scheme run-time system

(in-package "REVISED^4-SCHEME") ;should already exist.

(defmacro defune (name bvl &body body)
  (let ((new-name
	 (intern (let ((string (symbol-name name)))
		   ;; Cf. perhaps-rename in p-utils.scm
		   (if (multiple-value-bind (sym status)
			   (find-symbol string (find-package "PS-LISP"))
			 (declare (ignore sym))
			 (eq status :external))
		       (concatenate 'string "." string)
		       string))
		 *package*)))
    `(progn
	    (defun ,new-name ,bvl ,@body)
	    (ps:set-value-from-function ',new-name)
	    ',name)))

; Definitions for CAR and CDR for when they are *not* open-coded.
; There really ought to be definitions for CDADDR and friends, but the
; programmer is too lazy to produce them.

(defune car (pair)
  (if (not (consp pair))
      (error "Argument to CAR isn't a pair -- ~S" pair)
      (car pair)))

(defune cdr (pair)
  (if (not (consp pair))
      (error "Argument to CDR isn't a pair -- ~S" pair)
      (cdr pair)))

; Non-open-coded standard Scheme procedures, in alphabetical order (almost)

; DYNAMIC-WIND, sort of.

(defune dynamic-wind (in body out)
  (funcall in)
  (unwind-protect (funcall body)
    (funcall out)))

; MAKE-PROMISE (auxiliary for DELAY macro)

(defstruct (promise (:print-function print-promise)
		    (:predicate promisep)
		    (:constructor make-promise (thunk-or-value)))
  (forced-yet-p nil)
  thunk-or-value)

(defun print-promise (obj stream escapep)
  (declare (ignore escapep))
  (if (promise-forced-yet-p obj)
      (format stream "#{Forced ~S}" (promise-thunk-or-value obj))
      (format stream "#{Delayed}")))

; FORCE

(defune force (obj)
  (cond ((promisep obj)
         (let ((tv (promise-thunk-or-value obj)))
           (cond ((promise-forced-yet-p obj) tv)
                 (t (let ((val (funcall tv)))
                      (setf (promise-thunk-or-value obj) val)
                      (setf (promise-forced-yet-p obj) t)
                      val)))))
        (t obj)))

; LIST?

(defune list? (l)			;New in R4RS
  (do ((l l (cddr l))
       (lag l (cdr lag)))
      ((not (consp l)) (ps:true? (null l)))
    (when (not (consp (cdr l)))
      (return (ps:true? (null (cdr l)))))
    (when (eq (cdr l) lag)
      (return ps:false))))

; LOAD -- forward reference to not-yet-existing EVAL module

(defune load (filespec &rest optional-args)
  (apply #'ps:scheme-load filespec optional-args))

(defune eval (form env)
  (ps:scheme-eval form env))

; MAKE-STRING

(defune make-string (size &optional (fill #\?))
  (cond (fill (make-string size :initial-element fill))
        (t (make-string size))))

; MAKE-VECTOR

(defune make-vector (size &optional (fill ps:unspecific))
  (make-sequence 'vector size :initial-element fill))

; NOT

(defune not (obj)
  (ps:true? (eq obj ps:false)))

; NUMBER->STRING

(defune number->string (num &optional (radix 10))
  (let ((radix (if (equal radix '(scheme::heur)) 10 radix)))
    (ps:format-scheme-number num radix)))

; READ

(defune read (&optional (port *standard-input*))
  (funcall ps:*scheme-read* port))

; READ-CHAR

(defune read-char (&optional (port *standard-input*))
  (read-char port nil ps:eof-object))

(defune peek-char (&optional (port *standard-input*))
  (peek-char nil port nil ps:eof-object))

; STRING

(defune string (&rest chars)
  (coerce chars 'string))

; STRING->NUMBER

(defune string->number (string &optional (radix 10))
  (or (ps:parse-scheme-number string radix) ps:false))

; STRING-APPEND

(defune string-append (&rest strings)
  (apply #'concatenate 'simple-string strings))

; SYMBOL->STRING
;  The hair here is all to make printers written in Scheme produce
;  informative output, which wouldn't be the case if symbol->string were
;  the same as symbol-name.

(defune symbol->string (symbol)
  ;; Case is inverted between Scheme names and CL symbol names; see
  ;; INVERT-CASE in core.lisp.  The translator's own internals name CL
  ;; symbols and packages with PS-LISP:SYMBOL-NAME directly
  ;; (classify.scm's NAME->STRING, module.scm, emit.scm, reify.scm,
  ;; p-utils.scm), never through this.
  (let ((package (symbol-package symbol)))
    (cond ((or (eq package ps:scheme-package)
	       ;; Uninterned (GENSYM/MAKE-SYMBOL) symbols, e.g. hygienic
	       ;; renames, have no package to qualify with.
	       (null package)
	       ;; #:name keywords (docs/interop.md): their name
	       (keywordp symbol))
	   (ps:scheme-symbol-name symbol))
	  ((not (ps:scheme-symbol-p symbol))
	   (error "symbol->string: invalid argument - ~S"
		  symbol))
	  (t (multiple-value-bind (sym-again status)
		 (find-symbol (symbol-name symbol) package)
	       (declare (ignore sym-again))
	       (let ((fakename
		      (concatenate 'string
				   (if (keywordp symbol)
				       ""
				       (package-name package))
				   (if (eq status :external)
				       ":"
				       "::")
				   (symbol-name symbol))))
		 ;; A Lisp symbol (Scheme code can have them, see
		 ;; docs/interop.md): its qualified name.
		 fakename))))))

; VECTOR?

(proclaim '(inline vector?))
(defune vector? (obj)
  (ps:true? (simple-vector-p obj)))

; WRITE
; Do a real printer some time.
; It seems sensible to respect *print-pretty*, in any case.

(defune write (obj &optional (port *standard-output*))
  (funcall ps:*scheme-write* obj port))

(defune display (obj &optional (port *standard-output*))
  (funcall ps:*scheme-display* obj port))

; String ports (not required by R5RS itself, but widely provided as an
; extension, and relied on by chibi's R5RS/R7RS test suites).
;
; MAKE-STRING-OUTPUT-STREAM &co. aren't in PS's curated re-export of
; Common Lisp (see pack.lisp), so they're named with explicit package
; prefixes here rather than widening that shared allow-list.

(defune open-output-string ()
  (cl:make-string-output-stream))

(defune get-output-string (port)
  (cl:get-output-stream-string port))

(defune open-input-string (string)
  (cl:make-string-input-stream string))

(defune call-with-output-string (proc)
  (let ((port (cl:make-string-output-stream)))
    (funcall proc port)
    (cl:get-output-stream-string port)))

(defune with-output-to-string (thunk)
  (let ((port (cl:make-string-output-stream)))
    (let ((*standard-output* port))
      (funcall thunk))
    (cl:get-output-stream-string port)))

(defune flush-output (&optional (port *standard-output*))
  (cl:force-output port)
  ps:unspecific)

; CASE-AUX
;  Usually this should be open-coded, but sometimes it may not be.

(defune case-aux (key key-lists else-thunk &rest clause-thunks)
  (do ((ks key-lists (cdr ks))
       (ts clause-thunks (cdr ts)))
      ((null ks) (funcall else-thunk))
    (if (member key (car ks))
	(return (funcall (car ts))))))

; RATIONALIZE - implementation from IEEE Scheme standard

(defune rationalize (x e)
  ;; Inexact if either argument is (R7RS 6.2.6).
  (flet ((nanp (x) (ps:nan-p x))
	 (infp (x) (ps:infinite-p x)))
    (cond ((or (nanp x) (nanp e)) (if (nanp x) x e))
	  ((infp e) (if (infp x) (- e e) 0d0))
	  ((infp x) x)
	  (t (let* ((e (abs e))
		    (r (simplest-rational (cl:rational (- x e)) (cl:rational (+ x e)))))
	       (if (or (cl:floatp x) (cl:floatp e)) (cl:float r 1d0) r))))))

(defun simplest-rational (x y)
  (labels ((simplest-rational-internal
	    (x y)
	    (multiple-value-bind (fx x-fx)
		(floor x)
	      (multiple-value-bind (fy y-fy)
		  (floor y)
		(if (not (< fx x))
		    fx
		    (if (= fx fy)
			(+ fx
			   (/ 1
			      (simplest-rational-internal
			       (/ 1 y-fy)
			       (/ 1 x-fx))))
			(+ 1 fx)))))))
    (if (not (< x y))
	(if (rationalp x)
	    x
	    (error "(rationalize <irrational> 0) - ~S" x))
	(if (plusp x)
	    (simplest-rational-internal x y)
	    (if (minusp y)
		(- 0
		   (simplest-rational-internal (- 0 y)
					       (- 0 x)))
		0)))))

(defune interaction-environment ()
  (declare (special ps:*current-rep-environment*))
  ps:*current-rep-environment*)

(defune scheme-report-environment (n)
  (declare (special ps:scheme-report-environment))
  (case n
    ((4 5) ps:scheme-report-environment)
    (otherwise (error "invalid scheme report" n))))

; NULL-ENVIRONMENT is specified to contain only the R5RS syntactic
; keywords, with none of the procedure bindings SCHEME-REPORT-ENVIRONMENT
; provides. This implementation doesn't distinguish the two: there's
; only one environment object here, and EVAL doesn't enforce what may
; be called in it either way, so returning the fuller environment is a
; superset, not a divergent one, of what the standard specifies.

(defune null-environment (n)
  (declare (special ps:scheme-report-environment))
  (case n
    ((4 5) ps:scheme-report-environment)
    (otherwise (error "invalid scheme report" n))))

(defune syntax-error (message &rest irritants)
  (apply #'ps:scheme-warn message irritants))
