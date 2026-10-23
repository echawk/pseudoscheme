; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Racket CS's schemify layer as a linklet (racket/src/schemify), for its
;;;; linklet interpreter (schemify/interpret.rkt): Racket CS runs a
;;;; linklet compiled 'quick (an expand-time one, run once) by
;;;; schemifying it for the interpreter and interpreting it, not compiling
;;;; it.  utils/racket/extract.sh schemify makes
;;;; build/external/racket-derived/schemify.rktl, a linklet with no
;;;; imports; SCHEMIFY-EXPORT instantiates it the first time it's asked,
;;;; as the regexp layer is (regexp-layer.lisp).

(in-package "QUASISCHEME-RACKET")

(defvar *schemify-instance* nil "The schemify layer's instance, once instantiated.")

(defun derived-schemify-file ()
  (asdf:system-relative-pathname :quasischeme "build/external/racket-derived/schemify.rktl"))

(defun instantiate-schemify-layer ()
  "Compile and instantiate the schemify layer: its instance, or NIL if it
hasn't been extracted or doesn't instantiate."
  (or *schemify-instance*
      (let ((file (probe-file (derived-schemify-file))))
	(when file
	  (let* ((text (uiop:read-file-string file :external-format :utf-8))
		 (form (with-input-from-string (in text) (read-racket in)))
		 (compiled (compile-linklet-form form :name (rsym "schemify")))
		 (instance (make-linklet-instance (rsym "schemify"))))
	    (setf (compiled-linklet-source-key compiled) text)
	    (linklet-function compiled)
	    (setq *uncaught* nil)
	    (call-with-continuation-prompt
	     (lambda () (with-racket-errors (instantiate-linklet compiled '() instance)))
	     *default-prompt-tag* nil)
	    (unless *uncaught*
	      (setq *schemify-instance* instance)))))))

(defun schemify-export (name)
  "The schemify layer's export NAME (instantiating it), or NIL."
  (let ((instance (instantiate-schemify-layer)))
    (and instance (instance-value instance (rsym name)))))

;;; ------------------------------------------------------------------
;;; Interpreting a linklet
;;;
;;; As Racket CS's compile-linklet does in 'quick mode (racket/src/cs/
;;; linklet.sls): the linklet schemified for the target 'interp, made
;;; interpretable by interpretable-jitified-linklet and run by
;;; interpret-linklet, which gives a procedure of the instance's variable
;;; reference, the imported variables and the exported ones -- the shape
;;; of our compiled linklets' functions (INSTANTIATE-LINKLET).  The
;;; interpreter refers to primitives by name, in a table of all the
;;; instances' (INTERPRETER-PRIMITIVES), and to variables through the
;;; operations INTERPRETER-LINK! is given, here on our LINKLET-VARIABLEs.

(defvar *interpreter-primitives* nil
  "Symbol -> value: every primitive instance's, and the internal ones
schemify's output calls (cs/primitive/internal.ss).")

(defun call-with-module-prompt (proc &optional (syms nil syms-p) modes &rest vars)
  "Racket CS's: PROC under the default prompt, its values assigned to
VARS (if given), which must then be defined."
  (declare (ignore modes))
  (flet ((handler (&rest args)
	   (apply #'abort-current-continuation* *default-prompt-tag* args)))
    (if (not syms-p)
	(call-with-continuation-prompt proc *default-prompt-tag* #'handler)
	(progn
	  (call-with-continuation-prompt
	   (lambda ()
	     (let ((vals (multiple-value-list (funcall proc))))
	       (unless (= (length vals) (length vars))
		 (racket-arity-error "define-values" (length vals) (length vars)))
	       (loop for v in vars for x in vals do (setf (linklet-variable-value v) x))))
	   *default-prompt-tag* #'handler)
	  (loop for v in vars for s in syms
		when (eq (linklet-variable-value v) +undefined+)
		  do (raise-variable-error s "cannot use before initialization"))
	  *void*))))

(defun abort-current-continuation* (tag &rest values)
  (apply (primitive "abort-current-continuation") tag values))

(defun interpreter-primitives ()
  (or *interpreter-primitives*
      (let ((h (make-rhash :eq)))
	(flet ((add (name value) (with-rhash-lock (h) (setf (gethash (rsym name) (rhash-table h)) value))))
	  (maphash (lambda (instance table)
		     (declare (ignore instance))
		     (maphash #'add table))
		   *primitive-instances*)
	  (add "call-with-module-prompt" #'call-with-module-prompt)
	  (add "#%call-with-values" (primitive "call-with-values"))
	  (add "unbox/check-undefined"
	       (lambda (b name)
		 (let ((x (box-ref* b)))
		   (if (eq x +undefined+) (raise-variable-error name "cannot use before initialization") x))))
	  (add "set-box!/check-undefined"
	       (lambda (b v name)
		 (when (eq (box-ref* b) +undefined+)
		   (raise-variable-error name "assignment disallowed; cannot set variable before its definition"))
		 (setf (rbox-value b) v)
		 *void*))
	  (add "raise-binding-result-arity-error"
	       (lambda (expected args) (racket-arity-error "local-binding form" (length args) expected)))
	  (add "raise-definition-result-arity-error"
	       (lambda (expected args) (racket-arity-error "define-values" (length args) (length expected))))
	  (add "#%struct-constructor" (lambda (p mask) (declare (ignore mask)) p))
	  (add "#%struct-predicate" (lambda (p) p))
	  (add "#%struct-field-accessor" (lambda (p rtd pos) (declare (ignore rtd pos)) p))
	  (add "#%struct-field-mutator" (lambda (p rtd pos) (declare (ignore rtd pos)) p)))
	(setq *interpreter-primitives* h))))

(defvar *primitive-knowns* nil
  "Symbol -> schemify's `known' for each primitive, as Racket CS gives
schemify-linklet (cs/schemify.sls, PRIM-KNOWNS): schemify expects them
of some primitives, and uses them to optimize.")

(defparameter *known-table-files*
  '("kernel" "unsafe" "flfxnum" "paramz" "extfl" "network" "futures" "place"
    "foreign" "linklet" "internal")
  "racket/src/cs/primitive/'s tables, in Racket CS's order.")

(defun primitive-knowns ()
  "*PRIMITIVE-KNOWNS*, read from the tables in Racket's source: each
entry [prim (known-constructor arg ...)], the constructor schemify's."
  (or *primitive-knowns*
      (let ((h (make-rhash :eq)))
	(dolist (file *known-table-files*)
	  (let ((path (probe-file (racket-source (format nil "racket/src/cs/primitive/~A.ss" file)))))
	    (when path
	      (with-open-file (in path :external-format :utf-8)
		(loop for form = (read-racket in)
		      until (eq form *eof*)
		      when (and (consp form) (symbolp (car form))
				(string= (rname (car form)) "define-primitive-table"))
			do (dolist (entry (cddr form))
			     (destructuring-bind (prim (ctor &rest args)) entry
			       (let ((f (ignore-errors (schemify-export (rname ctor)))))
				 (when (functionp f)
				   (with-rhash-lock (h) (setf (gethash prim (rhash-table h))
					 (apply f (mapcar (lambda (a)
							    (if (and (consp a) (symbolp (car a))
								     (string= (rname (car a)) "quote"))
								(cadr a)
								a))
							  args)))))))))))))
	(setq *primitive-knowns* h))))

(defvar *interpreter-linked* nil)

(defun link-interpreter ()
  (unless *interpreter-linked*
    (funcall (schemify-export "interpreter-link!")
	     (interpreter-primitives)
	     #'correlated->datum
	     (lambda (name) (make-linklet-variable name))
	     #'variable-ref
	     #'linklet-variable-value
	     (lambda (v x) (setf (linklet-variable-value v) x) *void*)
	     (lambda (v x constance) (declare (ignore constance)) (setf (linklet-variable-value v) x) *void*)
	     (lambda (proc mask name) (declare (ignore mask name)) proc)
	     (lambda (name) name)
	     (lambda (e) (declare (ignore e)) (racket-error "interpret" "ffi isn't interpreted")))
    (setq *interpreter-linked* t)))

(defun interpreted-linklet (form name)
  "FORM, a linklet S-expression, as a COMPILED-LINKLET that is interpreted
\(Racket CS's 'quick mode), or NIL if the schemify layer isn't there, or
schemifying it raises an exception (then it's compiled)."
  (when (instantiate-schemify-layer)
    (link-interpreter)
    (catch 'schemify-failed
      (qs-r6rs::call-with-mark
       *exception-handler-key*
       (lambda (e)
	 (when (uiop:getenv "QUASISCHEME_RACKET_BACKTRACE")
	   (format *error-output* "~&;; not interpreted: ~A~%"
		   (if (exn-p e) (exn-message e) e)))
	 (throw 'schemify-failed nil))
       (lambda () (interpreted-linklet-1 form name))))))

(defun interpreted-linklet-1 (form name)
  (progn
    (multiple-value-bind (impl-lam importss exports)
	(funcall (schemify-export "schemify-linklet")
		 form (make-rbox :value qs:false) qs:true (rsym "interp") qs:false qs:false qs:false qs:true qs:false
		 (primitive-knowns) (interpreter-primitives) (lambda (v) (declare (ignore v)) qs:false)
		 (lambda (key) (declare (ignore key)) (values qs:false qs:false qs:false)) qs:false)
      (let* ((b (funcall (schemify-export "interpretable-jitified-linklet") impl-lam qs:false (rsym "racket")))
	     (proc (funcall (schemify-export "interpret-linklet") b))
	     (compiled (make-compiled-linklet :interpreted :name name :strategy "interp")))
	(setf (compiled-linklet-imports compiled) importss
	      (compiled-linklet-exports compiled) exports
	      (compiled-linklet-export-internals compiled) exports
	      (compiled-linklet-source-form compiled) form
	      (compiled-linklet-function compiled)
	      (lambda (instance &rest variables)
		(apply proc (make-variable-reference instance qs:false) variables)))
	compiled))))
