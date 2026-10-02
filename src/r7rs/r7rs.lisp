; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; Assembling R7RS-small
;;;;
;;;; 1. Start a fresh environment from a *copy* of the R5RS bindings
;;;;    (REVISED^4-SCHEME-STRUCTURE already carries R5RS's additions).
;;;; 2. Install the Lisp primitives of rts.lisp.
;;;; 3. Evaluate base.scm in it (derived syntax, easy procedures).
;;;; 4. For each library in Appendix A, export the names that are now
;;;;    defined; names still missing become STUBS that signal "not
;;;;    implemented", so programs importing them load and fail only if
;;;;    they actually call them.  (Missing *syntax* can't be stubbed that
;;;;    way and is just left out.)
;;;;
;;;; (PRINT-COVERAGE) tells you where things stand.

(in-package "PSEUDOSCHEME-R7RS")

(defvar *implementation-env* nil
  "The environment the standard libraries' bindings are taken from.")

(defvar *stubs* '()
  "Alist (library-key . list of export strings that are only stubs).")

(defvar *missing-syntax* '()
  "Alist (library-key . list of syntactic exports not provided at all).")

(defun table-get (table name)
  (cdr (assoc (psl:library-key-of name) table :test #'equal)))

(defun table-put (table-name name value)
  (push (cons (psl:library-key-of name) value) (symbol-value table-name)))

(defun sym (string) (psl:scheme-symbol string))

(defun read-scheme-from-string (string)
  (with-input-from-string (in string)
    (funcall ps:*scheme-read* in)))

(defun eval-in (form env)
  (ps:scheme-eval form env))

(defun load-scheme-file-from (path env)
  "Evaluate every form of PATH (relative to src/) in ENV with the
dedicated reader, natively."
  (with-open-file (in (asdf:system-relative-pathname
		       :pseudoscheme (concatenate 'string "src/" path)))
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  do (eval-in form env))))

(defun load-scheme-file (name env)
  (load-scheme-file-from (concatenate 'string "r7rs/" name) env))

;;; ------------------------------------------------------------------
;;; Primitives that need the library machinery itself

(defprim "features" ()
  (mapcar #'sym psl:*scheme-features*))

(defprim "environment" (&rest import-sets)
  (let ((env (psl:new-library-env "eval")))
    (psl:import-into env import-sets)
    env))

;;; ------------------------------------------------------------------
;;; cond-expand, include, syntax-error
;;;
;;; These need to look outside the program text (features, libraries,
;;; files) or fail at expansion time, which SYNTAX-RULES cannot do, so
;;; they are procedural macros.  Pseudoscheme's classifier already
;;; takes explicit-renaming transformers -- a procedure of (form rename
;;; compare), RENAME closing a name in the macro's own environment --
;;; wrapped by MAKE-MACRO, so none of this needs new machinery.

(defun define-procedural-macro (env name transformer)
  (psl:tr "PROGRAM-ENV-DEFINE!" env (sym name)
	  (psl:tr "MAKE-MACRO" transformer env)))

(defun strip-generated (form)
  (psl:tr "STRIP" form))

(defun cond-expand-transformer (form rename compare)
  (declare (ignore compare))
  (multiple-value-bind (body found)
      (psl:select-cond-expand-clause (strip-generated (cdr form)))
    (unless found
      (ps:scheme-error "cond-expand: no clause applies: ~S" (strip-generated form)))
    (cons (funcall rename (sym "begin")) body)))

(defun include-transformer (form rename compare)
  (declare (ignore compare))
  (cons (funcall rename (sym "begin"))
	(loop for file in (cdr form)
	      append (psl:read-forms-from-file file))))

(defun syntax-error-transformer (form rename compare)
  (declare (ignore rename compare))
  (destructuring-bind (message &rest forms) (strip-generated (cdr form))
    (apply #'ps:scheme-error "syntax-error: ~A~{ ~S~}" (list message forms))))

;;; ------------------------------------------------------------------
;;; Building the implementation environment and the libraries

(defparameter *auxiliary-keywords* '("else" "=>" "..." "_" "unquote" "unquote-splicing")
  "Exports that are only ever matched by name inside other syntax; they
need a binding to be importable but have no meaning of their own.")

(defun ensure-auxiliary-keyword (env name)
  "Bind NAME in ENV if nothing does yet, the way R4RS ELSE and => are."
  (let ((node (psl:tr "PROGRAM-ENV-LOOKUP" env name)))
    (declare (ignorable node))
    t))

(defun stub-function (library-name export)
  (lambda (&rest args)
    (declare (ignore args))
    (ps:scheme-error "~A: not implemented yet (~{~A~^ ~})"
		     export (mapcar #'psl:sname library-name))))

(defun build-implementation-env ()
  (let ((env (psl:new-library-env "r7rs implementation")))
    (psl:copy-all! env (psl:base-structure))
    (loop for (name . function) in *primitives*
	  do (psl:install-variable! env (sym name) function))
    (define-procedural-macro env "cond-expand" #'cond-expand-transformer)
    (define-procedural-macro env "include" #'include-transformer)
    (define-procedural-macro env "include-ci" #'include-transformer)
    (define-procedural-macro env "syntax-error" #'syntax-error-transformer)
    (load-scheme-file "base.scm" env)
    env))

(defun install-library (library-name export-strings env)
  "Register one standard library, stubbing procedures that aren't there."
  (let ((exports '()) (stubbed '()) (missing '()))
    (dolist (export export-strings)
      (let ((name (sym export)))
	(cond ((psl:binding-defined-p env name)
	       (push name exports))
	      ((member export *auxiliary-keywords* :test #'string=)
	       ;; PROGRAM-ENV-LOOKUP conjures an (unbound) variable node,
	       ;; which is all ELSE and => are in R5RS.
	       (ensure-auxiliary-keyword env name)
	       (push name exports))
	      ((syntax-export-p export)
	       (push export missing))
	      (t
	       (psl:install-variable! env name (stub-function library-name export))
	       (push export stubbed)
	       (push name exports)))))
    (table-put '*stubs* library-name (nreverse stubbed))
    (table-put '*missing-syntax* library-name (nreverse missing))
    (psl:make-library-from-env (mapcar (lambda (w) (sym (psl:sname w))) library-name)
			       env (nreverse exports))))

(defun install-r7rs ()
  (setq *stubs* '() *missing-syntax* '())
  (setq *implementation-env* (build-implementation-env))
  (loop for (name export-string) in *standard-libraries*
	do (install-library name (split-names export-string) *implementation-env*))
  *implementation-env*)

;;; ------------------------------------------------------------------
;;; Reporting

(defun coverage ()
  "List of (library-name total implemented stubbed missing-syntax)."
  (loop for (name export-string) in *standard-libraries*
	for total = (length (split-names export-string))
	for stubbed = (length (table-get *stubs* name))
	for missing = (length (table-get *missing-syntax* name))
	collect (list name total (- total stubbed missing) stubbed missing)))

(defun print-coverage (&optional (stream *standard-output*))
  (format stream "~&~28A ~7@A ~11@A ~7@A ~8@A~%" "library" "exports" "implemented" "stubbed" "missing")
  (let ((tt 0) (ti 0))
    (loop for (name total impl stubbed missing) in (coverage)
	  do (incf tt total) (incf ti impl)
	     (format stream "~28A ~7D ~11D ~7D ~8D~%"
		     (format nil "~{~A~^ ~}" (mapcar #'psl:sname name))
		     total impl stubbed missing))
    (format stream "~28A ~7D ~11D~%" "total" tt ti))
  (loop for (key . stubbed) in (reverse *stubs*)
	when stubbed
	  do (format stream "~&stubbed in (~{~A~^ ~}): ~{~A~^ ~}~%" key stubbed))
  (loop for (key . missing) in (reverse *missing-syntax*)
	when missing
	  do (format stream "~&missing syntax in (~{~A~^ ~}): ~{~A~^ ~}~%" key missing)))

(install-r7rs)
