; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Assembling R6RS
;;;;
;;;; The same recipe as r7rs.lisp, with two differences that make this
;;;; a sketch rather than a layer:
;;;;
;;;;  * R6RS code is expanded with syntax-case (the vendored
;;;;    Dybvig/Hieb expander), so library bodies are evaluated by
;;;;    R6RS-EVAL, not the native translator front end;
;;;;  * the macros are therefore syntax-case macros, which live in that
;;;;    expander's one global table, not in per-library nodes.  Exports
;;;;    of macros are recorded but not scoped, renamed or restricted.
;;;;
;;;; (rnrs base) here is the R7RS layer's bindings, overridden by the
;;;; R6RS-specific ones (ERROR, DIV/MOD, ...), minus whatever R6RS
;;;; doesn't export.

(in-package "PSEUDOSCHEME-R6RS")

(defvar *implementation-env* nil)
(defvar *stubs* '())
(defvar *missing-syntax* '())

(defun sym (string) (psl:scheme-symbol string))

(defun all-r7rs-names ()
  (remove-duplicates
   (append (mapcar #'sym (loop for (nil export-string) in r7rs:*standard-libraries*
			       append (r7rs::split-names export-string)))
	   (psl:tr "INTERFACE-NAMES"
		    (psl:tr "STRUCTURE-INTERFACE" (psl:base-structure))))))

(defun sc-macro-p (name)
  "Is NAME a macro or special form known to the syntax-case expander?"
  (let ((key (find-symbol "%MACRO-TRANSFORMER" "SCHEME")))
    (and key (get name key) t)))

(defun r6rs-eval (form env)
  "Expand FORM with syntax-case, then translate and evaluate it in ENV."
  (let ((sc:*sc-environment* env))
    (sc:sc-eval form env)))

;;; Names every R6RS library body can see regardless of its imports:
;;; the core forms the expander emits, and the run-time procedures its
;;; derived-form templates (CASE, DO, SYNTAX-CASE, ...) call.
(defparameter *expander-support-names*
  '("quote" "lambda" "if" "set!" "begin" "define" "letrec"
    "syntax-dispatch" "memv" "void" "car" "cdr" "cadr" "cddr" "caddr" "cons"
    "list" "apply" "eq?" "not" "equal?" "append" "map" "vector" "list->vector"
    "call-with-values" "values" "implicit-identifier" "error"))

(defun build-implementation-env ()
  (let ((env (psl:new-library-env "r6rs implementation")))
    (psl:copy-bindings! env r7rs:*implementation-env* (all-r7rs-names))
    ;; The expander's own run-time procedures live in the user env.
    (psl:copy-bindings! env ps:scheme-user-environment
			;; SYNTAX-ERROR: the expander's procedure, which
			;; syntax-case's generated "no clause matched" code
			;; calls -- not R7RS's macro of the same name.
			(mapcar #'sym '("syntax-dispatch" "implicit-identifier" "syntax-error"
					"syntax-object->datum" "identifier?"
					"bound-identifier=?" "free-identifier=?"
					"generate-temporaries" "void" "memv")))
    (loop for (name . function) in *primitives*
	  do (psl:install-variable! env (sym name) function))
    (r7rs::load-scheme-file-from "r6rs/base.scm" env)
    env))

(defun load-macros ()
  (let ((sc:*sc-environment* *implementation-env*))
    (with-open-file (in (asdf:system-relative-pathname :pseudoscheme "src/r6rs/macros.ss"))
      (loop for form = (funcall ps:*scheme-read* in)
	    until (eq form ps:eof-object)
	    do (sc:sc-eval form *implementation-env*)))))

(defun install-library (library-name export-strings env)
  (let ((exports '()) (syntax-names '()) (stubbed '()) (missing '()))
    (dolist (export export-strings)
      (let ((name (sym export)))
	(cond ((psl:binding-defined-p env name) (push name exports))
	      ((sc-macro-p name) (push name syntax-names))
	      ((member export r7rs::*auxiliary-keywords* :test #'string=)
	       (r7rs::ensure-auxiliary-keyword env name)
	       (push name exports))
	      ((member export (r7rs::split-names *syntax-exports*) :test #'string=)
	       (push export missing))
	      (t (psl:install-variable! env name (r7rs::stub-function library-name export))
		 (push export stubbed)
		 (push name exports)))))
    (push (cons (psl:library-key-of library-name) (nreverse stubbed)) *stubs*)
    (push (cons (psl:library-key-of library-name) (nreverse missing)) *missing-syntax*)
    (psl:make-library-from-env (mapcar (lambda (w) (sym (psl:sname w))) library-name)
			       env (nreverse exports)
			       :syntax-names (nreverse syntax-names))))

(defun install-support-library (env)
  "(pseudoscheme sc-support): what the expander needs visible in every
library; imported implicitly by EVALUATE-LIBRARY-FORM."
  (psl:make-library-from-env (mapcar #'sym '("pseudoscheme" "sc-support")) env
			     (remove-if-not (lambda (n) (psl:binding-defined-p env n))
					    (mapcar #'sym *expander-support-names*))))

(defun install-r6rs ()
  (setq *stubs* '() *missing-syntax* '())
  (setq *implementation-env* (build-implementation-env))
  (load-macros)
  (loop for (name export-string) in (append *standard-libraries* *partial-libraries*)
	do (install-library name (r7rs::split-names export-string) *implementation-env*))
  (install-support-library *implementation-env*)
  *implementation-env*)

;;; ------------------------------------------------------------------
;;; Entry points

(defparameter *implicit-imports*
  (list (mapcar #'sym '("pseudoscheme" "sc-support"))))

(defun evaluate-library-form (form)
  "Define the library in an R6RS (library name (export ...) (import ...) body ...) form."
  (let ((psl:*library-evaluator* #'r6rs-eval)
	(psl:*syntax-binding-p* #'sc-macro-p))
    (psl:r6rs-library-form form :implicit-imports *implicit-imports*)))

(defun load-r6rs-program (path)
  "Load a file containing zero or more (library ...) forms followed by an
(import ...) top-level program."
  (let ((forms (psl:read-forms-from-file path))
	(psl:*library-evaluator* #'r6rs-eval)
	(psl:*syntax-binding-p* #'sc-macro-p)
	(libraries '()))
    (loop while (and forms (psl:keyword-head-p (car forms) "library"))
	  do (push (evaluate-library-form (pop forms)) libraries))
    (if forms
	(psl:run-program forms :implicit-imports *implicit-imports*)
	(values nil libraries))))

(install-r6rs)
