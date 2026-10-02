; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-SYNTAX-CASE -*-

;;;; Dybvig & Hieb's syntax-case, running on Pseudoscheme.
;;;;
;;;; The expander itself is the vendored reference implementation under
;;;; vendor/syntax-case/ (see README-pseudoscheme.md there).  This file
;;;; loads it into the Scheme user environment and provides the glue:
;;;; a form is run through EXPAND-SYNTAX (macro expansion, hygienic
;;;; renaming, down to a handful of core forms) and the result is then
;;;; handed to Pseudoscheme's ordinary translator.
;;;;
;;;; It lives *alongside* Pseudoscheme's own classifier-based
;;;; DEFINE-SYNTAX/SYNTAX-RULES rather than replacing them: forms must
;;;; be sent through SC-EVAL / SC-LOAD to see syntax-case macros, and
;;;; macros defined that way are invisible to plain SCHEME-EVAL.  Making
;;;; the translator's own front end pluggable is a larger step (see
;;;; ROADMAP.md).

(defpackage "PSEUDOSCHEME-SYNTAX-CASE"
  (:nicknames "SC")
  (:use "COMMON-LISP")
  (:export "SC-EXPAND" "SC-EVAL" "SC-LOAD" "*SC-ENVIRONMENT*"))

(in-package "PSEUDOSCHEME-SYNTAX-CASE")

(defvar *sc-environment* ps:scheme-user-environment
  "The Scheme environment syntax-case's definitions live in and that
SC-EVAL evaluates in.")

(defun vendor-file (name)
  (merge-pathnames name
		   (asdf:system-relative-pathname :pseudoscheme
						  "vendor/syntax-case/")))

(defun sc-expander ()
  (symbol-value (find-symbol "EXPAND-SYNTAX" "SCHEME")))

(defun sc-expand (form)
  "Macro-expand FORM (a Scheme datum) with syntax-case, returning core-form
Scheme."
  (funcall (sc-expander) form))

(defvar *expansion-environment* nil
  "While SC-EVAL runs, the environment macro transformers are evaluated in.")

(defun sc-eval (form &optional (env *sc-environment*))
  "Expand FORM with syntax-case, then evaluate the result in ENV.
Transformer expressions encountered while expanding are evaluated in ENV
too (R6RS would say: at phase 1, with whatever ENV can see at run time)."
  (let ((*expansion-environment* env))
    (ps:scheme-eval (sc-expand form) env)))

(defun map-forms (path reader fn)
  (let ((ps:*scheme-read* reader))
    (with-open-file (in path)
      (loop (let ((form (funcall ps:*scheme-read* in)))
	      (when (eq form ps:eof-object) (return))
	      (funcall fn form))))))

(defun sc-load (path &optional (env *sc-environment*))
  "Load a Scheme source file with every form going through syntax-case."
  (map-forms path ps:*scheme-read*
	     #'(lambda (form) (sc-eval form env))))

(defun raw-load (name reader)
  (map-forms (vendor-file name) reader
	     #'(lambda (form)
		 (ps:scheme-eval form *sc-environment*))))

;;; Bring the expander up.  Order matters, as in the original loadpp.ss
;;; (compat, hooks, output, init, expand.pp, then the macro definitions);
;;; EXTRAS adds WHEN/UNLESS, which macro-defs.ss assumes Chez provides.
;;; hooks-pseudoscheme.ss uses bare PS-LISP:FOO symbols and so needs the
;;; CL-reader bridge; the rest read fine with the dedicated reader,
;;; which is what ps:*scheme-read* is once :pseudoscheme/reader loads.

(defun load-syntax-case ()
  (let ((cl-bridge #'ps:scheme-read-using-commonlisp-reader)
	(scheme-reader ps:*scheme-read*))
    (raw-load "compat.ss" scheme-reader)
    (raw-load "hooks-pseudoscheme.ss" cl-bridge)
    (raw-load "output.ss" scheme-reader)
    (raw-load "init.ss" scheme-reader)
    (raw-load "expand.pp" scheme-reader)
    (map-forms (vendor-file "extras-pseudoscheme.ss") scheme-reader #'sc-eval)
    (map-forms (vendor-file "macro-defs.ss") scheme-reader #'sc-eval)))

(load-syntax-case)

;;; Point the vendored expander's EXPANDER-ENVIRONMENT-HOOK (see
;;; hooks-pseudoscheme.ss) at *EXPANSION-ENVIRONMENT*.
(setf (symbol-value (find-symbol "EXPANDER-ENVIRONMENT-HOOK" "SCHEME"))
      (lambda () (or *expansion-environment* *sc-environment*)))
(ps:set-function-from-value (find-symbol "EXPANDER-ENVIRONMENT-HOOK" "SCHEME"))
