; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-API -*-

;;;; The Common Lisp face of Pseudoscheme: packages R5RS, R6RS and R7RS.
;;;;
;;;;   (r6rs:eval "(import (rnrs)) (display (list-sort < '(3 1 2)))")
;;;;   (r6rs:eval '(let-values (((q r) (div-and-mod 17 5))) (list q r)))
;;;;   (r7rs:load "prog.scm")
;;;;   (r6rs:repl)
;;;;
;;;; Each EVAL takes either Scheme source text (a string, read with the
;;;; Scheme reader -- the only way to write #f, vectors, chars and so on
;;;; exactly) or a datum written in Lisp, whose symbols are moved into
;;;; the SCHEME package (since Scheme names are case-inverted CL names,
;;;; CL's FOO is Scheme's `foo').  Values come back as the Lisp objects
;;;; Scheme uses: () is NIL, #t is T, #f is PS:FALSE (test with
;;;; R6RS:TRUE-P), procedures are functions, strings are strings.
;;;;
;;;; R6RS and R7RS evaluation go through psyntax.  A program (text beginning with
;;;; an `import' form) runs as an R6RS top-level program; anything else is
;;;; evaluated form by form at the REPL top level, where every binding of
;;;; (pseudoscheme) -- all of R6RS plus extensions -- is visible and
;;;; definitions accumulate.

(defpackage "PSEUDOSCHEME-API"
  (:use "COMMON-LISP")
  (:export "SCHEMIFY" "READ-SCHEME-FORMS" "TRUE-P" "REPL-LOOP"))

(defpackage "R6RS"
  (:use)
  (:import-from "PSEUDOSCHEME-API" "TRUE-P")
  (:export "EVAL" "LOAD" "REPL" "TRUE-P" "FALSE"))

(defpackage "R7RS"
  (:use)
  (:import-from "PSEUDOSCHEME-API" "TRUE-P")
  (:export "EVAL" "LOAD" "REPL" "TRUE-P" "FALSE"))

(defpackage "R5RS"
  (:use)
  (:import-from "PSEUDOSCHEME-API" "TRUE-P")
  (:export "EVAL" "LOAD" "REPL" "TRUE-P" "FALSE"))

(in-package "PSEUDOSCHEME-API")

(defconstant r6rs:false 'ps::false)
(defconstant r7rs:false 'ps::false)
(defconstant r5rs:false 'ps::false)

(defun true-p (x)
  "Scheme truth: everything but #f is true."
  (ps:truep x))

(defun schemify (datum)
  "A Lisp datum as a Scheme one: symbols move to the SCHEME package
(except T, which is #t, and NIL, which is ()); conses and vectors are
copied recursively."
  (typecase datum
    (null nil)
    ((eql t) t)
    (keyword (ps:intern-scheme-symbol (concatenate 'string ":" (ps:invert-case (symbol-name datum)))))
    ;; Already Scheme: SCHEME symbols, #f (PS:FALSE), uninterned symbols.
    (symbol (if (or (eq (symbol-package datum) ps:scheme-package)
		    (eq datum ps:false)
		    (null (symbol-package datum)))
		datum
		(intern (symbol-name datum) ps:scheme-package)))
    (cons (cons (schemify (car datum)) (schemify (cdr datum))))
    (simple-vector (map 'simple-vector #'schemify datum))
    (t datum)))

(defun read-scheme-forms (string)
  (with-input-from-string (in string)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun as-forms (source)
  (if (stringp source) (read-scheme-forms source) (list (schemify source))))

;;; ------------------------------------------------------------------
;;; R6RS: psyntax

(defun ensure-psyntax ()
  ;; One host serves R6RS and R7RS: this boots psyntax and installs the
  ;; R7RS and compatibility libraries too.
  (ps-r7rs::boot))

(defun r6rs-eval-forms (forms)
  (ensure-psyntax)
  (cond ((null forms) ps:unspecific)
	((or (psl:keyword-head-p (car forms) "import")
	     (psl:keyword-head-p (car forms) "library"))
	 (psx:eval-forms forms))
	(t (let ((v ps:unspecific))
	     (dolist (form forms v)
	       (setq v (psx:eval-top-level form)))))))

(defun r6rs:eval (source)
  (r6rs-eval-forms (as-forms source)))

(defun r6rs:load (path)
  (ensure-psyntax)
  (psx:load-file path))

;;; ------------------------------------------------------------------
;;; R7RS: psyntax too (src/r7rs/front.lisp)

(defun r7rs-eval-forms (forms)
  (ps-r7rs::boot)
  (cond ((null forms) ps:unspecific)
	((or (psl:keyword-head-p (car forms) "import")
	     (psl:keyword-head-p (car forms) "define-library")
	     (psl:keyword-head-p (car forms) "library"))
	 (ps-r7rs::eval-forms forms))
	(t (let ((v ps:unspecific))
	     (dolist (form forms v)
	       (setq v (ps-r7rs::eval-at-repl form)))))))

(defun r7rs:eval (source)
  (r7rs-eval-forms (as-forms source)))

(defun r7rs:load (path)
  (ps-r7rs::boot)
  (ps-r7rs::load-file path))

;;; ------------------------------------------------------------------
;;; R5RS: the classic translator, in the R5RS user environment, with
;;; case folding

(defun r5rs-eval-forms (forms)
  (let ((v ps:unspecific))
    (dolist (form forms v)
      (setq v (ps:scheme-eval form ps:scheme-user-environment)))))

(defun r5rs:eval (source)
  (if (stringp source)
      (let ((ps:*fold-case* t)) (r5rs-eval-forms (read-scheme-forms source)))
      (r5rs-eval-forms (list (schemify source)))))

(defun r5rs:load (path)
  (let ((ps:*fold-case* t))
    (r5rs-eval-forms (psl:read-forms-from-file path))))

;;; ------------------------------------------------------------------
;;; A REPL, shared by the three (and by the command-line program)

(defun print-values (values stream)
  (dolist (v values)
    (unless (eq v ps:unspecific)
      (funcall ps:*scheme-write* v stream)
      (terpri stream))))

(defun report-error (e stream)
  (format stream "~&;; Error: ~A~%" (string-trim '(#\Newline #\Space) (princ-to-string e))))

(defun quit-command-p (form)
  ;; ,q reads as (unquote q)
  (and (consp form) (psl:keyword-head-p form "unquote") (symbolp (cadr form))
       (member (ps:scheme-symbol-name (cadr form)) '("q" "quit" "exit") :test #'string=)))

(defun repl-loop (evaluator &key (prompt "> ") (input *standard-input*) (output *standard-output*))
  "Read Scheme forms from INPUT, evaluate each with EVALUATOR (a
function of one form), print the results.  ,q or end of file exits."
  (loop
    (format output "~A" prompt)
    (finish-output output)
    (let ((form (handler-case (funcall ps:*scheme-read* input)
		  (error (e) (report-error e output) (clear-input input) nil))))
      (cond ((eq form ps:eof-object) (terpri output) (return))
	    ((quit-command-p form) (return))
	    (form
	     (handler-case
		 (print-values (multiple-value-list (funcall evaluator form)) output)
	       (error (e) (report-error e output))))))))

(defun r6rs:repl ()
  (ensure-psyntax)
  (repl-loop (lambda (form) (r6rs-eval-forms (list form))) :prompt "r6rs> "))

(defun r7rs:repl ()
  (ps-r7rs::boot)
  (repl-loop (lambda (form) (ps-r7rs::eval-at-repl form)) :prompt "r7rs> "))

(defun r5rs:repl ()
  (let ((ps:*fold-case* t))
    (repl-loop (lambda (form) (r5rs-eval-forms (list form))) :prompt "r5rs> ")))
