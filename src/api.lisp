; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-API -*-

;;;; The Common Lisp face of Pseudoscheme: packages R5RS, R6RS and R7RS.
;;;;
;;;;   (r7rs:eval "(import (scheme base)) (exact-integer-sqrt 17)")
;;;;   (r7rs:scheme (let-values (((q r) (floor/ 17 5))) (list q r)))  ; => (3 2)
;;;;   (r7rs:load "prog.scm")
;;;;   (r7rs:use-library '(srfi 1))            ; Scheme library -> package
;;;;   (srfi-1:fold #'+ 0 '(1 2 3))            ; => 6
;;;;   (r7rs:expand '(let loop ((i 0)) (loop (+ i 1))))     ; core Scheme
;;;;   (r7rs:translate '(lambda (x) (* x x)))  ; the Lisp it compiles to
;;;;   (r7rs:repl)
;;;;
;;;; Each package has the same functions, for its dialect:
;;;;
;;;;   EVAL source          evaluate; SOURCE is Scheme text (a string, read
;;;;                        with the Scheme reader: the way to write #f,
;;;;                        #\a, #(...) exactly) or a Lisp datum, whose
;;;;                        symbols are moved into SCHEME (CL's FOO is
;;;;                        Scheme's `foo').  Text that starts with
;;;;                        `import' runs as a program; anything else at
;;;;                        the REPL top level.  Values are returned as
;;;;                        Scheme has them: #f is FALSE (test with
;;;;                        TRUE-P), () is NIL.
;;;;   SCHEME form ...      macro: EVAL of the unevaluated FORMs, with
;;;;                        results Lisp-style (#f -> NIL).
;;;;   LOAD file            load a source file.
;;;;   REPL                 a read-eval-print loop on *standard-input*.
;;;;   EXPAND source        the expansion (core Scheme) of an expression.
;;;;   TRANSLATE source     the Lisp code an expression translates to.
;;;;   PROCEDURE name &key library convert
;;;;                        a Scheme procedure as a Lisp function.
;;;;   READ-FROM-STRING string, WRITE-TO-STRING object
;;;;                        the Scheme reader and writer.
;;;;   TRUE-P x, FALSE      Scheme truth, and #f.
;;;;   VERBATIM function    pass FUNCTION to Scheme without conversion.
;;;;
;;;; and R6RS and R7RS, which have libraries, also
;;;;
;;;;   USE-LIBRARY name &key package convert
;;;;                        make a Scheme library a Lisp package.
;;;;   LIBRARY-EXPORTS name what a library exports.
;;;;   *LIBRARY-PATH*, ADD-LIBRARY-DIRECTORY dir
;;;;                        where libraries are looked for.
;;;;
;;;; The names shadow CL's (EVAL, LOAD, READ-FROM-STRING...): use them
;;;; package-qualified, don't USE-PACKAGE these packages.  How booleans
;;;; and functions are converted between the languages is described in
;;;; src/interop.lisp and docs/interop.md.

(defpackage "PSEUDOSCHEME-API"
  (:use "COMMON-LISP")
  (:shadow "WRITE-TO-STRING")
  (:import-from "PSEUDOSCHEME-INTEROP" "SCHEMIFY" "VERBATIM")
  (:export "SCHEMIFY" "READ-SCHEME-FORMS" "TRUE-P" "REPL-LOOP" "VERBATIM"
	   "WRITE-TO-STRING" "ADD-LIBRARY-DIRECTORY"))

(macrolet ((dialect-package (name &rest extra)
	     `(defpackage ,name
		(:use)
		(:import-from "PSEUDOSCHEME-API" "TRUE-P" "VERBATIM" "WRITE-TO-STRING"
			      ,@(when extra '("ADD-LIBRARY-DIRECTORY")))
		,@(when extra '((:import-from "PSEUDOSCHEME-PSYNTAX" "*LIBRARY-PATH*")))
		(:export "EVAL" "LOAD" "REPL" "EXPAND" "TRANSLATE" "SCHEME" "PROCEDURE"
			 "READ-FROM-STRING" "WRITE-TO-STRING" "TRUE-P" "FALSE" "VERBATIM"
			 ,@extra))))
  (dialect-package "R7RS" "USE-LIBRARY" "LIBRARY-EXPORTS" "*LIBRARY-PATH*" "ADD-LIBRARY-DIRECTORY")
  (dialect-package "R6RS" "USE-LIBRARY" "LIBRARY-EXPORTS" "*LIBRARY-PATH*" "ADD-LIBRARY-DIRECTORY")
  (dialect-package "R5RS"))

(in-package "PSEUDOSCHEME-API")

(defconstant r7rs:false 'ps::false "Scheme's #f.")
(defconstant r6rs:false 'ps::false "Scheme's #f.")
(defconstant r5rs:false 'ps::false "Scheme's #f.")

(defun true-p (x)
  "Scheme truth: everything but #f is true."
  (ps:truep x))

(defun read-scheme-forms (string)
  (with-input-from-string (in string)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun as-forms (source)
  (if (stringp source) (read-scheme-forms source) (list (schemify source))))

(defun as-form (source)
  "SOURCE as one form: a string holding several is a (begin ...)."
  (let ((forms (as-forms source)))
    (if (and forms (null (cdr forms)))
	(car forms)
	(cons (ps:intern-scheme-symbol "begin") forms))))

(defun write-to-string (object)
  "OBJECT as Scheme's WRITE prints it."
  (with-output-to-string (s) (funcall ps:*scheme-write* object s)))

(defun lisp-values (values)
  "VALUES (a list) as Lisp multiple values, #f as NIL."
  (values-list (mapcar #'pseudoscheme-interop:to-lisp values)))

(defun add-library-directory (directory)
  "Search DIRECTORY for Scheme libraries too, after the directories
already on *LIBRARY-PATH*."
  (let ((dir (namestring (uiop:ensure-directory-pathname directory))))
    (setf psx:*library-path* (append (remove dir psx:*library-path* :test #'string=) (list dir)))
    dir))

;;; ------------------------------------------------------------------
;;; R6RS and R7RS: psyntax (one host serves both)

(defun ensure-psyntax ()
  ;; Boots psyntax and installs the R7RS, compatibility and interop
  ;; libraries.
  (pseudoscheme-interop:boot))

(defun r6rs-eval-forms (forms)
  (ensure-psyntax)
  (cond ((null forms) ps:unspecific)
	((or (psl:keyword-head-p (car forms) "import")
	     (psl:keyword-head-p (car forms) "library"))
	 (psx:eval-forms forms))
	(t (let ((v ps:unspecific))
	     (dolist (form forms v)
	       (setq v (psx:eval-top-level form)))))))

(defun r7rs-eval-forms (forms)
  (ensure-psyntax)
  (cond ((null forms) ps:unspecific)
	((or (psl:keyword-head-p (car forms) "import")
	     (psl:keyword-head-p (car forms) "define-library")
	     (psl:keyword-head-p (car forms) "library"))
	 (ps-r7rs::eval-forms forms))
	(t (let ((v ps:unspecific))
	     (dolist (form forms v)
	       (setq v (ps-r7rs::eval-at-repl form)))))))

(defun psyntax-environment (library)
  (funcall (psx:host-ref "psyntax:environment")
	   (mapcar #'ps:intern-scheme-symbol library)))

(defun r7rs-repl-eval (forms)
  "Evaluate FORMS one by one at the R7RS REPL top level, (import ...)
included; the last one's values."
  (ensure-psyntax)
  (let ((values (list ps:unspecific)))
    (dolist (form forms (values-list values))
      (setq values (multiple-value-list (ps-r7rs::eval-at-repl form))))))

(defun r6rs-repl-eval (forms)
  (ensure-psyntax)
  (let ((values (list ps:unspecific)))
    (dolist (form forms (values-list values))
      (setq values (multiple-value-list (psx:eval-top-level form))))))

(defun psyntax-expand (source library)
  (ensure-psyntax)
  (values (psx:expand (as-form source) (psyntax-environment library))))

(defun r7rs:eval (source)
  "Evaluate SOURCE (Scheme text, or a Lisp datum) as R7RS."
  (r7rs-eval-forms (as-forms source)))

(defun r6rs:eval (source)
  "Evaluate SOURCE (Scheme text, or a Lisp datum) as R6RS."
  (r6rs-eval-forms (as-forms source)))

(defun r7rs:load (path)
  "Load the R7RS source file PATH (a program, or library definitions)."
  (ensure-psyntax)
  (ps-r7rs::load-file path))

(defun r6rs:load (path)
  "Load the R6RS source file PATH (a program, or library definitions)."
  (ensure-psyntax)
  (psx:load-file path))

(defun r7rs:expand (source)
  "The core Scheme that psyntax expands SOURCE to, in the R7RS REPL
environment."
  (psyntax-expand source '("pseudoscheme" "r7rs")))

(defun r6rs:expand (source)
  "The core Scheme that psyntax expands SOURCE to, in the R6RS REPL
environment."
  (psyntax-expand source '("pseudoscheme")))

(defun r7rs:translate (source)
  "The Lisp code SOURCE translates to."
  (scheme-translator:translate (r7rs:expand source) psx:*host*))

(defun r6rs:translate (source)
  "The Lisp code SOURCE translates to."
  (scheme-translator:translate (r6rs:expand source) psx:*host*))

(defun r7rs:read-from-string (string)
  "The first datum in STRING, read by the Scheme reader."
  (funcall ps:*scheme-read* (make-string-input-stream string)))

(defun r6rs:read-from-string (string)
  "The first datum in STRING, read by the Scheme reader."
  (funcall ps:*scheme-read* (make-string-input-stream string)))

(defun r7rs:use-library (name &key package (convert t))
  "Make R7RS library NAME, e.g. '(srfi 1), a Lisp package.  See
PSEUDOSCHEME-INTEROP:USE-LIBRARY."
  (ensure-psyntax)
  (pseudoscheme-interop:use-library name :package package :convert convert))

(defun r6rs:use-library (name &key package (convert t))
  "Make R6RS library NAME, e.g. '(rnrs sorting), a Lisp package.  See
PSEUDOSCHEME-INTEROP:USE-LIBRARY."
  (ensure-psyntax)
  (pseudoscheme-interop:use-library name :package package :convert convert))

(defun r7rs:library-exports (name)
  "What library NAME exports: a list of (scheme-symbol . kind)."
  (ensure-psyntax)
  (pseudoscheme-interop:library-exports name))

(defun r6rs:library-exports (name)
  "What library NAME exports: a list of (scheme-symbol . kind)."
  (ensure-psyntax)
  (pseudoscheme-interop:library-exports name))

;;; ------------------------------------------------------------------
;;; R5RS: the classic translator, in the R5RS user environment, with
;;; case folding

(defun r5rs-eval-forms (forms)
  (let ((v ps:unspecific))
    (dolist (form forms v)
      (setq v (ps:scheme-eval form ps:scheme-user-environment)))))

(defun r5rs-forms (source)
  (if (stringp source)
      (let ((ps:*fold-case* t)) (read-scheme-forms source))
      (list (schemify source))))

(defun r5rs:eval (source)
  "Evaluate SOURCE (Scheme text, read case-folded, or a Lisp datum) as
R5RS."
  (r5rs-eval-forms (r5rs-forms source)))

(defun r5rs:load (path)
  "Load the R5RS source file PATH."
  (let ((ps:*fold-case* t))
    (r5rs-eval-forms (psl:read-forms-from-file path))))

(defun r5rs:expand (source)
  "SOURCE expanded to core Scheme by psyntax in R5RS's environment
(scheme-report-environment 5).  R5RS evaluation itself goes through
the translator's own expander; R5RS:TRANSLATE shows its result."
  (let ((forms (r5rs-forms source)))
    (ensure-psyntax)
    (values (psx:expand (if (cdr forms) (cons (ps:intern-scheme-symbol "begin") forms) (car forms))
			(psyntax-environment '("psyntax" "scheme-report-environment-5"))))))

(defun r5rs:translate (source)
  "The Lisp code SOURCE translates to in the R5RS user environment."
  (let ((forms (r5rs-forms source)))
    (scheme-translator:translate
     (if (cdr forms) (cons (ps:intern-scheme-symbol "begin") forms) (car forms))
     ps:scheme-user-environment)))

(defun r5rs:read-from-string (string)
  "The first datum in STRING, read by the Scheme reader, case-folded."
  (let ((ps:*fold-case* t))
    (funcall ps:*scheme-read* (make-string-input-stream string))))

;;; ------------------------------------------------------------------
;;; SCHEME and PROCEDURE

(defmacro r7rs:scheme (&rest forms)
  "Evaluate FORMS (unevaluated Lisp data, as Scheme) at the R7RS REPL top
level; return the last one's values, #f as NIL."
  `(lisp-values (multiple-value-list (r7rs-repl-eval (mapcar #'schemify ',forms)))))

(defmacro r6rs:scheme (&rest forms)
  "Evaluate FORMS (unevaluated Lisp data, as Scheme) at the R6RS REPL top
level; return the last one's values, #f as NIL."
  `(lisp-values (multiple-value-list (r6rs-repl-eval (mapcar #'schemify ',forms)))))

(defmacro r5rs:scheme (&rest forms)
  "Evaluate FORMS (unevaluated Lisp data, as Scheme) in the R5RS user
environment; return the last one's values, #f as NIL."
  `(lisp-values (multiple-value-list (r5rs-eval-forms (mapcar #'schemify ',forms)))))

(defun procedure (evaluator name library convert)
  (let* ((sym (if (stringp name) (ps:intern-scheme-symbol name) (schemify name)))
	 (proc (if library
		   (let ((b (find sym (pseudoscheme-interop:export-bindings
				       (pseudoscheme-interop:parse-library-name library))
				  :key #'first)))
		     (unless (and b (eq (second b) :procedure))
		       (error "~A exports no procedure ~A" library (ps:scheme-symbol-name sym)))
		     (third b))
		   (funcall evaluator (list sym)))))
    (unless (functionp proc)
      (error "~A is not a procedure" (ps:scheme-symbol-name sym)))
    (if convert (pseudoscheme-interop:lisp-facing proc) proc)))

(defun r7rs:procedure (name &key library (convert t))
  "The R7RS procedure NAME (a string, \"string-pad\", or a symbol) from
LIBRARY if given, else the REPL environment, as a Lisp function that
converts booleans (or the procedure itself if CONVERT is NIL)."
  (procedure #'r7rs-eval-forms name (and library (progn (ensure-psyntax) library)) convert))

(defun r6rs:procedure (name &key library (convert t))
  "The R6RS procedure NAME from LIBRARY if given, else the REPL
environment, as a Lisp function (see R7RS:PROCEDURE)."
  (procedure #'r6rs-eval-forms name (and library (progn (ensure-psyntax) library)) convert))

(defun r5rs:procedure (name &key (convert t))
  "The R5RS procedure NAME, as a Lisp function (see R7RS:PROCEDURE)."
  (procedure #'r5rs-eval-forms name nil convert))

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

(defun r7rs:repl ()
  "An R7RS read-eval-print loop (,q quits)."
  (ensure-psyntax)
  (repl-loop (lambda (form) (ps-r7rs::eval-at-repl form)) :prompt "r7rs> "))

(defun r6rs:repl ()
  "An R6RS read-eval-print loop (,q quits)."
  (ensure-psyntax)
  (repl-loop (lambda (form) (r6rs-eval-forms (list form))) :prompt "r6rs> "))

(defun r5rs:repl ()
  "An R5RS read-eval-print loop (,q quits)."
  (let ((ps:*fold-case* t))
    (repl-loop (lambda (form) (r5rs-eval-forms (list form))) :prompt "r5rs> ")))
