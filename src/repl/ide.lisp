;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-IDE -*-

;;;; What an editor needs from a running Pseudoscheme, for any of its
;;;; languages: evaluating text, completion, arglists, ,describe,
;;;; expansion, loading, and the REPL's meta-commands.  Plain functions
;;;; of strings returning strings and lists, so that SLIME's Swank and
;;;; SLY's Slynk can both call them as they are (their -eval functions
;;;; take any function in the image): contrib/emacs/pseudoscheme.el
;;;; calls them through whichever is connected.  The languages are the
;;;; REPL's (repl.lisp, languages.lisp, guile.lisp); a language is named
;;;; by a string, "r7rs", "chez", "guile", ...

(defpackage "PSEUDOSCHEME-IDE"
  (:use "COMMON-LISP")
  (:import-from "PSEUDOSCHEME-REPL" "LANGUAGE-NAME" "LANGUAGE-TITLE")
  (:export "LANGUAGES" "EVAL-STRING" "COMPLETIONS" "ARGLIST" "DESCRIBE-NAME"
	   "APROPOS-NAMES" "EXPAND-STRING" "LOAD-FILE" "COMMAND" "BACKTRACE"
	   "FORM-COMPLETE-P"))

(in-package "PSEUDOSCHEME-IDE")

(defun language (name)
  (or (pseudoscheme-repl:find-language name)
      (error "No language ~A (one of ~{~(~A~)~^, ~})" name
	     (mapcar #'language-name pseudoscheme-repl:*languages*))))

(defmacro with-language ((name) &body body)
  "Run BODY in language NAME, as the REPL would without a terminal, its
output captured: returns BODY's value and the output."
  (let ((out (gensym)))
    `(let* ((pseudoscheme-repl::*language* (language ,name))
	    (pseudoscheme-repl::*interactive* nil)
	    (,out (make-string-output-stream))
	    (*standard-output* ,out))
       (values (progn ,@body) (get-output-stream-string ,out)))))

(defun languages ()
  "((name title) ...) for each language."
  (mapcar (lambda (l) (list (string-downcase (language-name l)) (language-title l)))
	  pseudoscheme-repl:*languages*))

(defun value-strings (values)
  (loop for v in values
	unless (funcall (pseudoscheme-repl::language-unspecified-p pseudoscheme-repl::*language*) v)
	  collect (pseudoscheme-repl::value-text v)))

(defun eval-string (language text)
  "Evaluate TEXT's forms in LANGUAGE.  A plist: :values, the last form's
values as written; :output, what it printed; :error, the message of the
error that stopped it (whose backtrace BACKTRACE then shows), or NIL."
  (let ((result '()) (error nil))
    (multiple-value-bind (ignore output)
	(with-language (language)
	  (let ((l pseudoscheme-repl::*language*))
	    (handler-case
		(handler-bind ((serious-condition
				 (lambda (e)
				   (setf pseudoscheme-repl::*last-backtrace*
					 (pseudoscheme-repl::capture-backtrace)
					 pseudoscheme-repl::*last-error* e))))
		  (dolist (form (funcall (pseudoscheme-repl::language-read-forms l) text))
		    (setq result (value-strings
				  (multiple-value-list (funcall (pseudoscheme-repl::language-eval l) form))))))
	      (serious-condition (e)
		(setq error (handler-case
				(if (pseudoscheme-repl::language-error-text l)
				    (funcall (pseudoscheme-repl::language-error-text l) e)
				    (princ-to-string e))
			      (error () (format nil "~A" (type-of e)))))))))
      (declare (ignore ignore))
      (list :values result :output output :error error))))

(defun completions (language prefix)
  "The names bound in LANGUAGE's top level that begin with PREFIX, sorted."
  (sort (remove-duplicates (with-language (language) (pseudoscheme-repl::completions prefix))
			   :test #'string=)
	#'string<))

(defun arglist (language name)
  "\"(name parameter ...)\" for the procedure NAME is bound to, or NIL."
  (ignore-errors
   (with-language (language)
     (let* ((l pseudoscheme-repl::*language*)
	    (form (car (funcall (pseudoscheme-repl::language-read-forms l) name)))
	    (value (and (symbolp form)
			(funcall (pseudoscheme-repl::language-eval l) form))))
       (when (functionp value)
	 (let ((args (or (guile-arglist value)
			 (pseudoscheme-repl::procedure-arglist value))))
	   (format nil "(~A~@[ ~A~])" name (and args (> (length args) 0) args))))))))

(defun guile-arglist (f)
  "F's parameters from Guile's arglist property, if it has one."
  (let ((property (and psg::*booted* (psg::root-value "procedure-property"))))
    (when property
      (let ((a (ignore-errors (funcall property f (ps:intern-scheme-symbol "arglist")))))
	(when (and (listp a) (= (length a) 5))
	  (destructuring-bind (req opt keys allow rest) a
	    (declare (ignore allow))
	    (format nil "~{~A~^ ~}"
		    (append (loop repeat req collect "_")
			    (and (plusp opt) (cons "#:optional" (loop repeat opt collect "_")))
			    (and (consp keys)
				 (cons "#:key" (mapcar (lambda (k) (ps:scheme-symbol-name
								    (if (consp k) (car k) k)))
						       keys)))
			    (and rest (not (eq rest ps:false))
				 (list "." (if (symbolp rest) (ps:scheme-symbol-name rest) "rest")))))))))))

(defun command (language text)
  "Run the REPL meta-command TEXT (\",describe car\", \",trace f\") in
LANGUAGE: its output."
  (nth-value 1 (with-language (language)
		 (pseudoscheme-repl::run-command
		  (if (and (> (length text) 0) (char= (char text 0) #\,)) text
		      (concatenate 'string "," text))))))

(defun describe-name (language name)
  (command language (format nil ",describe ~A" name)))

(defun apropos-names (language string)
  "The names bound in LANGUAGE containing STRING, sorted."
  (sort (remove-if-not (lambda (n) (search string n :test #'char-equal))
		       (completions language ""))
	#'string<))

(defun expand-string (language text)
  "TEXT's first form with its macros expanded, written."
  (with-language (language)
    (let* ((l pseudoscheme-repl::*language*)
	   (form (car (funcall (pseudoscheme-repl::language-read-forms l) text))))
      (if (pseudoscheme-repl::language-expand l)
	  (pseudoscheme-repl::value-text
	   (pseudoscheme-repl::readable-datum (funcall (pseudoscheme-repl::language-expand l) form)))
	  "(this language can't expand macros)"))))

(defun load-file (language path)
  "Load PATH in LANGUAGE: a plist as EVAL-STRING returns."
  (let ((error nil))
    (multiple-value-bind (ignore output)
	(with-language (language)
	  (handler-case (funcall (pseudoscheme-repl::language-load pseudoscheme-repl::*language*) path)
	    (serious-condition (e) (setq error (princ-to-string e)))))
      (declare (ignore ignore))
      (list :values '() :output output :error error))))

(defun backtrace ()
  "The backtrace of the last error, as ,bt shows it."
  (if pseudoscheme-repl::*last-backtrace*
      (pseudoscheme-repl::readable-text
       (pseudoscheme-repl::trim-backtrace pseudoscheme-repl::*last-backtrace*))
      "No error yet."))

(defun form-complete-p (text)
  "Is TEXT complete: parentheses closed, no string or comment open?"
  (pseudoscheme-repl::input-complete-p text))
