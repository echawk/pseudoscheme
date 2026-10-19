;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-REPL -*-

;;;; The languages of the REPL: R7RS, R6RS, R5RS, the older reports and
;;;; Chez Scheme, each at a psyntax top level of its own (an interaction
;;;; library whose source library has the language's bindings); Guile's
;;;; is in guile.lisp.

(in-package "QUASISCHEME-REPL")

(defun ssyms (names) (mapcar #'qs:intern-scheme-symbol names))

(defmacro with-top-level ((interaction source) &body body)
  "Run BODY with psyntax's top level that of INTERACTION, whose bindings
come from SOURCE (lists of strings)."
  `(progn
     (quasischeme-api::ensure-psyntax)
     (qs-r7rs::with-psyntax-parameter ("psyntax:interaction-library-name" (ssyms ,interaction))
       (qs-r7rs::with-psyntax-parameter ("psyntax:interaction-source-name" (ssyms ,source))
	 ,@body))))

(defun psyntax (name &rest args)
  (apply (qsx:host-ref (concatenate 'string "psyntax:" name)) args))

(defun top-level-names (interaction source)
  (with-top-level (interaction source)
    (mapcar #'qs:scheme-symbol-name (psyntax "interaction-symbols"))))

(defun parameter-name (x)
  "X's name as a parameter, or _ when it's one the compiler made up
(psyntax renames a lambda's parameters)."
  (let ((name (if (symbolp x) (qs:scheme-symbol-name x) (princ-to-string x))))
    (if (or (find-if (lambda (c) (find c "%$.~")) name)
	    (and (> (length name) 0) (digit-char-p (char name (1- (length name))))
		 (search "-v" name)))
	"_"
	name)))

(defun procedure-arglist (f)
  "F's parameters as a string, if the Lisp knows them."
  #+sbcl (let ((list (ignore-errors (sb-introspect:function-lambda-list f))))
	   (and (listp list)
		(format nil "~{~A~^ ~}"
			(loop for x in list
			      collect (cond ((member x lambda-list-keywords)
					     (case x (&optional "#!optional") (&rest ".") (t (string-downcase x))))
					    ((consp x) (parameter-name (car x)))
					    (t (parameter-name x)))))))
  #-sbcl (progn f nil))

(defun describe-value (name value stream)
  (cond ((functionp value)
	 (let ((args (procedure-arglist value)))
	   (format stream "~A is a procedure~@[: (~A~@[ ~A~])~]~%" name (and args name)
		   (and args (> (length args) 0) args))))
	(t (format stream "~A is a variable, whose value is ~A~%" name
		   (quasischeme-api:write-to-string value)))))

(defun describe-top-level (interaction source)
  (lambda (name stream)
    (let* ((sym (qs:intern-scheme-symbol (string-trim " " name)))
	   (b (with-top-level (interaction source) (psyntax "top-level-binding" sym))))
      (cond ((eq b qs:false) (format stream "~A is not bound here~%" name))
	    ((and (symbolp b) (string-equal (qs:scheme-symbol-name b) "variable"))
	     (format stream "~A is a top-level variable, not yet defined~%" name))
	    ((not (consp b)) (format stream "~A is bound: ~S~%" name b))
	    (t
	     (let ((type (qs:scheme-symbol-name (car b))))
	       (cond ((string= type "global")
		      (let ((cell (and (symbolp (cddr b)) (qsx::location (cddr b)))))
			(if (and cell (boundp cell))
			    (describe-value name (symbol-value cell) stream)
			    (format stream "~A is a variable, not yet assigned~%" name))))
		     ((string= type "core-prim")
		      (let ((value (ignore-errors (qsx:host-ref (qs:scheme-symbol-name (cdr b))))))
			(if value
			    (describe-value name value stream)
			    (format stream "~A is a primitive~%" name))))
		     ((member type '("global-macro" "global-macro!" "macro" "macro!") :test #'string=)
		      (format stream "~A is syntax: a macro~%" name))
		     ((member type '("core-macro" "$core-rtd") :test #'string=)
		      (format stream "~A is syntax, built in~%" name))
		     ((string= type "$rtd") (format stream "~A is a record type~%" name))
		     ((string= type "$module") (format stream "~A is a module~%" name))
		     (t (format stream "~A is syntax (~A)~%" name type)))))))))

(defun top-level-cell (interaction source)
  "A function of a name: the symbol whose value is the top-level
variable's (psyntax's location), or NIL if it isn't a variable."
  (lambda (name)
    (let ((b (with-top-level (interaction source)
	       (psyntax "top-level-binding" (qs:intern-scheme-symbol name)))))
      (and (consp b)
	   (string= (qs:scheme-symbol-name (car b)) "global")
	   (symbolp (cddr b))
	   (qsx::location (cddr b))))))

(defun expand-top-level (interaction source)
  (lambda (form)
    (with-top-level (interaction source) (psyntax "expand-top-level" form))))

(defun read-forms (&key fold-case chez)
  (lambda (text)
    (let ((qs:*fold-case* fold-case)
	  (qs:*chez-reader* chez))
      (quasischeme-api:read-scheme-forms text))))

(defun unspecific-p (v) (eq v qs:unspecific))

(defmacro define-psyntax-language (name title prompt interaction source &rest keys)
  `(define-language ,name
     ,@keys				; first, so that a key given here wins
     :title ,title :prompt ,prompt
     :names (lambda () (top-level-names ',interaction ',source))
     :describe (describe-top-level ',interaction ',source)
     :expand (expand-top-level ',interaction ',source)
     :cell (top-level-cell ',interaction ',source)
     :error-text #'quasischeme-api::error-message
     :unspecified-p #'unspecific-p))

(define-psyntax-language :r7rs "R7RS-small" "r7rs> "
  ("quasischeme" "r7rs" "interaction") ("quasischeme" "r7rs")
  :read-forms (read-forms)
  :eval (lambda (form) (quasischeme-api::ensure-psyntax) (qs-r7rs::eval-at-repl form))
  :load (lambda (path) (quasischeme-api::ensure-psyntax) (qs-r7rs::load-file-at-repl path)))

(define-psyntax-language :r6rs "R6RS" "r6rs> "
  ("quasischeme" "interaction") ("quasischeme")
  :read-forms (read-forms)
  :eval (lambda (form)
	  (with-top-level ('("quasischeme" "interaction") '("quasischeme"))
	    (quasischeme-api::r6rs-eval-forms (list form))))
  :load (lambda (path) (r6rs:load path)))

(define-psyntax-language :r5rs "R5RS" "r5rs> "
  ("quasischeme" "r5rs" "interaction") ("quasischeme" "r5rs")
  :read-forms (read-forms :fold-case t)
  :eval (lambda (form) (quasischeme-api::ensure-psyntax) (qs-r7rs::eval-at-r5rs-repl form))
  :load (lambda (path) (r5rs:load path)))

(define-psyntax-language :chez "Chez Scheme 10" "> "
  ("quasischeme" "chez" "interaction") ("chezscheme")
  :read-forms (read-forms :chez t)
  :eval (lambda (form) (quasischeme-api::ensure-psyntax) (qs-r7rs::eval-at-chez-top-level form))
  :error-text #'qs-r7rs::chez-error-text
  :load (lambda (path)
	  (quasischeme-api::ensure-psyntax)
	  (let ((qs:*chez-reader* t)) (qs-r7rs::load-file-at-chez-top-level path))))

(macrolet ((reports ()
	     `(progn
		,@(loop for (report title) in '((:r4rs "R4RS") (:ieee "IEEE 1178-1990")
						(:r3rs "R3RS") (:r2rs "R2RS")
						(:rrs "the Revised Report"))
			collect
			`(define-psyntax-language ,report ,title ,(format nil "~(~A~)> " report)
			   ,(qs-r7rs::report-interaction-library report)
			   ,(qs-r7rs::report-library-name report)
			   :read-forms (read-forms :fold-case t)
			   :eval (lambda (form)
				   (quasischeme-api::ensure-psyntax)
				   (qs-r7rs::eval-at-report-top-level ,report form))
			   :load (lambda (path)
				   (quasischeme-api::ensure-psyntax)
				   (qs-r7rs::load-file-at-report-top-level ,report path)))))))
  (reports))
