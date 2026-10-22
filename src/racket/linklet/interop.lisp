; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Racket and Common Lisp, both ways (docs/interop.md, "Guile and Racket").
;;;;
;;;; Racket calling Lisp: the collection cl, whose module cl/PACKAGE is a
;;;; Lisp package's external symbols, written as a module the first time
;;;; it's looked for (ENSURE-LISP-MODULE: FILE-KIND asks):
;;;;
;;;;   (require (prefix-in cl: cl/common-lisp))
;;;;   (cl:mapcar add1 '(1 2 3))                      ; => '(2 3 4)
;;;;   (let ([n 10]) (cl:loop for x in '(1 2 3) collect (* x n)))
;;;;
;;;; A function is a procedure, its booleans crossing as the R7RS (cl ...)
;;;; libraries' do (src/interop.lisp); a variable is syntax for its value
;;;; (set! too); a macro or special operator is syntax whose expansion is
;;;; (quasischeme-lisp-call 'package 'macro 'form 'names lexical ...): the
;;;; call as data, compiled as Lisp when first run (a module's expansion
;;;; is serialized into its .zo, so it can't hold a Lisp function); any
;;;; other symbol (a type, a class) is a variable holding that symbol.
;;;;
;;;; Lisp calling Racket: the package QUASI-RACKET (QR), below, and the
;;;; ASDF system quasi-racket.

(in-package "QUASISCHEME-RACKET")

(defun lisp-module-package (name)
  (or (quasischeme-interop::lisp-package-for (list (qs:intern-scheme-symbol name)))
      (racket-error "cl" "no Lisp package for ~A" name)))

(defun lisp-argument (x)
  "A Racket keyword passed to Lisp as Lisp's (#:sha256 is :SHA256)."
  (if (rkeyword-p x)
      (intern (qs:invert-case (rkeyword-name x)) "KEYWORD")
      x))

(defnative "quasischeme-lisp-functions" (package-name names) ()
  (let ((package (find-package package-name)))
    (values-list
     (mapcar (lambda (name)
	       (let ((f (quasischeme-interop:scheme-facing
			 (fdefinition (find-symbol name package))
			 (quasischeme-interop:lisp-predicate-name-p name))))
		 (lambda (&rest args)
		   (apply f (mapcar #'lisp-argument args)))))
	     names))))

(defnative "quasischeme-lisp-symbols" (package-name names) (:leaf t)
  (let ((package (find-package package-name)))
    (values-list (mapcar (lambda (name) (find-symbol name package)) names))))

(defnative "quasischeme-lisp-symbol-value" (symbol) (:leaf t)
  (quasischeme-interop:to-scheme (symbol-value symbol)))

(defnative "quasischeme-lisp-set-symbol-value!" (symbol value) (:leaf t)
  (setf (symbol-value symbol) (quasischeme-interop:to-lisp value))
  *void*)

(defnative "quasischeme-lisp-call" (package-name macro form names &rest args) ()
  (apply (quasischeme-interop:cached-lisp-macro-function
	  form names (find-package package-name) (find-symbol macro package-name))
	 args))

(defvar *kernel-procedures* (make-hash-table :test 'equal))

(defun kernel-call (name &rest args)
  "The expander's procedure NAME applied to ARGS (the syntax procedures
the cl collection's transformers use)."
  (apply (or (gethash name *kernel-procedures*)
	     ;; the expander's own, from its module #%core (not the primitive
	     ;; instance #%kernel; and not evaluated in the current namespace,
	     ;; which while a transformer runs is the module's at phase 1)
	     (setf (gethash name *kernel-procedures*)
		   (expander-call "dynamic-require" (list (rsym "quote") (rsym "#%core")) (rsym name))))
	 args))

(defun syntax-list (stx)
  "The parts of STX, a syntax object of a list, as a Lisp list (an
improper list's tail a syntax object or NIL)."
  (let ((e (if (consp stx) stx (kernel-call "syntax-e" stx))))
    (if (consp e) e nil)))

(defnative "quasischeme-lisp-variable-transformers" (get set package-name names) ()
  ;; each name's transformer: x is (get 'symbol), (set! x v) (set 'symbol v)
  (let ((package (find-package package-name)))
    (values-list
     (mapcar (lambda (name)
	       (let ((symbol (find-symbol name package)))
		 (kernel-call
		  "make-set!-transformer"
		  (lambda (stx)
		    (let ((e (kernel-call "syntax-e" stx)))
		      (kernel-call
		       "datum->syntax" stx
		       (cond ((not (consp e)) (list get (list (rsym "quote") symbol)))
			     ((eq (kernel-call "syntax-e" (car e)) (rsym "set!"))
			      (list set (list (rsym "quote") symbol) (third (syntax-list stx))))
			     (t (cons (list get (list (rsym "quote") symbol)) (cdr e))))))))))
	     names))))

(defun lexical-identifiers (stx)
  "The identifiers in STX that are bound lexically, one of each."
  (let ((found '()))
    (labels ((walk (x)
	       (cond ((consp x) (walk (car x)) (walk (cdr x)))
		     ((null x))
		     ((kernel-call "syntax?" x)
		      (let ((e (kernel-call "syntax-e" x)))
			(cond ((symbolp e)
			       (when (and (eq (kernel-call "identifier-binding" x) (rsym "lexical"))
					  (not (find-if (lambda (y) (truthy (kernel-call "bound-identifier=?" x y)))
							found)))
				 (push x found)))
			      ((consp e) (walk e))
			      ((simple-vector-p e) (map nil #'walk e))))))))
      (walk stx))
    (nreverse found)))

(defnative "quasischeme-lisp-macro-transformers" (call package-name names) ()
  (values-list
   (mapcar (lambda (name)
	     (lambda (stx)
	       (let ((ids (lexical-identifiers stx))
		     (q (rsym "quote")))
		 (kernel-call
		  "datum->syntax" stx
		  (list* call (list q package-name) (list q name)
			 (list q (kernel-call "syntax->datum" stx))
			 (list q (mapcar (lambda (id) (kernel-call "syntax-e" id)) ids))
			 ids)))))
	   names)))

;;; The modules

(defvar *lisp-collection-directory* nil)
(pushnew (lambda () (setq *lisp-collection-directory* nil)) sb-ext:*init-hooks*)

(defun lisp-collection-directory ()
  "The collection cl's directory, in the Racket cache (a native namestring)."
  (or *lisp-collection-directory*
      (setq *lisp-collection-directory*
	    (namestring (merge-pathnames "lisp-collection/cl/" (racket-cache-directory))))))

(defun racket-name (symbol)
  (qs:invert-case (symbol-name symbol)))

(defun lisp-module-text (module-name package)
  "The Racket module cl/MODULE-NAME: PACKAGE's external symbols."
  (let ((functions '()) (variables '()) (macros '()) (others '()))
    (do-external-symbols (s package)
      ;; (a name that can't be written between bars is left out)
      (unless (or (find #\| (symbol-name s)) (find #\\ (symbol-name s)))
	(cond ((quasischeme-interop::exportable-function-p s) (push s functions))
	      ((and (fboundp s) (or (macro-function s) (special-operator-p s))) (push s macros))
	      ((boundp s) (push s variables))
	      (t (push s others)))))
    (flet ((sorted (l) (sort l #'string< :key #'symbol-name)))
      (setq functions (sorted functions) variables (sorted variables)
	    macros (sorted macros) others (sorted others)))
    (let ((p (package-name package)))
      (flet ((ids (l) (format nil "~{|~A|~^ ~}" (mapcar #'racket-name l)))
	     (names (l) (format nil "~{~S~^ ~}" (mapcar #'symbol-name l))))
	;; the kernel's quote is %quote: the package may define quote
	(format nil "(module ~A '#%kernel
  (#%require (for-syntax '#%kernel) (rename '#%kernel %quote quote))
  (#%provide ~A)
  (define-values (~A) (quasischeme-lisp-functions ~S (%quote (~A))))
  (define-values (~A) (quasischeme-lisp-symbols ~S (%quote (~A))))
  (define-syntaxes (~A)
    (quasischeme-lisp-variable-transformers (quote-syntax quasischeme-lisp-symbol-value)
                                            (quote-syntax quasischeme-lisp-set-symbol-value!)
                                            ~S (quote (~A))))
  (define-syntaxes (~A)
    (quasischeme-lisp-macro-transformers (quote-syntax quasischeme-lisp-call) ~S (quote (~A)))))
"
		module-name (ids (append functions others variables macros))
		(ids functions) p (names functions)
		(ids others) p (names others)
		(ids variables) p (names variables)
		(ids macros) p (names macros))))))

(defun ensure-lisp-module (path)
  "If PATH (a native path string) is a module file of the collection cl,
write it, from its package, unless it is written already as it should
be.  (By the part after lisp-collection/cl/: Racket's paths are
normalized, the cache directory's namestring may not be.)"
  (let* ((marker "/lisp-collection/cl/")
	 (at (search marker path))
	 (start (and at (+ at (length marker)))))
    (when (and start (uiop:string-suffix-p path ".rkt") (not (find #\/ path :start start)))
      (let* ((name (subseq path start (- (length path) 4)))
	     (package (ignore-errors (lisp-module-package name))))
	(when package
	  (let ((text (lisp-module-text name package)))
	    (unless (and (probe-file path) (string= (uiop:read-file-string path) text))
	      (ensure-directories-exist path)
	      (with-open-file (out path :direction :output :if-exists :supersede)
		(write-string text out)))))))))

;;; ------------------------------------------------------------------
;;; Lisp calling Racket: QUASI-RACKET
;;;
;;;   (qr:eval "(string-append \"a\" \"b\")")                   ; => "ab"
;;;   (qr:require racket/list (only racket/string string-join))
;;;   (first '(1 2 3))                ; racket/list's (a clash with CL's: use :prefix)
;;;   (qr:require (prefix-in r- racket/match))
;;;   (r-match '(1 2) ((list a b) (+ a b)))              ; a Racket macro

(defpackage "QUASI-RACKET"
  (:nicknames "QR")
  (:use "COMMON-LISP")
  (:shadow "EVAL" "REQUIRE")
  (:export "EVAL" "REQUIRE" "PROCEDURE" "MODULE-EXPORTS" "FALSE" "TRUE"))

(in-package "QUASI-RACKET")

(defconstant false 'qs::false "Racket's #f.")
(defconstant true t "Racket's #t (which is T).")

(defun racket-datum (x)
  "A Lisp datum, Racket code or data written in Lisp, as Racket's (FOO is
foo, NIL '(), QR:FALSE and QR:TRUE #f and #t)."
  (cond ((eq x 'false) qs:false)
	((eq x 'true) t)
	(t (quasischeme-interop:schemify x))))

(defun call-in-racket (thunk)
  "THUNK's values, run as Racket code runs (the expander booted, under the
base of continuations and the default prompt); an uncaught Racket
exception, which Racket reports, is a Lisp error here."
  (qsr::boot)
  (qsr::boot-expander)
  (let ((values '()))
    (unless (qsr::call-at-racket-base (lambda () (setq values (multiple-value-list (funcall thunk)))))
      (error "Racket: an exception went uncaught (reported above)"))
    (values-list values)))

(defun call-with-namespace (ns thunk)
  "THUNK with Racket's current-namespace NS."
  (let* ((parameter (qsr::expander-export "current-namespace"))
	 (old (funcall parameter)))
    (funcall parameter ns)
    (unwind-protect (funcall thunk)
      (funcall parameter old))))

(defvar *namespaces* (make-hash-table :test 'equal)
  "Language (a module path datum) -> a namespace with it required.")

(defun namespace-for (language)
  (let ((key (racket-datum language)))
    (or (gethash key *namespaces*)
	(setf (gethash key *namespaces*)
	      (call-in-racket
	       (lambda ()
		 (let ((ns (funcall (qsr::expander-call "dynamic-require" (qsr::rsym "racket/base")
							(qsr::rsym "make-base-empty-namespace")))))
		   ;; racket/base too, for the code a macro's call is put in
		   (call-with-namespace ns (lambda ()
					     (qsr::expander-call "namespace-require" (qsr::rsym "racket/base"))
					     (qsr::expander-call "namespace-require" key)))
		   ns)))))))

(defun eval-in (datum language)
  (let ((ns (namespace-for language)))
    (call-in-racket (lambda () (qsr::expander-call "eval" datum ns)))))

(defun eval (source &key (language 'racket/base))
  "SOURCE, Racket text (every form, the last one's values) or a datum
written in Lisp (RACKET-DATUM), evaluated in a namespace with LANGUAGE (a
module path, racket/base by default).  Values come back as they are."
  (if (stringp source)
      (let ((values (list qsr::*void*)))
	(with-input-from-string (in source)
	  (loop for form = (qsr::read-racket in)
		until (eq form qsr::*eof*)
		do (setq values (multiple-value-list (eval-in form language)))))
	(values-list values))
      (eval-in (racket-datum source) language)))

(defun procedure (name &key (module 'racket/base) (convert t))
  "Racket's procedure NAME (a string) exported by MODULE, as a Lisp function."
  (let ((value (call-in-racket
		(lambda () (qsr::expander-call "dynamic-require" (racket-datum module) (qs:intern-scheme-symbol name))))))
    (if convert (quasischeme-interop:lisp-facing value) value)))

(defun module-export-names (module)
  "MODULE's phase-0 exports: (values variable-names syntax-names), symbols."
  (call-in-racket
   (lambda ()
     (let ((m (racket-datum module)))
       (qsr::expander-call "module-declared?" m t)
       (multiple-value-bind (variables syntax) (funcall (qsr::expander-call "dynamic-require" (qsr::rsym "racket/base") (qsr::rsym "module->exports")) m)
	 (flet ((phase-0 (l) (loop for (phase . exports) in l
				   when (eql phase 0) append (mapcar #'car exports))))
	   (values (phase-0 variables) (phase-0 syntax))))))))

(defun module-exports (module)
  "MODULE's phase-0 exports, as Racket spells them (strings)."
  (multiple-value-bind (variables syntax) (module-export-names module)
    (sort (mapcar (lambda (s) (qs:scheme-symbol-name s)) (append variables syntax)) #'string<)))

;;; A Racket macro brought into Lisp is a Lisp macro.  Its arguments are
;;; Racket code written in Lisp syntax, with Lisp's lexical variables and
;;; functions passed in as arguments of (lambda (p ...) <the call>),
;;; which Racket evaluates once, when the Lisp code is loaded, in a
;;; namespace with the macro's module required (as for Scheme's and
;;; Guile's macros: docs/interop.md 4.2).

(defun macro-call-expansion (form env module export)
  (let ((params '()) (args '()))
    (labels ((param (key value)
	       (or (car (find key params :key #'cdr :test #'equal))
		   (let ((p (qs:intern-scheme-symbol (format nil "%%lisp-~D" (length params)))))
		     (push (cons p key) params)
		     (push value args)
		     p)))
	     (convert (x)
	       (typecase x
		 (null nil)
		 ((eql t) t)
		 (keyword x)
		 (symbol
		  (cond ((eq x 'false) qs:false)
			((eq x 'true) t)
			((quasischeme-interop::lexical-variable-p x env)
			 (param x `(quasischeme-interop:to-scheme ,x)))
			((and (or (quasischeme-interop::local-function-p x env)
				  (and (fboundp x) (not (macro-function x)) (not (special-operator-p x))))
			      (not (eq (symbol-package x) (find-package "COMMON-LISP"))))
			 (param (list 'function x) `(quasischeme-interop:scheme-facing #',x)))
			(t (racket-datum x))))
		 (cons
		  (cond ((eq (car x) 'quote) (list (qs:intern-scheme-symbol "quote") (racket-datum (cadr x))))
			((and (eq (car x) 'function) (symbolp (cadr x)))
			 (param (list 'function (cadr x)) `(quasischeme-interop:scheme-facing #',(cadr x) nil)))
			(t (cons (convert (car x)) (convert-args (cdr x))))))
		 (t x)))
	     (convert-args (x)
	       (cond ((null x) nil)
		     ((consp x) (cons (convert (car x)) (convert-args (cdr x))))
		     (t (convert x)))))
      (let* ((call (cons export (convert-args (cdr form))))
	     (lambda-form (list (qs:intern-scheme-symbol "lambda") (mapcar #'car (reverse params)) call)))
	`(multiple-value-call #'quasischeme-interop::values-to-lisp
	   (funcall (load-time-value (racket-lambda ',lambda-form ',module)) ,@(reverse args)))))))

(defun racket-lambda (lambda-form module)
  "The procedure LAMBDA-FORM (Racket code) evaluates to, in a namespace
with MODULE required."
  (eval-in lambda-form module))

(defun import-modules (specs package-name)
  (let ((package (find-package package-name)))
    (dolist (spec specs)
      ;; (only m name ...), (prefix-in p m), (rename-in m (from to) ...), m
      (let ((module spec) (only nil) (prefix nil) (renames '()))
	(loop while (and (consp module) (member (symbol-name (car module)) '("ONLY" "ONLY-IN" "PREFIX-IN" "RENAME-IN") :test #'string=))
	      do (let ((head (symbol-name (car module))))
		   (cond ((member head '("ONLY" "ONLY-IN") :test #'string=)
			  (setq only (mapcar #'symbol-name (cddr module)) module (cadr module)))
			 ((string= head "PREFIX-IN")
			  (setq prefix (string (cadr module)) module (caddr module)))
			 (t (setq renames (mapcar (lambda (r) (cons (symbol-name (car r)) (symbol-name (cadr r))))
						  (cddr module))
				  module (cadr module))))))
	(multiple-value-bind (variables syntax) (module-export-names module)
	  (flet ((bind (export kind)
		   (let ((name (symbol-name export)))
		     (when (or (null only) (member name only :test #'string=))
		       (let* ((name (or (cdr (assoc name renames :test #'string=)) name))
			      (symbol (intern (if prefix (concatenate 'string prefix name) name) package)))
			 (when (and (not (eq (symbol-package symbol) package))
				    (or (fboundp symbol) (boundp symbol)))
			   (error "qr:require: ~A would redefine ~S; use only, prefix-in or rename-in" name symbol))
			 (if (and (eq kind :syntax) (not (keyword-procedure-p module export)))
			     (setf (macro-function symbol)
				   (lambda (form env) (macro-call-expansion form env module export)))
			     (let ((value (call-in-racket
					   (lambda () (qsr::expander-call "dynamic-require" (racket-datum module) export)))))
			       (if (functionp value)
				   (setf (fdefinition symbol) (quasischeme-interop:lisp-facing value))
				   (cl:eval `(define-symbol-macro ,symbol
						 (quasischeme-interop:to-lisp
						  (call-in-racket
						   (lambda () (qsr::expander-call "dynamic-require" ',(racket-datum module) ',export))))))))))))))
	    (dolist (v variables) (bind v :variable))
	    (dolist (s syntax) (bind s :syntax))))))))

(defun keyword-procedure-p (module export)
  "Whether EXPORT, syntax MODULE exports, is a procedure all the same: a
procedure taking keywords is exported as syntax, which dynamic-require
gives the procedure of."
  (functionp (eval-in (racket-datum `(with-handlers ((void (lambda (e) ,qs:false)))
				       (dynamic-require ',module ',export)))
		      module)))

(defmacro require (&rest specs)
  "Bring Racket modules' exports into the current package, with require's
syntax written in Lisp: racket/list, (only-in racket/string string-join),
(prefix-in r- racket/match), (rename-in racket/list (first r-first)).
Procedures become functions, variables symbol macros, syntax Lisp macros
(whose arguments are Racket code, with Lisp's lexical variables and
functions)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (import-modules ',specs ,(package-name *package*))))
