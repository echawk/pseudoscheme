; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-GUILE -*-

;;;; Guile and Common Lisp, both ways (docs/interop.md, "Guile and Racket").
;;;;
;;;; Guile calling Lisp: the modules (cl PACKAGE), made when first
;;;; resolved (the module (cl)'s submodule binder), are a Lisp package's
;;;; external symbols:
;;;;
;;;;   (use-modules ((cl common-lisp) #:prefix cl:))
;;;;   (cl:mapcar 1+ '(1 2 3))                        ; => (2 3 4)
;;;;   (cl:loop for x in '(1 2 3) collect (* x x))    ; a Lisp macro
;;;;
;;;; A function is a procedure (its booleans crossing as the R7RS (cl ...)
;;;; libraries' do: src/interop.lisp, TO-LISP and SCHEME-FACING); a
;;;; variable is syntax for its value (and set! of it); a macro or special
;;;; operator is syntax that compiles its call as Lisp, with the Guile
;;;; lexical variables it mentions passed in; any other symbol (a type,
;;;; a class) is a variable holding that Lisp symbol.
;;;;
;;;; Lisp calling Guile: the package QUASI-GUILE (QG), below, and the
;;;; ASDF system quasi-guile.

(in-package "QUASISCHEME-GUILE")

(defun lisp-package-for-module (name)
  "The Lisp package the module (cl NAME) is, NAME a Guile symbol."
  (quasischeme-interop::lisp-package-for (list name)))

(defguile "%lisp-macro-function" (form names package-name macro)
  (quasischeme-interop:lisp-macro-function form names (lisp-package-for-module package-name) macro))

(defguile "%lisp-symbol-value" (symbol)
  (quasischeme-interop:to-scheme (symbol-value symbol)))

(defguile "%lisp-set-symbol-value!" (symbol value)
  (setf (symbol-value symbol) (quasischeme-interop:to-lisp value))
  *unspecified*)

(defguile "%lisp-bindings" (package-name)
  "The bindings of the module (cl PACKAGE-NAME): (name . variable) for
each of the package's external symbols (%LISP-BINDING)."
  (let ((package (lisp-package-for-module package-name))
	(bindings '()))
    (do-external-symbols (s package)
      (let ((name (qs:intern-scheme-symbol (qs:invert-case (symbol-name s)))))
	(push (cons name (lisp-binding package-name name)) bindings)))
    bindings))

(defun lisp-binding (package-name name)
  "The variable for NAME (a Guile symbol) of the module (cl PACKAGE-NAME):
its Lisp external symbol as a procedure, syntax or itself; or #f."
  (let ((package (lisp-package-for-module package-name)))
    (multiple-value-bind (symbol status) (find-symbol (symbol-name name) package)
      (if (not (eq status :external))
	  qs:false
	  (make-gvariable
	   (cond ((quasischeme-interop::exportable-function-p symbol)
		  (quasischeme-interop:scheme-facing
		   (fdefinition symbol) (quasischeme-interop::lisp-predicate-name-p (symbol-name symbol))))
		 ((and (fboundp symbol) (or (macro-function symbol) (special-operator-p symbol)))
		  (make-gmacro name (ssym "macro")
			       (funcall (root-value "%lisp-macro-transformer") package-name symbol)))
		 ((boundp symbol)
		  (make-gmacro name (ssym "macro")
			       (funcall (root-value "%lisp-variable-transformer") symbol)))
		 (t symbol)))))))

(defparameter *cl-modules-source* "
(define (%lisp-macro-transformer package macro)
  (lambda (stx)
    (let ((ids '()))
      (let walk ((x stx))
        (syntax-case x ()
          ((a . b) (begin (walk #'a) (walk #'b)))
          (_ (if (and (identifier? x)
                      (eq? 'lexical (call-with-values (lambda () ((@ (system syntax) syntax-local-binding) x))
                                      (lambda (type value) type)))
                      (not (or-map (lambda (y) (bound-identifier=? x y)) ids)))
                 (set! ids (cons x ids))))))
      (let ((ids (reverse ids)))
        #`('#,(datum->syntax stx (%lisp-macro-function (syntax->datum stx) (map syntax->datum ids) package macro))
           #,@ids)))))

(define (%lisp-variable-transformer symbol)
  (make-variable-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       ((set! _ v) #`((@ (guile) %lisp-set-symbol-value!) '#,(datum->syntax stx symbol) v))
       ((_ . args) #`(((@ (guile) %lisp-symbol-value) '#,(datum->syntax stx symbol)) . args))
       (_ #`((@ (guile) %lisp-symbol-value) '#,(datum->syntax stx symbol)))))))

(let ((cl (make-module)))
  (set-module-kind! cl 'directory)
  (set-module-name! cl '(cl))
  (set-module-submodule-binder!
   cl
   (lambda (parent name)
     ;; all of its bindings in its obarray: interfaces with #:select or
     ;; #:prefix are made from that
     (let ((m (make-module)))
       (for-each (lambda (b) (module-add! m (car b) (cdr b))) (%lisp-bindings name))
       (set-module-name! m (list 'cl name))
       (set-module-kind! m 'interface)
       (set-module-public-interface! m m)
       (module-define-submodule! parent name m)
       m)))
  (nested-define-module! (resolve-module '() #f) '(cl) cl))
"
  "Guile code, evaluated in (guile) as Guile boots: the helpers above, and
the module (cl), whose submodules are made by its submodule binder.")

(defun install-cl-modules ()
  "Install the module (cl) and its helpers (BOOT calls this)."
  (let ((*current-module* (root-value "the-root-module")))
    (with-input-from-string (in *cl-modules-source*)
      (loop for form = (guile-read in)
	    until (eq form qs:eof-object)
	    do (call-with-guile-catch (lambda () (primitive-eval form)))))))

;;; ------------------------------------------------------------------
;;; Lisp calling Guile: QUASI-GUILE
;;;
;;;   (qg:eval "(use-modules (ice-9 match)) (match '(1 2) ((a b) (+ a b)))")  ; => 3
;;;   (qg:use-modules ((srfi srfi-1) :select (fold iota)) (ice-9 match))
;;;   (fold #'+ 0 (iota 4))                      ; => 6
;;;   (match '(1 2) ((a b) (+ a b)))             ; a Guile macro, as a Lisp macro
;;;   (funcall (qg:procedure "string-join") '("a" "b") "-")   ; => "a-b"

(defpackage "QUASI-GUILE"
  (:nicknames "QG")
  (:use "COMMON-LISP")
  (:shadow "EVAL" "LOAD")
  (:export "EVAL" "LOAD" "PROCEDURE" "USE-MODULES" "MODULE-EXPORTS" "FALSE" "TRUE"))

(in-package "QUASI-GUILE")

(defconstant false 'qs::false "Guile's #f.")
(defconstant true t "Guile's #t (which is T).")

(defun guile-datum (x)
  "A Lisp datum, Guile code or data written in Lisp, as Guile's: symbols
Guile's (FOO is foo), NIL the empty list, keywords Guile's #:keywords,
QG:FALSE and QG:TRUE #f and #t."
  (cond ((eq x 'false) qs:false)
	((eq x 'true) t)
	(t (quasischeme-interop:schemify x))))

(defun module-named (name)
  "The module NAME (a list, Lisp or Guile symbols), or (guile-user)."
  (qsg::boot)
  (if name
      (qsg::call-with-guile-catch
       (lambda () (funcall (qsg::root-value "resolve-module") (guile-datum name))))
      (funcall (qsg::root-value "resolve-module") (guile-datum '(guile-user)))))

(defun eval (source &key module)
  "SOURCE, Guile text (every form, the last one's values) or a datum
written in Lisp (GUILE-DATUM), evaluated in MODULE (a module name;
(guile-user) by default).  Values come back as they are (#f is FALSE)."
  (let ((qsg::*current-module* (module-named module)))
    (if (stringp source)
	(with-input-from-string (in source)
	  (let ((values (list qsg::*unspecified*)))
	    (loop for form = (qsg:guile-read in)
		  until (eq form qs:eof-object)
		  do (setq values (multiple-value-list
				   (qsg::call-with-guile-catch (lambda () (qsg::primitive-eval form))))))
	    (values-list values)))
	(qsg::call-with-guile-catch (lambda () (qsg::primitive-eval (guile-datum source)))))))

(defun load (path)
  "Load the Guile source file PATH in (guile-user)."
  (let ((qsg::*current-module* (module-named nil)))
    (qsg:load-file path)))

(defun interface-bindings (module)
  "The public bindings of MODULE (a name): ((guile-symbol . value) ...)."
  (let ((iface (qsg::call-with-guile-catch
		(lambda () (funcall (qsg::root-value "resolve-interface") (guile-datum module)))))
	(bindings '()))
    (funcall (qsg::root-value "module-for-each")
	     (lambda (symbol variable)
	       (push (cons symbol (qsg::gvariable-value variable)) bindings))
	     iface)
    (sort bindings #'string< :key (lambda (b) (symbol-name (car b))))))

(defun module-exports (module)
  "The names MODULE exports, as Lisp symbol names (strings)."
  (mapcar (lambda (b) (symbol-name (car b))) (interface-bindings module)))

(defun procedure (name &key module (convert t))
  "Guile's procedure NAME (a string, as Guile spells it) in MODULE (a
name; (guile-user) by default), as a Lisp function; CONVERT: booleans and
procedures cross as everywhere (#f is NIL)."
  (let ((value (let ((qsg::*current-module* (module-named module)))
		 (qsg::call-with-guile-catch
		  (lambda () (qsg::primitive-eval (qs:intern-scheme-symbol name)))))))
    (if convert (quasischeme-interop:lisp-facing value) value)))

;;; A Guile macro brought into Lisp is a Lisp macro.  Its arguments are
;;; Guile code written in Lisp syntax, with Lisp's meanings where they
;;; exist, as for Scheme's (docs/interop.md 4.2): a Lisp lexical
;;; variable, or a Lisp function name, is passed in as an argument of
;;; (lambda (p ...) <the call>), which Guile evaluates once, when the
;;; Lisp code is loaded; other symbols are Guile's.

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
			(t (guile-datum x))))
		 (cons
		  (cond ((eq (car x) 'quote) (list (qs:intern-scheme-symbol "quote") (guile-datum (cadr x))))
			((and (eq (car x) 'function) (symbolp (cadr x)))
			 (param (list 'function (cadr x)) `(quasischeme-interop:scheme-facing #',(cadr x) nil)))
			(t (cons (convert (car x)) (convert-args (cdr x))))))
		 (t x)))
	     (convert-args (x)
	       (cond ((null x) nil)
		     ((consp x) (cons (convert (car x)) (convert-args (cdr x))))
		     (t (convert x)))))
      (let* ((call (cons (list (qs:intern-scheme-symbol "@") (guile-datum module) export)
			 (convert-args (cdr form))))
	     (lambda-form (list (qs:intern-scheme-symbol "lambda") (mapcar #'car (reverse params)) call)))
	`(multiple-value-call #'quasischeme-interop::values-to-lisp
	   (funcall (load-time-value (guile-lambda ',lambda-form)) ,@(reverse args)))))))

(defun guile-lambda (lambda-form)
  "The procedure LAMBDA-FORM (Guile code) evaluates to, in a fresh user module."
  (qsg::boot)
  (let ((qsg::*current-module* (funcall (qsg::root-value "make-fresh-user-module"))))
    (qsg::call-with-guile-catch (lambda () (qsg::primitive-eval lambda-form)))))

(defun import-binding (symbol module export value)
  "Give Lisp SYMBOL Guile's binding EXPORT of MODULE, whose value is VALUE."
  (cond ((qsg::gmacro-p value)
	 (setf (macro-function symbol)
	       (lambda (form env) (macro-call-expansion form env module export))))
	((functionp value)
	 (setf (fdefinition symbol) (quasischeme-interop:lisp-facing value)))
	(t (cl:eval `(define-symbol-macro ,symbol
			 (quasischeme-interop:to-lisp
			  (qsg::gvariable-value (funcall (qsg::root-value "module-variable")
							 (funcall (qsg::root-value "resolve-interface") ',(guile-datum module))
							 ',export))))))))

(defun parse-module-spec (spec)
  "A USE-MODULES spec: NAME, or (NAME :select (sym ...) :prefix p- :hide (...))."
  (if (and (consp spec) (consp (car spec)))
      (values (car spec) (cdr spec))
      (values spec '())))

(defun import-modules (specs package-name)
  (let ((package (find-package package-name)))
    (dolist (spec specs)
      (multiple-value-bind (name options) (parse-module-spec spec)
	(let ((select (getf options :select :all))
	      (hide (getf options :hide))
	      (prefix (getf options :prefix)))
	  (dolist (b (interface-bindings name))
	    (let* ((export (car b))
		   (lisp-name (symbol-name export))
		   (selected (if (eq select :all)
				 (and (not (member lisp-name hide :key #'symbol-name :test #'string=)) lisp-name)
				 (let ((s (find lisp-name select
						:key (lambda (x) (symbol-name (if (consp x) (car x) x)))
						:test #'string=)))
				   (and s (symbol-name (if (consp s) (cdr s) s)))))))
	      (when selected
		(let ((symbol (intern (if prefix (concatenate 'string (symbol-name prefix) selected) selected)
				      package)))
		  (when (and (not (eq (symbol-package symbol) package))
			     (or (fboundp symbol) (boundp symbol)))
		    (error "qg:use-modules: ~A would redefine ~S; use :select, :hide or :prefix"
			   selected symbol))
		  (import-binding symbol name export (cdr b)))))))))))

(defmacro use-modules (&rest specs)
  "Bring Guile modules' public bindings into the current package, with
use-modules' syntax written in Lisp: (srfi srfi-1), ((ice-9 match)
:select (match)), ((srfi srfi-1) :prefix srfi1- :hide (map)), :select
taking (name . lisp-name) to rename.  Procedures become functions,
variables symbol macros, macros Lisp macros (whose arguments are Guile
code, with Lisp's lexical variables and functions)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (import-modules ',specs ,(package-name *package*))))
