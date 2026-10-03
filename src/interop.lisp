; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-INTEROP -*-

;;;; Scheme and Common Lisp, both ways (docs/interop.md)
;;;;
;;;; Scheme calling Lisp: the virtual library (cl <package>) exports a
;;;; Lisp package's external functions and variables to Scheme,
;;;;
;;;;   (import (scheme base) (prefix (cl common-lisp) cl:))
;;;;   (cl:sort (list 3 1 2) cl:< #:key cl:identity)
;;;;
;;;; loading the system that defines the package if it isn't there yet
;;;; (*LISP-SYSTEM-LOADER*), and (pseudoscheme lisp) has the helpers
;;;; (lisp-true?, lisp-let, lisp-function, ...).
;;;;
;;;; Lisp calling Scheme: USE-LIBRARY makes a Scheme library a Lisp
;;;; package whose functions are the library's procedures,
;;;;
;;;;   (r7rs:use-library '(srfi 1))
;;;;   (srfi-1:filter #'evenp '(1 2 3 4))   ; => (2 4)
;;;;
;;;; Most values are shared as they are (lists, numbers, strings,
;;;; vectors, functions...).  Booleans are not: Scheme's #f is PS:FALSE,
;;;; and Lisp's false is NIL, which is Scheme's ().  So every crossing
;;;; converts:
;;;;
;;;; * Scheme -> Lisp (arguments to a (cl ...) function, results of a
;;;;   USE-LIBRARY function): #f becomes NIL.
;;;; * Lisp -> Scheme: a (cl ...) function whose name says it's a
;;;;   predicate (EVENP, STRING=, MEMBER, ...) returns #f for NIL; other
;;;;   functions return NIL as (), since lists are what NIL usually is.
;;;; * Functions crossing either way are wrapped so their results
;;;;   follow the same rules: a Lisp function given to Scheme code is
;;;;   taken to be a predicate (NIL -> #f), the common case of
;;;;   (filter #'evenp ...).  VERBATIM exempts a function; a wrapper
;;;;   crossing back is unwrapped, so cl:equal reaches MAKE-HASH-TABLE
;;;;   as #'EQUAL.

(defpackage "PSEUDOSCHEME-INTEROP"
  (:use "COMMON-LISP")
  (:export "SCHEMIFY" "EXPORT-BINDINGS" "*LISP-SYSTEM-LOADER*" "*AUTOLOAD-LISP-SYSTEMS*" "LOAD-LISP-SYSTEM"
	   "USE-LIBRARY" "LIBRARY-EXPORTS" "VERBATIM" "BOOT"
	   "TO-LISP" "TO-SCHEME" "LISP-FACING" "SCHEME-FACING"
	   "LISP-PREDICATE-NAME-P" "*LISP-PREDICATES*" "*NOT-LISP-PREDICATES*"
	   "LIBRARY-PACKAGE-NAME" "PARSE-LIBRARY-NAME"))

(in-package "PSEUDOSCHEME-INTEROP")

(defun ssym (string) (ps:intern-scheme-symbol string))

(defun schemify (datum)
  "A Lisp datum as a Scheme one: symbols move to the SCHEME package
(except T, which is #t, NIL, which is (), and keywords, which are
#:keywords); conses and vectors are copied recursively."
  (typecase datum
    (null nil)
    ((eql t) t)
    (keyword datum)
    ;; Already Scheme: SCHEME symbols, #f (PS:FALSE), uninterned symbols.
    (symbol (if (or (eq (symbol-package datum) ps:scheme-package)
		    (eq datum ps:false)
		    (null (symbol-package datum)))
		datum
		(intern (symbol-name datum) ps:scheme-package)))
    (cons (cons (schemify (car datum)) (schemify (cdr datum))))
    (simple-vector (map 'simple-vector #'schemify datum))
    (t datum)))

;;; ------------------------------------------------------------------
;;; Crossing the boundary

(defvar *originals* (make-hash-table :test 'eq :weakness :key)
  "Wrapper function -> the function it wraps, so a wrapper crossing back
is unwrapped rather than wrapped again.")

(defvar *verbatim* (make-hash-table :test 'eq :weakness :key)
  "Functions that cross the boundary unwrapped (see VERBATIM).")

(defvar *lisp-facing* (make-hash-table :test 'eq :weakness :key))
(defvar *scheme-facing* (make-hash-table :test 'eq :weakness :key))

(defun verbatim (function)
  "Mark FUNCTION to be passed between Scheme and Lisp as it is, with no
boolean conversion of its results (e.g. a Lisp callback that returns
lists, NIL meaning ()).  Returns FUNCTION.  The mark is on the function
object, so mark a fresh closure, (verbatim (lambda (x) ...)), rather
than a named function like #'evenp."
  (setf (gethash function *verbatim*) t)
  function)

(defun false-to-nil (&rest values)
  (values-list (mapcar (lambda (v) (if (eq v ps:false) nil v)) values)))

(defun lisp-facing (procedure)
  "PROCEDURE (a Scheme procedure) as a Lisp function: its arguments go
to Scheme (TO-SCHEME), its results come back with #f as NIL."
  (or (gethash procedure *originals*)
      (gethash procedure *lisp-facing*)
      (let ((f (lambda (&rest args)
		 (multiple-value-call #'false-to-nil
		   (apply procedure (mapcar #'to-scheme args))))))
	(setf (gethash f *originals*) procedure
	      (gethash procedure *lisp-facing*) f))))

(defun scheme-facing (function &optional (predicate t))
  "FUNCTION (a Lisp function) as a Scheme procedure: its arguments go to
Lisp (TO-LISP); if PREDICATE, a NIL result comes back as #f."
  (or (gethash function *originals*)
      (and predicate (gethash function *scheme-facing*))
      (let ((f (if predicate
		   (lambda (&rest args)
		     (let ((r (apply function (mapcar #'to-lisp args))))
		       (if (null r) ps:false r)))
		   (lambda (&rest args)
		     (apply function (mapcar #'to-lisp args))))))
	(setf (gethash f *originals*) function)
	(when predicate (setf (gethash function *scheme-facing*) f))
	f)))

(defun to-lisp (x)
  "A Scheme value passed to Lisp."
  (cond ((eq x ps:false) nil)
	((and (functionp x) (not (gethash x *verbatim*))) (lisp-facing x))
	(t x)))

(defun to-scheme (x)
  "A Lisp value passed to Scheme."
  (cond ((and (functionp x) (not (gethash x *verbatim*))) (scheme-facing x t))
	(t x)))

;;; ------------------------------------------------------------------
;;; Which Lisp functions are predicates

(defparameter *lisp-predicates*
  '("EQ" "EQL" "EQUAL" "EQUALP" "NOT" "NULL" "ATOM"
    "=" "/=" "<" ">" "<=" ">="
    "CHAR=" "CHAR/=" "CHAR<" "CHAR>" "CHAR<=" "CHAR>=" "CHAR-EQUAL"
    "CHAR-NOT-EQUAL" "CHAR-LESSP" "CHAR-GREATERP" "CHAR-NOT-GREATERP" "CHAR-NOT-LESSP"
    "STRING=" "STRING/=" "STRING<" "STRING>" "STRING<=" "STRING>=" "STRING-EQUAL"
    "STRING-NOT-EQUAL" "STRING-LESSP" "STRING-GREATERP" "STRING-NOT-GREATERP"
    "STRING-NOT-LESSP"
    "MEMBER" "MEMBER-IF" "MEMBER-IF-NOT" "FIND" "FIND-IF" "FIND-IF-NOT"
    "POSITION" "POSITION-IF" "POSITION-IF-NOT" "ASSOC" "ASSOC-IF" "ASSOC-IF-NOT"
    "RASSOC" "RASSOC-IF" "RASSOC-IF-NOT" "SOME" "EVERY" "NOTANY" "NOTEVERY"
    "SEARCH" "MISMATCH" "ENDP" "TAILP" "LOGTEST" "LOGBITP" "BOUNDP" "FBOUNDP"
    "FIND-PACKAGE" "PROBE-FILE" "LISTEN" "MACRO-FUNCTION" "COMPILER-MACRO-FUNCTION"
    "READ-CHAR-NO-HANG" "STRING-PREFIX-P" "EMPTYP" "STARTS-WITH" "ENDS-WITH"
    "STARTS-WITH-SUBSEQ" "ENDS-WITH-SUBSEQ")
  "Lisp functions whose NIL result means false though their names don't
say so (ANSI's, and a few of Alexandria's).")

(defparameter *not-lisp-predicates* '("MAP" "STEP" "LOOP" "SLEEP" "DRIBBLE")
  "Names that look like predicates (end in P) but aren't.")

(defun lisp-predicate-name-p (name)
  "Does a Lisp function named NAME (a string) return NIL for false? By
convention: FOO-P, or a one-word FOOP, plus *LISP-PREDICATES*."
  (let ((n (length name)))
    (and (not (member name *not-lisp-predicates* :test #'string=))
	 (or (member name *lisp-predicates* :test #'string=)
	     (and (> n 2) (string= "-P" name :start2 (- n 2)))
	     (and (> n 1) (char= (char name (1- n)) #\P)
		  (not (find #\- name))
		  (alpha-char-p (char name (- n 2))))))))

;;; ------------------------------------------------------------------
;;; Loading Lisp systems

(defvar *autoload-lisp-systems* t
  "If true, importing (cl <package>) for a package that doesn't exist
loads the system of the same name with *LISP-SYSTEM-LOADER* first.")

(defun default-lisp-system-loader (system)
  "Quicklisp's QUICKLOAD if Quicklisp is loaded (it fetches systems it
doesn't have), otherwise ASDF:LOAD-SYSTEM (which finds systems on
ASDF's source registry: ~/common-lisp, ~/quicklisp/local-projects via
Quicklisp, an ocicl or qlot project's systems, CL_SOURCE_REGISTRY)."
  (let ((ql (find-package "QUICKLISP-CLIENT")))
    (if ql
	(funcall (find-symbol "QUICKLOAD" ql) system :silent t)
	(asdf:load-system system))))

(defvar *lisp-system-loader* 'default-lisp-system-loader
  "Function of one argument, a system name (a string), that loads it.")

(defun load-lisp-system (system)
  (funcall *lisp-system-loader* (string system))
  t)

(defun lisp-package-for (name)
  "The Lisp package named by the parts of (cl <part> ...): their names
joined with /, as package-inferred systems name packages."
  (let ((pname (format nil "~{~A~^/~}"
		       (mapcar (lambda (p) (if (symbolp p) (symbol-name p) (princ-to-string p)))
			       name))))
    (or (find-package pname)
	(and *autoload-lisp-systems*
	     (progn (handler-case (load-lisp-system (string-downcase pname))
		      (error (e)
			(error "(cl ~(~A~)): no Lisp package ~A, and loading system ~(~A~) failed: ~A"
			       pname pname pname
			       (substitute #\Space #\Newline (princ-to-string e)))))
		    (find-package pname)))
	(error "(cl ~(~A~)): there is no Lisp package ~A~:[~; (and loading system ~(~A~) didn't make one)~]"
	       pname pname *autoload-lisp-systems* pname))))

;;; ------------------------------------------------------------------
;;; (cl <package>) libraries

(defvar *cl-library-count* 0)

(defun host-global (n symbol)
  "The host global holding SYMBOL's wrapper in the Nth (cl ...) library."
  (format nil "%cl~D:~A:~A" n (package-name (symbol-package symbol)) (symbol-name symbol)))

(defvar *function-symbols* (make-hash-table :test 'eq :weakness :key)
  "Wrapper exported by a (cl ...) library -> its Lisp symbol, for
lisp-set!.")

(defvar *setters* (make-hash-table :test 'equal)
  "(symbol . number of arguments) -> compiled (lambda (value . args)
(setf (symbol . args) value)).")

(defun lisp-setf (accessor value args)
  "(setf (ACCESSOR . ARGS) VALUE), ACCESSOR a function from a (cl ...)
library or a Lisp symbol; any place Lisp's SETF knows works."
  (let* ((sym (cond ((gethash accessor *function-symbols*))
		    ((and (symbolp accessor) (not (eq (symbol-package accessor) ps:scheme-package))) accessor)
		    (t (ps:scheme-error "lisp-set!: not a Lisp accessor: ~S" accessor))))
	 (key (cons sym (length args)))
	 (setter (or (gethash key *setters*)
		     (setf (gethash key *setters*)
			   (let ((params (loop repeat (length args) collect (gensym))))
			     (compile nil `(lambda (value ,@params) (setf (,sym ,@params) value))))))))
    (apply setter (to-lisp value) (mapcar #'to-lisp args))))

(defun exportable-function-p (symbol)
  (and (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun cl-library-form (name)
  "The library (cl . NAME), for psyntax: installs a primitives library
of the package's functions (wrapped, see SCHEME-FACING) and returns a
library form that re-exports them and defines its variables as
identifier syntax over SYMBOL-VALUE."
  (let* ((package (lisp-package-for name))
	 (functions '()) (variables '()))
    (do-external-symbols (s package)
      (cond ((exportable-function-p s) (push s functions))
	    ((boundp s) (push s variables))))
    (setq functions (sort functions #'string< :key #'symbol-name)
	  variables (sort variables #'string< :key #'symbol-name))
    (let* ((n (incf *cl-library-count*))
	   (prims-name (list "pseudoscheme" "cl-primitives" (format nil "~A~D" (package-name package) n)))
	   (prims '()))
      (dolist (s functions)
	(let ((global (host-global n s)))
	  (let ((f (scheme-facing (fdefinition s) (lisp-predicate-name-p (symbol-name s)))))
	    (setf (gethash f *function-symbols*) s)
	    (psx:host-set! global f))
	  (push (cons (ps:invert-case (symbol-name s)) global) prims)))
      (ps-r7rs::install-host-library prims-name (nreverse prims))
      (labels ((scheme-name (s) (ssym (ps:invert-case (symbol-name s))))
	       (r (string) (ssym (concatenate 'string "%%r:" string))))
	(list* (ssym "library") (cons (ssym "cl") name)
	       (cons (ssym "export") (mapcar #'scheme-name (append functions variables)))
	       (list (ssym "import")
		     (list (ssym "prefix") (list (ssym "rnrs")) (ssym "%%r:"))
		     (list (ssym "only") (list (ssym "pseudoscheme") (ssym "lisp"))
			   (ssym "%%lisp-symbol"))
		     (list (ssym "prefix") (list (ssym "pseudoscheme") (ssym "lisp") (ssym "primitives"))
			   (ssym "%%lisp:"))
		     (mapcar #'ssym prims-name))
	       (mapcar (lambda (s) (variable-syntax (scheme-name s) s #'r)) variables))))))

(defun variable-syntax (name symbol r)
  "(define-syntax NAME ...): reading NAME reads SYMBOL's value, set!
sets it, and (NAME %%lisp-symbol) is SYMBOL itself (for lisp-let)."
  (let ((x (ssym "x")) (e (ssym "e")) (a (ssym "a")) (dots (funcall r "..."))
	(_ (funcall r "_")) (lsym (ssym "%%lisp-symbol"))
	(quoted (list (funcall r "quote") symbol)))
    (flet ((r (s) (funcall r s))
	   (stx (form) (list (funcall r "syntax") form)))
      (list (r "define-syntax") name
	    (list (r "make-variable-transformer")
		  (list (r "lambda") (list x)
			(list (r "syntax-case") x (list (r "set!") lsym)
			      (list (list (r "set!") _ e)
				    (stx (list (ssym "%%lisp:set-lisp-value!") quoted e)))
			      (list (list _ lsym) (stx quoted))
			      (list (list* _ a (list dots))
				    (stx (list* (list (ssym "%%lisp:lisp-value") quoted) a (list dots))))
			      (list _ (list (r "identifier?") x)
				    (stx (list (ssym "%%lisp:lisp-value") quoted))))))))))

(defun cl-library-hook (name)
  (when (and (consp name) (symbolp (car name))
	     (string= (ps:scheme-symbol-name (car name)) "cl")
	     (cdr name))
    (cl-library-form (cdr name))))

;;; ------------------------------------------------------------------
;;; (pseudoscheme lisp): helpers for Scheme code

(defun lisp-name (x)
  "A Lisp symbol or package name from a Scheme string or symbol, by the
symbol case rule: \"equal\" and 'equal both name EQUAL."
  (etypecase x
    (string (ps:invert-case x))
    (symbol (symbol-name x))))

(defun lisp-symbol (name &optional (package "common-lisp-user"))
  (let ((p (or (find-package (lisp-name package))
	       (ps:scheme-error "lisp-symbol: no Lisp package ~A" (lisp-name package)))))
    (multiple-value-bind (s status) (find-symbol (lisp-name name) p)
      (if status s (ps:scheme-error "lisp-symbol: no symbol ~A in ~A" (lisp-name name) (package-name p))))))

(defun install-lisp-primitives ()
  (let ((prims '()))
    (macrolet ((prim (name lambda-list &body body)
		 `(let ((global (concatenate 'string "%lisp:" ,name)))
		    (psx:defhost global ,lambda-list ,@body)
		    (push (cons ,name global) prims))))
      (prim "lisp-true?" (x) (if (or (null x) (eq x ps:false)) ps:false t))
      (prim "lisp-false?" (x) (if (or (null x) (eq x ps:false)) t ps:false))
      (prim "lisp-symbol" (name &optional (package "common-lisp-user")) (lisp-symbol name package))
      (prim "lisp-keyword" (name) (intern (lisp-name name) "KEYWORD"))
      ;; The function itself: Scheme calls it with no conversion.  (Not
      ;; marked VERBATIM: that would change how the function object
      ;; crosses everywhere, e.g. #'evenp passed from Lisp.)
      (prim "lisp-function" (name &optional (package "common-lisp"))
	    (fdefinition (if (and (symbolp name) (not (eq (symbol-package name) ps:scheme-package)))
			     name
			     (lisp-symbol name package))))
      (prim "lisp-funcall" (f &rest args) (apply (scheme-facing (to-lisp f) nil) args))
      (prim "lisp-apply" (f &rest args) (apply (scheme-facing (to-lisp f) nil) (apply #'list* args)))
      (prim "lisp-value" (s) (symbol-value s))
      (prim "set-lisp-value!" (s v) (setf (symbol-value s) (to-lisp v)) ps:unspecific)
      (prim "call-with-lisp-bindings" (symbols values thunk)
	    (progv symbols (mapcar #'to-lisp values) (funcall thunk)))
      (prim "lisp-eval-string" (string)
	    (let ((*package* (find-package "COMMON-LISP-USER")))
	      (eval (read-from-string string))))
      (prim "lisp-require" (system) (load-lisp-system (lisp-name system)) ps:unspecific)
      (prim "%lisp-setf!" (accessor value &rest args) (lisp-setf accessor value args) ps:unspecific)
      (prim "verbatim" (f) (verbatim f)))
    (ps-r7rs::install-host-library '("pseudoscheme" "lisp" "primitives") (nreverse prims))
    (dolist (form (ps-r7rs::read-forms
		   (asdf:system-relative-pathname :pseudoscheme "src/interop/lisp.sls")))
      (psx:eval-library form))))

;;; ------------------------------------------------------------------
;;; Lisp calling Scheme

(defun parse-library-name (name)
  "A library name given from Lisp -- a list like (srfi 1) or
(|scheme| |base|), or a string \"(srfi 1)\" -- as psyntax's."
  (let ((name (if (stringp name)
		  (with-input-from-string (in name) (funcall ps:*scheme-read* in))
		  (schemify name))))
    (ps-r7rs::translate-name name)))

(defun library-package-name (name)
  "(srfi :1) -> \"SRFI-1\", (scheme list) -> \"SCHEME-LIST\"."
  (format nil "~{~A~^-~}"
	  (mapcar (lambda (p)
		    (let ((s (if (symbolp p) (symbol-name p) (princ-to-string p))))
		      (if (and (> (length s) 1) (char= (char s 0) #\:)) (subseq s 1) s)))
		  name)))

(defun export-bindings (name)
  "((symbol kind value) ...) for library NAME: KIND is :procedure,
:variable (VALUE a function returning the current value) or :syntax."
  (ps-r7rs::boot)
  (loop for (sym type val) in (funcall (psx:host-ref "psyntax:library-export-bindings") name)
	for tname = (ps:scheme-symbol-name type)
	collect (cond ((member tname '("global" "core-prim") :test #'string=)
		       ;; a global's value is (library . location)
		       (let ((getter (let ((v (if (consp val) (cdr val) val)))
				       (lambda () (psx:host-ref v)))))
			 (if (functionp (funcall getter))
			     (list sym :procedure (funcall getter))
			     (list sym :variable getter))))
		      (t (list sym :syntax nil)))))

(defun library-exports (name)
  "The exports of Scheme library NAME, as (name . kind) with KIND
:procedure, :variable or :syntax (names are Scheme symbols)."
  (mapcar (lambda (b) (cons (first b) (second b))) (export-bindings (parse-library-name name))))

(defun use-library (name &key package (convert t))
  "Make Scheme library NAME (e.g. '(srfi 1) or \"(srfi 1)\") usable from
Lisp as a package (default: named after the library, (srfi 1) ->
SRFI-1), exporting one symbol per procedure or variable: procedures
become functions, variables symbol macros reading their current value.
Syntax exports are skipped (they're Scheme macros).  With CONVERT (the
default), functions convert booleans as described in this file's
header; with CONVERT NIL they are the Scheme procedures themselves.
Returns the package."
  (let* ((name (parse-library-name name))
	 (bindings (export-bindings name))
	 (pname (string (or package (library-package-name name))))
	 (pkg (or (find-package pname) (make-package pname :use '()))))
    (loop for (sym kind value) in bindings
	  for s = (intern (symbol-name sym) pkg)
	  do (ecase kind
	       (:procedure
		(setf (fdefinition s) (if convert (lisp-facing value) value))
		(export s pkg))
	       (:variable
		(eval `(define-symbol-macro ,s
			   ,(if convert
				`(to-lisp (funcall ,value))
				`(funcall ,value))))
		(export s pkg))
	       (:syntax nil)))
    pkg))

;;; ------------------------------------------------------------------
;;; Boot

(defvar *booted-host* nil)

(defun boot ()
  "Install (pseudoscheme lisp) and the (cl ...) hook, once per host."
  (ps-r7rs::boot)
  (unless (eq *booted-host* psx:*host*)
    (pushnew 'cl-library-hook ps-r7rs::*virtual-libraries*)
    (install-lisp-primitives)
    (setq *booted-host* psx:*host*))
  t)
