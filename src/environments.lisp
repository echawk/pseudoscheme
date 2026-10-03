;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-ENVIRONMENTS -*-

;;;; Program environments, from Lisp
;;;;
;;;; Helpers for building and filling Pseudoscheme program environments
;;;; (module.scm's PROGRAM-ENV: a CL package plus a table from names to
;;;; binding nodes) from Common Lisp: the R7RS runtime builds its
;;;; implementation environment with these, and psyntax's host
;;;; environment (src/psyntax.lisp) is one.
;;;;
;;;; This was src/library.lisp, which also implemented define-library and
;;;; import natively; psyntax does that now (src/r7rs/front.lisp).

(defpackage "PSEUDOSCHEME-ENVIRONMENTS"
  (:nicknames "PSL")
  (:use "COMMON-LISP")
  (:export "TR" "BASE-STRUCTURE" "SCHEME-SYMBOL" "SNAME" "KEYWORD-HEAD-P"
	   "NEW-LIBRARY-ENV" "BINDING-DEFINED-P" "INSTALL-VARIABLE!"
	   "COPY-ALL!" "COPY-BINDINGS!" "ALIAS-ALL!" "VARIABLE-NODE-P"
	   "READ-FORMS-FROM-FILE" "*SCHEME-FEATURES*"))

(in-package "PSEUDOSCHEME-ENVIRONMENTS")

;;; ------------------------------------------------------------------
;;; Reaching into the translator
;;;
;;; The translator is Scheme code, so its procedures are CL functions in
;;; the SCHEME-TRANSLATOR package (only the ones its interface exports
;;; are external).

(defun tr (name &rest args)
  (let ((sym (find-symbol name "SCHEME-TRANSLATOR")))
    (unless (and sym (fboundp sym))
      (error "Translator procedure ~A not found" name))
    (apply (symbol-function sym) args)))

(defun base-structure ()
  (symbol-value (find-symbol "REVISED^4-SCHEME-STRUCTURE" "SCHEME-TRANSLATOR")))

;;; Scheme symbols live in the SCHEME package, upcased by the reader.
(defun scheme-symbol (string)
  "The Scheme symbol named STRING."
  (ps:intern-scheme-symbol string))

(defun sname (symbol)
  "The Scheme name of a symbol (or the printed form of a number)."
  (typecase symbol
    (symbol (ps:scheme-symbol-name symbol))
    (t (princ-to-string symbol))))

(defun keyword-head-p (form word)
  "Is FORM a list whose car is the Scheme symbol named WORD?"
  (and (consp form) (symbolp (car form)) (string= (sname (car form)) word)))


;;; ------------------------------------------------------------------
;;; Environments and bindings

(defvar *env-counter* 0)

(defun new-library-env (id-string)
  "A fresh, empty program environment (a CL package of its own)."
  (let ((sym (intern (format nil "LIBRARY ~A ~D" id-string (incf *env-counter*))
		     "SCHEME")))
    (tr "MAKE-PROGRAM-ENV" sym '())))

(defun variable-node-p (node)
  ;; (Translator predicates return Scheme booleans, and #f is the
  ;; symbol PS:FALSE -- truthy to Lisp -- hence PS:TRUEP.)
  (and (ps:truep (tr "NODE?" node))
       (ps:truep (tr "PROGRAM-VARIABLE?" node))))

(defun binding-defined-p (env name)
  "Is NAME bound in ENV to something real (a macro/special form, or a
variable that has been given a value)?  Plain PROGRAM-ENV-LOOKUP can't
tell, since it conjures up unbound variables on demand."
  (let ((node (tr "PROGRAM-ENV-LOOKUP" env name)))
    (if (variable-node-p node)
	(boundp (tr "PROGRAM-VARIABLE-LOCATION" node))
	t)))

(defun install-variable! (env name value)
  "Bind NAME in ENV to a *fresh* variable holding VALUE (a Lisp function
for a procedure), the way MOVE-VALUE-OR-DENOTATION does for the user env.
Fresh matters: redefining a name in ENV must not disturb whatever the
name was copied or imported from."
  (let* ((node (tr "PROGRAM-ENV-NEW-VARIABLE" env name))
	 (loc (tr "PROGRAM-VARIABLE-LOCATION" node)))
    (setf (symbol-value loc) value)
    (when (functionp value)
      (funcall (symbol-function (find-symbol "SET-FUNCTION-FROM-VALUE" "PS")) loc))
    node))

(defun copy-all! (env structure)
  "Give ENV its own copy of every export of STRUCTURE: a fresh variable
holding the same value for each variable, the shared node for syntactic
keywords.  (Unlike an import, which aliases, a copy can be redefined
without affecting the original; this is how SCHEME-USER-ENVIRONMENT is
made too.)"
  (dolist (name (tr "INTERFACE-NAMES" (tr "STRUCTURE-INTERFACE" structure)))
    (let ((den (tr "STRUCTURE-REF" structure name)))
      (if (and (variable-node-p den)
	       (boundp (tr "PROGRAM-VARIABLE-LOCATION" den)))
	  (install-variable! env name
			     (symbol-value (tr "PROGRAM-VARIABLE-LOCATION" den)))
	  (tr "PROGRAM-ENV-DEFINE!" env name den)))))

(defun copy-bindings! (to from names)
  "Like COPY-ALL!, from the program env FROM, for just NAMES (those
actually bound there)."
  (dolist (name names)
    (when (binding-defined-p from name)
      (let ((den (tr "PROGRAM-ENV-LOOKUP" from name)))
	(if (and (variable-node-p den)
		 (boundp (tr "PROGRAM-VARIABLE-LOCATION" den)))
	    (install-variable! to name (symbol-value (tr "PROGRAM-VARIABLE-LOCATION" den)))
	    (tr "PROGRAM-ENV-DEFINE!" to name den))))))

(defun alias-all! (env structure)
  "Make every export of STRUCTURE visible in ENV under its own name,
sharing the very same bindings."
  (dolist (name (tr "INTERFACE-NAMES" (tr "STRUCTURE-INTERFACE" structure)))
    (tr "PROGRAM-ENV-DEFINE!" env name (tr "STRUCTURE-REF" structure name))))

;;; ------------------------------------------------------------------
;;; Features (R7RS appendix B), for cond-expand

(defparameter *scheme-features*
  (append '("r7rs" "exact-closed" "exact-complex" "ieee-float" "ratios"
	    "pseudoscheme" "common-lisp")
	  (when (> char-code-limit 255) '("full-unicode"))
	  (list (string-downcase (lisp-implementation-type))
		(string-downcase (machine-type))
		(string-downcase (software-type))))
  "Feature identifiers (R7RS appendix B) satisfied by this implementation.")

(defun read-forms-from-file (name)
  (with-open-file (in name)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

