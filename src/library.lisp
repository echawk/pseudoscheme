; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-LIBRARY -*-

;;;; Libraries and top-level programs, shared by R6RS and R7RS
;;;;
;;;; R6RS chapter 7 (LIBRARY) and R7RS section 5.6 (DEFINE-LIBRARY) are
;;;; two surface syntaxes for the same idea, and both map directly onto
;;;; machinery Pseudoscheme already has in module.scm:
;;;;
;;;;   library body environment   <-> a PROGRAM-ENV (a CL package, plus a
;;;;                                  table of name -> binding "node")
;;;;   library's exports          <-> an INTERFACE (a list of names)
;;;;   the library as a whole     <-> a STRUCTURE (interface + env)
;;;;
;;;; An import is done by *aliasing nodes*: PROGRAM-ENV-DEFINE! binds the
;;;; importing name directly to the exporting library's node.  Because
;;;; variable nodes name a CL symbol, an imported variable is the very
;;;; same location, not a copy; syntactic keywords are nodes too, so
;;;; macros import the same way.  IMPORT SETS (only, except, prefix,
;;;; rename) are then just transformations on lists of (name . node),
;;;; and renaming an export is an alias in a scratch export env.
;;;;
;;;; What this deliberately leaves out (see ROADMAP.md):
;;;;  * R6RS phases / `for' import levels -- accepted and ignored; there
;;;;    is one phase.
;;;;  * Per-library hygiene for syntax-case macros: those live in one
;;;;    global table (see syntax-case.lisp) rather than in nodes.
;;;;  * Library versions are parsed and matched, but only the newest
;;;;    version of a name is kept.

(defpackage "PSEUDOSCHEME-LIBRARY"
  (:nicknames "PSL")
  (:use "COMMON-LISP")
  (:export "LIBRARY" "LIBRARY-NAME" "FIND-LIBRARY" "REGISTER-LIBRARY"
	   "DEFINE-LIBRARY-FORM" "R6RS-LIBRARY-FORM" "READ-FORMS-FROM-FILE"
	   "KEYWORD-HEAD-P" "BASE-STRUCTURE" "TR" "SNAME" "LIBRARY-KEY-OF" "IMPORT-INTO" "RESOLVE-IMPORT-SET"
	   "SELECT-COND-EXPAND-CLAUSE" "RUN-PROGRAM" "LOAD-PROGRAM" "PROGRAM-ENVIRONMENT" "*LIBRARY-EVALUATOR*"
	   "*SCHEME-FEATURES*" "FEATURE-SATISFIED-P" "SCHEME-SYMBOL"
	   "MAKE-LIBRARY-FROM-ENV" "LIBRARY-EXPORT-NAMES"
	   "NEW-LIBRARY-ENV" "BINDING-DEFINED-P" "LIBRARY-ERROR"
	   "ALIAS-ALL!" "COPY-ALL!" "INSTALL-VARIABLE!" "LIBRARY-EXPORTS"
	   "*EXPLICIT-IMPORTS*" "*SYNTAX-BINDING-P*" "COPY-BINDINGS!"))

(in-package "PSEUDOSCHEME-LIBRARY")

(define-condition library-error (error)
  ((message :initarg :message :reader library-error-message))
  (:report (lambda (c stream)
	     (write-string (library-error-message c) stream))))

(defun lib-error (control &rest args)
  (error 'library-error :message (apply #'format nil control args)))

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
  (intern (string-upcase string) "SCHEME"))

(defun sname (symbol)
  "The textual name of a Scheme symbol (or number, in a library name)."
  (typecase symbol
    (symbol (string-downcase (symbol-name symbol)))
    (t (princ-to-string symbol))))

;;; ------------------------------------------------------------------
;;; Library objects and the registry

(defstruct (library (:constructor %make-library))
  key			; list of strings, e.g. ("scheme" "base")
  name			; as written, e.g. (SCHEME BASE)
  version		; list of integers, or NIL
  structure		; translator STRUCTURE (interface + program env)
  exports		; alist (external-symbol . node)
  env)			; the library's own body env, or NIL for native ones

(defmethod print-object ((l library) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~{~A~^ ~}~@[ ~S~]" (library-key l) (library-version l))))

(defvar *libraries* (make-hash-table :test 'equal))

(defun library-key-of (name)
  (mapcar #'sname name))

(defun split-version (name)
  "Split an R6RS library name `(id ... (1 2))' into name and version."
  (let ((last (car (last name))))
    (if (and (consp last) (every #'integerp last))
	(values (butlast name) last)
	(values name nil))))

(defun register-library (lib)
  (let ((old (gethash (library-key lib) *libraries*)))
    (when (and old (library-version old) (library-version lib)
	       (not (version<= (library-version old) (library-version lib))))
      (return-from register-library old))
    (setf (gethash (library-key lib) *libraries*) lib)))

(defun version<= (a b)
  (loop (cond ((null a) (return t))
	      ((null b) (return (every #'zerop a)))
	      ((< (car a) (car b)) (return t))
	      ((> (car a) (car b)) (return nil)))
	(pop a) (pop b)))

;;; R6RS version references (7.1): (sub ...), (and ..), (or ..), (not ..)
;;; with sub-versions n, (>= n), (<= n), (and ..), (or ..), (not ..).

(defun subversion-matches-p (ref n)
  (cond ((integerp ref) (= ref n))
	(t (let ((op (sname (car ref))))
	     (cond ((string= op ">=") (>= n (cadr ref)))
		   ((string= op "<=") (<= n (cadr ref)))
		   ((string= op "and") (every (lambda (r) (subversion-matches-p r n)) (cdr ref)))
		   ((string= op "or") (some (lambda (r) (subversion-matches-p r n)) (cdr ref)))
		   ((string= op "not") (not (subversion-matches-p (cadr ref) n)))
		   (t (lib-error "bad sub-version reference ~S" ref)))))))

(defun version-matches-p (ref version)
  (cond ((null ref) t)
	((and (consp ref) (symbolp (car ref)))
	 (let ((op (sname (car ref))))
	   (cond ((string= op "and") (every (lambda (r) (version-matches-p r version)) (cdr ref)))
		 ((string= op "or") (some (lambda (r) (version-matches-p r version)) (cdr ref)))
		 ((string= op "not") (not (version-matches-p (cadr ref) version)))
		 (t (lib-error "bad version reference ~S" ref)))))
	(t (and (>= (length version) (length ref))
		(every #'subversion-matches-p ref version)))))

(defun find-library (name &optional (errorp t))
  "NAME is a library name or R6RS library reference (name ... [version-ref])."
  ;; An R7RS name is all symbols/integers; a trailing list can only be
  ;; an R6RS version reference ((): any version).
  (let* ((last (car (last name)))
	 (has-vref (listp last))
	 (base (if has-vref (butlast name) name))
	 (vref (if has-vref last nil))
	 (lib (gethash (library-key-of base) *libraries*)))
    (cond ((and lib (version-matches-p vref (or (library-version lib) '())))
	   lib)
	  (errorp (lib-error "No such library: ~S" name))
	  (t nil))))

;;; ------------------------------------------------------------------
;;; Bindings: nodes, definedness, installing variables

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
;;; Import sets

(defun library-bindings (lib)
  (mapcar (lambda (pair) (cons (car pair) (cdr pair))) (library-exports lib)))

(defun keyword-head-p (spec word)
  (and (consp spec) (symbolp (car spec)) (string= (sname (car spec)) word)))

(defun resolve-import-set (spec)
  "Resolve an <import set> to an alist of (local-name . node)."
  (cond
    ((keyword-head-p spec "only")
     (let ((inner (resolve-import-set (cadr spec))))
       (mapcar (lambda (id)
		 (or (assoc id inner)
		     (lib-error "import: ~A is not exported (only ~S)" (sname id) (cadr spec))))
	       (cddr spec))))
    ((keyword-head-p spec "except")
     (let ((inner (resolve-import-set (cadr spec))))
       (dolist (id (cddr spec))
	 (unless (assoc id inner)
	   (lib-error "import: ~A is not exported (except ~S)" (sname id) (cadr spec))))
       (remove-if (lambda (pair) (member (car pair) (cddr spec))) inner)))
    ((keyword-head-p spec "prefix")
     (let ((prefix (sname (caddr spec))))
       (mapcar (lambda (pair)
		 (cons (scheme-symbol (concatenate 'string prefix (sname (car pair))))
		       (cdr pair)))
	       (resolve-import-set (cadr spec)))))
    ((keyword-head-p spec "rename")
     (let* ((inner (resolve-import-set (cadr spec)))
	    (renames (cddr spec)))
       (dolist (r renames)
	 (unless (assoc (car r) inner)
	   (lib-error "import: ~A is not exported (rename ~S)" (sname (car r)) (cadr spec))))
       (mapcar (lambda (pair)
		 (let ((r (assoc (car pair) renames)))
		   (if r (cons (cadr r) (cdr pair)) pair)))
	       inner)))
    ;; R6RS: (for <import set> <level> ...) -- levels ignored (one phase)
    ((keyword-head-p spec "for")
     (resolve-import-set (cadr spec)))
    ;; R6RS: (library <library reference>)
    ((keyword-head-p spec "library")
     (resolve-import-set (cadr spec)))
    ((consp spec)
     (library-bindings (find-library spec)))
    (t (lib-error "bad import set: ~S" spec))))

(defvar *explicit-imports* (make-hash-table :test 'eq :weakness :key)
  "env -> hash table of name -> node, recording what was imported so a
second, different binding for the same name can be reported.")

(defun import-into (env specs)
  "Import each <import set> in SPECS into ENV."
  (let ((seen (or (gethash env *explicit-imports*)
		  (setf (gethash env *explicit-imports*) (make-hash-table :test 'eq)))))
    (dolist (spec specs)
      (dolist (pair (resolve-import-set spec))
	(destructuring-bind (name . node) pair
	  (let ((old (gethash name seen)))
	    (when (and old (not (eq old node)))
	      (lib-error "import: ~A is imported with two different bindings" (sname name))))
	  (setf (gethash name seen) node)
	  (unless (eq node :syntax)
	    (tr "PROGRAM-ENV-DEFINE!" env name node)))))))

;;; ------------------------------------------------------------------
;;; cond-expand

(defparameter *scheme-features*
  (append '("r7rs" "exact-closed" "exact-complex" "ieee-float" "ratios"
	    "pseudoscheme" "common-lisp")
	  (when (> char-code-limit 255) '("full-unicode"))
	  (list (string-downcase (lisp-implementation-type))
		(string-downcase (machine-type))
		(string-downcase (software-type))))
  "Feature identifiers (R7RS appendix B) satisfied by this implementation.")

(defun feature-satisfied-p (req)
  (cond ((symbolp req)
	 (or (string= (sname req) "else")
	     (member (sname req) *scheme-features* :test #'string=)))
	((keyword-head-p req "and") (every #'feature-satisfied-p (cdr req)))
	((keyword-head-p req "or") (some #'feature-satisfied-p (cdr req)))
	((keyword-head-p req "not") (not (feature-satisfied-p (cadr req))))
	((keyword-head-p req "library") (and (find-library (cadr req) nil) t))
	(t (lib-error "bad feature requirement: ~S" req))))

(defun select-cond-expand-clause (clauses)
  "Body of the first clause whose requirement holds; second value true
if any clause did (the body itself may be empty)."
  (dolist (clause clauses (values nil nil))
    (when (feature-satisfied-p (car clause))
      (return (values (cdr clause) t)))))

;;; ------------------------------------------------------------------
;;; Evaluating forms in a library environment
;;;
;;; The evaluator is pluggable so the same library machinery serves a
;;; native-syntax-rules R7RS and a syntax-case R6RS; see r6rs.lisp.

(defvar *syntax-binding-p* nil
  "Optional function of a name: true if the name is a macro that lives
outside the program-env node tables (syntax-case's global table).  Such
an export is recorded as :SYNTAX; since syntax-case macros are global,
importing it has nothing to do (and cannot be renamed or restricted --
a known limitation, see ROADMAP.md).")

(defvar *library-evaluator*
  (lambda (form env) (ps:scheme-eval form env))
  "Function of (form env).")

(defun read-forms-from-file (name)
  (with-open-file (in name)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

;;; ------------------------------------------------------------------
;;; The two library forms

(defun collect-r7rs-declarations (decls)
  "Flatten define-library declarations into (:export ..)/(:import ..)/(:body ..)
entries, expanding cond-expand and include-library-declarations."
  (let ((out '()))
    (labels ((walk (decls)
	       (dolist (d decls)
		 (cond ((keyword-head-p d "export") (push (cons :export (cdr d)) out))
		       ((keyword-head-p d "import") (push (cons :import (cdr d)) out))
		       ((keyword-head-p d "begin") (push (cons :body (cdr d)) out))
		       ((or (keyword-head-p d "include") (keyword-head-p d "include-ci"))
			(push (cons :body (mapcan #'read-forms-from-file (cdr d))) out))
		       ((keyword-head-p d "include-library-declarations")
			(walk (mapcan #'read-forms-from-file (cdr d))))
		       ((keyword-head-p d "cond-expand")
			(walk (select-cond-expand-clause (cdr d))))
		       (t (lib-error "bad define-library declaration: ~S" d))))))
      (walk decls))
    (nreverse out)))

(defun build-library (name version export-specs import-specs body-forms
		      &key (implicit-imports '()))
  "Create, evaluate and register a library.  EXPORT-SPECS are R7RS style:
IDENTIFIER or (rename internal external)."
  (let ((env (new-library-env (format nil "~{~A~^ ~}" (library-key-of name)))))
    (when implicit-imports
      (import-into env implicit-imports))
    (import-into env import-specs)
    (dolist (form body-forms)
      (funcall *library-evaluator* form env))
    (let ((export-env (new-library-env "exports"))
	  (exports '()))
      (dolist (spec export-specs)
	(multiple-value-bind (internal external)
	    (if (consp spec)
		(values (cadr spec) (caddr spec))
		(values spec spec))
	  (cond ((and *syntax-binding-p* (funcall *syntax-binding-p* internal)
		      (not (binding-defined-p env internal)))
		 (push (cons external :syntax) exports))
		((binding-defined-p env internal)
		 (let ((node (tr "PROGRAM-ENV-LOOKUP" env internal)))
		   (tr "PROGRAM-ENV-DEFINE!" export-env external node)
		   (push (cons external node) exports)))
		(t
		 (lib-error "library ~S exports ~A, which is not defined"
			    name (sname internal))))))
      (setq exports (nreverse exports))
      (let* ((key (library-key-of name))
	     (id (intern (format nil "LIBRARY ~{~A~^ ~}" key) "SCHEME"))
	     (sig (tr "MAKE-INTERFACE" id
		      (mapcar #'car (remove :syntax exports :key #'cdr)) '()))
	     (structure (tr "MAKE-STRUCTURE" id sig export-env)))
	(register-library
	 (%make-library :key key :name name :version version
			:structure structure :exports exports :env env))))))

(defun define-library-form (form &key implicit-imports)
  "R7RS (define-library <name> <declaration> ...)"
  (let* ((name (cadr form))
	 (decls (collect-r7rs-declarations (cddr form))))
    (build-library name nil
		   (loop for d in decls when (eq (car d) :export) append (cdr d))
		   (loop for d in decls when (eq (car d) :import) append (cdr d))
		   (loop for d in decls when (eq (car d) :body) append (cdr d))
		   :implicit-imports implicit-imports)))

(defun r6rs-export-specs (specs)
  "R6RS export specs: IDENTIFIER | (rename (internal external) ...)  ->
the R7RS-style flat list."
  (loop for spec in specs
	append (if (keyword-head-p spec "rename")
		   (mapcar (lambda (pair) (list (scheme-symbol "rename") (car pair) (cadr pair)))
			   (cdr spec))
		   (list spec))))

(defun r6rs-library-form (form &key implicit-imports)
  "R6RS (library <name> (export <spec> ...) (import <spec> ...) <body> ...)"
  (destructuring-bind (name-and-version exports imports &rest body) (cdr form)
    (unless (and (keyword-head-p exports "export") (keyword-head-p imports "import"))
      (lib-error "malformed library form: ~S" form))
    (multiple-value-bind (name version) (split-version name-and-version)
      (build-library name version
		     (r6rs-export-specs (cdr exports))
		     (cdr imports)
		     body
		     :implicit-imports implicit-imports))))

;;; ------------------------------------------------------------------
;;; Native libraries: expose an existing environment under a library name

(defun make-library-from-env (name env export-names &key version syntax-names)
  "Register NAME as a library exporting EXPORT-NAMES (Scheme symbols,
already bound in ENV).  SYNTAX-NAMES are further exports that are
syntax-case macros: recorded, but with no node behind them."
  (let* ((export-env (new-library-env "exports"))
	 (exports (mapcar (lambda (n)
			    (let ((node (tr "PROGRAM-ENV-LOOKUP" env n)))
			      (tr "PROGRAM-ENV-DEFINE!" export-env n node)
			      (cons n node)))
			  (remove-if (lambda (n) (member n syntax-names)) export-names)))
	 (key (library-key-of name))
	 (id (intern (format nil "LIBRARY ~{~A~^ ~}" key) "SCHEME"))
	 (sig (tr "MAKE-INTERFACE" id
		  (remove-if (lambda (n) (member n syntax-names)) export-names) '())))
    (register-library
     (%make-library :key key :name name :version version
		    :structure (tr "MAKE-STRUCTURE" id sig export-env)
		    :exports (append exports
				     (mapcar (lambda (n) (cons n :syntax)) syntax-names))
		    :env env))))

(defun library-export-names (lib)
  (mapcar #'car (library-exports lib)))

;;; ------------------------------------------------------------------
;;; Top-level programs
;;;
;;; R6RS ch. 8 / R7RS 5.1: a program is an (import ...) form followed by
;;; definitions and expressions, evaluated in order in a fresh
;;; environment.  (R6RS says definitions and expressions may interleave
;;; and are treated as a letrec* body; evaluating in order in a
;;; top-level env is the same thing in practice.)

(defun program-environment (import-forms &key implicit-imports)
  "A fresh environment for a top-level program that begins with
IMPORT-FORM (an (import <import set> ...) form)."
  (let ((env (new-library-env "program")))
    (when implicit-imports (import-into env implicit-imports))
    (unless (keyword-head-p import-forms "import")
      (lib-error "A program must begin with an import form"))
    (import-into env (cdr import-forms))
    env))

(defun run-program (forms &key implicit-imports on-error)
  "Run a program.  If ON-ERROR is given (function of form and condition),
an error in one form is reported to it and evaluation continues with the
next -- for test runners.  Returns the last value and the environment."
  ;; R7RS allows (define-library ...) forms ahead of the program proper.
  (loop while (keyword-head-p (car forms) "define-library")
	do (define-library-form (pop forms) :implicit-imports implicit-imports))
  (let ((env (program-environment (car forms) :implicit-imports implicit-imports))
	(result nil))
    (dolist (form (cdr forms) (values result env))
      (if on-error
	  (handler-case (setq result (funcall *library-evaluator* form env))
	    (error (e) (funcall on-error form e)))
	  (setq result (funcall *library-evaluator* form env))))))

(defun load-program (path &rest keys)
  (apply #'run-program (read-forms-from-file path) keys))
