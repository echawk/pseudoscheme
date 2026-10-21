; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-GUILE -*-

;;;; Tree-IL, Guile's expanded code, compiled to Quasischeme's core
;;;; Scheme (docs/guile.md, stage 2), and evaluation.
;;;;
;;;; Guile's psyntax builds Tree-IL with the constructors in
;;;; %expanded-vtables: structs of 18 types (void, const, lexical-ref,
;;;; toplevel-ref, call, lambda-case, letrec, ...).  COMPILE-TREE-IL
;;;; makes core Scheme of them -- lambda, if, set!, begin, quote, calls --
;;;; which src/psyntax.lisp's HOST-EVAL translates and compiles, full
;;;; continuations and all.
;;;;
;;;;   lexical variables  are renamed apart (~1, ~2, ...), so that they
;;;;                      never shadow the host procedures the code calls;
;;;;   top-level and      are calls to %guile-ref etc. on a SITE: the
;;;;   module variables   module and name, and the variable once it's
;;;;                      been looked up, as Guile's compiled code caches
;;;;                      its variables;
;;;;   lambda-case        with optional and keyword arguments and several
;;;;                      clauses becomes a lambda of a rest list that
;;;;                      dispatches on its length and binds in order;
;;;;   primcall           is the host's procedure where Guile's is the
;;;;                      same (car, cons, apply, ...), which the
;;;;                      translator can open-code; else a call of the
;;;;                      (guile) module's binding, as Guile does.
;;;;
;;;; Before psyntax is loaded, `macroexpand' is PRE-EXPAND, standing in
;;;; for libguile's own expander (expand.c): it knows the core forms
;;;; boot-9's first pages and psyntax-pp.scm use, and makes the same
;;;; Tree-IL.

(in-package "QUASISCHEME-GUILE")

;;; ------------------------------------------------------------------
;;; Tree-IL nodes

(defparameter *tree-il-types*
  '(("void" "src") ("const" "src" "exp") ("primitive-ref" "src" "name")
    ("lexical-ref" "src" "name" "gensym") ("lexical-set" "src" "name" "gensym" "exp")
    ("module-ref" "src" "mod" "name" "public?") ("module-set" "src" "mod" "name" "public?" "exp")
    ("toplevel-ref" "src" "mod" "name") ("toplevel-set" "src" "mod" "name" "exp")
    ("toplevel-define" "src" "mod" "name" "exp")
    ("conditional" "src" "test" "consequent" "alternate") ("call" "src" "proc" "args")
    ("primcall" "src" "name" "args") ("seq" "src" "head" "tail") ("lambda" "src" "meta" "body")
    ("lambda-case" "src" "req" "opt" "rest" "kw" "inits" "gensyms" "body" "alternate")
    ("let" "src" "names" "gensyms" "vals" "body")
    ("letrec" "src" "in-order?" "names" "gensyms" "vals" "body"))
  "%expanded-vtables, in Guile's order: each type's stem and fields.")

(defvar *expanded-vtables*
  (let ((meta (make-struct* *standard-vtable*
			    (list (layout-symbol (concatenate 'string *standard-vtable-fields* "pwuwpw"))
				  qs:false))))
    (coerce (loop for (stem . fields) in *tree-il-types*
		  collect (make-struct* meta
					(list (layout-symbol (apply #'concatenate 'string
								    (mapcar (constantly "pw") fields)))
					      qs:false (ssym stem) (length fields)
					      (mapcar #'ssym fields))))
	    'simple-vector)))

(defvar *node-types*
  (let ((table (make-hash-table :test 'eq)))
    (loop for v across *expanded-vtables* for (stem) in *tree-il-types*
	  do (setf (gethash v table) (intern (string-upcase stem) "KEYWORD")))
    table))

(declaim (inline node-field))
(defun node-type (x)
  "X's Tree-IL type, a keyword; or NIL if it isn't Tree-IL.  Besides
psyntax's 18 core types, (language tree-il) defines <fix>, <let-values>,
<prompt> and <abort> as records, src first."
  (and (gstruct-p x)
       (or (gethash (gstruct-vtable x) *node-types*)
	   (let ((name (svref (gstruct-slots (gstruct-vtable x)) +vtable-index-name+)))
	     (and (symbolp name)
		  (cdr (assoc (qs:scheme-symbol-name name)
			      '(("<fix>" . :fix) ("<let-values>" . :let-values)
				("<prompt>" . :prompt) ("<abort>" . :abort))
			      :test #'string=)))))))
(defun node-field (x i) (svref (gstruct-slots x) i))

(defun make-node (type &rest fields)
  (%make-gstruct (svref *expanded-vtables* (position type *tree-il-types* :key #'car :test #'string-equal))
		 (coerce fields 'simple-vector)))

;;; ------------------------------------------------------------------
;;; Sites: the variables top-level code refers to

(defstruct (site (:constructor make-site (module name public kind)) (:copier nil))
  module				; a module, or a module name (module-ref), or #f
  name
  public
  kind					; :toplevel or :module
  (variable nil))

(defmethod print-object ((s site) stream)
  (format stream "#<site ~A>" (qs:scheme-symbol-name (site-name s))))

;; Sites are compiled as quoted constants, and compiled code isn't put
;; in files, so they need no load form; this is for the compiler's
;; coalescing of constants, which mustn't merge two of them.
(defmethod make-load-form ((s site) &optional environment)
  (declare (ignore environment))
  (error "a Guile site can't be dumped"))

(defun site-lookup (site)
  "SITE's variable, or NIL if it isn't bound."
  (ecase (site-kind site)
    (:toplevel (module-variable* (site-module site) (site-name site)))
    (:module (let ((module (resolve-module* (site-module site))))
	       (when (and module (site-public site))
		 (setq module (module-public-interface* module)))
	       (and module (module-variable* module (site-name site)))))))

(defvar *unbound-variable-hook* nil
  "NIL, or a function of a name and its module, called as that top-level
variable is found unbound, before the error: a file being loaded makes
a later definition first (boot.lisp's EVAL-LOAD-FORM).")

(defun signal-unbound-variable (name &optional module)
  (when (and *unbound-variable-hook* module)
    (funcall *unbound-variable-hook* name module))
  (guile-error (ssym "unbound-variable") qs:false "Unbound variable: ~S" (list name) qs:false))

(defun site-ref-slowly (site)
  (let ((v (or (site-variable site)
	       (let ((v (site-lookup site)))
		 (unless v (signal-unbound-variable (site-name site) (and (eq (site-kind site) :toplevel) (site-module site))))
		 (setf (site-variable site) v)))))
    (let ((x (gvariable-value v)))
      (when (eq x +unbound+) (signal-unbound-variable (site-name site) (and (eq (site-kind site) :toplevel) (site-module site))))
      x)))

(declaim (inline site-ref))
(defun site-ref (site)
  ;; the fast path of every top-level reference: the variable found
  ;; already, and bound
  (declare (optimize speed (safety 0)) (type site site))
  (let ((v (site-variable site)))
    (if v
	(let ((x (gvariable-value (the gvariable v))))
	  (if (eq x **unbound**) (site-ref-slowly site) x))
	(site-ref-slowly site))))

(defun site-set (site value)
  (let ((v (or (site-variable site)
	       (let ((v (site-lookup site)))
		 (unless v (signal-unbound-variable (site-name site) (and (eq (site-kind site) :toplevel) (site-module site))))
		 (setf (site-variable site) v)))))
    (setf (gvariable-value v) value)
    *unspecified*))

;;; ------------------------------------------------------------------
;;; Compiling Tree-IL to core Scheme

(defvar *lexicals* nil "gensym -> the core variable it's renamed to")
(defvar *lexical-count* 0)
(defvar *compile-module* nil "The module the form being compiled is in.")

(defun core (name) (ssym name))

(defvar *lexical-tag* "b"
  "Part of each lexical name, new in each process: big top-level forms'
lexicals become global definitions (FLATTEN-TOP-LEVEL-LETS, psyntax's
hoisting), and those of a cached module's fasl (cache.lisp), of the
image and of this process mustn't collide.")

(defun new-lexical-tag ()
  (setq *lexical-tag* (format nil "~36R" (random (expt 36 6) (make-random-state t)))))

(pushnew 'new-lexical-tag sb-ext:*init-hooks*)

(defun fresh-lexical ()
  (qsx::make-lexical (qs::invert-case (format nil "~~~D.~A" (incf *lexical-count*) *lexical-tag*))))

(defvar *unassigned* '()
  "The gensyms of letrec variables not yet assigned where the code being
compiled runs: a read of one checks.")

(defun lexical (gensym)
  (or (gethash gensym *lexicals*)
      ;; free in the node being compiled: can't happen in psyntax's output
      (error "Tree-IL: unbound lexical ~S" gensym)))

(defun bind-lexical (gensym)
  (setf (gethash gensym *lexicals*) (fresh-lexical)))

(defun node-module (x)
  "The module a toplevel node refers to: the one psyntax expanded it in
(its mod field, which a define-module earlier in the same top-level
begin changes), else the one being compiled for."
  (let ((name (node-field x 1)))
    (or (and (consp name)
	     (module-p* *compile-module*)
	     (not (equal name (module-slot *compile-module* +module-name+)))
	     ;; an existing module only: resolving here mustn't load one
	     (let* ((resolve (root-value "resolve-module"))
		    (m (and resolve (ignore-errors
				     (funcall resolve name qs:false qs:false :ensure qs:false)))))
	       (and m (truthy m) (module-p* m) m)))
	*compile-module*)))

(defvar *literals-read-only* nil
  "True while compiling for (compile ... #:to 'value): string constants
are made read-only, as Guile's compiler makes them.")

(defun quoted (x) (list (core "quote") x))

(defparameter *open-primitives*
  '("car" "cdr" "cons" "list" "append" "vector" "list->vector" "apply" "values"
    "call-with-values" "call-with-current-continuation" "eq?" "eqv?" "equal?"
    "pair?" "string?" "vector?" "procedure?" "char?" "number?" "integer?" "zero?"
    "+" "-" "*" "<" ">" "<=" ">=" "=" "vector-ref" "vector-set!" "vector-length"
    "string-ref" "string-length" "length" "memq" "memv" "member" "assq" "assv" "assoc"
    "caar" "cadr" "cdar" "cddr" "caddr" "cdddr" "cadddr" "set-car!" "set-cdr!" "reverse"
    "list?" "boolean?" "char=?" "quotient" "remainder" "modulo" "map" "for-each"
    "call-with-prompt" "abort-to-prompt" "dynamic-wind")
  "Guile primitives that are Quasischeme's host procedures of the same
name and behaviour: a primcall of one compiles to a call of the host's.")

(defparameter *primcall-renames*
  '(("call/cc" . "call-with-current-continuation") ("abort-to-prompt*" . "%guile-abort*"))
  "Guile primitives whose host procedure has another name.")

(defun primitive-operator (name)
  "The core expression for Guile primitive NAME (a symbol)."
  (let* ((string (qs:scheme-symbol-name name))
	 (rename (cdr (assoc string *primcall-renames* :test #'string=))))
    (cond (rename (core rename))
	  ((goops-generic-primitive-p string)
	   ;; GOOPS may give it methods, by rebinding its root variable
	   (root-reference name))
	  ((member string *open-primitives* :test #'string=) (core string))
	  ((member string '("wind" "unwind" "push-fluid" "pop-fluid" "push-dynamic-state"
			    "pop-dynamic-state")
		   :test #'string=)
	   ;; only boot-9's dynamic-wind, with-fluid* and with-dynamic-state
	   ;; use these, and they are replaced (BOOT-OVERRIDES)
	   (list (core "%guile-unsupported") (quoted name)))
	  ((and (not (obarray-variable name)) (primitive-module string))
	   (list (core "%guile-ref")
		 (quoted (make-site (mapcar #'ssym (primitive-module string)) name t :module))))
	  (t (root-reference name)))))

(defun primitive-module (name)
  "The module that exports primitive NAME (a string), if not (guile): as
(language tree-il primitives) has it, whose passes make primcalls of
references to these."
  (flet ((prefix-p (prefix) (and (>= (length name) (length prefix))
				 (string= prefix name :end2 (length prefix)))))
    (cond ((or (prefix-p "bytevector") (member name '("string->utf8" "utf8->string") :test #'string=))
	   '("rnrs" "bytevectors"))
	  ((or (prefix-p "make-atomic-box") (prefix-p "atomic-box")) '("ice-9" "atomic"))
	  ((string= name "current-thread") '("ice-9" "threads"))
	  ((string= name "class-of") '("oop" "goops"))
	  ((some (lambda (type) (prefix-p (format nil "~Avector-" type)))
		 '("u8" "s8" "u16" "s16" "u32" "s32" "u64" "s64" "f32" "f64"))
	   '("srfi" "srfi-4")))))

(defun root-reference (name)
  "A reference to NAME in the (guile) module."
  (list (core "%guile-ref") (quoted (make-site :root name nil :toplevel))))

;;; Calls of the root module's primitives by name: Guile's resolve-primitives
;;; pass turns (+ a b), where + is the (guile) module's variable, into a
;;; primcall; Quasischeme's native compiler doesn't run that pass, so this
;;; does the same for the hot ones.  Without it every + goes through
;;; %guile-ref and a generic, rest-argument procedure.  As Guile's, it is
;;; decided when the call is compiled: the module's variable is the root
;;; module's then, and not a later redefinition's.

(defparameter *resolvable-primitives*
  '(("+" 0 nil) ("*" 0 nil) ("-" 1 2) ("<" 2 2) (">" 2 2) ("<=" 2 2) (">=" 2 2) ("=" 2 2)
    ("car" 1 1) ("cdr" 1 1) ("cadr" 1 1) ("cddr" 1 1) ("caar" 1 1) ("cdar" 1 1) ("caddr" 1 1)
    ("cdddr" 1 1) ("cadddr" 1 1) ("pair?" 1 1) ("vector?" 1 1) ("string?" 1 1)
    ("procedure?" 1 1) ("char?" 1 1) ("number?" 1 1) ("integer?" 1 1) ("zero?" 1 1)
    ("null?" 1 1) ("not" 1 1) ("vector-length" 1 1) ("string-length" 1 1)
    ("cons" 2 2) ("eq?" 2 2) ("eqv?" 2 2) ("vector-ref" 2 2) ("string-ref" 2 2)
    ("set-car!" 2 2) ("set-cdr!" 2 2) ("char=?" 2 2) ("vector-set!" 3 3)
    ("memq" 2 2) ("memv" 2 2) ("length" 1 1) ("append" 2 2) ("list" 0 nil))
  "Root primitives a call of which is made a primcall, with their numbers of
arguments allowed: the others stay calls of the variable.  Not boolean?,
which the host's has #f for #nil.  All are in Guile's
*interesting-primitive-names*, which its pass resolves.")

(defun existing-module (name public)
  "The module named NAME, or its public interface if PUBLIC, if it exists:
resolving it mustn't load one (or NIL)."
  (let* ((resolve (root-value "resolve-module"))
	 (m (and resolve (consp name)
		 (ignore-errors (funcall resolve name qs:false qs:false :ensure qs:false)))))
    (and m (truthy m) (module-p* m)
	 (if public (module-public-interface* m) m))))

(defvar *resolve-primitives* t
  "Whether calls of the root module's primitives compile to primcalls.")

(defun resolved-primitive (f args)
  "The name (a symbol) of the root primitive that the call (F . ARGS)
calls, if F is a reference to the root module's variable of a primitive in
*RESOLVABLE-PRIMITIVES* and ARGS fit it; else NIL."
  (when (and *resolve-primitives* (member (node-type f) '(:toplevel-ref :module-ref)))
    (let* ((name (node-field f 2))
	   (entry (and (qs:scheme-symbol-p name)
		       (assoc (qs:scheme-symbol-name name) *resolvable-primitives* :test #'string=))))
      (when entry
	(let ((n (length args)))
	  (when (and (>= n (second entry)) (or (null (third entry)) (<= n (third entry)))
		     (not (goops-generic-primitive-p (car entry))))
	    ;; (@@ (guile) memv), say, from a macro of boot-9's (case): Guile's
	    ;; pass resolves a module reference too
	    (let ((root (obarray-variable name))
		  (module (if (eq (node-type f) :module-ref)
			      (existing-module (node-field f 1) (truthy (node-field f 3)))
			      (node-module f))))
	      (and root (not (eq module :root)) (module-p* module)
		   (eq root (module-variable* module name))
		   name))))))))

(defun compile-resolved-primitive (name args)
  (let ((string (qs:scheme-symbol-name name)))
    (if (and (string= string "-") (= (length args) 2)
	     (not (and (eq (node-type (second args)) :const) (rationalp (node-field (second args) 1)))))
	;; exact 0 minus a flonum zero is -0.0 in Guile's (see "-" in runtime.lisp)
	(list (core "%guile-sub2") (compile-tree-il (first args)) (compile-tree-il (second args)))
	(let ((helper (cdr (assoc string '(("<" . "%guile-<") (">" . "%guile->") ("<=" . "%guile-<=")
					   (">=" . "%guile->=") ("=" . "%guile-=")
					   ("null?" . "%guile-null?") ("not" . "%guile-not"))
				  :test #'string=))))
	  ;; not of a predicate's value, which can't be #nil: the host's
	  (when (and (string= string "not") (boolean-node-p (first args)))
	    (setq helper nil))
	  (cond ((and (string= string "append") (not (goops-generic-primitive-p string)))
		 (setq helper "%guile-append2"))
		((and (member string '("memq" "memv") :test #'string=)
		      (constant-member-list-p (second args) (string= string "memv")))
		 (return-from compile-resolved-primitive
		   (compile-constant-member (first args) (node-field (second args) 1))))
		((member string '("memq" "memv" "length") :test #'string=)
		 ;; the procedures that take #nil as a list's end (elisp.lisp)
		 (return-from compile-resolved-primitive
		   (cons (primitive-operator name) (mapcar #'compile-tree-il args)))))
	  ;; the comparisons report a wrong type as Guile's do: the root
	  ;; procedure's, after the fixnum case
	  (cons (core (or helper string)) (mapcar #'compile-tree-il args))))))

(defparameter *constant-member-limit* 12
  "The longest constant list that memq or memv of is made tests in line.")

(defun constant-member-list-p (node eqv)
  "Whether NODE is a constant short proper list, of objects that eq? tells
apart as eqv? does if EQV."
  (and (eq (node-type node) :const)
       (let ((list (node-field node 1)))
	 (and (consp list) (proper-list-p* list) (<= (length list) *constant-member-limit*)
	      (or (not eqv)
		  (every (lambda (x) (or (symbolp x) (typep x 'fixnum) (characterp x)
					 (eq x qs:false) (eq x qs:true)))
			 list))))))

(defun compile-constant-member (key list)
  ;; (memv k '(a b)), as Guile's peval makes it: a test of each element,
  ;; whose value is the list's own tail
  (let ((v (fresh-lexical)))
    (list (list (core "lambda") (list v)
		(labels ((tests (tail)
			   (if (null tail)
			       (quoted qs:false)
			       (list (core "if") (list (core "eq?") v (quoted (car tail)))
				     (quoted tail)
				     (tests (cdr tail))))))
		  (tests list)))
	  (compile-tree-il key))))

(defun compile-tree-il (x)
  (ecase (node-type x)
    (:void (quoted *unspecified*))
    (:const (let ((value (node-field x 1)))
	      ;; compile's string literals are constant, as Guile's compiled code's
	      (quoted (cond ((not *literals-read-only*) value)
			    ((stringp value) (read-only-string value))
			    ((and (simple-vector-p value) (plusp (length value))) (read-only-vector value))
			    ((and (typep value '(simple-array (unsigned-byte 8) (*))) (plusp (length value)))
			     (read-only-bytevector value))
			    (t value)))))
    (:primitive-ref (primitive-operator (node-field x 1)))
    (:lexical-ref
     (let ((gensym (node-field x 2)))
       (if (member gensym *unassigned* :test #'eq)
	   ;; a letrec variable read by an init before it's assigned
	   (list (core "%guile-assigned") (lexical gensym) (quoted (node-field x 1)))
	   (lexical gensym))))
    (:lexical-set (list (core "set!") (lexical (node-field x 2)) (compile-tree-il (node-field x 3))))
    (:module-ref
     (list (core "%guile-ref")
	   (quoted (make-site (node-field x 1) (node-field x 2) (truthy (node-field x 3)) :module))))
    (:module-set
     (list (core "%guile-set!")
	   (quoted (make-site (node-field x 1) (node-field x 2) (truthy (node-field x 3)) :module))
	   (compile-tree-il (node-field x 4))))
    (:toplevel-ref
     (list (core "%guile-ref") (quoted (make-site (node-module x) (node-field x 2) nil :toplevel))))
    (:toplevel-set
     (list (core "%guile-set!") (quoted (make-site (node-module x) (node-field x 2) nil :toplevel))
	   (compile-tree-il (node-field x 3))))
    (:toplevel-define
     (list (core "%guile-define!") (quoted (node-module x)) (quoted (node-field x 2))
	   (let ((value (node-field x 3)))
	     ;; a procedure defined at top level is named, as Guile's are
	     (if (eq (node-type value) :lambda)
		 (with-lambda-meta (node-field value 1)
				   (name-lambda (node-field value 1) (compile-lambda (node-field value 2)))
				   t)
		 (compile-tree-il value)))))
    (:conditional
     (list (core "if") (compile-test (node-field x 1)) (compile-tree-il (node-field x 2))
	   (compile-tree-il (node-field x 3))))
    (:call (let* ((f (node-field x 1)) (args (node-field x 2))
		  (resolved (resolved-primitive f args))
		  (body (inline-body-position f (length args))))
	     (when resolved
	       (return-from compile-tree-il (compile-resolved-primitive resolved args)))
	     ;; a prompt's or fluid binding's body: inline in Guile
	     (cons (compile-tree-il f)
		   (loop for a in args for i from 0
			 collect (if (and (eql i body) (eq (node-type a) :lambda))
				     (inline-thunk (compile-tree-il a))
				     (compile-tree-il a))))))
    (:primcall (let ((name (node-field x 1)) (args (node-field x 2)))
		 (cons (if (or (and (/= (length args) 2)
				    (member (qs:scheme-symbol-name name) '("eq?" "eqv?" "equal?") :test #'string=))
			       (and (< (length args) 2)
				    (member (qs:scheme-symbol-name name) '("=" "<" ">" "<=" ">=") :test #'string=)))
			   ;; Guile's take any number of arguments; the host's two
			   (root-reference name)
			   (primitive-operator name))
		       (mapcar #'compile-tree-il args))))
    (:seq (let ((forms '()))
	    (loop while (eq (node-type x) :seq)
		  do (push (compile-tree-il (node-field x 1)) forms)
		     (setq x (node-field x 2)))
	    (push (compile-tree-il x) forms)
	    (cons (core "begin") (nreverse forms))))
    (:lambda (let ((*unassigned* '()))	; a lambda's body runs later
	       (with-lambda-meta (node-field x 1) (name-lambda (node-field x 1) (compile-lambda (node-field x 2))))))
    (:let (let ((vals (mapcar #'compile-tree-il (node-field x 3)))
		(vars (mapcar #'bind-lexical (node-field x 2))))
	    (cons (list (core "lambda") vars (compile-tree-il (node-field x 4))) vals)))
    (:fix (compile-letrec (node-field x 2) (node-field x 3) (node-field x 4)))
    (:let-values
     (list (core "call-with-values")
	   (list (core "lambda") '() (compile-tree-il (node-field x 1)))
	   (compile-lambda (node-field x 2))))
    (:prompt				; the body is a thunk, inline in Guile
     (list (core "call-with-prompt") (compile-tree-il (node-field x 2))
	   (inline-thunk (compile-tree-il (node-field x 3))) (compile-tree-il (node-field x 4))))
    (:abort
     (list* (core "apply") (core "abort-to-prompt") (compile-tree-il (node-field x 1))
	    (append (mapcar #'compile-tree-il (node-field x 2))
		    (list (compile-tree-il (node-field x 3))))))
    (:letrec (compile-letrec (node-field x 3) (node-field x 4) (node-field x 5)
			     (eq (node-field x 1) qs:false)))))

(defun compile-letrec (gensyms vals body &optional parallel)
  ;; as psyntax's letrec* for the translator: ((lambda (v ...) (set! v e) ... body) #f ...),
  ;; which src/psyntax.lisp makes a LABELS when the e are lambdas.  A
  ;; letrec (PARALLEL) whose inits aren't all lambdas evaluates them all
  ;; before assigning any, so that an init returning twice (by a
  ;; continuation) assigns every variable again (R7RS 4.2.2).
  (when (and parallel (cdr vals) (notevery (lambda (v) (eq (node-type v) :lambda)) vals))
    (let* ((vars (mapcar #'bind-lexical gensyms))
	   (temps (mapcar (lambda (v) (declare (ignore v)) (fresh-lexical)) vars))
	   ;; every init runs before any variable is assigned
	   (vals (let ((*unassigned* (append gensyms *unassigned*)))
		   (mapcar #'compile-tree-il vals))))
      (return-from compile-letrec
	(cons (list (core "lambda") vars
		    (cons (list (core "lambda") temps
				(list* (core "begin")
				       (append (mapcar (lambda (v tmp) (list (core "set!") v tmp)) vars temps)
					       (list (compile-tree-il body)))))
			  vals))
	      (mapcar (constantly (quoted qs:unassigned)) vars)))))
  (let* ((vars (mapcar #'bind-lexical gensyms))
	 (all-lambdas (every (lambda (v) (eq (node-type v) :lambda)) vals))
	 ;; letrec*: an init may read the variables before it, not the
	 ;; ones from its own on (reads inside lambdas, which run later,
	 ;; aren't checked)
	 (vals (loop for v in vals
		     for rest on gensyms
		     collect (let ((*unassigned* (if all-lambdas *unassigned* (append rest *unassigned*))))
			       (compile-tree-il v)))))
    (cons (list (core "lambda") vars
		(list* (core "begin")
		       (append (mapcar (lambda (v e) (list (core "set!") v e)) vars vals)
			       (list (compile-tree-il body)))))
	  ;; the LABELS case (all lambdas) as psyntax makes it, with #f
	  (mapcar (constantly (quoted (if all-lambdas qs:false qs:unassigned))) vars))))

(defun compile-test (x)
  "Test X of a conditional.  Emacs Lisp's nil, #nil, is false to Guile's
if, though it isn't #f: a test that might be #nil goes through
%guile-true, which makes it #f.  The calls of predicates (name?) and
comparisons can't return it."
  (let ((core (compile-tree-il x)))
    (if (boolean-node-p x) core (list (core "%guile-true") core))))

(defun boolean-node-p (x)
  (flet ((predicate-name-p (name)
	   (and (symbolp name)
		(let ((s (qs:scheme-symbol-name name)))
		  (or (char= (char s (1- (length s))) #\?)
		      (member s '("not" "<" ">" "<=" ">=" "=") :test #'string=))))))
    (case (node-type x)
      (:const (not (eq (node-field x 1) *elisp-nil*)))
      ((:lambda :void) t)
      (:primcall (predicate-name-p (node-field x 1)))
      (:call (let ((f (node-field x 1)))
	       (case (node-type f)
		 (:primitive-ref (predicate-name-p (node-field f 1)))
		 ((:toplevel-ref :module-ref) (predicate-name-p (node-field f 2))))))
      (:conditional (and (boolean-node-p (node-field x 2)) (boolean-node-p (node-field x 3))))
      (t nil))))

(defun inline-body-position (f nargs)
  "Which argument of a call of F with NARGS arguments is a body Guile
compiles inline: call-with-prompt's thunk, with-fluid*'s."
  (let ((name (case (node-type f)
		((:toplevel-ref :module-ref) (qs:scheme-symbol-name (node-field f 2)))
		(:primitive-ref (qs:scheme-symbol-name (node-field f 1))))))
    (cond ((null name) nil)
	  ((and (string= name "call-with-prompt") (= nargs 3)) 1)
	  ((and (member name '("with-fluid*" "with-dynamic-state") :test #'string=) (plusp nargs))
	   (1- nargs)))))

(defun inline-thunk (thunk)
  "THUNK, compiled, marked as code that is inline in its caller in Guile
(a prompt's or fluid binding's body), whose frames a backtrace leaves out."
  (list (core "%guile-inline")
	(name-lambda (list (cons (ssym "name") (ssym +inline-body-name+))) thunk)))

(defparameter +inline-body-name+ "%inline-body"
  "The name an inline thunk's machine is made with (NAME-LAMBDA).")

(defparameter *frame-keeping-procedures*
  '("call-with-prompt" "with-fluid*" "with-fluids*" "with-dynamic-state")
  "Procedures that are instructions in the caller in Guile (a prompt, a
fluid binding), whose frame stays while their body runs: a tail call of
one keeps the caller's frame, for backtraces (src/continuations.lisp).")

(defun keep-frame-operator-p (op)
  (let ((name (cond ((and (consp op) (symbolp (car op))
			  (string-equal (symbol-name (car op)) "%guile-ref")
			  (qsx::quote-form-p (cadr op)) (site-p (cadr (cadr op))))
		     (let ((n (site-name (cadr (cadr op))))) (and (symbolp n) (qs:scheme-symbol-name n))))
		    ((symbolp op) (string-downcase (symbol-name op)))
		    (t (qsx::primitive-name op)))))
    (and name (member name *frame-keeping-procedures* :test #'string=) t)))

(setq qsx::*keep-frame-operator-p* 'keep-frame-operator-p)

(defun name-lambda (meta lambda)
  "LAMBDA, a compiled lambda, with the name META gives it (if any) as the
first expression of its body: a constant, '(:procedure-name . name),
which the transformation for full continuations makes the name of its
machine, for backtraces (src/continuations.lisp), and which otherwise
does nothing."
  (let ((name (loop for entry in (if (listp meta) meta '())
		    when (and (consp entry) (symbolp (car entry))
			      (string= (qs:scheme-symbol-name (car entry)) "name")
			      (symbolp (cdr entry)) (cdr entry))
		      return (cdr entry))))
    ;; a lambda with properties (an arglist): the lambda inside named
    (when (and (consp lambda) (eq (car lambda) (core "%guile-with-meta")))
      (return-from name-lambda
	(list* (car lambda) (name-lambda meta (second lambda)) (cddr lambda))))
    (if (and name (consp lambda) (eq (car lambda) (core "lambda")) (consp (cddr lambda)))
	(let ((body (caddr lambda)) (marker (quoted (cons :procedure-name name))))
	  ;; into a sequence, not around it: long sequences are cut into
	  ;; chunks (src/continuations.lisp), which a nested one wouldn't be
	  (list (core "lambda") (cadr lambda)
		(if (and (consp body) (eq (car body) (core "begin")))
		    (list* (core "begin") marker (cdr body))
		    (list (core "begin") marker body))))
	lambda)))

(defun with-lambda-meta (meta lambda &optional named)
  "LAMBDA, compiled, given the properties in META, the lambda's metadata
alist, that procedure-documentation and procedure-name read: its
documentation, and if NAMED its name."
  (let ((props (loop for entry in (if (listp meta) meta '())
		     when (and (consp entry) (symbolp (car entry))
			       (let ((key (qs:scheme-symbol-name (car entry))))
				 (or (string= key "documentation")
				     (and named (string= key "name")))))
		       collect entry)))
    (cond ((null props) lambda)
	  ;; one with properties already (an arglist): all of them
	  ((and (consp lambda) (eq (car lambda) (core "%guile-with-meta")))
	   (list (core "%guile-with-meta") (second lambda)
		 (quoted (append props (cadr (third lambda))))))
	  (t (list (core "%guile-with-meta") lambda (quoted props))))))

(defun compile-lambda (case)
  (cond ((not (node-type case))
	 ;; no clauses: an error whatever the arguments
	 (list (core "lambda") (core "~args") (list (core "%guile-arity-error"))))
	((and (simple-case-p case) (not (node-type (node-field case 8))))
	 (let* ((req (length (node-field case 1)))
		(gensyms (node-field case 6))
		(vars (mapcar #'bind-lexical gensyms))
		(formals (if (truthy (node-field case 3))
			     (append (subseq vars 0 req) (nth req vars))
			     vars)))
	   (list (core "lambda") formals (compile-tree-il (node-field case 7)))))
	(t (let* ((args (fresh-lexical))
		  (lambda (list (core "lambda") args (compile-clauses case args)))
		  (arglist (if (not (node-type (node-field case 8)))
			       (clause-arglist case)
			       (clauses-arglist case))))
	     ;; one clause with optional or keyword arguments: its shape, for
	     ;; procedure-arguments and procedure-minimum-arity
	     (if arglist
		 (list (core "%guile-with-meta") lambda (quoted (list (cons (ssym "arglist") arglist))))
		 lambda)))))

(defun clause-arglist (case)
  "CASE's arguments as Guile's arglist property gives them: (required
optional keywords allow-other-keys? rest), the counts of required and
optional ones, keywords as (keyword . index)."
  (let* ((req (length (node-field case 1)))
	 (opt (if (truthy (node-field case 2)) (length (node-field case 2)) 0))
	 (rest (node-field case 3))
	 (kw-spec (node-field case 4))
	 (gensyms (node-field case 6)))
    (list req opt
	  (if (truthy kw-spec)
	      (loop for (keyword nil var) in (cdr kw-spec)
		    collect (cons keyword (or (position var gensyms) 0)))
	      '())
	  (if (and (truthy kw-spec) (truthy (car kw-spec))) qs:true qs:false)
	  (if (truthy rest) rest qs:false))))

(defun clauses-arglist (case)
  "The arglist of a case-lambda: the arities of all its clauses, as one
(so that Guile's analyzer's arity check, through procedure-minimum-arity,
takes every clause's calls): the fewest required arguments, optional ones
up to the most any clause takes, a rest argument if any clause has one."
  (let ((min-req nil) (max-args 0) (rest nil))
    (loop for c = case then (node-field c 8)
	  while (node-type c)
	  do (let* ((req (length (node-field c 1)))
		    (opt (if (truthy (node-field c 2)) (length (node-field c 2)) 0)))
	       (setq min-req (if min-req (min min-req req) req)
		     max-args (max max-args (+ req opt)))
	       (when (or (truthy (node-field c 3)) (truthy (node-field c 4)))
		 (setq rest (or rest (if (truthy (node-field c 3)) (node-field c 3) (ssym "rest")))))))
    (list (or min-req 0) (- max-args (or min-req 0)) '() qs:false (or rest qs:false))))

(defun simple-case-p (case)
  (and (not (truthy (node-field case 2))) (not (truthy (node-field case 4)))))

(defun compile-clauses (case args)
  (if (not (node-type case))
      (list (core "%guile-arity-error"))
      (let* ((req (length (node-field case 1)))
	     (opt (if (truthy (node-field case 2)) (length (node-field case 2)) 0))
	     (rest (truthy (node-field case 3)))
	     (kw (truthy (node-field case 4)))
	     (test (cond (rest (list (core "%guile-nargs>=") args req))
			 ;; and, if another clause follows, no more positional
			 ;; arguments than it takes
			 ((and kw (node-type (node-field case 8)))
			  (list (core "%guile-keywords-fit?") args req opt))
			 (kw (list (core "%guile-nargs>=") args req))
			 (t (list (core "%guile-nargs-between") args req (+ req opt))))))
	(list (core "if") test
	      (compile-clause case args)
	      (compile-clauses (node-field case 8) args)))))

(defun compile-clause (case args)
  "Bind CASE's variables from the list ARGS, in order: required,
optional, rest, keywords, each optional's and keyword's initializer
evaluated in the scope of those before it."
  (let* ((req (length (node-field case 1)))
	 (opt (if (truthy (node-field case 2)) (length (node-field case 2)) 0))
	 (rest (truthy (node-field case 3)))
	 (kw-spec (node-field case 4))
	 (kw (and (truthy kw-spec) (cdr kw-spec)))
	 (allow-other-keys (and (truthy kw-spec) (truthy (car kw-spec))))
	 (inits (node-field case 5))
	 (gensyms (node-field case 6))
	 (body (node-field case 7)))
    (labels ((bind (var value tail-var tail-value inner)
	       ;; ((lambda (var tail-var) inner) value tail-value)
	       (list (list (core "lambda") (list var tail-var) inner) value tail-value))
	     (bind-from (i tail)
	       (cond ((< i req)
		      (let ((var (bind-lexical (pop gensyms))) (next (fresh-lexical)))
			(bind var (list (core "car") tail) next (list (core "cdr") tail)
			      (bind-from (1+ i) next))))
		     ((< i (+ req opt))
		      (let* ((init (compile-tree-il (pop inits)))
			     (var (bind-lexical (pop gensyms)))
			     (next (fresh-lexical))
			     (present (if kw
					  (list (core "%guile-positional?") tail)
					  (list (core "pair?") tail))))
			(bind var (list (core "if") present (list (core "car") tail) init)
			      next (list (core "if") present (list (core "cdr") tail) tail)
			      (bind-from (1+ i) next))))
		     (t (keywords tail))))
	     (keywords (tail)
	       (let ((rest-var (and rest (bind-lexical (pop gensyms))))
		     (check (cond (kw (list (core "%guile-check-keywords") tail
					    (quoted (mapcar #'car kw))
					    (quoted (if allow-other-keys qs:true qs:false))
					    (quoted (if rest qs:true qs:false))))
				  ((not rest) (list (core "%guile-check-no-more") tail)))))
		 (let ((inner (bind-keywords tail)))
		   (when rest-var
		     (setq inner (list (list (core "lambda") (list rest-var) inner) tail)))
		   (if check (list (core "begin") check inner) inner))))
	     (bind-keywords (tail)
	       (if (null kw)
		   (compile-tree-il body)
		   (let* ((spec (pop kw))
			  (init (compile-tree-il (pop inits)))
			  (var (bind-lexical (pop gensyms)))
			  (value (fresh-lexical)))
		     ;; ((lambda (value) ((lambda (var) inner) (if (eq? value unbound) init value)))
		     ;;  (%guile-keyword-ref tail 'kw))
		     (list (list (core "lambda") (list value)
				 (list (list (core "lambda") (list var) (bind-keywords tail))
				       (list (core "if")
					     (list (core "eq?") value (quoted +unbound+))
					     init value)))
			   (list (core "%guile-keyword-ref") tail (quoted (car spec))))))))
      (bind-from 0 args))))

;;; Helpers the compiled code calls

;;; Cross-form knowledge for the continuation transformation
;;; (src/continuations.lisp, *SAFE-OPERATOR-HOOK*): a reference to a
;;; top-level variable of a declarative module (Guile's default: its
;;; bindings are defined once, which Guile's own compiler relies on too)
;;; that holds a procedure proved unable to capture is safe to call
;;; without a frame.

(defun site-module-object (site)
  (ecase (site-kind site)
    (:toplevel (site-module site))
    (:module (resolve-module* (site-module site)))))

(defparameter *procedure-parameter-names*
  '("F" "FN" "PROC" "PROCEDURE" "THUNK" "PRED" "PREDICATE" "LESS" "HASH" "ASSOC" "HANDLER"
    "KONS" "CMP" "COMPARE" "TEST" "BEFORE" "AFTER" "BODY" "CONSUMER" "PRODUCER" "READER"
    "WRITER" "GETTER" "SETTER" "INIT-PROC" "PRE-UNWIND-HANDLER" "PROMPT-HANDLER" "K")
  "Parameter names of Guile mode's primitives that are procedures they
may call: a primitive with none of them is taken to call none.")

(defvar *safe-primitive-functions* nil
  "Primitive functions that call no procedure, so can't capture: the
host's that the continuation transformation knows don't, and Guile mode's
own with no parameter named as a procedure.  Made on first use.")

(defun safe-primitive-functions ()
  (or *safe-primitive-functions*
      (let ((table (make-hash-table :test 'eq)))
	;; the root module's values of the host primitives' names (not
	;; QSX:HOST-REF, which would define a name it doesn't find: quote!)
	(maphash (lambda (name kind)
		   (declare (ignore kind))
		   (unless (member name qsx::*calling-primitives* :test #'string=)
		     (let* ((v (module-variable* :root (ssym name)))
			    (f (and v (gvariable-value v))))
		       (when (functionp f) (setf (gethash f table) t)))))
		 (qsx::primitive-names))
	(maphash (lambda (name f)
		   (declare (ignore name))
		   (when (functionp f)
		     (let ((parameters (ignore-errors (sb-kernel:%fun-lambda-list f))))
		       (when (and (listp parameters)
				  (notany (lambda (p)
					    (let ((p (if (consp p) (car p) p)))
					      (and (symbolp p)
						   (member (symbol-name p) *procedure-parameter-names* :test #'string=))))
					  parameters))
			 (setf (gethash f table) t)))))
		 *guile-primitives*)
	(setq *safe-primitive-functions* table))))

(defun raising-procedure-p (value)
  "Whether VALUE is one of the root's procedures that raise a
non-continuable exception (error, throw, scm-error): a handler can't
return to their caller, so a call of one needn't be a capture site, as
--racket's raising natives aren't (src/racket/linklet/compiler.lisp)."
  (and (functionp value)
       (or (eq value *r7rs-error*)
	   (loop for name in '("error" "throw" "scm-error")
		 thereis (eq value (root-value name))))))

(defun safe-site-p (site)
  "Whether SITE's variable holds a procedure that can't capture, and
will: a primitive (Guile's compiler, too, takes the root module's
primitives as they are), or a procedure the continuation transformation
proved safe, defined in a declarative module."
  ;; only what's resolved already: resolving a module may load it, and
  ;; compile, and come back here
  (let ((v (or (site-variable site)
	       (and (eq (site-kind site) :toplevel)
		    (or (eq (site-module site) :root) (module-p* (site-module site)))
		    (module-variable* (site-module site) (site-name site)))
	       (and (eq (site-kind site) :module)
		    (let ((m (existing-module (site-module site) (site-public site))))
		      (and m (module-variable* m (site-name site))))))))
    (and v
	 (let ((value (gvariable-value v)))
	   (and (functionp value)
		(or (gethash value (safe-primitive-functions))
		    (raising-procedure-p value)
		    (and (gethash value qsx::*safe-functions*)
			 (let ((m (cond ((eq (site-module site) :root) qs:false)
					((eq (site-kind site) :module) (or (existing-module (site-module site) nil) qs:false))
					(t (site-module-object site))))
			       (declarative (root-value "module-declarative?")))
			   (and (module-p* m) declarative (truthy (funcall declarative m)))))))))))

(defvar *guarded-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A W of GUARD-DEFINITION (src/continuations.lisp) -> the sites of the
variables whose procedures it calls: it can't capture while each holds
a procedure that can't (%guile-note-guarded).")

(defvar *safety-in-progress* '()
  "The variables GVARIABLE-SAFE-P-SLOWLY is judging: one met again is
taken as safe (procedures that call each other, each guarded).")

(defun gvariable-safe-p-slowly (v x)
  (let ((epoch *procedure-epoch*))
    (if (member v *safety-in-progress* :test #'eq)
	t
	(let ((safe (and (functionp x)
			 (or (gethash x (safe-primitive-functions))
			     (gethash x qsx::*safe-functions*)
			     (raising-procedure-p x)
			     (let ((sites (gethash x *guarded-procedures*)))
			       (and sites
				    (let ((*safety-in-progress* (cons v *safety-in-progress*)))
				      (every (lambda (site)
					       (let ((w (resolve-site-for-guard site)))
						 (and w (gvariable-safe-p w))))
					     sites)))))
			 t)))
	  ;; kept only if nothing changed meanwhile, and not while judging a
	  ;; cycle (what it found assumed something)
	  (when (and (null *safety-in-progress*) (= epoch *procedure-epoch*))
	    (setf (gvariable-safe v) safe (gvariable-safe-for v) x (gvariable-safe-epoch v) epoch))
	  safe))))

(declaim (inline gvariable-safe-p))
(defun gvariable-safe-p (v)
  "Whether variable V holds a procedure that can't capture (a primitive,
one the continuation transformation proved safe, or a guarded one whose
callees' variables do): known for its value as last asked unless a
procedure-valued variable has changed since, else found again."
  (let ((x (gvariable-value v)))
    (if (and (eq x (gvariable-safe-for v)) (= (gvariable-safe-epoch v) *procedure-epoch*))
	(gvariable-safe v)
	(gvariable-safe-p-slowly v x))))

(defun guarded-note-form (value checks)
  "For QSX::*GUARDED-NOTE-HOOK*: VALUE, a guarded W, recorded with the
sites of CHECKS, its guards (GUARD-REFERENCE-FORM's)."
  (list (core "%guile-note-guarded") value
	(list (core "quote") (mapcar (lambda (c) (cadr (cadr c))) checks))))

(defun resolve-site-for-guard (site)
  "SITE's variable, found now if it's a top-level one not yet referenced
(a guard asks before the call that would find it), else NIL."
  (or (site-variable site)
      (and (eq (site-kind site) :toplevel)
	   (let ((v (ignore-errors (site-lookup site))))
	     (when v (setf (site-variable site) v))
	     v))))

(defun guard-reference-form (operator)
  "For QSX::*GUARD-REFERENCE-HOOK*: (%guile-safe-ref-p 'site) if OPERATOR
is (%guile-ref 'site), a top-level variable's reference."
  (and (consp operator) (eq (car operator) (core "%guile-ref"))
       (consp (cdr operator)) (qsx::quote-form-p (cadr operator))
       (site-p (cadr (cadr operator)))
       ;; one known safe for good (a primitive) needs no guard
       (not (safe-operator-form-p operator))
       (list (core "%guile-safe-ref-p") (cadr operator))))

(defun safe-operator-form-p (form)
  "(%guile-ref 'site), SITE being a SAFE-SITE-P."
  (and (consp form) (eq (car form) (core "%guile-ref"))
       (consp (cdr form)) (qsx::quote-form-p (cadr form))
       (site-p (cadr (cadr form)))
       (ignore-errors (safe-site-p (cadr (cadr form))))))

(defun self-reference-form-p (form definition)
  ":READS if FORM is (%guile-ref 'site), SITE being the top-level variable
DEFINITION, (%guile-define! 'module 'name value), defines; :ASSIGNS if it
is (%guile-set! 'site value) or defines the same variable
(src/continuations.lisp, GUARD-SELF-CALLS)."
  (flet ((same-site-p (x)
	   (and (qsx::quote-form-p x) (site-p (cadr x))
		(let ((site (cadr x)))
		  (and (eq (site-kind site) :toplevel)
		       (qsx::quote-form-p (second definition)) (qsx::quote-form-p (third definition))
		       (eq (site-module site) (cadr (second definition)))
		       (eq (site-name site) (cadr (third definition))))))))
    (and (consp form) (consp (cdr form))
	 (cond ((eq (car form) (core "%guile-ref")) (and (same-site-p (cadr form)) :reads))
	       ((eq (car form) (core "%guile-set!")) (and (same-site-p (cadr form)) :assigns))
	       ((eq (car form) (core "%guile-define!"))
		(and (consp (cddr form)) (equal (cadr form) (second definition))
		     (equal (caddr form) (third definition)) :assigns))))))

(defun install-compiler-primitives ()
  (setq qsx::*definition-operators* '("%guile-define!")
	qsx::*safe-operator-hook* 'safe-operator-form-p
	qsx::*self-reference-hook* 'self-reference-form-p
	qsx::*frameless-stack-checks* t
	qsx::*guard-reference-hook* 'guard-reference-form
	qsx::*guarded-note-hook* 'guarded-note-form)
  (qsx:defhost "%guile-note-guarded" (f sites)
    (setf (gethash f *guarded-procedures*) sites)
    f)
  (qsx:defhost "%guile-safe-ref-p" (site)
    (let ((v (resolve-site-for-guard site)))
      (bool (and v (gvariable-safe-p v)))))
  (qsx:defhost "%guile-ref" (site) (site-ref site))
  ;; past the fixnums inlined (*GUILE-INLINE-PRIMITIVES*), flonums: the
  ;; host's arithmetic is the root procedure's for them, without its
  ;; lookup by name and its list of arguments.  Not 0 - 0.0, which is
  ;; -0.0 in Guile (see "-" in runtime.lisp).
  (qsx:defhost "%guile-sub2" (x y)
    (cond ((and (typep x 'fixnum) (typep y 'fixnum)) (- x y))
	  ((and (typep x 'double-float) (typep y '(or double-float fixnum))) (- x y))
	  ((and (typep x 'fixnum) (typep y 'double-float) (not (eql x 0))) (- x y))
	  (t (funcall (root-value "-") x y))))
  (macrolet ((comparisons (&rest pairs)
	       `(progn
		  ,@(loop for (name test) in pairs
			  collect `(qsx:defhost ,(format nil "%guile-~A" name) (x y)
				     (if (and (typep x '(or fixnum double-float)) (typep y '(or fixnum double-float)))
					 (bool (,test x y))
					 (funcall (root-value ,name) x y)))))))
    (comparisons ("<" <) (">" >) ("<=" <=) (">=" >=) ("=" =)))
  ;; null? and not, for which Emacs Lisp's nil, #nil, counts as the empty
  ;; list and as false
  (qsx:defhost "%guile-null?" (x) (bool (or (null x) (eq x *elisp-nil*))))
  (qsx:defhost "%guile-not" (x) (bool (or (eq x qs:false) (eq x *elisp-nil*))))
  (qsx:defhost "%guile-true" (x) (if (eq x *elisp-nil*) qs:false x))
  (qsx:defhost "%guile-append2" (x y)
    ;; lists that don't end in #nil: the host's append; else the root's,
    ;; which takes #nil as a list's end and reports the wrong type
    (if (and (proper-list-p* x) (not (nil-terminated-p y)))
	(append x y)
	(funcall (root-value "append") x y)))
  (qsx:defhost "%guile-set!" (site value) (site-set site value))
  (qsx:defhost "%guile-define!" (module name value) (define-in-module module name value))
  (qsx:defhost "%guile-unsupported" (name)
    (lambda (&rest args)
      (declare (ignore args))
      (error "Guile primitive ~A isn't supported here" (qs:scheme-symbol-name name))))
  (qsx:defhost "%guile-with-meta" (f props)
    (setf (gethash f *procedure-properties*) (append props (gethash f *procedure-properties* '())))
    (let ((name (assoc (ssym "name") props)))
      (when name (register-code-name f (cdr name))))
    f)
  (qsx:defhost "%guile-inline" (f)
    (register-code-name f :inline)
    f)
  (qsx:defhost "%guile-arity-error" ()
    (guile-error (ssym "wrong-number-of-args") qs:false "Wrong number of arguments" '()))
  (qsx:defhost "%guile-abort*" (tag args) (apply #'qsx::abort-to-prompt tag args))
  (qsx:defhost "%guile-assigned" (value name)
    (if (eq value qs:unassigned)
	(guile-error (ssym "misc-error") qs:false "Unbound variable: ~S" (list name))
	value))
  (qsx:defhost "%guile-nargs>=" (args n) (bool (nthcdr-p args n)))
  (qsx:defhost "%guile-nargs-between" (args min max)
    (let ((n (list-length args))) (bool (<= min n max))))
  (qsx:defhost "%guile-positional?" (tail) (bool (and (consp tail) (not (keywordp (car tail))))))
  (qsx:defhost "%guile-keywords-fit?" (args req opt)
    (bool (and (nthcdr-p args req)
	       (let ((tail (nthcdr req args)))
		 (loop repeat opt
		       while (and (consp tail) (not (keywordp (car tail))))
		       do (setq tail (cdr tail)))
		 (or (atom tail) (keywordp (car tail)))))))
  (qsx:defhost "%guile-check-no-more" (tail)
    (when tail (funcall (qsx:host-ref "%guile-arity-error")))
    *unspecified*)
  (qsx:defhost "%guile-check-keywords" (tail keywords allow-other rest)
    (check-keywords tail keywords (truthy allow-other) (truthy rest)))
  (qsx:defhost "%guile-keyword-ref" (tail keyword)
    ;; the last value given, as Guile's; a non-keyword (with a rest
    ;; argument) is passed over
    (let ((value +unbound+))
      (loop while (consp tail)
	    do (if (and (keywordp (car tail)) (consp (cdr tail)))
		   (progn (when (eq (car tail) keyword) (setq value (cadr tail)))
			  (setq tail (cddr tail)))
		   (setq tail (cdr tail))))
      value))
  (install-guile-inline-primitives)
  (qsx::register-primitive-names
   '("%guile-append2" "%guile-nil:memq" "%guile-nil:memv" "%guile-nil:assq" "%guile-nil:assv"
     "%guile-nil:length" "%guile-nil:list?" "%guile-nil:reverse" "%guile-nil:last-pair"
     "%guile-nil:append"
     "%guile-ref" "%guile-safe-ref-p" "%guile-note-guarded" "%guile-sub2" "%guile-<" "%guile->" "%guile-<=" "%guile->=" "%guile-=" "%guile-null?" "%guile-not" "%guile-true" "%guile-set!" "%guile-define!" "%guile-unsupported" "%guile-arity-error"
     "%guile-with-meta" "%guile-inline"
     "%guile-nargs>=" "%guile-nargs-between" "%guile-keywords-fit?" "%guile-positional?" "%guile-check-no-more"
     "%guile-check-keywords" "%guile-keyword-ref")))

;;; Inline code for the hottest of those helpers.  The host calls a
;;; primitive out of line; as src/psyntax.lisp's *INLINE-PRIMITIVES* do, a
;;; compiler macro on the host global's function puts the common case in
;;; the caller (a fixnum comparison, a top-level variable already found
;;; and bound) and calls the function otherwise, for its error or its
;;; lookup.  Without them a call of fib costs several full calls.

(defparameter *guile-inline-primitives*
  `(("%guile-ref" (s)
     (let ((v (site-variable (sb-ext:truly-the site s))))
       (if v
	   (let ((value (gvariable-value (sb-ext:truly-the gvariable v))))
	     (if (eq value **unbound**) :call value))
	   :call)))
    ("%guile-safe-ref-p" (s)
     (let ((v (or (site-variable (sb-ext:truly-the site s)) (resolve-site-for-guard s))))
       (bool (and v (gvariable-safe-p (sb-ext:truly-the gvariable v))))))
    ("%guile-sub2" (x y)
     (cond ((and (typep x 'fixnum) (typep y 'fixnum)) (- x y))
	   ((and (typep x 'double-float) (typep y 'double-float)) (- x y))
	   (t :call)))
    ,@(loop for (name test) in '(("<" <) (">" >) ("<=" <=) (">=" >=) ("=" =))
	    collect `(,(format nil "%guile-~A" name) (x y)
		      (cond ((and (typep x 'fixnum) (typep y 'fixnum)) (bool (,test x y)))
			    ((and (typep x 'double-float) (typep y 'double-float)) (bool (,test x y)))
			    (t :call))))
    ;; #nil is made once, at load, and never replaced
    ("%guile-null?" (x) (bool (or (null x) (eq x (load-time-value *elisp-nil* t)))))
    ("%guile-not" (x) (bool (or (eq x qs:false) (eq x (load-time-value *elisp-nil* t)))))
    ("%guile-true" (x) (if (eq x (load-time-value *elisp-nil* t)) qs:false x)))
  "(name lambda-list body), as in QSX::*INLINE-PRIMITIVES*.")

(defun install-guile-inline-primitives ()
  (loop for (name params body) in *guile-inline-primitives*
	for global = (qsx:location (ssym name))
	do (let ((global global) (params params) (body body))
	     (setf (compiler-macro-function global)
		   (lambda (form env)
		     (declare (ignore env))
		     (let ((args (if (eq (car form) 'funcall) (cddr form) (cdr form))))
		       (if (or (eq (car form) 'funcall) (/= (length args) (length params)))
			   form		; the call in the slow path, or another arity
			   (let ((vars (mapcar (lambda (p) (gensym (symbol-name p))) params)))
			     `(let ,(mapcar #'list vars args)
				,(sublis (acons :call `(funcall #',global ,@vars)
						(mapcar #'cons params vars))
					 body))))))))))

(defun proper-list-p* (x)
  "Whether X is a list ending in (), not circular."
  (let ((slow x))
    (loop
      (cond ((null x) (return t)) ((atom x) (return nil)))
      (setq x (cdr x))
      (cond ((null x) (return t)) ((atom x) (return nil)))
      (setq x (cdr x) slow (cdr slow))
      (when (eq x slow) (return nil)))))

(defun nthcdr-p (list n)
  "Whether LIST has at least N elements."
  (loop repeat n
	do (if (consp list) (setq list (cdr list)) (return-from nthcdr-p nil)))
  t)

(defun check-keywords (tail keywords allow-other rest)
  ;; Guile skips non-keyword arguments only when there's a rest argument
  (loop while tail
	do (let ((k (car tail)))
	     (cond ((keywordp k)
		    (unless (consp (cdr tail))
		      (guile-error (ssym "keyword-argument-error") qs:false
				   "Keyword argument has no value" '() (list k)))
		    (unless (or allow-other (member k keywords))
		      (guile-error (ssym "keyword-argument-error") qs:false
				   "Unrecognized keyword" '() (list k)))
		    (setq tail (cddr tail)))
		   (rest (setq tail (cdr tail)))
		   (t (guile-error (ssym "keyword-argument-error") qs:false
				   "Invalid keyword" '() (list k))))))
  *unspecified*)

;;; ------------------------------------------------------------------
;;; The pre-expander: libguile's expand.c, for what comes before psyntax

(defvar *pre-env* '() "symbol -> gensym, the lexical variables in scope")

(defun keyword-is (x name)
  (and (symbolp x) x (not (keywordp x)) (string= (qs:scheme-symbol-name x) name)
       (not (assoc x *pre-env*))))

(defun pre-gensym (name)
  (ssym (format nil "~A-~D" (qs:scheme-symbol-name name) (incf *guile-gensym-counter*))))

(defun pre-expand (x &rest ignore)
  (declare (ignore ignore))
  (let ((*pre-env* '()))
    (pre-x x)))

(defun pre-seq (forms)
  (cond ((null forms) (make-node "void" qs:false))
	((null (cdr forms)) (pre-x (car forms)))
	(t (make-node "seq" qs:false (pre-x (car forms)) (pre-seq (cdr forms))))))

(defun pre-x (x)
  (cond ((and (symbolp x) x (not (keywordp x)) (qs:scheme-symbol-p x))
	 (let ((b (assoc x *pre-env*)))
	   (if b
	       (make-node "lexical-ref" qs:false x (cdr b))
	       (make-node "toplevel-ref" qs:false qs:false x))))
	((not (consp x)) (make-node "const" qs:false x))
	(t (let ((head (car x)))
	     (flet ((is (name) (keyword-is head name)))
	       (cond ((is "quote") (make-node "const" qs:false (cadr x)))
		     ((is "quasiquote") (pre-x (quasi (cadr x) 1)))
		     ((is "if") (make-node "conditional" qs:false (pre-x (cadr x)) (pre-x (caddr x))
					   (if (cdddr x) (pre-x (cadddr x)) (make-node "void" qs:false))))
		     ((is "define")
		      (if (consp (cadr x))
			  (pre-x (list (ssym "define") (caadr x)
				       (list* (ssym "lambda") (cdadr x) (cddr x))))
			  (make-node "toplevel-define" qs:false qs:false (cadr x)
				     (named (cadr x) (pre-x (caddr x))))))
		     ((is "set!")
		      (let ((b (assoc (cadr x) *pre-env*)))
			(if b
			    (make-node "lexical-set" qs:false (cadr x) (cdr b) (pre-x (caddr x)))
			    (make-node "toplevel-set" qs:false qs:false (cadr x) (pre-x (caddr x))))))
		     ((is "begin") (pre-seq (cdr x)))
		     ((or (is "lambda") (is "lambda*"))
		      (make-node "lambda" qs:false '() (pre-clause (cadr x) (cddr x) qs:false)))
		     ((or (is "case-lambda") (is "case-lambda*"))
		      (make-node "lambda" qs:false '()
				 (reduce (lambda (clause alt) (pre-clause (car clause) (cdr clause) alt))
					 (cdr x) :from-end t :initial-value qs:false)))
		     ((is "let")
		      (if (and (cadr x) (symbolp (cadr x)))
			  ;; named let
			  (let ((name (cadr x)) (bindings (caddr x)))
			    (pre-x `(,(ssym "letrec") ((,name (,(ssym "lambda") ,(mapcar #'car bindings) ,@(cdddr x))))
				     (,name ,@(mapcar #'cadr bindings)))))
			  (pre-let (cadr x) (cddr x))))
		     ((is "let*")
		      (if (null (cadr x))
			  (pre-let '() (cddr x))
			  (pre-let (list (car (cadr x)))
				   (list (list* (ssym "let*") (cdr (cadr x)) (cddr x))))))
		     ((or (is "letrec") (is "letrec*")) (pre-letrec (cadr x) (cddr x) (is "letrec*")))
		     ((is "and") (cond ((null (cdr x)) (make-node "const" qs:false qs:true))
				       ((null (cddr x)) (pre-x (cadr x)))
				       (t (make-node "conditional" qs:false (pre-x (cadr x))
						     (pre-x (cons head (cddr x)))
						     (make-node "const" qs:false qs:false)))))
		     ((is "or") (cond ((null (cdr x)) (make-node "const" qs:false qs:false))
				      ((null (cddr x)) (pre-x (cadr x)))
				      (t (let ((tmp (gensym-symbol "t")))
					   (pre-x `(,(ssym "let") ((,tmp ,(cadr x)))
						    (,(ssym "if") ,tmp ,tmp (,head ,@(cddr x)))))))))
		     ((is "when") (pre-x `(,(ssym "if") ,(cadr x) (,(ssym "begin") ,@(cddr x)))))
		     ((is "unless") (pre-x `(,(ssym "if") ,(cadr x) (,(ssym "if") #.qs:false #.qs:false) (,(ssym "begin") ,@(cddr x)))))
		     ((is "cond") (pre-x (expand-cond (cdr x))))
		     ((is "case") (pre-x (expand-case (cadr x) (cddr x))))
		     ((is "do") (pre-x (expand-do x)))
		     ((or (is "@") (is "@@"))
		      (if (and (keyword-is (cadr x) "primitive") (is "@@"))
			  (make-node "primitive-ref" qs:false (caddr x))
			  (make-node "module-ref" qs:false (cadr x) (caddr x) (bool (is "@")))))
		     ((is "eval-when")
		      (if (some (lambda (s) (member (qs:scheme-symbol-name s) '("eval" "load" "expand")
						    :test #'string=))
				(cadr x))
			  (pre-seq (cddr x))
			  (make-node "void" qs:false)))
		     ((and (consp head) (keyword-is (car head) "@@") (keyword-is (cadr head) "primitive"))
		      (make-node "primcall" qs:false (caddr head) (mapcar #'pre-x (cdr x))))
		     (t (make-node "call" qs:false (pre-x head) (mapcar #'pre-x (cdr x))))))))))

(defun gensym-symbol (stem) (ssym (format nil " ~A~D" stem (incf *guile-gensym-counter*))))

(defun named (name node)
  (if (eq (node-type node) :lambda)
      (make-node "lambda" qs:false (acons (ssym "name") name (node-field node 1)) (node-field node 2))
      node))

(defun quasi (x depth)
  (cond ((not (consp x))
	 (if (simple-vector-p x)
	     (list (ssym "list->vector") (quasi (coerce x 'list) depth))
	     (list (ssym "quote") x)))
	((keyword-is (car x) "unquote")
	 (if (= depth 1) (cadr x) (list (ssym "list") (list (ssym "quote") (ssym "unquote")) (quasi (cadr x) (1- depth)))))
	((keyword-is (car x) "quasiquote")
	 (list (ssym "list") (list (ssym "quote") (ssym "quasiquote")) (quasi (cadr x) (1+ depth))))
	((and (consp (car x)) (keyword-is (caar x) "unquote-splicing") (= depth 1))
	 (list (ssym "append") (cadar x) (quasi (cdr x) depth)))
	(t (list (ssym "cons") (quasi (car x) depth) (quasi (cdr x) depth)))))

(defun scan-body (body)
  "BODY with its leading internal definitions made a letrec*."
  (let ((defs '()))
    (loop while (and (consp (car body)) (keyword-is (caar body) "define"))
	  do (let ((d (pop body)))
	       (push (if (consp (cadr d))
			 (list (caadr d) (list* (ssym "lambda") (cdadr d) (cddr d)))
			 (list (cadr d) (caddr d)))
		     defs)))
    (if defs
	(list (list* (ssym "letrec*") (nreverse defs) body))
	body)))

(defun pre-body (body) (pre-seq (scan-body body)))

(defun pre-let (bindings body)
  (let* ((names (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
	 (vals (mapcar (lambda (b) (pre-x (if (and (consp b) (cdr b)) (cadr b) qs:false))) bindings))
	 (gensyms (mapcar #'pre-gensym names)))
    (let ((*pre-env* (append (mapcar #'cons names gensyms) *pre-env*)))
      (make-node "let" qs:false names gensyms vals (pre-body body)))))

(defun pre-letrec (bindings body &optional (in-order t))
  (let* ((names (mapcar #'car bindings))
	 (gensyms (mapcar #'pre-gensym names)))
    (let ((*pre-env* (append (mapcar #'cons names gensyms) *pre-env*)))
      (make-node "letrec" qs:false (if in-order qs:true qs:false) names gensyms
		 (mapcar (lambda (b) (named (car b) (pre-x (cadr b)))) bindings)
		 (pre-body body)))))

(defun pre-clause (formals body alternate)
  "A lambda-case node for FORMALS, which may have lambda*'s #:optional,
#:key, #:allow-other-keys and #:rest."
  (let ((req '()) (opt '()) (rest nil) (kw '()) (allow-other nil) (mode :req))
    (loop
      (cond ((null formals) (return))
	    ((symbolp formals) (if (keywordp formals) (return) (progn (setq rest formals) (return))))
	    (t (let ((f (pop formals)))
		 (cond ((eq f :optional) (setq mode :opt))
		       ((eq f :key) (setq mode :key))
		       ((eq f :allow-other-keys) (setq allow-other t))
		       ((eq f :rest) (setq rest (pop formals)))
		       ((eq mode :req) (push f req))
		       ((eq mode :opt) (push (if (consp f) f (list f qs:false)) opt))
		       (t (push (if (consp f) f (list f qs:false)) kw)))))))
    (setq req (nreverse req) opt (nreverse opt) kw (nreverse kw))
    ;; documentation string
    (when (and (stringp (car body)) (cdr body)) (pop body))
    (let ((*pre-env* *pre-env*) (gensyms '()) (inits '()))
      (dolist (r req)
	(let ((g (pre-gensym r))) (push (cons r g) *pre-env*) (push g gensyms)))
      (dolist (o opt)
	(push (pre-x (cadr o)) inits)
	(let ((g (pre-gensym (car o)))) (push (cons (car o) g) *pre-env*) (push g gensyms)))
      (when rest
	(let ((g (pre-gensym rest))) (push (cons rest g) *pre-env*) (push g gensyms)))
      (let ((kw-spec '()))
	(dolist (k kw)
	  (push (pre-x (cadr k)) inits)
	  (let ((g (pre-gensym (car k))))
	    (push (cons (car k) g) *pre-env*) (push g gensyms)
	    (push (list (intern (symbol-name (car k)) "KEYWORD") (car k) g) kw-spec)))
	(make-node "lambda-case" qs:false req
		   (if opt (mapcar #'car opt) qs:false)
		   (or rest qs:false)
		   (if (or kw allow-other) (cons (bool allow-other) (nreverse kw-spec)) qs:false)
		   (nreverse inits) (nreverse gensyms) (pre-body body) alternate)))))

(defun expand-cond (clauses)
  (if (null clauses)
      (list (ssym "if") qs:false qs:false)
      (let ((c (car clauses)))
	(cond ((keyword-is (car c) "else") (cons (ssym "begin") (cdr c)))
	      ((and (cdr c) (keyword-is (cadr c) "=>"))
	       (let ((tmp (gensym-symbol "c")))
		 `(,(ssym "let") ((,tmp ,(car c)))
		   (,(ssym "if") ,tmp (,(caddr c) ,tmp) ,(expand-cond (cdr clauses))))))
	      ((null (cdr c)) `(,(ssym "or") ,(car c) ,(expand-cond (cdr clauses))))
	      (t `(,(ssym "if") ,(car c) (,(ssym "begin") ,@(cdr c)) ,(expand-cond (cdr clauses))))))))

(defun expand-case (key clauses)
  (let ((tmp (gensym-symbol "k")))
    `(,(ssym "let") ((,tmp ,key))
      ,(expand-cond
	(mapcar (lambda (c)
		  (if (keyword-is (car c) "else")
		      c
		      (cons `(,(ssym "memv") ,tmp (,(ssym "quote") ,(car c))) (cdr c))))
		clauses)))))

(defun expand-do (x)
  (destructuring-bind (specs (test . result) &rest body) (cdr x)
    (let ((loop (gensym-symbol "do")))
      `(,(ssym "let") ,loop ,(mapcar (lambda (s) (list (car s) (cadr s))) specs)
	(,(ssym "if") ,test (,(ssym "begin") ,@result)
	 (,(ssym "begin") ,@body
	  (,loop ,@(mapcar (lambda (s) (if (cddr s) (caddr s) (car s))) specs))))))))
