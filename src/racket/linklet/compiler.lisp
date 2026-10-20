; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Strategy B: linklets to core Scheme, then the translator
;;;; (docs/racket-linklet-compiler.md).  Modelled on the Tree-IL compiler
;;;; (src/guile/compile.lisp).
;;;;
;;;;   1 DECORRELATE  correlated objects off; names and locations kept aside
;;;;   3 ANALYSE      per variable: defined, imported, exported, mutated
;;;;                  (more to come: used before definition, known value)
;;;;   2+4 LOWER      core Scheme, form by form (the document's table),
;;;;                  renaming lexicals apart as it goes: one walk, with an
;;;;                  environment from Racket names to core names
;;;;   5 host-eval    translate-core, the continuation transformation, SBCL
;;;;
;;;; What a compiled linklet is: a core lambda of the linklet's variable
;;;; objects, imports first (in order), then exports, which runs the
;;;; body.  INSTANTIATE-LINKLET (api.lisp) finds or makes the variables
;;;; and calls it.
;;;;
;;;;   imported x      (racket:variable-ref v), v the variable object
;;;;   defined x       a lexical of the body's letrec*; exported, it is
;;;;                   also stored into its variable after each assignment
;;;;   exported only   (racket:variable-ref v): exports may name variables
;;;;                   the body doesn't define, which another linklet sets
;;;;   anything else   a primitive: the host's procedure where Racket's is
;;;;                   the same (native.lisp, *HOST-SHARED*), else the
;;;;                   native one, a host global named racket:NAME

(in-package "QUASISCHEME-RACKET")

(defstruct (linklet-info (:constructor make-linklet-info ()))
  (locations (make-hash-table :test 'eq))	; form -> source location (pass 1)
  (names (make-hash-table :test 'eq))		; lambda -> inferred name (pass 1)
  (variables (make-hash-table :test 'eq)))	; Racket symbol -> VARIABLE-INFO (pass 3)

(defstruct (variable-info (:constructor make-variable-info (name)))
  name
  (kind :local)				; :local :defined :imported :exported
  (mutated nil)				; set! anywhere
  (early-use nil)			; referenced before its definition runs
  (known nil)				; (:procedure arity) (:constant v) (:struct-accessor type i) ...
  (core nil)				; its core name (a lexical) or variable object's
  (cell nil))				; for an exported or imported one: the core name of its variable object

;;; ------------------------------------------------------------------
;;; Names

(defvar *safe-locals* (make-hash-table :test 'eq)
  "Lexicals bound to struct procedures by a let-values or letrec-values
(SAFE-LOCAL-STRUCT-IDS), whose references are (racket:safe-ref lexical).")

(defvar *lambda-depth* 0
  "How many lambdas the expression being lowered is in: 0 at a linklet's
outer level, whose code runs once.")

(defun core (name) (qsx::sym name))
(defun quoted (x) (list (core "quote") x))
(defun form-p (x name) (and (consp x) (symbolp (car x)) (string= (rname (car x)) name)))

;; With a constant NAME, the symbol itself is compared: (rname symbol)
;; makes a string each time, and lowering asks this of every form.
(define-compiler-macro form-p (&whole whole x name)
  (if (stringp name)
      (let ((v (gensym "X")))
	`(let ((,v ,x)) (and (consp ,v) (eq (car ,v) (load-time-value (rsym ,name))))))
      whole))

(defvar *lexical-tag* "r"
  "Part of each lexical name, new in each process, as Tree-IL's (big
top-level forms' lexicals become global definitions).")

(defun new-lexical-tag ()
  (setq *lexical-tag* (format nil "r~36R" (random (expt 36 6) (make-random-state t)))))

(pushnew 'new-lexical-tag sb-ext:*init-hooks*)

(defvar *lexical-count* 0)

(defun fresh-lexical (&optional (stem ""))
  "A new lexical: STEM (the Racket name, so that backtraces show it), and
a number and tag that make it unique."
  (qsx::make-lexical
   (qs::invert-case (concatenate 'string stem "~" (write-to-string (incf *lexical-count*) :base 10 :radix nil) "." *lexical-tag*))))

;;; ------------------------------------------------------------------
;;; Pass 1: correlated objects off

(defun decorrelate (form info)
  "Pass 1: FORM without correlated objects; their names and locations go
into INFO."
  (labels ((walk (x)
	     (cond ((correlated-p x)
		    (let ((e (walk (correlated-e x))))
		      (when (consp e)
			(setf (gethash e (linklet-info-locations info)) (correlated-source-location x))
			(let ((name (correlated-property x (rsym "inferred-name"))))
			  (when name (setf (gethash e (linklet-info-names info)) name))))
		      e))
		   ((form-p x "quote") x)	; a datum: correlated only in syntax literals, kept
		   ((consp x) (let ((a (walk (car x))) (d (walk (cdr x))))
				(if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
		   (t x))))
    (walk form)))

;;; ------------------------------------------------------------------
;;; Parsing the linklet form

(defstruct (parsed-linklet (:constructor make-parsed-linklet (imports exports body)))
  imports				; ((( external . internal) ...) ...) per import set
  exports				; ((internal . external) ...)
  body)

(defun parse-linklet (form)
  "(linklet [[import ...] ...] [export ...] body ...)"
  (unless (and (form-p form "linklet") (consp (cdr form)) (consp (cddr form)))
    (error "compile-linklet: not a linklet form: ~S" form))
  (destructuring-bind (imports exports &rest body) (cdr form)
    (flet ((pair (x reverse)
	     (cond ((symbolp x) (cons x x))
		   ((and (consp x) (symbolp (car x)) (consp (cdr x)) (symbolp (cadr x)))
		    (if reverse (cons (cadr x) (car x)) (cons (car x) (cadr x))))
		   (t (error "compile-linklet: bad import or export: ~S" x)))))
      (make-parsed-linklet
       (mapcar (lambda (set) (mapcar (lambda (x) (pair x nil)) set)) imports)
       (mapcar (lambda (x) (pair x nil)) exports)
       body))))

;;; ------------------------------------------------------------------
;;; Pass 3: analysis

(defun info-of (info name)
  (or (gethash name (linklet-info-variables info))
      (setf (gethash name (linklet-info-variables info)) (make-variable-info name))))

(defun analyse (parsed info imports-known)
  "Pass 3: what each of the linklet's top-level names is.  IMPORTS-KNOWN
is what get-import told us about the imported linklets (cross-linklet
knowledge): per import set, the external names of its procedures that
can't capture a continuation (SAFE-EXPORTS), or NIL."
  (loop for set in (parsed-linklet-imports parsed)
	for safe = (pop imports-known)
	do (loop for (external . internal) in set
		 do (let ((v (info-of info internal)))
		      (setf (variable-info-kind v) :imported)
		      (let ((entry (find external safe :key (lambda (e) (if (consp e) (car e) e)))))
			(cond ((null entry))
			      ((and (consp entry) (eq (cadr entry) :host))
			       (setf (variable-info-known v) (cons :host (caddr entry))))
			      ((consp entry) (setf (variable-info-known v) (cons :arg-safe-import (cdr entry))))
			      (t (setf (variable-info-known v) :safe-import)))))))
  (dolist (form (parsed-linklet-body parsed))
    (when (form-p form "define-values")
      (dolist (id (cadr form))
	(setf (variable-info-kind (info-of info id)) :defined))))
  (loop for (internal . nil) in (parsed-linklet-exports parsed)
	do (let ((v (info-of info internal)))
	     (setf (variable-info-kind v)
		   (if (eq (variable-info-kind v) :defined) :defined-exported :exported))))
  ;; set!s anywhere (lexicals are told apart from top-level names by
  ;; shadowing in LOWER; a set! of a top-level name marks it here)
  (labels ((walk (x)
	     (cond ((form-p x "quote"))
		   ((form-p x "set!")
		    (let ((v (gethash (cadr x) (linklet-info-variables info))))
		      (when v (setf (variable-info-mutated v) t)))
		    (walk (caddr x)))
		   ((consp x) (walk (car x)) (walk (cdr x))))))
    (mapc #'walk (parsed-linklet-body parsed)))
  parsed)

;;; ------------------------------------------------------------------
;;; Pass 4: lowering

(defvar *info*)

(defun lookup (name env)
  "NAME's core reference: a lexical, a top-level name's, or a primitive's."
  (let ((local (assoc name env)))
    (cond (local (if (gethash (cdr local) *safe-locals*)
		     (list (core "racket:safe-ref") (cdr local))
		     (cdr local)))
	  ((let ((v (gethash name (linklet-info-variables *info*))))
	     (and v (case (variable-info-kind v)
		      ((:defined :defined-exported)
		       ;; a lexical; in a split linklet, a cell
		       (or (let ((c (variable-info-core v)))
			     (and c (if (eq (variable-info-known v) :safe-struct)
					(list (core "racket:safe-ref") c)
					c)))
			   (list (core (if (member (variable-info-known v) '(:safe-candidate :safe-struct))
					   "racket:safe-variable-ref"
					   "racket:variable-ref"))
				 (variable-info-cell v))))
		      ((:imported :exported)
		       (let ((known (variable-info-known v)))
			 (cond ((eq known :safe-import)
				(list (core "racket:safe-variable-ref") (variable-info-cell v)))
			       ;; *NATIVE-EXPORTS*': the host primitive the cell holds
			       ((and (consp known) (eq (car known) :host)) (core (cdr known)))
			       ((and (consp known) (eq (car known) :arg-safe-import))
				(list (core "racket:arg-safe-ref") (quoted (cdr known)) (variable-info-cell v)))
			       (t (list (core "racket:variable-ref") (variable-info-cell v))))))))))
	  (t (primitive-reference name)))))

(defun primitive-reference (name)
  (let ((string (rname name)))
    (cond ((member string *host-shared* :test #'string=) (core string))
	  ((host-alias string) (core (host-alias string)))
	  ((native-host-name string) (core (native-host-name string)))
	  (t (list (core "racket:unknown-primitive") (quoted name))))))

(defun bind (names env &optional (stem ""))
  "ENV with NAMES (Racket formals: a list, possibly improper) bound to
fresh lexicals; and the core formals."
  (let ((formals '()) (rest nil))
    (loop for tail = names then (cdr tail)
	  while (consp tail)
	  do (let ((l (fresh-lexical (if (equal stem "") (rname (car tail)) stem))))
	       (push (cons (car tail) l) env)
	       (push l formals))
	  finally (when (and tail (symbolp tail))
		    (setq rest (fresh-lexical (if (equal stem "") (rname tail) stem)))
		    (push (cons tail rest) env)))
    (values env (let ((f (nreverse formals))) (if rest (append f rest) f)))))

(defun lower-body (forms env)
  (cond ((null forms) (quoted *void*))
	((null (cdr forms)) (lower-expr (car forms) env))
	(t (cons (core "begin") (mapcar (lambda (f) (lower-expr f env)) forms)))))

(defun lower-lambda (formals body env)
  (multiple-value-bind (env formals) (bind formals env)
    (let ((*lambda-depth* (1+ *lambda-depth*)))
      (list (core "lambda") formals (lower-body body env)))))

(defun rest-formal (formals)
  "The rest argument of FORMALS (a b . c), if it has one."
  (loop for tail = formals then (cdr tail)
	while (consp tail)
	finally (return (and tail (symbolp tail) tail))))

(defun lower-case-lambda (clauses env &optional name)
  "One clause is a plain lambda; several dispatch on the argument count.
NAME, if the lambda is bound to a variable: its object-name."
  (if (and clauses (null (cdr clauses)))
      (lower-lambda (car (first clauses)) (cdr (first clauses)) env)
      (let ((args (fresh-lexical "args")) (n (fresh-lexical "n"))
	    ;; its arity: the clauses', which (lambda args ...) doesn't show
	    (mask (reduce #'logior
			  (loop for (formals) in clauses
				for required = (loop for tail = formals then (cdr tail)
						     while (consp tail) count t)
				collect (if (rest-formal formals) (- (ash 1 required)) (ash 1 required)))
			  :initial-value 0)))
       (append (list (core "racket:with-arity") (quoted mask)
	(list (core "lambda") args
	      (list (list (core "lambda") (list n)
			  (reduce (lambda (clause else)
				    (destructuring-bind (formals &rest body) clause
				      (let ((required (loop for tail = formals then (cdr tail)
							    while (consp tail) count t)))
					(list (core "if")
					      (list (core (if (rest-formal formals) ">=" "=")) n required)
					      (list (core "apply") (lower-lambda formals body env) args)
					      else))))
				  clauses :from-end t
				  :initial-value (list (core "racket:arity-error") args)))
		    (list (core "length") args))))
	       ;; its name, for object-name
	       (and name (symbolp name) (not (eq name qs:false)) (list (quoted name)))))))

(defun assigned-p (id x)
  "Does X (Racket code) hold a set! of ID?  Any one, even of another
variable of the same name: this only has to say no when it's so."
  (cond ((form-p x "quote") nil)
	((form-p x "set!") (or (eq (cadr x) id) (assigned-p id (cddr x))))
	((consp x) (loop for tail = x then (cdr tail)
			 while (consp tail)
			 thereis (assigned-p id (car tail))))
	(t nil)))

(defun procedures-only-p (bindings body)
  "Are BINDINGS (letrec-values') each one variable, bound to a lambda,
and none of them assigned in them or BODY?"
  (and (every (lambda (b) (and (consp (car b)) (null (cdr (car b))) (form-p (cadr b) "lambda")))
	      bindings)
       (notany (lambda (b) (or (assigned-p (car (car b)) body)
			       (some (lambda (b2) (assigned-p (car (car b)) (cadr b2))) bindings)))
	       bindings)))

(defun name-lambda (core id)
  "CORE, a lowered expression; if it is a lambda (or a case-lambda's
racket:with-arity), with '(:racket-name . ID) first in its body: the
name Racket infers for a procedure a let binds, which object-name
returns (TRANSLATE-CORE makes it the function's name)."
  (cond ((and (zerop *lambda-depth*)
	      (or (qsx::lambda-form-p core) (and (consp core) (eq (car core) (core "racket:with-arity")))))
	 ;; at the outer level, which runs once: named as it's made, which
	 ;; nothing the continuation transformation does can lose
	 (list (core "racket:named") (quoted id) core))
	((and (qsx::lambda-form-p core) (null (cdddr core)))
	 (list (car core) (cadr core)
	       (list (core "begin") (quoted (cons :racket-name id)) (caddr core))))
	((and (consp core) (eq (car core) (core "racket:with-arity")))
	 (list* (car core) (cadr core) (name-lambda (caddr core) id) (cdddr core)))
	(t core)))

(defun note-safe-local-structs (bindings body env)
  "Mark in *SAFE-LOCALS* the lexicals (in ENV) that BINDINGS bind to struct
procedures (STRUCT-SAFE-IDS), when nothing assigns them."
  (dolist (b bindings)
    (destructuring-bind (ids rhs) b
      (when (and (consp ids) (cdr ids))
	(let ((safe (struct-safe-ids (list (rsym "define-values") ids rhs) '())))
	  (dolist (id safe)
	    (unless (or (assigned-p id body) (some (lambda (b2) (assigned-p id (cadr b2))) bindings))
	      (let ((l (assoc id env)))
		(when l (setf (gethash (cdr l) *safe-locals*) t))))))))))

(defun lower-let-values (bindings body env &optional recursive)
  "let-values: each right-hand side in the outer ENV, then all bound.
letrec-values: as Racket's, a letrec* (left to right, each variable
assigned as its right-hand side returns)."
  (if recursive
      (let* ((ids (loop for (ids) in bindings append ids))
	     (env (bind ids env))
	     (vars (mapcar (lambda (id) (cdr (assoc id env))) ids)))
       (note-safe-local-structs bindings body env)
       (if (procedures-only-p bindings body)
	;; every right-hand side a lambda, none assigned: a letrec the
	;; translator makes into labels, whose calls SBCL compiles as local
	;; ones (a named let's loop, say)
	(list (core "letrec")
	      (loop for (ids rhs) in bindings
		    collect (list (cdr (assoc (car ids) env)) (lower-expr rhs env)))
	      (lower-body body env))
	(cons (list (core "lambda") vars
		    (cons (core "begin")
			  (append (loop for (ids rhs) in bindings
					collect (lower-assignment ids rhs env))
				  (list (lower-body body env)))))
	      (mapcar (constantly (quoted qs:false)) vars))))
      (labels ((nest (bindings inner-env)
		 (if (null bindings)
		     (lower-body body inner-env)
		     (destructuring-bind ((ids rhs) &rest more) bindings
		       (multiple-value-bind (inner-env formals) (bind ids inner-env)
			 (note-safe-local-structs (list (list ids rhs)) body inner-env)
			 (if (and (consp ids) (null (cdr ids)))
			     (list (list (core "lambda") formals (nest more inner-env))
				   (name-lambda (lower-expr rhs env) (car ids)))
			     (list (core "call-with-values")
				   (list (core "lambda") '() (lower-expr rhs env))
				   (list (core "lambda") formals (nest more inner-env)))))))))
	(nest bindings env))))

(defun named (id rhs core env)
  "CORE, RHS lowered: when RHS is a procedure assigned to a top-level
cell (a split linklet's), bound by a letrec of ID's name first, so that
its function has the name: the translator names a letrec's procedures
by their variables."
  (let ((v (and (not (assoc id env)) (gethash id (linklet-info-variables *info*)))))
    (if (and v (null (variable-info-core v))
	     (or (form-p rhs "lambda") (form-p rhs "case-lambda")))
	(let ((f (fresh-lexical (rname id))))
	  (list (list (core "lambda") (list f)
		      (list (core "begin") (list (core "set!") f core) f))
		(quoted qs:false)))
	core)))

(defvar *self-call* nil
  "While lowering a top-level procedure's lambda: (ID . LEXICAL), ID the
variable it's defined as, never assigned otherwise, and LEXICAL the
letrec variable that names the procedure itself.  A call of ID in its
body is a call of it, whatever ID is then: the body runs only once ID
has been defined.")

(defun self-call-p (id rhs env)
  "May a call of ID in RHS, the lambda it's defined as, be a direct
call of the procedure?"
  (and (not (assoc id env)) (form-p rhs "lambda")
       (let ((v (gethash id (linklet-info-variables *info*))))
	 (and v (member (variable-info-kind v) '(:defined :defined-exported))
	      ;; not a split linklet's, whose variables are cells
	      (variable-info-core v)
	      (not (variable-info-mutated v))))))

(defun lower-self-procedure (id rhs env)
  "RHS, a lambda, lowered as (letrec ((SELF lambda)) SELF), its calls of
ID being calls of SELF."
  (let* ((self (fresh-lexical (rname id)))
	 (*self-call* (cons id self)))
    (list (core "letrec") (list (list self (lower-expr rhs env))) self)))

(defun object-named (id rhs set env)
  "SET, the lowered assignment of ID, a top-level definition, to RHS; and
when RHS is a lambda, (racket:named 'ID ID) after it: the procedure's
object-name, which compiled code doesn't keep when the continuation
transformation makes it a state machine behind an anonymous wrapper.
After the assignment, so that analyses still see the lambda assigned."
  (if (and (not (assoc id env)) (or (form-p rhs "lambda") (form-p rhs "case-lambda")))
      (list (core "begin") set (list (core "racket:named") (quoted id) (lookup id env)))
      set))

(defun lower-assignment (ids rhs env)
  "IDS (already bound in ENV) assigned RHS's values."
  (if (and (consp ids) (null (cdr ids)))
      (object-named (car ids) rhs
		    (lower-set (car ids)
			       (if (self-call-p (car ids) rhs env)
				   (lower-self-procedure (car ids) rhs env)
				   (named (car ids) rhs (lower-expr rhs env) env))
			       env)
		    env)
      (let ((temps (loop repeat (length ids) collect (fresh-lexical "v"))))
	(list (core "call-with-values")
	      (list (core "lambda") '() (lower-expr rhs env))
	      (list (core "lambda") temps
		    (cons (core "begin")
			  (append (mapcar (lambda (id temp) (lower-set id temp env)) ids temps)
				  (list (quoted *void*)))))))))

(defun lower-set (name value env)
  "set! of NAME to core VALUE: an exported definition's variable too."
  (let ((local (assoc name env)))
    (if local
	(list (core "set!") (cdr local) value)
	(let* ((v (gethash name (linklet-info-variables *info*)))
	       (kind (and v (variable-info-kind v))))
	  (cond ((and (member kind '(:defined :defined-exported)) (null (variable-info-core v)))
		 ;; a split linklet's: a cell
		 (list (core "racket:variable-set!") (variable-info-cell v) value))
		((eq kind :defined) (list (core "set!") (variable-info-core v) value))
		((eq kind :defined-exported)
		 (list (core "begin")
		       (list (core "set!") (variable-info-core v) value)
		       (list (core "racket:variable-set!") (variable-info-cell v) (variable-info-core v))))
		((eq kind :exported) (list (core "racket:variable-set!") (variable-info-cell v) value))
		(t (list (core "racket:assign-error") (quoted name))))))))

(defun lower-expr (x env)
  (cond ((symbolp x)
	 (if (or (null x) (eq x qs:false) (eq x qs:true)) (quoted x) (lookup x env)))
	((atom x) (quoted x))
	((and (symbolp (car x)) (not (assoc (car x) env)) (special-form (car x)))
	 (let ((args (cdr x)))
	   (ecase (special-form (car x))
	     (:quote (quoted (car args)))
	     (:if (list (core "if") (lower-expr (first args) env) (lower-expr (second args) env)
			(lower-expr (third args) env)))
	     (:begin (lower-body args env))
	     (:begin0 (if (null (cdr args))
			  (lower-expr (car args) env)
			  (let ((vals (fresh-lexical "vals")))
			    (list (core "call-with-values")
				  (list (core "lambda") '() (lower-expr (car args) env))
				  (list (core "lambda") vals
					(list* (core "begin")
					       (append (mapcar (lambda (f) (lower-expr f env)) (cdr args))
						       (list (list (core "apply") (core "values") vals)))))))))
	     (:lambda (lower-lambda (car args) (cdr args) env))
	     (:case-lambda (lower-case-lambda args env (gethash x (linklet-info-names *info*))))
	     ;; Racket CS's (#%foreign-inline e mode): e
	     (:foreign-inline (lower-expr (first args) env))
	     (:let-values (lower-let-values (car args) (cdr args) env))
	     (:letrec-values (lower-let-values (car args) (cdr args) env t))
	     (:set! (lower-set (first args) (lower-expr (second args) env) env))
	     (:with-continuation-mark
	      (list (core "racket:call-with-mark") (lower-expr (first args) env)
		    (lower-expr (second args) env)
		    (list (core "lambda") '() (lower-expr (third args) env))))
	     (:variable-reference
	      (list (core "racket:variable-reference") (core "racket:instance")
		    (if args (quoted (car args)) (quoted qs:false))))
	     (:define-values
	      (error "compile-linklet: define-values not at a linklet's top level: ~S" x)))))
	((and *self-call* (eq (car x) (car *self-call*)) (not (assoc (car x) env)))
	 (cons (cdr *self-call*) (mapcar (lambda (y) (lower-expr y env)) (cdr x))))
	;; (equal? x 'datum) of a datum equal? to only itself, as case makes
	;; for its symbols: eq?, open-coded, not the equal? primitive's call
	((and (symbolp (car x)) (not (assoc (car x) env)) (= (length x) 3)
	      (member (rname (car x)) '("equal?" "eqv?") :test #'string=)
	      (eq (gethash (car x) (linklet-info-variables *info*) :none) :none)
	      (or (eq-datum-p (second x)) (eq-datum-p (third x))))
	 (list (core "eq?") (lower-expr (second x) env) (lower-expr (third x) env)))
	(t (mapcar (lambda (y) (lower-expr y env)) x))))

(defun eq-datum-p (x)
  "Whether X is (quote d), or a literal, of a datum equal? and eqv? to
only what is eq? to it: a symbol, a fixnum, a character, a boolean, '()."
  (let ((d (cond ((form-p x "quote") (cadr x))
		 ((consp x) (return-from eq-datum-p nil))
		 ((symbolp x) (return-from eq-datum-p (or (null x) (eq x qs:false) (eq x qs:true))))
		 (t x))))
    (or (and (symbolp d) d) (null d) (typep d 'fixnum) (characterp d))))

(defparameter *special-forms*
  '(("quote" . :quote) ("if" . :if) ("begin" . :begin) ("begin0" . :begin0)
    ("lambda" . :lambda) ("case-lambda" . :case-lambda) ("let-values" . :let-values)
    ("letrec-values" . :letrec-values) ("set!" . :set!) ("define-values" . :define-values)
    ("with-continuation-mark" . :with-continuation-mark)
    ("#%variable-reference" . :variable-reference) ("#%foreign-inline" . :foreign-inline)))

(defvar *special-form-table*
  (let ((table (make-hash-table :test 'eq)))
    (loop for (name . keyword) in *special-forms* do (setf (gethash (rsym name) table) keyword))
    table))

(defun special-form (symbol)
  (gethash symbol *special-form-table*))

(defparameter *split-size* 10000
  "A linklet bigger than this (in conses) is compiled form by form, as
Racket CS compiles a big linklet's outer level piecewise: compiled whole,
the expander's 2 MB linklet needs more than 8 GB to compile.  Lowered
code is about ten times as big as the linklet, and SBCL on arm64 can't
compile a code object past about 150,000 conses (a branch's reach): a
linklet of 16,000 failed (racket/pretty's) and was interpreted, after
twenty seconds of trying at each start.")

;;; Procedures that can't capture a continuation, in a split linklet.  A
;;; whole linklet's definitions are a letrec*, which the continuation
;;; transformation sees into; a split one's are cells, so a call of one
;;; is a capture site unless the cell is known to hold such a procedure
;;; for good: defined once by a lambda, never assigned, and the lambda's
;;; body making no call that can capture.  Assuming every candidate safe
;;; and striking out those that aren't, until none changes, finds them
;;; (as FIND-SAFE-PROCEDURES does for a letrec); their references are
;;; (racket:safe-variable-ref cell), which SAFE-OPERATOR-P takes as safe.

(defun safe-cell-candidates (parsed info)
  "Mark as :SAFE-CANDIDATE each defined variable, never assigned, whose
one definition is a lambda."
  (dolist (form (parsed-linklet-body parsed))
    (when (and (form-p form "define-values") (consp (cadr form)) (null (cdr (cadr form)))
	       (or (form-p (caddr form) "lambda") (form-p (caddr form) "case-lambda")))
      (let ((v (info-of info (car (cadr form)))))
	(when (and (member (variable-info-kind v) '(:defined :defined-exported))
		   (not (variable-info-mutated v)))
	  (setf (variable-info-known v) :safe-candidate))))))

;;; Struct procedures (as Racket CS's schemify knows them): a definition
;;;
;;;   (define-values (struct:s make-s s? s-a ...)
;;;     (let-values (((S M P R W) (let-values () ... (make-struct-type ...))))
;;;       (values S M P (make-struct-field-accessor R 0 'a) ...)))
;;;
;;; of a type with no guard, whose parent is #f or such a type of the
;;; same linklet, makes a constructor, a predicate, accessors and mutators
;;; that call no procedure of the program's (an impersonator's
;;; redirections aside), so that calls of them aren't capture sites.  In
;;; a whole linklet their references are (racket:safe-ref lexical).

(defun strip-empty-let-values (x)
  (loop while (and (form-p x "let-values") (null (cadr x)) (null (cdddr x)))
	do (setq x (caddr x)))
  x)

(defun false-literal-p (x)
  (or (eq x qs:false) (and (form-p x "quote") (eq (cadr x) qs:false))))

(defun struct-safe-ids (form guard-free-types)
  "For a definition of FORM's shape (above): the ids that are safe struct
procedures, and the struct type's id if its type has no guard; or NIL."
  (when (and (form-p form "define-values") (consp (cadr form)))
    (let ((ids (cadr form)) (rhs (caddr form)))
      (when (and (form-p rhs "let-values") (= (length (cadr rhs)) 1) (null (cdddr rhs)))
	(destructuring-bind ((names inner)) (cadr rhs)
	  (let ((make (strip-empty-let-values inner))
		(vals (caddr rhs)))
	    (when (and (form-p make "make-struct-type") (consp names) (= (length names) 5)
		       (form-p vals "values") (= (length (cdr vals)) (length ids)))
	      (let ((parent (nth 2 make)) (guard (nth 10 make)))
		(when (and (or (null guard) (false-literal-p guard))
			   (or (false-literal-p parent) (member parent guard-free-types)))
		  (destructuring-bind (s m p r w) names
		    (let ((safe '()) (type nil))
		      (loop for id in ids for v in (cdr vals)
			    do (cond ((eq v s) (setq type id))
				     ((or (eq v m) (eq v p)) (push id safe))
				     ((and (or (form-p v "make-struct-field-accessor") (form-p v "make-struct-field-mutator"))
					   (member (cadr v) (list r w)))
				      (push id safe))))
		      (values safe type))))))))))))

(defun safe-struct-procedures (parsed info)
  "Mark as :SAFE-STRUCT each defined variable, never assigned, that
STRUCT-SAFE-IDS finds."
  (let ((types '()))
    (dolist (form (parsed-linklet-body parsed))
      (multiple-value-bind (safe type) (struct-safe-ids form types)
	(when type (push type types))
	(dolist (id safe)
	  (let ((v (info-of info id)))
	    (when (and (member (variable-info-kind v) '(:defined :defined-exported))
		       (not (variable-info-mutated v)))
	      (setf (variable-info-known v) :safe-struct))))))))

(defun safe-ref-p (form)
  (and (consp form) (eq (car form) (core "racket:safe-ref"))))

(defun safe-variable-ref-p (form)
  (and (consp form) (eq (car form) (core "racket:safe-variable-ref"))))

(defun arg-safe-ref-p (form)
  (and (consp form) (eq (car form) (core "racket:arg-safe-ref"))))

(defun arg-safe-call-p (call)
  "Whether CALL calls an import that is safe when the procedures passed
it at its positions are, and they are."
  (let ((op (car call)))
    (and (arg-safe-ref-p op)
	 (every (lambda (i) (let ((arg (nthcdr (1+ i) call)))
			      (or (null arg) (qsx::safe-procedure-p (car arg)))))
		(cadr (cadr op))))))

(defun call-with-safe-cells (thunk)
  "Call THUNK with (racket:safe-variable-ref cell) taken as safe by the
continuation transformation, and calls of (racket:arg-safe-ref ...)
whose procedure arguments are safe."
  (let ((qsx::*safe-call-hook*
	  (let ((outer qsx::*safe-call-hook*))
	    (lambda (call) (or (arg-safe-call-p call) (and outer (funcall outer call))))))
	(qsx::*safe-operator-hook*
	  (let ((outer qsx::*safe-operator-hook*))
	    (lambda (form) (or (safe-variable-ref-p form) (safe-ref-p form) (and outer (funcall outer form)))))))
    (funcall thunk)))

(defun settle-safe-cells (forms info)
  "FORMS, a split linklet's lowered forms, with references to the
candidates that turn out to capture made plain."
  (let ((lambdas '()))			; (cell . lambda)
    (dolist (form forms)
      ;; (lambda (instance cells) (begin (racket:variable-set! cell value) '#<void>))
      (let ((set (and (consp (caddr form)) (consp (cdr (caddr form))) (cadr (caddr form)))))
	;; OBJECT-NAMED's (begin (racket:variable-set! cell value) (racket:named ...))
	(when (and (consp set) (eq (car set) (core "begin")) (consp (cdr set)))
	  (setq set (cadr set)))
	(when (and (consp set) (eq (car set) (core "racket:variable-set!")))
	  (let* ((value (caddr set))
		 (l (labels ((procedure (x)
			       (cond ((qsx::lambda-form-p x) x)
				     ;; a case-lambda's (racket:with-arity 'mask lambda)
				     ((and (consp x) (eq (car x) (core "racket:with-arity")))
				      (procedure (caddr x)))
				     ;; NAMED's ((lambda (f) (begin (set! f lambda) f)) '#f)
				     ((and (consp x) (qsx::lambda-form-p (car x)))
				      (let ((b (caddr (car x))))
					(and (consp b) (consp (cdr b)) (consp (cadr b))
					     (procedure (caddr (cadr b)))))))))
		      (procedure value))))
	    (when l (push (cons (cadr set) l) lambdas))))))
    (let ((safe (make-hash-table :test 'equal)))
      (maphash (lambda (name v)
		 (declare (ignore name))
		 (case (variable-info-known v)
		   ;; an import another linklet's compilation found safe, or a
		   ;; struct procedure (SAFE-STRUCT-PROCEDURES)
		   ((:safe-import :safe-struct) (setf (gethash (variable-info-cell v) safe) t))
		   (:safe-candidate
		    (if (assoc (variable-info-cell v) lambdas :test #'equal)
			(setf (gethash (variable-info-cell v) safe) t)
			(setf (variable-info-known v) nil)))))
	       (linklet-info-variables info))
      (let ((qsx::*safe-procedures* nil) (qsx::*known-procedures* nil) (qsx::*downward-parameters* nil)
	    (qsx::*safe-call-hook* #'arg-safe-call-p)
	    (qsx::*safe-operator-hook* (lambda (form) (and (safe-variable-ref-p form) (gethash (cadr form) safe)))))
	;; each body with its own named lets and letrecs known (as
	;; SAFE-EXPORTS judges them)
	(let ((bodies (loop for (cell . l) in lambdas
			    collect (cons cell (direct-letrec-calls (caddr l))))))
	  (loop while (loop with changed = nil
			    for (cell . body) in bodies
			    when (and (gethash cell safe)
				      (let ((qsx::*safe-procedures* (qsx::find-safe-procedures body)))
					(qsx::may-capture-p body)))
			      do (remhash cell safe) (setq changed t)
			    finally (return changed)))))
      (labels ((plain (x)
		 (cond ((atom x) x)
		       ((and (safe-variable-ref-p x) (not (gethash (cadr x) safe)))
			(list (core "racket:variable-ref") (cadr x)))
		       (t (let ((a (plain (car x))) (d (plain (cdr x))))
			    (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d)))))))
	(mapcar #'plain forms)))))

(defun lower-split (parsed info)
  "Pass 4 for a big linklet: (racket:split N (lambda (racket:instance
racket:cells) FORM) ...), each top-level form its own lambda, every
top-level variable a cell: a variable object in the vector CELLS, imports
then exports then the rest (N cells in all)."
  (let ((*info* info) (i -1)
	(cells (core "racket:cells")))
    (flet ((cell (v)
	     (setf (variable-info-cell v) (list (core "vector-ref") cells (incf i)))))
      (loop for set in (parsed-linklet-imports parsed)
	    do (loop for (nil . internal) in set do (cell (info-of info internal))))
      (loop for (internal . nil) in (parsed-linklet-exports parsed)
	    do (cell (info-of info internal)))
      (maphash (lambda (name v)
		 (declare (ignore name))
		 (when (eq (variable-info-kind v) :defined) (cell v)))
	       (linklet-info-variables info)))
    (safe-cell-candidates parsed info)
    (list* (core "racket:split") (1+ i)
	   (settle-safe-cells
	    (loop for form in (parsed-linklet-body parsed)
		 collect (list (core "lambda") (list (core "racket:instance") cells)
			       (if (form-p form "define-values")
				   (list (core "begin")
					 (lower-assignment (cadr form) (caddr form) '())
					 (quoted *void*))
				   (lower-expr form '()))))
	    info))))

(defun lower (parsed info)
  "Pass 4: the linklet as a core lambda of its variable objects (imports,
then exports), whose body is a letrec* of the definitions; or, for a big
one, LOWER-SPLIT's form."
  (safe-struct-procedures parsed info)
  (when (> (qsx::tree-size (parsed-linklet-body parsed)) *split-size*)
    (return-from lower (lower-split parsed info)))
  (let ((*info* info))
    ;; core names: a lexical per definition, a variable object per
    ;; import and export
    (maphash (lambda (name v)
	       (when (member (variable-info-kind v) '(:defined :defined-exported))
		 (setf (variable-info-core v) (fresh-lexical (rname name))))
	       (when (member (variable-info-kind v) '(:imported :exported :defined-exported))
		 (setf (variable-info-cell v) (fresh-lexical "var"))))
	     (linklet-info-variables info))
    (let* ((imports (loop for set in (parsed-linklet-imports parsed)
			  append (loop for (nil . internal) in set
				       collect (variable-info-cell (info-of info internal)))))
	   (exports (loop for (internal . nil) in (parsed-linklet-exports parsed)
			  collect (variable-info-cell (info-of info internal))))
	   (defined (let ((l '()))
		      (maphash (lambda (name v)
				 (declare (ignore name))
				 (when (variable-info-core v) (push (variable-info-core v) l)))
			       (linklet-info-variables info))
		      l))
	   (body (loop for form in (parsed-linklet-body parsed)
		       collect (if (form-p form "define-values")
				   (lower-assignment (cadr form) (caddr form) '())
				   (lower-expr form '()))))
	   (body (if (and body (form-p (car (last (parsed-linklet-body parsed))) "define-values"))
		     (append body (list (quoted *void*)))
		     (or body (list (quoted *void*))))))
      (list (core "lambda") (list* (core "racket:instance") (append imports exports))
	    (if defined
		(cons (list (core "lambda") defined (cons (core "begin") body))
		      (mapcar (constantly (quoted qs:unassigned)) defined))
		(cons (core "begin") body))))))

;;; Exports that can't capture, for the linklets that import them
;;; (cross-linklet knowledge, through compile-linklet's get-import): a
;;; defined, exported variable never assigned, whose one definition is a
;;; lambda (or case-lambda) whose body, lowered, makes no call that can
;;; capture.  Calls among them are assumed safe and the assumption struck
;;; out where it fails, until none changes.  An importer's references to
;;; them are (racket:safe-variable-ref cell), which the continuation
;;; transformation doesn't count as capture sites.  Sound because the
;;; variable holds that procedure once defined (an earlier call is an
;;; error, not a capture), and the cache key of an importer's code
;;; includes the safe imports it was compiled with (COMPILE-LINKLET).

(defun lowered-procedure (x)
  "The lambda of X, a lowered definition's value: a lambda, a self-call
letrec, a case-lambda's racket:with-arity, or NAMED's binding; or NIL."
  (cond ((qsx::lambda-form-p x) x)
	((atom x) nil)
	((eq (car x) (core "racket:with-arity")) (lowered-procedure (caddr x)))
	((and (eq (car x) (core "letrec")) (consp (cadr x)) (null (cdr (cadr x)))
	      (eq (caddr x) (car (car (cadr x)))))
	 (lowered-procedure (cadr (car (cadr x)))))
	;; (let ((f procedure)) f), as (define g (let ([f (case-lambda ...)]) f))
	((and (qsx::lambda-form-p (car x)) (consp (cadr (car x))) (null (cdr (cadr (car x))))
	      (eq (caddr (car x)) (car (cadr (car x)))) (consp (cdr x)) (null (cddr x)))
	 (lowered-procedure (cadr x)))
	((and (qsx::lambda-form-p (car x)) (consp (cdr x)))
	 (let ((b (caddr (car x))))
	   (and (consp b) (consp (cdr b)) (consp (cadr b)) (eq (car (cadr b)) (core "set!"))
		(lowered-procedure (caddr (cadr b))))))))

(defun lowered-definitions (core targets)
  "TARGETS (lexicals and cells) -> the value each is assigned in CORE."
  (let ((found (make-hash-table :test 'equal)))
    (labels ((walk (x)
	       (when (consp x)
		 (cond ((eq (car x) (core "quote")))
		       ((and (or (eq (car x) (core "set!")) (eq (car x) (core "racket:variable-set!")))
			     (consp (cdr x)) (consp (cddr x)) (gethash (cadr x) targets))
			(setf (gethash (cadr x) found) (caddr x))
			(walk (caddr x)))
		       (t (loop for y on x while (consp y) do (walk (car y))))))))
      (walk core))
    found))

(defun case-lambda-clauses (l)
  "The clause lambdas of L, if it is LOWER-CASE-LAMBDA's dispatch,
(lambda args ((lambda (n) (if ... (apply clause args) ...)) (length args))):
each applied to the whole argument list."
  (let ((args (cadr l)))
    (when (and args (symbolp args))
      (let ((found '()) (ok t))
	(labels ((walk (x)
		   (cond ((eq x args) (setq ok nil))
			 ((atom x))
			 ((eq (car x) (core "quote")))
			 ((and (eq (car x) (core "apply")) (qsx::lambda-form-p (cadr x))
			       (eq (caddr x) args) (null (cdddr x)))
			  (push (cadr x) found))
			 ((and (eq (car x) (core "length")) (eq (cadr x) args)))
			 ((and (eq (car x) (core "racket:arity-error")) (eq (cadr x) args)))
			 (t (loop for y on x while (consp y) do (walk (car y)))))))
	  (walk (caddr l)))
	(and ok found (nreverse found))))))

(defun direct-letrec-calls (x)
  "X with each call ((letrec ((f lambda)) f) arg ...), a named let's, as
(letrec ((f lambda)) (f arg ...)), as cp0 makes it before the
continuation transformation: a call of a known procedure."
  (cond ((atom x) x)
	((eq (car x) (core "quote")) x)
	((and (consp (car x)) (eq (car (car x)) (core "letrec"))
	      (consp (cadr (car x))) (null (cdr (cadr (car x))))
	      (eq (caddr (car x)) (car (car (cadr (car x))))) (null (cdddr (car x))))
	 (let ((f (caddr (car x))))
	   (list (core "letrec") (direct-letrec-calls (cadr (car x)))
		 (cons f (mapcar #'direct-letrec-calls (cdr x))))))
	(t (labels ((each (l) (if (consp l) (cons (direct-letrec-calls (car l)) (each (cdr l))) l)))
	     (each x)))))

(defparameter *raising-natives*
  '("racket:raise" "racket:error" "racket:raise-argument-error" "racket:raise-arguments-error"
    "racket:raise-type-error" "racket:raise-range-error" "racket:raise-result-error"
    "racket:raise-mismatch-error" "racket:raise-arity-error" "racket:raise-syntax-error"
    "racket:raise-user-error")
  "Primitives that raise a non-continuable exception: a continuation
captured in its handler can't return through their caller (the handler
returning is itself an error), so to SAFE-EXPORTS a call of one isn't a
capture site.")

(defun raising-call-p (call)
  (let ((op (car call)))
    (and (symbolp op) (qsx::scheme-symbol-p op)
	 (member (qs:scheme-symbol-name op) *raising-natives* :test #'string=))))

(defun parameter-read-p (call)
  "Whether CALL reads a primitive parameter, (error-print-width): no
procedure of the program's is called."
  (let ((op (car call)))
    (and (null (cdr call)) (symbolp op) (qsx::scheme-symbol-p op)
	 (let ((name (qs:scheme-symbol-name op)))
	   (and (> (length name) 7) (string= name "racket:" :end1 7)
		(multiple-value-bind (v found) (primitive (subseq name 7))
		  (and found (parameter-data-of v))))))))

(defun core-keyword-head-p (op)
  (and (symbolp op)
       (member (symbol-name op) '("QUOTE" "IF" "BEGIN" "SET!" "DEFINE" "LAMBDA" "LETREC" "LETREC*" "PRIMITIVE" "CASE-LAMBDA")
	       :test #'string-equal)
       (qsx::scheme-symbol-p op)))

(defun safe-exports (parsed info core)
  "The linklet's exports that can't capture (above): an external name, or
(name . positions) for one that can't if the procedures at those argument
positions can't, as with map's procedure.  Every procedure the linklet
defines once is judged, so that its exports' helpers are known too."
  (let ((candidates '())		; (external-or-nil target)
	(externals (make-hash-table :test 'eq)))
    (loop for (internal . external) in (parsed-linklet-exports parsed)
	  do (setf (gethash internal externals) external))
    (maphash (lambda (name v)
	       (when (and (member (variable-info-kind v) '(:defined :defined-exported))
			  (not (variable-info-mutated v)))
		 (push (list (gethash name externals) (or (variable-info-core v) (variable-info-cell v)))
		       candidates)))
	     (linklet-info-variables info))
    (when (some #'first candidates)
      (let* ((targets (let ((h (make-hash-table :test 'equal)))
			(dolist (c candidates) (setf (gethash (second c) h) t))
			h))
	     (definitions (lowered-definitions core targets))
	     ;; (external target lambda clauses): a case-lambda's clauses are
	     ;; each judged on its own
	     (lambdas (loop for (external target) in candidates
			    for l = (lowered-procedure (gethash target definitions))
			    when l collect (list external target l (or (case-lambda-clauses l) (list l)))))
	     ;; target -> T (safe) or a list of positions (safe given those)
	     (status (make-hash-table :test 'equal)))
	(dolist (c lambdas) (setf (gethash (second c) status) t))
	(labels ((target-of (op)
		   (cond ((symbolp op) op)
			 ((and (consp op) (consp (cdr op))
			       (or (eq (car op) (core "racket:safe-variable-ref"))
				   (eq (car op) (core "racket:variable-ref"))))
			  (cadr op))))
		 (only-calls-position-p (op i)
		   ;; whether a call with operator OP doesn't call its I'th
		   ;; argument other than as a safe procedure would be
		   (let ((st (and (not (core-keyword-head-p op)) (gethash (target-of op) status))))
		     (or (eq st t) (and (consp st) (member i st))
			 (and (arg-safe-ref-p op) (member i (cadr (cadr op))))
			 (safe-ref-p op)
			 (and (safe-variable-ref-p op) (not (gethash (cadr op) targets)))
			 (let ((name (qsx::primitive-name op)))
			   (and name
				(or (not (member name qsx::*calling-primitives* :test #'string=))
				    ;; a primitive that only calls it (map's procedure)
				    (member i (cdr (assoc name qsx::*procedure-arguments* :test #'string=)))))))))
		 (only-called-p (p body)
		   ;; P is only called in BODY, or passed where it's only called
		   (labels ((ok (x)
			      (cond ((eq x p) nil)
				    ((atom x) t)
				    ((eq (car x) (core "quote")) t)
				    ((and (eq (car x) (core "set!")) (eq (cadr x) p)) nil)
				    ((core-keyword-head-p (car x))
				     (loop for y on (cdr x) always (if (consp y) (ok (car y)) (not (eq y p)))))
				    (t (and (or (eq (car x) p) (ok (car x)))
					    (loop for y on (cdr x) for i from 0
						  always (cond ((not (consp y)) (not (eq y p)))
							       ((eq (car y) p) (only-calls-position-p (car x) i))
							       (t (ok (car y))))))))))
		     (ok body)))
		 (own-call-safe-p (call)
		   ;; a call of one of these procedures, safe given its arguments
		   (let ((st (and (consp call) (not (core-keyword-head-p (car call)))
				  (gethash (target-of (car call)) status))))
		     (and (consp st)
			  (every (lambda (i) (let ((arg (nthcdr (1+ i) call)))
					       (or (null arg) (qsx::safe-procedure-p (car arg)))))
				 st))))
		 (judge-clause (l)
		   ;; T, a list of positions, or NIL
		   (let* ((formals (cadr l))
			  (proper (and (listp formals) (null (cdr (last formals)))))
			  (params (and proper
				       (loop for p in formals for i from 0
					     when (only-called-p p (caddr l)) collect i))))
		     (flet ((captures-p (assumed)
			      (let ((qsx::*safe-procedures* (make-hash-table :test 'eq)))
				(loop for target being the hash-keys of status using (hash-value st)
				      do (when (and (eq st t) (symbolp target))
					   (setf (gethash target qsx::*safe-procedures*) t)))
				(dolist (i assumed) (setf (gethash (nth i formals) qsx::*safe-procedures*) t))
				;; and the body's own letrecs and named lets
				(let* ((body (direct-letrec-calls (caddr l)))
				       (qsx::*safe-procedures* (qsx::find-safe-procedures body)))
				  (qsx::may-capture-p body)))))
		       (cond ((not (captures-p '())) t)
			     ((and params (not (captures-p params))) params)))))
		 (judge (clauses)
		   (let ((js (mapcar #'judge-clause clauses)))
		     (cond ((every (lambda (j) (eq j t)) js) t)
			   ((every #'identity js)
			    (sort (remove-duplicates (loop for j in js when (consp j) append j)) #'<))))))
	  (let ((qsx::*known-procedures* nil) (qsx::*downward-parameters* nil)
		(qsx::*safe-call-hook* (lambda (call) (or (arg-safe-call-p call) (own-call-safe-p call) (raising-call-p call) (parameter-read-p call))))
		(qsx::*safe-operator-hook*
		  (lambda (form)
		    (or (safe-ref-p form)
			(and (safe-variable-ref-p form)
			     ;; an import found safe, or one of these cells
			     (or (not (gethash (cadr form) targets)) (eq (gethash (cadr form) status) t)))
			;; a split linklet's cell of one of these found safe
			(and (consp form) (eq (car form) (core "racket:variable-ref"))
			     (eq (gethash (cadr form) status) t))))))
	    ;; from the most optimistic, judged again until nothing changes
	    (loop repeat 30
		  for changed = nil
		  do (loop for (nil target nil clauses) in lambdas
			   do (let ((new (judge clauses)))
				(unless (equal new (gethash target status))
				  (setq changed t)
				  (if new (setf (gethash target status) new) (remhash target status)))))
		  until (not changed)
		  finally (when changed (clrhash status)))))
	(loop for (external target) in lambdas
	      for st = (and external (gethash target status))
	      when (eq st t) collect external
	      when (consp st) collect (cons external st))))))

(defparameter *native-exports*
  '(("map" ("map2" "%full-map" "map") ("for-each2" "%full-for-each" "for-each")))
  "Module (the last element of its name) -> exports replaced, once its
body has run, by host primitives that do the same: racket/private/map's
map and for-each, written in Racket, as the host's frame-aware %full-map
and %full-for-each, which re-entered continuations go through as well.
Importers know them safe when the procedure passed them is.")

(defun linklet-module-name (name)
  "The module a linklet named NAME (a hasheq with module) is the body of."
  (and (ihash-p name)
       (multiple-value-bind (m found) (lookup* name (rsym "module"))
	 (and found (symbolp m) m (not (eq m qs:false)) (rname m)))))

(defun native-overrides (name parsed info)
  "((cell . host-name) ...) for the exports of the linklet named NAME that
*NATIVE-EXPORTS* replaces, and the safe-export entries they make."
  (let ((entry (cdr (assoc (linklet-module-name name) *native-exports* :test #'equal))))
    (when (and entry
	       (every (lambda (e) (find (car e) (parsed-linklet-exports parsed) :key (lambda (x) (rname (cdr x))) :test #'string=))
		      entry))
      (loop for (export full plain) in entry
	    for (internal . external) = (find export (parsed-linklet-exports parsed) :key (lambda (x) (rname (cdr x))) :test #'string=)
	    for v = (gethash internal (linklet-info-variables info))
	    collect (cons (variable-info-cell v) full) into sets
	    ;; importers call the host primitive itself: the continuation
	    ;; transformation then uses its plain or frame-aware version, as
	    ;; for R7RS's map
	    collect (list external :host plain) into safe
	    finally (return (values sets safe))))))

(defun add-native-overrides (core sets)
  "CORE, a lowered whole linklet (lambda params body), with SETS' cells
assigned their host primitives after BODY, whose value it still returns."
  (if (or (null sets) (not (qsx::lambda-form-p core)))
      core
      (let ((r (fresh-lexical "result")))
	(list (car core) (cadr core)
	      (list (list (core "lambda") (list r)
			  (cons (core "begin")
				(append (loop for (cell . host) in sets
					      collect (list (core "racket:variable-set!") cell (core host)))
					(list r))))
		    (caddr core))))))

(defun compile-linklet-form (form &key name imports-known options split)
  "A linklet S-expression to a COMPILED-LINKLET (cache.lisp).  OPTIONS
are compile-linklet's: serializable unsafe static use-prompt
uninterned-literal quick.  SPLIT: split, whatever its size."
  (declare (ignore options))
  (let* ((*split-size* (if split -1 *split-size*))
	 (*safe-locals* (make-hash-table :test 'eq))
	 (info (make-linklet-info))
	 (parsed (analyse (parse-linklet (decorrelate form info)) info imports-known))
	 (core (lower parsed info))
	 (compiled nil))
    (multiple-value-bind (sets safe) (and (qsx::lambda-form-p core) (native-overrides name parsed info))
      (setq core (add-native-overrides core sets))
      (setq compiled (make-compiled-linklet core :name name :strategy "linklet"))
      (setf (compiled-linklet-safe-exports compiled)
	    (append safe (remove-if (lambda (e) (find (if (consp e) (car e) e) safe :key #'car))
				    (safe-exports parsed info core)))))
    (setf (compiled-linklet-imports compiled)
	  (mapcar (lambda (set) (mapcar #'car set)) (parsed-linklet-imports parsed))
	  (compiled-linklet-exports compiled)
	  (mapcar #'cdr (parsed-linklet-exports parsed))
	  (compiled-linklet-export-internals compiled)
	  (mapcar #'car (parsed-linklet-exports parsed)))
    compiled))
