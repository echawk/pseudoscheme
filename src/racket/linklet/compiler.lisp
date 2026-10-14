; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

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

(in-package "PSEUDOSCHEME-RACKET")

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

(defun core (name) (psx::sym name))
(defun quoted (x) (list (core "quote") x))
(defun form-p (x name) (and (consp x) (symbolp (car x)) (string= (rname (car x)) name)))

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
  (psx::make-lexical
   (ps::invert-case (format nil "~A~~~D.~A" stem (incf *lexical-count*) *lexical-tag*))))

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
is what get-import told us about the imported linklets' variables
(cross-linklet knowledge): not used yet."
  (declare (ignore imports-known))
  (loop for set in (parsed-linklet-imports parsed)
	do (loop for (nil . internal) in set
		 do (setf (variable-info-kind (info-of info internal)) :imported)))
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
    (cond (local (cdr local))
	  ((let ((v (gethash name (linklet-info-variables *info*))))
	     (and v (case (variable-info-kind v)
		      ((:defined :defined-exported)
		       ;; a lexical; in a split linklet, a cell
		       (or (variable-info-core v)
			   (list (core "racket:variable-ref") (variable-info-cell v))))
		      ((:imported :exported)
		       (list (core "racket:variable-ref") (variable-info-cell v)))))))
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
    (list (core "lambda") formals (lower-body body env))))

(defun rest-formal (formals)
  "The rest argument of FORMALS (a b . c), if it has one."
  (loop for tail = formals then (cdr tail)
	while (consp tail)
	finally (return (and tail (symbolp tail) tail))))

(defun lower-case-lambda (clauses env)
  "One clause is a plain lambda; several dispatch on the argument count."
  (if (and clauses (null (cdr clauses)))
      (lower-lambda (car (first clauses)) (cdr (first clauses)) env)
      (let ((args (fresh-lexical "args")) (n (fresh-lexical "n")))
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
		    (list (core "length") args))))))

(defun lower-let-values (bindings body env &optional recursive)
  "let-values: each right-hand side in the outer ENV, then all bound.
letrec-values: as Racket's, a letrec* (left to right, each variable
assigned as its right-hand side returns)."
  (if recursive
      (let* ((ids (loop for (ids) in bindings append ids))
	     (env (bind ids env))
	     (vars (mapcar (lambda (id) (cdr (assoc id env))) ids)))
	(cons (list (core "lambda") vars
		    (cons (core "begin")
			  (append (loop for (ids rhs) in bindings
					collect (lower-assignment ids rhs env))
				  (list (lower-body body env)))))
	      (mapcar (constantly (quoted ps:false)) vars)))
      (labels ((nest (bindings inner-env)
		 (if (null bindings)
		     (lower-body body inner-env)
		     (destructuring-bind ((ids rhs) &rest more) bindings
		       (multiple-value-bind (inner-env formals) (bind ids inner-env)
			 (if (and (consp ids) (null (cdr ids)))
			     (list (list (core "lambda") formals (nest more inner-env))
				   (lower-expr rhs env))
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
		(quoted ps:false)))
	core)))

(defun lower-assignment (ids rhs env)
  "IDS (already bound in ENV) assigned RHS's values."
  (if (and (consp ids) (null (cdr ids)))
      (lower-set (car ids) (named (car ids) rhs (lower-expr rhs env) env) env)
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
	 (if (or (null x) (eq x ps:false) (eq x ps:true)) (quoted x) (lookup x env)))
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
	     (:case-lambda (lower-case-lambda args env))
	     (:let-values (lower-let-values (car args) (cdr args) env))
	     (:letrec-values (lower-let-values (car args) (cdr args) env t))
	     (:set! (lower-set (first args) (lower-expr (second args) env) env))
	     (:with-continuation-mark
	      (list (core "racket:call-with-mark") (lower-expr (first args) env)
		    (lower-expr (second args) env)
		    (list (core "lambda") '() (lower-expr (third args) env))))
	     (:variable-reference
	      (list (core "racket:variable-reference") (core "racket:instance")
		    (if args (quoted (car args)) (quoted ps:false))))
	     (:define-values
	      (error "compile-linklet: define-values not at a linklet's top level: ~S" x)))))
	(t (mapcar (lambda (y) (lower-expr y env)) x))))

(defparameter *special-forms*
  '(("quote" . :quote) ("if" . :if) ("begin" . :begin) ("begin0" . :begin0)
    ("lambda" . :lambda) ("case-lambda" . :case-lambda) ("let-values" . :let-values)
    ("letrec-values" . :letrec-values) ("set!" . :set!) ("define-values" . :define-values)
    ("with-continuation-mark" . :with-continuation-mark)
    ("#%variable-reference" . :variable-reference)))

(defun special-form (symbol)
  (cdr (assoc (rname symbol) *special-forms* :test #'string=)))

(defparameter *split-size* 20000
  "A linklet bigger than this (in conses) is compiled form by form, as
Racket CS compiles a big linklet's outer level piecewise: compiled whole,
the expander's 2 MB linklet needs more than 8 GB to compile.")

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
    (list* (core "racket:split") (1+ i)
	   (loop for form in (parsed-linklet-body parsed)
		 collect (list (core "lambda") (list (core "racket:instance") cells)
			       (if (form-p form "define-values")
				   (list (core "begin")
					 (lower-assignment (cadr form) (caddr form) '())
					 (quoted *void*))
				   (lower-expr form '())))))))

(defun lower (parsed info)
  "Pass 4: the linklet as a core lambda of its variable objects (imports,
then exports), whose body is a letrec* of the definitions; or, for a big
one, LOWER-SPLIT's form."
  (when (> (psx::tree-size (parsed-linklet-body parsed)) *split-size*)
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
		      (mapcar (constantly (quoted ps:unassigned)) defined))
		(cons (core "begin") body))))))

(defun compile-linklet-form (form &key name imports-known options)
  "A linklet S-expression to a COMPILED-LINKLET (cache.lisp).  OPTIONS
are compile-linklet's: serializable unsafe static use-prompt
uninterned-literal quick."
  (declare (ignore options))
  (let* ((info (make-linklet-info))
	 (parsed (analyse (parse-linklet (decorrelate form info)) info imports-known))
	 (core (lower parsed info))
	 (compiled (make-compiled-linklet core :name name :strategy "linklet")))
    (setf (compiled-linklet-imports compiled)
	  (mapcar (lambda (set) (mapcar #'car set)) (parsed-linklet-imports parsed))
	  (compiled-linklet-exports compiled)
	  (mapcar #'cdr (parsed-linklet-exports parsed))
	  (compiled-linklet-export-internals compiled)
	  (mapcar #'car (parsed-linklet-exports parsed)))
    compiled))
