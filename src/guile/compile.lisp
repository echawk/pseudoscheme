; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Tree-IL, Guile's expanded code, compiled to Pseudoscheme's core
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

(in-package "PSEUDOSCHEME-GUILE")

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
				  ps:false))))
    (coerce (loop for (stem . fields) in *tree-il-types*
		  collect (make-struct* meta
					(list (layout-symbol (apply #'concatenate 'string
								    (mapcar (constantly "pw") fields)))
					      ps:false (ssym stem) (length fields)
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
		  (cdr (assoc (ps:scheme-symbol-name name)
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
  (format stream "#<site ~A>" (ps:scheme-symbol-name (site-name s))))

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

(defun signal-unbound-variable (name)
  (guile-error (ssym "unbound-variable") ps:false "Unbound variable: ~S" (list name) ps:false))

(defun site-ref (site)
  (let ((v (or (site-variable site)
	       (let ((v (site-lookup site)))
		 (unless v (signal-unbound-variable (site-name site)))
		 (setf (site-variable site) v)))))
    (let ((x (gvariable-value v)))
      (when (eq x +unbound+) (signal-unbound-variable (site-name site)))
      x)))

(defun site-set (site value)
  (let ((v (or (site-variable site)
	       (let ((v (site-lookup site)))
		 (unless v (signal-unbound-variable (site-name site)))
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
  (ssym (format nil "~~~D.~A" (incf *lexical-count*) *lexical-tag*)))

(defun lexical (gensym)
  (or (gethash gensym *lexicals*)
      ;; free in the node being compiled: can't happen in psyntax's output
      (error "Tree-IL: unbound lexical ~S" gensym)))

(defun bind-lexical (gensym)
  (setf (gethash gensym *lexicals*) (fresh-lexical)))

(defun quoted (x) (list (core "quote") x))

(defparameter *open-primitives*
  '("car" "cdr" "cons" "list" "append" "vector" "list->vector" "apply" "values"
    "call-with-values" "call-with-current-continuation" "eq?" "eqv?" "equal?"
    "pair?" "string?" "vector?" "procedure?" "char?" "number?" "integer?" "zero?"
    "+" "-" "*" "/" "<" ">" "<=" ">=" "=" "vector-ref" "vector-set!" "vector-length"
    "string-ref" "string-length" "length" "memq" "memv" "member" "assq" "assv" "assoc"
    "caar" "cadr" "cdar" "cddr" "caddr" "cdddr" "cadddr" "set-car!" "set-cdr!" "reverse"
    "list?" "boolean?" "char=?" "string=?" "quotient" "remainder" "modulo" "map" "for-each"
    "call-with-prompt" "abort-to-prompt" "dynamic-wind")
  "Guile primitives that are Pseudoscheme's host procedures of the same
name and behaviour: a primcall of one compiles to a call of the host's.")

(defparameter *primcall-renames*
  '(("call/cc" . "call-with-current-continuation") ("abort-to-prompt*" . "%guile-abort*"))
  "Guile primitives whose host procedure has another name.")

(defun primitive-operator (name)
  "The core expression for Guile primitive NAME (a symbol)."
  (let* ((string (ps:scheme-symbol-name name))
	 (rename (cdr (assoc string *primcall-renames* :test #'string=))))
    (cond (rename (core rename))
	  ((member string *open-primitives* :test #'string=) (core string))
	  ((member string '("wind" "unwind" "push-fluid" "pop-fluid" "push-dynamic-state"
			    "pop-dynamic-state")
		   :test #'string=)
	   ;; only boot-9's dynamic-wind, with-fluid* and with-dynamic-state
	   ;; use these, and they are replaced (BOOT-OVERRIDES)
	   (list (core "%guile-unsupported") (quoted name)))
	  (t (root-reference name)))))

(defun root-reference (name)
  "A reference to NAME in the (guile) module."
  (list (core "%guile-ref") (quoted (make-site :root name nil :toplevel))))

(defun compile-tree-il (x)
  (ecase (node-type x)
    (:void (quoted *unspecified*))
    (:const (quoted (node-field x 1)))
    (:primitive-ref (primitive-operator (node-field x 1)))
    (:lexical-ref (lexical (node-field x 2)))
    (:lexical-set (list (core "set!") (lexical (node-field x 2)) (compile-tree-il (node-field x 3))))
    (:module-ref
     (list (core "%guile-ref")
	   (quoted (make-site (node-field x 1) (node-field x 2) (truthy (node-field x 3)) :module))))
    (:module-set
     (list (core "%guile-set!")
	   (quoted (make-site (node-field x 1) (node-field x 2) (truthy (node-field x 3)) :module))
	   (compile-tree-il (node-field x 4))))
    (:toplevel-ref
     (list (core "%guile-ref") (quoted (make-site *compile-module* (node-field x 2) nil :toplevel))))
    (:toplevel-set
     (list (core "%guile-set!") (quoted (make-site *compile-module* (node-field x 2) nil :toplevel))
	   (compile-tree-il (node-field x 3))))
    (:toplevel-define
     (list (core "%guile-define!") (quoted *compile-module*) (quoted (node-field x 2))
	   (compile-tree-il (node-field x 3))))
    (:conditional
     (list (core "if") (compile-test (node-field x 1)) (compile-tree-il (node-field x 2))
	   (compile-tree-il (node-field x 3))))
    (:call (cons (compile-tree-il (node-field x 1)) (mapcar #'compile-tree-il (node-field x 2))))
    (:primcall (let ((name (node-field x 1)) (args (node-field x 2)))
		 (cons (if (and (/= (length args) 2)
				(member (ps:scheme-symbol-name name) '("eq?" "eqv?" "equal?") :test #'string=))
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
    (:lambda (compile-lambda (node-field x 2)))
    (:let (let ((vals (mapcar #'compile-tree-il (node-field x 3)))
		(vars (mapcar #'bind-lexical (node-field x 2))))
	    (cons (list (core "lambda") vars (compile-tree-il (node-field x 4))) vals)))
    (:fix (compile-letrec (node-field x 2) (node-field x 3) (node-field x 4)))
    (:let-values
     (list (core "call-with-values")
	   (list (core "lambda") '() (compile-tree-il (node-field x 1)))
	   (compile-lambda (node-field x 2))))
    (:prompt				; the body is a thunk
     (list (core "call-with-prompt") (compile-tree-il (node-field x 2))
	   (compile-tree-il (node-field x 3)) (compile-tree-il (node-field x 4))))
    (:abort
     (list* (core "apply") (core "abort-to-prompt") (compile-tree-il (node-field x 1))
	    (append (mapcar #'compile-tree-il (node-field x 2))
		    (list (compile-tree-il (node-field x 3))))))
    (:letrec (compile-letrec (node-field x 3) (node-field x 4) (node-field x 5)))))

(defun compile-letrec (gensyms vals body)
  ;; as psyntax's letrec* for the translator: ((lambda (v ...) (set! v e) ... body) #f ...),
  ;; which src/psyntax.lisp makes a LABELS when the e are lambdas
  (let* ((vars (mapcar #'bind-lexical gensyms))
	 (vals (mapcar #'compile-tree-il vals)))
    (cons (list (core "lambda") vars
		(list* (core "begin")
		       (append (mapcar (lambda (v e) (list (core "set!") v e)) vars vals)
			       (list (compile-tree-il body)))))
	  (mapcar (constantly (quoted ps:false)) vars))))

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
		(let ((s (ps:scheme-symbol-name name)))
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
	(t (let ((args (fresh-lexical)))
	     (list (core "lambda") args (compile-clauses case args))))))

(defun simple-case-p (case)
  (and (not (truthy (node-field case 2))) (not (truthy (node-field case 4)))))

(defun compile-clauses (case args)
  (if (not (node-type case))
      (list (core "%guile-arity-error"))
      (let* ((req (length (node-field case 1)))
	     (opt (if (truthy (node-field case 2)) (length (node-field case 2)) 0))
	     (rest (truthy (node-field case 3)))
	     (kw (truthy (node-field case 4)))
	     (test (if (or rest kw)
		       (list (core "%guile-nargs>=") args req)
		       (list (core "%guile-nargs-between") args req (+ req opt)))))
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
					    (quoted (if allow-other-keys ps:true ps:false))
					    (quoted (if rest ps:true ps:false))))
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

(defun install-compiler-primitives ()
  (psx:defhost "%guile-ref" (site) (site-ref site))
  (psx:defhost "%guile-true" (x) (if (eq x *elisp-nil*) ps:false x))
  (psx:defhost "%guile-set!" (site value) (site-set site value))
  (psx:defhost "%guile-define!" (module name value) (define-in-module module name value))
  (psx:defhost "%guile-unsupported" (name)
    (lambda (&rest args)
      (declare (ignore args))
      (error "Guile primitive ~A isn't supported here" (ps:scheme-symbol-name name))))
  (psx:defhost "%guile-arity-error" ()
    (guile-error (ssym "wrong-number-of-args") ps:false "Wrong number of arguments" '()))
  (psx:defhost "%guile-abort*" (tag args) (apply #'psx::abort-to-prompt tag args))
  (psx:defhost "%guile-nargs>=" (args n) (bool (nthcdr-p args n)))
  (psx:defhost "%guile-nargs-between" (args min max)
    (let ((n (list-length args))) (bool (<= min n max))))
  (psx:defhost "%guile-positional?" (tail) (bool (and (consp tail) (not (keywordp (car tail))))))
  (psx:defhost "%guile-check-no-more" (tail)
    (when tail (funcall (psx:host-ref "%guile-arity-error")))
    *unspecified*)
  (psx:defhost "%guile-check-keywords" (tail keywords allow-other rest)
    (check-keywords tail keywords (truthy allow-other) (truthy rest)))
  (psx:defhost "%guile-keyword-ref" (tail keyword)
    (loop for x on tail by #'cddr
	  when (and (eq (car x) keyword) (consp (cdr x))) return (cadr x)
	  finally (return +unbound+)))
  (psx::register-primitive-names
   '("%guile-ref" "%guile-true" "%guile-set!" "%guile-define!" "%guile-unsupported" "%guile-arity-error"
     "%guile-nargs>=" "%guile-nargs-between" "%guile-positional?" "%guile-check-no-more"
     "%guile-check-keywords" "%guile-keyword-ref")))

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
		      (guile-error (ssym "keyword-argument-error") ps:false
				   "Keyword argument has no value" '() (list k)))
		    (unless (or allow-other (member k keywords))
		      (guile-error (ssym "keyword-argument-error") ps:false
				   "Unrecognized keyword" '() (list k)))
		    (setq tail (cddr tail)))
		   (rest (setq tail (cdr tail)))
		   (t (guile-error (ssym "keyword-argument-error") ps:false
				   "Invalid keyword" '() (list k))))))
  *unspecified*)

;;; ------------------------------------------------------------------
;;; The pre-expander: libguile's expand.c, for what comes before psyntax

(defvar *pre-env* '() "symbol -> gensym, the lexical variables in scope")

(defun keyword-is (x name)
  (and (symbolp x) x (not (keywordp x)) (string= (ps:scheme-symbol-name x) name)
       (not (assoc x *pre-env*))))

(defun pre-gensym (name)
  (ssym (format nil "~A-~D" (ps:scheme-symbol-name name) (incf *guile-gensym-counter*))))

(defun pre-expand (x &rest ignore)
  (declare (ignore ignore))
  (let ((*pre-env* '()))
    (pre-x x)))

(defun pre-seq (forms)
  (cond ((null forms) (make-node "void" ps:false))
	((null (cdr forms)) (pre-x (car forms)))
	(t (make-node "seq" ps:false (pre-x (car forms)) (pre-seq (cdr forms))))))

(defun pre-x (x)
  (cond ((and (symbolp x) x (not (keywordp x)) (ps:scheme-symbol-p x))
	 (let ((b (assoc x *pre-env*)))
	   (if b
	       (make-node "lexical-ref" ps:false x (cdr b))
	       (make-node "toplevel-ref" ps:false ps:false x))))
	((not (consp x)) (make-node "const" ps:false x))
	(t (let ((head (car x)))
	     (flet ((is (name) (keyword-is head name)))
	       (cond ((is "quote") (make-node "const" ps:false (cadr x)))
		     ((is "quasiquote") (pre-x (quasi (cadr x) 1)))
		     ((is "if") (make-node "conditional" ps:false (pre-x (cadr x)) (pre-x (caddr x))
					   (if (cdddr x) (pre-x (cadddr x)) (make-node "void" ps:false))))
		     ((is "define")
		      (if (consp (cadr x))
			  (pre-x (list (ssym "define") (caadr x)
				       (list* (ssym "lambda") (cdadr x) (cddr x))))
			  (make-node "toplevel-define" ps:false ps:false (cadr x)
				     (named (cadr x) (pre-x (caddr x))))))
		     ((is "set!")
		      (let ((b (assoc (cadr x) *pre-env*)))
			(if b
			    (make-node "lexical-set" ps:false (cadr x) (cdr b) (pre-x (caddr x)))
			    (make-node "toplevel-set" ps:false ps:false (cadr x) (pre-x (caddr x))))))
		     ((is "begin") (pre-seq (cdr x)))
		     ((or (is "lambda") (is "lambda*"))
		      (make-node "lambda" ps:false '() (pre-clause (cadr x) (cddr x) ps:false)))
		     ((or (is "case-lambda") (is "case-lambda*"))
		      (make-node "lambda" ps:false '()
				 (reduce (lambda (clause alt) (pre-clause (car clause) (cdr clause) alt))
					 (cdr x) :from-end t :initial-value ps:false)))
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
		     ((or (is "letrec") (is "letrec*")) (pre-letrec (cadr x) (cddr x)))
		     ((is "and") (cond ((null (cdr x)) (make-node "const" ps:false ps:true))
				       ((null (cddr x)) (pre-x (cadr x)))
				       (t (make-node "conditional" ps:false (pre-x (cadr x))
						     (pre-x (cons head (cddr x)))
						     (make-node "const" ps:false ps:false)))))
		     ((is "or") (cond ((null (cdr x)) (make-node "const" ps:false ps:false))
				      ((null (cddr x)) (pre-x (cadr x)))
				      (t (let ((tmp (gensym-symbol "t")))
					   (pre-x `(,(ssym "let") ((,tmp ,(cadr x)))
						    (,(ssym "if") ,tmp ,tmp (,head ,@(cddr x)))))))))
		     ((is "when") (pre-x `(,(ssym "if") ,(cadr x) (,(ssym "begin") ,@(cddr x)))))
		     ((is "unless") (pre-x `(,(ssym "if") ,(cadr x) (,(ssym "if") #.ps:false #.ps:false) (,(ssym "begin") ,@(cddr x)))))
		     ((is "cond") (pre-x (expand-cond (cdr x))))
		     ((is "case") (pre-x (expand-case (cadr x) (cddr x))))
		     ((is "do") (pre-x (expand-do x)))
		     ((or (is "@") (is "@@"))
		      (if (and (keyword-is (cadr x) "primitive") (is "@@"))
			  (make-node "primitive-ref" ps:false (caddr x))
			  (make-node "module-ref" ps:false (cadr x) (caddr x) (bool (is "@")))))
		     ((is "eval-when")
		      (if (some (lambda (s) (member (ps:scheme-symbol-name s) '("eval" "load" "expand")
						    :test #'string=))
				(cadr x))
			  (pre-seq (cddr x))
			  (make-node "void" ps:false)))
		     ((and (consp head) (keyword-is (car head) "@@") (keyword-is (cadr head) "primitive"))
		      (make-node "primcall" ps:false (caddr head) (mapcar #'pre-x (cdr x))))
		     (t (make-node "call" ps:false (pre-x head) (mapcar #'pre-x (cdr x))))))))))

(defun gensym-symbol (stem) (ssym (format nil " ~A~D" stem (incf *guile-gensym-counter*))))

(defun named (name node)
  (if (eq (node-type node) :lambda)
      (make-node "lambda" ps:false (acons (ssym "name") name (node-field node 1)) (node-field node 2))
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
	 (vals (mapcar (lambda (b) (pre-x (if (and (consp b) (cdr b)) (cadr b) ps:false))) bindings))
	 (gensyms (mapcar #'pre-gensym names)))
    (let ((*pre-env* (append (mapcar #'cons names gensyms) *pre-env*)))
      (make-node "let" ps:false names gensyms vals (pre-body body)))))

(defun pre-letrec (bindings body)
  (let* ((names (mapcar #'car bindings))
	 (gensyms (mapcar #'pre-gensym names)))
    (let ((*pre-env* (append (mapcar #'cons names gensyms) *pre-env*)))
      (make-node "letrec" ps:false ps:true names gensyms
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
		       ((eq mode :opt) (push (if (consp f) f (list f ps:false)) opt))
		       (t (push (if (consp f) f (list f ps:false)) kw)))))))
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
	(make-node "lambda-case" ps:false req
		   (if opt (mapcar #'car opt) ps:false)
		   (or rest ps:false)
		   (if (or kw allow-other) (cons (bool allow-other) (nreverse kw-spec)) ps:false)
		   (nreverse inits) (nreverse gensyms) (pre-body body) alternate)))))

(defun expand-cond (clauses)
  (if (null clauses)
      (list (ssym "if") ps:false ps:false)
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
