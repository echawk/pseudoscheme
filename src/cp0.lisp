; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-PSYNTAX -*-

;;;; cp0: a source-level optimizer for core forms (docs/optimizations.md,
;;;; "Plan (2026-10-18)", item 1), after the model of Chez Scheme's cp0
;;;; (Waddell and Dybvig, "Fast and Effective Procedure Inlining", 1997),
;;;; much simplified.
;;;;
;;;; OPTIMIZE-CORE runs on every core form between OPEN-PRIMITIVES (or
;;;; OPEN-TOP-LEVEL) and the continuation transformation, whatever made
;;;; the form: psyntax (R6RS, R7RS, R5RS and the older reports, Chez
;;;; mode), Guile's Tree-IL compiler, Racket's linklet compiler.  It
;;;;
;;;;  - inlines calls of small known procedures: a variable bound by a
;;;;    letrec all of whose inits are lambdas (a library's or a program's
;;;;    body of definitions, made so by DEFINITIONS-AS-LETREC), or by a
;;;;    let, and never assigned;
;;;;  - propagates constants and copies bound by lets (an unassigned
;;;;    variable bound to a constant, a primitive, or another such
;;;;    variable);
;;;;  - folds calls of pure primitives on constants, and IFs on constant
;;;;    tests;
;;;;  - drops code whose value isn't used and that has no effect, and the
;;;;    bindings nothing refers to.
;;;;
;;;; Then CPTYPES (src/cptypes.lisp) recovers types, and CPDX
;;;; (src/cpdx.lisp) finds what can be allocated on the stack.
;;;;
;;;; Soundness.  Only lexical variables are known, and only within one
;;;; core form: top-level variables (a REPL's, Chez mode's, Guile's
;;;; modules', the R5RS top level's) are late-bound, as before.  A
;;;; procedure is inlined only at a call, never copied to where its value
;;;; is used, so procedures keep their identity.  A call is inlined only
;;;; with exactly the arguments the procedure takes: a wrong count is left
;;;; to raise its error.  Folding calls the primitive itself, and keeps
;;;; the call if that raises anything or the result is not a plain number
;;;; (finite, if a flonum), character or boolean.  The continuation
;;;; transformation runs on the result, so it sees inlined bodies as
;;;; they are.  Letrec bindings are inlined only when every init is a
;;;; lambda, so no init can run before a variable it calls is defined.
;;;;
;;;; QUASISCHEME_OPTIMIZE=0 turns the pass off, =1 runs cp0 only, and
;;;; anything else (the default) runs cp0, cptypes and cpdx.  The level is part
;;;; of BUILD-SIGNATURE, so compiled-library caches don't mix levels.

(in-package "QUASISCHEME-PSYNTAX")

;;; ------------------------------------------------------------------
;;; The switch

(defvar *optimize-off* nil
  "True to skip OPTIMIZE-CORE (compiling psyntax's own image).")

(defun optimize-level ()
  "0: no optimization; 1: cp0; 2: cp0 and cptypes (QUASISCHEME_OPTIMIZE)."
  (if *optimize-off*
      0
      (let ((v (uiop:getenv "QUASISCHEME_OPTIMIZE")))
	(cond ((or (null v) (string= v "")) 2)
	      ((string= v "0") 0)
	      ((string= v "1") 1)
	      (t 2)))))

(defparameter *cp0-trace* nil
  "True to print each form before and after optimization (for debugging;
QUASISCHEME_OPTIMIZE_TRACE=1).")

(defvar *cp0-stats* (list 0 0 0 0 nil 0 0)
  "Forms optimized, forms given up on, calls inlined, calls folded, the
reasons for giving up, and cpdx's closures and rest lists put on the
stack (src/cpdx.lisp).")

(defun report-optimizer-stats ()
  "With QUASISCHEME_OPTIMIZE_STATS=1, print *CP0-STATS* on exit: whether,
and how much, the optimizer ran in a mode."
  (when (equal (uiop:getenv "QUASISCHEME_OPTIMIZE_STATS") "1")
    (destructuring-bind (ok bailed inlined folded reasons dx-closures dx-rests) *cp0-stats*
      (format *error-output* "~&;; optimizer: ~D forms optimized, ~D given up on~@[ (~{~A~^; ~})~]; ~D calls inlined, ~D folded; ~D closures and ~D rest lists on the stack~%"
	      ok bailed (subseq reasons 0 (min 5 (length reasons))) inlined folded dx-closures dx-rests))))

(defun reset-optimizer-stats ()
  (setq *cp0-stats* (list 0 0 0 0 nil 0 0)))

#+sbcl (pushnew 'report-optimizer-stats sb-ext:*exit-hooks*)
#+sbcl (pushnew 'reset-optimizer-stats sb-ext:*init-hooks*)   ; a saved image's own count

;;; ------------------------------------------------------------------
;;; Core syntax

(defvar *cp-keywords* nil)

(defun cp-keyword (x)
  "The core keyword X is, as a Lisp keyword (:LAMBDA, ...), or NIL."
  (and (symbolp x) x
       (gethash x (or *cp-keywords*
		      (setq *cp-keywords*
			    (let ((h (make-hash-table :test 'eq)))
			      (loop for (name key) in '(("quote" :quote) ("if" :if) ("begin" :begin)
							("set!" :set!) ("define" :define) ("lambda" :lambda)
							("case-lambda" :case-lambda) ("letrec" :letrec)
							("letrec*" :letrec*) ("primitive" :primitive))
				    do (setf (gethash (sym name) h) key))
			      h))))))

(defvar *cp-variable-cache* (make-hash-table :test 'eq :weakness :key))

(defun host-variable-name-p (x)
  "Whether X, a symbol of SCHEME, names a variable of *HOST* rather than a
keyword (a macro or special form of the translator's)."
  (let ((entry (gethash x *cp-variable-cache*)))
    (if (and entry (eq (car entry) *host*))
	(cdr entry)
	(let ((v (qsl:variable-node-p (qsl:tr "PROGRAM-ENV-LOOKUP" *host* x))))
	  (setf (gethash x *cp-variable-cache*) (cons *host* v))
	  v))))

(defun cp-quote (datum) (list (sym "quote") datum))
(defun cp-void () (cp-quote qs:unspecific))
(defun quote-p (e) (and (consp e) (eq (cp-keyword (car e)) :quote)))
(defun cp-lambda-p (e) (and (consp e) (eq (cp-keyword (car e)) :lambda)))

(defun constant-p (e)
  "Whether E is a constant expression; its value second."
  (cond ((quote-p e) (values t (cadr e)))
	((and (atom e) (not (symbolp e))) (values t e))
	(t (values nil nil))))

(defun scheme-true-p (datum) (not (eq datum qs:false)))

(defun formals-list (formals)
  "The variables of lambda FORMALS, a proper or dotted list or a symbol."
  (loop with l = formals
	while (consp l) collect (pop l) into vars
	finally (return (if (and l (symbolp l)) (append vars (list l)) vars))))

(defun name-marker-p (e)
  (and (quote-p e) (consp (cadr e)) (eq (car (cadr e)) :procedure-name)))

;;; ------------------------------------------------------------------
;;; Analysis: every variable the form binds, how, and whether it's assigned

(defstruct (vi (:constructor make-vi ()))
  (binds 0)		; binding occurrences (more than one: names not unique)
  (sets 0)		; set!s
  kind			; :param or :letrec
  lambda-letrec)	; bound by a letrec whose inits are all lambdas

(defvar *vi*)

(defun vi (x) (and (symbolp x) (gethash x *vi*)))

(defun cp-bail (why)
  (throw 'cp-bail why))

(defun proper-list-p (l)
  (loop (cond ((null l) (return t)) ((atom l) (return nil)) (t (pop l)))))

(defun analyze (form)
  "Fill *VI* for FORM; give up (CP-BAIL) on a form this pass doesn't know."
  (labels ((bind (v kind &optional lambda-letrec)
	     (unless (and (symbolp v) v) (cp-bail "bad binder"))
	     (let ((info (or (gethash v *vi*) (setf (gethash v *vi*) (make-vi)))))
	       (incf (vi-binds info))
	       (setf (vi-kind info) kind (vi-lambda-letrec info) lambda-letrec)))
	   (body (forms)
	     (unless (and (consp forms) (proper-list-p forms)) (cp-bail "bad body"))
	     (mapc #'walk forms))
	   (walk (e)
	     (cond ((atom e))
		   ((not (proper-list-p e)) (cp-bail "dotted form"))
		   ((and (symbolp (car e)) (cp-keyword (car e)))
		    (ecase (cp-keyword (car e))
		      ((:quote :primitive))
		      (:if (unless (<= 3 (length e) 4) (cp-bail "bad if"))
		       (mapc #'walk (cdr e)))
		      (:begin (when (null (cdr e)) (cp-bail "empty begin"))
		       (mapc #'walk (cdr e)))
		      ((:set! :define)
		       (unless (and (= (length e) 3) (symbolp (cadr e))) (cp-bail "bad set!"))
		       (when (eq (cp-keyword (car e)) :set!)
			 (let ((info (gethash (cadr e) *vi*)))
			   ;; a set! before the binder is seen is counted when it is
			   (if info (incf (vi-sets info)) (push (cadr e) pending-sets))))
		       (walk (caddr e)))
		      (:lambda (unless (consp (cdr e)) (cp-bail "bad lambda"))
		       (dolist (v (formals-list (cadr e))) (bind v :param))
		       (body (cddr e)))
		      (:case-lambda
		       (dolist (clause (cdr e))
			 (unless (consp clause) (cp-bail "bad case-lambda"))
			 (dolist (v (formals-list (car clause))) (bind v :param))
			 (body (cdr clause))))
		      ((:letrec :letrec*)
		       (unless (and (consp (cdr e)) (proper-list-p (cadr e))) (cp-bail "bad letrec"))
		       (let ((all-lambdas (every (lambda (b) (and (consp b) (consp (cdr b)) (cp-lambda-p (cadr b))))
						 (cadr e))))
			 (dolist (b (cadr e))
			   (unless (and (consp b) (consp (cdr b)) (null (cddr b))) (cp-bail "bad binding"))
			   (bind (car b) :letrec all-lambdas))
			 (dolist (b (cadr e)) (walk (cadr b)))
			 (body (cddr e))))))
		   ((and (symbolp (car e)) (car e) (eq (symbol-package (car e)) (find-package "SCHEME"))
			 (not (gethash (car e) *vi*)) (not (host-variable-name-p (car e))))
		    ;; a keyword this pass doesn't know (a translator macro)
		    (cp-bail (car e)))
		   (t (mapc #'walk e))))
	   (pending-sets-fixup ()
	     (dolist (v pending-sets)
	       (let ((info (gethash v *vi*)))
		 (when info (incf (vi-sets info)))))))
    (declare (special pending-sets))
    (let ((pending-sets '()))
      (declare (special pending-sets))
      (walk form)
      (pending-sets-fixup))))

(defun assigned-p (x) (let ((i (vi x))) (and i (> (vi-sets i) 0))))

(defun unique-binders-p ()
  (loop for info being the hash-values of *vi* always (= (vi-binds info) 1)))

;;; ------------------------------------------------------------------
;;; Primitives

(defun cp-primitive-p (x)
  "Whether X is a reference to a host primitive: a symbol the form doesn't
bind that names one."
  (and (symbolp x) x (not (vi x)) (primitive-symbol-p x)))

(defun primitive-call-name (e)
  "The name (a string) of the primitive E calls, or NIL."
  (and (consp e) (cp-primitive-p (car e)) (qs:scheme-symbol-name (car e))))

(defparameter *foldable-primitives*
  '("+" "-" "*" "/" "=" "<" ">" "<=" ">=" "zero?" "positive?" "negative?" "odd?" "even?"
    "quotient" "remainder" "modulo" "max" "min" "abs" "not" "null?" "pair?" "eq?" "eqv?"
    "number?" "integer?" "rational?" "real?" "complex?" "exact?" "inexact?" "exact-integer?"
    "exact->inexact" "inexact->exact" "exact" "inexact" "char->integer" "integer->char"
    "char?" "boolean?" "symbol?" "string?" "vector?" "procedure?" "fixnum?" "flonum?"
    "char=?" "char<?" "char>?" "char<=?" "char>=?" "char-upcase" "char-downcase"
    "floor" "ceiling" "round" "truncate" "gcd" "lcm" "square" "nan?" "finite?" "infinite?"
    "fx+" "fx-" "fx*" "fx=?" "fx<?" "fx>?" "fx<=?" "fx>=?" "fxzero?"
    "fl+" "fl-" "fl*" "fl/" "fl=?" "fl<?" "fl>?" "fl<=?" "fl>=?" "boolean=?")
  "Primitives with no effect whose result depends only on their arguments:
calls of them on constants are folded.")

(defparameter *type-predicates*
  '("null?" "pair?" "number?" "integer?" "rational?" "real?" "complex?" "exact-integer?"
    "char?" "boolean?" "symbol?" "string?" "vector?" "procedure?" "fixnum?" "flonum?" "not")
  "Predicates that take any object: folded on any constant.")

(defparameter *pure-primitives*
  '("not" "null?" "pair?" "eq?" "eqv?" "number?" "integer?" "rational?" "real?" "complex?"
    "exact-integer?" "char?" "boolean?" "symbol?" "string?" "vector?" "procedure?" "fixnum?"
    "flonum?" "cons" "list" "vector" "values" "void")
  "Primitives that can't raise an error or have an effect, whatever their
arguments: a call of one whose value isn't used is dropped (its
arguments' effects kept).")

(defvar *primitive-sets* nil)

(defun primitive-in-p (name set)
  (unless *primitive-sets*
    (setq *primitive-sets* (make-hash-table :test 'eq))
    (loop for (key list) in `((:fold ,*foldable-primitives*) (:type ,*type-predicates*)
			      (:pure ,*pure-primitives*))
	  do (let ((h (make-hash-table :test 'equal)))
	       (dolist (n list) (setf (gethash n h) t))
	       (setf (gethash key *primitive-sets*) h))))
  (and name (gethash name (gethash set *primitive-sets*))))

(defun foldable-datum-p (x)
  "Whether X can be a folded result: an atom of fixed identity."
  (typecase x
    (fixnum t)
    (integer (<= (integer-length x) 128))
    (ratio (and (<= (integer-length (numerator x)) 128) (<= (integer-length (denominator x)) 128)))
    (double-float (and (not (sb-ext:float-infinity-p x)) (not (sb-ext:float-nan-p x))))
    (character t)
    (t (or (eq x t) (eq x qs:false) (null x)))))

(defun eq-comparable-datum-p (x)
  (or (typep x 'fixnum) (characterp x) (eq x t) (eq x qs:false) (null x)
      (and (symbolp x) (qs:scheme-symbol-p x))))

(defun try-fold (name args)
  "The constant form of NAME's call on the constant forms ARGS, or NIL."
  (let ((values (mapcar (lambda (a) (nth-value 1 (constant-p a))) args)))
    (when (and (primitive-in-p name :fold)
	       (if (primitive-in-p name :type)
		   t
		   (every (lambda (v) (or (numberp v) (characterp v) (eq v t) (eq v qs:false)
					  (and (symbolp v) (qs:scheme-symbol-p v))))
			  values))
	       (or (not (member name '("eq?" "eqv?") :test #'string=))
		   (every #'eq-comparable-datum-p values))
	       (every (lambda (v) (or (not (integerp v)) (<= (integer-length v) 128))) values))
      (let ((f (ignore-errors (host-ref name))))
	(when (functionp f)
	  (multiple-value-bind (result ok)
	      (handler-case (values (apply f values) t)
		(error () (values nil nil)))
	    (when (and ok (foldable-datum-p result))
	      (incf (fourth *cp0-stats*))
	      (cp-quote result))))))))

(defun pure-p (e)
  "Whether evaluating E can't raise an error or have an effect (or fail to
terminate)."
  (cond ((symbolp e)
	 (if (vi e)
	     (let ((i (vi e))) (or (eq (vi-kind i) :param) (vi-lambda-letrec i)))
	     (cp-primitive-p e)))
	((atom e) t)
	((quote-p e) t)
	((member (cp-keyword (car e)) '(:lambda :case-lambda :primitive)) t)
	((cp-keyword (car e)) nil)
	(t (and (primitive-in-p (primitive-call-name e) :pure)
		(every #'pure-p (cdr e))))))

;;; ------------------------------------------------------------------
;;; The walk

(defstruct (cpb (:constructor make-cpb (name &optional kind value)))
  name		; the variable in the output (renamed in a copy)
  kind		; NIL, :const or :copy (VALUE replaces it), :lambda (VALUE is its source)
  value)

(defvar *env*)				; variable -> CPB, scoped dynamically
(defvar *copying* nil)			; true in an inlined copy: binders renamed
(defvar *inline-stack* '())		; lambdas being inlined or walked
(defvar *inline-depth* 0)		; inlined calls being optimized
(defvar *effort*)			; inlines left for this form
(defvar *cp-counter* 0)

(defparameter *inline-size-limit* 60
  "The biggest lambda, in conses, inlined at a call.")

(defparameter *inline-result-limit* 200
  "The biggest an inlined call may become, in conses, after optimization.")

(defparameter *inline-depth-limit* 4)
(defparameter *inline-effort* 2000 "Inlines per top-level form.")

(defun fresh-variable (v)
  (let ((new (make-lexical (format nil "~A_~D" (if (symbol-package v)
						   (qs:scheme-symbol-name v)
						   (symbol-name v))
				   (incf *cp-counter*)))))
    (setf (gethash new *vi*) (gethash v *vi*))
    new))

(defmacro with-cp-bindings ((vars cpbs) &body body)
  "Run BODY with each of VARS bound in *ENV* to the CPB in the same place
of CPBS."
  (let ((saved (gensym)) (v (gensym)) (b (gensym)))
    `(let ((,saved (loop for ,v in ,vars for ,b in ,cpbs
			 collect (gethash ,v *env*)
			 do (setf (gethash ,v *env*) ,b))))
       (multiple-value-prog1 (progn ,@body)
	 (loop for ,v in ,vars for ,b in ,saved
	       do (if ,b (setf (gethash ,v *env*) ,b) (remhash ,v *env*)))))))

(defun binder-name (v) (if *copying* (fresh-variable v) v))

(defun rename-formals (formals names)
  "FORMALS with its variables replaced by NAMES, in order."
  (cond ((null formals) nil)
	((symbolp formals) (car names))
	(t (cons (car names) (rename-formals (cdr formals) (cdr names))))))

(defun cp (e ctx)
  "E optimized, in context CTX: :value, :test or :effect.  In :effect,
NIL if nothing of E need remain."
  (cond ((symbolp e) (cp-ref e ctx))
	((atom e) (if (eq ctx :effect) nil e))
	(t (case (and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
	     (:quote (if (and (eq ctx :effect) (not (name-marker-p e))) nil e))
	     (:primitive (if (eq ctx :effect) nil e))
	     (:if (cp-if e ctx))
	     (:begin (cp-sequence-form (cdr e) ctx))
	     (:set! (list (car e) (cp-variable-name (cadr e)) (cp (caddr e) :value)))
	     (:define (list (car e) (cadr e) (cp (caddr e) :value)))
	     (:lambda (if (eq ctx :effect) nil (cp-lambda e)))
	     (:case-lambda (if (eq ctx :effect) nil (cp-case-lambda e)))
	     ((:letrec :letrec*) (cp-letrec e ctx))
	     (t (cp-call e ctx))))))

(defun cp-variable-name (x)
  (let ((b (gethash x *env*)))
    (if b (cpb-name b) x)))

(defun cp-ref (x ctx)
  (let ((b (gethash x *env*)))
    (cond ((null b)
	   ;; free: a primitive, or a top-level variable (which may be unbound)
	   (if (and (eq ctx :effect) (cp-primitive-p x)) nil x))
	  ((member (cpb-kind b) '(:const :copy))
	   (if (eq ctx :effect) nil (cpb-value b)))
	  ((and (eq ctx :effect) (pure-p x)) nil)
	  (t (cpb-name b)))))

(defun cp-sequence (forms ctx)
  "FORMS, a body, optimized: a list of forms, empty only in :effect."
  (let ((out '()))
    (loop for (e . rest) on forms
	  do (let ((x (cp e (if rest :effect ctx))))
	       (cond ((null x))
		     ((and (consp x) (eq (cp-keyword (car x)) :begin))
		      (dolist (y (cdr x)) (push y out)))
		     (t (push x out)))))
    (when (and (null out) (not (eq ctx :effect)))
      (push (cp-void) out))
    ;; constants and markers that aren't last are kept only if markers
    (let ((l (nreverse out)))
      (if (cdr l)
	  (append (remove-if (lambda (x) (and (constant-p x) (not (name-marker-p x)))) (butlast l))
		  (last l))
	  l))))

(defun make-sequence-form (forms)
  (cond ((null forms) nil)
	((null (cdr forms)) (car forms))
	(t (cons (sym "begin") forms))))

(defun cp-sequence-form (forms ctx)
  (make-sequence-form (cp-sequence forms ctx)))

(defun cp-if (e ctx)
  (destructuring-bind (test then &optional (else nil else-p)) (cdr e)
    (let ((test (cp test :test)))
      ;; (if (not x) a b) => (if x b a)
      (loop while (and else-p (equal (primitive-call-name test) "not") (= (length test) 2))
	    do (setq test (cadr test))
	       (rotatef then else))
      (multiple-value-bind (known value) (constant-p test)
	(cond (known
	       (if (scheme-true-p value)
		   (cp then ctx)
		   (if else-p (cp else ctx) (if (eq ctx :effect) nil (cp-void)))))
	      ((cp-lambda-p test) (cp then ctx))
	      (t
	       (let ((then (cp then ctx))
		     (else (and else-p (cp else ctx))))
		 (if (and (eq ctx :effect) (null then) (null else))
		     (cp-effect-of test)
		     `(,(car e) ,test ,(or then (cp-void))
			,@(when (or else-p (eq ctx :effect)) (list (or else (cp-void)))))))))))))

(defun cp-effect-of (e)
  "Already-optimized E for its effect only, or NIL."
  (cond ((pure-p e) nil)
	((and (primitive-in-p (primitive-call-name e) :pure))
	 (make-sequence-form (remove nil (mapcar #'cp-effect-of (cdr e)))))
	(t e)))

(defun cp-body (body)
  "A lambda's BODY (its list of forms) optimized, keeping a leading name
marker first."
  (cond ((and (null (cdr body)) (consp (car body)) (eq (cp-keyword (car (car body))) :begin)
	      (consp (cdr (car body))) (name-marker-p (cadr (car body))))
	 (list (list* (car (car body)) (cadr (car body)) (cp-sequence (cddr (car body)) :value))))
	((name-marker-p (car body))
	 (list (list* (sym "begin") (car body) (cp-sequence (cdr body) :value))))
	(t (list (make-sequence-form (cp-sequence body :value))))))

(defun cp-lambda (e)
  (let* ((vars (formals-list (cadr e)))
	 (names (mapcar #'binder-name vars)))
    (with-cp-bindings (vars (mapcar #'make-cpb names))
      (list* (car e) (rename-formals (cadr e) names) (cp-body (cddr e))))))

(defun cp-case-lambda (e)
  (cons (car e)
	(mapcar (lambda (clause)
		  (let* ((vars (formals-list (car clause)))
			 (names (mapcar #'binder-name vars)))
		    (with-cp-bindings (vars (mapcar #'make-cpb names))
		      (cons (rename-formals (car clause) names) (cp-body (cdr clause))))))
		(cdr e))))

(defun cp-letrec (e ctx)
  (let* ((bindings (cadr e))
	 (vars (mapcar #'car bindings))
	 (names (mapcar #'binder-name vars))
	 (cpbs (mapcar (lambda (v name b)
			 ;; not one that calls itself: that would unroll it
			 (if (and (vi-lambda-letrec (vi v)) (not (assigned-p v))
				  (not (occurs-in-p v (cadr b))))
			     (make-cpb name :lambda (cadr b))
			     (make-cpb name)))
		       vars names bindings)))
    (with-cp-bindings (vars cpbs)
      (let ((inits (mapcar (lambda (b)
			     (let ((*inline-stack* (cons (cadr b) *inline-stack*)))
			       (cp (cadr b) :value)))
			   bindings))
	    (body (cp-sequence (cddr e) ctx)))
	(if (and (null body) (every #'pure-p inits))
	    nil
	    (list (car e) (mapcar #'list names inits) (or (make-sequence-form body) (cp-void))))))))

(defun occurs-in-p (v e)
  (cond ((eq e v) t)
	((atom e) nil)
	((quote-p e) nil)
	(t (loop for x on e
		 thereis (or (occurs-in-p v (car x)) (and (atom (cdr x)) (eq (cdr x) v)))))))

(defun copyable-p (x)
  "Whether X, an optimized expression, can replace a variable bound to it:
a constant, a primitive, or an unassigned variable whose reference can't
raise an error."
  (or (constant-p x)
      (cp-primitive-p x)
      (and (symbolp x) (vi x) (not (assigned-p x)) (pure-p x))))

(defun cp-bind (formals body args sources ctx)
  "((lambda FORMALS . BODY) . ARGS) optimized, ARGS already optimized
(SOURCES their unoptimized forms, NIL for none)."
  (let* ((vars (formals-list formals))
	 (names (mapcar #'binder-name vars))
	 (cpbs (loop for v in vars for name in names for a in args for s in sources
		     collect (cond ((assigned-p v) (make-cpb name))
				   ((constant-p a) (make-cpb name :const a))
				   ((copyable-p a) (make-cpb name :copy a))
				   ((cp-lambda-p s) (make-cpb name :lambda s))
				   (t (make-cpb name))))))
    (with-cp-bindings (vars cpbs)
      (let* ((body (cp-sequence body ctx))
	     (kept (loop for b in cpbs for a in args
			 unless (member (cpb-kind b) '(:const :copy)) collect (cons (cpb-name b) a))))
	(cond ((and (null kept) (null body)) nil)
	      ((null kept) (make-sequence-form body))
	      (t (list* (list (sym "lambda") (mapcar #'car kept) (or (make-sequence-form body) (cp-void)))
			(mapcar #'cdr kept))))))))

(defun clause-for (lambda nargs)
  "The formals and body of LAMBDA (a lambda or case-lambda source) for a call
with NARGS arguments, if it takes exactly NARGS fixed arguments."
  (flet ((fixed (formals) (and (proper-list-p formals) (= (length formals) nargs))))
    (if (cp-lambda-p lambda)
	(and (fixed (cadr lambda)) (values (cadr lambda) (cddr lambda) t))
	(let ((clause (find-if (lambda (c)
				 (let ((f (car c)))
				   (if (proper-list-p f) (= (length f) nargs) (<= (length (formals-list f)) (1+ nargs)))))
			       (cdr lambda))))
	  (and clause (fixed (car clause)) (values (car clause) (cdr clause) t))))))

(defun strip-name-marker (body)
  (cond ((and (null (cdr body)) (consp (car body)) (eq (cp-keyword (car (car body))) :begin)
	      (consp (cdr (car body))) (name-marker-p (cadr (car body))))
	 (or (cddr (car body)) (list (cp-void))))
	((and (name-marker-p (car body)) (cdr body)) (cdr body))
	(t body)))

(defun cp-call (e ctx)
  (let ((op (car e)) (sources (cdr e)))
    (cond
      ;; ((lambda (v ...) body) arg ...): a let
      ((and (cp-lambda-p op) (proper-list-p (cadr op)) (= (length (cadr op)) (length sources)))
       (cp-bind (cadr op) (cddr op) (mapcar (lambda (a) (cp a :value)) sources) sources ctx))
      (t
       (let* ((b (and (symbolp op) (gethash op *env*)))
	      (args (mapcar (lambda (a) (cp a :value)) sources)))
	 (or (and b (eq (cpb-kind b) :lambda) (cp-inline b args sources ctx))
	     (cp-primitive-call (cp op :value) args ctx)))))))

(defun cp-inline (b args sources ctx)
  "The call of B's procedure on ARGS inlined, or NIL."
  (let ((lambda (cpb-value b)))
    (when (and (> *effort* 0)
	       (< *inline-depth* *inline-depth-limit*)
	       (not (member lambda *inline-stack* :test #'eq))
	       (<= (tree-size lambda) *inline-size-limit*))
      (multiple-value-bind (formals body ok) (clause-for lambda (length args))
	(when ok
	  (decf *effort*)
	  (let ((result (let ((*copying* t)
			      (*inline-depth* (1+ *inline-depth*))
			      (*inline-stack* (cons lambda *inline-stack*)))
			  (cp-bind formals (strip-name-marker body) args sources ctx))))
	    (when (<= (tree-size result) *inline-result-limit*)
	      (incf (third *cp0-stats*))
	      (or result (if (eq ctx :effect) nil (cp-void))))))))))

(defun cp-primitive-call (op args ctx)
  "The call (OP . ARGS), its parts optimized."
  (let ((name (and (cp-primitive-p op) (qs:scheme-symbol-name op))))
    (or (and name (every #'constant-p args) (try-fold name args))
	(and name (string= name "values") (= (length args) 1) (not (eq ctx :effect)) (car args))
	(and name (string= name "call-with-values") (= (length args) 2)
	     (cp-call-with-values (first args) (second args)))
	(if (and (eq ctx :effect) name (primitive-in-p name :pure))
	    (make-sequence-form (remove nil (mapcar #'cp-effect-of args)))
	    (cons op args)))))

(defun cp-call-with-values (producer consumer)
  "(call-with-values (lambda () (values a ...)) (lambda (v ...) body)),
optimized: ((lambda (v ...) body) a ...); or NIL."
  (when (and (cp-lambda-p producer) (null (cadr producer)) (null (cdddr producer))
	     (cp-lambda-p consumer) (proper-list-p (cadr consumer)))
    (let ((body (caddr producer)))
      (cond ((and (equal (primitive-call-name body) "values")
		  (= (length (cdr body)) (length (cadr consumer))))
	     (cons consumer (cdr body)))
	    ;; one value, from an expression that returns one
	    ((and (= (length (cadr consumer)) 1)
		  (or (constant-p body) (and (symbolp body) (vi body))))
	     (list consumer body))))))

;;; ------------------------------------------------------------------
;;; Dead bindings

(defun count-references (form)
  "A table of each variable FORM refers to (or assigns) to its count."
  (let ((counts (make-hash-table :test 'eq)))
    (labels ((walk (e)
	       (cond ((symbolp e) (when e (incf (gethash e counts 0))))
		     ((atom e))
		     ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		      (case (cp-keyword (car e))
			((:quote :primitive))
			((:set! :define) (incf (gethash (cadr e) counts 0)) (walk (caddr e)))
			(:lambda (mapc #'walk (cddr e)))
			(:case-lambda (dolist (c (cdr e)) (mapc #'walk (cdr c))))
			((:letrec :letrec*) (dolist (b (cadr e)) (walk (cadr b))) (mapc #'walk (cddr e)))
			(t (mapc #'walk (cdr e)))))
		     (t (mapc #'walk e)))))
      (walk form))
    counts))

(defun drop-dead-bindings (form)
  "FORM without the letrec bindings and let bindings of variables nothing
refers to whose inits are pure.  Second value true if anything changed."
  (let ((counts (count-references form)) (changed nil))
    (labels ((dead-p (v init)
	       (and (not (assigned-p v)) (pure-p init)
		    (= (gethash v counts 0) (gethash v (count-references init) 0))))
	     (walk (e)
	       (cond ((atom e) e)
		     ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		      (case (cp-keyword (car e))
			((:quote :primitive) e)
			((:set! :define) (list (car e) (cadr e) (walk (caddr e))))
			(:lambda (list* (car e) (cadr e) (mapcar #'walk (cddr e))))
			(:case-lambda (cons (car e) (mapcar (lambda (c) (cons (car c) (mapcar #'walk (cdr c)))) (cdr e))))
			((:letrec :letrec*)
			 (let ((live (remove-if (lambda (b) (dead-p (car b) (cadr b))) (cadr e))))
			   (unless (= (length live) (length (cadr e))) (setq changed t))
			   (let ((body (mapcar #'walk (cddr e))))
			     (if live
				 (list* (car e) (mapcar (lambda (b) (list (car b) (walk (cadr b)))) live) body)
				 (make-sequence-form body)))))
			(t (cons (car e) (mapcar #'walk (cdr e))))))
		     ((and (cp-lambda-p (car e)) (proper-list-p (cadr (car e)))
			   (= (length (cadr (car e))) (length (cdr e))))
		      (let* ((pairs (mapcar #'cons (cadr (car e)) (cdr e)))
			     (live (remove-if (lambda (p) (and (= (gethash (car p) counts 0) 0)
							       (not (assigned-p (car p)))
							       (pure-p (cdr p))))
					      pairs))
			     (body (mapcar #'walk (cddr (car e)))))
			(unless (= (length live) (length pairs)) (setq changed t))
			(if live
			    (cons (list* (car (car e)) (mapcar #'car live) body)
				  (mapcar (lambda (p) (walk (cdr p))) live))
			    (make-sequence-form body))))
		     (t (mapcar #'walk e)))))
      (values (walk form) changed))))

;;; ------------------------------------------------------------------
;;; The entry point

(defun optimize-core (form)
  "FORM, a core form ready for the translator (after OPEN-PRIMITIVES),
optimized; FORM itself if optimization is off or FORM has something this
pass doesn't know."
  (let ((level (optimize-level)))
    (if (or (= level 0) (atom form))
	form
	(let ((*vi* (make-hash-table :test 'eq)))
	  (let ((result
		  (catch 'cp-bail
		    (analyze form)
		    (if (not (unique-binders-p))
			(cp-bail "names not unique")
			(let* ((*env* (make-hash-table :test 'eq))
			       (*effort* *inline-effort*)
			       (*inline-stack* '())
			       (out (or (cp form :value) (cp-void))))
			  (loop repeat 3
				do (multiple-value-bind (new changed) (drop-dead-bindings out)
				     (setq out new)
				     (unless changed (return))))
			  (when (>= level 2)
			    (setq out (cpdx (cptypes out))))
			  (incf (first *cp0-stats*))
			  (list :ok out))))))
	    (when (or *cp0-trace* (uiop:getenv "QUASISCHEME_OPTIMIZE_TRACE"))
	      (let ((*print-right-margin* 110) (*package* (find-package "SCHEME")))
		(format *trace-output* "~&;; cp0 in:~%~S~%;; cp0 out:~%~S~%" form result)))
	    (if (and (consp result) (eq (car result) :ok))
		(cadr result)
		(progn (incf (second *cp0-stats*))
		       (pushnew (princ-to-string result) (fifth *cp0-stats*) :test #'string=)
		       form)))))))
