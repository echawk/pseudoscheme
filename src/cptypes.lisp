; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-PSYNTAX -*-

;;;; cptypes: type recovery on core forms (docs/optimizations.md, "Plan
;;;; (2026-10-18)", item 2), after cp0 (src/cp0.lisp), as Chez Scheme's
;;;; cptypes runs after its cp0.
;;;;
;;;; A type is a set of kinds of object, as bits (FIX, BIG, RAT, FLO, ...).
;;;; Facts come from
;;;;
;;;;  - constants, and the result types of primitives ((fl+ a b) is a
;;;;    flonum, (vector-length v) a fixnum, (+ x 1.0) a flonum when x is
;;;;    real);
;;;;  - predicate tests: in (if (pair? x) A B), x is a pair in A and isn't
;;;;    in B; x is not #f where (if x A B) takes A;
;;;;  - calls that returned: after (car x), x is a pair; after (< x y), x
;;;;    and y are reals (they raise an error otherwise);
;;;;  - the call sites of known procedures: a variable bound once and for
;;;;    all to a lambda (as cp0 finds them) whose every reference is a call
;;;;    with the arguments it takes has as each parameter's type the union
;;;;    of what its calls pass, found by iterating to a fixed point from
;;;;    the empty type.  A known procedure that escapes (its value stored:
;;;;    a program's or a library's definitions are also assigned to their
;;;;    top-level locations) is first cloned: the calls go to the copy,
;;;;    which doesn't escape, and the original keeps its other uses.  A
;;;;    clone none of whose types are of use is dropped again.
;;;;
;;;; What is found is emitted for SBCL, which can't see through the
;;;; generic calls: a parameter of a known procedure with a useful type is
;;;; rebound as (%cp-the-flonum p), which is CL's (the double-float p), so
;;;; SBCL drops the generic arithmetic's dispatch on it (src/numbers.lisp);
;;;; a variable an operation's argument whose type a test or an earlier
;;;; call narrowed is wrapped as (%cp-assume-pair x), CL's TRULY-THE.
;;;;
;;;; Soundness: only unassigned variables have types; every operation's
;;;; result type is what this implementation's primitive returns (CL's
;;;; float contagion, for instance: (* 0 1.5) is 0.0 here); a known
;;;; procedure's parameters are typed only from calls that are all known.
;;;; A continuation re-entered restores the same values, so a type that
;;;; held still holds.

(in-package "QUASISCHEME-PSYNTAX")

;;; ------------------------------------------------------------------
;;; Types

(defconstant +fix+ 1)
(defconstant +big+ 2)
(defconstant +rat+ 4)
(defconstant +flo+ 8)
(defconstant +oreal+ 16)		; other reals (single floats from Lisp)
(defconstant +cpx+ 32)
(defconstant +pair+ 64)
(defconstant +null+ 128)
(defconstant +true+ 256)
(defconstant +false+ 512)
(defconstant +sym+ 1024)
(defconstant +char+ 2048)
(defconstant +proc+ 4096)
(defconstant +other+ 8192)
(defconstant +all+ 16383)
(defconstant +int+ (logior +fix+ +big+))
(defconstant +exact+ (logior +int+ +rat+))
(defconstant +real+ (logior +exact+ +flo+ +oreal+))
(defconstant +num+ (logior +real+ +cpx+))
(defconstant +bool+ (logior +true+ +false+))

(defun subtype-p (a b) (zerop (logandc2 a b)))

(defun datum-type (x)
  (typecase x
    (fixnum +fix+)
    (integer +big+)
    (ratio +rat+)
    (double-float +flo+)
    (real +oreal+)
    (number +cpx+)
    (cons +pair+)
    (null +null+)
    (character +char+)
    (function +proc+)
    (t (cond ((eq x t) +true+)
	     ((eq x qs:false) +false+)
	     ((and (symbolp x) (qs:scheme-symbol-p x)) +sym+)
	     (t +other+)))))

;;; (name bits exact): (name x) is true exactly when x is of BITS (EXACT),
;;; or at least only then.
(defparameter *predicate-types*
  `(("pair?" ,+pair+ t) ("null?" ,+null+ t) ("fixnum?" ,+fix+ t) ("flonum?" ,+flo+ t)
    ("exact-integer?" ,+int+ t) ("char?" ,+char+ t) ("boolean?" ,+bool+ t)
    ("number?" ,+num+ t) ("symbol?" ,+sym+ nil) ("procedure?" ,+proc+ nil)))

;;; ------------------------------------------------------------------
;;; Result types of primitives

(defun arith-type (ts)
  (cond ((some #'zerop ts) 0)
	((every (lambda (b) (subtype-p b +int+)) ts) +int+)
	((every (lambda (b) (subtype-p b +exact+)) ts) +exact+)
	((and (every (lambda (b) (subtype-p b +real+)) ts)
	      (some (lambda (b) (subtype-p b +flo+)) ts))
	 +flo+)
	((every (lambda (b) (subtype-p b +real+)) ts) +real+)
	(t +num+)))

(defun division-type (ts)
  (cond ((some #'zerop ts) 0)
	((every (lambda (b) (subtype-p b +exact+)) ts) +exact+)
	((and (every (lambda (b) (subtype-p b +real+)) ts)
	      (some (lambda (b) (subtype-p b +flo+)) ts))
	 +flo+)
	((every (lambda (b) (subtype-p b +real+)) ts) +real+)
	(t +num+)))

(defun unary-type (ts rules default)
  "The first of RULES ((arg-bits result-bits) ...) whose ARG-BITS the one
argument type is within, else DEFAULT."
  (let ((b (car ts)))
    (cond ((null ts) default)
	  ((zerop b) 0)
	  (t (or (loop for (arg result) in rules when (subtype-p b arg) return result) default)))))

(defparameter *result-types* (make-hash-table :test 'equal))

(macrolet ((def (names (ts) &body body)
	     `(dolist (n ',(if (listp names) names (list names)))
		(setf (gethash n *result-types*) (lambda (,ts) (declare (ignorable ,ts)) ,@body)))))
  (def ("+" "-" "*") (ts) (if ts (arith-type ts) +fix+))
  (def "square" (ts) (arith-type (append ts ts)))
  (def "/" (ts) (division-type ts))
  (def ("inexact" "exact->inexact") (ts) (unary-type ts `((,+real+ ,+flo+)) (logior +flo+ +cpx+)))
  (def ("exact" "inexact->exact") (ts) (unary-type ts `((,+real+ ,+exact+)) +num+))
  (def "abs" (ts) (unary-type ts `((,+int+ ,+int+) (,+flo+ ,+flo+) (,+exact+ ,+exact+) (,+real+ ,+real+)) +num+))
  (def ("floor" "ceiling" "round" "truncate") (ts)
    (unary-type ts `((,+exact+ ,+int+) (,+flo+ ,+flo+) (,+real+ ,+real+)) +num+))
  (def ("max" "min") (ts)
    (cond ((some #'zerop ts) 0)
	  ((every (lambda (b) (subtype-p b +fix+)) ts) +fix+)
	  ((every (lambda (b) (subtype-p b +int+)) ts) +int+)
	  ((every (lambda (b) (subtype-p b +real+)) ts) +real+)
	  (t +num+)))
  (def ("quotient" "remainder" "modulo") (ts)
    (if (every (lambda (b) (subtype-p b +int+)) ts) +int+ +num+))
  (def ("length" "vector-length" "string-length" "bytevector-length" "char->integer"
	"fx+" "fx-" "fx*" "fxquotient" "fxremainder" "fxmodulo" "fxand" "fxior" "fxxor" "fxnot"
	"fxabs" "fxmax" "fxmin" "fxdiv" "fxmod" "fxarithmetic-shift" "fxarithmetic-shift-left"
	"fxarithmetic-shift-right" "bytevector-u8-ref")
      (ts) +fix+)
  (def ("fl+" "fl-" "fl*" "fl/" "flabs" "flmax" "flmin" "flfloor" "flceiling" "flround"
	"fltruncate" "fixnum->flonum")
      (ts) +flo+)
  (def ("cons" "cons*") (ts) +pair+)
  (def "list" (ts) (if ts +pair+ +null+))
  (def ("not" "eq?" "eqv?" "equal?" "null?" "pair?" "number?" "integer?" "rational?" "real?"
	"complex?" "exact?" "inexact?" "exact-integer?" "char?" "boolean?" "symbol?" "string?"
	"vector?" "procedure?" "fixnum?" "flonum?" "zero?" "positive?" "negative?" "odd?" "even?"
	"=" "<" ">" "<=" ">=" "fx=?" "fx<?" "fx>?" "fx<=?" "fx>=?" "fl=?" "fl<?" "fl>?" "fl<=?" "fl>=?"
	"char=?" "char<?" "char>?" "char<=?" "char>=?" "list?" "vector?" "string=?")
      (ts) +bool+)
  (def ("integer->char" "char-upcase" "char-downcase" "string-ref") (ts) +char+))

(defun primitive-result-type (name ts)
  (let ((f (gethash name *result-types*)))
    (if f (funcall f ts) +all+)))

;;; What a call that returned says of its arguments: (name . bits), each
;;; argument (the first, with :FIRST) of BITS.
(defparameter *argument-facts*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (entry `((("car" "cdr" "set-car!" "set-cdr!") :first ,+pair+)
		     (("+" "-" "*" "/" "=" "zero?") :all ,+num+)
		     (("<" ">" "<=" ">=" "positive?" "negative?" "floor" "ceiling" "round" "truncate")
		      :all ,+real+)
		     (("fl+" "fl-" "fl*" "fl/" "fl=?" "fl<?" "fl>?" "fl<=?" "fl>=?" "flabs") :all ,+flo+)
		     (("fx+" "fx-" "fx*" "fx=?" "fx<?" "fx>?" "fx<=?" "fx>=?") :all ,+fix+)))
      (dolist (n (first entry)) (setf (gethash n h) (rest entry))))
    h))

;;; ------------------------------------------------------------------
;;; The state of an analysis

(defvar *known*)	; variable -> its lambda, for known procedures that don't escape
(defvar *ptypes*)	; parameter of a known procedure -> type (this iteration's)
(defvar *ptypes-next*)	; ... joined from the calls seen this iteration
(defvar *vtypes*)	; let-bound unassigned variable -> its init's type
(defvar *facts*)	; variable -> narrowed type, scoped dynamically
(defvar *emitting* nil)	; true in the final walk, which rewrites

(defun base-type (v)
  (cond ((not (vi v)) +all+)
	((assigned-p v) +all+)
	((nth-value 1 (gethash v *ptypes*)) (gethash v *ptypes*))
	((nth-value 1 (gethash v *vtypes*)) (gethash v *vtypes*))
	(t +all+)))

(defun var-type (v)
  (multiple-value-bind (b found) (gethash v *facts*)
    (if found b (base-type v))))

(defmacro with-facts ((facts) &body body)
  "Run BODY with FACTS ((var . bits) ...) narrowing the variables' types."
  (let ((saved (gensym)) (f (gensym)))
    `(let ((,saved (loop for ,f in ,facts
			 collect (multiple-value-list (gethash (car ,f) *facts*))
			 do (setf (gethash (car ,f) *facts*) (logand (var-type (car ,f)) (cdr ,f))))))
       (multiple-value-prog1 (progn ,@body)
	 (loop for ,f in (reverse ,facts) for (old found) in (reverse ,saved)
	       do (if found (setf (gethash (car ,f) *facts*) old) (remhash (car ,f) *facts*)))))))

(defun typed-variable-p (v) (and (symbolp v) (vi v) (not (assigned-p v))))

(defun test-facts (e)
  "Facts that hold where test E is true, and where it is false."
  (cond ((typed-variable-p e)
	 (values (list (cons e (logandc2 +all+ +false+))) (list (cons e +false+))))
	((and (consp e) (equal (primitive-call-name e) "not") (= (length e) 2))
	 (multiple-value-bind (yes no) (test-facts (cadr e)) (values no yes)))
	((and (consp e) (= (length e) 2) (typed-variable-p (cadr e)) (primitive-call-name e))
	 (let ((entry (assoc (primitive-call-name e) *predicate-types* :test #'string=)))
	   (if entry
	       (values (list (cons (cadr e) (second entry)))
		       (and (third entry) (list (cons (cadr e) (logandc2 +all+ (second entry))))))
	       (values nil nil))))
	(t (values nil nil))))

(defun call-facts (name args)
  "Facts about ARGS once a call of primitive NAME on them has returned."
  (let ((entry (gethash name *argument-facts*)))
    (when entry
      (loop for a in (if (eq (first entry) :first) (list (car args)) args)
	    when (typed-variable-p a) collect (cons a (second entry))))))

;;; ------------------------------------------------------------------
;;; The walk: an expression's type, and (when *EMITTING*) its rewriting

(defvar *after-facts* '()
  "Facts gathered from calls that have returned, for what follows them.")

(defun ty (e)
  "E's type, and E rewritten (when *EMITTING*)."
  (cond ((symbolp e)
	 (values (cond ((typed-variable-p e) (var-type e))
		       ((cp-primitive-p e) +proc+)
		       (t +all+))
		 e))
	((atom e) (values (datum-type e) e))
	(t (case (and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
	     (:quote (values (datum-type (cadr e)) e))
	     (:primitive (values +all+ e))
	     (:if (ty-if e))
	     (:begin (multiple-value-bind (type forms) (ty-sequence (cdr e))
		       (values type (cons (car e) forms))))
	     ((:set! :define)
	      (values +all+ (list (car e) (cadr e) (nth-value 1 (ty (caddr e))))))
	     (:lambda (values +proc+ (ty-lambda e)))
	     (:case-lambda
	      (values +proc+
		      (cons (car e) (mapcar (lambda (c)
					      (let ((*after-facts* '()))
						(cons (car c) (nth-value 1 (ty-sequence (cdr c))))))
					    (cdr e)))))
	     ((:letrec :letrec*)
	      (let ((inits (mapcar (lambda (b) (nth-value 1 (ty (cadr b)))) (cadr e))))
		(multiple-value-bind (type body) (ty-sequence (cddr e))
		  (values type (list* (car e) (mapcar (lambda (b init) (list (car b) init)) (cadr e) inits)
				      body)))))
	     (t (ty-call e))))))

(defun ty-sequence (forms)
  "The type of the last of FORMS, and FORMS rewritten; the facts of the
calls in each hold for the rest."
  (if (null (cdr forms))
      (multiple-value-bind (type x) (ty (car forms)) (values type (list x)))
      (let ((first nil) (facts nil))
	(let ((*after-facts* '()))
	  (setq first (nth-value 1 (ty (car forms))) facts *after-facts*))
	(setq *after-facts* (append facts *after-facts*))
	(multiple-value-bind (type rest) (with-facts (facts) (ty-sequence (cdr forms)))
	  (values type (cons first rest))))))

(defun ty-if (e)
  (destructuring-bind (test then &optional (else nil else-p)) (cdr e)
    (let (test2 after-test)
      (let ((*after-facts* '()))
	(setq test2 (nth-value 1 (ty test)) after-test *after-facts*))
      ;; what the test's calls say holds after the IF too
      (setq *after-facts* (append after-test *after-facts*))
      (multiple-value-bind (yes no) (test-facts test)
	(with-facts (after-test)
	  (multiple-value-bind (then-type then2) (let ((*after-facts* '())) (with-facts (yes) (ty then)))
	    (multiple-value-bind (else-type else2)
		(if else-p (let ((*after-facts* '())) (with-facts (no) (ty else))) (values +all+ nil))
	      (values (logior then-type else-type)
		      (list* (car e) test2 then2 (if else-p (list else2) '()))))))))))

(defun ty-lambda (e)
  "Lambda E rewritten; its parameters' types are known if it's a known
procedure's (from *PTYPES*) and else unknown."
  (let* ((*after-facts* '())
	 (body (nth-value 1 (ty-sequence (cddr e)))))
    (list* (car e) (cadr e) body)))

(defun ty-call (e)
  (let ((op (car e)) (args (cdr e)))
    (cond
      ;; a let: its variables have their inits' types
      ((and (cp-lambda-p op) (proper-list-p (cadr op)) (= (length (cadr op)) (length args)))
       (let ((types-and-args nil) (facts nil))
	 (let ((*after-facts* '()))
	   (setq types-and-args (mapcar (lambda (a) (multiple-value-list (ty a))) args)
		 facts *after-facts*))
	 (setq *after-facts* (append facts *after-facts*))
	 (loop for v in (cadr op) for (type) in types-and-args
	       unless (assigned-p v) do (setf (gethash v *vtypes*) type))
	 (multiple-value-bind (type body) (with-facts (facts) (ty-sequence (cddr op)))
	   (values type (cons (list* (car op) (cadr op) body) (mapcar #'second types-and-args))))))
      (t
       (let* ((types-and-args (mapcar (lambda (a) (multiple-value-list (ty a))) args))
	      (types (mapcar #'first types-and-args))
	      (args2 (mapcar #'second types-and-args))
	      (name (and (cp-primitive-p op) (qs:scheme-symbol-name op)))
	      (op2 (if (symbolp op) op (nth-value 1 (ty op)))))
	 (when (and (symbolp op) (gethash op *known*))
	   ;; a known procedure's call: its parameters get these types
	   (loop for p in (cadr (gethash op *known*)) for type in types
		 do (setf (gethash p *ptypes-next*) (logior (gethash p *ptypes-next* 0) type))))
	 (when name
	   (setq *after-facts* (append (call-facts name args) *after-facts*)))
	 (values (if name (primitive-result-type name types) +all+)
		 (cond ((and *emitting* name) (cons op2 (emit-argument-types name args2)))
		       ((and *emitting* (symbolp op) (gethash op *known*))
			(cons op2 (emit-call-types (gethash op *known*) args2)))
		       (t (cons op2 args2)))))))))

;;; ------------------------------------------------------------------
;;; Emitting

(defparameter *emitted-types*
  `((,+fix+ "fixnum") (,+flo+ "flonum") (,+pair+ "pair") (,+int+ "integer"))
  "Types worth telling SBCL, by the name of their %cp-the- primitive.")

(defun emitted-type (bits)
  (and (/= bits 0) (second (assoc bits *emitted-types*))))

(defparameter *typed-operations*
  '("car" "cdr" "+" "-" "*" "=" "<" ">" "<=" ">=" "zero?" "positive?" "negative?"
    "quotient" "remainder" "modulo" "abs" "max" "min")
  "Primitives whose arguments are worth wrapping in what a test narrowed.")

(defvar *emit-count*)

(defun emit-argument-types (name args)
  (if (member name *typed-operations* :test #'string=)
      (mapcar (lambda (a)
		(let ((narrowed (and (typed-variable-p a) (nth-value 1 (gethash a *facts*))
				     (emitted-type (gethash a *facts*)))))
		  (if (and narrowed (not (equal narrowed (emitted-type (base-type a)))))
		      (progn (incf *emit-count*)
			     (list (sym (concatenate 'string "%cp-assume-" narrowed)) a))
		      a)))
	      args)
      args))

(defun emit-call-types (lambda args)
  "ARGS of a call of known procedure LAMBDA, each one that's a parameter of
a useful type wrapped to say so: as every call says it, SBCL knows the
parameter's type (and a loop variable's, where the translator makes a
self-call an assignment)."
  (loop for p in (cadr lambda) for a in args
	collect (let ((type (and (not (constant-p a)) (nth-value 1 (gethash p *ptypes*))
				 (emitted-type (gethash p *ptypes*)))))
		  (if type
		      (progn (incf *emit-count*)
			     (list (sym (concatenate 'string "%cp-assume-" type)) a))
		      a))))

(defun emit-parameter-types (lambda)
  "LAMBDA with its parameters that have useful types rebound to themselves
declared so."
  (let ((typed (loop for p in (formals-list (cadr lambda))
		     for type = (and (proper-list-p (cadr lambda)) (not (assigned-p p))
				     (nth-value 1 (gethash p *ptypes*))
				     (emitted-type (gethash p *ptypes*)))
		     when type collect (list p (fresh-variable p) type))))
    (if (null typed)
	lambda
	(let* ((body (cddr lambda))
	       (marker (and (null (cdr body)) (consp (car body)) (eq (cp-keyword (car (car body))) :begin)
			    (name-marker-p (cadr (car body))) (cadr (car body))))
	       (inner (if marker (make-sequence-form (cddr (car body))) (make-sequence-form body)))
	       (rebound `((,(sym "lambda") ,(mapcar #'first typed) ,inner)
			  ,@(mapcar (lambda (x) (list (sym (concatenate 'string "%cp-the-" (third x))) (second x)))
				    typed))))
	  (incf *emit-count* (length typed))
	  (list (car lambda)
		(sublis (mapcar (lambda (x) (cons (first x) (second x))) typed) (cadr lambda))
		(if marker (list (sym "begin") marker rebound) rebound))))))

;;; ------------------------------------------------------------------
;;; Known procedures, and clones of the ones that escape

(defparameter *clone-size-limit* 2000
  "The biggest escaping procedure, in conses, cloned for its calls.")

(defun known-candidates (form)
  "A table of the variables of FORM bound once and for all to a lambda of
fixed arity, to the lambda; and a table of each one's references that
aren't calls with that many arguments."
  (let ((lambdas (make-hash-table :test 'eq))
	(escapes (make-hash-table :test 'eq)))
    (labels ((candidate (v init)
	       (when (and (not (assigned-p v)) (cp-lambda-p init) (proper-list-p (cadr init)))
		 (setf (gethash v lambdas) init)))
	     (walk (e)
	       (cond ((symbolp e) (setf (gethash e escapes) t))
		     ((atom e))
		     ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		      (case (cp-keyword (car e))
			((:quote :primitive))
			((:set! :define) (walk (caddr e)))
			(:lambda (mapc #'walk (cddr e)))
			(:case-lambda (dolist (c (cdr e)) (mapc #'walk (cdr c))))
			((:letrec :letrec*)
			 (when (vi-lambda-letrec (vi (car (car (cadr e)))))
			   (dolist (b (cadr e)) (candidate (car b) (cadr b))))
			 (dolist (b (cadr e)) (walk (cadr b)))
			 (mapc #'walk (cddr e)))
			(t (mapc #'walk (cdr e)))))
		     (t
		      (when (and (cp-lambda-p (car e)) (proper-list-p (cadr (car e)))
				 (= (length (cadr (car e))) (length (cdr e))))
			(loop for v in (cadr (car e)) for a in (cdr e) do (candidate v a)))
		      (if (symbolp (car e))
			  ;; a call: an escape only with the wrong count
			  (let ((l (gethash (car e) lambdas)))
			    (when (and l (/= (length (cadr l)) (length (cdr e))))
			      (setf (gethash (car e) escapes) t)))
			  (walk (car e)))
		      (mapc #'walk (cdr e))))))
      (walk form))
    (values lambdas escapes)))

(defun alpha-copy (e)
  "E with every variable it binds renamed afresh."
  (let ((renames (make-hash-table :test 'eq)))
    (labels ((bind (v) (setf (gethash v renames) (fresh-variable v)))
	     (formals (f) (cond ((null f) nil)
				((symbolp f) (bind f))
				(t (cons (bind (car f)) (formals (cdr f))))))
	     (walk (e)
	       (cond ((symbolp e) (or (gethash e renames) e))
		     ((atom e) e)
		     ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		      (case (cp-keyword (car e))
			((:quote :primitive) e)
			((:set! :define) (list (car e) (walk (cadr e)) (walk (caddr e))))
			(:lambda (let ((f (formals (cadr e)))) (list* (car e) f (mapcar #'walk (cddr e)))))
			(:case-lambda (cons (car e) (mapcar (lambda (c) (let ((f (formals (car c))))
									  (cons f (mapcar #'walk (cdr c)))))
							    (cdr e))))
			((:letrec :letrec*)
			 (let ((names (mapcar (lambda (b) (bind (car b))) (cadr e))))
			   (list* (car e) (mapcar (lambda (n b) (list n (walk (cadr b)))) names (cadr e))
				  (mapcar #'walk (cddr e)))))
			(t (cons (car e) (mapcar #'walk (cdr e))))))
		     (t (mapcar #'walk e)))))
      (walk e))))

(defun add-clones (form lambdas escapes)
  "FORM with each escaping known procedure of a letrec outside any
procedure cloned, the clone's calls made calls of the clone.  Second
value: the clones, (clone . original) each."
  (let ((clones '()) (budget (tree-size form)))
    (labels ((rewrite-letrec (e)
	       (let ((extra '()))
		 (dolist (b (cadr e))
		   (let ((v (car b)))
		     (when (and (gethash v lambdas) (gethash v escapes)
				(<= (tree-size (cadr b)) (min *clone-size-limit* budget)))
		       (decf budget (tree-size (cadr b)))
		       (let ((clone (fresh-variable v)))
			 (push (cons clone v) clones)
			 (push (list clone (alpha-copy (cadr b))) extra)))))
		 (list* (car e) (append (cadr e) (nreverse extra)) (cddr e))))
	     (walk (e)
	       ;; outside procedures: through lets, begins, ifs
	       (cond ((atom e) e)
		     ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		      (case (cp-keyword (car e))
			((:letrec :letrec*)
			 (let ((e (if (vi-lambda-letrec (vi (car (car (cadr e))))) (rewrite-letrec e) e)))
			   (list* (car e) (cadr e) (mapcar #'walk (cddr e)))))
			((:begin :if) (cons (car e) (mapcar #'walk (cdr e))))
			((:set! :define) (list (car e) (cadr e) (walk (caddr e))))
			(t e)))
		     ((cp-lambda-p (car e))
		      (cons (list* (car (car e)) (cadr (car e)) (mapcar #'walk (cddr (car e))))
			    (mapcar #'walk (cdr e))))
		     (t (mapcar #'walk e)))))
      (let ((form (walk form)))
	(if (null clones)
	    (values form '())
	    (let ((redirect (make-hash-table :test 'eq)))
	      (dolist (c clones) (setf (gethash (cdr c) redirect) (car c)))
	      (values (map-calls (lambda (e)
				   (if (and (symbolp (car e)) (gethash (car e) redirect)
					    (= (length (cdr e)) (length (cadr (gethash (car e) lambdas)))))
				       (cons (gethash (car e) redirect) (cdr e))
				       e))
				 form
				 ;; an original calls originals: its arguments
				 ;; are of unknown types, and would spoil the clones'
				 (lambda (v) (gethash v redirect)))
		      clones)))))))

(defun map-calls (f e &optional skip)
  "E with F applied to each call in it (its parts done first), except in
the inits of letrec variables SKIP is true of."
  (labels ((walk (e)
	     (cond ((atom e) e)
		   ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		    (case (cp-keyword (car e))
		      ((:quote :primitive) e)
		      ((:set! :define) (list (car e) (cadr e) (walk (caddr e))))
		      (:lambda (list* (car e) (cadr e) (mapcar #'walk (cddr e))))
		      (:case-lambda (cons (car e) (mapcar (lambda (c) (cons (car c) (mapcar #'walk (cdr c)))) (cdr e))))
		      ((:letrec :letrec*)
		       (list* (car e) (mapcar (lambda (b)
						(list (car b) (if (and skip (funcall skip (car b))) (cadr b) (walk (cadr b)))))
					      (cadr e))
			      (mapcar #'walk (cddr e))))
		      (t (cons (car e) (mapcar #'walk (cdr e))))))
		   (t (funcall f (mapcar #'walk e))))))
    (walk e)))

(defun drop-clones (form drop)
  "FORM without the clones DROP ((clone . original) ...), their calls
made calls of the originals again."
  (let ((back (make-hash-table :test 'eq)))
    (dolist (c drop) (setf (gethash (car c) back) (cdr c)))
    (labels ((walk (e)
	       (cond ((symbolp e) (gethash e back e))
		     ((atom e) e)
		     ((and (symbolp (car e)) (not (vi (car e))) (cp-keyword (car e)))
		      (case (cp-keyword (car e))
			((:quote :primitive) e)
			((:set! :define) (list (car e) (cadr e) (walk (caddr e))))
			(:lambda (list* (car e) (cadr e) (mapcar #'walk (cddr e))))
			(:case-lambda (cons (car e) (mapcar (lambda (c) (cons (car c) (mapcar #'walk (cdr c)))) (cdr e))))
			((:letrec :letrec*)
			 (list* (car e)
				(loop for b in (cadr e) unless (gethash (car b) back)
				      collect (list (car b) (walk (cadr b))))
				(mapcar #'walk (cddr e))))
			(t (cons (car e) (mapcar #'walk (cdr e))))))
		     (t (mapcar #'walk e)))))
      (walk form))))

(defun useful-clone-p (clone lambdas)
  "Whether CLONE's procedure, or a known one inside it, has a parameter
of an emitted type."
  (labels ((useful-lambda-p (l)
	     (some (lambda (p) (and (not (assigned-p p)) (nth-value 1 (gethash p *ptypes*))
				    (emitted-type (gethash p *ptypes*))))
		   (cadr l)))
	   (inside (e)
	     (cond ((atom e) nil)
		   ((quote-p e) nil)
		   ((and (cp-lambda-p e) (gethash e known-lambdas)) (or (useful-lambda-p e) (some #'inside (cddr e))))
		   (t (loop for x on e thereis (and (consp x) (inside (car x))))))))
    (declare (special known-lambdas))
    (let ((known-lambdas (make-hash-table :test 'eq)))
      (declare (special known-lambdas))
      (loop for l being the hash-values of *known* do (setf (gethash l known-lambdas) t))
      (let ((l (gethash clone lambdas)))
	;; called at all (a clone whose calls cp0 inlined isn't), and of use
	(and l
	     (some (lambda (p) (/= 0 (gethash p *ptypes* 0))) (cadr l))
	     (inside l))))))

;;; ------------------------------------------------------------------
;;; The entry point

(defparameter *cptypes-iterations* 12)

(defun cptypes (form)
  "FORM, cp0's output, with the types found told to SBCL (see above)."
  (multiple-value-bind (lambdas escapes) (known-candidates form)
    (multiple-value-bind (form clones) (add-clones form lambdas escapes)
      ;; the clones' variables, and the known procedures again
      (clrhash *vi*)
      (analyze form)
      (multiple-value-bind (lambdas escapes) (known-candidates form)
	(let ((*known* (make-hash-table :test 'eq))
	      (*ptypes* (make-hash-table :test 'eq))
	      (*vtypes* (make-hash-table :test 'eq))
	      (*facts* (make-hash-table :test 'eq))
	      (*after-facts* '())
	      (*emit-count* 0))
	  (loop for v being the hash-keys of lambdas using (hash-value l)
		unless (gethash v escapes)
		  do (setf (gethash v *known*) l)
		     (dolist (p (cadr l)) (setf (gethash p *ptypes*) 0)))
	  (let ((converged nil))
	    (loop repeat *cptypes-iterations*
		  do (let ((*ptypes-next* (make-hash-table :test 'eq)))
		       (clrhash *vtypes*)
		       (ty form)
		       (let ((changed nil))
			 (loop for p being the hash-keys of *ptypes* using (hash-value old)
			       do (let ((new (logior old (gethash p *ptypes-next* 0))))
				    (unless (= new old) (setf changed t (gethash p *ptypes*) new))))
			 (unless changed (setq converged t) (return)))))
	    (unless converged
	      (loop for p being the hash-keys of *ptypes* do (setf (gethash p *ptypes*) +all+))))
	  ;; useless clones go
	  (let ((drop (remove-if (lambda (c) (useful-clone-p (car c) lambdas)) clones)))
	    (when drop
	      (setq form (drop-clones form drop))
	      (dolist (c drop) (remhash (car c) *known*))))
	  (clrhash *vtypes*)
	  (let ((*emitting* t) (*ptypes-next* (make-hash-table :test 'eq)))
	    (nth-value 1 (ty form))))))))

;;; ------------------------------------------------------------------
;;; The primitives the emitted code calls

(defun install-cptypes-primitives ()
  (let ((names '()))
    (loop for (name type) in '(("fixnum" fixnum) ("flonum" double-float) ("pair" cons) ("integer" integer))
	  do (dolist (kind '("the" "assume"))
	       (let* ((full (concatenate 'string "%cp-" kind "-" name))
		      (type type)
		      (op (if (string= kind "the") 'the #+sbcl 'sb-ext:truly-the #-sbcl 'the)))
		 (push full names)
		 (host-set! full (lambda (x) x))
		 (setf (compiler-macro-function (location (sym full)))
		       (lambda (form env)
			 (declare (ignore env))
			 (if (and (consp form) (not (eq (car form) 'funcall)) (= (length form) 2))
			     `(,op ,type ,(second form))
			     form))))))
    (register-primitive-names names)))
