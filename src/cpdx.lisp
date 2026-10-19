; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-PSYNTAX -*-
;;;; cpdx: escape analysis for stack allocation (docs/optimizations.md,
;;;; "Plan (2026-10-18)", item 5, and idea 5), after cp0 and cptypes, on
;;;; the core forms every mode makes (OPTIMIZE-CORE, src/cp0.lisp).
;;;;
;;;; Two kinds of object are found not to outlive the call that makes
;;;; them, and SBCL is told so, which then allocates them on the stack:
;;;;
;;;;  - a lambda passed to a primitive that only calls it, during the call
;;;;    (map, for-each, vector-for-each, ..., the *DX-PROCEDURE-ARGUMENTS*
;;;;    below), when the lambda's body can't capture a continuation.  The
;;;;    continuation transformation then calls the primitive itself (a
;;;;    plain call, PLAIN-PRIMITIVE-CALL-P), so no frame holds the
;;;;    closure; the argument is marked (%cp-dx (lambda ...)).  The call's
;;;;    other arguments mustn't capture either: the closure is made before
;;;;    they are evaluated, and a function that captures is a state
;;;;    machine whose frames a re-entry rebuilds without it;
;;;;
;;;;  - a rest list, in a lambda whose body can't capture, that is only
;;;;    read: the argument of car, null?, pair?, length and the like, or of
;;;;    cdr whose result is only read so too, never stored, returned,
;;;;    assigned or closed over.  The body is marked
;;;;    (begin (%cp-dx-rest v) body).
;;;;
;;;; TRANSLATE-CORE (src/psyntax.lisp) turns the marks into DYNAMIC-EXTENT
;;;; declarations (STACK-ALLOCATE-MARKED).  Why "can't capture" matters: a
;;;; continuation captured while the object is live copies the frames
;;;; that refer to it, and re-entered after the call returned, they would
;;;; refer to a stack that is gone.  Escape-only exits unwind, which is
;;;; fine.

(in-package "QUASISCHEME-PSYNTAX")

(defparameter *dx-procedure-arguments*
  '(("map" 0) ("for-each" 0) ("vector-map" 0) ("vector-for-each" 0)
    ("string-map" 0) ("string-for-each" 0) ("call-with-values" 0 1)
    ("find" 0) ("filter" 0) ("partition" 0) ("fold-left" 0) ("fold-right" 0)
    ("remp" 0) ("memp" 0) ("assp" 0) ("exists" 0) ("for-all" 0)
    ("list-sort" 0) ("vector-sort" 0) ("vector-sort!" 0)
    ("member" 2) ("assoc" 2) ("hashtable-update!" 2))
  "Primitives that call the procedures at these argument positions only
during the call, and keep no reference to them: *PROCEDURE-ARGUMENTS*
(src/continuations.lisp) less apply, whose procedure may keep its rest
list.")

(defparameter *dx-rest-readers*
  '("car" "cadr" "caddr" "cadddr" "null?" "pair?" "length" "list-ref")
  "Primitives whose first argument a stack-allocated rest list may be:
they read the list and return none of its pairs.")

(defparameter *dx-rest-tails* '("cdr" "cddr" "cdddr")
  "Primitives that return a tail of their first argument.")

(defvar *dx-bound* nil
  "The variables bound in the form being analyzed: an operator that is
one of them is no primitive.")

(defun dx-primitive-name (op)
  "The name of the primitive OP refers to, if it is one, not shadowed."
  (and (not (gethash op *dx-bound*)) (primitive-name op)))

(defun note-dx-bindings (form)
  (cond ((atom form))
	((quote-p form))
	(t (when (cp-lambda-p form)
	     (dolist (v (formals-list (cadr form))) (setf (gethash v *dx-bound*) t)))
	   (when (member (cp-keyword (car form)) '(:letrec :letrec*))
	     (dolist (b (cadr form)) (when (consp b) (setf (gethash (car b) *dx-bound*) t))))
	   (loop for x on form while (consp x) do (note-dx-bindings (car x))))))

(defun dx-closure-p (e)
  "Whether E is a lambda that can go on the stack as a primitive's
argument: its body can't capture."
  (and (cp-lambda-p e) (listp (cadr e)) (null (cdddr e))
       (not (may-capture-p (caddr e)))))

(defun marker-wrapper-p (e)
  "Whether E is (%cp-assume-T x) or (%cp-the-T x), cptypes' identity
wrappers."
  (and (consp e) (symbolp (car e)) (consp (cdr e)) (null (cddr e))
       (let ((name (symbol-name (car e))))
	 (and (> (length name) 4) (string-equal name "%cp-" :end1 4)))))

(defun rest-alias-p (e v)
  "Whether E evaluates to V's list or a tail of it."
  (cond ((eq e v) t)
	((marker-wrapper-p e) (rest-alias-p (cadr e) v))
	((and (consp e) (consp (cdr e)) (null (cddr e)))
	 (and (member (dx-primitive-name (car e)) *dx-rest-tails* :test #'equal)
	      (rest-alias-p (cadr e) v)))))

(defun rest-confined-p (e v)
  "Whether, in E, V's list (or a tail of it) is only read."
  (cond ((rest-alias-p e v) nil)
	((atom e) t)
	((quote-p e) t)
	((cp-lambda-p e) (not (tree-mentions-p (cddr e) v)))
	((eq (cp-keyword (car e)) :case-lambda) (not (tree-mentions-p (cdr e) v)))
	((member (cp-keyword (car e)) '(:set! :define))
	 (and (not (eq (cadr e) v)) (rest-confined-p (caddr e) v)))
	((eq (cp-keyword (car e)) :if)
	 ;; the test may be the list itself: only its truth is used
	 (and (or (rest-alias-p (cadr e) v) (rest-confined-p (cadr e) v))
	      (every (lambda (x) (rest-confined-p x v)) (cddr e))))
	((and (member (dx-primitive-name (car e)) *dx-rest-readers* :test #'equal)
	      (consp (cdr e)) (rest-alias-p (cadr e) v))
	 (every (lambda (x) (rest-confined-p x v)) (cddr e)))
	(t (loop for x on e
		 always (and (consp x) (rest-confined-p (car x) v))))))

(defun tree-mentions-p (tree x)
  (cond ((eq tree x) t)
	((atom tree) nil)
	(t (or (tree-mentions-p (car tree) x) (tree-mentions-p (cdr tree) x)))))

(defun dx-rest-variable (lambda)
  "LAMBDA's rest variable, if its list can go on the stack."
  (let* ((formals (cadr lambda))
	 (v (if (consp formals) (cdr (last formals)) formals)))   ; (a b . v) or v
    (and v (symbolp v)
	 (null (cdddr lambda))
	 (not (may-capture-p (caddr lambda)))
	 (rest-confined-p (caddr lambda) v)
	 v)))

(defun cpdx-kinds ()
  "What cpdx marks, from QUASISCHEME_CPDX: 0 (nothing), closures, rest,
or (the default) both."
  (let ((v (uiop:getenv "QUASISCHEME_CPDX")))
    (cond ((equal v "0") '())
	  ((equal v "closures") '(:closures))
	  ((equal v "rest") '(:rest))
	  (t '(:closures :rest)))))

(defun cpdx (form)
  "FORM with the closures and rest lists that can go on the stack marked."
  (let ((*dx-bound* (make-hash-table :test 'eq))
	(kinds (cpdx-kinds))
	(dx (sym "%cp-dx"))
	(dx-rest (sym "%cp-dx-rest")))
    (note-dx-bindings form)
    (labels ((walk (e)
	       (cond ((atom e) e)
		     ((quote-p e) e)
		     ((cp-lambda-p e)
		      (let* ((body (mapcar #'walk (cddr e)))
			     (new (list* (car e) (cadr e) body))
			     (v (and (member :rest kinds) (dx-rest-variable new))))
			(if v
			    (progn
			      (incf (seventh *cp0-stats*))
			      (list (car e) (cadr e) (list (sym "begin") (list dx-rest v) (car body))))
			    new)))
		     ((symbolp (car e))
		      (let* ((name (and (not (cp-keyword (car e))) (dx-primitive-name (car e))))
			     (positions (and name (cdr (assoc name *dx-procedure-arguments* :test #'string=))))
			     (args (mapcar #'walk (cdr e))))
			(cons (car e)
			      (loop for a in args for i from 0
				    collect (if (and (member i positions) (member :closures kinds) (dx-closure-p a)
						     ;; no other argument is a capture site: the
						     ;; closure, made first, mustn't be live across one
						     (notany #'may-capture-p args))
						(progn (incf (sixth *cp0-stats*)) (list dx a))
						a)))))
		     (t (loop for x on e
			      collect (if (consp x) (walk (car x)) x))))))
      (walk form))))

(defun install-cpdx-primitives ()
  (dolist (name '("%cp-dx" "%cp-dx-rest"))
    (host-set! name (if (string= name "%cp-dx") (lambda (x) x) (lambda (x) (declare (ignore x)) qs:unspecific)))
    (let ((name name))
      (setf (compiler-macro-function (location (sym name)))
	    (lambda (form env)
	      (declare (ignore env))
	      ;; what STACK-ALLOCATE-MARKED didn't take: the value, or nothing
	      (if (and (consp form) (not (eq (car form) 'funcall)) (= (length form) 2))
		  (if (string= name "%cp-dx") (second form) `(progn ,(second form) nil))
		  form)))))
  (register-primitive-names '("%cp-dx" "%cp-dx-rest")))

;;; ------------------------------------------------------------------
;;; In the translated Lisp (TRANSLATE-CORE)

(defun stack-allocate-marked (lisp)
  "LISP, the translation of a core form, with cpdx's marks made
DYNAMIC-EXTENT declarations: a call with (%cp-dx f) arguments binds each
f to a variable declared so, around the call; a lambda whose body has
(%cp-dx-rest v) declares its rest list V so."
  (let ((dx (location (sym "%cp-dx")))
	(dx-rest (location (sym "%cp-dx-rest"))))
    (labels ((mark-p (x sym)
	       (and (consp x) (eq (car x) sym) (consp (cdr x)) (null (cddr x))))
	     (rest-mark-p (x v)
	       (and (mark-p x dx-rest) (eq (second x) v)))
	     (strip-rest-mark (body v)
	       ;; BODY with V's mark taken out, and whether it was there:
	       ;; at the top of the body, or of a PROGN in it
	       (let ((found nil))
		 (values (loop for x in body
			       unless (and (rest-mark-p x v) (setq found t))
				 collect (if (and (not found) (consp x) (eq (car x) 'progn)
						  (some (lambda (y) (rest-mark-p y v)) (cdr x)))
					     (progn (setq found t)
						    (cons 'progn (remove-if (lambda (y) (rest-mark-p y v)) (cdr x))))
					     x))
			 found)))
	     (walk (x)
	       (cond ((atom x) x)
		     ((eq (car x) 'quote) x)
		     ((and (eq (car x) 'lambda) (consp (cdr x)) (listp (cadr x))
			   (member '&rest (cadr x)))
		      (let ((v (cadr (member '&rest (cadr x))))
			    (body (mapcar #'walk (cddr x))))
			(multiple-value-bind (body found) (strip-rest-mark body v)
			  (if found
			      `(lambda ,(cadr x) (declare (dynamic-extent ,v)) ,@body)
			      `(lambda ,(cadr x) ,@body)))))
		     ((and (symbolp (car x)) (not (eq (car x) 'function))
			   (some (lambda (a) (mark-p a dx)) (cdr x)))
		      (let ((bindings '()))
			(let ((call (cons (car x)
					  (mapcar (lambda (a)
						    (if (mark-p a dx)
							(let ((g (gensym "DX")))
							  (push (list g (walk (second a))) bindings)
							  g)
							(walk a)))
						  (cdr x)))))
			  (setq bindings (nreverse bindings))
			  `(let ,bindings
			     (declare (dynamic-extent ,@(mapcar #'car bindings)))
			     ,call))))
		     (t (let ((a (walk (car x))) (d (walk (cdr x))))
			  (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d)))))))
      (walk lisp))))
