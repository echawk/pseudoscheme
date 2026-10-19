; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-PSYNTAX -*-
;;;; cpletrec: letrec and letrec* made nested lets and letrecs of lambdas
;;;; (docs/optimizations.md, "Plan (2026-10-18)", item 6), after Waddell,
;;;; Sarkar and Dybvig, "Fixing Letrec" (2005), much simplified.  The
;;;; first step of OPTIMIZE-CORE (src/cp0.lisp), so every mode's core
;;;; forms get it.
;;;;
;;;; A body of definitions is a letrec* (DEFINITIONS-AS-LETREC), which
;;;; the translator makes a LET of variables first false, LABELS for the
;;;; lambdas, and an assignment for each other init: those variables are
;;;; assigned, so SBCL infers no type for them and boxes them when a
;;;; closure refers to them.  Here
;;;;
;;;;   (letrec* ((a (* n 2)) (f (lambda (x) (+ x a))) (b (f 3))) body)
;;;;
;;;; becomes
;;;;
;;;;   ((lambda (a) (letrec ((f (lambda (x) (+ x a))))
;;;;                  ((lambda (b) body) (f 3))))
;;;;    (* n 2))
;;;;
;;;; in which a and b are bound once, and f's letrec is all lambdas, which
;;;; cp0 can inline from.  The inits that aren't lambdas keep their order;
;;;; each is bound by a LET as soon as everything it refers to (through
;;;; the lambdas it refers to, too) is bound; the lambdas it needs are
;;;; bound just before it, in one letrec.  When an init refers to a
;;;; variable not bound yet (a use before definition, which the letrec
;;;; checks report), it and the bindings after it stay a letrec*, inside
;;;; what was bound before, as they were.

(in-package "QUASISCHEME-PSYNTAX")

(defun core-mentions-p (e v)
  "Whether core form E refers to variable V (outside quoted data)."
  (cond ((eq e v) t)
	((atom e) nil)
	((quote-p e) nil)
	(t (loop for x on e
		 thereis (if (consp x) (core-mentions-p (car x) v) (eq x v))))))

(defun make-let-form (var init body)
  (list (list (sym "lambda") (list var) body) init))

(defun fix-bindings (bindings)
  "For BINDINGS in letrec* order, the steps that bind a prefix of them
nested -- (:let b) or (:letrec bs), outermost first -- and the bindings
left, in their order."
  (let* ((vars (mapcar #'car bindings))
	 (refs (make-hash-table :test 'eq))
	 (bound '())			; variables bound so far, outside
	 (pending (remove-if-not (lambda (b) (cp-lambda-p (cadr b))) bindings))
	 (steps '())
	 (stuck nil))
    ;; the variables of the group each init refers to
    (dolist (b bindings)
      (setf (gethash (car b) refs)
	    (remove-if-not (lambda (v) (core-mentions-p (cadr b) v)) vars)))
    (labels ((needed-lambdas (b)
	       ;; the lambdas not yet bound that B's init reaches
	       (let ((seen '()) (todo (list b)))
		 (loop while todo
		       do (dolist (v (gethash (car (pop todo)) refs))
			    (let ((lb (find v pending :key #'car)))
			      (when (and lb (not (member lb seen)))
				(push lb seen) (push lb todo)))))
		 seen))
	     (refers-only-to-p (b allowed)
	       (every (lambda (v) (or (member v bound) (member v allowed))) (gethash (car b) refs))))
      (dolist (b bindings)
	(unless (or stuck (cp-lambda-p (cadr b)))
	  (let* ((lams (needed-lambdas b))
		 (lam-vars (mapcar #'car lams)))
	    ;; B's init and the lambdas it needs refer only to what is bound
	    ;; already, or to those lambdas: not to B, nor to what comes after
	    (if (and (refers-only-to-p b lam-vars)
		     (every (lambda (l) (refers-only-to-p l lam-vars)) lams))
		(progn
		  (when lams
		    (push (list :letrec (reverse lams)) steps)
		    (setq pending (set-difference pending lams))
		    (setq bound (append lam-vars bound)))
		  (push (list :let b) steps)
		  (push (car b) bound))
		(setq stuck t)))))
      (values (reverse steps)
	      (remove-if (lambda (b) (member (car b) bound)) bindings)))))

(defun wrap-steps (steps inner)
  (dolist (step (reverse steps) inner)
    (setq inner
	  (ecase (first step)
	    (:let (make-let-form (car (second step)) (cadr (second step)) inner))
	    (:letrec (list (sym "letrec") (second step) inner))))))

(defun definitions-shape (e)
  "If E is what DEFINITIONS-AS-LETREC makes of a body of definitions,
((lambda (v ...) [(letrec (lambda-binding ...)] (begin (set! v init) ...
form ...)[)]) '#f ...), its parts: the variables, the lambda bindings,
the leading assignments as bindings, and the forms after them."
  (when (and (consp e) (cp-lambda-p (car e))
	     (listp (cadr (car e))) (proper-list-p (cadr (car e)))
	     (null (cdddr (car e)))
	     (= (length (cadr (car e))) (length (cdr e)))
	     (every (lambda (a) (and (quote-p a) (eq (cadr a) qs:false))) (cdr e)))
    (let* ((vars (cadr (car e)))
	   (body (caddr (car e)))
	   (lambdas (and (consp body) (eq (cp-keyword (car body)) :letrec)
			 (null (cdddr body))
			 (every (lambda (b) (cp-lambda-p (cadr b))) (cadr body))
			 (cadr body)))
	   (seq (if lambdas (caddr body) body))
	   (forms (if (and (consp seq) (eq (cp-keyword (car seq)) :begin)) (cdr seq) (list seq)))
	   (sets '()))
      (loop while (and forms (consp (car forms)) (eq (cp-keyword (car (car forms))) :set!)
		       (member (cadr (car forms)) vars)
		       (not (assoc (cadr (car forms)) sets))
		       ;; not its last form: that's the body's value
		       (cdr forms))
	    do (let ((f (pop forms))) (push (list (cadr f) (caddr f)) sets)))
      (when sets
	(values vars lambdas (reverse sets) forms)))))

(defun fix-definitions (e)
  "E, of DEFINITIONS-SHAPE, nested as FIX-BINDINGS says, or NIL."
  (multiple-value-bind (vars lambdas sets forms) (definitions-shape e)
    (when vars
      (multiple-value-bind (steps left) (fix-bindings (append lambdas sets))
	(when (find :let steps :key #'first)
	  (let* ((left-lambdas (remove-if-not (lambda (b) (member b lambdas)) left))
		 (left-sets (remove-if (lambda (b) (member b lambdas)) left))
		 ;; the variables not bound by a step: assigned later, as before
		 (left-vars (remove-if (lambda (v) (find v steps :key (lambda (s) (and (eq (first s) :let) (car (second s))))))
				       vars))
		 (seq (make-sequence-form
		       (append (mapcar (lambda (b) (list (sym "set!") (car b) (cadr b))) left-sets) forms)))
		 (inner (if left-lambdas (list (sym "letrec") left-lambdas seq) seq))
		 (inner (if left-vars
			    (cons (list (sym "lambda") left-vars inner)
				  (mapcar (lambda (v) (declare (ignore v)) (cp-quote qs:false)) left-vars))
			    inner)))
	    (wrap-steps steps inner)))))))

(defun fix-letrec (bindings body letrec)
  "The nested form for (LETREC BINDINGS . BODY), LETREC its keyword, or
NIL if nothing would change."
  (unless (every (lambda (b) (cp-lambda-p (cadr b))) bindings)
    (multiple-value-bind (steps left) (fix-bindings bindings)
      (when steps
	(wrap-steps steps (if left (list* letrec left body) (make-sequence-form body)))))))

(defun cpletrec (form)
  "FORM with its letrecs, letrec*s and bodies of definitions fixed (see
above)."
  (labels ((walk-list (l)
	     ;; L's elements walked, a dotted tail kept
	     (if (consp l) (cons (walk (car l)) (walk-list (cdr l))) l))
	   (walk (e)
	     (cond ((atom e) e)
		   ((quote-p e) e)
		   ((not (symbolp (car e)))
		    (let ((e (walk-list e)))
		      (or (fix-definitions e) e)))
		   (t (case (cp-keyword (car e))
			(:lambda (list* (car e) (cadr e) (walk-list (cddr e))))
			(:case-lambda (cons (car e) (mapcar (lambda (c) (cons (car c) (walk-list (cdr c)))) (cdr e))))
			((:letrec :letrec*)
			 (let ((bindings (mapcar (lambda (b) (list (car b) (walk (cadr b)))) (cadr e)))
			       (body (walk-list (cddr e))))
			   (or (and bindings (fix-letrec bindings body (car e)))
			       (list* (car e) bindings body))))
			(t (walk-list e)))))))
    (walk form)))

;;; ------------------------------------------------------------------
;;; A closed world (docs/optimizations.md, "Plan", item 4)
;;;
;;; At a top level whose variables are late-bound (Chez scripts, R5RS
;;; and the older reports), a call to a procedure another top-level form
;;; defines goes through the variable, since a later form, or eval, could
;;; define it again.  A file compiled into an executable (--compile) is a
;;; closed world: what it defines is all there is.  CLOSE-TOP-LEVEL makes
;;; its forms one: each variable defined once, as a lambda, and never
;;; assigned is bound in a letrec around them all, so calls to it are
;;; direct calls, which the continuation analysis can see; the global is
;;; still defined, for eval and the like.
;;;
;;;   (define (f n) ... (g n) ...) (define (g n) ...) (display (f 1))
;;;
;;; becomes
;;;
;;;   (begin (define f '#f) (define g '#f)            ; every definition's variable
;;;          (letrec ((f' (lambda (n) ... (g' n) ...)) (g' (lambda (n) ...)))
;;;            (begin (set! f f') (set! g g') (display (f' 1)))))
;;;
;;; What changes: a call before the definition's form has run finds the
;;; procedure rather than an unbound variable, and defining it again (by
;;; eval or load) doesn't change what the file's own calls reach.

(defun top-level-definitions (core)
  "The (define v e) forms at the top of CORE, through begins."
  (cond ((atom core) '())
	((eq (cp-keyword (car core)) :define) (list core))
	((eq (cp-keyword (car core)) :begin) (mapcan #'top-level-definitions (cdr core)))
	(t '())))

(defun core-assigned-variables (core table)
  (cond ((atom core))
	((quote-p core))
	(t (when (and (eq (cp-keyword (car core)) :set!) (symbolp (cadr core)))
	     (setf (gethash (cadr core) table) t))
	   (loop for x on core while (consp x) do (core-assigned-variables (car x) table)))))

(defun close-top-level (cores)
  "CORES, a file's top-level core forms in order, as one form (see above),
or CORES themselves if no procedure qualifies."
  (let ((counts (make-hash-table :test 'eq))
	(assigned (make-hash-table :test 'eq))
	(renames (make-hash-table :test 'eq)))
    (dolist (c cores)
      (dolist (d (top-level-definitions c))
	(incf (gethash (cadr d) counts 0)))
      (core-assigned-variables c assigned))
    (dolist (c cores)
      (dolist (d (top-level-definitions c))
	(let ((v (cadr d)))
	  (when (and (= (gethash v counts) 1) (not (gethash v assigned))
		     (symbolp v) (symbol-package v) (cp-lambda-p (caddr d)))
	    (setf (gethash v renames) (make-lexical (symbol-name v)))))))
    (if (zerop (hash-table-count renames))
	cores
	(let ((lambdas '()))
	  (labels ((rename (e)
		     (cond ((symbolp e) (gethash e renames e))
			   ((atom e) e)
			   ((quote-p e) e)
			   (t (let ((a (rename (car e))) (d (rename-tail (cdr e))))
				(if (and (eq a (car e)) (eq d (cdr e))) e (cons a d))))))
		   (rename-tail (l)
		     (if (consp l) (let ((a (rename (car l))) (d (rename-tail (cdr l))))
				     (if (and (eq a (car l)) (eq d (cdr l))) l (cons a d)))
			 (rename l)))
		   (top (e)
		     ;; a top-level form: its qualifying definitions taken out
		     (cond ((atom e) (rename e))
			   ((and (eq (cp-keyword (car e)) :define) (gethash (cadr e) renames))
			    (push (list (gethash (cadr e) renames) (rename (caddr e))) lambdas)
			    (list (sym "set!") (cadr e) (gethash (cadr e) renames)))
			   ;; any other definition: an assignment, its variable
			   ;; defined at the start (a define can't be in a letrec)
			   ((eq (cp-keyword (car e)) :define)
			    (list (sym "set!") (cadr e) (if (cddr e) (rename (caddr e)) (cp-void))))
			   ((eq (cp-keyword (car e)) :begin)
			    (cons (car e) (mapcar #'top (cdr e))))
			   (t (rename e)))))
	    (let ((body (mapcar #'top cores)))
	      (list (list* (sym "begin")
			   (append
			    (loop for v being the hash-keys of counts
				  collect (list (sym "define") v (cp-quote qs:false)))
			    (list (list (sym "letrec") (reverse lambdas)
					(make-sequence-form body))))))))))))
