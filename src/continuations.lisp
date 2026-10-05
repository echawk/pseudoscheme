; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; Full continuations from generalized stack inspection
;;;;
;;;; See docs/continuations.md.  With *FULL-CONTINUATIONS* true, each
;;;; core form psyntax produces goes through CC-TRANSFORM before the
;;;; translator.
;;;;
;;;; A procedure whose body makes a call that may capture a continuation,
;;;; other than in tail position, becomes a state machine: one Lisp
;;;; function whose body is a flat TAGBODY, with a label after each such
;;;; call (a "site") and the procedure's local variables hoisted to its
;;;; top.  A site pushes a frame, a stack-allocated vector
;;;;
;;;;    #(machine site-number parameter-count live-variable ...)
;;;;
;;;; onto *FSTACK* for the extent of the call.  Capturing a continuation
;;;; copies the frames up to the base.  Re-entering one rebuilds them,
;;;; outermost first: each frame's machine is called with the site number
;;;; and the value returned to it, restores the live variables and jumps
;;;; to the label after the call.  So the normal path allocates nothing
;;;; on the heap and creates no closures, and the code stays flat, which
;;;; SBCL needs: it compiles a function with a thousand closures in it in
;;;; minutes, or not at all.
;;;;
;;;; Variables that are assigned, and that a frame may hold across an
;;;; assignment, are boxed, so that a re-entered frame shares them with
;;;; whatever else refers to them, as Scheme requires.
;;;;
;;;; Invoking a continuation within the extent of its call/cc throws to
;;;; that call/cc.  Otherwise it throws to the base, established around
;;;; each top-level evaluation, which runs the befores of the
;;;; continuation's dynamic-winds and rebuilds its frames.
;;;;
;;;; Known gaps:
;;;;  - Lisp frames between Scheme frames (a Scheme procedure passed to a
;;;;    Lisp function that calls it, other than the ones redefined here)
;;;;    aren't recorded, and re-entering through one is not detected.
;;;;  - Re-entry unwinds every active dynamic-wind and runs the befores
;;;;    of all of the continuation's, even those the two share.
;;;;  - Exception handlers (with-exception-handler) and parameterize
;;;;    aren't re-established on re-entry.

(in-package "PSEUDOSCHEME-PSYNTAX")

;;; ------------------------------------------------------------------
;;; The run-time side

(defvar *fstack* '()
  "The frames of the calls in progress, innermost first.  A base binds
it to (:BASE).")

(defvar *winders* '()
  "The active dynamic-winds' (before . after) pairs, innermost first.")

(defvar *base-tag* nil
  "The catch tag of the innermost base, to which a re-entered
continuation throws.")

(defmacro with-frame ((frame) &body body)
  "Run BODY with FRAME (a vector, already allocated) pushed on *FSTACK*."
  (let ((cell (gensym "CELL")))
    `(let ((,cell (cons ,frame *fstack*)))
       (declare (dynamic-extent ,cell))
       (let ((*fstack* ,cell))
	 ,@body))))

(defun call-with-frame (thunk k)
  "Call THUNK with a frame whose continuation is K, a Lisp function."
  (let ((frame (vector :k k)))
    (declare (dynamic-extent frame))
    (multiple-value-call k (with-frame (frame) (funcall thunk)))))

(defun call-with-continuation-base (thunk)
  "Call THUNK as the outermost frame continuations can capture.  A
continuation re-entered inside throws back here with a thunk that
rebuilds it."
  (let* ((tag (list 'base))
	 (*base-tag* tag)
	 (*fstack* (list :base))
	 (*winders* '()))
    (loop
      (setq thunk (catch tag
		    (return-from call-with-continuation-base (funcall thunk)))))))

(defun winder-extent (winder thunk)
  "Call THUNK inside dynamic-wind WINDER (whose before has run): its
after runs however THUNK is left."
  (let ((frame (vector :winder winder))
	(normal nil))
    (declare (dynamic-extent frame))
    (multiple-value-prog1
	(unwind-protect
	     (multiple-value-prog1
		 (let ((*winders* (cons winder *winders*)))
		   (with-frame (frame) (funcall thunk)))
	       (setq normal t))
	  (unless normal (funcall (cdr winder))))
      (funcall (cdr winder)))))

(defun full-dynamic-wind (before thunk after)
  (funcall before)
  (winder-extent (cons before after) thunk))

(defun full-call-with-values (producer consumer)
  (call-with-frame producer (lambda (&rest values) (apply consumer values))))

(defun capture-frames ()
  "Copies of the current frames, outermost first, and whether they reach
a base."
  (let ((frames '()))
    (do ((s *fstack* (cdr s)))
	((null s) (values frames nil))
      (if (eq (car s) :base)
	  (return (values frames t))
	  (push (copy-seq (car s)) frames)))))

(defun resume-frame (frame inner)
  "Re-establish FRAME around INNER, a thunk computing what the frame's
call returns, then continue the frame."
  (let ((head (svref frame 0)))
    (case head
      (:k (multiple-value-call (svref frame 1) (with-frame (frame) (funcall inner))))
      (:winder (winder-extent (svref frame 1) inner))
      (t (let ((value (with-frame (frame) (funcall inner))))
	   (apply head (svref frame 1) value frame (make-list (svref frame 2))))))))

(defun rebuild-frames (frames values)
  "Re-establish FRAMES, outermost first, and return VALUES to the
innermost."
  (if (null frames)
      (values-list values)
      (resume-frame (car frames) (lambda () (rebuild-frames (cdr frames) values)))))

(defun reenter (frames winders values)
  ;; The base has no dynamic-winds active: run the befores of the
  ;; continuation's, outermost first.
  (let ((outer '()))
    (dolist (w (reverse winders))
      (let ((*winders* outer)) (funcall (car w)))
      (push w outer)))
  (rebuild-frames frames values))

(defun full-call/cc (f)
  (multiple-value-bind (frames rebuildable) (capture-frames)
    (let ((winders *winders*)
	  (live (list t))
	  (tag (list 'continuation)))
      (flet ((k (&rest values)
	       (cond ((car live) (throw tag (values-list values)))
		     ((and rebuildable *base-tag*)
		      (throw *base-tag* (lambda () (reenter frames winders values))))
		     (t (ps:scheme-error "continuation invoked after its extent ended, and not re-entrant (no base around its capture)")))))
	(unwind-protect (catch tag (funcall f #'k))
	  (setf (car live) nil))))))

;;; What the transformation's output uses.  Each takes Scheme
;;; expressions, translated in place by the translator, and quoted data.

(defmacro %site (frame call)
  "Make CALL with FRAME, a (vector ...) form, pushed: both on the stack."
  (let ((f (gensym "FRAME")) (cell (gensym "CELL")))
    `(let* ((,f ,frame)
	    (,cell (cons ,f *fstack*)))
       (declare (dynamic-extent ,f ,cell))
       (let ((*fstack* ,cell))
	 ,call))))

(defmacro %machine (entry dispatch &rest statements)
  "A machine's body: STATEMENTS (expressions, and quoted symbols as
labels), entered at the start if ENTRY is 0, else at the label
DISPATCH, '((site . label) ...), gives for site ENTRY, where statements
restore the site's live variables and continue after it."
  `(block %machine
     (tagbody
	(case ,entry
	  ,@(loop for (site . label) in (second dispatch)
		  collect `(,site (go ,label))))
	,@(mapcar (lambda (s)
		    (if (and (consp s) (eq (car s) 'quote)) (second s) s))
		  statements))))

(defmacro %return (x) `(return-from %machine ,x))
(defmacro %go (label) `(go ,(second label)))

;;; ------------------------------------------------------------------
;;; Which calls may capture

(defvar *full-continuations* nil
  "True to compile with full, re-entrant continuations.")

(defparameter *full-replacements*
  '(("call-with-current-continuation" . "%full-call/cc")
    ("call/cc" . "%full-call/cc")
    ("dynamic-wind" . "%full-dynamic-wind")
    ("call-with-values" . "%full-call-with-values")
    ("map" . "%full-map")
    ("for-each" . "%full-for-each"))
  "Primitives that call procedures, and their frame-aware versions.")

(defparameter *calling-primitives*
  '("apply" "call-with-current-continuation" "call/cc" "dynamic-wind"
    "call-with-values" "map" "for-each" "%full-call/cc" "%full-dynamic-wind" "%full-call-with-values"
    "%full-map" "%full-for-each" "%call-with-frame"
    "vector-map" "vector-for-each" "string-map" "string-for-each"
    "with-exception-handler" "raise" "raise-continuable" "force"
    "call-with-port" "call-with-input-file" "call-with-output-file"
    "with-input-from-file" "with-output-to-file" "eval" "load"
    "list-sort" "vector-sort" "vector-sort!" "find" "filter" "partition"
    "fold-left" "fold-right" "remp" "memp" "assp" "exists" "for-all"
    "member" "assoc" "hashtable-update!" "make-parameter")
  "Primitives that may call a procedure, so that a call to one is a
call site like any other.  Calls to every other primitive are not.")

(defvar *primitive-names* nil)

(defun keyword-p (x name)
  (and (symbolp x) (string= (symbol-name x) name)))

(defun scheme-symbol-p (x)
  (and (symbolp x) x (eq (symbol-package x) (find-package "SCHEME"))))

(defun primitive-names ()
  (or *primitive-names*
      (let ((table (make-hash-table :test 'equal)))
	(dolist (s (all-primitive-names)) (setf (gethash (ps:scheme-symbol-name s) table) t))
	(dolist (p ps-r6rs:*primitives*) (setf (gethash (car p) table) t))
	(dolist (p *full-replacements*)
	  (setf (gethash (car p) table) t (gethash (cdr p) table) t))
	(setf (gethash "%call-with-frame" table) t)
	(setq *primitive-names* table))))

(defun primitive-name (x)
  "The name of primitive X, if X refers to one: a host global with a
primitive's name (psyntax renames everything else), or (primitive x)."
  (cond ((and (consp x) (keyword-p (car x) "PRIMITIVE")) (ps:scheme-symbol-name (cadr x)))
	((scheme-symbol-p x)
	 (let ((name (ps:scheme-symbol-name x)))
	   (and (gethash name (primitive-names)) name)))))

(defun calling-call-p (operator)
  "Whether a call with OPERATOR (normalized) may capture.  Operators that
are Lisp symbols are the transformation's own, or the translator's."
  (cond ((and (symbolp operator) (not (scheme-symbol-p operator))) nil)
	(t (let ((name (primitive-name operator)))
	     (or (null name) (member name *calling-primitives* :test #'string=))))))

(defun lambda-form-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
(defun quote-form-p (x) (and (consp x) (keyword-p (car x) "QUOTE")))

(defun let-form-p (e)
  "((lambda (v ...) body) arg ...), with as many arguments as variables."
  (and (consp e) (lambda-form-p (car e))
       (listp (cadr (car e))) (null (cdr (last (cadr (car e)))))
       (= (length (cadr (car e))) (length (cdr e)))))

(defun atomic-p (x)
  (or (atom x) (quote-form-p x) (lambda-form-p x)
      (and (consp x) (keyword-p (car x) "PRIMITIVE"))))

(defun simple-p (e)
  "Whether E makes no call that may capture (outside nested lambdas)."
  (cond ((atomic-p e) t)
	(t (let ((head (car e)))
	     (cond ((or (keyword-p head "IF") (keyword-p head "BEGIN"))
		    (every #'simple-p (cdr e)))
		   ((or (keyword-p head "SET!") (keyword-p head "DEFINE"))
		    (simple-p (caddr e)))
		   ((keyword-p head "LETREC")
		    (and (every (lambda (b) (simple-p (cadr b))) (cadr e))
			 (simple-p (caddr e))))
		   ((lambda-form-p head)
		    (and (simple-p (caddr head)) (every #'simple-p (cdr e))))
		   (t (and (not (calling-call-p head)) (every #'simple-p (cdr e)))))))))

(defun replacement (x)
  (let ((name (primitive-name x)))
    (let ((new (and name (cdr (assoc name *full-replacements* :test #'string=)))))
      (if new (sym new) x))))

(defun fresh ()
  (funcall (host-ref "gensym")))

(defun formal-variables (formals)
  (cond ((null formals) '())
	((symbolp formals) (list formals))
	(t (cons (car formals) (formal-variables (cdr formals))))))

;;; ------------------------------------------------------------------
;;; Boxing assigned variables
;;;
;;; A variable that is assigned, bound in a procedure that becomes a
;;; machine, is boxed (a cons): the frame then holds the box, which a
;;; re-entered frame shares.  Exempt are the variables psyntax's letrec*
;;; (internal definitions) assigns once, when no site can run between
;;; the variable's binding and its assignment.

(defun box-assigned (form)
  (let ((counts (make-hash-table :test 'eq))	; lexical -> assignments
	(binder (make-hash-table :test 'eq))	; lexical -> machine-p of its procedure
	(exempt (make-hash-table :test 'eq))
	(boxed (make-hash-table :test 'eq)))
    (labels ((scan (e machine-p)
	       ;; MACHINE-P: whether the enclosing procedure has sites
	       (cond ((atom e))
		     ((quote-form-p e))
		     ((lambda-form-p e)
		      (let ((m (not (tail-simple-p (caddr e)))))
			(dolist (v (formal-variables (cadr e))) (setf (gethash v binder) m))
			(scan (caddr e) m)))
		     ((let-form-p e)
		      (dolist (v (cadr (car e))) (setf (gethash v binder) machine-p))
		      (note-letrec* e)
		      (dolist (a (cdr e)) (scan a machine-p))
		      (scan (caddr (car e)) machine-p))
		     ((keyword-p (car e) "SET!")
		      (incf (gethash (cadr e) counts 0))
		      (scan (caddr e) machine-p))
		     ((keyword-p (car e) "LETREC")
		      (dolist (b (cadr e))
			(setf (gethash (car b) binder) machine-p)
			(scan (cadr b) machine-p))
		      (scan (caddr e) machine-p))
		     (t (dolist (x e) (scan x machine-p)))))
	     (note-letrec* (e)
	       ;; ((lambda (v ...) (begin (set! v e) ... body)) '#f ...)
	       (let ((vars (cadr (car e))) (body (caddr (car e))))
		 (when (and (every (lambda (a) (equal a `(,(sym "quote") ,ps:false))) (cdr e))
			    (consp body) (keyword-p (car body) "BEGIN"))
		   (loop for s in (cdr body)
			 while (and (consp s) (keyword-p (car s) "SET!") (member (cadr s) vars))
			 while (simple-p (caddr s))
			 do (setf (gethash (cadr s) exempt) t)))))
	     (rewrite (e)
	       (cond ((and (symbolp e) (gethash e boxed)) `(car ,e))
		     ((atom e) e)
		     ((quote-form-p e) e)
		     ((lambda-form-p e)
		      (let* ((formals (cadr e))
			     (bvs (remove-if-not (lambda (v) (gethash v boxed)) (formal-variables formals)))
			     (body (rewrite (caddr e))))
			`(,(car e) ,formals
			  ,(if bvs
			       `((,(sym "lambda") ,bvs ,body) ,@(mapcar (lambda (v) `(list ,v)) bvs))
			       body))))
		     ((and (keyword-p (car e) "SET!") (gethash (cadr e) boxed))
		      `(rplaca ,(cadr e) ,(rewrite (caddr e))))
		     ((keyword-p (car e) "SET!")
		      `(,(car e) ,(cadr e) ,(rewrite (caddr e))))
		     ((keyword-p (car e) "LETREC")
		      `(,(car e) ,(mapcar (lambda (b) (list (car b) (rewrite (cadr b)))) (cadr e))
			,(rewrite (caddr e))))
		     (t (mapcar #'rewrite e)))))
      (scan form (not (tail-simple-p form)))
      (maphash (lambda (v n)
		 (when (and (plusp n) (gethash v binder)
			    (not (and (= n 1) (gethash v exempt))))
		   (setf (gethash v boxed) t)))
	       counts)
      ;; letrec-bound variables are bound by LETREC, not a lambda: box them
      ;; where they're bound only if they're assigned (psyntax doesn't).
      (if (zerop (hash-table-count boxed)) form (rewrite form)))))

;;; ------------------------------------------------------------------
;;; Simple expressions, and procedures

(defun simple (e)
  "Simple E with its lambdas transformed and its references to the
primitives in *FULL-REPLACEMENTS* replaced."
  (cond ((quote-form-p e) e)
	((and (consp e) (keyword-p (car e) "PRIMITIVE")) (replacement e))
	((symbolp e) (if (scheme-symbol-p e) (replacement e) e))
	((atom e) e)
	((lambda-form-p e) (transform-lambda e))
	((keyword-p (car e) "LETREC")
	 `(,(car e) ,(mapcar (lambda (b) (list (car b) (simple (cadr b)))) (cadr e))
	   ,(simple (caddr e))))
	((or (keyword-p (car e) "SET!") (keyword-p (car e) "DEFINE"))
	 `(,(car e) ,(cadr e) ,(simple (caddr e))))
	(t (mapcar #'simple e))))

(defun tail-simple-p (e)
  "Whether E makes no call that may capture except in tail position: a
procedure with such a body needs no machine."
  (cond ((simple-p e) t)
	(t (let ((head (car e)))
	     (cond ((keyword-p head "IF")
		    (and (simple-p (cadr e)) (tail-simple-p (caddr e)) (tail-simple-p (cadddr e))))
		   ((keyword-p head "BEGIN")
		    (and (every #'simple-p (butlast (cdr e))) (tail-simple-p (car (last e)))))
		   ((keyword-p head "LETREC")
		    (and (every (lambda (b) (simple-p (cadr b))) (cadr e))
			 (tail-simple-p (caddr e))))
		   ((let-form-p e)
		    (and (every #'simple-p (cdr e)) (tail-simple-p (caddr head))))
		   ((or (keyword-p head "SET!") (keyword-p head "DEFINE")) nil)
		   (t (every #'simple-p e)))))))

(defun transform-lambda (e)
  (let ((formals (cadr e)) (body (caddr e)))
    (if (tail-simple-p body)
	`(,(car e) ,formals ,(simple body))
	(build-machine formals body))))

;;; ------------------------------------------------------------------
;;; Machines

(defstruct (machine (:conc-name m-))
  (statements '())			; reversed
  (locals '())
  (sites '())				; (site label variable)
  (count 0)
  (labels 0))

(defvar *m*)

(defun emit (statement) (push statement (m-statements *m*)))

(defun new-local ()
  (let ((v (fresh))) (push v (m-locals *m*)) v))

(defun new-label-named (name)
  (intern name "PSEUDOSCHEME-PSYNTAX"))

(defun new-label ()
  (intern (format nil "%L~D" (incf (m-labels *m*))) "PSEUDOSCHEME-PSYNTAX"))

(defun finish (x k)
  (cond ((eq k :return) (emit `(%return ,x)))
	((eq k :drop) (unless (atomic-p x) (emit x)))
	(t (emit `(,(sym "set!") ,(cdr k) ,x)))))

(defun value-of (e)
  "A simple expression for E's value, emitting what computes it."
  (if (simple-p e)
      (simple e)
      (let ((v (new-local)))
	(flat e (cons :set v))
	v)))

(defun flat (e k)
  "Emit statements that compute E and, as K says, return its value
(:RETURN), drop it (:DROP), or assign it to a variable ((:SET . v))."
  (if (simple-p e)
      (finish (simple e) k)
      (let ((head (car e)))
	(cond ((keyword-p head "IF") (flat-if e k))
	      ((keyword-p head "BEGIN")
	       (loop for (x . more) on (cdr e) do (flat x (if more :drop k))))
	      ((keyword-p head "SET!")
	       (finish `(,head ,(cadr e) ,(value-of (caddr e))) k))
	      ((keyword-p head "LETREC")
	       (dolist (b (cadr e))
		 (push (car b) (m-locals *m*))
		 (flat `(,(sym "set!") ,(car b) ,(cadr b)) :drop))
	       (flat (caddr e) k))
	      ((let-form-p e)
	       (loop for v in (cadr head) for a in (cdr e)
		     do (push v (m-locals *m*))
			(flat a (cons :set v)))
	       (flat (caddr head) k))
	      (t (flat-call e k))))))

(defun flat-if (e k)
  (let ((c (value-of (cadr e)))
	(else (new-label)))
    (emit `(,(sym "if") ,c (,(sym "quote") ,ps:false) (%go ',else)))
    (if (eq k :return)
	(progn (flat (caddr e) :return)
	       (emit `',else)
	       (flat (cadddr e) :return))
	(let ((end (new-label)))
	  (flat (caddr e) k)
	  (emit `(%go ',end))
	  (emit `',else)
	  (flat (cadddr e) k)
	  (emit `',end)))))

(defun flat-call (e k)
  (let ((xs (operands e)))
    (cond ((not (calling-call-p (car xs))) (finish xs k))
	  ((eq k :return) (emit `(%return ,xs)))
	  (t (let ((site (incf (m-count *m*)))
		   (label (new-label))
		   (var (and (consp k) (cdr k))))
	       (emit (if var
			 `(,(sym "set!") ,var (%site ,site ,xs))
			 `(%site ,site ,xs)))
	       (emit `',label)
	       (push (list site label var) (m-sites *m*)))))))

(defun operands (es)
  "Simple expressions for ES, left to right.  One that isn't atomic is
bound to a variable first if one after it may capture, to keep order."
  (let ((out '()))
    (loop for (e . more) on es
	  do (if (simple-p e)
		 (push (simple e) out)
		 (progn
		   (setq out (mapcar (lambda (x)
				       (if (atomic-p x)
					   x
					   (let ((v (new-local)))
					     (emit `(,(sym "set!") ,v ,x))
					     v)))
				     (reverse out)))
		   (setq out (reverse out))
		   (push (value-of e) out))))
    (reverse out)))

(defun variables-in (e table)
  "The variables of TABLE that E refers to (or assigns)."
  (let ((found '()))
    (labels ((walk (x)
	       (cond ((symbolp x) (when (gethash x table) (pushnew x found)))
		     ((atom x))
		     ((quote-form-p x))
		     (t (loop for y on x
			      do (walk (car y))
				 (unless (listp (cdr y)) (walk (cdr y))))))))
      (walk e))
    found))

(defun build-machine (formals body)
  (let* ((params (formal-variables formals))
	 (*m* (make-machine)))
    (flat body :return)
    (let* ((statements (coerce (reverse (m-statements *m*)) 'vector))
	   (locals (remove-duplicates (set-difference (m-locals *m*) params)))
	   (vars (make-hash-table :test 'eq))
	   (first-def (make-hash-table :test 'eq))
	   (last-use (make-hash-table :test 'eq))
	   (m (fresh)) (entry (fresh)) (value (fresh)) (frame (fresh))
	   (n (length params)))
      (dolist (v params) (setf (gethash v vars) t (gethash v first-def) -1))
      (dolist (v locals) (setf (gethash v vars) t))
      ;; Liveness: control only jumps forward, so a variable is live at a
      ;; site if it was assigned before it and is referred to after it.
      (loop for s across statements for i from 0
	    do (let ((assigned (and (consp s) (keyword-p (car s) "SET!") (cadr s))))
		 (when (and assigned (gethash assigned vars) (not (gethash assigned first-def)))
		   (setf (gethash assigned first-def) i))
		 (dolist (v (variables-in (if assigned (caddr s) s) vars))
		   (setf (gethash v last-use) i))))
      (let ((dispatch '()) (resumes '()))
	(loop for s across statements for i from 0
	      do (let* ((site-form (cond ((and (consp s) (eq (car s) '%site)) s)
					 ((and (consp s) (keyword-p (car s) "SET!")
					       (consp (caddr s)) (eq (car (caddr s)) '%site))
					  (caddr s))))
			(site (and site-form (second site-form))))
		   (when site
		     (let* ((info (assoc site (m-sites *m*)))
			    (var (third info))
			    (live (loop for v being the hash-keys of vars
					when (and (not (eq v var))
						  (let ((d (gethash v first-def)) (u (gethash v last-use)))
						    (and d u (< d i) (> u i))))
					  collect v)))
		       (setf (second site-form)
			     `(vector ,m ,site ,n ,@live))
		       ;; Resuming the site: restore its live variables, set
		       ;; its variable to the value returned, and go on after it.
		       (let ((resume (new-label)))
			 (push (cons site resume) dispatch)
			 (setq resumes
			       (append resumes
				       `(',resume
					 ,@(loop for v in live for j from 3
						 collect `(,(sym "set!") ,v (svref ,frame ,j)))
					 ,@(when var `((,(sym "set!") ,var ,value)))
					 (%go ',(second info))))))))))
	`(,(sym "letrec")
	  ((,m (,(sym "lambda") (,entry ,value ,frame ,@params)
		((,(sym "lambda") ,locals
		  (%machine ,entry (,(sym "quote") ,dispatch)
			    ,@(when resumes `((%go ',(new-label-named "%ENTRY"))))
			    ,@resumes
			    ,@(when resumes `(',(new-label-named "%ENTRY")))
			    ,@(coerce statements 'list)))
		 ,@(mapcar (lambda (v) (declare (ignore v)) `(,(sym "quote") ,ps:false)) locals)))))
	  (,(sym "lambda") ,formals
	   (,m 0 (,(sym "quote") ,ps:false) (,(sym "quote") ,ps:false) ,@params)))))))

;;; ------------------------------------------------------------------
;;; Keeping machines small
;;;
;;; A procedure's machine is one Lisp function, its locals hoisted to its
;;; top; SBCL's analyses are superlinear in the size of a function, so a
;;; body of thousands of calls (a test suite's) won't compile.  A long
;;; sequence is cut into chunks, each called as ((lambda () ...)) and so
;;; a machine of its own.  The chunks start at the first element that may
;;; capture: the definitions before it stay where letrec* left them.

(defparameter *chunk-sites* 32
  "About how many calls that may capture a chunk of a sequence holds.")

(defun count-sites (e)
  "Calls in E (outside nested lambdas) that may capture."
  (cond ((atomic-p e) 0)
	((keyword-p (car e) "SET!") (count-sites (caddr e)))
	((keyword-p (car e) "LETREC")
	 (+ (loop for b in (cadr e) sum (count-sites (cadr b))) (count-sites (caddr e))))
	((or (keyword-p (car e) "IF") (keyword-p (car e) "BEGIN"))
	 (loop for x in (cdr e) sum (count-sites x)))
	((lambda-form-p (car e))
	 (+ (count-sites (caddr (car e))) (loop for x in (cdr e) sum (count-sites x))))
	(t (+ (if (calling-call-p (car e)) 1 0) (loop for x in (cdr e) sum (count-sites x))))))

(defun chunk-sequences (e)
  (cond ((atomic-p e)
	 (if (lambda-form-p e)
	     `(,(car e) ,(cadr e) ,(chunk-sequences (caddr e)))
	     e))
	((keyword-p (car e) "BEGIN")
	 (let* ((elements (mapcar #'chunk-sequences (cdr e)))
		(counts (mapcar #'count-sites elements)))
	   (if (<= (reduce #'+ counts) (* 2 *chunk-sites*))
	       `(,(car e) ,@elements)
	       (let ((prefix '()) (chunks '()) (chunk '()) (n 0))
		 (loop while (and elements (zerop (car counts)))
		       do (push (pop elements) prefix) (pop counts))
		 (loop for x in elements for c in counts
		       do (when (and chunk (> (+ n c) *chunk-sites*))
			    (push (reverse chunk) chunks)
			    (setq chunk '() n 0))
			  (push x chunk) (incf n c))
		 (when chunk (push (reverse chunk) chunks))
		 `(,(car e) ,@(reverse prefix)
		   ;; (lambda args ...), not (lambda () ...), which FLAT
		   ;; would take for a let and put back in place
		   ,@(mapcar (lambda (c)
			       `((,(sym "lambda") ,(fresh) (,(sym "begin") ,@c))))
			     (reverse chunks)))))))
	((quote-form-p e) e)
	(t (mapcar #'chunk-sequences e))))

(defun cc-transform (form)
  "Top-level FORM for full continuations."
  (cond ((and (consp form) (keyword-p (car form) "BEGIN") (cdr form))
	 `(,(car form) ,@(mapcar #'cc-transform (cdr form))))
	((and (consp form) (keyword-p (car form) "DEFINE"))
	 (if (simple-p (caddr form))
	     `(,(car form) ,(cadr form) ,(simple (box-assigned (chunk-sequences (caddr form)))))
	     ;; defined first, so that the assignment can be in a frame
	     `(,(sym "begin")
	       (,(car form) ,(cadr form) (,(sym "quote") ,ps:false))
	       ,(cc-transform `(,(sym "set!") ,(cadr form) ,(caddr form))))))
	(t (let ((form (box-assigned (chunk-sequences form))))
	     (if (tail-simple-p form)
		 (simple form)
		 `(,(build-machine '() form)))))))

;;; ------------------------------------------------------------------
;;; The host's side

(defparameter *full-scheme-definitions*
  "(define %full-map
     (lambda (f l . ls)
       (if (null? ls)
           (letrec ((loop (lambda (l)
                            (if (pair? l)
                                ((lambda (v) (cons v (loop (cdr l)))) (f (car l)))
                                '()))))
             (loop l))
           (letrec ((cars (lambda (ls) (if (null? ls) '() (cons (car (car ls)) (cars (cdr ls))))))
                    (cdrs (lambda (ls) (if (null? ls) '() (cons (cdr (car ls)) (cdrs (cdr ls))))))
                    (all-pairs? (lambda (ls) (if (null? ls) #t (if (pair? (car ls)) (all-pairs? (cdr ls)) #f))))
                    (loop (lambda (ls)
                            (if (all-pairs? ls)
                                ((lambda (v) (cons v (loop (cdrs ls)))) (apply f (cars ls)))
                                '()))))
             (loop (cons l ls))))))
   (define %full-for-each
     (lambda (f l . ls)
       (if (null? ls)
           (letrec ((loop (lambda (l) (if (pair? l) (begin (f (car l)) (loop (cdr l))) (if #f #f)))))
             (loop l))
           (begin (apply %full-map f l ls) (if #f #f)))))"
  "Frame-aware map and for-each, compiled with full continuations.")

(defun install-continuation-primitives ()
  (defhost "%call-with-frame" (thunk k) (call-with-frame thunk k))
  (defhost "%full-call/cc" (f) (full-call/cc f))
  (defhost "%full-dynamic-wind" (before thunk after) (full-dynamic-wind before thunk after))
  (defhost "%full-call-with-values" (producer consumer) (full-call-with-values producer consumer))
  (let ((*full-continuations* t))
    (dolist (form (read-scheme-text *full-scheme-definitions*))
      (host-eval form))))
