; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; Full continuations from generalized stack inspection (prototype)
;;;;
;;;; See docs/continuations.md.  With *FULL-CONTINUATIONS* true, each
;;;; core form psyntax produces goes through CC-TRANSFORM before the
;;;; translator:
;;;;
;;;;  - It is put in A-normal form, as far as calls that might capture a
;;;;    continuation are concerned: each such call that isn't a tail call
;;;;    becomes (%call-with-frame (lambda () call) (lambda (t . _) rest)),
;;;;    where REST is the remainder of the enclosing procedure's body.
;;;;  - %CALL-WITH-FRAME makes the call inside a HANDLER-BIND for the
;;;;    CAPTURE condition.  Capturing a continuation signals CAPTURE, and
;;;;    each handler, innermost first, records its frame (REST, a closure)
;;;;    and declines.  CL handlers run without unwinding, so capture
;;;;    costs a walk over the frames and the program carries on: unlike
;;;;    the paper's .NET version, nothing is unwound and rebuilt.
;;;;  - Invoking a continuation within the extent of its call/cc throws
;;;;    to that call/cc.  Otherwise it throws to the base, established
;;;;    around each top-level evaluation, which runs the befores of the
;;;;    continuation's dynamic-winds and rebuilds its frames, outermost
;;;;    first, each inside its handler again so that it can be captured
;;;;    again.
;;;;
;;;; Calls to primitives that don't call procedures (car, +, display)
;;;; are left alone; so is code that never captures, apart from the
;;;; handler around each of its non-tail calls.
;;;;
;;;; Known gaps of the prototype:
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

(define-condition capture (condition)
  ((frames :initform '() :accessor capture-frames)
   (complete :initform nil :accessor capture-complete))
  (:documentation "Signalled to capture the current continuation.  Each
frame's handler pushes its record onto FRAMES, so they end up outermost
first; the base's sets COMPLETE."))

(defstruct (frame (:constructor make-frame (k &optional winder)))
  k					; the rest of the frame's procedure
  winder)				; (before . after) for a dynamic-wind

(defvar *winders* '()
  "The active dynamic-winds' (before . after) pairs, innermost first.")

(defvar *base-tag* nil
  "The catch tag of the innermost base, to which a re-entered
continuation throws.")

(defmacro recording-frame ((frame) &body body)
  `(handler-bind ((capture (lambda (c)
			     (unless (capture-complete c)
			       (push ,frame (capture-frames c))))))
     ,@body))

(defun call-with-frame (thunk k)
  "Call THUNK as a call site whose continuation within its procedure is
K, recorded as a frame if a continuation is captured inside."
  (multiple-value-call k (recording-frame ((make-frame k)) (funcall thunk))))

(defun call-with-continuation-base (thunk)
  "Call THUNK as the outermost frame continuations can capture.  A
continuation re-entered inside throws back here with a thunk that
rebuilds it."
  (let* ((tag (list 'base))
	 (*base-tag* tag)
	 (*winders* '()))
    (handler-bind ((capture (lambda (c) (setf (capture-complete c) t))))
      (loop
	(setq thunk (catch tag
		      (return-from call-with-continuation-base (funcall thunk))))))))

(defun winder-extent (winder thunk)
  "Call THUNK inside dynamic-wind WINDER (whose before has run): its
after runs however THUNK is left."
  (let ((normal nil))
    (multiple-value-prog1
	(unwind-protect
	     (multiple-value-prog1
		 (let ((*winders* (cons winder *winders*)))
		   (recording-frame ((make-frame nil winder))
		     (funcall thunk)))
	       (setq normal t))
	  (unless normal (funcall (cdr winder))))
      (funcall (cdr winder)))))

(defun full-dynamic-wind (before thunk after)
  (funcall before)
  (winder-extent (cons before after) thunk))

(defun full-call-with-values (producer consumer)
  (call-with-frame producer (lambda (&rest values) (apply consumer values))))

(defun rebuild-frames (frames values)
  "Re-establish FRAMES, outermost first, and return VALUES to the
innermost."
  (if (null frames)
      (values-list values)
      (let ((frame (car frames)))
	(flet ((inner () (rebuild-frames (cdr frames) values)))
	  (if (frame-winder frame)
	      (winder-extent (frame-winder frame) #'inner)
	      (multiple-value-call (frame-k frame)
		(recording-frame (frame) (inner))))))))

(defun reenter (frames winders values)
  ;; The base has no dynamic-winds active: run the befores of the
  ;; continuation's, outermost first.
  (let ((outer '()))
    (dolist (w (reverse winders))
      (let ((*winders* outer)) (funcall (car w)))
      (push w outer)))
  (rebuild-frames frames values))

(defun full-call/cc (f)
  (let ((c (make-condition 'capture)))
    (signal c)
    (let ((frames (capture-frames c))
	  (rebuildable (capture-complete c))
	  (winders *winders*)
	  (live (list t))
	  (tag (list 'continuation)))
      (flet ((k (&rest values)
	       (cond ((car live) (throw tag (values-list values)))
		     ((and rebuildable *base-tag*)
		      (throw *base-tag* (lambda () (reenter frames winders values))))
		     (t (ps:scheme-error "continuation invoked after its extent ended, and not re-entrant (no base around its capture)")))))
	(unwind-protect (catch tag (funcall f #'k))
	  (setf (car live) nil))))))

;;; ------------------------------------------------------------------
;;; The transformation

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

(defun primitive-name (x)
  "The name of primitive X, if X refers to one: a host global with a
primitive's name (psyntax renames everything else), or (primitive x)."
  (cond ((and (consp x) (keyword-p (car x) "PRIMITIVE")) (ps:scheme-symbol-name (cadr x)))
	((and (symbolp x) x (not (eq x ps:false)) (not (keywordp x)))
	 (let ((name (ps:scheme-symbol-name x)))
	   (and (gethash name (primitive-names)) name)))))

(defun primitive-names ()
  (or *primitive-names*
      (let ((table (make-hash-table :test 'equal)))
	(dolist (s (all-primitive-names)) (setf (gethash (ps:scheme-symbol-name s) table) t))
	(dolist (p ps-r6rs:*primitives*) (setf (gethash (car p) table) t))
	(dolist (p *full-replacements*)
	  (setf (gethash (car p) table) t (gethash (cdr p) table) t))
	(setf (gethash "%call-with-frame" table) t)
	(setq *primitive-names* table))))

(defun calling-call-p (operator)
  "Whether a call with OPERATOR (normalized) may capture."
  (let ((name (primitive-name operator)))
    (or (null name) (member name *calling-primitives* :test #'string=))))

(defun fresh (&optional (hint "cc"))
  (declare (ignore hint))
  (funcall (host-ref "gensym")))

(defun lambda-form-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
(defun quote-form-p (x) (and (consp x) (keyword-p (car x) "QUOTE")))

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

(defun simple (e)
  "Simple E with its lambdas transformed and its references to the
primitives in *FULL-REPLACEMENTS* replaced."
  (cond ((quote-form-p e) e)
	((and (consp e) (keyword-p (car e) "PRIMITIVE")) (replacement e))
	((symbolp e) (replacement e))
	((atom e) e)
	((lambda-form-p e) (transform-lambda e))
	((keyword-p (car e) "LETREC")
	 `(,(car e) ,(mapcar (lambda (b) (list (car b) (simple (cadr b)))) (cadr e))
	   ,(simple (caddr e))))
	((or (keyword-p (car e) "SET!") (keyword-p (car e) "DEFINE"))
	 `(,(car e) ,(cadr e) ,(simple (caddr e))))
	(t (mapcar #'simple e))))

(defun transform-lambda (e)
  `(,(car e) ,(cadr e) ,(norm (caddr e) :tail)))

(defun finish (x k) (if (eq k :tail) x (funcall k x)))

(defun norm (e k)
  "E normalized; K is :TAIL, or a function that, given a simple
expression for E's value, returns the code that continues."
  (if (simple-p e)
      (finish (simple e) k)
      (let ((head (car e)))
	(cond ((keyword-p head "IF")
	       (norm (cadr e) (lambda (c) (norm-if c (caddr e) (cadddr e) k))))
	      ((keyword-p head "BEGIN") (norm-sequence (cdr e) k))
	      ((keyword-p head "SET!")
	       (norm (caddr e) (lambda (x) (finish `(,head ,(cadr e) ,x) k))))
	      ((keyword-p head "LETREC") (norm-letrec e k))
	      ((and (lambda-form-p head) (listp (cadr head))
		    (= (length (cadr head)) (length (cdr e))))
	       ;; a let: the body continues in place
	       (norm-list (cdr e)
			  (lambda (xs) `((,(car head) ,(cadr head) ,(norm (caddr head) k)) ,@xs))))
	      (t (norm-list e (lambda (xs) (call-site xs k))))))))

(defun call-site (xs k)
  (cond ((not (calling-call-p (car xs))) (finish xs k))
	((eq k :tail) xs)
	(t (let ((v (fresh)) (ignore (fresh)))
	     `(,(sym "%call-with-frame")
	       (,(sym "lambda") () ,xs)
	       (,(sym "lambda") (,v . ,ignore) ,(funcall k v)))))))

(defun norm-if (c then else k)
  (if (eq k :tail)
      `(,(sym "if") ,c ,(norm then :tail) ,(norm else :tail))
      ;; a join point, so that K's code isn't duplicated
      (let ((j (fresh)) (v (fresh)))
	(flet ((jump (x) `(,j ,x)))
	  `((,(sym "lambda") (,j)
	     (,(sym "if") ,c ,(norm then #'jump) ,(norm else #'jump)))
	    (,(sym "lambda") (,v) ,(funcall k v)))))))

(defun sequence-of (x rest)
  (if (atomic-p x) rest `(,(sym "begin") ,x ,rest)))

(defun norm-sequence (es k)
  (if (null (cdr es))
      (norm (car es) k)
      (norm (car es) (lambda (x) (sequence-of x (norm-sequence (cdr es) k))))))

(defun norm-list (es f)
  "Normalize ES left to right, calling F with simple expressions for
their values.  A non-atomic one is bound to a variable when one after
it may capture, so that order is kept."
  (cond ((null es) (funcall f '()))
	((every #'simple-p es) (funcall f (mapcar #'simple es)))
	(t (norm (car es)
		 (lambda (x)
		   (if (or (atomic-p x) (every #'simple-p (cdr es)))
		       (norm-list (cdr es) (lambda (xs) (funcall f (cons x xs))))
		       (let ((v (fresh)))
			 `((,(sym "lambda") (,v)
			    ,(norm-list (cdr es) (lambda (xs) (funcall f (cons v xs)))))
			   ,x))))))))

(defun norm-letrec (e k)
  (let ((bindings (cadr e)) (body (caddr e)))
    (if (every (lambda (b) (lambda-form-p (cadr b))) bindings)
	`(,(car e) ,(mapcar (lambda (b) (list (car b) (transform-lambda (cadr b)))) bindings)
	  ,(norm body k))
	(norm `((,(sym "lambda") ,(mapcar #'car bindings)
		 (,(sym "begin")
		  ,@(mapcar (lambda (b) `(,(sym "set!") ,(car b) ,(cadr b))) bindings)
		  ,body))
		,@(mapcar (lambda (b) (declare (ignore b)) `(,(sym "quote") ,ps:false)) bindings))
	      k))))

(defun cc-transform (form)
  "Top-level FORM for full continuations."
  (cond ((and (consp form) (keyword-p (car form) "BEGIN") (cdr form))
	 `(,(car form) ,@(mapcar #'cc-transform (cdr form))))
	((and (consp form) (keyword-p (car form) "DEFINE"))
	 (if (simple-p (caddr form))
	     `(,(car form) ,(cadr form) ,(simple (caddr form)))
	     ;; defined first, so that the assignment can be in a frame
	     `(,(sym "begin")
	       (,(car form) ,(cadr form) (,(sym "quote") ,ps:false))
	       ,(norm `(,(sym "set!") ,(cadr form) ,(caddr form)) :tail))))
	(t (norm form :tail))))

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
