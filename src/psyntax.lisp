; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; psyntax: the R6RS expander as Pseudoscheme's front end
;;;;
;;;; vendor/psyntax/ is Ghuloum & Dybvig's portable R6RS library and
;;;; syntax-case system (2007, MIT license; see its README.txt and
;;;; README-pseudoscheme.md).  It is written as R6RS libraries and
;;;; distributed pre-expanded into plain R5RS core forms (a ".pp" file);
;;;; a host provides a handful of primitives and an EVAL for core forms.
;;;;
;;;; Here the host is a Pseudoscheme program environment, *HOST*:
;;;;
;;;;   source --psyntax--> core Scheme --translator--> Common Lisp
;;;;
;;;; psyntax does all macro expansion (syntax-case, syntax-rules,
;;;; identifier macros, libraries, phases); the translator only ever sees
;;;; lambda/if/set!/define/quote/begin/letrec and calls.  Every R6RS
;;;; library binding that isn't a macro is a "primitive": a global
;;;; variable of *HOST* with the standard name, which is what
;;;; src/r6rs/*.lisp define.

(defpackage "PSEUDOSCHEME-PSYNTAX"
  (:nicknames "PSX")
  (:use "COMMON-LISP")
  (:export "*HOST*" "EVAL-PROGRAM" "EVAL-LIBRARY" "EVAL-TOP-LEVEL"
	   "EVAL-FORMS" "LOAD-FILE" "EXPAND" "REBUILD" "*LIBRARY-PATH*" "*SYSTEM-LIBRARY-PATH*"
	   "DEFHOST" "HOST-REF" "HOST-SET!" "MISSING-PRIMITIVES"
	   "TABLE-EXPORTS" "*LIBRARY-FORM-HOOK*" "*LIBRARY-EXTENSIONS*" "CANDIDATE-FILES" "*IMPLEMENTATION-VARIANTS*" "LOCATION" "HOST-EVAL"))

(in-package "PSEUDOSCHEME-PSYNTAX")

(defun sym (string) (psl:scheme-symbol string))

(defvar *host* nil
  "The program environment psyntax and the code it expands run in.")

(defvar *shared-primitives* '()
  "Host primitives that are R5RS's own (integrated) bindings; see MAKE-HOST.")

(defun location (name)
  "The CL symbol holding the host global NAME (a Scheme symbol)."
  (psl:tr "PROGRAM-VARIABLE-LOCATION" (psl:tr "PROGRAM-ENV-ENSURE-DEFINED" *host* name)))

(defun host-ref (name)
  (symbol-value (location (if (stringp name) (sym name) name))))

(defun host-set! (name value)
  (let* ((name (if (stringp name) (sym name) name))
	 (loc (progn
		;; A primitive shared with R5RS (see MAKE-HOST) gets its own
		;; variable before being changed, so R5RS's isn't.
		(when (member name *shared-primitives*)
		  (setq *shared-primitives* (remove name *shared-primitives*))
		  (psl:install-variable! *host* name value))
		(location name))))
    (setf (symbol-value loc) value)
    (if (functionp value)
	(ps:set-function-from-value loc)
	(unless (get loc 'ps::defined) (setf (get loc 'ps::defined) t)))
    value))

(defmacro defhost (name lambda-list &body body)
  "Define host primitive NAME (a string) as a Lisp function."
  `(host-set! ,name (lambda ,lambda-list ,@body)))

;;; ------------------------------------------------------------------
;;; The adapter: what psyntax's (psyntax system $bootstrap) and compat
;;; expect of a host (compare vendor/psyntax/scheme48.r6rs.ss).

(defun session-gensym-prefix ()
  ;; Expanded code names library globals with gensyms, and expanded
  ;; code is saved (the .pp itself, compiled libraries), so names must
  ;; not repeat across sessions: prefix with the time and a random part,
  ;; for sessions that start in the same second.
  (format nil "g$~(~36R~36R~)$" (get-universal-time)
	  (random (expt 36 4) (make-random-state t))))

(defvar *gensym-prefix* (session-gensym-prefix))
(defvar *gensym-count* 0)

;; A saved image (the command line's) is a new session each time it starts.
(uiop:register-image-restore-hook
 (lambda () (setq *gensym-prefix* (session-gensym-prefix))) nil)

(defun install-adapter ()
  (defhost "gensym" (&rest args)
    (declare (ignore args))
    (intern (format nil "~A~D" *gensym-prefix* (incf *gensym-count*)) "SCHEME"))
  (defhost "void" () ps:unspecific)
  (defhost "symbol-value" (s) (symbol-value (location s)))
  (defhost "set-symbol-value!" (s v) (host-set! s v) ps:unspecific)
  (defhost "eval-core" (x) (host-eval x))
  (defhost "lisp-keyword?" (x) (ps:true? (keywordp x)))
  ;; SRFI 4's #s16(...), SRFI 88's foo:, and records (SRFI 163's array
  ;; literals)
  (defhost "host-literal?" (x)
    (ps:true? (or (and (ps:numeric-vector-tag x) t) (ps:keyword-object-p x)
		  (typep x 'ps-r6rs::record))))
  (defhost "pretty-print" (x &optional (port *standard-output*))
    (funcall ps:*scheme-write* x port) (terpri port) ps:unspecific))

(defparameter *closed-primitives* '("assv" "for-each")
  "Primitives whose host binding is one of the translator's integrated
built-ins but whose value the R6RS/R7RS layers replaced: OPEN-PRIMITIVES
must leave them as (primitive x), or the translator would open-code the
old built-in.")

(defun open-primitives (form)
  "FORM with each (primitive x) replaced by a reference to x (see
OPEN-PRIMITIVES-1), and its internal definitions made letrecs (see
DEFINITIONS-AS-LETREC)."
  (definitions-as-letrec (open-primitives-1 form)))

(defun open-primitives-1 (form)
  "FORM with each (primitive x) -- psyntax's reference to host global
x -- replaced by the plain variable reference x, so the translator
integrates x as it does in R5RS code ((primitive +) becomes CL's +
rather than a call through a function named PRIMITIVE).  Safe because
psyntax renames every user variable: nothing in its output can shadow a
host global."
  (cond ((atom form) form)
	((and (symbolp (car form)) (string= (symbol-name (car form)) "QUOTE")) form)
	((and (symbolp (car form)) (string= (symbol-name (car form)) "PRIMITIVE")
	      (consp (cdr form)) (symbolp (cadr form)) (null (cddr form))
	      (not (member (ps:scheme-symbol-name (cadr form)) *closed-primitives* :test #'string=)))
	 (cadr form))
	;; ((primitive void)), psyntax's unspecified value: the value itself
	((and (consp (car form)) (null (cdr form))
	      (symbolp (caar form)) (string= (symbol-name (caar form)) "PRIMITIVE")
	      (consp (cdar form)) (symbolp (cadar form))
	      (string= (ps:scheme-symbol-name (cadar form)) "void"))
	 (list (sym "quote") ps:unspecific))
	((eq-membership-p form)
	 ;; (memv x '(datum ...)), as case expands: memq when eqv? is eq?
	 ;; on every datum, which the translator open-codes
	 (list (sym (if (string= (ps:scheme-symbol-name (cadr (car form))) "memv") "memq" "assq"))
	       (open-primitives-1 (cadr form))
	       (caddr form)))
	(t (let ((a (open-primitives-1 (car form))) (d (open-primitives-1 (cdr form))))
	     (if (and (eq a (car form)) (eq d (cdr form))) form (cons a d))))))

(defun eq-membership-p (form)
  "Whether FORM is ((primitive memv) x '(datum ...)) or ((primitive assv)
x '((key . value) ...)) where no datum or key is a number but a fixnum,
so that eqv? on them is eq?."
  (and (consp form) (consp (car form))
       (symbolp (caar form)) (string= (symbol-name (caar form)) "PRIMITIVE")
       (consp (cdar form)) (symbolp (cadar form))
       (member (ps:scheme-symbol-name (cadar form)) '("memv" "assv") :test #'string=)
       (= (length form) 3)
       (let ((list (caddr form)))
	 (and (consp list) (symbolp (car list)) (string= (symbol-name (car list)) "QUOTE")
	      (listp (cadr list))
	      (let ((assv (string= (ps:scheme-symbol-name (cadar form)) "assv")))
		(every (lambda (d)
			 (let ((key (if assv (if (consp d) (car d) 0) d)))
			   (not (and (numberp key) (not (typep key 'fixnum))))))
		       (cadr list)))))))

;;; psyntax expands a body's definitions (a lambda's, a library's, a
;;; program's) as letrec*:
;;;
;;;    ((lambda (f x g) (begin (set! f (lambda ...)) (set! x ...) (set! g (lambda ...)) body ...))
;;;     '#f '#f '#f)
;;;
;;; The translator makes each such variable a Lisp variable that is
;;; assigned and closed over, so SBCL boxes it, and a call to f is a
;;; FUNCALL through the box.  A variable assigned only there, to a
;;; lambda, can be bound by a letrec instead, which the translator makes
;;; a LABELS function, called directly:
;;;
;;;    ((lambda (x) (letrec ((f (lambda ...)) (g (lambda ...))) (begin (set! x ...) body ...)))
;;;     '#f)
;;;
;;; A variable assigned only once, to one of these procedures or to a
;;; primitive, (define graph-nodes internal-graph-nodes), is replaced by
;;; it, so its calls are direct too, or open-coded.
;;;
;;; This makes f and g procedures from the start rather than when their
;;; definitions are reached, which only a body that uses a variable
;;; before its definition, an error, can tell.
;;;
;;; Procedures calling each other directly are compiled together, as one
;;; Lisp code object, where through variables each is compiled on its
;;; own; past a megabyte or so of code SBCL can't compile the object
;;; (arm64's conditional branches reach 1 MB).  So the definitions of a
;;; body bigger than *LETREC-DEFINITIONS-LIMIT* conses are left as they
;;; are: a big program's top level, but not the bodies inside it.

(defparameter *letrec-definitions-limit* 20000
  "The largest body of definitions, in conses, made a letrec.")

(defun tree-size (x)
  "The conses of code X, a quoted datum counting as one (it may be
circular, R7RS 2.4)."
  (let ((n 0))
    (loop while (consp x)
	  do (if (and (symbolp (car x)) (string= (symbol-name (car x)) "QUOTE"))
		 (return (incf n 2))
		 (incf n (1+ (tree-size (car x)))))
	     (setq x (cdr x)))
    n))

(defvar *primitive-symbols* nil)

(defun primitive-symbol-p (x)
  "Whether X names a host primitive (not a variable psyntax made)."
  (unless *primitive-symbols*
    (setq *primitive-symbols* (make-hash-table :test 'eq))
    (dolist (name (all-primitive-names)) (setf (gethash name *primitive-symbols*) t)))
  (gethash x *primitive-symbols*))

(defun definitions-as-letrec (form)
  (let ((assignments (make-hash-table :test 'eq)))
    (labels ((keyword-p (x name) (and (symbolp x) (string= (symbol-name x) name)))
	     (quote-p (x) (and (consp x) (keyword-p (car x) "QUOTE")))
	     (lambda-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
	     (false-p (x) (and (quote-p x) (eq (cadr x) ps:false)))
	     (count-assignments (e)
	       (cond ((atom e))
		     ((quote-p e))
		     (t (when (and (keyword-p (car e) "SET!") (consp (cdr e)))
			  (incf (gethash (cadr e) assignments 0)))
			(loop for x on e
			      do (count-assignments (car x))
			      while (consp (cdr x))))))
	     (definition-p (s vars)
	       ;; (set! v (lambda ...)), V one of VARS assigned only here
	       (and (consp s) (keyword-p (car s) "SET!")
		    (member (cadr s) vars) (lambda-p (caddr s))
		    (= (gethash (cadr s) assignments 0) 1)))
	     (convert (e)
	       (cond ((atom e) e)
		     ((quote-p e) e)
		     (t (let ((e (map-tree e)))
			  (or (convert-body e) e)))))
	     (map-tree (e)
	       (let ((a (convert (car e)))
		     (d (if (consp (cdr e)) (map-tree (cdr e)) (cdr e))))
		 (if (and (eq a (car e)) (eq d (cdr e))) e (cons a d))))
	     (convert-body (e)
	       ;; E, the letrec* form above, converted; or NIL
	       (when (and (lambda-p (car e)) (consp (cdr (car e))) (consp (cddr (car e)))
			  (null (cdddr (car e))))
		 (let* ((vars (cadr (car e))) (body (caddr (car e))) (args (cdr e))
			(statements (if (and (consp body) (keyword-p (car body) "BEGIN"))
					(cdr body)
					(list body))))
		   (when (and (listp vars) (null (cdr (last vars)))
			      (= (length vars) (length args))
			      (every #'false-p args))
		     (let* ((definitions (remove-if-not (lambda (s) (definition-p s vars)) statements))
			    (definitions (and (<= (tree-size definitions) *letrec-definitions-limit*)
					      definitions))
			    (defined (mapcar #'cadr definitions))
			    (aliases (find-aliases statements vars defined)))
		       (when (or definitions aliases)
			 (let* ((statements (substitute-aliases
					     (remove-if (lambda (s) (and (consp s) (assoc (cadr s) aliases)
									 (keyword-p (car s) "SET!")
									 (symbolp (caddr s))))
							statements)
					     aliases))
				(definitions (remove-if-not (lambda (s) (and (consp s) (keyword-p (car s) "SET!")
									     (member (cadr s) defined)
									     (lambda-p (caddr s))))
							    statements))
				(rest (remove-if (lambda (s) (member s definitions)) statements))
				(others (remove-if (lambda (v) (or (member v defined) (assoc v aliases))) vars))
				(inner (let ((body (cond ((null rest) `(,(sym "quote") ,ps:unspecific))
							 ((null (cdr rest)) (car rest))
							 (t `(,(sym "begin") ,@rest)))))
					 (if definitions
					     `(,(sym "letrec")
					       ,(mapcar (lambda (s) (list (cadr s) (caddr s))) definitions)
					       ,body)
					     body))))
			   (if others
			       `((,(caar e) ,others ,inner) ,@(mapcar (lambda (v) (declare (ignore v)) `(,(sym "quote") ,ps:false)) others))
			       inner))))))))
	     (find-aliases (statements vars defined)
	       ;; ((v . w) ...) for each (set! v w), V one of VARS assigned
	       ;; only there and W one of DEFINED, a primitive, or another
	       ;; such V (resolved)
	       (let ((aliases '()))
		 (dolist (s statements)
		   (when (and (consp s) (keyword-p (car s) "SET!") (member (cadr s) vars)
			      (symbolp (caddr s)) (not (eq (cadr s) (caddr s)))
			      (= (gethash (cadr s) assignments 0) 1))
		     (let ((w (or (cdr (assoc (caddr s) aliases)) (caddr s))))
		       (when (or (member w defined) (primitive-symbol-p w))
			 (push (cons (cadr s) w) aliases)))))
		 aliases))
	     (substitute-aliases (x aliases)
	       (cond ((symbolp x) (let ((a (assoc x aliases))) (if a (cdr a) x)))
		     ((atom x) x)
		     ((quote-p x) x)
		     (t (let ((a (substitute-aliases (car x) aliases))
			      (d (substitute-aliases (cdr x) aliases)))
			  (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d)))))))
      (count-assignments form)
      (convert form))))

;;; A big top-level form's own definitions go further: they become
;;; top-level definitions, of the (unique) names psyntax gave them,
;;;
;;;    (begin (define x '#f) (define f (lambda ...)) (set! x ...) (define g (lambda ...)) body ...)
;;;
;;; so that SBCL compiles each procedure on its own, where as closures
;;; over one another's variables they'd be one code object, too big to
;;; compile (see above), or to compile in reasonable time and space with
;;; full continuations.  A call to one is a call through its global
;;; function cell.

(defvar *hoisted-procedures* '()
  "The variables HOIST-DEFINITIONS last made global procedures, each
defined once and never assigned (for src/continuations.lisp).")

(defun hoist-definitions (form)
  "FORM, a top-level core form, with the definitions of its outermost
body made top-level definitions; or FORM if it has no such body."
  (setq *hoisted-procedures* '())
  (labels ((keyword-p (x name) (and (symbolp x) (string= (symbol-name x) name)))
	   (lambda-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
	   (false-p (x) (and (consp x) (keyword-p (car x) "QUOTE") (eq (cadr x) ps:false)))
	   (body-form-p (e)
	     ;; ((lambda (v ...) body) '#f ...)
	     (and (consp e) (lambda-p (car e)) (consp (cdr (car e))) (consp (cddr (car e)))
		  (null (cdddr (car e)))
		  (listp (cadr (car e))) (null (cdr (last (cadr (car e)))))
		  (= (length (cadr (car e))) (length (cdr e)))
		  (every #'false-p (cdr e))))
	   (count-assignments (e table)
	     (cond ((atom e))
		   ((keyword-p (car e) "QUOTE"))
		   (t (when (and (keyword-p (car e) "SET!") (consp (cdr e)))
			(incf (gethash (cadr e) table 0)))
		      (loop for x on e do (count-assignments (car x) table) while (consp (cdr x))))))
	   (hoist (e)
	     (let* ((vars (cadr (car e))) (body (caddr (car e)))
		    (statements (if (and (consp body) (keyword-p (car body) "BEGIN")) (cdr body) (list body)))
		    (counts (make-hash-table :test 'eq)))
	       (count-assignments e counts)
	       (let ((defined (loop for s in statements
				    when (and (consp s) (keyword-p (car s) "SET!") (member (cadr s) vars)
					      (lambda-p (caddr s)) (= (gethash (cadr s) counts 0) 1))
				      collect (cadr s))))
		 (setq *hoisted-procedures* defined)
		 `(,@(loop for v in vars unless (member v defined)
			   collect `(,(sym "define") ,v (,(sym "quote") ,ps:false)))
		   ,@(loop for s in statements
			   collect (if (and (consp s) (keyword-p (car s) "SET!") (member (cadr s) defined))
				       `(,(sym "define") ,(cadr s) ,(caddr s))
				       s)))))))
    (cond ((body-form-p form) `(,(sym "begin") ,@(hoist form)))
	  ((and (consp form) (keyword-p (car form) "BEGIN") (body-form-p (car (last form))))
	   `(,@(butlast form) ,@(hoist (car (last form)))))
	  (t form))))

(defun open-top-level (form)
  "OPEN-PRIMITIVES for a form evaluated at top level, whose definitions
are hoisted if it's big."
  (let ((form (open-primitives-1 form)))
    (definitions-as-letrec
     (if (> (tree-size form) *letrec-definitions-limit*)
	 (hoist-definitions form)
	 (progn (setq *hoisted-procedures* '()) form)))))

(defvar *full-continuations*)		; src/continuations.lisp

;;; Shared compiled lambdas.  psyntax evaluates every macro transformer
;;; it meets -- a syntax-rules is (lambda (x) ...) -- and a macro that
;;; expands into local macros (let-syntax, define-syntax in a body), as
;;; CPS-style syntax-rules macros do, has a new transformer to evaluate
;;; at every step of every use: SRFI 257's tests evaluate 10,000, of
;;; only 149 shapes.  Compiling each with SBCL costs a millisecond or so.
;;; So a lambda form is compiled once per shape: with its bound
;;; variables renamed canonically and its quoted constants (syntax
;;; objects, mostly) taken out as parameters, it is the key to a
;;; compiled function of those constants, which each evaluation calls.

(defvar *shared-lambdas* (make-hash-table :test 'equal)
  "Canonical lambda form -> compiled function of its constants.")

(defparameter *shared-lambda-size-limit* 5000
  "The largest lambda form, in conses, compiled for sharing.")

(defparameter *shared-lambda-count-limit* 5000
  "How many compiled lambdas to keep; past this, the table is cleared.")

(defun canonical-variable (prefix n)
  (sym (format nil "%%shared-~A~D" prefix n)))

(defun canonical-lambda (form)
  "If core FORM is a lambda expression whose free variables are all
host primitives or psyntax's renamed globals, (key . constants): KEY the
form with its bound variables renamed canonically and each constant
that isn't a number, character or symbol replaced by a parameter, and
CONSTANTS those constants in order.  Otherwise NIL: anything not
understood is just not shared."
  (let ((renames (make-hash-table :test 'eq))
	(constants '())
	(count 0))
    (labels ((named (x name) (and (symbolp x) (string= (symbol-name x) name)))
	     (fail () (return-from canonical-lambda nil))
	     (bind (v)
	       (unless (and (symbolp v) v (not (gethash v renames))) (fail))
	       (setf (gethash v renames) (canonical-variable "v" (hash-table-count renames))))
	     (formals (f)
	       (cond ((null f) nil)
		     ((symbolp f) (bind f))
		     ((consp f) (let ((a (bind (car f)))) (cons a (formals (cdr f)))))
		     (t (fail))))
	     (body (forms)
	       (unless (listp forms) (fail))
	       (mapcar #'walk forms))
	     (constant (datum)
	       (if (or (numberp datum) (characterp datum) (symbolp datum))
		   (list (sym "quote") datum)
		   (progn (push datum constants)
			  (canonical-variable "c" (1- (incf count))))))
	     (walk (x)
	       (cond ((symbolp x)
		      (or (gethash x renames)
			  ;; a free variable: a host primitive, or one of
			  ;; psyntax's renamed globals (which are unique); or
			  ;; else not shared
			  (if (and x (or (find #\$ (symbol-name x)) (primitive-symbol-p x))) x (fail))))
		     ((atom x) (if (or (numberp x) (characterp x)) x (fail)))
		     ((not (symbolp (car x))) (body x)) ; an application
		     ((named (car x) "QUOTE")
		      (unless (and (consp (cdr x)) (null (cddr x))) (fail))
		      (constant (cadr x)))
		     ((named (car x) "PRIMITIVE") x)
		     ((named (car x) "LAMBDA")
		      (unless (consp (cdr x)) (fail))
		      (let ((f (formals (cadr x))))
			(list* (car x) f (body (cddr x)))))
		     ((named (car x) "CASE-LAMBDA")
		      (cons (car x)
			    (mapcar (lambda (clause)
				      (unless (consp clause) (fail))
				      (let ((f (formals (car clause))))
					(cons f (body (cdr clause)))))
				    (cdr x))))
		     ((or (named (car x) "LETREC") (named (car x) "LETREC*"))
		      (unless (and (consp (cdr x)) (listp (cadr x))) (fail))
		      (let ((names (mapcar (lambda (b) (if (consp b) (bind (car b)) (fail))) (cadr x))))
			(list* (car x)
			       (mapcar (lambda (name b) (list name (walk (cadr b)))) names (cadr x))
			       (body (cddr x)))))
		     ((or (named (car x) "IF") (named (car x) "BEGIN") (named (car x) "SET!"))
		      (cons (car x) (body (cdr x))))
		     ;; any other operator: a variable, if it's one of ours
		     (t (body x)))))
      (unless (and (consp form) (named (car form) "LAMBDA")
		   (<= (tree-size form) *shared-lambda-size-limit*))
	(fail))
      ;; compiled code differs by host and continuations mode
      (let ((key (list* *host* (and (boundp '*full-continuations*) *full-continuations*)
			(walk form))))
	(cons key (nreverse constants))))))

(defun host-eval (form)
  "Translate and evaluate core FORM in *HOST*; a lambda expression by a
compiled function shared with the others of its shape (see
CANONICAL-LAMBDA)."
  (let ((shared (canonical-lambda form)))
    (if (null shared)
	(host-eval-1 form)
	(destructuring-bind (key . constants) shared
	  (let ((maker (gethash key *shared-lambdas*)))
	    (unless maker
	      (when (>= (hash-table-count *shared-lambdas*) *shared-lambda-count-limit*)
		(clrhash *shared-lambdas*))
	      (setq maker
		    (setf (gethash key *shared-lambdas*)
			  (host-eval-1
			   (list (sym "lambda")
				 (loop for i below (length constants) collect (canonical-variable "c" i))
				 (cddr key))))))
	    (apply maker constants))))))

(defun eval-compiled-or-interpreted (form)
  "EVAL FORM, or, if SBCL's compiler fails on it (a procedure too big for
one code object, past arm64's branch and constant reach), evaluate it
with SBCL's interpreter: nothing of it has run when compiling fails."
  #+sbcl
  (let ((form form))
    (multiple-value-bind (values failure)
	(catch 'compile-failed
	  (handler-bind ((error (lambda (c)
				  (when (and (boundp 'sb-c::*compilation*) sb-c::*compilation*
					     (not (typep c 'sb-int:simple-program-error)))
				    (throw 'compile-failed (values nil t))))))
	    (values (multiple-value-list (eval form)) nil)))
      (if failure
	  (let ((sb-ext:*evaluator-mode* :interpret))
	    (eval form))
	  (values-list values))))
  #-sbcl (eval form))

(defun host-eval-1 (form)
  "Translate and evaluate core FORM in *HOST*.  The CL compiler's
style warnings about the generated code (an unknown arity, say) are
about psyntax's output, not the user's program, so they're muffled."
  (handler-bind ((warning #'muffle-warning))
    (if *full-continuations*
	(let ((form (cc-transform (open-top-level form))))
	  (call-with-full-policy
	   (lambda ()
	     (call-with-continuation-base
	      (lambda ()
		;; a big form is many (open-top-level): evaluated one by one,
		;; so that a continuation captured in one goes on to the rest
		(if (and (consp form) (symbolp (car form))
			 (string= (symbol-name (car form)) "BEGIN") (cdr form))
		    (eval-top-level-forms (cdr form))
		    (eval-compiled-or-interpreted (translate-core form t))))))))
	(eval-compiled-or-interpreted (translate-core (open-top-level form) t)))))

(defparameter *inline-arithmetic-limit* 50000
  "The largest core form, in conses, compiled with +, - and * inline.")

(defun translate-core (form &optional top-level)
  "Core FORM translated to Lisp.  SBCL compiles a top-level form and the
closures in it as one code object, which it can't compile past a
megabyte or so (arm64's conditional branches reach 1 MB); inline
arithmetic (SCHEME+ and so on, src/numbers.lisp) makes code bigger, so
a form bigger than *INLINE-ARITHMETIC-LIMIT* is compiled without it.  A
TOP-LEVEL begin's forms are compiled one by one: its biggest counts."
  (let ((lisp (psl:tr "TRANSLATE" form *host*))
	(size (if (and top-level (consp form) (symbolp (car form))
		       (string= (symbol-name (car form)) "BEGIN"))
		  (reduce #'max (cdr form) :key #'tree-size :initial-value 0)
		  (tree-size form))))
    (if (> size *inline-arithmetic-limit*)
	`(locally (declare (notinline ps:scheme+ ps:scheme- ps:scheme*)) ,lisp)
	lisp)))

;;; ------------------------------------------------------------------
;;; Building the host environment

(defun all-primitive-names ()
  (remove-duplicates
   (append (mapcar #'sym (loop for (nil export-string) in ps-r7rs:*standard-libraries*
			       append (ps-r7rs:split-names export-string)))
	   (psl:tr "INTERFACE-NAMES"
		   (psl:tr "STRUCTURE-INTERFACE" (psl:base-structure))))))

(defun overridden-primitive-names ()
  "Names the host's INSTALL-PRIMITIVES (re)defines."
  (mapcar (lambda (p) (sym (car p))) ps-r6rs:*primitives*))

(defun make-host (&optional (package-name *host-package-name*))
  "The host environment: a copy of the R7RS implementation env's
bindings -- except that where a binding is still R5RS's own built-in
(same value, and not redefined by the R6RS layer), the host shares the
R5RS binding itself, so the translator open-codes it ((+ a b) becomes
CL's +, (vector-ref v i) SVREF) as it does in R5RS code.  A copy would
be a fresh variable, called out of line."
  (let ((env (fresh-host-env package-name))
	(base (psl:base-structure))
	(overridden (overridden-primitive-names))
	(shared '()))
    (dolist (name (all-primitive-names))
      (when (psl:binding-defined-p ps-r7rs:*implementation-env* name)
	(let ((den (psl:tr "PROGRAM-ENV-LOOKUP" ps-r7rs:*implementation-env* name))
	      (base-den (and (member name (psl:tr "INTERFACE-NAMES" (psl:tr "STRUCTURE-INTERFACE" base)))
			     (psl:tr "STRUCTURE-REF" base name))))
	  (if (and base-den
		   (psl:variable-node-p den) (psl:variable-node-p base-den)
		   (boundp (psl:tr "PROGRAM-VARIABLE-LOCATION" den))
		   (boundp (psl:tr "PROGRAM-VARIABLE-LOCATION" base-den))
		   (eq (symbol-value (psl:tr "PROGRAM-VARIABLE-LOCATION" den))
		       (symbol-value (psl:tr "PROGRAM-VARIABLE-LOCATION" base-den)))
		   (not (member name overridden))
		   (not (member (ps:scheme-symbol-name name) *closed-primitives* :test #'string=)))
	      (progn (psl:tr "PROGRAM-ENV-DEFINE!" env name base-den) (push name shared))
	      (psl:copy-bindings! env ps-r7rs:*implementation-env* (list name))))))
    (setq *shared-primitives* shared)
    env))

(defparameter *host-package-name* "LIBRARY psyntax host"
  "The CL package of the host environment's globals.  Its name is fixed,
not numbered like other library environments', because compiled code
refers to it: the fasl of psyntax's image (see COMPILE-IMAGE).")

(defun fresh-host-env (package-name)
  "An empty program environment for a new host, in a package of its own
named PACKAGE-NAME: a previous host's package is deleted, so nothing of
it (bindings, say, from a REBUILD's seed) carries over."
  (let ((old (find-package package-name)))
    (when old (delete-package old)))
  (psl:tr "MAKE-PROGRAM-ENV" (intern package-name "SCHEME") '()))

(defun install-primitives (alist)
  "ALIST of (name-string . function), e.g. from DEFPRIM registries."
  (loop for (name . fn) in alist do (host-set! name fn)))

;;; ------------------------------------------------------------------
;;; Loading the expander

(defun vendor-file (name)
  (asdf:system-relative-pathname :pseudoscheme
				 (concatenate 'string "vendor/psyntax/" name)))

(defun read-scheme-text (string)
  (with-input-from-string (in string)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun read-file-forms (path)
  (with-open-file (in path)
    (ps:skip-script-header in)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun load-image (path &key (drop-last nil))
  "Evaluate the core forms of a psyntax .pp file in *HOST*.  (The
original pre-built images end with a form that runs a script named on
the command line and exits; DROP-LAST skips it.)"
  (let ((forms (read-file-forms path)))
    (dolist (form (if drop-last (butlast forms) forms))
      (host-eval form))))

;;; psyntax's image is compiled once, by ASDF (the PSYNTAX-IMAGE
;;; component in pseudoscheme.asd): translated to Lisp, which
;;; COMPILE-FILE compiles.  Translating the image takes a moment; it's
;;; the Lisp compiler that's slow, and BOOT would otherwise run it on
;;; every start.  The fasl is valid only for a host built the same way,
;;; with the same shared primitives (see MAKE-HOST), which it checks.

(defvar *image-fasl* nil
  "The compiled psyntax-pseudoscheme.pp, if ASDF has built it.")

(defun prepare-host (&optional (package-name *host-package-name*))
  "Create *HOST*, with everything psyntax's image expects of it."
  (setq *host* (make-host package-name))
  (setf ps-r6rs::*globals-hook* #'host-ref)
  (ps-r6rs::install-exception-hooks)
  (install-primitives ps-r6rs:*primitives*)
  (install-adapter)
  (install-continuation-primitives)
  (install-inline-primitives)
  *host*)

;;; Inline primitives.  A host primitive that isn't one of the
;;; translator's own built-ins is called out of line, through its
;;; checks.  For a few that programs call in their inner loops, a
;;; compiler macro on the host global's function inlines the case where
;;; the arguments are right, and calls the primitive (for its error)
;;; otherwise.

(defparameter *inline-primitives*
  '(("bytevector-u8-ref" (bv k)
     (if (and (typep bv '(simple-array (unsigned-byte 8) (*)))
	      (typep k 'fixnum) (< -1 k (length bv)))
	 (aref bv k)
	 :call))
    ("bytevector-u8-set!" (bv k byte)
     (if (and (typep bv '(simple-array (unsigned-byte 8) (*)))
	      (typep k 'fixnum) (< -1 k (length bv))
	      (typep byte '(unsigned-byte 8)))
	 (progn (setf (aref bv k) byte) ps:unspecific)
	 :call))
    ("bytevector-length" (bv)
     (if (typep bv '(simple-array (unsigned-byte 8) (*)))
	 (length bv)
	 :call))
    ;; two strings: CL's comparisons, which compare code points as R7RS does
    ("string=?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string= a b) t ps:false) :call))
    ("string<?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string< a b) t ps:false) :call))
    ("string>?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string> a b) t ps:false) :call))
    ("string<=?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string<= a b) t ps:false) :call))
    ("string>=?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string>= a b) t ps:false) :call)))
  "(name lambda-list body): BODY, with the arguments bound to the
variables of LAMBDA-LIST, is the inline code, in which :CALL stands for
calling the primitive itself.")

(defun install-inline-primitives ()
  (loop for (name params body) in *inline-primitives*
	for global = (location (sym name))
	do (let ((global global) (params params) (body body))
	     (setf (compiler-macro-function global)
		   (lambda (form env)
		     (declare (ignore env))
		     (let ((args (if (eq (car form) 'funcall) (cddr form) (cdr form))))
		       (if (or (eq (car form) 'funcall) (/= (length args) (length params)))
			   form	; the call in the slow path, or another arity
			   (let ((vars (mapcar (lambda (p) (gensym (symbol-name p))) params)))
			     `(let ,(mapcar #'list vars args)
				,(sublis (acons :call `(funcall #',global ,@vars)
						(mapcar #'cons params vars))
					 body))))))))))

(defun host-signature ()
  "What compiled code depends on in a host: which primitives are R5RS's
own bindings, and so open-coded."
  (sort (mapcar #'ps:scheme-symbol-name *shared-primitives*) #'string<))

(defun compile-image (image fasl)
  "Translate the psyntax IMAGE (a .pp file) to Lisp and compile it into
FASL.  The translating is done in a host of its own, under another
package name, so that a host already running in this Lisp (ASDF may
recompile the image in a live session) is left alone; the code is then
written with the real host package's name."
  (let* ((compiling (concatenate 'string *host-package-name* " (compiling)"))
	 (*host* nil)
	 (*shared-primitives* '())
	 (ps-r6rs::*globals-hook* ps-r6rs::*globals-hook*)
	 ;; Written under temporary names and renamed into place: other
	 ;; Lisp processes sharing the fasl cache may be loading the old
	 ;; fasl.
	 (temp (format nil "~A-~36R" (pathname-name fasl) (random (expt 36 8) (make-random-state t))))
	 (lisp (make-pathname :name temp :type "lisp" :defaults fasl))
	 (temp-fasl (make-pathname :name temp :defaults fasl))
	 (forms (read-file-forms image)))
    (prepare-host compiling)
    (let* ((from (find-package compiling))
	   (to (or (find-package *host-package-name*)
		   (make-package *host-package-name* :use '("COMMON-LISP"))))
	   (signature (host-signature))
	   (code (handler-bind ((warning #'muffle-warning))
		   (mapcar (lambda (form) (psl:tr "TRANSLATE" (open-primitives form) *host*))
			   forms))))
      (labels ((rename (x)
		 (cond ((and (symbolp x) (eq (symbol-package x) from))
			(intern (symbol-name x) to))
		       ((consp x)		; iterative along the list
			(let* ((head (list nil)) (tail head))
			  (loop while (consp x)
				do (setf tail (setf (cdr tail) (list (rename (pop x))))))
			  (setf (cdr tail) (rename x))
			  (cdr head)))
		       (t x))))
	(setq code (mapcar #'rename code)))
      (delete-package from)
      (ensure-directories-exist lisp)
      (with-open-file (out lisp :direction :output :if-exists :supersede)
	(with-standard-io-syntax
	  (let ((*package* (find-package "SCHEME"))
		(*print-circle* t)
		(*print-readably* t))
	    (format out ";;; ~A, translated by COMPILE-IMAGE.~%" (file-namestring image))
	    (print `(unless (equal (host-signature) ',signature)
		      (throw 'stale-image nil))
		   out)
	    (dolist (form code) (print form out))))))
    (handler-bind ((warning #'muffle-warning))
      (with-standard-io-syntax
	(let ((*package* (find-package "SCHEME")))
	  (compile-file lisp :output-file temp-fasl))))
    (delete-file lisp)
    (uiop:rename-file-overwriting-target temp-fasl fasl)
    fasl))

(defun load-compiled-image ()
  "Load *IMAGE-FASL* into *HOST*; false if there's none, if the image has
changed since (a REBUILD rewrites it), or if it was compiled for a host
that differs from this one."
  (and *image-fasl*
       (probe-file *image-fasl*)
       (>= (file-write-date *image-fasl*)
	   (or (file-write-date (vendor-file "psyntax-pseudoscheme.pp")) 0))
       (catch 'stale-image
	 (handler-bind ((warning #'muffle-warning))
	   (load *image-fasl*))
	 t)))

(defun boot (&key seed)
  "Create *HOST* and load psyntax into it: our own rebuilt image if there
is one (compiled, if ASDF has compiled it), else the original Scheme48
one (which lacks our entry points; see REBUILD).  SEED: T for the
Scheme48 image regardless, or the pathname of an image built from our
sources elsewhere (boot/ builds one with Chez Scheme)."
  (let ((*full-continuations* nil))	; booting: no Scheme procedures to speak of
    (boot-1 seed)))

(defun boot-1 (seed)
  (prepare-host)
  (let ((own (vendor-file "psyntax-pseudoscheme.pp")))
    (cond ((and seed (not (eq seed t)))
	   (load-image seed))
	  ((and (not seed) (load-compiled-image)))
	  ((and (not seed) (probe-file own))
	   (load-image own))
	  (t
	   (load-image (vendor-file "pre-built/psyntax-scheme48.pp") :drop-last t)
	   ;; The original image has none of our entry points, only its
	   ;; own script runner (the dropped last form), which calls
	   ;; EVAL-R6RS-TOP-LEVEL through this global.  That's enough to
	   ;; run the build script and get an image that has them.
	   (host-set! "psyntax:eval-r6rs-top-level" (host-ref "g$747$23171")))))
;; DELAY expands into ($delay thunk); promises are R7RS's.
  (host-eval (car (read-scheme-text
		   "(define ($delay thunk) (delay-force (make-promise (thunk))))")))
  (when (boundp (location (sym "psyntax:file-locator")))
    (funcall (host-ref "psyntax:file-locator") #'locate-library-file))
  (when (boundp (location (sym "psyntax:library-locator")))
    (funcall (host-ref "psyntax:library-locator") #'locate-library))
  (install-library-cache-hooks)
  *host*)

;;; ------------------------------------------------------------------
;;; Finding libraries in files

(defvar *library-path* (list "./")
  "Directories searched, in order, for library source files: (foo bar)
is looked for as foo/bar.sls, then .ss, .sld and .scm, in each.")

(defvar *system-library-path*
  (list (cons "srfi" (namestring (asdf:system-relative-pathname :pseudoscheme "src/srfi/"))))
  "Libraries that ship with Pseudoscheme, searched after *LIBRARY-PATH*.
An entry (PREFIX . DIR) roots names beginning with PREFIX at DIR: (srfi
1) is src/srfi/1.sld.")

(defparameter *library-extensions* '("sls" "ss" "sld" "scm"))

(defparameter *implementation-variants* '("pseudoscheme" nil "chezscheme" "ikarus")
  "Implementation-specific variants of a library file to try, in order:
foo.pseudoscheme.sls first, then the generic foo.sls (NIL), then other
systems' variants, as Akku lays them out.  Chez's is next because
nearly every Akku package has one, and src/chez/ supplies the
(chezscheme) library such variants import; Ikarus is psyntax-based.")

(defun native-path (string)
  "STRING as a pathname, with no CL wildcard syntax (* ? [ in Scheme
file names, like and-let*.sls, are just characters)."
  (uiop:parse-native-namestring string))

(defun candidate-files (stem &optional (dirs (append *library-path* *system-library-path*)))
  "Files that might hold the library whose name gives STEM (foo/bar), in
search order: each variant (see *IMPLEMENTATION-VARIANTS*) in every
directory before the next variant, so a generic file anywhere on the
path beats another system's variant.  A (PREFIX . DIR) entry of DIRS
applies only to stems under PREFIX (see *SYSTEM-LIBRARY-PATH*)."
  (let ((roots (loop for d in (if (listp dirs) dirs (list dirs))
		     for (prefix . dir) = (if (consp d) d (cons nil d))
		     for p = (and prefix (concatenate 'string prefix "/"))
		     when (or (null p) (and (> (length stem) (length p))
					    (string= p stem :end2 (length p))))
		       collect (cons (namestring (uiop:ensure-directory-pathname dir))
				     (if p (subseq stem (length p)) stem)))))
    (loop for variant in *implementation-variants*
	  append (loop for (dir . stem) in roots
		       append (if variant
				  (list (native-path (format nil "~A~A.~A.sls" dir stem variant)))
				  (loop for ext in *library-extensions*
					collect (native-path (format nil "~A~A.~A" dir stem ext))))))))

(defun library-name-file-stem (name)
  "(foo bar (1)) -> \"foo/bar\": the identifiers of a library name,
version dropped."
  (format nil "~{~A~^/~}"
	  (loop for part in name
		while (symbolp part)
		collect (ps:scheme-symbol-name part))))

(defvar *library-form-hook* nil
  "If set, a function from a library name to its (R6RS library) form or
NIL; used before the plain file search, e.g. to translate R7RS
define-library forms (src/r7rs/front.lisp).")

(defvar *pending-libraries*)		; src/library-cache.lisp

(defun locate-library (name)
  "psyntax's LIBRARY-LOCATOR: the form defining library NAME, or #f."
  (or (let ((pending (assoc name *pending-libraries* :test #'equal)))
	;; just read by LOAD-COMPILED-LIBRARY, which found no compiled one
	(and pending (fourth pending)
	     (prog1 (fourth pending) (setf (fourth pending) nil))))
      (and *library-form-hook* (funcall *library-form-hook* name))
      ;; the file's first datum, if it is a library form (a file that
      ;; begins otherwise may define its library at top level, as Chez
      ;; allows: *LIBRARY-LOADERS*)
      (let ((file (locate-library-file name)))
	(and (stringp file)
	     (let ((form (with-open-file (in file) (funcall ps:*scheme-read* in))))
	       (and (psl:keyword-head-p form "library") form))))
      ps:false))

(defun locate-library-file (name)
  "psyntax's FILE-LOCATOR: a file name for library NAME, or #f."
  (let ((stem (library-name-file-stem name)))
    (or (loop for path in (candidate-files stem)
	      when (probe-file path) return (namestring path))
	ps:false)))

;;; ------------------------------------------------------------------
;;; Entry points

(defun entry (name)
  (let ((loc (location (sym name))))
    (unless (boundp loc)
      (error "psyntax entry point ~A is missing: the loaded image predates ~
              vendor/psyntax/psyntax/main.ss -- run (psx:rebuild)" name))
    (symbol-value loc)))


(defun eval-program (forms)
  "Run an R6RS top-level program: FORMS begin with (import ...)."
  (funcall (entry "psyntax:eval-r6rs-top-level") forms))

(defun eval-library (form)
  "Expand, install and (lazily) instantiate an R6RS (library ...) form."
  (funcall (entry "psyntax:library-expander") form))

(defun eval-top-level (form)
  "Evaluate FORM at the REPL, in (pseudoscheme interaction)."
  (funcall (entry "psyntax:eval-top-level") form))

(defun expand (form &optional (env (funcall (entry "psyntax:environment")
					     (list (mapcar #'sym '("rnrs"))))))
  (funcall (entry "psyntax:expand") form env))

(defun eval-forms (forms)
  "Evaluate a file's worth of forms: any (library ...) forms are
installed, and what follows them, if anything, is run as a program."
  (let ((result ps:unspecific))
    (loop while (and forms (psl:keyword-head-p (car forms) "library"))
	  do (eval-library (pop forms)))
    (when forms
      (setq result (eval-program forms)))
    result))

(defun load-file (path)
  (eval-forms (read-file-forms path)))



;;; ------------------------------------------------------------------
;;; Rebuilding the expander image on Pseudoscheme itself

(defun rebuild (&key seed (directory (vendor-file "")))
  "Run psyntax-buildscript.ss -- the expander expanding its own sources --
writing psyntax-pseudoscheme.pp.  SEED is as for BOOT.  DIRECTORY holds
the build script and psyntax/ and receives the image; by default
vendor/psyntax/, else (as for boot/) a copy of it.  Escape-only: the
build is a program of no continuations to speak of, and big."
  (let ((*full-continuations* nil))
    (rebuild-1 seed directory)))

(defun rebuild-1 (seed directory)
  (boot :seed seed)
  (let* ((directory (truename directory))
	 (*default-pathname-defaults* directory)
	 (start (get-internal-real-time)))
    (eval-program (read-file-forms (merge-pathnames "psyntax-buildscript.ss" directory)))
    (canonicalize-gensyms (merge-pathnames "psyntax-pseudoscheme.pp" directory))
    (format t "~&Rebuilt psyntax in ~,1Fs~%"
	    (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
  (boot))

(defun canonicalize-gensyms (image)
  "Rename the gensyms in IMAGE, which carry this session's prefix (see
*GENSYM-PREFIX*), to g$1, g$2 ... in order of appearance, so that the
image depends only on the sources and the image that built it.  (The
canonical names can't clash with those of a later session.)"
  (let* ((lines (with-open-file (in image)
		  (loop for line = (read-line in nil) while line collect line)))
	 (header (loop for line in lines
		       while (and (plusp (length line)) (char= (char line 0) #\;))
		       collect line))
	 (forms (read-file-forms image))
	 (names (make-hash-table :test #'eq))
	 (count 0))
    (labels ((rename (x)
	       (cond ((consp x)
		      (let ((a (rename (car x))) (d (rename (cdr x))))
			(if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
		     ((vectorp x) (if (stringp x) x (map 'vector #'rename x)))
		     ((and (symbolp x)
			   (eq (symbol-package x) (find-package "SCHEME"))
			   (let ((name (symbol-name x)))
			     (and (> (length name) (length *gensym-prefix*))
				  (string= *gensym-prefix* name :end2 (length *gensym-prefix*)))))
		      ;; Named as the gensym host primitive names them.
		      (or (gethash x names)
			  (setf (gethash x names)
				(intern (format nil "g$~D" (incf count)) "SCHEME"))))
		     (t x))))
      (let ((forms (mapcar #'rename forms)))
	(with-open-file (out image :direction :output :if-exists :supersede)
	  (dolist (line header) (write-line line out))
	  (terpri out)
	  (dolist (form forms)
	    (funcall ps:*scheme-write* form out)
	    (format out "~%~%~%")))))))

;;; ------------------------------------------------------------------
;;; psyntax's identifier table

(defun table-exports (keys)
  "Names (strings) the build script's identifier->library-map sends to
any of the library KEYS (strings: \"r\" for (rnrs), \"r5\", ...)."
  (let* ((forms (read-file-forms (vendor-file "psyntax-buildscript.ss")))
	 (def (find-if (lambda (f)
			 (and (consp f) (consp (cdr f)) (symbolp (cadr f))
			      (string= (ps:scheme-symbol-name (cadr f)) "identifier->library-map")))
		       forms)))
    (loop for (name . libs) in (cadr (caddr def))
	  when (some (lambda (k) (member (ps:scheme-symbol-name k) keys :test #'string=)) libs)
	    collect (ps:scheme-symbol-name name))))

;;; ------------------------------------------------------------------
;;; Diagnostics

(defun missing-primitives ()
  "Names exported by some psyntax library that have no host binding."
  (let ((pslib (funcall (entry "psyntax:environment") (list (list (sym "pseudoscheme"))))))
    (declare (ignore pslib))
    nil))
