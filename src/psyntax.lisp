; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-PSYNTAX -*-

;;;; psyntax: the R6RS expander as Quasischeme's front end
;;;;
;;;; vendor/psyntax/ is Ghuloum & Dybvig's portable R6RS library and
;;;; syntax-case system (2007, MIT license; see its README.txt and
;;;; README-quasischeme.md).  It is written as R6RS libraries and
;;;; distributed pre-expanded into plain R5RS core forms (a ".pp" file);
;;;; a host provides a handful of primitives and an EVAL for core forms.
;;;;
;;;; Here the host is a Quasischeme program environment, *HOST*:
;;;;
;;;;   source --psyntax--> core Scheme --translator--> Common Lisp
;;;;
;;;; psyntax does all macro expansion (syntax-case, syntax-rules,
;;;; identifier macros, libraries, phases); the translator only ever sees
;;;; lambda/if/set!/define/quote/begin/letrec and calls.  Every R6RS
;;;; library binding that isn't a macro is a "primitive": a global
;;;; variable of *HOST* with the standard name, which is what
;;;; src/r6rs/*.lisp define.

(defpackage "QUASISCHEME-PSYNTAX"
  (:nicknames "QSX")
  (:use "COMMON-LISP")
  (:export "*HOST*" "EVAL-PROGRAM" "EVAL-LIBRARY" "EVAL-TOP-LEVEL"
	   "EVAL-FORMS" "LOAD-FILE" "EXPAND" "REBUILD" "*LIBRARY-PATH*" "*SYSTEM-LIBRARY-PATH*"
	   "DEFHOST" "HOST-REF" "HOST-SET!" "MISSING-PRIMITIVES"
	   "TABLE-EXPORTS" "*LIBRARY-FORM-HOOK*" "*LIBRARY-EXTENSIONS*" "CANDIDATE-FILES" "*IMPLEMENTATION-VARIANTS*" "LOCATION" "HOST-EVAL"))

(in-package "QUASISCHEME-PSYNTAX")

(defun sym (string) (qsl:scheme-symbol string))

(defvar *optimize-off* nil
  "True to skip OPTIMIZE-CORE (src/cp0.lisp): compiling psyntax's own image.")

(defvar *host* nil
  "The program environment psyntax and the code it expands run in.")

(defvar *shared-primitives* '()
  "Host primitives that are R5RS's own (integrated) bindings; see MAKE-HOST.")

(defun location (name)
  "The CL symbol holding the host global NAME (a Scheme symbol)."
  (qsl:tr "PROGRAM-VARIABLE-LOCATION" (qsl:tr "PROGRAM-ENV-ENSURE-DEFINED" *host* name)))

(defun make-lexical (name)
  "A lexical variable for core code, named NAME: uninterned, so that it
is garbage once the code is compiled.  Interned, the names compilers make
for their lexicals (Guile mode's ~N.tag, --racket's) pile up in SCHEME for
as long as the process runs, as no package lets go of a symbol."
  (let ((symbol (make-symbol name)))
    (setf (get symbol 'scheme-lexical) t)
    symbol))

(defun host-ref (name)
  (symbol-value (location (if (stringp name) (sym name) name))))

(defun host-set! (name value)
  (let* ((name (if (stringp name) (sym name) name))
	 (loc (progn
		;; A primitive shared with R5RS (see MAKE-HOST) gets its own
		;; variable before being changed, so R5RS's isn't.
		(when (member name *shared-primitives*)
		  (setq *shared-primitives* (remove name *shared-primitives*))
		  (define-builtin-alias *host* name (qsl:tr "PROGRAM-ENV-LOOKUP" *host* name))
		  (qsl:install-variable! *host* name value))
		(location name))))
    (setf (symbol-value loc) value)
    (if (functionp value)
	(qs:set-function-from-value loc)
	(unless (get loc 'qs::defined) (setf (get loc 'qs::defined) t)))
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

(defun strip-annotations (x)
  "X with each annotation in it replaced by its stripped datum."
  (if (not qs::*annotations-made*)
      x
      (let ((seen (make-hash-table :test 'eq)))
	(labels ((walk (x)
		   (cond ((qs::annotation-p x) (qs::annotation-stripped x))
			 ((gethash x seen) x)
			 ((consp x)
			  (setf (gethash x seen) t)
			  (let ((a (walk (car x))) (d (walk (cdr x))))
			    (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
			 ((simple-vector-p x)
			  (setf (gethash x seen) t)
			  (let ((new (map 'simple-vector #'walk x)))
			    (if (every #'eq new x) x new)))
			 (t x))))
	  (walk x)))))

(defun install-adapter ()
  (defhost "gensym" (&rest args)
    ;; the source name, if one is given, is kept for messages about an
    ;; unbound variable (src/r6rs/conditions.lisp)
    (let ((symbol (intern (format nil "~A~D" *gensym-prefix* (incf *gensym-count*)) "SCHEME")))
      (when (and args (symbolp (first args)))
	(setf (get symbol :source-name) (qs:scheme-symbol-name (first args))))
      symbol))
  (defhost "void" () qs:unspecific)
  (defhost "%letrec-check" (flag value name)
    (if (eq flag qs:false)
	(qs-r6rs::r6rs-assertion-violation
	 qs:false (format nil "attempt to reference undefined variable ~A"
			  (or (get name :source-name) (qs:scheme-symbol-name name))))
	value))
  (defhost "symbol-value" (s) (symbol-value (location s)))
  (defhost "set-symbol-value!" (s v) (host-set! s v) qs:unspecific)
  ;; the expanded code runs without the expander lock (CALL-WITH-EXPANDER-LOCK)
  (defhost "eval-core" (x) (call-without-expander-lock (lambda () (host-eval x))))
  (defhost "lisp-keyword?" (x) (qs:true? (keywordp x)))
  ;; SRFI 4's #s16(...), SRFI 88's foo:, and records (SRFI 163's array
  ;; literals)
  ;; Chez's source annotations, which psyntax looks through
  (defhost "annotation?" (x) (qs:true? (qs::annotation-p x)))
  (defhost "annotation-expression" (x) (qs::annotation-expression x))
  (defhost "annotation-stripped" (x) (qs::annotation-stripped x))
  (defhost "strip-annotations" (x) (strip-annotations x))
  (defhost "host-literal?" (x)
    ;; Chez's #!eof too: the end-of-file object is self-evaluating there
    (qs:true? (or (and (qs:numeric-vector-tag x) t) (qs:keyword-object-p x)
		  (typep x 'qs-r6rs::record) (eq x qs:eof-object))))
  (defhost "pretty-print" (x &optional (port *standard-output*))
    (funcall qs:*scheme-write* x port) (terpri port) qs:unspecific))

(defparameter *closed-primitives* '("assv" "for-each")
  "Primitives whose host binding is one of the translator's integrated
built-ins but whose value the R6RS/R7RS layers replaced: OPEN-PRIMITIVES
must leave them as (primitive x), or the translator would open-code the
old built-in.")

(defparameter *chez-closed-primitives* '("map")
  "Primitives closed in the Chez mode only: map, whose arguments Chez
checks (QS-R6RS::CHECK-MAP-ARGUMENTS).")

(defvar *chez-renamed-primitives* (make-hash-table :test 'equal)
  "In the Chez mode, primitive name -> the host global that stands for it
there (src/chez/chez.lisp): Chez's checking mutators and comparisons,
and constructors that give the single empty object.  A reference to the
primitive is one to that global, so calls are direct, and inlined where
*INLINE-PRIMITIVES* says.  An entry (name . :nullary) renames only
calls with no arguments.")

(defun proper-length (l)
  "L's length if it's a proper list, else NIL."
  (loop for n from 0 for x = l then (cdr x)
	do (cond ((null x) (return n)) ((atom x) (return nil)))))

(defun chez-renamed-primitive (name nargs)
  "The host global standing for primitive NAME called with NARGS
arguments (NIL: not called) in the Chez mode, or NIL."
  (when qs:*chez-reader*
    (let ((r (gethash name *chez-renamed-primitives*)))
      (cond ((null r) nil)
	    ((consp r) (and (eql nargs 0) (car r)))
	    (t r)))))

(defun closed-primitive-p (name)
  "Whether primitive NAME (a string) is closed (*CLOSED-PRIMITIVES*,
and in the Chez mode *CHEZ-CLOSED-PRIMITIVES*)."
  (or (member name *closed-primitives* :test #'string=)
      (and qs:*chez-reader* (member name *chez-closed-primitives* :test #'string=))))

(defparameter *extended-primitives* '()
  "(name min max): closed primitives (see *CLOSED-PRIMITIVES*) whose new
value is an extension of the built-in that agrees with it when called
with MIN to MAX (NIL: any number of) arguments, as Chez's variadic fx+
does.  OPEN-PRIMITIVES still opens such calls, so they stay open-coded.")

(defun builtin-alias-name (name)
  (sym (concatenate 'string "%builtin:" (if (stringp name) name (qs:scheme-symbol-name name)))))

(defun define-builtin-alias (env name den)
  "Define %builtin:NAME in ENV as DEN, R5RS's built-in NAME, which the
translator open-codes: the host's NAME is another procedure now (an
extension of it), whose calls with the arguments the built-in takes
(*EXTENDED-PRIMITIVES*) go to the built-in still."
  (qsl:tr "PROGRAM-ENV-DEFINE!" env (builtin-alias-name name) den))

(defun builtin-alias (name)
  "%builtin:NAME if the host has it, else NAME."
  (let ((alias (builtin-alias-name name)))
    (if (qsl:binding-defined-p *host* alias) alias name)))

(defun extended-primitive-name (f)
  "The host global F, (primitive x) or psyntax's plain x, names."
  (cond ((and (consp f) (symbolp (car f)) (string= (symbol-name (car f)) "PRIMITIVE")
	      (consp (cdr f)) (symbolp (cadr f)) (null (cddr f)))
	 (cadr f))
	((and f (symbolp f)) f)))

(defun open-call-of-extended-primitive-p (form)
  (let ((f (extended-primitive-name (car form))))
    (and f
	 (let ((entry (assoc (qs:scheme-symbol-name f) *extended-primitives* :test #'string=))
	       (n (proper-length (cdr form))))
	   (and entry n (<= (second entry) n) (or (null (third entry)) (<= n (third entry))))))))

(defvar *canonical-empty* nil
  "In the Chez mode, a function of an empty vector, string or bytevector:
the single one of its kind (src/chez/host.lisp).")

(defun open-primitives (form)
  "FORM with each (primitive x) replaced by a reference to x (see
OPEN-PRIMITIVES-1), and its internal definitions made letrecs (see
DEFINITIONS-AS-LETREC)."
  (optimize-core (definitions-as-letrec (open-primitives-1 form))))

(defun open-primitives-1 (form)
  "FORM with each (primitive x) -- psyntax's reference to host global
x -- replaced by the plain variable reference x, so the translator
integrates x as it does in R5RS code ((primitive +) becomes CL's +
rather than a call through a function named PRIMITIVE).  Safe because
psyntax renames every user variable: nothing in its output can shadow a
host global."
  (cond ((atom form)
	 ;; a host global by name, as at Chez's top level
	 (let ((r (and qs:*chez-reader* (symbolp form) form (chez-renamed-primitive (qs:scheme-symbol-name form) nil))))
	   (if r (sym r) form)))
	((and qs:*chez-reader* (symbolp (car form)) (car form) (proper-length (cdr form))
	      (chez-renamed-primitive (qs:scheme-symbol-name (car form)) (proper-length (cdr form))))
	 (cons (sym (chez-renamed-primitive (qs:scheme-symbol-name (car form)) (proper-length (cdr form))))
	       (mapcar #'open-primitives-1 (cdr form))))
	((and (symbolp (car form)) (string= (symbol-name (car form)) "QUOTE"))
	 ;; '#() and "" are Chez's single empty vector and string
	 (if (and qs:*chez-reader* *canonical-empty* (consp (cdr form))
		  (typep (cadr form) '(simple-array * (0))))
	     (list (car form) (funcall *canonical-empty* (cadr form)))
	     form))
	((and (consp (car form)) (symbolp (caar form)) (string= (symbol-name (caar form)) "PRIMITIVE")
	      (consp (cdar form)) (symbolp (cadar form)) qs:*chez-reader* (proper-length (cdr form))
	      (chez-renamed-primitive (qs:scheme-symbol-name (cadar form)) (proper-length (cdr form))))
	 (cons (sym (chez-renamed-primitive (qs:scheme-symbol-name (cadar form)) (proper-length (cdr form))))
	       (mapcar #'open-primitives-1 (cdr form))))
	((open-call-of-extended-primitive-p form)
	 (cons (builtin-alias (extended-primitive-name (car form))) (mapcar #'open-primitives-1 (cdr form))))
	((and (symbolp (car form)) (string= (symbol-name (car form)) "PRIMITIVE")
	      (consp (cdr form)) (symbolp (cadr form)) (null (cddr form))
	      (chez-renamed-primitive (qs:scheme-symbol-name (cadr form)) nil))
	 (sym (chez-renamed-primitive (qs:scheme-symbol-name (cadr form)) nil)))
	((and (symbolp (car form)) (string= (symbol-name (car form)) "PRIMITIVE")
	      (consp (cdr form)) (symbolp (cadr form)) (null (cddr form))
	      (not (closed-primitive-p (qs:scheme-symbol-name (cadr form)))))
	 (cadr form))
	;; ((primitive void)), psyntax's unspecified value: the value itself
	((and (consp (car form)) (null (cdr form))
	      (symbolp (caar form)) (string= (symbol-name (caar form)) "PRIMITIVE")
	      (consp (cdar form)) (symbolp (cadar form))
	      (string= (qs:scheme-symbol-name (cadar form)) "void"))
	 (list (sym "quote") qs:unspecific))
	((eq-membership-p form)
	 ;; (memv x '(datum ...)), as case expands: memq when eqv? is eq?
	 ;; on every datum, which the translator open-codes
	 (list (sym (if (string= (qs:scheme-symbol-name (cadr (car form))) "memv") "memq" "assq"))
	       (open-primitives-1 (cadr form))
	       (caddr form)))
	(t (let ((a (open-primitives-1 (car form))) (d (open-primitives-list (cdr form))))
	     (if (and (eq a (car form)) (eq d (cdr form))) form (cons a d))))))

(defun open-primitives-list (l)
  "OPEN-PRIMITIVES-1 of each element of L (a form's arguments, say)."
  (if (consp l)
      (let ((a (open-primitives-1 (car l))) (d (open-primitives-list (cdr l))))
	(if (and (eq a (car l)) (eq d (cdr l))) l (cons a d)))
      (open-primitives-1 l)))

(defun eq-membership-p (form)
  "Whether FORM is ((primitive memv) x '(datum ...)) or ((primitive assv)
x '((key . value) ...)) where no datum or key is a number but a fixnum,
so that eqv? on them is eq?."
  (and (consp form) (consp (car form))
       (symbolp (caar form)) (string= (symbol-name (caar form)) "PRIMITIVE")
       (consp (cdar form)) (symbolp (cadar form))
       (member (qs:scheme-symbol-name (cadar form)) '("memv" "assv") :test #'string=)
       (= (length form) 3)
       (let ((list (caddr form)))
	 (and (consp list) (symbolp (car list)) (string= (symbol-name (car list)) "QUOTE")
	      (listp (cadr list))
	      (let ((assv (string= (qs:scheme-symbol-name (cadar form)) "assv")))
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

;;; R6RS's restriction on letrec and letrec* (11.4.6): it must be
;;; possible to evaluate each init without referring to or assigning a
;;; variable whose init hasn't been evaluated -- for letrec, any of its
;;; variables -- and an implementation must detect a violation.  The
;;; translator checks a letrec variable an init refers to directly
;;; (src/generate.scm); LETREC-CHECKS, before DEFINITIONS-AS-LETREC, the
;;; rest:
;;;
;;;  - A letrec*, psyntax's body of definitions ((lambda (v ...) (begin
;;;    (set! v init) ... body ...)) '#f ...): the body's own procedures
;;;    can run before V is defined only from a statement before V's
;;;    definition (or V's init) that refers to one of the body's
;;;    variables and isn't a lambda definition.  Such a statement may
;;;    call the procedures it mentions, the ones they mention, and so on;
;;;    a reference or assignment in it or in one of those to a variable
;;;    not yet defined when it runs checks a flag set just after that
;;;    variable's definition.  Only there: a check makes a call through
;;;    the variable, not a direct one, unless the check is beside the
;;;    call ((begin check (v arg ...))), which is how a call is checked.
;;;    A body of procedure definitions and then expressions, the usual
;;;    kind, gets no checks.  A statement that is only a reference calls
;;;    nothing; one whose value is a call of a procedure doesn't call the
;;;    lambdas that procedure returns, which are the value, but a later
;;;    statement calling the variable does: it reaches them, as the
;;;    procedure's body (so (define red-edges (make-edge-getter ...))
;;;    doesn't put checks in every edge getter).
;;;
;;;  - A letrec with an init that runs code (not a lambda, constant or
;;;    variable) or is one of its variables: a reference to one of its
;;;    variables in an init, in a procedure there included, checks a flag
;;;    set once all of them have been evaluated.

(defun letrec-checks (form)
  (labels ((keyword-p (x name) (and (symbolp x) (string= (symbol-name x) name)))
	   (quote-p (x) (and (consp x) (keyword-p (car x) "QUOTE")))
	   (lambda-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
	   (false-p (x) (and (quote-p x) (eq (cadr x) qs:false)))
	   (set-p (s vars) (and (consp s) (keyword-p (car s) "SET!") (consp (cdr s)) (consp (cddr s))
				(member (cadr s) vars)))
	   (flag-for (v) (sym (format nil "~A%defined" (qs:scheme-symbol-name v))))
	   (check (flag v) `(,(sym "%letrec-check") ,flag ,v (,(sym "quote") ,v)))
	   ;; a check before a call or assignment of V, which needs no value
	   ;; of V: one that mentioned V would let V escape, so that the
	   ;; continuation transformation couldn't know V's calls
	   (check-only (flag v) `(,(sym "%letrec-check") ,flag (,(sym "quote") ,qs:false) (,(sym "quote") ,v)))
	   (mentioned (e table)
	     ;; the variables of TABLE (a hash set) E mentions
	     (let ((found '()))
	       (labels ((scan (e)
			  (cond ((symbolp e) (when (gethash e table) (pushnew e found)))
				((atom e))
				((quote-p e))
				(t (loop for x on e
					 do (scan (car x))
					 while (consp (cdr x)))))))
		 (scan e))
	       found))
	   (mentioned-but-returned-lambdas (lam table)
	     ;; the variables of TABLE that lambda LAM's body mentions,
	     ;; except in the lambdas it returns (in tail position)
	     (labels ((tail (e)
			(cond ((lambda-p e) '())
			      ((and (consp e) (keyword-p (car e) "IF") (consp (cdr e)) (consp (cddr e)))
			       (union (mentioned (cadr e) table)
				      (union (tail (caddr e)) (if (cdddr e) (tail (cadddr e)) '()))))
			      ((and (consp e) (keyword-p (car e) "BEGIN") (cdr e))
			       (union (mentioned (butlast (cdr e)) table) (tail (car (last e)))))
			      (t (mentioned e table)))))
	       (tail (caddr lam))))
	   (guard-references (e flags)
	     ;; E with each reference to and assignment of a variable of
	     ;; FLAGS, ((v . flag) ...), checking its flag; a call of one
	     ;; checks before the call, so that it stays a call of the
	     ;; variable (a direct call, if it's a known procedure)
	     (cond ((symbolp e) (let ((f (cdr (assoc e flags)))) (if f (check f e) e)))
		   ((atom e) e)
		   ((quote-p e) e)
		   ((and (symbolp (car e)) (assoc (car e) flags))
		    `(,(sym "begin") ,(check-only (cdr (assoc (car e) flags)) (car e))
		      (,(car e) ,@(guard-list (cdr e) flags))))
		   ((and (keyword-p (car e) "SET!") (consp (cdr e)) (assoc (cadr e) flags))
		    `(,(sym "begin") ,(check-only (cdr (assoc (cadr e) flags)) (cadr e))
		      (,(car e) ,(cadr e) ,@(guard-list (cddr e) flags))))
		   ((and (keyword-p (car e) "LAMBDA") (consp (cdr e)))
		    `(,(car e) ,(cadr e) ,@(guard-list (cddr e) flags)))
		   (t (guard-list e flags))))
	   (guard-list (l flags)
	     (if (consp l)
		 (let ((a (guard-references (car l) flags)) (d (guard-list (cdr l) flags)))
		   (if (and (eq a (car l)) (eq d (cdr l))) l (cons a d)))
		 l))
	   (walk (e)
	     (cond ((atom e) e)
		   ((quote-p e) e)
		   (t (let ((e (walk-list e)))
			(or (letrec*-checked e) (letrec-checked e) e)))))
	   (walk-list (l)
	     (if (consp l)
		 (let ((a (walk (car l))) (d (walk-list (cdr l))))
		   (if (and (eq a (car l)) (eq d (cdr l))) l (cons a d)))
		 l))
	   (letrec*-checked (e)
	     (when (and (lambda-p (car e)) (consp (cdr (car e))) (consp (cddr (car e)))
			(null (cdddr (car e))))
	       (let* ((vars (cadr (car e))) (body (caddr (car e))) (args (cdr e))
		      (statements (if (and (consp body) (keyword-p (car body) "BEGIN"))
				      (cdr body)
				      (list body))))
		 (when (and (listp vars) vars (null (cdr (last vars)))
			    (= (length vars) (length args))
			    (every #'false-p args))
		   (let ((table (make-hash-table :test 'eq))
			 (index (make-hash-table :test 'eq))	; v -> where it's defined
			 (lambdas (make-hash-table :test 'eq))	; v -> its lambda, if it's defined by one
			 (made-by (make-hash-table :test 'eq))	; v -> p, if v is defined as (p arg ...), p a lambda's
			 (danger (make-hash-table :test 'eql)))	; procedure v or statement j -> vars to check there
		     (dolist (v vars) (setf (gethash v table) t))
		     (loop for s in statements for j from 0
			   do (when (and (set-p s vars) (not (gethash (cadr s) index)))
				(setf (gethash (cadr s) index) j)
				(when (lambda-p (caddr s)) (setf (gethash (cadr s) lambdas) (caddr s)))))
		     ;; a variable whose value a procedure made: the lambdas it
		     ;; returned are that value, and run when the variable is called
		     (loop for s in statements for j from 0
			   do (when (and (set-p s vars) (eql (gethash (cadr s) index) j)
					 (consp (caddr s)) (symbolp (car (caddr s)))
					 (gethash (car (caddr s)) lambdas))
				(setf (gethash (cadr s) made-by) (car (caddr s)))))
		     (flet ((undefined-at (v j)
			      ;; is V not yet defined while statement J (not a
			      ;; lambda definition) runs?
			      (let ((i (gethash v index)))
				(and i (>= i j)))))
		       ;; A statement that isn't a lambda definition runs its
		       ;; code, and may call the body's procedures it mentions,
		       ;; and those they mention, and so on: a reference in any
		       ;; of them to a variable not yet defined then is checked.
		       (loop for s in statements for j from 0
			     do (let* ((definition (and (set-p s vars) (eql (gethash (cadr s) index) j) (cadr s)))
				       (code (if definition (caddr s) s))
				       (refs (mentioned code table)))
				  (unless (or (and definition (lambda-p code)) (null refs))
				    (let ((here (remove-if-not (lambda (v) (undefined-at v j))
							       (if (and (set-p s vars) (not definition))
								   (cons (cadr s) refs)
								   refs))))
				      (when here (setf (gethash j danger) here)))
				    ;; the procedures it can reach: none if it's only a
				    ;; reference ((define x y) calls nothing).  If its
				    ;; value is a call of a procedure, the lambdas that
				    ;; procedure returns are the value, not called
				    ;; (make-edge-getter in conform's
				    ;; (define red-edges (make-edge-getter ...))).
				    (let* ((value-call (and definition (consp code) (symbolp (car code))
							    (gethash (car code) lambdas)
							    ;; and nowhere else in the statement
							    (not (member (car code) (mentioned (cdr code) table)))
							    (car code)))
					   (seen '())
					   (stack (if (symbolp code) '() (copy-list refs))))
				      (loop while stack
					    do (let* ((p (pop stack))
						      ;; calling a variable a procedure made calls
						      ;; the lambdas that procedure returned: in
						      ;; its body, which is checked if need be
						      (maker (and (not (gethash p lambdas)) (gethash p made-by))))
						 (unless (or (member p seen) (not (or (gethash p lambdas) maker)))
						   (push p seen)
						   (let* ((proc (or maker p))
							  (body-refs (if (eq p value-call)
									 (mentioned-but-returned-lambdas (gethash p lambdas) table)
									 (mentioned (caddr (gethash proc lambdas)) table))))
						     (dolist (v body-refs)
						       (when (undefined-at v j)
							 (pushnew v (gethash proc danger))))
						     (setq stack (append body-refs stack)))))))))))
		     (let ((checked '()))
		       (maphash (lambda (k vs)
				  (declare (ignore k))
				  (dolist (v vs) (unless (assoc v checked) (push (cons v (flag-for v)) checked))))
				danger)
		       (when checked
			 (let ((new '()))
			   (flet ((flags-of (key) (remove-if-not (lambda (c) (member (car c) (gethash key danger))) checked)))
			     (loop for s in statements for j from 0
				   do (let* ((defines (and (set-p s vars) (eql (gethash (cadr s) index) j) (cadr s)))
					     (s (cond ((and defines (gethash defines lambdas) (gethash defines danger))
						       `(,(car s) ,defines ,(guard-references (caddr s) (flags-of defines))))
						      ((gethash j danger)
						       (if defines
							   `(,(car s) ,defines ,(guard-references (caddr s) (flags-of j)))
							   (guard-references s (flags-of j))))
						      (t s))))
					(push s new)
					(let ((c (and defines (assoc defines checked))))
					  (when c (push `(,(sym "set!") ,(cdr c) (,(sym "quote") ,qs:true)) new))))))
			   `((,(caar e) ,(append vars (mapcar #'cdr checked))
			      (,(sym "begin") ,@(nreverse new)))
			     ,@args
			     ,@(mapcar (lambda (c) (declare (ignore c)) `(,(sym "quote") ,qs:false)) checked))))))))))
	   (letrec-checked (e)
	     (when (and (keyword-p (car e) "LETREC") (consp (cdr e)) (listp (cadr e)) (cadr e)
			(every (lambda (b) (and (consp b) (consp (cdr b)))) (cadr e))
			;; an init can reach a variable before all are defined
			;; only if it runs code (a call, say) or is one of them;
			;; lambdas, constants and other variables run nothing
			(some (lambda (b)
				(let ((init (cadr b)))
				  (not (or (lambda-p init) (quote-p init)
					   (and (atom init) (not (and (symbolp init) (assoc init (cadr e)))))))))
			      (cadr e)))
	       (let* ((vars (mapcar #'car (cadr e)))
		      (table (let ((h (make-hash-table :test 'eq))) (dolist (v vars h) (setf (gethash v h) t))))
		      (flag (flag-for (car vars)))
		      (flags (mapcar (lambda (v) (cons v flag)) vars)))
		 (when (some (lambda (b) (mentioned (cadr b) table)) (cadr e))
		   `((,(sym "lambda") (,flag)
		      (,(car e) ,(mapcar (lambda (b) (list (car b) (guard-references (cadr b) flags))) (cadr e))
		       (,(sym "begin") (,(sym "set!") ,flag (,(sym "quote") ,qs:true)) ,@(cddr e))))
		     (,(sym "quote") ,qs:false)))))))
    (walk form)))

(defun definitions-as-letrec (form)
  (let ((form (letrec-checks form))
	(assignments (make-hash-table :test 'eq)))
    (labels ((keyword-p (x name) (and (symbolp x) (string= (symbol-name x) name)))
	     (quote-p (x) (and (consp x) (keyword-p (car x) "QUOTE")))
	     (lambda-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
	     (false-p (x) (and (quote-p x) (eq (cadr x) qs:false)))
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
				(inner (let ((body (cond ((null rest) `(,(sym "quote") ,qs:unspecific))
							 ((null (cdr rest)) (car rest))
							 (t `(,(sym "begin") ,@rest)))))
					 (if definitions
					     `(,(sym "letrec")
					       ,(mapcar (lambda (s) (list (cadr s) (caddr s))) definitions)
					       ,body)
					     body))))
			   (if others
			       `((,(caar e) ,others ,inner) ,@(mapcar (lambda (v) (declare (ignore v)) `(,(sym "quote") ,qs:false)) others))
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
body made top-level definitions, and those of any big body run once in
that body, as (let () ...) is; or FORM if it has no such body."
  (setq *hoisted-procedures* '())
  (labels ((keyword-p (x name) (and (symbolp x) (string= (symbol-name x) name)))
	   (lambda-p (x) (and (consp x) (keyword-p (car x) "LAMBDA")))
	   (false-p (x) (and (consp x) (keyword-p (car x) "QUOTE") (eq (cadr x) qs:false)))
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
	   (unwrap (e)
	     ;; ((lambda () body)), as (let () body ...) is, is body at top level
	     (loop while (and (consp e) (null (cdr e)) (lambda-p (car e))
			      (consp (cdr (car e))) (null (cadr (car e)))
			      (consp (cddr (car e))) (null (cdddr (car e))))
		   do (setq e (caddr (car e))))
	     (if (and (consp e) (keyword-p (car e) "BEGIN") (consp (cdr e)))
		 (let ((last (unwrap (car (last e)))))
		   (if (eq last (car (last e))) e `(,@(butlast e) ,last)))
		 e))
	   (hoist (e)
	     (let* ((vars (cadr (car e))) (body (caddr (car e)))
		    (statements (if (and (consp body) (keyword-p (car body) "BEGIN")) (cdr body) (list body)))
		    (counts (make-hash-table :test 'eq)))
	       (count-assignments e counts)
	       (let ((defined (loop for s in statements
				    when (and (consp s) (keyword-p (car s) "SET!") (member (cadr s) vars)
					      (lambda-p (caddr s)) (= (gethash (cadr s) counts 0) 1))
				      collect (cadr s))))
		 (setq *hoisted-procedures* (append defined *hoisted-procedures*))
		 `(,@(loop for v in vars unless (member v defined)
			   collect `(,(sym "define") ,v (,(sym "quote") ,qs:false)))
		   ,@(loop for s in statements
			   append (cond ((and (consp s) (keyword-p (car s) "SET!") (member (cadr s) defined))
					 (list `(,(sym "define") ,(cadr s) ,(caddr s))))
					;; a big (let () body ...) in the body, run
					;; once where it is, is hoisted in its turn
					((> (tree-size s) *letrec-definitions-limit*)
					 (let ((h (hoist-form s)))
					   (if (eq h s) (list s) (cdr h))))
					(t (list s))))))))
	   (hoist-form (form)
	     ;; (begin form ...) of FORM's definitions and body, or FORM
	     (let ((u (unwrap form)))
	       (cond ((body-form-p u) `(,(sym "begin") ,@(hoist u)))
		     ((and (consp u) (keyword-p (car u) "BEGIN") (body-form-p (car (last u))))
		      `(,@(butlast u) ,@(hoist (unwrap (car (last u))))))
		     (t form)))))
    (hoist-form form)))

(defun open-top-level (form)
  "OPEN-PRIMITIVES for a form evaluated at top level, whose definitions
are hoisted if it's big."
  (let ((form (open-primitives-1 form)))
    (optimize-core
     (definitions-as-letrec
      (if (> (tree-size form) *letrec-definitions-limit*)
	  (hoist-definitions form)
	  (progn (setq *hoisted-procedures* '()) form))))))

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

(defparameter *interpret-limit* 2000000
  "The largest Lisp form, in conses, that SBCL compiles at all: past this,
its register allocation needs more than a 16 GB heap (a body of
thousands of definitions that can't be hoisted, say), so a form is
evaluated by SBCL's interpreter.")

(defvar *evaluator* nil
  "How top-level forms are evaluated, from QUASISCHEME_EVALUATOR, read
once per process: :COMPILE (the default) compiles every form with SBCL;
:AUTO interprets a form that makes no closure, which runs once, so that
compiling it costs more than it saves; :INTERPRET interprets every form.
SBCL's interpreter is sb-eval, or, in an SBCL built with
--with-sb-fasteval, sb-fasteval, which is several times faster.")

#+sbcl (pushnew (lambda () (setq *evaluator* nil)) sb-ext:*init-hooks*)

(defun evaluator ()
  (or *evaluator*
      (setq *evaluator*
	    (let ((v (uiop:getenv "QUASISCHEME_EVALUATOR")))
	      (cond ((equal v "interpret") :interpret)
		    ((equal v "auto") :auto)
		    (t :compile))))))

(defun run-once-form-p (form)
  "Whether Lisp FORM makes no closure, so that whatever it does runs once:
it has no LAMBDA, FUNCTION, FLET, LABELS or the like."
  (labels ((walk (x)
	     (cond ((symbolp x)
		    (not (member x '(lambda function flet labels macrolet sb-int:named-lambda))))
		   ((atom x) t)
		   ((eq (car x) 'quote) t)
		   (t (loop for y on x
			    always (and (walk (car y)) (or (listp (cdr y)) (walk (cdr y)))))))))
    (walk form)))

(defparameter *constraint-propagation-limit* 50000
  "The largest Lisp form, in conses, SBCL compiles with constraint
propagation: its sets of constraints, a bit vector per block, grow with
the square of a function's size, and one of Typed Racket's linklets took
more than 3 GB of them (CALL-WITH-SIZE-POLICY).")

(defun call-with-size-policy (size thunk)
  "Call THUNK, which compiles code whose biggest form is SIZE conses:
without SBCL's constraint propagation if that's past
*CONSTRAINT-PROPAGATION-LIMIT*.  The code is as correct, its types less
well known."
  #+sbcl (if (> size *constraint-propagation-limit*)
	     (let ((sb-c::*constraint-propagate* nil)) (funcall thunk))
	     (funcall thunk))
  #-sbcl (progn size (funcall thunk)))

(defun eval-compiled-or-interpreted (form)
  "EVAL FORM, or, if SBCL's compiler fails on it (a procedure too big for
one code object, past arm64's branch and constant reach), evaluate it
with SBCL's interpreter: nothing of it has run when compiling fails.  A
form bigger than *INTERPRET-LIMIT* is interpreted, and so is one
*EVALUATOR* says to interpret."
  #+sbcl
  (when (or (> (tree-size form) *interpret-limit*)
	    (case (evaluator)
	      (:interpret t)
	      (:auto (run-once-form-p form))))
    (return-from eval-compiled-or-interpreted
      (let ((sb-ext:*evaluator-mode* :interpret))
	(eval form))))
  #+sbcl
  (let ((form form))
    (multiple-value-bind (values failure)
	(catch 'compile-failed
	  (handler-bind ((error (lambda (c)
				  (when (and (boundp 'sb-c::*compilation*) sb-c::*compilation*
					     (not (typep c 'sb-int:simple-program-error)))
				    (throw 'compile-failed (values nil t))))))
	    (values (multiple-value-list (call-with-size-policy (tree-size form) (lambda () (eval form)))) nil)))
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
  (let ((lisp (qsl:tr "TRANSLATE" form *host*))
	(size (if (and top-level (consp form) (symbolp (car form))
		       (string= (symbol-name (car form)) "BEGIN"))
		  (reduce #'max (cdr form) :key #'tree-size :initial-value 0)
		  (tree-size form))))
    (if (> size *inline-arithmetic-limit*)
	`(locally (declare (notinline qs:scheme+ qs:scheme- qs:scheme*)) ,lisp)
	(inline-small-labels (declare-loop-types (stack-allocate-marked (name-marked-lambdas lisp)))))))

(defun name-marked-lambdas (lisp)
  "LISP with each #'(lambda args '(:racket-name . id) ...) made
#'(sb-int:named-lambda (:racket id) args ...): --racket's let-bound
procedures keep their names (object-name) when the continuation
transformation makes them state machines."
  (labels ((marker-p (x) (and (consp x) (eq (car x) 'quote) (consp (cadr x)) (eq (car (cadr x)) :racket-name)))
	   (walk (x)
	     (cond ((atom x) x)
		   ((eq (car x) 'quote) x)
		   ((and (eq (car x) 'function) (consp (cadr x)) (eq (car (cadr x)) 'lambda)
			 (consp (cdr (cadr x))))
		    (let* ((l (cadr x)) (body (cddr l))
			   (m (or (and (marker-p (car body)) (car body))
				  (and (consp (car body)) (eq (car (car body)) 'progn) (marker-p (cadr (car body)))
				       (cadr (car body))))))
		      (if m
			  (let ((body (if (marker-p (car body))
					  (cdr body)
					  (cons (cons 'progn (cddr (car body))) (cdr body)))))
			    `(function (sb-int:named-lambda (:racket ,(cdr (cadr m))) ,(cadr l) ,@(mapcar #'walk body))))
			  (cons (car x) (walk (cdr x))))))
		   (t (let ((a (walk (car x))) (d (walk (cdr x))))
			(if (and (eq a (car x)) (eq d (cdr x))) x (cons a d)))))))
    (walk lisp)))

;;; Small known procedures inline (docs/optimizations.md, idea 6).  SBCL
;;; inlines a LABELS function only if it's called once; declared INLINE,
;;; a small one is inlined wherever it's called, which saves a call in
;;; helpers like (define (square x) (* x x)).  Not one that can reach
;;; itself through the others it calls: that would expand without end.

(defparameter *inline-labels-size* 40
  "The biggest LABELS function, in conses, declared inline.")

(defun inline-small-labels (form)
  "FORM, Lisp code, with the small non-recursive functions of each LABELS
in it declared inline."
  (labels ((walk (x)
	     (cond ((atom x) x)
		   ((eq (car x) 'quote) x)
		   ((and (eq (car x) 'labels) (consp (cdr x)) (listp (cadr x)))
		    (let* ((bindings (mapcar (lambda (b)
					       (if (and (consp b) (consp (cdr b)))
						   (list* (car b) (cadr b) (mapcar #'walk (cddr b)))
						   b))
					     (cadr x)))
			   (inline (inline-candidates bindings))
			   (body (mapcar #'walk (cddr x))))
		      (if inline
			  `(labels ,bindings (declare (inline ,@inline)) ,@body)
			  `(labels ,bindings ,@body))))
		   (t (let ((a (walk (car x))) (d (walk (cdr x))))
			(if (and (eq a (car x)) (eq d (cdr x))) x (cons a d)))))))
    (walk form)))

(defun inline-candidates (bindings)
  "The names of BINDINGS' functions small enough to inline that can't
reach themselves through the others."
  (let* ((names (loop for b in bindings when (and (consp b) (symbolp (car b))) collect (car b)))
	 (small (loop for b in bindings
		      when (and (consp b) (symbolp (car b)) (consp (cdr b))
				(<= (tree-size (cddr b)) *inline-labels-size*))
			collect (car b)))
	 (calls (make-hash-table :test 'eq)))
    (labels ((mentions (tree name)
	       (cond ((eq tree name) t)
		     ((atom tree) nil)
		     (t (or (mentions (car tree) name) (mentions (cdr tree) name))))))
      (dolist (b bindings)
	(when (and (consp b) (symbolp (car b)))
	  (setf (gethash (car b) calls)
		(remove-if-not (lambda (n) (mentions (cddr b) n)) names))))
      (flet ((reaches-itself-p (f)
	       (let ((seen '()) (stack (copy-list (gethash f calls))))
		 (loop while stack
		       do (let ((g (pop stack)))
			    (when (eq g f) (return t))
			    (unless (member g seen)
			      (push g seen)
			      (setq stack (append (gethash g calls) stack))))))))
	(remove-if #'reaches-itself-p small)))))

;;; ------------------------------------------------------------------
;;; Building the host environment

(defun all-primitive-names ()
  (remove-duplicates
   (append (mapcar #'sym (loop for (nil export-string) in qs-r7rs:*standard-libraries*
			       append (qs-r7rs:split-names export-string)))
	   (qsl:tr "INTERFACE-NAMES"
		   (qsl:tr "STRUCTURE-INTERFACE" (qsl:base-structure))))))

(defun overridden-primitive-names ()
  "Names the host's INSTALL-PRIMITIVES (re)defines."
  (mapcar (lambda (p) (sym (car p))) qs-r6rs:*primitives*))

(defun make-host (&optional (package-name *host-package-name*))
  "The host environment: a copy of the R7RS implementation env's
bindings -- except that where a binding is still R5RS's own built-in
(same value, and not redefined by the R6RS layer), the host shares the
R5RS binding itself, so the translator open-codes it ((+ a b) becomes
CL's +, (vector-ref v i) SVREF) as it does in R5RS code.  A copy would
be a fresh variable, called out of line."
  (let ((env (fresh-host-env package-name))
	(base (qsl:base-structure))
	(overridden (overridden-primitive-names))
	(shared '()))
    (dolist (name (all-primitive-names))
      (when (qsl:binding-defined-p qs-r7rs:*implementation-env* name)
	(let ((den (qsl:tr "PROGRAM-ENV-LOOKUP" qs-r7rs:*implementation-env* name))
	      (base-den (and (member name (qsl:tr "INTERFACE-NAMES" (qsl:tr "STRUCTURE-INTERFACE" base)))
			     (qsl:tr "STRUCTURE-REF" base name))))
	  (if (and base-den
		   (qsl:variable-node-p den) (qsl:variable-node-p base-den)
		   (boundp (qsl:tr "PROGRAM-VARIABLE-LOCATION" den))
		   (boundp (qsl:tr "PROGRAM-VARIABLE-LOCATION" base-den))
		   (eq (symbol-value (qsl:tr "PROGRAM-VARIABLE-LOCATION" den))
		       (symbol-value (qsl:tr "PROGRAM-VARIABLE-LOCATION" base-den)))
		   (not (member name overridden)))
	      (if (closed-primitive-p (qs:scheme-symbol-name name))
		  (progn (define-builtin-alias env name base-den)
			 (qsl:copy-bindings! env qs-r7rs:*implementation-env* (list name)))
		  (progn (qsl:tr "PROGRAM-ENV-DEFINE!" env name base-den) (push name shared)))
	      (qsl:copy-bindings! env qs-r7rs:*implementation-env* (list name))))))
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
  (qsl:tr "MAKE-PROGRAM-ENV" (intern package-name "SCHEME") '()))

(defun install-primitives (alist)
  "ALIST of (name-string . function), e.g. from DEFPRIM registries."
  (loop for (name . fn) in alist do (host-set! name fn)))

;;; ------------------------------------------------------------------
;;; Loading the expander

(defun vendor-file (name)
  (asdf:system-relative-pathname :quasischeme
				 (concatenate 'string "vendor/psyntax/" name)))

(defun read-scheme-text (string)
  (with-input-from-string (in string)
    (loop for form = (funcall qs:*scheme-read* in)
	  until (eq form qs:eof-object)
	  collect form)))

(defun read-file-forms (path)
  (with-open-file (in path)
    (qs:skip-script-header in)
    (loop for form = (funcall qs:*scheme-read* in)
	  until (eq form qs:eof-object)
	  collect form)))

(defun load-image (path &key (drop-last nil))
  "Evaluate the core forms of a psyntax .pp file in *HOST*.  (The
original pre-built images end with a form that runs a script named on
the command line and exits; DROP-LAST skips it.)"
  (let ((forms (read-file-forms path)))
    (dolist (form (if drop-last (butlast forms) forms))
      (host-eval form))))

;;; psyntax's image is compiled once, by ASDF (the PSYNTAX-IMAGE
;;; component in quasischeme.asd): translated to Lisp, which
;;; COMPILE-FILE compiles.  Translating the image takes a moment; it's
;;; the Lisp compiler that's slow, and BOOT would otherwise run it on
;;; every start.  The fasl is valid only for a host built the same way,
;;; with the same shared primitives (see MAKE-HOST), which it checks.

(defvar *image-fasl* nil
  "The compiled psyntax-quasischeme.pp, if ASDF has built it.")

(defun prepare-host (&optional (package-name *host-package-name*))
  "Create *HOST*, with everything psyntax's image expects of it."
  (setq *host* (make-host package-name))
  (setf qs-r6rs::*globals-hook* #'host-ref)
  (qs-r6rs::install-exception-hooks)
  (install-primitives qs-r6rs:*primitives*)
  (install-adapter)
  (install-continuation-primitives)
  (install-inline-primitives)
  (install-cptypes-primitives)
  (install-cpdx-primitives)
  *host*)

;;; Inline primitives.  A host primitive that isn't one of the
;;; translator's own built-ins is called out of line, through its
;;; checks.  For a few that programs call in their inner loops, a
;;; compiler macro on the host global's function inlines the case where
;;; the arguments are right, and calls the primitive (for its error)
;;; otherwise.

(defparameter *inline-primitives*
  `(("bytevector-u8-ref" (bv k)
     (if (and (typep bv '(simple-array (unsigned-byte 8) (*)))
	      (typep k 'fixnum) (< -1 k (length bv)))
	 (aref bv k)
	 :call))
    ("bytevector-u8-set!" (bv k byte)
     (if (and (typep bv '(simple-array (unsigned-byte 8) (*)))
	      (typep k 'fixnum) (< -1 k (length bv))
	      (typep byte '(unsigned-byte 8)))
	 (progn (setf (aref bv k) byte) qs:unspecific)
	 :call))
    ("bytevector-length" (bv)
     (if (typep bv '(simple-array (unsigned-byte 8) (*)))
	 (length bv)
	 :call))
    ;; R6RS's fixnum operations (and --racket's fx and unsafe-fx ones, their
    ;; aliases) on two fixnums; anything else, or a result past the
    ;; fixnums, the procedure's own way (its condition)
    ("fx+" (a b) (if (and (typep a 'fixnum) (typep b 'fixnum))
		     (let ((r (+ a b))) (if (typep r 'fixnum) r :call))
		     :call))
    ("fx-" (a b) (if (and (typep a 'fixnum) (typep b 'fixnum))
		     (let ((r (- a b))) (if (typep r 'fixnum) r :call))
		     :call))
    ,@(loop for (name op) in '(("fx=?" =) ("fx<?" <) ("fx>?" >) ("fx<=?" <=) ("fx>=?" >=))
	    collect `(,name (a b) (if (and (typep a 'fixnum) (typep b 'fixnum))
				      (if (,op a b) t qs:false)
				      :call)))
    ;; two strings: CL's comparisons, which compare code points as R7RS does
    ("string=?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string= a b) t qs:false) :call))
    ("string<?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string< a b) t qs:false) :call))
    ("string>?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string> a b) t qs:false) :call))
    ("string<=?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string<= a b) t qs:false) :call))
    ("string>=?" (a b) (if (and (simple-string-p a) (simple-string-p b)) (if (string>= a b) t qs:false) :call))
    ;; the Chez mode's mutators (src/chez/chez.lisp), which refuse an immutable
    ;; object: one with the header flag of src/chez/host.lisp
    ("chez:vector-set!" (v k obj)
     (if (and (simple-vector-p v) (typep k 'fixnum) (< -1 k (length v))
	      (not (logtest (ash sb-vm:+vector-shareable-nonstd+ 8) (sb-kernel:get-header-data v))))
	 (progn (setf (svref v k) obj) qs:unspecific)
	 :call))
    ("chez:string-set!" (s k c)
     (if (and (typep s '(simple-array character (*))) (typep k 'fixnum) (< -1 k (length s))
	      (characterp c)
	      (not (logtest (ash sb-vm:+vector-shareable-nonstd+ 8) (sb-kernel:get-header-data s))))
	 (progn (setf (schar s k) c) qs:unspecific)
	 :call))
    ;; the Chez mode's comparisons, which check their arguments' types
    ,@(loop for (name test type) in '(("char=?" char= character) ("char<?" char< character)
				      ("char>?" char> character) ("char<=?" char<= character)
				      ("char>=?" char>= character) ("char-ci=?" char-equal character)
				      ("char-ci<?" char-lessp character) ("char-ci>?" char-greaterp character)
				      ("char-ci<=?" char-not-greaterp character) ("char-ci>=?" char-not-lessp character)
				      ("string=?" string= simple-string) ("string<?" string< simple-string)
				      ("string>?" string> simple-string) ("string<=?" string<= simple-string)
				      ("string>=?" string>= simple-string))
	    collect `(,(concatenate 'string "chez:" name) (a b)
		      (if (and (typep a ',type) (typep b ',type)) (if (,test a b) t qs:false) :call)))
    ;; R7RS define-record-type's procedures (src/r7rs/syntax.sls), for a
    ;; record of exactly the type: a subtype's, or an error, by the call
    ("r7rs:record-ref" (x rtd i who)
     (if (and (qs-r6rs::record-p x) (eq (qs-r6rs::record-rtd x) rtd))
	 (qs-r6rs::record-field x i)
	 :call))
    ("r7rs:record-set!" (x rtd i value who)
     (if (and (qs-r6rs::record-p x) (eq (qs-r6rs::record-rtd x) rtd))
	 (progn (setf (qs-r6rs::record-field x i) value) qs:unspecific)
	 :call))
    ("r7rs:record?" (x rtd)
     (if (and (qs-r6rs::record-p x) (eq (qs-r6rs::record-rtd x) rtd)) t :call))
    ;; a letrec variable's reference that may come before its init (LETREC-CHECKS)
    ("%letrec-check" (flag value name) (if (eq flag qs:false) :call value)))
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
					 body)))))))))
  ;; R7RS define-record-type's constructor, (r7rs:make-record rtd (vector
  ;; x ...)): the record made at once, without the vector
  (let ((global (location (sym "r7rs:make-record"))))
    (setf (compiler-macro-function global)
	  (lambda (form env)
	    (declare (ignore env))
	    (let ((args (cdr form)))
	      (if (and (not (eq (car form) 'funcall)) (= (length args) 2)
		       (consp (second args)) (eq (car (second args)) 'vector))
		  (let* ((fields (cdr (second args)))
			 (rtd (gensym "RTD"))
			 (vars (loop repeat (length fields) collect (gensym "F")))
			 (r (gensym "R")))
		    `(let* ((,rtd ,(first args)) ,@(mapcar #'list vars fields)
			    (,r (qs-r6rs::allocate-record ,rtd ,(length fields))))
		       ,@(loop for v in vars for i from 0 collect `(qs-r6rs::init-record-field ,r ,i ,v))
		       ,r))
		  form))))))

(defun host-signature ()
  "What compiled code depends on in a host: which primitives are R5RS's
own bindings, and so open-coded."
  (sort (mapcar #'qs:scheme-symbol-name *shared-primitives*) #'string<))

(defun compile-image (image fasl)
  "Translate the psyntax IMAGE (a .pp file) to Lisp and compile it into
FASL.  The translating is done in a host of its own, under another
package name, so that a host already running in this Lisp (ASDF may
recompile the image in a live session) is left alone; the code is then
written with the real host package's name."
  (let* ((compiling (concatenate 'string *host-package-name* " (compiling)"))
	 (*host* nil)
	 (*shared-primitives* '())
	 (qs-r6rs::*globals-hook* qs-r6rs::*globals-hook*)
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
		   (let ((*optimize-off* t)) ; psyntax's own image: as it was
		     (mapcar (lambda (form) (qsl:tr "TRANSLATE" (open-primitives form) *host*))
			     forms)))))
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
	   (or (file-write-date (vendor-file "psyntax-quasischeme.pp")) 0))
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
  (let ((own (vendor-file "psyntax-quasischeme.pp")))
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
  ;; one thread at a time expands (*EXPANDER-LOCK*)
  (when (boundp (location (sym "psyntax:expansion-lock-hook")))
    (funcall (host-ref "psyntax:expansion-lock-hook") #'call-with-expander-lock))
  *host*)

;;; ------------------------------------------------------------------
;;; Threads: psyntax's state is global, so one thread at a time expands.
;;; psyntax's entry points (expansion-lock-hook) hold *EXPANDER-LOCK*, a
;;; recursive lock, and eval-core lets go of it while the expanded code
;;; runs: what's serialized is expanding, not running.  A program's
;;; threads that load libraries or call eval wait for each other only
;;; while one of them is expanding.

(defstruct (expander-lock (:constructor make-expander-lock ()))
  (mutex (bt:make-lock "psyntax expander"))
  (owner nil)
  (depth 0 :type fixnum))

(defvar *expander-lock* (make-expander-lock))

(defun call-with-expander-lock (thunk)
  (let ((lock *expander-lock*) (self (bt:current-thread)))
    (if (eq (expander-lock-owner lock) self)
	(progn
	  (incf (expander-lock-depth lock))
	  (unwind-protect (funcall thunk)
	    (decf (expander-lock-depth lock))))
	(bt:with-lock-held ((expander-lock-mutex lock))
	  (setf (expander-lock-owner lock) self
		(expander-lock-depth lock) 1)
	  (unwind-protect (funcall thunk)
	    (setf (expander-lock-owner lock) nil
		  (expander-lock-depth lock) 0))))))

(defun call-without-expander-lock (thunk)
  "Call THUNK with the expander lock let go of, if this thread holds it,
and taken again after."
  (let ((lock *expander-lock*) (self (bt:current-thread)))
    (if (not (eq (expander-lock-owner lock) self))
	(funcall thunk)
	(let ((depth (expander-lock-depth lock)))
	  (setf (expander-lock-owner lock) nil
		(expander-lock-depth lock) 0)
	  (bt:release-lock (expander-lock-mutex lock))
	  (unwind-protect (funcall thunk)
	    (bt:acquire-lock (expander-lock-mutex lock))
	    (setf (expander-lock-owner lock) self
		  (expander-lock-depth lock) depth))))))

;;; ------------------------------------------------------------------
;;; Finding libraries in files

(defvar *library-path* (list "./")
  "Directories searched, in order, for library source files: (foo bar)
is looked for as foo/bar.sls, then .ss, .sld and .scm, in each.")

(defvar *system-library-path*
  (list (cons "srfi" (namestring (asdf:system-relative-pathname :quasischeme "src/srfi/"))))
  "Libraries that ship with Quasischeme, searched after *LIBRARY-PATH*.
An entry (PREFIX . DIR) roots names beginning with PREFIX at DIR: (srfi
1) is src/srfi/1.sld.")

(defparameter *library-extensions* '("sls" "ss" "sld" "scm"))

(defparameter *implementation-variants* '("quasischeme" nil "chezscheme" "ikarus")
  "Implementation-specific variants of a library file to try, in order:
foo.quasischeme.sls first, then the generic foo.sls (NIL), then other
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
		collect (qs:scheme-symbol-name part))))

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
	     (let ((form (with-open-file (in file) (funcall qs:*scheme-read* in))))
	       (and (qsl:keyword-head-p form "library") form))))
      qs:false))

(defun locate-library-file (name)
  "psyntax's FILE-LOCATOR: a file name for library NAME, or #f."
  (let ((stem (library-name-file-stem name)))
    (or (loop for path in (candidate-files stem)
	      when (probe-file path) return (namestring path))
	qs:false)))

;;; ------------------------------------------------------------------
;;; Entry points

(defun entry (name)
  (let ((loc (location (sym name))))
    (unless (boundp loc)
      (error "psyntax entry point ~A is missing: the loaded image predates ~
              vendor/psyntax/psyntax/main.ss -- run (qsx:rebuild)" name))
    (symbol-value loc)))


(defun eval-program (forms)
  "Run an R6RS top-level program: FORMS begin with (import ...)."
  (funcall (entry "psyntax:eval-r6rs-top-level") forms))

(defun eval-library (form)
  "Expand, install and (lazily) instantiate an R6RS (library ...) form."
  (funcall (entry "psyntax:library-expander") form))

(defvar *top-level-self-calls*)		; src/continuations.lisp

(defun eval-top-level (form)
  "Evaluate FORM at the REPL, in (quasischeme interaction)."
  (let ((*top-level-self-calls* t))
    (funcall (entry "psyntax:eval-top-level") form)))

(defun expand (form &optional (env (funcall (entry "psyntax:environment")
					     (list (mapcar #'sym '("rnrs"))))))
  (funcall (entry "psyntax:expand") form env))

(defun eval-forms (forms)
  "Evaluate a file's worth of forms: any (library ...) forms are
installed, and what follows them, if anything, is run as a program."
  (let ((result qs:unspecific))
    (loop while (and forms (qsl:keyword-head-p (car forms) "library"))
	  do (eval-library (pop forms)))
    (when forms
      (setq result (eval-program forms)))
    result))

(defun load-file (path)
  (eval-forms (read-file-forms path)))



;;; ------------------------------------------------------------------
;;; Rebuilding the expander image on Quasischeme itself

(defun rebuild (&key seed (directory (vendor-file "")))
  "Run psyntax-buildscript.ss -- the expander expanding its own sources --
writing psyntax-quasischeme.pp.  SEED is as for BOOT.  DIRECTORY holds
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
    (canonicalize-gensyms (merge-pathnames "psyntax-quasischeme.pp" directory))
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
	    (funcall qs:*scheme-write* form out)
	    (format out "~%~%~%")))))))

;;; ------------------------------------------------------------------
;;; psyntax's identifier table

(defun table-exports (keys)
  "Names (strings) the build script's identifier->library-map sends to
any of the library KEYS (strings: \"r\" for (rnrs), \"r5\", ...)."
  (let* ((forms (read-file-forms (vendor-file "psyntax-buildscript.ss")))
	 (def (find-if (lambda (f)
			 (and (consp f) (consp (cdr f)) (symbolp (cadr f))
			      (string= (qs:scheme-symbol-name (cadr f)) "identifier->library-map")))
		       forms)))
    (loop for (name . libs) in (cadr (caddr def))
	  when (some (lambda (k) (member (qs:scheme-symbol-name k) keys :test #'string=)) libs)
	    collect (qs:scheme-symbol-name name))))

;;; ------------------------------------------------------------------
;;; Diagnostics

(defun missing-primitives ()
  "Names exported by some psyntax library that have no host binding."
  (let ((pslib (funcall (entry "psyntax:environment") (list (list (sym "quasischeme"))))))
    (declare (ignore pslib))
    nil))
