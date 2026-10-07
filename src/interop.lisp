; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-INTEROP -*-

;;;; Scheme and Common Lisp, both ways (docs/interop.md)
;;;;
;;;; Scheme calling Lisp: the virtual library (cl <package>) exports a
;;;; Lisp package's external functions and variables to Scheme,
;;;;
;;;;   (import (scheme base) (prefix (cl common-lisp) cl:))
;;;;   (cl:sort (list 3 1 2) cl:< #:key cl:identity)
;;;;
;;;; loading the system that defines the package if it isn't there yet
;;;; (*LISP-SYSTEM-LOADER*), and (pseudoscheme lisp) has the helpers
;;;; (lisp-true?, lisp-let, lisp-function, ...).
;;;;
;;;; Lisp calling Scheme: USE-LIBRARY makes a Scheme library a Lisp
;;;; package whose functions are the library's procedures,
;;;;
;;;;   (r7rs:use-library '(srfi 1))
;;;;   (srfi-1:filter #'evenp '(1 2 3 4))   ; => (2 4)
;;;;
;;;; Most values are shared as they are (lists, numbers, strings,
;;;; vectors, functions...).  Booleans are not: Scheme's #f is PS:FALSE,
;;;; and Lisp's false is NIL, which is Scheme's ().  So every crossing
;;;; converts:
;;;;
;;;; * Scheme -> Lisp (arguments to a (cl ...) function, results of a
;;;;   USE-LIBRARY function): #f becomes NIL.
;;;; * Lisp -> Scheme: a (cl ...) function whose name says it's a
;;;;   predicate (EVENP, STRING=, MEMBER, ...) returns #f for NIL; other
;;;;   functions return NIL as (), since lists are what NIL usually is.
;;;; * Functions crossing either way are wrapped so their results
;;;;   follow the same rules: a Lisp function given to Scheme code is
;;;;   taken to be a predicate (NIL -> #f), the common case of
;;;;   (filter #'evenp ...).  VERBATIM exempts a function; a wrapper
;;;;   crossing back is unwrapped, so cl:equal reaches MAKE-HASH-TABLE
;;;;   as #'EQUAL.

(defpackage "PSEUDOSCHEME-INTEROP"
  (:use "COMMON-LISP")
  (:export "SCHEMIFY" "EXPORT-BINDINGS" "IMPORT-INTO-PACKAGE" "*LISP-SYSTEM-LOADER*" "*AUTOLOAD-LISP-SYSTEMS*" "LOAD-LISP-SYSTEM"
	   "USE-LIBRARY" "LIBRARY-EXPORTS" "VERBATIM" "BOOT"
	   "TO-LISP" "TO-SCHEME" "LISP-FACING" "SCHEME-FACING"
	   "LISP-PREDICATE-NAME-P" "*LISP-PREDICATES*" "*NOT-LISP-PREDICATES*"
	   "LIBRARY-PACKAGE-NAME" "PARSE-LIBRARY-NAME"))

(in-package "PSEUDOSCHEME-INTEROP")

(defun ssym (string) (ps:intern-scheme-symbol string))

(defun scheme-false-constant-p (x)
  (and (symbolp x) x (symbol-package x)
       (string= (symbol-name x) "FALSE")
       (member (package-name (symbol-package x)) '("R7RS" "R6RS" "R5RS") :test #'string=)))

(defun scheme-true-constant-p (x)
  (and (symbolp x) x (symbol-package x)
       (string= (symbol-name x) "TRUE")
       (member (package-name (symbol-package x)) '("R7RS" "R6RS" "R5RS") :test #'string=)))

(defun schemify (datum)
  "A Lisp datum as a Scheme one: symbols move to the SCHEME package
(except T, which is #t, NIL, which is (), keywords, which are
#:keywords, and R7RS:FALSE / R7RS:TRUE (and R6RS:, R5RS:), which are #f
and #t); conses and vectors are copied recursively."
  (typecase datum
    (null nil)
    ((eql t) t)
    (keyword datum)
    ((satisfies scheme-false-constant-p) ps:false)
    ((satisfies scheme-true-constant-p) t)
    ;; Already Scheme: SCHEME symbols, #f (PS:FALSE), uninterned symbols.
    (symbol (if (or (eq (symbol-package datum) ps:scheme-package)
		    (eq datum ps:false)
		    (null (symbol-package datum)))
		datum
		(intern (symbol-name datum) ps:scheme-package)))
    (cons (cons (schemify (car datum)) (schemify (cdr datum))))
    (simple-vector (map 'simple-vector #'schemify datum))
    (t datum)))

;;; ------------------------------------------------------------------
;;; Crossing the boundary

(defvar *originals* (make-hash-table :test 'eq :weakness :key)
  "Wrapper function -> the function it wraps, so a wrapper crossing back
is unwrapped rather than wrapped again.")

(defvar *verbatim* (make-hash-table :test 'eq :weakness :key)
  "Functions that cross the boundary unwrapped (see VERBATIM).")

(defvar *lisp-facing* (make-hash-table :test 'eq :weakness :key))
(defvar *scheme-facing* (make-hash-table :test 'eq :weakness :key))

(defun verbatim (function)
  "Mark FUNCTION to be passed between Scheme and Lisp as it is, with no
boolean conversion of its results (e.g. a Lisp callback that returns
lists, NIL meaning ()).  Returns FUNCTION.  The mark is on the function
object, so mark a fresh closure, (verbatim (lambda (x) ...)), rather
than a named function like #'evenp."
  (setf (gethash function *verbatim*) t)
  function)

(defun values-to-lisp (&rest values)
  "VALUES, from Scheme, as Lisp gets them (TO-LISP): #f is NIL, and a
Scheme procedure is wrapped, so that it is recognized when it crosses
back."
  (values-list (mapcar #'to-lisp values)))

(defun lisp-facing (procedure)
  "PROCEDURE (a Scheme procedure) as a Lisp function: its arguments go
to Scheme (TO-SCHEME), its results come back to Lisp (TO-LISP: #f as
NIL, procedures wrapped).  It's
called in a barrier frame (src/continuations.lisp): the Lisp code
calling it isn't recorded in a captured continuation, so re-entering one
captured in PROCEDURE would resume as if that Lisp code had returned at
once; the barrier makes it an error instead.  Escaping works."
  (or (gethash procedure *originals*)
      (gethash procedure *lisp-facing*)
      (let ((f (lambda (&rest args)
		 (psx::%barrier "Lisp code that called a Scheme procedure"
		   (multiple-value-call #'values-to-lisp
		     (apply procedure (mapcar #'to-scheme args)))))))
	(setf (gethash f *originals*) procedure
	      (gethash procedure *lisp-facing*) f))))

(defun scheme-facing (function &optional (predicate t))
  "FUNCTION (a Lisp function) as a Scheme procedure: its arguments go to
Lisp (TO-LISP); if PREDICATE, a NIL result comes back as #f."
  (or (gethash function *originals*)
      (and predicate (gethash function *scheme-facing*))
      (let ((f (if predicate
		   (lambda (&rest args)
		     (let ((r (apply function (mapcar #'to-lisp args))))
		       (if (null r) ps:false r)))
		   (lambda (&rest args)
		     (apply function (mapcar #'to-lisp args))))))
	(setf (gethash f *originals*) function)
	(when predicate (setf (gethash function *scheme-facing*) f))
	f)))

(defun to-lisp (x)
  "A Scheme value passed to Lisp.  A condition a Scheme handler got for
a Lisp error is that Lisp condition again."
  (cond ((eq x ps:false) nil)
	((and (functionp x) (not (gethash x *verbatim*))) (lisp-facing x))
	((typep x 'structure-object) (or (ps-r6rs::foreign-original x) x))
	(t x)))

(defun to-scheme (x)
  "A Lisp value passed to Scheme."
  (cond ((and (functionp x) (not (gethash x *verbatim*))) (scheme-facing x t))
	(t x)))

;;; ------------------------------------------------------------------
;;; Which Lisp functions are predicates

(defparameter *lisp-predicates*
  '("EQ" "EQL" "EQUAL" "EQUALP" "NOT" "NULL" "ATOM"
    "=" "/=" "<" ">" "<=" ">="
    "CHAR=" "CHAR/=" "CHAR<" "CHAR>" "CHAR<=" "CHAR>=" "CHAR-EQUAL"
    "CHAR-NOT-EQUAL" "CHAR-LESSP" "CHAR-GREATERP" "CHAR-NOT-GREATERP" "CHAR-NOT-LESSP"
    "STRING=" "STRING/=" "STRING<" "STRING>" "STRING<=" "STRING>=" "STRING-EQUAL"
    "STRING-NOT-EQUAL" "STRING-LESSP" "STRING-GREATERP" "STRING-NOT-GREATERP"
    "STRING-NOT-LESSP"
    "MEMBER" "MEMBER-IF" "MEMBER-IF-NOT" "FIND" "FIND-IF" "FIND-IF-NOT"
    "POSITION" "POSITION-IF" "POSITION-IF-NOT" "ASSOC" "ASSOC-IF" "ASSOC-IF-NOT"
    "RASSOC" "RASSOC-IF" "RASSOC-IF-NOT" "SOME" "EVERY" "NOTANY" "NOTEVERY"
    "SEARCH" "MISMATCH" "ENDP" "TAILP" "LOGTEST" "LOGBITP" "BOUNDP" "FBOUNDP"
    "FIND-PACKAGE" "PROBE-FILE" "LISTEN" "MACRO-FUNCTION" "COMPILER-MACRO-FUNCTION"
    "READ-CHAR-NO-HANG" "STRING-PREFIX-P" "EMPTYP" "STARTS-WITH" "ENDS-WITH"
    "STARTS-WITH-SUBSEQ" "ENDS-WITH-SUBSEQ")
  "Lisp functions whose NIL result means false though their names don't
say so (ANSI's, and a few of Alexandria's).")

(defparameter *not-lisp-predicates* '("MAP" "STEP" "LOOP" "SLEEP" "DRIBBLE")
  "Names that look like predicates (end in P) but aren't.")

(defun lisp-predicate-name-p (name)
  "Does a Lisp function named NAME (a string) return NIL for false? By
convention: FOO-P, or a one-word FOOP, plus *LISP-PREDICATES*."
  (let ((n (length name)))
    (and (not (member name *not-lisp-predicates* :test #'string=))
	 (or (member name *lisp-predicates* :test #'string=)
	     (and (> n 2) (string= "-P" name :start2 (- n 2)))
	     (and (> n 1) (char= (char name (1- n)) #\P)
		  (not (find #\- name))
		  (alpha-char-p (char name (- n 2))))))))

;;; ------------------------------------------------------------------
;;; Loading Lisp systems

(defvar *autoload-lisp-systems* t
  "If true, importing (cl <package>) for a package that doesn't exist
loads the system of the same name with *LISP-SYSTEM-LOADER* first.")

(defun default-lisp-system-loader (system)
  "Quicklisp's QUICKLOAD if Quicklisp is loaded (it fetches systems it
doesn't have), otherwise ASDF:LOAD-SYSTEM (which finds systems on
ASDF's source registry: ~/common-lisp, ~/quicklisp/local-projects via
Quicklisp, an ocicl or qlot project's systems, CL_SOURCE_REGISTRY)."
  (let ((ql (find-package "QUICKLISP-CLIENT")))
    (if ql
	(funcall (find-symbol "QUICKLOAD" ql) system :silent t)
	(asdf:load-system system))))

(defvar *lisp-system-loader* 'default-lisp-system-loader
  "Function of one argument, a system name (a string), that loads it.")

(defun load-lisp-system (system)
  (funcall *lisp-system-loader* (string system))
  t)

(defun lisp-package-for (name)
  "The Lisp package named by the parts of (cl <part> ...): their names
joined with /, as package-inferred systems name packages."
  (let ((pname (format nil "~{~A~^/~}"
		       (mapcar (lambda (p) (if (symbolp p) (symbol-name p) (princ-to-string p)))
			       name))))
    (or (find-package pname)
	(and *autoload-lisp-systems*
	     (progn (handler-case (load-lisp-system (string-downcase pname))
		      (error (e)
			(error "(cl ~(~A~)): no Lisp package ~A, and loading system ~(~A~) failed: ~A"
			       pname pname pname
			       (substitute #\Space #\Newline (princ-to-string e)))))
		    (find-package pname)))
	(error "(cl ~(~A~)): there is no Lisp package ~A~:[~; (and loading system ~(~A~) didn't make one)~]"
	       pname pname *autoload-lisp-systems* pname))))

;;; ------------------------------------------------------------------
;;; (cl <package>) libraries

(defvar *cl-library-count* 0)

(defun host-global (n symbol)
  "The host global holding SYMBOL's wrapper in the Nth (cl ...) library."
  (format nil "%cl~D:~A:~A" n (package-name (symbol-package symbol)) (symbol-name symbol)))

(defvar *function-symbols* (make-hash-table :test 'eq :weakness :key)
  "Wrapper exported by a (cl ...) library -> its Lisp symbol, for
lisp-set!.")

(defvar *setters* (make-hash-table :test 'equal)
  "(symbol . number of arguments) -> compiled (lambda (value . args)
(setf (symbol . args) value)).")

(defun lisp-setf (accessor value args)
  "(setf (ACCESSOR . ARGS) VALUE), ACCESSOR a function from a (cl ...)
library or a Lisp symbol; any place Lisp's SETF knows works."
  (let* ((sym (cond ((gethash accessor *function-symbols*))
		    ((and (symbolp accessor) (not (eq (symbol-package accessor) ps:scheme-package))) accessor)
		    (t (ps:scheme-error "lisp-set!: not a Lisp accessor: ~S" accessor))))
	 (key (cons sym (length args)))
	 (setter (or (gethash key *setters*)
		     (setf (gethash key *setters*)
			   (let ((params (loop repeat (length args) collect (gensym))))
			     (compile nil `(lambda (value ,@params) (setf (,sym ,@params) value))))))))
    (apply setter (to-lisp value) (mapcar #'to-lisp args))))

(defun exportable-function-p (symbol)
  (and (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun cl-library-form (name)
  "The library (cl . NAME), for psyntax: installs a primitives library
of the package's functions (wrapped, see SCHEME-FACING) and returns a
library form that re-exports them, defines its variables as identifier
syntax over SYMBOL-VALUE, its macros and special operators as syntax
that compiles the call as Lisp (LISP-MACRO-TRANSFORMER), and every
other external symbol (types, classes, lambda-list keywords...) as
syntax for the symbol itself.  Inside a Lisp macro call each of these
is its Lisp symbol."
  (let* ((package (lisp-package-for name))
	 (functions '()) (variables '()) (macros '()) (others '()))
    (do-external-symbols (s package)
      (cond ((exportable-function-p s) (push s functions))
	    ((and (fboundp s) (or (macro-function s) (special-operator-p s))) (push s macros))
	    ((boundp s) (push s variables))
	    (t (push s others))))
    (setq functions (sort functions #'string< :key #'symbol-name)
	  variables (sort variables #'string< :key #'symbol-name)
	  macros (sort macros #'string< :key #'symbol-name)
	  others (sort others #'string< :key #'symbol-name))
    (let* ((n (incf *cl-library-count*))
	   (prims-name (list "pseudoscheme" "cl-primitives" (format nil "~A~D" (package-name package) n)))
	   (prims '()))
      (dolist (s functions)
	(let ((global (host-global n s)))
	  (let ((f (scheme-facing (fdefinition s) (lisp-predicate-name-p (symbol-name s)))))
	    (setf (gethash f *function-symbols*) s)
	    (psx:host-set! global f))
	  (push (cons (ps:invert-case (symbol-name s)) global) prims)))
      (ps-r7rs::install-host-library prims-name (nreverse prims))
      (labels ((scheme-name (s) (ssym (ps:invert-case (symbol-name s))))
	       (r (string) (ssym (concatenate 'string "%%r:" string))))
	(list* (ssym "library") (cons (ssym "cl") name)
	       (cons (ssym "export") (mapcar #'scheme-name (append functions variables macros others)))
	       (list (ssym "import")
		     (list (ssym "prefix") (list (ssym "rnrs")) (ssym "%%r:"))
		     (list (ssym "prefix")
			   (list (ssym "only") (list (ssym "pseudoscheme") (ssym "lisp"))
				 (ssym "lisp-macro-transformer") (ssym "lisp-variable-transformer")
				 (ssym "lisp-symbol-transformer"))
			   (ssym "%%lisp-m:"))
		     (list (ssym "prefix") (list (ssym "pseudoscheme") (ssym "lisp") (ssym "primitives"))
			   (ssym "%%lisp:"))
		     (mapcar #'ssym prims-name))
	       (append
		(mapcar (lambda (s)
			  (list (r "define-syntax") (scheme-name s)
				(list (ssym "%%lisp-m:lisp-variable-transformer")
				      (list (r "quote") s))))
			variables)
		(mapcar (lambda (s)
			  (list (r "define-syntax") (scheme-name s)
				(list (ssym "%%lisp-m:lisp-macro-transformer")
				      (list (r "quote") s))))
			macros)
		(mapcar (lambda (s)
			  (list (r "define-syntax") (scheme-name s)
				(list (ssym "%%lisp-m:lisp-symbol-transformer")
				      (list (r "quote") s))))
			others)))))))

(defun cl-library-hook (name)
  (when (and (consp name) (symbolp (car name))
	     (string= (ps:scheme-symbol-name (car name)) "cl")
	     (cdr name))
    (cl-library-form (cdr name))))

;;; ------------------------------------------------------------------
;;; Lisp macros (and special operators) used from Scheme
;;;
;;; (cl:loop for x in xs when (even? x) collect (f x)) is expanded by a
;;; transformer written in Scheme (lisp-macro-transformer in
;;; src/interop/lisp.sls) that walks the form and asks LISP-IDENTIFIER
;;; what each identifier is:
;;;
;;; * a Scheme variable (xs, even?, f): a placeholder, an uninterned
;;;   symbol of the same name.  The form is compiled as a Lisp function
;;;   of the placeholders, and the Scheme values are passed in -- as
;;;   TO-LISP converts them, so #f is NIL and procedures return NIL for
;;;   #f -- and called as functions through a MACROLET.
;;; * Scheme syntax with a Lisp counterpart (lambda, if, let, quote,
;;;   begin, set!, cond, ...), or a Lisp macro imported from a (cl ...)
;;;   library: that Lisp operator.
;;; * unbound (for, in, collect, and x, which LOOP binds): the Lisp
;;;   symbol of that name in the macro's package, else COMMON-LISP, else
;;;   the Scheme symbol itself.
;;;
;;; So inside a Lisp macro call the code is Lisp, with Scheme's
;;; variables and procedures visible.  The compiled function is the
;;; expansion: ((quote #<function>) xs even? f).

(defvar *lisp-macro-transformers* (make-hash-table :test 'eq :weakness :key)
  "Transformer procedure of a (cl ...) library's macro -> the Lisp
macro's symbol, so a nested use (cl:when inside cl:loop) is recognized.")

(defparameter *scheme-syntax-in-lisp*
  '(("lambda" . lambda) ("if" . if) ("quote" . quote) ("let" . let)
    ("let*" . let*) ("set!" . setq) ("begin" . progn) ("and" . and)
    ("or" . or) ("when" . when) ("unless" . unless) ("cond" . cond)
    ("case" . case) ("do" . do) ("else" . t) ("quasiquote" . quasiquote))
  "Scheme syntax allowed inside a Lisp macro call, and what it means there.")

(defvar *variable-marker* (make-symbol "SCHEME-VARIABLE"))

(defun lisp-name-symbol (symbol macro)
  "The Lisp symbol for an unbound Scheme identifier, the Scheme SYMBOL,
inside a call of the Lisp macro MACRO: the symbol of that name in the
macro's package (ITERATE's FOR) or in COMMON-LISP (FIRST), else SYMBOL
itself -- so a class or function defined from Scheme, (cl:defclass
circle ...), is named by the same symbol as Scheme's 'circle.  A name
beginning with a colon is a keyword, as in Lisp."
  (let ((name (symbol-name symbol)))
    ;; :foo inside Lisp code is Lisp's keyword
    (when (and (> (length name) 1) (char= (char name 0) #\:))
      (return-from lisp-name-symbol (intern (subseq name 1) "KEYWORD")))
    (or (let ((home (symbol-package macro)))
	  (and home (multiple-value-bind (s status) (find-symbol name home)
		      (and status s))))
	(multiple-value-bind (s status) (find-symbol name "COMMON-LISP")
	  (and status s))
	symbol)))

(defun lisp-identifier (id macro)
  "What identifier ID (a syntax object) means inside a call of the Lisp
macro MACRO: *VARIABLE-MARKER* for a Scheme variable, else a Lisp
symbol."
  (let* ((b (funcall (psx:host-ref "psyntax:identifier-binding") id))
	 (symbol (funcall (psx:host-ref "psyntax:syntax->datum") id))
	 (name (symbol-name symbol)))
    (cond ((and (symbolp b) (string= (ps:scheme-symbol-name b) "variable")) *variable-marker*)
	  ((and (symbolp b) (string= (ps:scheme-symbol-name b) "unbound")) (lisp-name-symbol symbol macro))
	  ((consp b)
	   (let ((kind (ps:scheme-symbol-name (car b))))
	     (cond ((string= kind "core-prim")
		    ;; a function of a (cl ...) library is its Lisp symbol
		    ;; (so it works as a type or class name too); any other
		    ;; primitive is a Scheme value
		    (let* ((global (cdr b))
			   (value (and (symbolp global) (boundp (psx:location global)) (psx:host-ref global))))
		      (or (and value (gethash value *function-symbols*))
			  *variable-marker*)))
		   ((member kind '("global-macro" "global-macro!") :test #'string=)
		    (let* ((loc (if (consp (cdr b)) (cddr b) (cdr b)))
			   (transformer (and (symbolp loc) (boundp (psx:location loc)) (psx:host-ref loc))))
		      ;; (the symbol may be NIL: cl:nil)
		      (multiple-value-bind (symbol found)
			  (if transformer (gethash transformer *lisp-macro-transformers*) (values nil nil))
			(if found
			    symbol
			    (ps:scheme-error "Scheme macro ~A can't be used inside a Lisp macro call"
					     (string-downcase name))))))
		   (t (let ((meaning (assoc (if (symbolp (cdr b)) (ps:scheme-symbol-name (cdr b)) "")
					    *scheme-syntax-in-lisp* :test #'string=)))
			(if meaning
			    (cdr meaning)
			    (ps:scheme-error "Scheme syntax ~A can't be used inside a Lisp macro call"
					     (ps:invert-case name))))))))
	  (t (lisp-name-symbol symbol macro)))))

(defun lisp-import-symbol (id)
  "If identifier ID is bound to an export of a (cl ...) library, that
export's Lisp symbol; the second value says whether it is one (the
symbol may be NIL)."
  (let ((b (funcall (psx:host-ref "psyntax:identifier-binding") id)))
    (when (consp b)
      (let ((kind (ps:scheme-symbol-name (car b))))
	(cond ((string= kind "core-prim")
	       (let* ((global (cdr b))
		      (value (and (symbolp global) (boundp (psx:location global)) (psx:host-ref global))))
		 (and value (gethash value *function-symbols*))))
	      ((member kind '("global-macro" "global-macro!") :test #'string=)
	       (let* ((loc (if (consp (cdr b)) (cddr b) (cdr b)))
		      (transformer (and (symbolp loc) (boundp (psx:location loc)) (psx:host-ref loc))))
		 (if transformer (gethash transformer *lisp-macro-transformers*) (values nil nil)))))))))

(defun process-compile-time (form placeholders)
  "Do what COMPILE-FILE does at compile time for FORM as a top-level
form: evaluate the bodies of its (EVAL-WHEN (:COMPILE-TOPLEVEL) ...)
forms, found through macros, PROGN and EVAL-WHEN.  A Scheme program or
library is expanded before any of it runs, so without this DEFVAR's
special proclamation, DEFMACRO, DEFSTRUCT or CFFI's DEFCSTRUCT, used from
Scheme, wouldn't be seen by the Lisp forms after them.  A body that
mentions a Scheme variable (one of PLACEHOLDERS) can't be evaluated
before the program runs, and is skipped; so is one that fails, since
some compile-time effects make sense only inside COMPILE-FILE (DEFUN's
note to the compiler) -- the form still runs when the program does."
  (labels ((mentions-placeholder-p (x)
	     (cond ((symbolp x) (member x placeholders))
		   ((consp x) (or (mentions-placeholder-p (car x)) (mentions-placeholder-p (cdr x))))))
	   (process (form)
	     ;; a macro that fails to expand is left for COMPILE to report
	     (let ((form (handler-case (macroexpand form) (error () nil))))
	       (when (consp form)
		 (case (car form)
		   (progn (mapc #'process (cdr form)))
		   (eval-when
		    (let ((situations (second form)) (body (cddr form)))
		      (if (intersection '(:compile-toplevel compile) situations)
			  (unless (mentions-placeholder-p body)
			    (handler-case (eval `(progn ,@body)) (error () nil)))
			  (when (intersection '(:load-toplevel load) situations)
			    (mapc #'process body))))))))))
    (process form)))

(defun compile-lisp-form (form placeholders)
  "A compiled function of PLACEHOLDERS (uninterned symbols standing for
Scheme variables) that evaluates the Lisp FORM.  Each argument is
converted with TO-LISP, and a placeholder in operator position calls
its value.  FORM's compile-time effects happen first, as in a file
(PROCESS-COMPILE-TIME)."
  (process-compile-time form placeholders)
  (let ((code `(lambda ,placeholders
		 (let ,(mapcar (lambda (p) `(,p (to-lisp ,p))) placeholders)
		   (declare (ignorable ,@placeholders))
		   (macrolet ,(mapcar (lambda (p) `(,p (&rest args) (list* 'funcall ',p args))) placeholders)
		     ,form)))))
    (handler-bind ((warning #'muffle-warning))
      (let ((*error-output* (make-broadcast-stream)))	; compiler notes
	(compile nil code)))))

;;; ------------------------------------------------------------------
;;; Scheme macros used from Lisp
;;;
;;; A syntax export of a library brought into Lisp (USE-LIBRARY,
;;; R7RS:IMPORT) becomes a Lisp macro.  Its expansion: the call, as
;;; Scheme, inside (lambda (p ...) <call>), where each p stands for a
;;; Lisp lexical variable the call mentions (found with &environment) or
;;; a Lisp function Scheme doesn't define; that lambda expanded by
;;; psyntax -- hygienically -- and translated to Lisp; and the Lisp
;;; expansion (funcall <translation> var ...), compiled with the
;;; caller's code.  Booleans cross as everywhere else.

(defun lexical-variable-p (symbol env)
  (eq (trivial-cltl2:variable-information symbol env) :lexical))

(defun local-function-p (symbol env)
  "Is SYMBOL a function FLET or LABELS binds in ENV?"
  (multiple-value-bind (kind local) (trivial-cltl2:function-information symbol env)
    (and (eq kind :function) local)))

(defun unbound-operator (e)
  "If E is psyntax's error for a call (f ...) of an unbound identifier f,
that identifier's symbol."
  (when (typep e 'ps-r7rs::uncaught-raise)
    (let ((c (ps-r7rs::uncaught-payload e)))
      (when (ps-r6rs::condition-p* c)
	(let ((message (ps-r6rs::component-of c "&message"))
	      (irritants (ps-r6rs::component-of c "&irritants")))
	  (when (and message irritants
		     (equal (svref (ps-r6rs::record-values message) 0) "unbound identifier"))
	    (let ((form (first (svref (ps-r6rs::record-values irritants) 0))))
	      (and (consp form) (symbolp (car form)) (car form)))))))))

(defun scheme-bound-p (symbol environment)
  "Does Scheme SYMBOL mean anything in psyntax ENVIRONMENT?"
  (handler-case (progn (psx:expand symbol environment) t)
    (error () nil)))

(defun boolean-constant-p (symbol name)
  "Is SYMBOL R7RS:NAME, R6RS:NAME or R5RS:NAME (FALSE or TRUE)?  (Those
packages are made later, in src/api.lisp.)"
  (and (string= (symbol-name symbol) name)
       (member (package-name (symbol-package symbol)) '("R7RS" "R6RS" "R5RS") :test #'string=)))

(defun scheme-macro-expansion (form env library macro-name names)
  "The Lisp expansion of FORM, a call of the Scheme macro MACRO-NAME (a
Scheme symbol) exported by LIBRARY (a psyntax library name), written in
Lisp with lexical environment ENV.  NAMES maps the Lisp symbols the
library's exports were given to the exports (so SRFI-26:<> is the
library's <>).

The arguments are Scheme code written in Lisp syntax, with Lisp's
meanings where they exist: a Lisp lexical variable is that variable, a
Lisp function name that function (wrapped as a (cl ...) library's
would be), a quoted datum Lisp data, NIL the empty list; other symbols
are Scheme identifiers (Scheme syntax, Scheme procedures that aren't
Lisp's, and variables the macro binds).

A function name is a Lisp function's when FLET or LABELS binds it, or it
is FBOUNDP.  A call of a name that is neither, and that Scheme doesn't
bind either, is a call of the global Lisp function of that name, looked
up when called, as in Lisp: the function DEFUN is defining, calling
itself, or one defined later.  (Such a call is found when psyntax
reports its name unbound, and the expansion is redone.)"
  (let ((environment (funcall (psx:host-ref "psyntax:environment")
			      (list (ssym "pseudoscheme") (ssym "r7rs"))
			      (list (ssym "prefix") library (ssym "%%lib:"))))
	(late-bound '())		; Lisp symbols named by unbound calls
	(origins (make-hash-table :test 'eq)) ; Scheme symbol -> Lisp symbol
	(params '()) (args '()))
    (labels ((lib-name (export) (ssym (concatenate 'string "%%lib:" (ps:scheme-symbol-name export))))
	     (param (key value)
	       (or (car (find key params :key #'cdr :test #'equal))
		   (let ((p (ps:intern-scheme-symbol (format nil "%%lisp-~D" (length params)))))
		     (push (cons p key) params)
		     (push value args)
		     p)))
	     (lisp-function-p (x)
	       (and (fboundp x) (not (macro-function x)) (not (special-operator-p x))))
	     (convert (x)
	       (typecase x
		 (null (list (ssym "quote") nil))
		 ((eql t) t)
		 (keyword x)
		 (symbol
		  (cond ((lexical-variable-p x env) (param x `(to-scheme ,x)))
			((assoc x names) (lib-name (cdr (assoc x names))))
			((eq (symbol-package x) ps:scheme-package) x)
			((boolean-constant-p x "FALSE") ps:false)
			((boolean-constant-p x "TRUE") t)
			((or (local-function-p x env) (lisp-function-p x))
			 (param (list 'function x)
				`(scheme-facing #',x ,(and (lisp-predicate-name-p (symbol-name x)) t))))
			((member x late-bound)
			 (param (list 'function x)
				`(scheme-facing (lambda (&rest args) (apply ',x args))
						,(and (lisp-predicate-name-p (symbol-name x)) t))))
			(t (let ((s (schemify x)))
			     (setf (gethash s origins) x)
			     s))))
		 (cons
		  (cond ((eq (car x) 'quote) (list (ssym "quote") (cadr x)))
			((and (eq (car x) 'function) (symbolp (cadr x)))
			 (param (list 'function (cadr x)) `(scheme-facing #',(cadr x) nil)))
			(t (cons (convert-head (car x)) (convert-args (cdr x))))))
		 (t x)))
	     ;; in operator position a symbol isn't (quote ()) for NIL
	     (convert-head (x) (if (null x) x (convert x)))
	     (convert-args (x)
	       (cond ((null x) nil)
		     ((consp x) (cons (convert (car x)) (convert-args (cdr x))))
		     (t (convert x)))))
      (loop
	(setf params '() args '())
	(let* ((call (cons (lib-name macro-name) (convert-args (cdr form))))
	       (lambda-form (list (ssym "lambda") (mapcar #'car (reverse params)) call))
	       (core (handler-case (psx::open-primitives (values (psx:expand lambda-form environment)))
		       (ps-r7rs::uncaught-raise (e)
			 (let ((lisp-symbol (gethash (unbound-operator e) origins)))
			   (if (and lisp-symbol (not (member lisp-symbol late-bound)))
			       (progn (push lisp-symbol late-bound) nil)
			       (error e)))))))
	  (when core
	    (return
	      `(multiple-value-call #'values-to-lisp
		 (funcall ,(scheme-translator:translate core psx:*host*) ,@(reverse args))))))))))

(defun define-scheme-macro (symbol library macro-name names)
  "Make SYMBOL a Lisp macro for Scheme macro MACRO-NAME of LIBRARY; NAMES
as for SCHEME-MACRO-EXPANSION."
  (setf (macro-function symbol)
	(lambda (form env)
	  (scheme-macro-expansion form env library macro-name names))))

;;; ------------------------------------------------------------------
;;; (pseudoscheme lisp): helpers for Scheme code

(defun lisp-name (x)
  "A Lisp symbol or package name from a Scheme string or symbol, by the
symbol case rule: \"equal\" and 'equal both name EQUAL."
  (etypecase x
    (string (ps:invert-case x))
    (symbol (symbol-name x))))

(defun lisp-symbol (name &optional (package "common-lisp-user"))
  (let ((p (or (find-package (lisp-name package))
	       (ps:scheme-error "lisp-symbol: no Lisp package ~A" (lisp-name package)))))
    (multiple-value-bind (s status) (find-symbol (lisp-name name) p)
      (if status s (ps:scheme-error "lisp-symbol: no symbol ~A in ~A" (lisp-name name) (package-name p))))))

(defun install-lisp-primitives ()
  (let ((prims '()))
    (macrolet ((prim (name lambda-list &body body)
		 `(let ((global (concatenate 'string "%lisp:" ,name)))
		    (psx:defhost global ,lambda-list ,@body)
		    (push (cons ,name global) prims))))
      (prim "lisp-true?" (x) (if (or (null x) (eq x ps:false)) ps:false t))
      (prim "lisp-false?" (x) (if (or (null x) (eq x ps:false)) t ps:false))
      (prim "lisp-symbol" (name &optional (package "common-lisp-user")) (lisp-symbol name package))
      (prim "lisp-keyword" (name) (intern (lisp-name name) "KEYWORD"))
      ;; The function itself: Scheme calls it with no conversion.  (Not
      ;; marked VERBATIM: that would change how the function object
      ;; crosses everywhere, e.g. #'evenp passed from Lisp.)
      (prim "lisp-function" (name &optional (package "common-lisp"))
	    ;; a Lisp symbol, or a Scheme symbol naming a function defined
	    ;; from Scheme (cl:defun ...), or a name in PACKAGE
	    (fdefinition (if (and (symbolp name)
				  (or (not (eq (symbol-package name) ps:scheme-package))
				      (fboundp name)))
			     name
			     (lisp-symbol name package))))
      (prim "lisp-funcall" (f &rest args) (apply (scheme-facing (to-lisp f) nil) args))
      (prim "lisp-apply" (f &rest args) (apply (scheme-facing (to-lisp f) nil) (apply #'list* args)))
      (prim "lisp-value" (s) (symbol-value s))
      (prim "set-lisp-value!" (s v) (setf (symbol-value s) (to-lisp v)) ps:unspecific)
      (prim "call-with-lisp-bindings" (symbols values thunk)
	    (progv symbols (mapcar #'to-lisp values) (funcall thunk)))
      (prim "lisp-eval-string" (string)
	    (let ((*package* (find-package "COMMON-LISP-USER")))
	      (eval (read-from-string string))))
      (prim "lisp-require" (system) (load-lisp-system (lisp-name system)) ps:unspecific)
      (prim "%lisp-setf!" (accessor value &rest args) (lisp-setf accessor value args) ps:unspecific)
      ;; for lisp-macro-transformer (src/interop/lisp.sls)
      (prim "%lisp-identifier" (id macro) (lisp-identifier id macro))
      (prim "%lisp-variable-marker?" (x) (if (eq x *variable-marker*) t ps:false))
      (prim "%lisp-placeholder" (name) (make-symbol (symbol-name name)))
      (prim "%lisp-literal" (x) (if (eq x ps:false) nil x))
      (prim "%compile-lisp-form" (form placeholders) (compile-lisp-form form placeholders))
      (prim "%register-lisp-macro!" (transformer macro)
	    ;; psyntax may store a variable transformer's procedure rather
	    ;; than the transformer object: register both
	    (setf (gethash transformer *lisp-macro-transformers*) macro)
	    (when (and (consp transformer) (functionp (cdr transformer)))
	      (setf (gethash (cdr transformer) *lisp-macro-transformers*) macro))
	    (when (and (consp transformer) (consp (cdr transformer)))
	      (dolist (x (cdr transformer))
		(when (functionp x) (setf (gethash x *lisp-macro-transformers*) macro))))
	    transformer)
      (prim "%lisp-import-symbol" (id)
	    (multiple-value-bind (symbol found) (lisp-import-symbol id)
	      (if found symbol ps:false)))
      (prim "verbatim" (f) (verbatim f)))
    (ps-r7rs::install-host-library '("pseudoscheme" "lisp" "primitives") (nreverse prims))
    (dolist (form (ps-r7rs::read-forms
		   (asdf:system-relative-pathname :pseudoscheme "src/interop/lisp.sls")))
      (psx:eval-library form))))

;;; ------------------------------------------------------------------
;;; Lisp calling Scheme

(defun parse-library-name (name)
  "A library name given from Lisp -- a list like (srfi 1) or
(|scheme| |base|), or a string \"(srfi 1)\" -- as psyntax's."
  (let ((name (if (stringp name)
		  (with-input-from-string (in name) (funcall ps:*scheme-read* in))
		  (schemify name))))
    (ps-r7rs::translate-name name)))

(defun library-package-name (name)
  "(srfi :1) -> \"SRFI-1\", (scheme list) -> \"SCHEME-LIST\"."
  (format nil "~{~A~^-~}"
	  (mapcar (lambda (p)
		    (let ((s (if (symbolp p) (symbol-name p) (princ-to-string p))))
		      (if (and (> (length s) 1) (char= (char s 0) #\:)) (subseq s 1) s)))
		  name)))

(defun export-bindings (name)
  "((symbol kind value) ...) for library NAME: KIND is :procedure,
:variable (VALUE a function returning the current value) or :syntax."
  (ps-r7rs::boot)
  (loop for (sym type val) in (funcall (psx:host-ref "psyntax:library-export-bindings") name)
	for tname = (ps:scheme-symbol-name type)
	collect (cond ((member tname '("global" "core-prim") :test #'string=)
		       ;; a global's value is (library . location)
		       (let ((getter (let ((v (if (consp val) (cdr val) val)))
				       (lambda () (psx:host-ref v)))))
			 (if (functionp (funcall getter))
			     (list sym :procedure (funcall getter))
			     (list sym :variable getter))))
		      (t (list sym :syntax nil)))))

(defun library-exports (name)
  "The exports of Scheme library NAME, as (name . kind) with KIND
:procedure, :variable or :syntax (names are Scheme symbols)."
  (mapcar (lambda (b) (cons (first b) (second b))) (export-bindings (parse-library-name name))))

(defun use-library (name &key package (convert t))
  "Make Scheme library NAME (e.g. '(srfi 1) or \"(srfi 1)\") usable from
Lisp as a package (default: named after the library, (srfi 1) ->
SRFI-1), exporting one symbol per procedure or variable: procedures
become functions, variables symbol macros reading their current value,
and syntax becomes Lisp macros (see SCHEME-MACRO-EXPANSION).  With CONVERT (the
default), functions convert booleans as described in this file's
header; with CONVERT NIL they are the Scheme procedures themselves.
Returns the package."
  (let* ((name (parse-library-name name))
	 (bindings (export-bindings name))
	 (pname (string (or package (library-package-name name))))
	 (pkg (or (find-package pname) (make-package pname :use '())))
	 (names (mapcar (lambda (b) (cons (intern (symbol-name (first b)) pkg) (first b))) bindings)))
    (loop for (sym kind value) in bindings
	  for s = (intern (symbol-name sym) pkg)
	  do (ecase kind
	       (:procedure
		(setf (fdefinition s) (if convert (lisp-facing value) value))
		(export s pkg))
	       (:variable
		(eval `(define-symbol-macro ,s
			   ,(if convert
				`(to-lisp (funcall ,value))
				`(funcall ,value))))
		(export s pkg))
	       (:syntax
		(define-scheme-macro s name sym names)
		(export s pkg))))
    pkg))

;;; ------------------------------------------------------------------
;;; R7RS:IMPORT: import sets into a Lisp package

(defun import-set-keyword-p (x name)
  (and (symbolp x) (string-equal (symbol-name x) name)))

(defun import-set-bindings (set)
  "The bindings an import set (R7RS syntax, written in Lisp) brings:
((lisp-name library scheme-export kind value) ...), LISP-NAME a string."
  (let ((head (and (consp set) (car set))))
    (flet ((names (xs) (mapcar #'symbol-name xs)))
      (cond ((import-set-keyword-p head "ONLY")
	     (let ((keep (names (cddr set))))
	       (remove-if-not (lambda (b) (member (first b) keep :test #'string=))
			      (import-set-bindings (second set)))))
	    ((import-set-keyword-p head "EXCEPT")
	     (let ((drop (names (cddr set))))
	       (remove-if (lambda (b) (member (first b) drop :test #'string=))
			  (import-set-bindings (second set)))))
	    ((import-set-keyword-p head "PREFIX")
	     (let ((prefix (symbol-name (third set))))
	       (mapcar (lambda (b) (cons (concatenate 'string prefix (first b)) (rest b)))
		       (import-set-bindings (second set)))))
	    ((import-set-keyword-p head "RENAME")
	     (let ((renames (mapcar (lambda (r) (cons (symbol-name (first r)) (symbol-name (second r))))
				    (cddr set))))
	       (mapcar (lambda (b)
			 (let ((r (assoc (first b) renames :test #'string=)))
			   (if r (cons (cdr r) (rest b)) b)))
		       (import-set-bindings (second set)))))
	    (t
	     (let ((library (parse-library-name set)))
	       (mapcar (lambda (b)
			 (destructuring-bind (sym kind value) b
			   (list (symbol-name sym) library sym kind value)))
		       (export-bindings library))))))))

(defun import-into-package (sets package-name &key (convert t))
  "Bring the bindings of import SETS into the package named
PACKAGE-NAME, as USE-LIBRARY does into a library's own package.  A name
that would capture a symbol the package inherits or imports (most often
from COMMON-LISP: FIND, REMOVE, MEMBER...) is an error: use except,
prefix or rename."
  (let* ((pkg (find-package package-name))
	 (bindings (loop for set in sets append (import-set-bindings set)))
	 (names (mapcar (lambda (b) (cons (intern (first b) pkg) (third b))) bindings)))
    (dolist (b bindings)
      (multiple-value-bind (s status) (find-symbol (first b) pkg)
	(when (and status (not (eq (symbol-package s) pkg)))
	  (error "Importing ~A into ~A would redefine ~S; use except, prefix or rename"
		 (ps:scheme-symbol-name (third b)) package-name s))))
    (dolist (b bindings)
      (destructuring-bind (lisp-name library export kind value) b
	(let ((s (intern lisp-name pkg)))
	  (ecase kind
	    (:procedure (setf (fdefinition s) (if convert (lisp-facing value) value)))
	    (:variable (eval `(define-symbol-macro ,s
				  ,(if convert `(to-lisp (funcall ,value)) `(funcall ,value)))))
	    (:syntax (define-scheme-macro s library export names))))))
    (mapcar (lambda (b) (intern (first b) pkg)) bindings)))

;;; ------------------------------------------------------------------
;;; Boot

(defvar *booted-host* nil)

(defun boot ()
  "Install (pseudoscheme lisp) and the (cl ...) hook, once per host."
  (ps-r7rs::boot)
  (unless (eq *booted-host* psx:*host*)
    (pushnew 'cl-library-hook ps-r7rs::*virtual-libraries*)
    (let ((psx::*full-continuations* nil))
      (install-lisp-primitives))
    (setq *booted-host* psx:*host*))
  t)
