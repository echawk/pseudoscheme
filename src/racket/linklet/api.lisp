; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Strategy B: the #%linklet primitive instance, the interface between
;;;; Racket's expander and a backend (docs/racket.md, "Linklets"):
;;;; linklets, instances and variable objects, correlated objects, and
;;;; the functions over them.

(in-package "QUASISCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Instances and variables

(defstruct (linklet-instance (:constructor make-linklet-instance (name &optional (data qs:false))))
  name data
  (variables (make-hash-table :test 'eq)))	; symbol -> LINKLET-VARIABLE

(defvar +undefined+ (make-symbol "UNDEFINED")
  "A variable's value before anything sets it.")

(defstruct (linklet-variable (:constructor make-linklet-variable (name &optional (value +undefined+))))
  name value
  (constant nil))			; set by instance-describe-variable! or analysis

(defun instance-variable (instance name &optional create)
  (or (gethash name (linklet-instance-variables instance))
      (and create
	   (setf (gethash name (linklet-instance-variables instance)) (make-linklet-variable name)))))

(defun instance-value (instance name &optional (default nil default-p))
  (let ((v (instance-variable instance name)))
    (cond ((and v (not (eq (linklet-variable-value v) +undefined+))) (linklet-variable-value v))
	  (default-p default)
	  (t (racket-error "instance-variable-value" "instance variable not found: ~A" (rname name))))))

(defun raise-variable-error (name message)
  "Raise exn:fail:contract:variable: NAME (a symbol) is undefined, with
MESSAGE's reason."
  (do-raise (make-exn "exn:fail:contract:variable"
		      (format nil "~A: undefined;~% ~A" (rname name) message)
		      name)))

(defun variable-ref (v)
  (let ((x (linklet-variable-value v)))
    (if (eq x +undefined+)
	(raise-variable-error (linklet-variable-name v) "cannot reference an identifier before its definition")
	x)))

;;; A letrec's variable used before its definition: the host's
;;; %letrec-check (psyntax.lisp) raises an R6RS condition; for the
;;; lexicals this compiler makes (FRESH-LEXICAL: NAME~N.TAG) Racket's
;;; exn:fail:contract:variable instead, named as Racket names it.
(defun racket-lexical-name (symbol)
  "The Racket name of SYMBOL, if it is one of FRESH-LEXICAL's, else NIL."
  (let* ((name (rname symbol))
	 (tilde (position #\~ name :from-end t))
	 (dot (and tilde (position #\. name :start tilde))))
    (when (and dot (> tilde 0) (> dot (1+ tilde))
	       (every #'digit-char-p (subseq name (1+ tilde) dot))
	       (< (1+ dot) (length name)) (char-equal (char name (1+ dot)) #\r))
      (let* ((stem (subseq name 0 tilde))
	     (us (position #\_ stem :from-end t)))
	;; the expander's NAME_N
	(if (and us (> us 0) (< (1+ us) (length stem)) (every #'digit-char-p (subseq stem (1+ us))))
	    (subseq stem 0 us)
	    stem)))))

(defvar *racket-letrec-check* nil
  "The host's %letrec-check as INSTALL-LETREC-CHECK last replaced it.")

(defun install-letrec-check ()
  "Replace the host's %letrec-check, unless this process already has."
  (unless (and *racket-letrec-check* (eq (qsx:host-ref "%letrec-check") *racket-letrec-check*))
    (let* ((old (qsx:host-ref "%letrec-check"))
	   (new (lambda (flag value name)
		  (let ((racket-name (and (eq flag qs:false) (racket-lexical-name name))))
		    (if racket-name
			(raise-variable-error (rsym racket-name) "cannot use before initialization")
			(funcall old flag value name))))))
      (qsx:host-set! "%letrec-check" new)
      (setq *racket-letrec-check* new))))

(defun variable-set (v value)
  (when (linklet-variable-constant v)
    (racket-error (rname (linklet-variable-name v)) "cannot re-define a constant"))
  (setf (linklet-variable-value v) value)
  *void*)

;;; (#%variable-reference) and (#%variable-reference x)
(defstruct (variable-reference (:constructor make-variable-reference (instance name)))
  instance name)

;;; ------------------------------------------------------------------
;;; Correlated objects: a datum with a source location and properties,
;;; the expander's syntax objects as linklets carry them

(defstruct (correlated (:constructor make-correlated (e &key source line column position span props)))
  e source line column position span
  (props '()))				; alist: key -> value

(defun correlated-source-location (c)
  (list (correlated-source c) (correlated-line c) (correlated-column c)
	(correlated-position c) (correlated-span c)))

(defun correlated-property (c key)
  (cdr (assoc key (correlated-props c))))

(defun correlated->datum (x)
  "X without its correlated objects: in pairs, vectors, boxes, immutable
hash tables and prefab structs too; what holds none is returned as is."
  (cond ((correlated-p x) (correlated->datum (correlated-e x)))
	((consp x) (let ((a (correlated->datum (car x))) (d (correlated->datum (cdr x))))
		     (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
	((simple-vector-p x)
	 (let ((new (map 'simple-vector #'correlated->datum x)))
	   (if (every #'eq new x) x new)))
	((rbox-p x)
	 (let ((v (correlated->datum (rbox-value x))))
	   (if (eq v (rbox-value x)) x (make-rbox :value v))))
	((ihash-p x)
	 (let ((changed nil) (new (empty-ihash (ihash-kind x))))
	   (loop for (k . v) across (ihash-entries x)
		 do (let ((k2 (correlated->datum k)) (v2 (correlated->datum v)))
		      (unless (and (eq k k2) (eq v v2)) (setq changed t))
		      (setq new (ihash-set new k2 v2))))
	   (if changed new x)))
	((and (rstruct-p x) (struct-type-prefab-key (rstruct-type x)))
	 (let ((new (map 'simple-vector #'correlated->datum (rstruct-fields x))))
	   (if (every #'eq new (rstruct-fields x)) x (make-rstruct (rstruct-type x) new))))
	(t x)))

;;; ------------------------------------------------------------------
;;; Linklets: compile, instantiate

(defvar *stats* (list :lower 0 :compile 0 :linklets 0)
  "Time spent lowering linklets and compiling them to Lisp (internal
time units), and how many: QUASISCHEME_RACKET_STATS prints them.")

(defmacro timing ((key) &body body)
  `(let ((t0 (get-internal-real-time)))
     (multiple-value-prog1 (progn ,@body)
       (incf (getf *stats* ,key) (- (get-internal-real-time) t0)))))

(defun print-stats ()
  (when (uiop:getenv "QUASISCHEME_RACKET_STATS")
    (format *error-output* "~&;; ~D linklets: lowered in ~,1Fs, compiled to Lisp in ~,1Fs~%"
	    (getf *stats* :linklets)
	    (/ (getf *stats* :lower) internal-time-units-per-second)
	    (/ (getf *stats* :compile) internal-time-units-per-second))))

(defun undumpable-literal-p (core)
  "Whether lowered CORE quotes a value a fasl can't hold: a procedure, or
another object without a load form.  A macro can put such a value into
the code it makes (Racket's \"3-D syntax\"), which Racket compiles in
memory, as is done for it here, uncached."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((value (x)
	       (cond ((or (numberp x) (stringp x) (symbolp x) (characterp x)) nil)
		     ((gethash x seen) nil)
		     (t (setf (gethash x seen) t)
			(typecase x
			  (cons (or (value (car x)) (value (cdr x))))
			  (simple-vector (some #'value x))
			  (function t)
			  ((or structure-object standard-object)
			   (handler-case (progn (make-load-form x) nil) (error () t)))
			  (t nil)))))
	     (walk (x)
	       (cond ((atom x) nil)
		     ((eq (car x) (core "quote")) (value (cadr x)))
		     (t (loop for y on x thereis (and (consp y) (walk (car y))))))))
      (walk core))))

(defun compile-linklet (form &optional name key imports-known)
  "compile-linklet: FORM (an S-expression or correlated) to a linklet.
KEY, from a .zo, is the cache key it was compiled under.  IMPORTS-KNOWN:
per import set, the safe exports of the linklet it imports
(IMPORTS-SAFE-EXPORTS)."
  (incf (getf *stats* :linklets))
  (install-letrec-check)
  ;; its machine code and what lowering it says cached already
  (when key
    (let ((compiled (make-compiled-linklet :deferred :name name :strategy "linklet")))
      (setf (compiled-linklet-source-key compiled) key
	    (compiled-linklet-source-form compiled) form
	    (compiled-linklet-imports-known compiled) imports-known)
      (when (linklet-from-meta compiled)
	(return-from compile-linklet compiled))))
  (timing (:lower)
    (let* ((form (if (functionp form) (funcall form) form)) ; a .zo's (READ-LINKLET-BUNDLE)
	   (compiled (compile-linklet-form form :name name :imports-known imports-known)))
      ;; compiled in memory if it can't be cached (UNDUMPABLE-LITERAL-P)
      (when (undumpable-literal-p (compiled-linklet-form compiled))
	(setf (compiled-linklet-source-form compiled) form
	      (compiled-linklet-imports-known compiled) imports-known)
	(return-from compile-linklet compiled))
      ;; its compiled code is cached, keyed by the linklet as given and
      ;; the imports it was told are safe (its code depends on them)
      (unless (compiled-linklet-source-key compiled)
	(setf (compiled-linklet-source-key compiled)
	      (or key (if (some #'identity imports-known)
			  (linklet-hash (list form imports-known))
			  (linklet-hash form)))))
      (setf (compiled-linklet-source-form compiled) form
	    (compiled-linklet-imports-known compiled) imports-known)
      (write-linklet-meta compiled)
      compiled)))

(defvar *instance-safe-exports* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A module's linklet instance -> its linklet's safe exports.")

(defun module-body-linklet-p (compiled)
  "Whether COMPILED is a module's body (its name a hasheq whose module is
not #f), not a top-level form's, whose namespace may define its names
again."
  (let ((name (compiled-linklet-name compiled)))
    (and (or (rhash-p name) (ihash-p name))
	 (multiple-value-bind (m found) (lookup* name (rsym "module"))
	   (and found (truthy m))))))

(defun note-instance-safe-exports (compiled instance)
  "Record, for the linklets compiled against INSTANCE, which of the
variables COMPILED, a module body, defines in it are safe: each
definition replaces what was known of the name."
  (when (module-body-linklet-p compiled)
    (let ((safe (compiled-linklet-safe-exports compiled))
	  (known (gethash instance *instance-safe-exports*)))
      (setq known (set-difference known (compiled-linklet-exports compiled)))
      (setf (gethash instance *instance-safe-exports*) (union safe known)))))

(defun imports-safe-exports (import-keys get-import)
  "Per import set, the safe exports (COMPILED-LINKLET-SAFE-EXPORTS) of the
linklet or module instance GET-IMPORT gives for its key in IMPORT-KEYS, a
vector; NIL where there is none.  Turned off by
QUASISCHEME_RACKET_SAFE_IMPORTS=0."
  (when (and (functionp get-import) (vectorp import-keys)
	     (not (equal (uiop:getenv "QUASISCHEME_RACKET_SAFE_IMPORTS") "0")))
    (loop for key across import-keys
	  collect (and (truthy key)
		       (let ((l (ignore-errors (funcall get-import key))))
			 (cond ((compiled-linklet-p l) (compiled-linklet-safe-exports l))
			       ((linklet-instance-p l) (values (gethash l *instance-safe-exports*)))))))))

(defun linklet-text (form)
  "FORM, a linklet, printed without its correlated objects' locations:
what identifies it in the cache."
  (with-standard-io-syntax
    (let ((*package* (find-package "SCHEME")) (*print-circle* t) (*print-readably* nil))
      (prin1-to-string (correlated->datum form)))))

(defun linklet-hash (form)
  "A 64-bit digest of FORM, a linklet, without its correlated objects'
locations: what identifies it in the cache.  Printing it (LINKLET-TEXT)
and hashing the text took a tenth of a warm start's time, SBCL's printer
checking each symbol's name for normalization; this walks the tree.  Each
uninterned symbol counts by its name and the order it first appears in,
as printing with *PRINT-CIRCLE* labels them; a leaf of a kind not listed
is hashed by its printed text."
  (let ((hash #xcbf29ce484222325)
	(scheme (find-package "SCHEME"))
	(uninterned (make-hash-table :test 'eq)))
    (declare (type (unsigned-byte 64) hash))
    (labels ((mix (n)
	       (setq hash (ldb (byte 64 0) (* (logxor hash (ldb (byte 64 0) n)) #x100000001b3))))
	     (mix-string (string)
	       (mix (length string))
	       (loop for c across string do (mix (char-code c))))
	     (walk (x)
	       (loop
		 (cond ((consp x) (mix 1) (walk (car x)) (setq x (cdr x)))
		       ((null x) (mix 2) (return))
		       ((symbolp x)
			(let ((package (symbol-package x)))
			  (cond ((null package)
				 (mix 3)
				 (mix (or (gethash x uninterned)
					  (setf (gethash x uninterned) (hash-table-count uninterned))))
				 (mix-string (symbol-name x)))
				(t (mix 4)
				   (unless (eq package scheme)
				     (mix-string (package-name package)))
				   (mix-string (symbol-name x)))))
			(return))
		       ((typep x 'fixnum) (mix 5) (mix x) (return))
		       ((stringp x) (mix 6) (mix-string x) (return))
		       ((characterp x) (mix 7) (mix (char-code x)) (return))
		       ((simple-vector-p x) (mix 8) (mix (length x)) (map nil #'walk x) (return))
		       (t (mix 9)
			  (mix-string (with-standard-io-syntax
					(let ((*package* (find-package "SCHEME")) (*print-circle* t)
					      (*print-readably* nil))
					  (prin1-to-string x))))
			  (return))))))
      (walk (correlated->datum form)))
    hash))

(defun linklet-function (compiled)
  "COMPILED's instantiation function, of the instance and the variable
objects: compiled now if need be."
  (or (compiled-linklet-function compiled)
      (setf (compiled-linklet-function compiled)
	    (timing (:compile)
	      ;; SBCL's notes, and its summary when a form too big for it
	      ;; aborts a compilation unit (retried with compact frames),
	      ;; aren't the program's output
	      (let ((*error-output* (make-broadcast-stream)))
		(linklet-function-1 compiled))))))

(defun linklet-function-1 (compiled)
  (let ((qsx::*full-continuations* t)
	(shape (linklet-shape compiled)))
    (call-with-safe-cells
     (lambda ()
       (if (eq (car shape) :split)
	   (split-function (second shape) (split-functions compiled))
	   (or (whole-function compiled)
	       (if (compiled-linklet-source-form compiled)
		   (let ((split (split-again compiled)))
		     (split-function (cadr (compiled-linklet-form split)) (split-functions split)))
		   (qsx:host-eval (linklet-form compiled)))))))))

(defun split-form-p (form)
  (and (consp form) (eq (car form) (qsx::sym "racket:split"))))

(defun split-again (compiled)
  "COMPILED, a whole linklet SBCL can't compile (code past arm64's
branch reach, or an SBCL failure: \"full call to DATA-VECTOR-REF\"),
lowered again split, so that its forms are compiled one by one: one
form that fails is interpreted, not the whole linklet.  Its fasl has a
key of its own (another strategy)."
  (let ((split (compile-linklet-form (linklet-source-form compiled)
				     :name (compiled-linklet-name compiled)
				     :imports-known (compiled-linklet-imports-known compiled)
				     :split t)))
    (setf (compiled-linklet-strategy split) "linklet-split"
	  (compiled-linklet-source-key split) (compiled-linklet-source-key compiled))
    split))

(defun split-function (n functions)
  "A split linklet's instantiation: the cells (the variables given, then
new ones for the rest), and each form's function in turn."
  (lambda (instance &rest variables)
    (let ((cells (make-array n)))
      (replace cells variables)
      (loop for i from (length variables) below n
	    do (setf (svref cells i) (make-linklet-variable (rsym "internal"))))
      (let ((result (list *void*)))
	(loop for f in functions
	      for i from 0
	      do (setq *split-form-index* i
		       result (multiple-value-list (funcall f instance cells))))
	(values-list result)))))

(defvar *split-form-index* nil
  "The index of the top-level form a split linklet's instantiation is
running (or ran last): for error reports.")

(defun instantiate-linklet (compiled import-instances &optional target)
  "instantiate-linklet: with no TARGET, a new instance with the exports;
with one, the body's result, the exports defined in TARGET."
  (unless (and (listp import-instances) (every #'linklet-instance-p import-instances))
    (racket-error "instantiate-linklet" "contract violation~%  expected: (listof instance?)~%  given: ~A"
		  (written import-instances)))
  (unless (or (null target) (linklet-instance-p target))
    (racket-error "instantiate-linklet" "contract violation~%  expected: (or/c instance? #f)~%  given: ~A"
		  (written target)))
  (unless (= (length import-instances) (length (compiled-linklet-imports compiled)))
    (racket-error "instantiate-linklet" "expected ~D import instances, given ~D"
		  (length (compiled-linklet-imports compiled)) (length import-instances)))
  (let* ((instance (or target (make-linklet-instance (compiled-linklet-name compiled))))
	 (imports (loop for names in (compiled-linklet-imports compiled)
			for from in import-instances
			append (loop for n in names
				     collect (or (instance-variable from n)
						 (racket-error "instantiate-linklet"
							       "variable not found in imported instance: ~A"
							       (rname n))))))
	 (exports (prog1 (loop for n in (compiled-linklet-exports compiled)
			       collect (instance-variable instance n t))
		    (note-instance-safe-exports compiled instance)))
	 (result (multiple-value-list
		  ;; the frames a continuation captured in the body can reach:
		  ;; the body's (inside an enclosing base, all of that one's)
		  (qsx::call-with-continuation-base
		   (lambda () (apply (linklet-function compiled) instance (append imports exports)))))))
    (if target (values-list result) instance)))

;;; ------------------------------------------------------------------
;;; #%linklet's names: those written so far, and stubs for the rest

(defstruct (linklet-bundle (:constructor make-linklet-bundle (table))) table)
(defstruct (linklet-directory (:constructor make-linklet-directory (table))) table)

(defparameter *linklet-primitives*
  '("primitive-table" "primitive->compiled-primitive" "primitive-in-category?"
    "primitive-lookup" "linklet?" "compile-linklet" "recompile-linklet" "eval-linklet"
    "instantiate-linklet" "linklet-import-variables" "linklet-export-variables"
    "linklet-directory?" "hash->linklet-directory" "linklet-directory->hash"
    "linklet-bundle?" "hash->linklet-bundle" "linklet-bundle->hash"
    "linklet-virtual-machine-bytes" "write-linklet-bundle-hash" "read-linklet-bundle-hash"
    "instance?" "make-instance" "instance-name" "instance-data" "instance-variable-names"
    "instance-variable-value" "instance-set-variable-value!" "instance-unset-variable!"
    "instance-describe-variable!" "linklet-performance-init!" "linklet-performance-report!"
    "variable-reference?" "variable-reference->instance" "variable-reference-constant?"
    "variable-reference-from-unsafe?" "correlated?" "correlated-source" "correlated-line"
    "correlated-column" "correlated-position" "correlated-span" "correlated-e"
    "correlated->datum" "datum->correlated" "correlated-property" "correlated-property-symbol-keys"
    ;; what BC's expander also uses (racket/src/bc/src/startup.inc)
    "linklet-cross-machine-type" "linklet-add-target-machine-info"
    "linklet-summarize-target-machine-info" "primitive->compiled-position"
    "compiled-position->primitive")
  "A first list of #%linklet's names, from Racket's reference (\"Linklets
and the Core Compiler\") and its correlated objects; milestone 1 replaces
it with (primitive-table '#%linklet) of the pinned release.")

(defvar *interpret-quick-linklets* (not (equal (uiop:getenv "QUASISCHEME_RACKET_INTERPRET") "0"))
  "Whether a linklet compiled 'quick is interpreted (schemify-layer.lisp),
not compiled; QUASISCHEME_RACKET_INTERPRET=0 compiles it.")

(defparameter *linklet-functions*
  `(("linklet?" ,(lambda (x) (bool (compiled-linklet-p x))))
    ("compile-linklet" ,(lambda (form &optional (name qs:false) (import-keys qs:false) get-import options)
			  ;; one compiled 'quick (run once at expand time) is
			  ;; interpreted, as Racket CS does (schemify-layer.lisp)
			  (let ((interpreted (and (consp options) (member (rsym "quick") options)
						  *interpret-quick-linklets*
						  (interpreted-linklet form name))))
			    (cond ((and interpreted (truthy import-keys)) (values interpreted import-keys))
				  (interpreted interpreted)
				  ;; with import keys, they come back too, unchanged; the
				  ;; linklets they name tell which of their exports are safe
				  ((truthy import-keys)
				   (values (compile-linklet form name nil (imports-safe-exports import-keys get-import))
					   import-keys))
				  (t (compile-linklet form name))))))
    ("eval-linklet" ,#'identity)
    ("instantiate-linklet" ,(lambda (linklet imports &optional (target qs:false) use-prompt)
			      (declare (ignore use-prompt))
			      (instantiate-linklet linklet imports (and (truthy target) target))))
    ("linklet-import-variables" ,(lambda (l) (compiled-linklet-imports l)))
    ("linklet-export-variables" ,(lambda (l) (compiled-linklet-exports l)))
    ("instance?" ,(lambda (x) (bool (linklet-instance-p x))))
    ("make-instance" ,(lambda (name &optional (data qs:false) mode &rest pairs)
			(declare (ignore mode))
			(let ((i (make-linklet-instance name data)))
			  (loop for (n v) on pairs by #'cddr
				do (setf (linklet-variable-value (instance-variable i n t)) v))
			  i)))
    ("instance-name" ,#'linklet-instance-name)
    ("instance-data" ,#'linklet-instance-data)
    ("instance-variable-names"
     ,(lambda (i) (loop for n being the hash-keys of (linklet-instance-variables i)
			  using (hash-value v)
			unless (eq (linklet-variable-value v) +undefined+) collect n)))
    ("instance-variable-value"
     ,(lambda (i name &optional (fail nil fail-p))
	(if fail-p
	    (let ((x (instance-value i name +undefined+)))
	      (cond ((not (eq x +undefined+)) x)
		    ((functionp fail) (funcall fail))
		    (t fail)))
	    (instance-value i name))))
    ("instance-set-variable-value!"
     ,(lambda (i name value &optional mode)
	(let ((v (instance-variable i name t)))
	  (setf (linklet-variable-value v) value)
	  (when (and mode (truthy mode)) (setf (linklet-variable-constant v) t))
	  *void*)))
    ("instance-unset-variable!"
     ,(lambda (i name)
	(let ((v (instance-variable i name)))
	  (when v (setf (linklet-variable-value v) +undefined+))
	  *void*)))
    ("variable-reference?" ,(lambda (x) (bool (variable-reference-p x))))
    ("variable-reference->instance"
     ,(lambda (r &optional (ref-site qs:false))
	;; an anonymous reference, (#%variable-reference), has no
	;; variable's instance; the instance it occurs in is REF-SITE's
	(if (or (truthy ref-site) (truthy (variable-reference-name r)))
	    (variable-reference-instance r)
	    qs:false)))
    ("variable-reference-from-unsafe?" ,(lambda (r) (declare (ignore r)) qs:false))
    ("correlated?" ,(lambda (x) (bool (correlated-p x))))
    ("correlated-e" ,#'correlated-e)
    ("correlated-source" ,(lambda (c) (or (correlated-source c) qs:false)))
    ("correlated-line" ,(lambda (c) (or (correlated-line c) qs:false)))
    ("correlated-column" ,(lambda (c) (or (correlated-column c) qs:false)))
    ("correlated-position" ,(lambda (c) (or (correlated-position c) qs:false)))
    ("correlated-span" ,(lambda (c) (or (correlated-span c) qs:false)))
    ("correlated->datum" ,#'correlated->datum)
    ("datum->correlated"
     ,(lambda (d &optional srcloc prop)
	(declare (ignore srcloc prop))
	(if (correlated-p d) d (make-correlated d))))
    ("correlated-property"
     ,(lambda (c key &optional (value nil value-p))
	(if value-p
	    (make-correlated (correlated-e c) :source (correlated-source c) :line (correlated-line c)
			     :column (correlated-column c) :position (correlated-position c)
			     :span (correlated-span c)
			     :props (acons key value (remove key (correlated-props c) :key #'car)))
	    (let ((p (assoc key (correlated-props c)))) (if p (cdr p) qs:false)))))
    ;; the interned symbols
    ("correlated-property-symbol-keys"
     ,(lambda (c) (loop for (k) in (correlated-props c) when (and (symbolp k) (symbol-package k)) collect k)))
    ("primitive-table"
     ,(lambda (name &optional table)
	(declare (ignore table))
	(let ((instance (gethash (rname name) *primitive-instances*)))
	  (if instance
	      (let ((h (empty-ihash :eq)))
		(maphash (lambda (k v) (setq h (ihash-set h (rsym k) v))) instance)
		h)
	      qs:false))))
    ("primitive-lookup" ,(lambda (name) (multiple-value-bind (v found) (primitive (rname name))
					  (if found v qs:false))))
    ("instance-describe-variable!"
     ,(lambda (i name desc)
	(declare (ignore desc))
	(instance-variable i name t)	; a description is a hint for the compiler
	*void*))
    ("variable-reference-constant?"
     ,(lambda (r)
	(let* ((name (variable-reference-name r))
	       (v (and (truthy name) (instance-variable (variable-reference-instance r) name))))
	  (bool (and v (linklet-variable-constant v))))))
    ("write-linklet-bundle-hash" ,(lambda (h port) (write-linklet-bundle h port)))
    ("read-linklet-bundle-hash" ,(lambda (port &optional cross) (declare (ignore cross))
				   (read-linklet-bundle port)))
    ;; our .zo's VM: compiled code here is ours alone (cache.lisp)
    ("linklet-virtual-machine-bytes" ,(lambda () (ascii-bytes *vm-name*)))
    ;; bundles (a linklet per phase, and data) and directories (of bundles)
    ("hash->linklet-bundle" ,(lambda (h) (make-linklet-bundle h)))
    ("linklet-bundle->hash" ,(lambda (b) (linklet-bundle-table b)))
    ("linklet-bundle?" ,(lambda (x) (bool (linklet-bundle-p x))))
    ("hash->linklet-directory" ,(lambda (h) (make-linklet-directory h)))
    ("linklet-directory->hash" ,(lambda (d) (linklet-directory-table d)))
    ("linklet-directory?" ,(lambda (x) (bool (linklet-directory-p x))))
    ;; one machine: no cross-compilation, and no machine-specific info
    ("linklet-cross-machine-type" ,(lambda (&optional target) (declare (ignore target)) qs:false))
    ("linklet-add-target-machine-info" ,(lambda (linklet info) (declare (ignore info)) linklet))
    ("linklet-summarize-target-machine-info" ,(lambda (linklet) (declare (ignore linklet)) qs:false))
    ("primitive->compiled-position" ,(lambda (p) (declare (ignore p)) qs:false))
    ("compiled-position->primitive" ,(lambda (i) (declare (ignore i)) qs:false)))
  "#%linklet's functions written so far, as (name function).")

(defun install-linklet-primitives ()
  "B: #%linklet: the functions written so far, stubs for the rest."
  (dolist (name *linklet-primitives*)
    (let ((name name)
	  (f (second (assoc name *linklet-functions* :test #'string=))))
      (define-primitive "#%linklet" name
	(or f (lambda (&rest args) (declare (ignore args)) (not-yet name)))))))

;;; #%kernel's syntax objects are correlated objects, as in Racket CS's
;;; rumble: the expander takes these from the kernel table for the
;;; syntax objects of the runtime layers (its own are its own structs)
(defnative "syntax?" (x) (:leaf t) (bool (correlated-p x)))
(defnative "syntax-e" (c) (:leaf t) (correlated-e c))
(defnative "syntax->datum" (c) (:leaf t) (correlated->datum c))
(defnative "datum->syntax" (ctx d &optional srcloc prop ignored) (:leaf t)
  (declare (ignore ctx srcloc prop ignored))
  (if (correlated-p d) d (make-correlated d)))
(defnative "syntax-source" (c) (:leaf t) (or (correlated-source c) qs:false))
(defnative "syntax-line" (c) (:leaf t) (or (correlated-line c) qs:false))
(defnative "syntax-column" (c) (:leaf t) (or (correlated-column c) qs:false))
(defnative "syntax-position" (c) (:leaf t) (or (correlated-position c) qs:false))
(defnative "syntax-span" (c) (:leaf t) (or (correlated-span c) qs:false))
(defnative "syntax-property" (c key &optional (value nil value-p) preserved?) (:leaf t)
  (declare (ignore preserved?))
  (if value-p
      (funcall (second (assoc "correlated-property" *linklet-functions* :test #'string=)) c key value)
      (let ((p (assoc key (correlated-props c)))) (if p (cdr p) qs:false))))
(defnative "syntax-property-symbol-keys" (c) (:leaf t) (mapcar #'car (correlated-props c)))

(defmethod make-load-form ((x correlated) &optional environment)
  (make-load-form-saving-slots x :environment environment))
