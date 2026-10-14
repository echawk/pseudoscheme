; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B: the #%linklet primitive instance, the interface between
;;;; Racket's expander and a backend (docs/racket.md, "Linklets"):
;;;; linklets, instances and variable objects, correlated objects, and
;;;; the functions over them.

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Instances and variables

(defstruct (linklet-instance (:constructor make-linklet-instance (name &optional (data ps:false))))
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

(defun variable-ref (v)
  (let ((x (linklet-variable-value v)))
    (if (eq x +undefined+)
	(racket-error (rname (linklet-variable-name v)) "undefined;~% cannot reference an identifier before its definition")
	x)))

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
  (cond ((correlated-p x) (correlated->datum (correlated-e x)))
	((consp x) (let ((a (correlated->datum (car x))) (d (correlated->datum (cdr x))))
		     (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
	(t x)))

;;; ------------------------------------------------------------------
;;; Linklets: compile, instantiate

(defvar *stats* (list :lower 0 :compile 0 :linklets 0)
  "Time spent lowering linklets and compiling them to Lisp (internal
time units), and how many: PSEUDOSCHEME_RACKET_STATS prints them.")

(defmacro timing ((key) &body body)
  `(let ((t0 (get-internal-real-time)))
     (multiple-value-prog1 (progn ,@body)
       (incf (getf *stats* ,key) (- (get-internal-real-time) t0)))))

(defun print-stats ()
  (when (uiop:getenv "PSEUDOSCHEME_RACKET_STATS")
    (format *error-output* "~&;; ~D linklets: lowered in ~,1Fs, compiled to Lisp in ~,1Fs~%"
	    (getf *stats* :linklets)
	    (/ (getf *stats* :lower) internal-time-units-per-second)
	    (/ (getf *stats* :compile) internal-time-units-per-second))))

(defun compile-linklet (form &optional name)
  "compile-linklet: FORM (an S-expression or correlated) to a linklet."
  (incf (getf *stats* :linklets))
  (timing (:lower)
    (let ((compiled (compile-linklet-form form :name name)))
      ;; its compiled code is cached, keyed by the linklet as given
      (unless (compiled-linklet-source-key compiled)
	(setf (compiled-linklet-source-key compiled) (linklet-text form)))
      (setf (compiled-linklet-source-form compiled) form)
      compiled)))

(defun linklet-text (form)
  "FORM, a linklet, printed without its correlated objects' locations:
what identifies it in the cache."
  (with-standard-io-syntax
    (let ((*package* (find-package "SCHEME")) (*print-circle* t) (*print-readably* nil))
      (prin1-to-string (correlated->datum form)))))

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
  (let ((psx::*full-continuations* t)
	(form (compiled-linklet-form compiled)))
    (call-with-safe-cells
     (lambda ()
       (if (and (consp form) (eq (car form) (psx::sym "racket:split")))
	   (split-function (cadr form) (split-functions compiled))
	   (whole-function compiled))))))

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
	 (exports (loop for n in (compiled-linklet-exports compiled)
			collect (instance-variable instance n t)))
	 (result (multiple-value-list
		  (apply (linklet-function compiled) instance (append imports exports)))))
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

(defparameter *linklet-functions*
  `(("linklet?" ,(lambda (x) (bool (compiled-linklet-p x))))
    ("compile-linklet" ,(lambda (form &optional (name ps:false) (import-keys ps:false) get-import options)
			  (declare (ignore get-import options))
			  ;; with import keys, they come back too (we use no
			  ;; cross-linklet knowledge yet, so unchanged)
			  (if (truthy import-keys)
			      (values (compile-linklet form name) import-keys)
			      (compile-linklet form name))))
    ("eval-linklet" ,#'identity)
    ("instantiate-linklet" ,(lambda (linklet imports &optional (target ps:false) use-prompt)
			      (declare (ignore use-prompt))
			      (instantiate-linklet linklet imports (and (truthy target) target))))
    ("linklet-import-variables" ,(lambda (l) (compiled-linklet-imports l)))
    ("linklet-export-variables" ,(lambda (l) (compiled-linklet-exports l)))
    ("instance?" ,(lambda (x) (bool (linklet-instance-p x))))
    ("make-instance" ,(lambda (name &optional (data ps:false) mode &rest pairs)
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
     ,(lambda (r &optional (ref-site ps:false))
	;; an anonymous reference, (#%variable-reference), has no
	;; variable's instance; the instance it occurs in is REF-SITE's
	(if (or (truthy ref-site) (truthy (variable-reference-name r)))
	    (variable-reference-instance r)
	    ps:false)))
    ("variable-reference-from-unsafe?" ,(lambda (r) (declare (ignore r)) ps:false))
    ("correlated?" ,(lambda (x) (bool (correlated-p x))))
    ("correlated-e" ,#'correlated-e)
    ("correlated-source" ,(lambda (c) (or (correlated-source c) ps:false)))
    ("correlated-line" ,(lambda (c) (or (correlated-line c) ps:false)))
    ("correlated-column" ,(lambda (c) (or (correlated-column c) ps:false)))
    ("correlated-position" ,(lambda (c) (or (correlated-position c) ps:false)))
    ("correlated-span" ,(lambda (c) (or (correlated-span c) ps:false)))
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
	    (let ((p (assoc key (correlated-props c)))) (if p (cdr p) ps:false)))))
    ("correlated-property-symbol-keys" ,(lambda (c) (mapcar #'car (correlated-props c))))
    ("primitive-table"
     ,(lambda (name &optional table)
	(declare (ignore table))
	(let ((instance (gethash (rname name) *primitive-instances*)))
	  (if instance
	      (let ((h (empty-ihash :eq)))
		(maphash (lambda (k v) (setq h (ihash-set h (rsym k) v))) instance)
		h)
	      ps:false))))
    ("primitive-lookup" ,(lambda (name) (multiple-value-bind (v found) (primitive (rname name))
					  (if found v ps:false))))
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
    ("linklet-cross-machine-type" ,(lambda (&optional target) (declare (ignore target)) ps:false))
    ("linklet-add-target-machine-info" ,(lambda (linklet info) (declare (ignore info)) linklet))
    ("linklet-summarize-target-machine-info" ,(lambda (linklet) (declare (ignore linklet)) ps:false))
    ("primitive->compiled-position" ,(lambda (p) (declare (ignore p)) ps:false))
    ("compiled-position->primitive" ,(lambda (i) (declare (ignore i)) ps:false)))
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
(defnative "datum->syntax" (ctx d &optional srcloc prop) (:leaf t)
  (declare (ignore ctx srcloc prop))
  (if (correlated-p d) d (make-correlated d)))
(defnative "syntax-source" (c) (:leaf t) (or (correlated-source c) ps:false))
(defnative "syntax-line" (c) (:leaf t) (or (correlated-line c) ps:false))
(defnative "syntax-column" (c) (:leaf t) (or (correlated-column c) ps:false))
(defnative "syntax-position" (c) (:leaf t) (or (correlated-position c) ps:false))
(defnative "syntax-span" (c) (:leaf t) (or (correlated-span c) ps:false))
(defnative "syntax-property" (c key &optional (value nil value-p)) (:leaf t)
  (if value-p
      (funcall (second (assoc "correlated-property" *linklet-functions* :test #'string=)) c key value)
      (let ((p (assoc key (correlated-props c)))) (if p (cdr p) ps:false))))
(defnative "syntax-property-symbol-keys" (c) (:leaf t) (mapcar #'car (correlated-props c)))

(defmethod make-load-form ((x correlated) &optional environment)
  (make-load-form-saving-slots x :environment environment))
