; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's structs, natively (docs/racket-linklet-compiler.md, "The
;;;; runtime it calls"): make-struct-type with properties, guards, auto
;;;; fields, immutable fields and prop:procedure.
;;;;
;;;; An instance is an RSTRUCT: its type and a vector of all its fields,
;;;; the ancestors' first.  An applicable one (its type has
;;;; prop:procedure) is an APPLICABLE-RSTRUCT, a funcallable instance,
;;;; as Guile mode's applicable structs are, so it is a procedure to the
;;;; host.  Later (docs/optimizations.md, idea 2): a struct type as an
;;;; SBCL structure class of its own, with typed slots, which the
;;;; compiler's struct-shape analysis (pass 3) can inline.
;;;; Not yet: inspectors (every struct is transparent to struct->vector),
;;;; prefab structs, impersonators and chaperones.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (struct-type (:constructor %make-struct-type))
  name parent
  field-count				; this type's fields: init + auto
  init-field-count auto-field-count auto-value
  (total 0)				; fields with the ancestors'
  (properties '())			; alist: property -> value, the ancestors' too
  inspector (immutables '()) guard constructor-name
  prefab-key
  proc-spec				; prop:procedure's value: a field index or a procedure
  (class nil))				; later: the SBCL class its instances are

(defmethod print-object ((x struct-type) stream)
  (format stream "#<struct-type:~A>" (rname (struct-type-name x))))

(defstruct (rstruct (:constructor make-rstruct (type fields)))
  type (fields #() :type simple-vector))

(defclass applicable-rstruct (sb-mop:funcallable-standard-object)
  ((type :initarg :type :reader applicable-rstruct-type)
   (fields :initarg :fields :reader applicable-rstruct-fields))
  (:metaclass sb-mop:funcallable-standard-class))

(defun struct-instance-p (x) (or (rstruct-p x) (typep x 'applicable-rstruct)))

(defun struct-of-type (x)
  (cond ((rstruct-p x) (rstruct-type x))
	((typep x 'applicable-rstruct) (applicable-rstruct-type x))))

(defun struct-fields (x)
  (if (rstruct-p x) (rstruct-fields x) (applicable-rstruct-fields x)))

(defun subtype-p (type ancestor)
  (loop for ty = type then (struct-type-parent ty)
	while ty
	thereis (eq ty ancestor)))

(defmethod print-object ((x rstruct) stream)
  (format stream "#<~A>" (rname (struct-type-name (rstruct-type x)))))

;;; ------------------------------------------------------------------
;;; Properties

(defstruct (struct-type-property (:constructor %make-struct-type-property (name guard supers)))
  name guard
  supers)				; ((property . function) ...)

(defmethod print-object ((x struct-type-property) stream)
  (format stream "#<struct-type-property:~A>" (rname (struct-type-property-name x))))

(defvar *prop-procedure* (%make-struct-type-property (rsym "prop:procedure") nil '()))

(defun attach-property (alist property value type-info)
  "ALIST with PROPERTY (and its supers, each through its function) set
to VALUE, after PROPERTY's guard."
  (let* ((guard (struct-type-property-guard property))
	 (value (if guard (funcall guard value type-info) value))
	 (alist (acons property value (remove property alist :key #'car))))
    (loop for (super . f) in (struct-type-property-supers property)
	  do (setq alist (attach-property alist super (funcall f value) type-info)))
    alist))

(defun property-value (x property)
  "PROPERTY's value for X, a struct type or an instance; and whether it has one."
  (let* ((type (if (struct-type-p x) x (struct-of-type x)))
	 (entry (and type (assoc property (struct-type-properties type)))))
    (values (cdr entry) (and entry t))))

;;; ------------------------------------------------------------------
;;; Instances

(defun applicable-procedure (type)
  "TYPE's prop:procedure, from it or an ancestor."
  (loop for ty = type then (struct-type-parent ty)
	while ty
	do (when (struct-type-proc-spec ty)
	     (return (values (struct-type-proc-spec ty) ty)))))

(defun make-instance-of (type fields)
  (multiple-value-bind (spec owner) (applicable-procedure type)
    (if (null spec)
	(make-rstruct type fields)
	(let ((s (make-instance 'applicable-rstruct :type type :fields fields)))
	  (sb-mop:set-funcallable-instance-function
	   s (if (integerp spec)
		 ;; a field holding the procedure: the field of OWNER's own
		 (let ((index (+ spec (- (struct-type-total owner) (struct-type-field-count owner)))))
		   (lambda (&rest args) (apply (svref fields index) args)))
		 (lambda (&rest args) (apply spec s args))))
	  s))))

(defun constructor (type)
  (let ((n-init (loop for ty = type then (struct-type-parent ty)
		      while ty sum (struct-type-init-field-count ty))))
    (lambda (&rest args)
      (unless (= (length args) n-init)
	(racket-error (rname (or (struct-type-constructor-name type) (struct-type-name type)))
		      "arity mismatch;~% expected: ~D~% given: ~D" n-init (length args)))
      ;; each type's guard, the ancestors' first ... innermost first, as
      ;; Racket's: the type's own guard gets all the fields, then its
      ;; parent's gets what it returns for the parent's fields
      (let ((args (run-guards type args)))
	(let ((fields (make-array (struct-type-total type)))
	      (i 0) (rest args))
	  (dolist (ty (reverse (loop for ty = type then (struct-type-parent ty)
				     while ty collect ty)))
	    (loop repeat (struct-type-init-field-count ty)
		  do (setf (svref fields i) (pop rest)) (incf i))
	    (loop repeat (struct-type-auto-field-count ty)
		  do (setf (svref fields i) (struct-type-auto-value ty)) (incf i)))
	  (make-instance-of type fields))))))

(defun run-guards (type args)
  (if (null type)
      args
      (let* ((guard (struct-type-guard type))
	     (args (if guard
		       (multiple-value-list (apply guard (append args (list (struct-type-name type)))))
		       args)))
	;; the parent's guard sees the parent's share of the fields
	(let* ((parent (struct-type-parent type))
	       (n (loop for ty = parent then (struct-type-parent ty)
			while ty sum (struct-type-init-field-count ty))))
	  (append (run-guards parent (subseq args 0 n)) (subseq args n))))))

(defun field-index (type i who)
  (unless (and (integerp i) (< -1 i (struct-type-field-count type)))
    (racket-error who "index out of range~%  index: ~A" i))
  (+ i (- (struct-type-total type) (struct-type-field-count type))))

(defun check-instance (type x who)
  (unless (and (struct-instance-p x) (subtype-p (struct-of-type x) type))
    (racket-error who "contract violation~%  expected: ~A?~%  given: ~S"
		  (rname (struct-type-name type)) x)))

;;; ------------------------------------------------------------------
;;; The primitives

(defnative "make-struct-type" (name parent-type init-field-count auto-field-count
				    &optional (auto-v ps:false) (props '()) (inspector ps:false)
				    (proc-spec ps:false) (immutables '()) (guard ps:false)
				    (constructor-name ps:false))
    (:rumble "make-struct-type")
  (declare (ignore inspector))
  (let* ((parent (and (truthy parent-type) parent-type))
	 (count (+ init-field-count auto-field-count))
	 (type (%make-struct-type
		:name name :parent parent :field-count count
		:init-field-count init-field-count :auto-field-count auto-field-count
		:auto-value auto-v
		:total (+ count (if parent (struct-type-total parent) 0))
		:properties (if parent (struct-type-properties parent) '())
		:immutables immutables
		:guard (and (truthy guard) guard)
		:constructor-name (and (truthy constructor-name) constructor-name)
		:proc-spec (and (truthy proc-spec) proc-spec))))
    (loop for (property . value) in props
	  do (setf (struct-type-properties type)
		   (attach-property (struct-type-properties type) property value
				    (list name init-field-count auto-field-count))))
    (let ((proc (cdr (assoc *prop-procedure* props))))
      (when proc (setf (struct-type-proc-spec type) proc)))
    (let ((who (rname name)))
      (values type
	      (constructor type)
	      (lambda (x) (bool (and (struct-instance-p x) (subtype-p (struct-of-type x) type))))
	      (lambda (x i)
		(check-instance type x (format nil "~A-ref" who))
		(svref (struct-fields x) (field-index type i (format nil "~A-ref" who))))
	      (lambda (x i v)
		(check-instance type x (format nil "~A-set!" who))
		(when (member i (struct-type-immutables type))
		  (racket-error (format nil "~A-set!" who) "field is immutable: ~D" i))
		(setf (svref (struct-fields x) (field-index type i (format nil "~A-set!" who))) v)
		*void*)))))

(defnative "make-struct-type-property" (name &optional (guard ps:false) (supers '())
					     (can-impersonate? ps:false) (realm ps:false))
    (:rumble "make-struct-type-property")
  (declare (ignore can-impersonate? realm))
  (let ((p (%make-struct-type-property name (and (truthy guard) guard) supers)))
    (values p
	    (lambda (x) (bool (and (or (struct-type-p x) (struct-instance-p x))
				   (nth-value 1 (property-value x p)))))
	    (lambda (x &optional (fail nil fail-p))
	      (multiple-value-bind (v found) (property-value x p)
		(cond (found v)
		      ((not fail-p) (racket-error (format nil "~A-accessor" (rname name))
						  "contract violation~%  given: ~S" x))
		      ((functionp fail) (funcall fail))
		      (t fail)))))))

(defnative "make-struct-field-accessor" (accessor-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-accessor")
  (declare (ignore field-name contract realm))
  (lambda (s) (funcall accessor-proc s field-pos)))

(defnative "make-struct-field-mutator" (mutator-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-mutator")
  (declare (ignore field-name contract realm))
  (lambda (s v) (funcall mutator-proc s field-pos v)))

(defnative "struct?" (x) (:leaf t) (bool (struct-instance-p x)))
(defnative "struct-type?" (x) (:leaf t) (bool (struct-type-p x)))
(defnative "struct-type-property?" (x) (:leaf t) (bool (struct-type-property-p x)))
(defnative "struct->vector" (x &optional opaque) (:leaf t)
  (declare (ignore opaque))
  (if (struct-instance-p x)
      (concatenate 'simple-vector
		   (vector (rsym (format nil "struct:~A" (rname (struct-type-name (struct-of-type x))))))
		   (struct-fields x))
      (vector (rsym "struct:?"))))
(defnative "current-inspector" (&optional i) (:leaf t) (declare (ignore i)) ps:false)
(defnative "make-inspector" (&optional i) (:leaf t) (declare (ignore i)) ps:false)

(define-primitive "#%kernel" "prop:procedure" *prop-procedure*)
