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
;;;; A struct type made with inspector #f is transparent (equal? and
;;;; struct->vector see its fields); any other inspector makes it opaque.
;;;; Not yet: inspectors' powers beyond that,
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
  accessor mutator			; the general ones make-struct-type returned
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
				    &optional (auto-v ps:false) (props '()) (inspector *current-inspector*)
				    (proc-spec ps:false) (immutables '()) (guard ps:false)
				    (constructor-name ps:false))
    (:rumble "make-struct-type")
  ;; a field that is the procedure (proc-spec, or prop:procedure's value,
  ;; an index) is immutable, as Racket makes it
  (let ((trace (uiop:getenv "PSEUDOSCHEME_RACKET_TRACE_STRUCT")))
    ;; for finding where a struct type is made (twice, when its
    ;; instances aren't its own)
    (when (and trace (string= trace (rname name)))
      (format *error-output* "~&;; make-struct-type ~A~%" trace)
      (let ((*print-length* 3) (*print-level* 2))
	(sb-debug:print-backtrace :count 30 :stream *error-output*))))
  (let ((index (if (integerp proc-spec) proc-spec (cdr (assoc *prop-procedure* props)))))
    (when (and (integerp index) (not (member index immutables)))
      (setq immutables (cons index immutables))))
  (let* ((parent (and (truthy parent-type) parent-type))
	 (count (+ init-field-count auto-field-count))
	 (type (%make-struct-type
		:name name :parent parent :field-count count
		:init-field-count init-field-count :auto-field-count auto-field-count
		:auto-value auto-v
		:total (+ count (if parent (struct-type-total parent) 0))
		:properties (if parent (struct-type-properties parent) '())
		:immutables immutables
		:inspector inspector
		:guard (and (truthy guard) guard)
		:constructor-name (and (truthy constructor-name) constructor-name)
		:proc-spec (and (truthy proc-spec) proc-spec))))
    (let* ((who (rname name))
	   (accessor (lambda (x i)
		       (check-instance type x (format nil "~A-ref" who))
		       (svref (struct-fields x) (field-index type i (format nil "~A-ref" who)))))
	   (mutator (lambda (x i v)
		      (check-instance type x (format nil "~A-set!" who))
		      (when (member i (struct-type-immutables type))
			(racket-error (format nil "~A-set!" who) "field is immutable: ~D" i))
		      (setf (svref (struct-fields x) (field-index type i (format nil "~A-set!" who))) v)
		      *void*))
	   ;; what a property's guard gets, as struct-type-info returns it
	   (info (list name init-field-count auto-field-count accessor mutator
		       immutables (or parent ps:false) ps:false)))
      (loop for (property . value) in props
	    do (setf (struct-type-properties type)
		     (attach-property (struct-type-properties type) property value info)))
      (let ((proc (cdr (assoc *prop-procedure* props))))
	(when proc (setf (struct-type-proc-spec type) proc)))
      ;; inspector 'prefab: a prefab struct type, the same for all who
      ;; make one of this name and shape
      (when (and (symbolp inspector) inspector (string= (rname inspector) "prefab"))
	(let* ((key (normalized-prefab-key name (loop for i below count unless (member i immutables) collect i)
					   parent))
	       (existing (gethash (cons (rkey key) (struct-type-total type)) *prefab-types*)))
	  (if existing
	      ;; the same prefab type as before: its instances are each other's
	      (setq type existing)
	      (setf (struct-type-prefab-key type) key
		    (gethash (cons (rkey key) (struct-type-total type)) *prefab-types*) type))))
      (let ((constructor (constructor type))
	    (predicate (lambda (x) (bool (and (struct-instance-p x) (subtype-p (struct-of-type x) type))))))
	(setf (struct-type-accessor type) accessor
	      (struct-type-mutator type) mutator)
	(note-struct-procedure constructor :constructor type)
	(note-struct-procedure predicate :predicate type)
	(note-struct-procedure accessor :accessor type)
	(note-struct-procedure mutator :mutator type)
	(values type constructor predicate accessor mutator)))))

(defvar *struct-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A struct type's constructor, predicate, accessor or mutator -> (kind . type).")

(defun note-struct-procedure (f kind type)
  (setf (gethash f *struct-procedures*) (cons kind type))
  f)

(defun struct-procedure-kind (f) (car (gethash f *struct-procedures*)))

(defnative "make-struct-type-property" (name &optional (guard ps:false) (supers '())
					     (can-impersonate? ps:false) accessor-name contract realm)
    (:rumble "make-struct-type-property")
  (declare (ignore can-impersonate? accessor-name contract realm))
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
  (note-struct-procedure (lambda (s) (funcall accessor-proc s field-pos))
			 :field-accessor (cdr (gethash accessor-proc *struct-procedures*))))

(defnative "make-struct-field-mutator" (mutator-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-mutator")
  (declare (ignore field-name contract realm))
  (note-struct-procedure (lambda (s v) (funcall mutator-proc s field-pos v))
			 :field-mutator (cdr (gethash mutator-proc *struct-procedures*))))

(defnative "struct-predicate-procedure?" (f) (:leaf t) (bool (eq (struct-procedure-kind f) :predicate)))
(defnative "struct-constructor-procedure?" (f) (:leaf t) (bool (eq (struct-procedure-kind f) :constructor)))
(defnative "struct-accessor-procedure?" (f) (:leaf t)
  (bool (member (struct-procedure-kind f) '(:accessor :field-accessor))))
(defnative "struct-mutator-procedure?" (f) (:leaf t)
  (bool (member (struct-procedure-kind f) '(:mutator :field-mutator))))
(defnative "struct-type-info" (type) (:leaf t)
  ;; name, init fields, auto fields, accessor, mutator, immutables,
  ;; supertype, skipped?
  (values (struct-type-name type) (struct-type-init-field-count type) (struct-type-auto-field-count type)
	  (struct-type-accessor type) (struct-type-mutator type) (struct-type-immutables type)
	  (or (struct-type-parent type) ps:false) ps:false))
(defnative "struct-info" (v) (:leaf t)
  (if (and (struct-instance-p v) (transparent-p (struct-of-type v)))
      (values (struct-of-type v) ps:false)
      (values ps:false ps:true)))
(defnative "struct-type-make-constructor" (type &optional name) (:leaf t)
  (declare (ignore name))
  (note-struct-procedure (constructor type) :constructor type))
(defnative "struct-type-make-predicate" (type) (:leaf t)
  (note-struct-procedure (lambda (x) (bool (and (struct-instance-p x) (subtype-p (struct-of-type x) type))))
			 :predicate type))
(defnative "struct-type-authentic?" (type) (:leaf t) (declare (ignore type)) ps:false)
(defnative "struct-type-sealed?" (type) (:leaf t) (declare (ignore type)) ps:false)

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
(defstruct (inspector (:constructor make-inspector* (superior))) superior)
(defvar *root-inspector* (make-inspector* nil))
(defvar *current-inspector* (make-inspector* *root-inspector*))

(defun transparent-p (type)
  "Whether TYPE's instances show their fields: made with inspector #f, or prefab."
  (or (eq (struct-type-inspector type) ps:false) (struct-type-prefab-key type)))

(defnative "current-inspector" (&optional (i nil i-p)) (:leaf t)
  (if i-p (progn (setq *current-inspector* i) *void*) *current-inspector*))
(defnative "make-inspector" (&optional (superior *current-inspector*)) (:leaf t)
  (make-inspector* superior))
(defnative "make-sibling-inspector" (&optional (inspector *current-inspector*)) (:leaf t)
  (make-inspector* (inspector-superior inspector)))
(defnative "inspector?" (x) (:leaf t) (bool (inspector-p x)))
(defnative "inspector-superior?" (a b) (:leaf t)
  (bool (loop for i = (inspector-superior b) then (inspector-superior i)
	      while i thereis (eq i a))))

(define-primitive "#%kernel" "prop:procedure" *prop-procedure*)

;;; ------------------------------------------------------------------
;;; Prefab structs: a struct type per (key, field count), shared by all
;;; who name it.  Keys as Racket normalizes them: NAME, or (NAME N
;;; [(AUTO-N AUTO-V)] [#(MUTABLE-INDEX ...)] PARENT-KEY ...); the
;;; normalized key drops the defaults.  Not yet: auto fields in keys.

(defvar *prefab-types* (make-hash-table :test 'equalp :synchronized t)
  "(key . field count) -> the prefab struct type.  EQUALP: a key is
symbols, numbers and vectors of numbers.")

(defun prefab-key-parts (key)
  "KEY's own name, its own field count (or NIL), its mutable indexes and
its parent key (or NIL)."
  (if (symbolp key)
      (values key nil '() nil)
      (let ((name (car key)) (rest (cdr key)) (n nil) (mutable '()))
	(when (and rest (integerp (car rest))) (setq n (pop rest)))
	(when (and rest (consp (car rest)) (integerp (caar rest))) (pop rest))
	(when (and rest (vectorp (car rest))) (setq mutable (coerce (pop rest) 'list)))
	(values name n mutable (and rest (if (cdr rest) rest (car rest)))))))

(defun prefab-struct-type (key field-count)
  "The prefab struct type for KEY with FIELD-COUNT fields in all."
  (or (gethash (cons (rkey key) field-count) *prefab-types*)
      (multiple-value-bind (name own mutable parent-key) (prefab-key-parts key)
	(let* ((parent (and parent-key
			    (prefab-struct-type parent-key (- field-count (or own 0)))))
	       (own (or own (- field-count (if parent (struct-type-total parent) 0)))))
	  (multiple-value-bind (type)
	      (make-struct-type* name (or parent ps:false) own 0 ps:false '() ps:false ps:false
				 (loop for i below own unless (member i mutable) collect i))
	    (setf (struct-type-prefab-key type) (normalized-prefab-key name mutable parent)
		  (gethash (cons (rkey key) field-count) *prefab-types*) type))))))

(defun rkey (key) key)

(defun normalized-prefab-key (name mutable parent)
  (let ((own (append (list name) (and mutable (list (coerce mutable 'simple-vector))))))
    (if parent
	(append own (let ((pk (struct-type-prefab-key parent))) (if (symbolp pk) (list pk) pk)))
	(if (cdr own) own name))))

(defnative "prefab-struct-key" (v) (:leaf t)
  (let ((type (and (struct-instance-p v) (struct-of-type v))))
    (or (and type (struct-type-prefab-key type)) ps:false)))
(defnative "make-prefab-struct" (key &rest fields) (:leaf t)
  (let ((type (prefab-struct-type key (length fields))))
    (make-instance-of type (coerce fields 'simple-vector))))
(defnative "prefab-key->struct-type" (key field-count) (:leaf t)
  (prefab-struct-type key field-count))
(defnative "prefab-key?" (x) (:leaf t)
  (bool (or (symbolp x) (and (consp x) (symbolp (car x))))))
(defnative "prefab-struct-type-key+field-count" (type) (:leaf t)
  (if (and (struct-type-p type) (struct-type-prefab-key type))
      (cons (struct-type-prefab-key type) (struct-type-total type))
      ps:false))

;;; Unsafe field access: the index counts every field, the ancestors' first
(defnative "unsafe-struct-ref" (s i) (:instance "#%unsafe" :leaf t) (svref (struct-fields s) i))
(defnative "unsafe-struct*-ref" (s i) (:instance "#%unsafe" :leaf t) (svref (struct-fields s) i))
(defnative "unsafe-struct-set!" (s i v) (:instance "#%unsafe" :leaf t)
  (setf (svref (struct-fields s) i) v) *void*)
(defnative "unsafe-struct*-set!" (s i v) (:instance "#%unsafe" :leaf t)
  (setf (svref (struct-fields s) i) v) *void*)
(defnative "unsafe-struct*-cas!" (s i old new) (:instance "#%unsafe" :leaf t)
  (let ((fields (struct-fields s)))
    (bool (eq (sb-ext:compare-and-swap (svref fields i) old new) old))))

;;; Keyword procedures' check: V an instance of TYPE (which has
;;; prop:checked-procedure) whose first field accepts V1 and V2 gives its
;;; second field; anything else is ALT's to deal with
(defnative "checked-procedure-check-and-extract" (type v alt v1 v2) ()
  (if (and (struct-instance-p v) (subtype-p (struct-of-type v) type)
	   (truthy (funcall (svref (struct-fields v) 0) v1 v2)))
      (svref (struct-fields v) 1)
      (funcall alt v v1 v2)))
