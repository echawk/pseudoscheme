; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

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

(in-package "QUASISCHEME-RACKET")

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

;;; A chaperone or impersonator of a struct instance (chaperone.lisp
;;; makes them): an instance of the same type that shares the fields
;;; vector, with the layer it wraps, and its operations' redirection
;;; procedures: an alist, KEY -> procedure of the instance and the
;;; value.  A key is (TYPE . POS) for a field's accessor, (TYPE . -1-POS)
;;; for its mutator, a property for its accessor, :INFO for struct-info.
(defstruct (cstruct (:include rstruct) (:constructor make-cstruct (type fields of kind procs)))
  of kind procs)

(defclass applicable-cstruct (applicable-rstruct)
  ((of :initarg :of :reader applicable-cstruct-of)
   (kind :initarg :kind :reader applicable-cstruct-kind)
   (procs :initarg :procs :reader applicable-cstruct-procs))
  (:metaclass sb-mop:funcallable-standard-class))

(declaim (inline chaperone-layer-p))
(defun chaperone-layer-p (x)
  (or (cstruct-p x) (and (not (rstruct-p x)) (typep x 'applicable-cstruct))))
(defun layer-of (x) (if (cstruct-p x) (cstruct-of x) (applicable-cstruct-of x)))
(defun layer-kind (x) (if (cstruct-p x) (cstruct-kind x) (applicable-cstruct-kind x)))
(defun layer-procs (x) (if (cstruct-p x) (cstruct-procs x) (applicable-cstruct-procs x)))

(defun key= (a b)
  (or (eq a b) (and (consp a) (consp b) (eq (car a) (car b)) (eql (cdr a) (cdr b)))))

(defun raise-chaperone-error (who what orig new)
  ;; a box's or vector's chaperone says it differently
  (when (string= what "result")
    (racket-error who "chaperone produced a result that is not a chaperone of the original result~%  chaperone result: ~A~%  original result: ~A"
		  (written new) (written orig)))
  (racket-error who "non-chaperone result; received a~A ~A that is not a chaperone of the original ~A~%  original: ~A~%  received: ~A"
		(if (string= what "argument") "n" "") what what (written orig) (written new)))

(defun checked-chaperone (who kind what orig new)
  "NEW, which a chaperone's redirection procedure returned for ORIG: it
must be ORIG or a chaperone of it."
  (when (and (eq kind :chaperone) (not (impersonates-p new orig)))
    (raise-chaperone-error who what orig new))
  new)

(defun layered-value (x key base orig)
  "BASE, the value of a struct operation KEY on the instance under X's
layers, as the layers' redirection procedures (called with ORIG, the
outermost) change it, innermost first."
  (if (chaperone-layer-p x)
      (let* ((r (layered-value (layer-of x) key base orig))
	     (w (cdr (assoc key (layer-procs x) :test #'key=))))
	(if w
	    (checked-chaperone "struct-ref" (layer-kind x) "value" r (funcall w orig r))
	    r))
      base))

(defun layered-set (x key abs-index v orig)
  "Store V in field ABS-INDEX, as the layers' mutator redirections (outermost first) change it."
  (if (chaperone-layer-p x)
      (let* ((w (cdr (assoc key (layer-procs x) :test #'key=)))
	     (new (if w
		      (checked-chaperone "struct-set!" (layer-kind x) "value" v (funcall w orig v))
		      v)))
	(layered-set (layer-of x) key abs-index new orig))
      (setf (svref (struct-fields x) abs-index) v)))

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
	  ;; its arity, as Racket's: the field procedure's, or the
	  ;; procedure's less the instance
	  (setf (gethash s *arity-masks*)
		(or (ignore-errors
		     (if (integerp spec)
			 (arity-mask (svref fields (+ spec (- (struct-type-total owner) (struct-type-field-count owner)))))
			 (ash (arity-mask spec) -1)))
		    -1))
	  s))))

(defvar *renamed-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Procedures -> their object-name (basics.lisp; defined here too, as the
struct types exn.lisp makes are named before basics.lisp loads).")

(defun simple-type-p (type)
  "Whether TYPE and its ancestors have no guards and no auto fields: a
constructor's arguments are then the fields, in order."
  (loop for ty = type then (struct-type-parent ty)
	while ty
	always (and (null (struct-type-guard ty)) (zerop (struct-type-auto-field-count ty)))))

(defun constructor (type)
  (let ((n-init (loop for ty = type then (struct-type-parent ty)
		      while ty sum (struct-type-init-field-count ty))))
    (if (and (simple-type-p type) (null (applicable-procedure type)))
	;; the common case: the arguments are the fields, and the instance
	;; a plain one; the argument list on the stack, copied
	(with-arity-mask (ash 1 n-init) (simple-constructor type n-init))
	(general-constructor type n-init))))

(defvar +missing+ (make-symbol "MISSING"))

(defmacro fixed-constructors (max)
  "A CASE on N choosing, for N fields up to MAX, a constructor taking N
optional arguments (so a wrong count is Racket's arity error, not
SBCL's) and making the fields vector from them with no list."
  `(case n
     ,@(loop for k from 0 to max
	     collect (let ((vars (loop for i below k collect (intern (format nil "A~D" i)))))
		       `(,k (lambda (&optional ,@(mapcar (lambda (v) `(,v (load-time-value +missing+ t))) vars) &rest more)
			      (declare (dynamic-extent more))
			      (when (or more ,@(and vars `((eq ,(car (last vars)) (load-time-value +missing+ t)))))
				(arity-error (list* ,@(remove-if (lambda (v) (declare (ignore v)) nil) vars) more)))
			      (make-rstruct type (vector ,@vars))))))
     (t nil)))

(defun simple-constructor (type n)
  "The constructor of TYPE, simple (SIMPLE-TYPE-P, no prop:procedure),
whose instances have N fields, all its arguments."
  (flet ((arity-error (args)
	   (racket-arity-error (rname (or (struct-type-constructor-name type) (struct-type-name type)))
			       (count-if-not (lambda (x) (eq x +missing+)) args) n)))
    (or (fixed-constructors 10)
	(lambda (&rest args)
	  (unless (= (length args) n) (arity-error args))
	  (make-rstruct type (coerce args 'simple-vector))))))

(defun general-constructor (type n-init)
    (with-arity-mask
	(ash 1 n-init)
      (lambda (&rest args)
	(unless (= (length args) n-init)
	  (racket-arity-error (rname (or (struct-type-constructor-name type) (struct-type-name type)))
			      (length args) n-init))
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

(defun with-arity-mask (mask function)
  "FUNCTION, which procedure-arity and the like see as taking MASK's
numbers of arguments."
  (setf (gethash function *arity-masks*) mask)
  function)

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
				    &optional (auto-v qs:false) (props '()) (inspector (current-inspector*))
				    (proc-spec qs:false) (immutables '()) (guard qs:false)
				    (constructor-name qs:false))
    (:rumble "make-struct-type")
  ;; a field that is the procedure (proc-spec, or prop:procedure's value,
  ;; an index) is immutable, as Racket makes it
  (let ((trace (uiop:getenv "QUASISCHEME_RACKET_TRACE_STRUCT")))
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
	   (who-ref (format nil "~A-ref" who))
	   (who-set (format nil "~A-set!" who))
	   (accessor (lambda (x i)
		       (check-instance type x who-ref)
		       (let* ((index (field-index type i who-ref))
			      (v (svref (struct-fields x) index)))
			 (if (chaperone-layer-p x)
			     (layered-value x (cons type i) v x)
			     v))))
	   (mutator (lambda (x i v)
		      (check-instance type x who-set)
		      (when (member i (struct-type-immutables type))
			(racket-error who-set "field is immutable: ~D" i))
		      (let ((index (field-index type i who-set)))
			(if (chaperone-layer-p x)
			    (layered-set x (cons type (- -1 i)) index v x)
			    (setf (svref (struct-fields x) index) v)))
		      *void*))
	   ;; what a property's guard gets, as struct-type-info returns it
	   (info (list name init-field-count auto-field-count accessor mutator
		       immutables (or parent qs:false) qs:false)))
      ;; its own properties, supers included (racket/base's prop:procedure
      ;; is racket/private/kw.rkt's, whose super is the kernel's), over
      ;; the parent's
      (let ((own '()))
	(loop for (property . value) in props
	      do (setq own (attach-property own property value info)))
	(setf (struct-type-properties type)
	      (append own (remove-if (lambda (e) (assoc (car e) own)) (struct-type-properties type))))
	(let ((proc (cdr (assoc *prop-procedure* own))))
	  (when proc (setf (struct-type-proc-spec type) proc))))
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
	;; their names, as Racket's: make-NAME (or the constructor name),
	;; NAME?, NAME-ref, NAME-set!
	(flet ((named (f s) (setf (gethash f *renamed-procedures*) (rsym s))))
	  (named constructor (if (struct-type-constructor-name type)
				 (rname (struct-type-constructor-name type))
				 (format nil "make-~A" who)))
	  (named predicate (format nil "~A?" who))
	  (named accessor who-ref)
	  (named mutator who-set))
	(note-struct-procedure constructor :constructor type)
	(note-struct-procedure predicate :predicate type)
	(note-struct-procedure accessor :accessor type)
	(note-struct-procedure mutator :mutator type)
	(values type constructor predicate accessor mutator)))))

;;; A property accessor made by make-struct-type-property -> its property;
;;; a field accessor or mutator -> (type pos), for chaperone-struct
(defvar *property-accessors* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *field-operations* (make-hash-table :test 'eq :weakness :key :synchronized t))

(defvar *struct-procedures* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A struct type's constructor, predicate, accessor or mutator -> (kind . type).")

(defun note-struct-procedure (f kind type)
  (setf (gethash f *struct-procedures*) (cons kind type))
  f)

(defun struct-procedure-kind (f) (car (gethash f *struct-procedures*)))

(defnative "make-struct-type-property" (name &optional (guard qs:false) (supers '())
					     (can-impersonate? qs:false) accessor-name contract realm)
    (:rumble "make-struct-type-property")
  (declare (ignore can-impersonate? accessor-name contract realm))
  ;; the guard may be the symbol can-impersonate: no guard, the property
  ;; allowing impersonators (which aren't distinguished here)
  (let ((p (%make-struct-type-property name (and (functionp guard) guard) supers))
	(accessor nil))
    (setq accessor
	  (lambda (x &optional (fail nil fail-p))
	    (multiple-value-bind (v found) (property-value x p)
	      (cond (found (if (and (struct-instance-p x) (chaperone-layer-p x))
			       (layered-value x p v x)
			       v))
		    ((not fail-p) (racket-error (format nil "~A-accessor" (rname name))
						"contract violation~%  given: ~S" x))
		    ((functionp fail) (funcall fail))
		    (t fail)))))
    (setf (gethash accessor *property-accessors*) p)
    (values p
	    (lambda (x) (bool (and (or (struct-type-p x) (struct-instance-p x))
				   (nth-value 1 (property-value x p)))))
	    accessor)))

(defun plain-instance-of-p (s type)
  "Whether S is a plain instance (not a chaperone, nor applicable) of
TYPE or a subtype: its fields can be read and written directly."
  (and (rstruct-p s) (not (cstruct-p s))
       (let ((ty (rstruct-type s))) (or (eq ty type) (subtype-p ty type)))))

(defnative "make-struct-field-accessor" (accessor-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-accessor")
  (declare (ignore contract realm))
  (let* ((type (cdr (gethash accessor-proc *struct-procedures*)))
	 (index (and type (integerp field-pos) (< -1 field-pos (struct-type-field-count type))
		     (+ field-pos (- (struct-type-total type) (struct-type-field-count type)))))
	 (f (note-struct-procedure
	     (if index
		 ;; a plain instance's field directly; anything else (a
		 ;; chaperone, the wrong type) through the general accessor
		 (lambda (s)
		   (if (plain-instance-of-p s type)
		       (svref (rstruct-fields s) index)
		       (funcall accessor-proc s field-pos)))
		 (lambda (s) (funcall accessor-proc s field-pos)))
	     :field-accessor type)))
    (when (and type (symbolp field-name) field-name (not (eq field-name qs:false)))
      (setf (gethash f *renamed-procedures*)
	    (rsym (format nil "~A-~A" (rname (struct-type-name type)) (rname field-name)))))
    (setf (gethash f *field-operations*) (list :accessor (cdr (gethash accessor-proc *struct-procedures*)) field-pos))
    f))

(defnative "make-struct-field-mutator" (mutator-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-mutator")
  (declare (ignore contract realm))
  (let* ((type (cdr (gethash mutator-proc *struct-procedures*)))
	 (index (and type (integerp field-pos) (< -1 field-pos (struct-type-field-count type))
		     (not (member field-pos (struct-type-immutables type)))
		     (+ field-pos (- (struct-type-total type) (struct-type-field-count type)))))
	 (f (note-struct-procedure
	     (if index
		 (lambda (s v)
		   (if (plain-instance-of-p s type)
		       (progn (setf (svref (rstruct-fields s) index) v) *void*)
		       (funcall mutator-proc s field-pos v)))
		 (lambda (s v) (funcall mutator-proc s field-pos v)))
	     :field-mutator type)))
    (when (and type (symbolp field-name) field-name (not (eq field-name qs:false)))
      (setf (gethash f *renamed-procedures*)
	    (rsym (format nil "set-~A-~A!" (rname (struct-type-name type)) (rname field-name)))))
    (setf (gethash f *field-operations*) (list :mutator (cdr (gethash mutator-proc *struct-procedures*)) field-pos))
    f))

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
	  (or (struct-type-parent type) qs:false) qs:false))
(defnative "struct-info" (v) ()
  (labels ((base (x)
	     (if (and (struct-instance-p x) (transparent-p (struct-of-type x)))
		 (values (struct-of-type x) qs:false)
		 (values qs:false qs:true)))
	   (walk (x)
	     (if (and (struct-instance-p x) (chaperone-layer-p x))
		 (multiple-value-bind (type skipped) (walk (layer-of x))
		   (let ((w (cdr (assoc :info (layer-procs x)))))
		     (if (and w (truthy type))
			 (multiple-value-bind (new-type new-skipped) (funcall w type skipped)
			   (checked-chaperone "struct-info" (layer-kind x) "value" type new-type)
			   (checked-chaperone "struct-info" (layer-kind x) "value" skipped new-skipped)
			   (values new-type new-skipped))
			 (values type skipped))))
		 (base x))))
    (walk v)))
(defnative "struct-type-make-constructor" (type &optional name) (:leaf t)
  (declare (ignore name))
  (note-struct-procedure (constructor type) :constructor type))
(defnative "struct-type-make-predicate" (type) (:leaf t)
  (note-struct-procedure (lambda (x) (bool (and (struct-instance-p x) (subtype-p (struct-of-type x) type))))
			 :predicate type))
(defnative "struct-type-authentic?" (type) (:leaf t) (declare (ignore type)) qs:false)
(defnative "struct-type-sealed?" (type) (:leaf t) (declare (ignore type)) qs:false)

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
(defvar *current-inspector-parameter* nil
  "The current-inspector parameter, once it can be made (hash.lisp).")

(defun current-inspector* ()
  "The current inspector: the parameter's value, once there is the parameter."
  (if *current-inspector-parameter*
      (funcall *current-inspector-parameter*)
      *current-inspector*))

(defun transparent-p (type)
  "Whether TYPE's instances show their fields: made with inspector #f, or prefab."
  (or (eq (struct-type-inspector type) qs:false) (struct-type-prefab-key type)))

;;; (current-inspector is a parameter: hash.lisp)
(defnative "make-inspector" (&optional (superior (current-inspector*))) (:leaf t)
  (make-inspector* superior))
(defnative "make-sibling-inspector" (&optional (inspector (current-inspector*))) (:leaf t)
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
	      (make-struct-type* name (or parent qs:false) own 0 qs:false '() qs:false qs:false
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
    (or (and type (struct-type-prefab-key type)) qs:false)))
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
      qs:false))

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

;;; ------------------------------------------------------------------
;;; chaperone-struct and impersonate-struct (chaperone.lisp's natives)

(defun struct-chaperone-error (who what &rest details)
  "WHO: WHAT, with DETAILS (field name, value ...) lines."
  (racket-error who "~A~{~%  ~A: ~A~}" what details))

(defun make-struct-chaperone (who chaperone? v args)
  "V chaperoned (or impersonated) by ARGS: an optional struct type, then
accessors, mutators, property accessors (and struct-info, for a
chaperone) each with its redirection procedure, and impersonator
properties each with its value."
  (when (null args) (return-from make-struct-chaperone v))
  (let* ((kind (if chaperone? :chaperone :impersonator))
	 (st (and (struct-type-p (car args)) (car args)))
	 (args (if st (cdr args) args))
	 (procs '()) (props '()) (witnessed (and st t)))
    (unless (struct-instance-p v)
      (arg-error who "struct?" v))
    (when (and st (not (subtype-p (struct-of-type v) st)))
      (struct-chaperone-error who "given value is not an instance of the given structure type"
			      "struct type" (written st) "value" (written v)))
    (flet ((operation (what key proc-arg applies redirect arity)
	     (unless applies
	       (struct-chaperone-error who "operation does not apply to given value"
				       "operation kind" what "operation procedure" (written proc-arg)
				       "value" (written v)))
	     (when (assoc key procs :test #'key=)
	       (struct-chaperone-error who "given operation accesses the same value as a previous operation argument"
				       "operation kind" what "operation procedure" (written proc-arg)))
	     (when (and (truthy redirect) (not (logbitp arity (arity-mask redirect))))
	       (struct-chaperone-error who "operation's redirection procedure does not match the expected arity"
				       "given" (written redirect)
				       "expected" (format nil "(or/c #f (procedure-arity-includes/c ~D))" arity)
				       "operation kind" what "operation procedure" (written proc-arg)))
	     (when (truthy redirect) (push (cons key redirect) procs))))
      (loop while args
	    do (let ((a (car args)))
		 (cond
		   ((impersonator-property-p a)
		    (unless (cdr args)
		      (struct-chaperone-error who "missing value argument after an imperonsonator-property argument"
					      "impersonator property" (written a)))
		    (push (cons a (cadr args)) props)
		    (setq args (cddr args)))
		   (t
		    (when (null (cdr args))
		      (struct-chaperone-error who "missing redirection procedure after operation"
					      "operation kind" "operation" "operation procedure" (written a)))
		    (let ((redirect (cadr args))
			  (op (and (functionp a) (gethash a *field-operations*)))
			  (prop (and (functionp a) (gethash a *property-accessors*))))
		      (cond
			((and op (eq (first op) :accessor))
			 (destructuring-bind (type pos) (rest op)
			   (when (and (not chaperone?) (member pos (struct-type-immutables type)))
			     (struct-chaperone-error who "cannot replace operation for an immutable field"
						     "operation kind" "property accessor"
						     "operation procedure" (written a)))
			   (operation "accessor" (cons type pos) a (subtype-p (struct-of-type v) type) redirect 2)))
			((and op (eq (first op) :mutator))
			 (destructuring-bind (type pos) (rest op)
			   (operation "mutator" (cons type (- -1 pos)) a (subtype-p (struct-of-type v) type) redirect 2)))
			(prop
			 (operation "property accessor" prop a
				    (nth-value 1 (property-value v prop)) redirect 2))
			((and chaperone? (eq a (native-function (gethash "struct-info" *natives*))))
			 (operation "struct-info procedure" :info a t redirect 2))
			(t (arg-error who (format nil "(or/c struct-accessor-procedure?~%      struct-mutator-procedure?~%      struct-type-property-accessor-procedure?~A)"
						  (if chaperone? (format nil "~%      struct-info") ""))
				      a)))
		      (setq witnessed t)
		      (setq args (cddr args)))))))
      (unless witnessed
	(struct-chaperone-error who (format nil "cannot ~A value as a structure without a witness"
					    (if chaperone? "chaperone" "impersonate"))
				"explanation" "a structure type, accessor, or mutator acts as a witness that the given value's representation can be chaperoned or impersonated"
				"given value" (written v)))
      (when (and (null procs) (null props))
	(return-from make-struct-chaperone v))
      (let* ((procs (nreverse procs))
	     (new (if (rstruct-p v)
		      (make-cstruct (rstruct-type v) (rstruct-fields v) v kind procs)
		      (let ((n (make-instance 'applicable-cstruct :type (applicable-rstruct-type v)
							   :fields (applicable-rstruct-fields v)
							   :of v :kind kind :procs procs)))
			(sb-mop:set-funcallable-instance-function n (lambda (&rest args) (apply v args)))
			(let ((mask (gethash v *arity-masks*)))
			  (when mask (setf (gethash n *arity-masks*) mask)))
			n))))
	(setf (gethash new *impersonators*) (make-impersonation kind v (nreverse props)))
	new))))
