; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; (rnrs records procedural (6)) and (rnrs records inspection (6)),
;;;; library report chapter 6.  (rnrs records syntactic (6)) is psyntax's
;;;; DEFINE-RECORD-TYPE, which expands into calls to these.
;;;;
;;;; A record is an instance of RECORD: its type descriptor and a simple
;;;; vector holding all of its fields, the parent type's first.

(in-package "PSEUDOSCHEME-R6RS")

;;; Defined in conditions.lisp; records signal violations through it.
(declaim (ftype function r6rs-assertion-violation))

(defstruct (rtd (:constructor %make-rtd))
  name parent uid sealed opaque
  fields			; simple-vector of (mutable-p . name), own fields only
  (count 0))			; number of fields including inherited ones

(defstruct (rcd (:constructor %make-rcd (rtd parent protocol)))
  rtd parent protocol)

(defstruct (record (:constructor %make-record (rtd values)))
  rtd values)

(defun scheme-name (x) (if (symbolp x) (ps:scheme-symbol-name x) x))

(defmethod print-object ((r rtd) stream)
  (print-unreadable-object (r stream) (format stream "Record-type ~A" (scheme-name (rtd-name r)))))
(defmethod print-object ((r rcd) stream)
  (print-unreadable-object (r stream)
    (format stream "Record-constructor ~A" (scheme-name (rtd-name (rcd-rtd r))))))
(defmethod print-object ((r record) stream)
  (print-unreadable-object (r stream) (format stream "Record ~A" (scheme-name (rtd-name (record-rtd r))))))

(defun check-rtd (who x)
  (unless (rtd-p x) (ps:scheme-error "~A: not a record-type descriptor: ~S" who x)))

(defvar *nongenerative-types* (make-hash-table :test 'eq)
  "uid -> rtd, for nongenerative record types (6.3).")

(defun field-spec (spec)
  (unless (and (consp spec) (consp (cdr spec)) (symbolp (cadr spec)))
    (ps:scheme-error "make-record-type-descriptor: bad field spec ~S" spec))
  (let ((kind (ps:scheme-symbol-name (car spec))))
    (cons (cond ((string= kind "mutable") t)
		((string= kind "immutable") nil)
		(t (ps:scheme-error "make-record-type-descriptor: bad field spec ~S" spec)))
	  (cadr spec))))

(defprim "make-record-type-descriptor" (name parent uid sealed opaque fields)
  (let ((parent (if (eq parent ps:false) nil parent))
	(uid (if (eq uid ps:false) nil uid)))
    (when parent
      (check-rtd "make-record-type-descriptor" parent)
      (when (rtd-sealed parent)
	(ps:scheme-error "make-record-type-descriptor: parent ~A is sealed" (rtd-name parent))))
    (let ((existing (and uid (gethash uid *nongenerative-types*))))
      (or existing
	  (let* ((own (map 'simple-vector #'field-spec fields))
		 (rtd (%make-rtd :name name :parent parent :uid uid
				 :sealed (truthy sealed)
				 :opaque (or (truthy opaque) (and parent (rtd-opaque parent)))
				 :fields own
				 :count (+ (if parent (rtd-count parent) 0) (length own)))))
	    (when uid (setf (gethash uid *nongenerative-types*) rtd))
	    rtd)))))

(defprim "record-type-descriptor?" (x) (ps:true? (rtd-p x)))

(defprim "make-record-constructor-descriptor" (rtd parent-rcd protocol)
  (check-rtd "make-record-constructor-descriptor" rtd)
  (let ((parent-rcd (if (eq parent-rcd ps:false) nil parent-rcd))
	(protocol (if (eq protocol ps:false) nil protocol)))
    (when (and parent-rcd (not (rtd-parent rtd)))
      (ps:scheme-error "make-record-constructor-descriptor: ~A has no parent" (rtd-name rtd)))
    (%make-rcd rtd parent-rcd protocol)))

;;; record-constructor (6.3).  A protocol receives a procedure p and
;;; returns the constructor.  For a base type p takes the field values;
;;; for a derived type p takes the parent constructor's arguments and
;;; returns a procedure that takes this type's own field values.  The
;;; record built always has the *most derived* type, FINAL.

(defun parent-rcd-of (rcd)
  (or (rcd-parent rcd)
      (let ((parent (rtd-parent (rcd-rtd rcd))))
	(and parent (%make-rcd parent nil nil)))))

(defun default-protocol (rtd)
  (let ((parent (rtd-parent rtd)))
    (if (null parent)
	#'identity
	(let ((nparent (rtd-count parent)))
	  (lambda (n)
	    (lambda (&rest all)
	      (apply (apply n (subseq all 0 (min nparent (length all))))
		     (nthcdr nparent all))))))))

(defun constructor-maker (rcd final tail)
  "The procedure the protocol of RCD is handed, when building a record of
type FINAL whose fields after RCD's type's are TAIL."
  (let* ((rtd (rcd-rtd rcd))
	 (nown (length (rtd-fields rtd)))
	 (check (lambda (own)
		  (unless (= (length own) nown)
		    (ps:scheme-error "~A constructor: expected ~D field values, got ~D"
				     (rtd-name rtd) nown (length own))))))
    (if (null (rtd-parent rtd))
	(lambda (&rest own)
	  (funcall check own)
	  (%make-record final (coerce (append own tail) 'simple-vector)))
	(let ((parent-rcd (parent-rcd-of rcd)))
	  (lambda (&rest parent-args)
	    (lambda (&rest own)
	      (funcall check own)
	      (apply (build-constructor parent-rcd final (append own tail)) parent-args)))))))

(defun build-constructor (rcd final tail)
  (funcall (or (rcd-protocol rcd) (default-protocol (rcd-rtd rcd)))
	   (constructor-maker rcd final tail)))

(defprim "record-constructor" (rcd)
  (unless (rcd-p rcd) (ps:scheme-error "record-constructor: not a constructor descriptor: ~S" rcd))
  (build-constructor rcd (rcd-rtd rcd) '()))

(defun rtd-descends-p (rtd ancestor)
  (loop for r = rtd then (rtd-parent r)
	while r
	thereis (eq r ancestor)))

(defprim "record-predicate" (rtd)
  (check-rtd "record-predicate" rtd)
  (lambda (x) (ps:true? (and (record-p x) (rtd-descends-p (record-rtd x) rtd)))))

(defun field-index (who rtd k)
  (unless (and (integerp k) (< -1 k (length (rtd-fields rtd))))
    (ps:scheme-error "~A: bad field index ~S for ~A" who k (rtd-name rtd)))
  (+ (- (rtd-count rtd) (length (rtd-fields rtd))) k))

(defprim "record-accessor" (rtd k)
  (check-rtd "record-accessor" rtd)
  (let ((i (field-index "record-accessor" rtd k))
	(name (cdr (svref (rtd-fields rtd) k))))
    (lambda (x)
      (unless (and (record-p x) (rtd-descends-p (record-rtd x) rtd))
	(r6rs-assertion-violation name "not a record of the right type" x))
      (svref (record-values x) i))))

(defprim "record-mutator" (rtd k)
  (check-rtd "record-mutator" rtd)
  (let ((i (field-index "record-mutator" rtd k))
	(field (svref (rtd-fields rtd) k)))
    (unless (car field)
      (ps:scheme-error "record-mutator: field ~A of ~A is immutable"
		       (ps:scheme-symbol-name (cdr field)) (rtd-name rtd)))
    (lambda (x v)
      (unless (and (record-p x) (rtd-descends-p (record-rtd x) rtd))
	(r6rs-assertion-violation (cdr field) "not a record of the right type" x))
      (setf (svref (record-values x) i) v)
      ps:unspecific)))

;;; 6.4 Inspection

(defprim "record?" (x) (ps:true? (and (record-p x) (not (rtd-opaque (record-rtd x))))))
(defprim "record-rtd" (x)
  (unless (and (record-p x) (not (rtd-opaque (record-rtd x))))
    (r6rs-assertion-violation "record-rtd" "not a non-opaque record" x))
  (record-rtd x))
(defprim "record-type-name" (rtd) (check-rtd "record-type-name" rtd) (rtd-name rtd))
(defprim "record-type-parent" (rtd) (check-rtd "record-type-parent" rtd) (or (rtd-parent rtd) ps:false))
(defprim "record-type-uid" (rtd) (check-rtd "record-type-uid" rtd) (or (rtd-uid rtd) ps:false))
(defprim "record-type-generative?" (rtd) (check-rtd "record-type-generative?" rtd) (ps:true? (null (rtd-uid rtd))))
(defprim "record-type-sealed?" (rtd) (check-rtd "record-type-sealed?" rtd) (ps:true? (rtd-sealed rtd)))
(defprim "record-type-opaque?" (rtd) (check-rtd "record-type-opaque?" rtd) (ps:true? (rtd-opaque rtd)))
(defprim "record-type-field-names" (rtd)
  (check-rtd "record-type-field-names" rtd)
  (map 'simple-vector #'cdr (rtd-fields rtd)))
(defprim "record-field-mutable?" (rtd k)
  (check-rtd "record-field-mutable?" rtd)
  (field-index "record-field-mutable?" rtd k)
  (ps:true? (car (svref (rtd-fields rtd) k))))

