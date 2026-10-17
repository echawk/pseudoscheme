; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-LIBGUILE -*-

;;;; The bridge between Guile mode's objects and C's (docs/libguile.md).
;;;;
;;;; C sees Guile objects in Guile's own layout (abi.lisp).  A Lisp object
;;;; C is given gets a *mirror*: memory from our libguile's heap (core.c)
;;;; laid out as Guile lays out that kind of object, whose hidden first
;;;; word is a handle, the index in *ENTRIES* of the mirror's entry.  An
;;;; object C makes (a pair from inline scm_cons, a smob) that the Lisp
;;;; side is given gets a Lisp counterpart: a pair becomes a cons, and
;;;; from then on the memory is that cons's mirror; a smob, which only C
;;;; understands, becomes a C-OBJECT that holds its address.
;;;;
;;;; Mutable objects (pairs, vectors, variables, bytevectors, structs)
;;;; exist twice, so the two copies are synchronized when the object
;;;; crosses between C and Lisp: EXPORT writes the Lisp object's current
;;;; contents to its mirror, IMPORT the mirror's to the Lisp object.  Each
;;;; entry keeps the words last written to its mirror (its snapshot), so
;;;; a word that differs from the snapshot is one C changed, and only
;;;; those are copied back: what Lisp changed meanwhile isn't undone.
;;;; Synchronization goes all through the structure (a list's spine and
;;;; elements), once per crossing (*EPOCH*), except that a mirror isn't
;;;; written again if no Lisp code has run since (*LISP-GEN*), and the
;;;; functions that only read their arguments (:shallow, impl.lisp's
;;;; READER-FUNCTION-P) read back a bounded depth (*IMPORT-DEPTH*).  An object C keeps
;;;; and changes without passing it back isn't seen by Lisp until it
;;;; next crosses; that is the bridge's one departure from Guile.
;;;;
;;;; Lifetimes: an entry holds its Lisp object while the mirror lives.
;;;; The collector in core.c tells us (psg_take_unreachable) when C can
;;;; no longer reach a mirror; then the entry goes, unless the object was
;;;; exported again meanwhile.  A C-OBJECT keeps its C memory alive with
;;;; a root cell until the C-OBJECT is collected by SBCL.

(in-package "PSEUDOSCHEME-LIBGUILE")

(defmacro pg (name) `(load-time-value (intern ,(string name) "PSEUDOSCHEME-GUILE")))

;;; ------------------------------------------------------------------
;;; The library

(defvar *library* nil "The loaded libguile-3.0.so.1's path, once loaded.")

(defun library-path ()
  (or (uiop:getenv "PSEUDOSCHEME_LIBGUILE")
      (namestring (asdf:system-relative-pathname :pseudoscheme "build/libguile/libguile-3.0.so.1"))))

(defmacro c-call (name return &rest args)
  "Call our libguile's C function NAME; ARGS alternate types and values."
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien ,name (function ,return ,@(loop for (type) on args by #'cddr collect type)))
    ,@(loop for (nil value) on args by #'cddr collect value)))

(defun sap-word (sap offset) (sb-sys:sap-ref-64 sap offset))
(defun (setf sap-word) (value sap offset) (setf (sb-sys:sap-ref-64 sap offset) value))

(declaim (inline address-sap))
(defun address-sap (address) (sb-sys:int-sap address))

(declaim (inline word))
(defun word (address i) (sb-sys:sap-ref-64 (address-sap address) (* 8 i)))
(defun (setf word) (value address i)
  (setf (sb-sys:sap-ref-64 (address-sap address) (* 8 i)) (ldb (byte 64 0) value)))

(defun hidden (address i) (word address (- i 2)))
(defun (setf hidden) (value address i) (setf (word address (- i 2)) value))

(defun heap-object-p (address)
  (/= 0 (c-call "psg_heap_object_p" sb-alien:int sb-sys:system-area-pointer (address-sap address))))

(defun alloc (bytes &optional pointerless)
  (sb-sys:sap-int (if pointerless
		      (c-call "psg_alloc_pointerless" sb-sys:system-area-pointer sb-alien:unsigned-long bytes)
		      (c-call "psg_alloc" sb-sys:system-area-pointer sb-alien:unsigned-long bytes))))

;;; ------------------------------------------------------------------
;;; Entries

(defstruct (entry (:constructor make-entry (object address)))
  object address
  (snapshot nil)			; the words (or bytes) last written
  (epoch -1)				; the crossing it was last synchronized in
  (depth -1)				; how far (levels) it was then absorbed
  (gen -1)				; the *LISP-GEN* its mirror was last refreshed in
  (dead nil)				; its mirror is gone (REAP)
  (pins 0))				; calls to C now holding it as an argument

(defvar *lock* (sb-thread:make-mutex :name "libguile bridge"))
(defvar *entries* (make-array 1024 :adjustable t :fill-pointer 1)
  "Handle -> entry; handle 0 is no entry.")
(defvar *free-handles* '())
(defvar *mirrors* (make-hash-table :test 'eq) "Lisp object -> its entry.")
(defvar *c-objects* (make-hash-table :test 'eql :weakness :value)
  "Address -> the C-OBJECT for C's object there.")
(defvar *epoch* 0 "Counts crossings between C and Lisp.")
(defvar *lisp-gen* 0
  "Counts the times Lisp code that may have changed a mirrored object has
run.  A mirror refreshed in the current generation is up to date with
its Lisp object, so EXPORT-SCM needn't write it again: C calls that only
read (docs/libguile.md, \"Dirty tracking\") don't advance it.")
(defmacro lisp-ran () '(incf *lisp-gen*))
(defvar *import-depth* most-positive-fixnum
  "How many levels below an argument IMPORT-SCM brings up to date, for
functions that read only the top of a structure (caar, hashq-set!).")
(defvar *variable-entries* (make-array 256 :adjustable t :fill-pointer 0)
  "The entries of variables, which C reads and writes in place
(SCM_VARIABLE_REF) between crossings: SYNC-VARIABLES-TO-C and
SYNC-VARIABLES-TO-LISP bring them up to date at the crossings where
Lisp code runs.")
(defvar *variables-dead* nil "True if some entries of *VARIABLE-ENTRIES* are dead.")
(defvar *refreshed* 0 "Entries written to their mirrors so far, for the profile.")
(defvar *absorbed* 0 "Entries read back from their mirrors so far, for the profile.")
(defvar *variables-gen* -1 "The *LISP-GEN* at the last SYNC-VARIABLES-TO-C.")
(defvar *c-ports*)			; api.lisp

(defmacro with-bridge (&body body)
  `(sb-thread:with-recursive-lock (*lock*) ,@body))

(defun new-entry (object address)
  (let ((e (make-entry object address))
	(handle (or (pop *free-handles*) (vector-push-extend nil *entries*))))
    (setf (aref *entries* handle) e
	  (gethash object *mirrors*) e
	  (hidden address 0) handle
	  (hidden address 1) 1)		; exported: not unreachable yet
    (c-call "psg_watch" sb-alien:void sb-sys:system-area-pointer (address-sap address))
    (when (typep object (pg gvariable)) (vector-push-extend e *variable-entries*))
    e))

(defun entry-at (address)
  "The entry of the mirror at ADDRESS, or NIL."
  (and (heap-object-p address)
       (let ((handle (hidden address 0)))
	 (and (< 0 handle (fill-pointer *entries*)) (aref *entries* handle)))))

(defun reap ()
  "Drop the entries of the mirrors C can no longer reach."
  (sb-alien:with-alien ((buffer (array sb-alien:unsigned-long 256)))
    (loop
      (let ((n (c-call "psg_take_unreachable" sb-alien:unsigned-long
		       sb-sys:system-area-pointer (sb-alien:alien-sap buffer) sb-alien:unsigned-long 256)))
	(when (zerop n) (return))
	(dotimes (i n)
	  (let* ((address (sb-alien:deref buffer i))
		 (handle (hidden address 0)))
	    (cond ((logbitp 0 (hidden address 1))
		   ;; exported again since: watch it again
		   (setf (hidden address 1) (logandc2 (hidden address 1) 1))
		   (c-call "psg_watch" sb-alien:void sb-sys:system-area-pointer (address-sap address)))
		  ((and (< 0 handle (fill-pointer *entries*)) (aref *entries* handle)
			(plusp (entry-pins (aref *entries* handle))))
		   ;; an argument of a call to C that hasn't returned: C
		   ;; may have dropped it, but the call will read it again
		   (c-call "psg_watch" sb-alien:void sb-sys:system-area-pointer (address-sap address)))
		  ((and (< 0 handle (fill-pointer *entries*)) (aref *entries* handle))
		   (let ((e (aref *entries* handle)))
		     (remhash (entry-object e) *mirrors*)
		     (setf (entry-dead e) t)
		     (when (typep (entry-object e) (pg gvariable)) (setq *variables-dead* t))
		     (setf (aref *entries* handle) nil
			   (hidden address 0) 0)
		     (push handle *free-handles*))))))))))

;;; C's own objects, as the Lisp side holds them
;;; Two classes of them: a smob whose type has an apply function
;;; (scm_set_smob_apply) can be called as a procedure, so it is a
;;; funcallable instance; the others are not procedures
(defstruct (c-plain (:constructor %make-c-plain (address root)))
  address root)

(defclass c-applicable (sb-mop:funcallable-standard-object)
  ((address :initarg :address) (root :initarg :root))
  (:metaclass sb-mop:funcallable-standard-class))

(deftype c-object () '(or c-plain c-applicable))
(defun c-object-p (x) (typep x 'c-object))
(defun c-object-address (x) (if (c-plain-p x) (c-plain-address x) (slot-value x 'address)))
(defun c-object-root (x) (if (c-plain-p x) (c-plain-root x) (slot-value x 'root)))

(defun %make-c-object (address root)
  (if (smob-applicable-p address)
      (let ((o (make-instance 'c-applicable :address address :root root)))
	(sb-mop:set-funcallable-instance-function o (lambda (&rest args) (apply-c-object o args)))
	o)
      (%make-c-plain address root)))

(defun c-object (address)
  (or (gethash address *c-objects*)
      (let* ((root (c-call "psg_make_root" sb-alien:unsigned-long sb-sys:system-area-pointer (address-sap address)))
	     (o (%make-c-object address root)))
	(sb-ext:finalize o (lambda () (c-call "psg_free_root" sb-alien:void sb-alien:unsigned-long root))
			 :dont-save t)
	(setf (gethash address *c-objects*) o))))

(defun print-c-object (o stream)
  (if (smob-p o)
      (write-string (print-smob o) stream)
      (format stream "#<c-object ~(~X~)>" (c-object-address o))))

(defmethod print-object ((o c-plain) stream) (print-c-object o stream))
(defmethod print-object ((o c-applicable) stream) (print-c-object o stream))

;;; ------------------------------------------------------------------
;;; Immediates

(defun immediate-bits (x)
  "X's SCM bits if it is an immediate, else NIL."
  (cond ((and (integerp x) (<= (- -1 +most-positive-fixnum+) x +most-positive-fixnum+))
	 (ldb (byte 64 0) (logior (ash x 2) +fixnum-tag+)))
	((characterp x) (logior (ash (char-code x) 8) +char-tag+))
	((eq x ps:false) +false+)
	((eq x ps:true) +true+)
	((null x) +null+)
	((eq x ps:unspecific) +unspecified+)
	((eq x ps:eof-object) +eof+)
	((eq x (symbol-value (pg *elisp-nil*))) +elisp-nil+)
	((eq x (symbol-value (pg +unbound+))) +undefined+)
	(t nil)))

(defun bits-double (w)
  (sb-kernel:make-double-float (let ((hi (ldb (byte 32 32) w))) (if (logbitp 31 hi) (- hi (ash 1 32)) hi))
			       (ldb (byte 32 0) w)))

(defun bits-immediate (bits)
  "The immediate whose SCM bits are BITS, and T; or NIL, NIL for a pointer."
  (cond ((= (logand bits 3) +fixnum-tag+)
	 (values (let ((n (ash bits -2))) (if (logbitp 61 n) (- n (ash 1 62)) n)) t))
	((= (logand bits #xff) +char-tag+) (values (code-char (ash bits -8)) t))
	((= bits +false+) (values ps:false t))
	((= bits +true+) (values ps:true t))
	((= bits +null+) (values '() t))
	((= bits +unspecified+) (values ps:unspecific t))
	((= bits +eof+) (values ps:eof-object t))
	((= bits +elisp-nil+) (values (symbol-value (pg *elisp-nil*)) t))
	((= bits +undefined+) (values (symbol-value (pg +unbound+)) t))
	((/= (logand bits 7) 0) (error "libguile: can't decode the SCM ~X" bits))
	(t (values nil nil))))

;;; ------------------------------------------------------------------
;;; Export: Lisp -> C

(defvar *deep* t "Whether synchronization goes all through a structure.")

(defun export-scm (x)
  "X as an SCM, its mirror written up to date."
  (or (immediate-bits x)
      (typecase x
	(double-float
	 (let ((a (alloc 16 t)))
	   (setf (word a 0) +tc16-flonum+
		 (word a 1) (ldb (byte 64 0) (sb-kernel:double-float-bits x)))
	   a))
	(c-object (c-object-address x))
	(t (with-bridge
	     (let ((e (gethash x *mirrors*)))
	       (cond (e (setf (hidden (entry-address e) 1) (logior (hidden (entry-address e) 1) 1))
			(when (and *deep* (/= (entry-gen e) *lisp-gen*))
			  (refresh e))
			(entry-address e))
		     (t (entry-address (make-mirror x))))))))))

(defun mutable-p (x)
  (or (consp x) (simple-vector-p x) (typep x 'ps-r6rs::octets)
      (typep x (pg gvariable)) (funcall (pg struct-p) x)))

(defun make-mirror (x)
  (let ((e (new-entry x (allocate-mirror x))))
    (when (mutable-p x) (refresh e))
    e))

(defun allocate-mirror (x)
  (flet ((cell (n tag &optional pointerless)
	   (let ((a (alloc (* 8 n) pointerless)))
	     (setf (word a 0) tag)
	     a)))
    (cond ((consp x) (alloc 16))
	  ((simple-vector-p x) (cell (1+ (length x)) (logior (ash (length x) 8) +tc7-vector+)))
	  ((typep x 'ps-r6rs::octets)
	   (let ((a (cell 4 (logior +bytevector-u8-flags+ +tc7-bytevector+ #x80)))
		 (buffer (alloc (max 1 (length x)) t)))
	     (setf (word a 1) (length x) (word a 2) buffer (word a 3) +false+)
	     a))
	  ((typep x (pg gvariable)) (cell 2 +tc7-variable+))
	  ((funcall (pg struct-p) x)
	   (cell (1+ (length (funcall (pg struct-slots-of) x))) 0))
	  ((stringp x) (let ((a (cell 4 +tc7-string+ t))) (setf (word a 3) (length x)) a))
	  ((keywordp x) (cell 2 +tc7-keyword+ t))
	  ((symbolp x) (cell 3 +tc7-symbol+ t))
	  ((integerp x) (cell 2 +tc16-bignum+ t))
	  ((rationalp x) (let ((a (cell 3 +tc16-fraction+)))
			   (setf (word a 1) (export-scm (numerator x)) (word a 2) (export-scm (denominator x)))
			   a))
	  ((complexp x) (let ((a (cell 3 +tc16-complex+ t)))
			  (setf (word a 1) (sb-kernel:double-float-bits (coerce (realpart x) 'double-float))
				(word a 2) (sb-kernel:double-float-bits (coerce (imagpart x) 'double-float)))
			  a))
	  ((typep x (pg gpointer)) (let ((a (cell 2 +tc7-pointer+ t)))
				     (setf (word a 1) (funcall (pg gpointer-address) x))
				     a))
	  ((functionp x) (cell 2 (logior #x200 +tc7-program+) t))
	  ((typep x (pg ghash)) (cell 2 +tc7-hashtable+ t))
	  ((typep x (pg fluid)) (cell 2 +tc7-fluid+ t))
	  ((streamp x)
	   (let ((a (cell 4 (logior #x500 +tc7-port+) t))
		 (c (gethash x *c-ports*)))
	     (when c
	       (setf (word a 0) (logior (third c) #x100 +tc7-port+)
		     (word a 1) (second c) (word a 3) (first c)))
	     a))
	  (t (cell 2 +tc7-smob+ t)))))

(defun refresh (e)
  "Write E's object's contents to its mirror (and on, through what it holds)."
  (setf (entry-epoch e) *epoch* (entry-depth e) most-positive-fixnum (entry-gen e) *lisp-gen*)
  (incf *refreshed*)
  (let ((x (entry-object e)) (a (entry-address e)))
    (cond ((consp x)
	   ;; along the spine without recursion
	   (loop
	     (let ((car-bits (export-scm (car x)))
		   (cdr (cdr x)))
	       (setf (word a 0) car-bits)
	       (cond ((and (consp cdr) (let ((next (gethash cdr *mirrors*)))
					 (or (null next) (/= (entry-gen next) *lisp-gen*))))
		      (let ((next (or (gethash cdr *mirrors*) (new-entry cdr (alloc 16)))))
			(setf (word a 1) (entry-address next)
			      (entry-snapshot e) (vector car-bits (entry-address next)))
			(setf e next x cdr a (entry-address next)
			      (entry-epoch e) *epoch* (entry-depth e) most-positive-fixnum
			      (entry-gen e) *lisp-gen*)))
		     (t (let ((cdr-bits (export-scm cdr)))
			  (setf (word a 1) cdr-bits
				(entry-snapshot e) (vector car-bits cdr-bits)))
			(return))))))
	  ((simple-vector-p x)
	   (let ((snapshot (make-array (length x))))
	     (dotimes (i (length x))
	       (setf (svref snapshot i) (export-scm (svref x i))
		     (word a (1+ i)) (svref snapshot i)))
	     (setf (entry-snapshot e) snapshot)))
	  ((typep x 'ps-r6rs::octets)
	   (let ((buffer (word a 2)))
	     (dotimes (i (length x))
	       (setf (sb-sys:sap-ref-8 (address-sap buffer) i) (aref x i)))
	     (setf (entry-snapshot e) (copy-seq x))))
	  ((typep x (pg gvariable))
	   (let* ((value (funcall (pg gvariable-value) x))
		  (bits (export-scm value)))
	     (setf (word a 1) bits (entry-snapshot e) (vector bits value))))
	  ((funcall (pg struct-p) x)
	   (let* ((vtable (funcall (pg struct-vtable-of) x))
		  (slots (funcall (pg struct-slots-of) x))
		  (raw (raw-slots vtable (length slots)))
		  (snapshot (make-array (length slots))))
	     (setf (word a 0) (logior (if (eq vtable x) a (export-scm vtable)) +tc3-struct+))
	     (dotimes (i (length slots))
	       (setf (svref snapshot i) (if (logbitp i raw)
					    (let ((v (svref slots i))) (if (integerp v) (ldb (byte 64 0) v) 0))
					    (export-scm (svref slots i)))
		     (word a (1+ i)) (svref snapshot i)))
	     (setf (entry-snapshot e) snapshot))))))

(defun raw-slots (vtable n)
  "A bit mask of the slots of VTABLE's structs that are unboxed words
(layout letter u) rather than SCM values."
  (let* ((layout (svref (funcall (pg struct-slots-of) vtable) 0))
	 (s (if (symbolp layout) (ps:scheme-symbol-name layout) (princ-to-string layout)))
	 (mask 0))
    (dotimes (i (min n (floor (length s) 2)) mask)
      (when (char= (char s (* 2 i)) #\u) (setq mask (logior mask (ash 1 i)))))))

;;; ------------------------------------------------------------------
;;; Import: C -> Lisp

(defun import-scm (bits)
  "The Lisp object for the SCM BITS, brought up to date with its mirror."
  (multiple-value-bind (x immediate) (bits-immediate bits)
    (if immediate
	x
	(with-bridge
	  (let ((e (entry-at bits)))
	    (cond (e (when (and (mutable-p (entry-object e))
				(or (/= (entry-epoch e) *epoch*) (> *import-depth* (entry-depth e))))
		       (absorb e))
		     (entry-object e))
		  (t (import-c-object bits))))))))

(defun import-c-object (a)
  "C's object at A, which has no entry: what it is in Lisp."
  (when (< a 4096)			; an SCM C never set: not an object
    (return-from import-c-object ps:unspecific))
  (let ((w0 (word a 0)))
    (cond ((zerop (logand w0 1))	; a pair C made
	   (if (heap-object-p a)
	       (let* ((x (cons nil nil)) (e (new-entry x a)))
		 (setf (entry-snapshot e) (vector 0 0))
		 (absorb e)
		 x)
	       (cons (import-scm (word a 0)) (import-scm (word a 1)))))
	  ((= (logand w0 #xffff) +tc16-flonum+)
	   (bits-double (word a 1)))
	  ((= (logand w0 #x7f) +tc7-string+) (static-string a))
	  (t (c-object a)))))

(defun absorb-bounded (e depth)
  "ABSORB for a pair, a function that reads DEPTH levels below it: its
two words, and its children's to DEPTH-1 levels."
  (setf (entry-depth e) (if (= (entry-epoch e) *epoch*) (max depth (entry-depth e)) depth)
	(entry-epoch e) *epoch*)
  (let ((x (entry-object e)) (a (entry-address e)) (snapshot (entry-snapshot e)))
    (let ((w0 (word a 0)) (w1 (word a 1)))
      (if (= w0 (svref snapshot 0))
	  (when (and (plusp depth) (mutable-p (car x))) (sync-child-bounded (car x) (1- depth)))
	  (setf (car x) (let ((*deep* nil) (*import-depth* most-positive-fixnum)) (import-scm w0))
		(svref snapshot 0) w0))
      (if (= w1 (svref snapshot 1))
	  (when (and (plusp depth) (mutable-p (cdr x))) (sync-child-bounded (cdr x) (1- depth)))
	  (setf (cdr x) (let ((*deep* nil) (*import-depth* most-positive-fixnum)) (import-scm w1))
		(svref snapshot 1) w1)))))

(defun sync-child-bounded (x depth)
  (let ((e (gethash x *mirrors*)))
    (when (and e (entry-snapshot e) (or (/= (entry-epoch e) *epoch*) (> depth (entry-depth e))))
      (if (consp x) (absorb-bounded e depth) (absorb e)))))

(defun absorb (e)
  "Copy into E's object what C changed in its mirror since the snapshot."
  (when (and (consp (entry-object e)) (< *import-depth* most-positive-fixnum))
    (return-from absorb (absorb-bounded e *import-depth*)))
  (setf (entry-epoch e) *epoch* (entry-depth e) most-positive-fixnum)
  (incf *absorbed*)
  (let ((x (entry-object e)) (a (entry-address e)) (snapshot (entry-snapshot e)))
    (cond ((consp x)
	   (loop
	     (let ((w0 (word a 0)) (w1 (word a 1)))
	       (unless (= w0 (svref snapshot 0))
		 (setf (car x) (let ((*deep* *deep*)) (import-scm w0)) (svref snapshot 0) w0))
	       (cond ((= w1 (svref snapshot 1))
		      ;; the same cdr: on along it, if it's a mirror due
		      (let ((next (and (consp (cdr x)) *deep* (gethash (cdr x) *mirrors*))))
			(if (and next (/= (entry-epoch next) *epoch*) (entry-snapshot next))
			    (setf e next x (cdr x) a (entry-address next) snapshot (entry-snapshot e)
				  (entry-epoch e) *epoch* (entry-depth e) most-positive-fixnum)
			    (return))))
		     (t (setf (svref snapshot 1) w1
			      (cdr x) (import-scm w1))
			(return))))
	     (unless *deep* (return))))
	  ((simple-vector-p x)
	   (dotimes (i (length x))
	     (let ((w (word a (1+ i))))
	       (if (= w (svref snapshot i))
		   (when *deep* (sync-child (svref x i)))
		   (setf (svref x i) (import-scm w) (svref snapshot i) w)))))
	  ((typep x 'ps-r6rs::octets)
	   (let ((buffer (address-sap (word a 2))))
	     (dotimes (i (length x))
	       (let ((b (sb-sys:sap-ref-8 buffer i)))
		 (unless (= b (aref snapshot i))
		   (setf (aref x i) b (aref snapshot i) b))))))
	  ((typep x (pg gvariable))
	   (let ((w (word a 1)))
	     (unless (= w (svref snapshot 0))
	       (let ((value (import-scm w)))
		 (setf (svref snapshot 0) w (svref snapshot 1) value)
		 (funcall (fdefinition (list 'setf (pg gvariable-value))) value x)))))
	  ((funcall (pg struct-p) x)
	   (let* ((slots (funcall (pg struct-slots-of) x))
		  (raw (raw-slots (funcall (pg struct-vtable-of) x) (length slots))))
	     (dotimes (i (length slots))
	       (let ((w (word a (1+ i))))
		 (unless (= w (svref snapshot i))
		   (setf (svref snapshot i) w
			 (svref slots i) (if (logbitp i raw) w (import-scm w)))))))))))

(defun sync-child (x)
  (let ((e (and (mutable-p x) (gethash x *mirrors*))))
    (when (and e (/= (entry-epoch e) *epoch*) (entry-snapshot e))
      (absorb e))))

(defvar *static-strings* (make-hash-table :test 'eql))

(defun static-string (a)
  "A string in C's static data (SCM_IMMUTABLE_STRING): its stringbuf,
start and length; the buffer's characters are bytes, or 32-bit
characters if its first word has #x400."
  (or (gethash a *static-strings*)
      (setf (gethash a *static-strings*)
	    (let* ((buf (word a 1)) (start (word a 2)) (n (word a 3))
		   (wide (logbitp 10 (word buf 0)))
		   (chars (+ buf 16)))
	      (let ((s (make-string n)))
		(dotimes (i n s)
		  (setf (char s i)
			(code-char (if wide
				       (sb-sys:sap-ref-32 (address-sap chars) (* 4 (+ start i)))
				       (sb-sys:sap-ref-8 (address-sap chars) (+ start i)))))))))))

;;; ------------------------------------------------------------------
;;; Crossings

;;; Variables are read and written by C's inline macros in place, so
;;; unlike the other mutable objects they are brought up to date at each
;;; crossing, not just when they are passed.
(defun sync-variables-to-c ()
  "Write what Lisp changed in variables C holds to their mirrors.  Only
Lisp code can have changed them, so nothing is done if none has run since."
  (unless (= *variables-gen* *lisp-gen*)
    (setq *variables-gen* *lisp-gen*)
    (with-bridge
      (let ((entries *variable-entries*))
	(dotimes (i (length entries))
	  (let ((e (aref entries i)))
	    (unless (entry-dead e)
	      (let ((snapshot (entry-snapshot e)))
		(when (and snapshot (not (eq (svref snapshot 1) (funcall (pg gvariable-value) (entry-object e)))))
		  (refresh e))))))))))

(defun sync-variables-to-lisp ()
  "Copy into variables what C wrote in their mirrors."
  (when (plusp (length *variable-entries*))
    (with-bridge
      (when *variables-dead*
	(setq *variables-dead* nil)
	(let ((entries *variable-entries*) (n 0))
	  (dotimes (i (length entries))
	    (unless (entry-dead (aref entries i))
	      (setf (aref entries n) (aref entries i))
	      (incf n)))
	  (setf (fill-pointer entries) n)))
      (let ((entries *variable-entries*))
	(dotimes (i (length entries))
	  (let* ((e (aref entries i)) (snapshot (entry-snapshot e)))
	    (when (and snapshot (/= (word (entry-address e) 1) (svref snapshot 0)))
	      (absorb e))))))))

;;; The mirrors of a call's arguments stay while the call runs: C may
;;; drop them, and the collector find them unreachable, before it returns
(defun pin-words (words delta)
  (with-bridge
    (dolist (w words)
      (unless (nth-value 1 (bits-immediate w))
	(let ((e (entry-at w)))
	  (when e (incf (entry-pins e) delta)))))))

(defmacro with-pinned-words ((words) &body body)
  `(let ((words ,words))
     (pin-words words 1)
     (unwind-protect (progn ,@body) (pin-words words -1))))

(defmacro c-funcall-same-gen (&rest alien-funcall-arguments)
  "C-FUNCALL, for a loop in which no Lisp code that changes mirrored
objects runs between the calls (a fold over a table): objects the calls
pass back and forth aren't written to their mirrors again each time."
  `(progn (sync-variables-to-c)
	  (multiple-value-prog1 (sb-alien:alien-funcall ,@alien-funcall-arguments)
	    (sync-variables-to-lisp))))

(defmacro c-funcall (&rest alien-funcall-arguments)
  "ALIEN-FUNCALL of C code that may read or write variables."
  `(progn (lisp-ran)
	  (sync-variables-to-c)
	  (multiple-value-prog1 (sb-alien:alien-funcall ,@alien-funcall-arguments)
	    (sync-variables-to-lisp))))

(defmacro crossing (&body body)
  "BODY, as a new crossing between C and Lisp."
  `(progn (incf *epoch*) ,@body))
