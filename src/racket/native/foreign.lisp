; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; #%foreign, natively: C types, pointers, memory, calling C and being
;;;; called, libraries, errno.  It follows rumble's foreign.ss (Racket
;;;; CS on Chez's FFI) and is made of src/ffi.lisp's layer (CFFI) for
;;;; the calls, callbacks, dlopen and dlsym.
;;;;
;;;; A cpointer is a Racket struct (type cpointer, so equal? and hashing
;;;; work as ptr-equal?'s) of: BASE, OFFSET, TAGS, KEEPER, ORIGIN.
;;;;   BASE    NIL: OFFSET is a C address; else a Lisp vector (a byte
;;;;           string, or an flvector) the pointer points into at
;;;;           byte OFFSET.  This is Chez's "scheme object pointer":
;;;;           Racket CS's 'atomic and 'nonatomic malloc memory is a
;;;;           bytevector, and so is ours.  SBCL's collector moves
;;;;           vectors, so every access pins the vector for its
;;;;           duration and a call to C pins the vectors it passes (as
;;;;           Chez keeps the collector out of a foreign call): C may
;;;;           not keep such an address, as Racket's documentation
;;;;           also says; 'atomic-interior and 'interior memory is
;;;;           malloc'd, freed when the pointer is collected, for C to
;;;;           keep.  Accesses to vector memory are bounds-checked
;;;;           (Racket CS's aren't), since a wild write would corrupt
;;;;           the Lisp heap.
;;;;   KEEPER  an object a finalizer frees the malloc'd memory of
;;;;   ORIGIN  NIL, or the OFFSET the pointer was made from, for the
;;;;           pointers that are offset-ptr? (ptr-add's)
;;;; What Racket calls an fptr (the C-level pointer, the representation
;;;; of the primitive pointer types) is such a struct too; a NULL one
;;;; has no base and offset 0, and is #f as a cpointer.
;;;;
;;;; Not supported: a struct or union passed or returned by value
;;;; (CFFI's :struct-by-value needs libffi), variadic calls' ABI on
;;;; arm64 (varargs-after is ignored), extflonum vectors,
;;;; make-sized-byte-string, vector->cpointer and 'tagged, 'stubborn,
;;;; 'uncollectable, 'eternal memory (Racket CS doesn't either).  A
;;;; callback is never freed (src/ffi.lisp), and Racket exceptions
;;;; escape from one by unwinding.  _scheme values cross C as handles
;;;; (integers) of a table.

(in-package "QUASISCHEME-RACKET")

(defmacro defforeign (name lambda-list (&key leaf) &body body)
  `(defnative ,name ,lambda-list (:instance "#%foreign" :leaf ,leaf) ,@body))

(defmacro with-fp (&body body)
  `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact) ,@body))

;;; ------------------------------------------------------------------
;;; Small helpers

(defun rcall (name &rest args)
  (apply (native-function (gethash name *natives*)) args))

(defun wsym (who) (if (stringp who) (rsym who) who))

(defun arguments-error (who what &rest more)
  (apply #'rcall "raise-arguments-error" (wsym who) what more))

(defun unsupported (who &optional what)
  (raise-exn "exn:fail:unsupported"
	     (format nil "~A: ~A" (who-string who) (or what "unsupported"))))

(defun nonneg-fixnum-p (x) (typep x '(integer 0 #.(1- (expt 2 60)))))
(defun fixnum-p (x) (typep x '(signed-byte 61)))
(defun exact-nonneg-p (x) (and (integerp x) (>= x 0)))
(defun symbol-named-p (x name) (and (racket-symbol-p x) (string= (rname x) name)))
(defun false-p (x) (eq x qs:false))
(defun octets-p (x) (typep x 'octets))

(defun bad-ctype-value (first second value)
  "Racket's complaint that VALUE doesn't fit the C type named SECOND,
raised for FIRST (the arguments' order is rumble's, swapped as it is
there in places)."
  (when (false-p first) (arg-error "raise-arguments-error" "symbol?" first))
  (unless (racket-symbol-p second) (arg-error "symbol->string" "symbol?" second))
  (arguments-error first "given value does not fit primitive C type"
		   "C type" (make-unquoted-ps (rname second))
		   "value" value))

;;; ------------------------------------------------------------------
;;; Cpointers

(defvar *prop-cpointer*
  (%make-struct-type-property
   (rsym "prop:cpointer")
   (lambda (v info)
     (cond ((exact-nonneg-p v)
	    (unless (< v (nth 1 info))
	      (arguments-error "prop:cpointer" "index is out of range" "index" v))
	    (unless (member v (nth 5 info))
	      (arguments-error "prop:cpointer" "index does not refer to an immutable field" "index" v))
	    (+ v (let ((p (nth 6 info))) (if (false-p p) 0 (struct-type-total p)))))
	   ((accepts-arity-p v 1) v)
	   ((cp-p v) v)
	   (t (arg-error "prop:cpointer"
			 (format nil "(or/c exact-nonnegative-integer?~%      (procedure-arity-includes/c 1)~%      cpointer?)")
			 v))))
   '()))
(define-primitive "#%foreign" "prop:cpointer" *prop-cpointer*)

(defvar *cpointer-type* nil)
(defvar *ffi-obj-type* nil)
(defvar *callback-type* nil)

(defun cp-p (x)
  (and (rstruct-p x) *cpointer-type* (subtype-p (rstruct-type x) *cpointer-type*)))
(defun ffi-obj-p (x) (and (rstruct-p x) (subtype-p (rstruct-type x) *ffi-obj-type*)))
(defun callback-p (x) (and (rstruct-p x) (subtype-p (rstruct-type x) *callback-type*)))

(defmacro cp-field (x i) `(svref (rstruct-fields ,x) ,i))
(defmacro cp-base (x) `(cp-field ,x 0))
(defmacro cp-offset (x) `(cp-field ,x 1))
(defmacro cp-tags (x) `(cp-field ,x 2))
(defmacro cp-keeper (x) `(cp-field ,x 3))
(defmacro cp-origin (x) `(cp-field ,x 4))

(defun make-cp (base offset &optional (tags qs:false) keeper origin)
  (make-rstruct *cpointer-type* (vector base offset tags keeper origin)))

(defun cp-null-p (m) (and (null (cp-base m)) (zerop (cp-offset m))))
(defun fptr->cptr (m) (if (cp-null-p m) qs:false m))

(defun octet-length (base)
  (if (octets-p base) (length base) (* 8 (length base))))

(defun cp-address (m)
  "M's address: a C address, or the vector's right now (good only until
the collector next runs)."
  (let ((base (cp-base m)))
    (if base
	(sb-sys:with-pinned-objects (base)
	  (ldb (byte 64 0) (+ (sb-sys:sap-int (sb-sys:vector-sap base)) (cp-offset m))))
	(ldb (byte 64 0) (cp-offset m)))))

(defun cp-gcable-p (m) (or (cp-base m) (cp-keeper m)))

(defun ptr-equal-p (a b)
  (if (cp-base a)
      (and (eq (cp-base a) (cp-base b)) (= (cp-offset a) (cp-offset b)))
      (and (null (cp-base b)) (= (ldb (byte 64 0) (cp-offset a)) (ldb (byte 64 0) (cp-offset b))))))

(defun has-cpointer-property-p (x)
  (and (struct-instance-p x) (nth-value 1 (property-value x *prop-cpointer*))))

(defun cpointer-p (x)
  (or (cp-p x) (false-p x) (octets-p x) (has-cpointer-property-p x)))

(defun extract-cp (p)
  "The cpointer struct behind P's prop:cpointer, or NIL (never fails)."
  (let ((v (property-value p *prop-cpointer*)))
    (cond ((exact-nonneg-p v)
	   (let ((x (svref (struct-fields p) v)))
	     (if (cp-p x) x (extract-cp x))))
	  ((functionp v)
	   (let ((p2 (funcall v p)))
	     (if (cp-p p2) p2 (extract-cp p2))))
	  ((cp-p v) v)
	  (t nil))))

(defun cptr->fptr (who p)
  (cond ((cp-p p) p)
	((false-p p) (make-cp nil 0))
	((octets-p p) (make-cp p 0))
	(t (multiple-value-bind (v found) (property-value p *prop-cpointer*)
	     (unless found (arg-error who "cpointer?" p))
	     (cond ((exact-nonneg-p v)
		    (let ((x (svref (struct-fields p) v)))
		      (if (cpointer-p x) (cptr->fptr who x) (arg-error who "cpointer?" x))))
		   ((functionp v)
		    (let ((p2 (funcall v p)))
		      (unless (cpointer-p p2) (result-error "prop:cpointer-accessor" "cpointer?" p2))
		      (cptr->fptr who p2)))
		   (t (cptr->fptr who v)))))))

(defun cp-hash (x recur)
  (declare (ignore recur))
  (logand (+ (if (cp-base x) (eq-hash (cp-base x)) 0) (ldb (byte 62 0) (cp-offset x)))
	  most-positive-fixnum))

(defun make-foreign-struct-type (name parent fields &optional (props '()))
  (values (make-struct-type* (rsym name) (or parent qs:false) fields 0 qs:false props)))

(setq *cpointer-type*
      (make-foreign-struct-type
       "cpointer" nil 5
       (list (cons *prop-equal+hash*
		   (list (lambda (a b recur) (declare (ignore recur)) (bool (ptr-equal-p a b)))
			 #'cp-hash)))))
(setq *ffi-obj-type* (make-foreign-struct-type "cpointer/ffi-obj" *cpointer-type* 2))
(setq *callback-type* (make-foreign-struct-type "callback" *cpointer-type* 1))

(defun make-ffi-obj (address lib name)
  (make-rstruct *ffi-obj-type* (vector nil address qs:false nil nil lib name)))

(defun make-callback (address code)
  (make-rstruct *callback-type* (vector nil address qs:false nil nil code)))

(defun do-ptr-add (p n save-tags)
  (cond ((cp-p p)
	 (make-cp (cp-base p) (+ (cp-offset p) n) (if save-tags (cp-tags p) qs:false)
		  (cp-keeper p) (or (cp-origin p) (cp-offset p))))
	((octets-p p) (make-cp p n qs:false nil 0))
	((false-p p) (make-cp nil n qs:false nil 0))
	(t (let ((q (extract-cp p)))
	     (do-ptr-add (or q qs:false) n save-tags)))))

;;; Memory access, with the vector pinned

(defun call-with-cp-sap (who cp offset size function)
  "Call FUNCTION with a SAP SIZE bytes at OFFSET past CP, pinned."
  (let ((base (cp-base cp))
	(pos (+ (cp-offset cp) offset)))
    (cond ((null base) (funcall function (sb-sys:int-sap (ldb (byte 64 0) pos))))
	  (t (unless (<= 0 pos (- (octet-length base) size))
	       (racket-error (who-string who) "access outside the memory of a byte string~%  offset: ~D~%  size: ~D~%  memory size: ~D"
			     pos size (octet-length base)))
	     (sb-sys:with-pinned-objects (base)
	       (funcall function (sb-sys:sap+ (sb-sys:vector-sap base) pos)))))))

(defvar *handles* (make-hash-table :weakness :value :synchronized t)
  "_scheme values as C sees them: handle -> object.")
(defvar *cells* (make-hash-table :synchronized t) "Immobile cells' handles -> objects, kept.")
(defvar *handle-counter* #x7f000000)

(defun scheme-handle (object)
  (let ((h (incf *handle-counter* 16)))
    (setf (gethash h *handles*) object)
    h))

(defun handle-object (h)
  (multiple-value-bind (o found) (gethash h *handles*)
    (if found
	o
	(multiple-value-bind (o found) (gethash h *cells*)
	  (if found o (racket-error "ptr-ref" "not the handle of a _scheme value: ~D" h))))))

(defun rep-size (rep)
  (ecase rep
    (:void 0)
    (:boolean 4) (:float 4) (:integer-32 4) (:unsigned-32 4)
    (:integer-8 1) (:unsigned-8 1) (:integer-16 2) (:unsigned-16 2)
    ((:double :integer-64 :unsigned-64 :fixnum :ftype-pointer :ftype-scheme-object-pointer
      :scheme-object)
     8)))

(defun read-rep (rep m offset)
  "The C-level value of REP at OFFSET past the cpointer struct M."
  (call-with-cp-sap "ptr-ref" m offset (rep-size rep)
    (lambda (sap)
      (ecase rep
	(:boolean (if (zerop (sb-sys:sap-ref-32 sap 0)) qs:false qs:true))
	(:integer-8 (sb-sys:signed-sap-ref-8 sap 0))
	(:unsigned-8 (sb-sys:sap-ref-8 sap 0))
	(:integer-16 (sb-sys:signed-sap-ref-16 sap 0))
	(:unsigned-16 (sb-sys:sap-ref-16 sap 0))
	(:integer-32 (sb-sys:signed-sap-ref-32 sap 0))
	(:unsigned-32 (sb-sys:sap-ref-32 sap 0))
	((:integer-64 :fixnum) (sb-sys:signed-sap-ref-64 sap 0))
	(:unsigned-64 (sb-sys:sap-ref-64 sap 0))
	(:double (with-fp (sb-sys:sap-ref-double sap 0)))
	(:float (with-fp (coerce (sb-sys:sap-ref-single sap 0) 'double-float)))
	((:ftype-pointer :ftype-scheme-object-pointer) (make-cp nil (sb-sys:sap-ref-64 sap 0)))
	(:scheme-object (handle-object (sb-sys:sap-ref-64 sap 0)))))))

(defun write-rep (rep m offset v)
  (call-with-cp-sap "ptr-set!" m offset (rep-size rep)
    (lambda (sap)
      (ecase rep
	(:boolean (setf (sb-sys:sap-ref-32 sap 0) (if (false-p v) 0 1)))
	((:integer-8 :unsigned-8) (setf (sb-sys:sap-ref-8 sap 0) (ldb (byte 8 0) v)))
	((:integer-16 :unsigned-16) (setf (sb-sys:sap-ref-16 sap 0) (ldb (byte 16 0) v)))
	((:integer-32 :unsigned-32) (setf (sb-sys:sap-ref-32 sap 0) (ldb (byte 32 0) v)))
	((:integer-64 :unsigned-64 :fixnum) (setf (sb-sys:sap-ref-64 sap 0) (ldb (byte 64 0) v)))
	(:double (with-fp (setf (sb-sys:sap-ref-double sap 0) (coerce v 'double-float))))
	(:float (with-fp (setf (sb-sys:sap-ref-single sap 0) (coerce v 'single-float))))
	((:ftype-pointer :ftype-scheme-object-pointer)
	 (setf (sb-sys:sap-ref-64 sap 0) (cp-address v)))
	(:scheme-object (setf (sb-sys:sap-ref-64 sap 0) (scheme-handle v)))))
    ))

(defun nul-octets (m width)
  "The bytes of M's data up to a NUL of WIDTH bytes (a copy), or NIL for NULL."
  (if (cp-null-p m)
      nil
      (flet ((scan (sap limit)
	       (let ((n (loop for i from 0 by width
			      while (or (null limit) (<= (+ i width) limit))
			      until (zerop (ecase width
					     (1 (sb-sys:sap-ref-8 sap i))
					     (2 (sb-sys:sap-ref-16 sap i))
					     (4 (sb-sys:sap-ref-32 sap i))))
			      finally (return i))))
		 (let ((out (make-octets n)))
		   (dotimes (j n) (setf (aref out j) (sb-sys:sap-ref-8 sap j)))
		   out))))
	(let ((base (cp-base m)))
	  (if base
	      (sb-sys:with-pinned-objects (base)
		(scan (sb-sys:sap+ (sb-sys:vector-sap base) (cp-offset m))
		      (- (octet-length base) (cp-offset m))))
	      (scan (sb-sys:int-sap (cp-address m)) nil))))))

;;; ------------------------------------------------------------------
;;; Ctypes

(defstruct (fctype (:conc-name fct-))
  host-rep nonatomic basetype c->scheme scheme->c c->s s->c ref set
  members size alignment malloc-mode compound)

(defmethod print-object ((x fctype) stream)
  (format stream (if (fct-compound x) "#<compound-ctype>" "#<ctype>")))

(defun check-ctype (who x) (unless (fctype-p x) (arg-error who "ctype?" x)) x)

(defun identity-c->scheme (v) v)
(defun identity-scheme->c (v) v)
(defun identity-s->c (for-whom s) (declare (ignore for-whom)) s)

(defun checker (type-name pred)
  (lambda (for-whom x) (if (funcall pred x) x (bad-ctype-value for-whom type-name x))))

(defun make-prim (name rep basetype &key c->s s->c nonatomic (ref nil))
  (let ((who (rsym name))
	(s->c (or s->c #'identity-s->c)))
    (make-fctype
     :host-rep rep :nonatomic nonatomic :basetype (rsym basetype)
     :c->scheme #'identity-c->scheme :scheme->c #'identity-scheme->c
     :c->s (or c->s #'identity-c->scheme) :s->c s->c
     :ref (or ref
	      (lambda (p offset)
		(let ((raw (read-rep rep (cptr->fptr who p) offset)))
		  (if c->s (funcall c->s raw) raw))))
     :set (lambda (for-whom dest offset s)
	    (write-rep rep (cptr->fptr for-whom dest) offset (funcall s->c for-whom s))))))

(defmacro defctype (var name rep basetype &rest keys)
  `(progn (defvar ,var (make-prim ,name ,rep ,basetype ,@keys))
	  (define-primitive "#%foreign" ,name ,var)))

(defun int-checker (name lo hi) (checker (rsym name) (lambda (x) (typep x `(integer ,lo ,hi)))))
(defun flonum-checker (name) (checker (rsym name) (lambda (x) (typep x 'double-float))))

(defctype *_bool* "_bool" :boolean "bool")
(defctype *_double* "_double" :double "double" :s->c (flonum-checker "_double"))
(defctype *_fixnum* "_fixnum" :fixnum "fixnum" :s->c (checker (rsym "_fixnum") #'fixnum-p))
(defctype *_float* "_float" :float "float" :s->c (flonum-checker "_float"))
(defctype *_int8* "_int8" :integer-8 "int8" :s->c (int-checker "_int8" -128 127))
(defctype *_int16* "_int16" :integer-16 "int16" :s->c (int-checker "_int16" -32768 32767))
(defctype *_int32* "_int32" :integer-32 "int32" :s->c (int-checker "_int32" (- (expt 2 31)) (1- (expt 2 31))))
(defctype *_int64* "_int64" :integer-64 "int64" :s->c (int-checker "_int64" (- (expt 2 63)) (1- (expt 2 63))))
(defctype *_uint8* "_uint8" :unsigned-8 "uint8" :s->c (int-checker "_uint8" 0 255))
(defctype *_uint16* "_uint16" :unsigned-16 "uint16" :s->c (int-checker "_uint16" 0 65535))
(defctype *_uint32* "_uint32" :unsigned-32 "uint32" :s->c (int-checker "_uint32" 0 (1- (expt 2 32))))
(defctype *_uint64* "_uint64" :unsigned-64 "uint64" :s->c (int-checker "_uint64" 0 (1- (expt 2 64))))
(defctype *_double** "_double*" :double "double"
  :s->c (lambda (for-whom x)
	  (if (realp x)
	      (coerce x 'double-float)
	      (bad-ctype-value for-whom (rsym "_double*") x))))
(defctype *_ufixnum* "_ufixnum" :fixnum "fixnum" :s->c (checker (rsym "_ufixnum") #'fixnum-p))
(defctype *_fixint* "_fixint" :integer-32 "fixint" :s->c (checker (rsym "_fixint") #'fixnum-p))
(defctype *_ufixint* "_ufixint" :unsigned-32 "ufixint" :s->c (checker (rsym "_ufixint") #'fixnum-p))
(defctype *_longdouble* "_longdouble" :double "double"
  :s->c (lambda (for-whom x) (bad-ctype-value for-whom (rsym "_longdouble") x)))
(defctype *_stdbool* "_stdbool" :integer-8 "stdbool"
  :c->s (lambda (c) (bool (not (zerop c))))
  :s->c (lambda (for-whom v) (declare (ignore for-whom)) (if (false-p v) 0 1)))

(defctype *_pointer* "_pointer" :ftype-pointer "pointer"
  :c->s #'fptr->cptr :s->c (lambda (w s) (cptr->fptr w s)))
(defctype *_gcpointer* "_gcpointer" :ftype-scheme-object-pointer "pointer"
  :c->s #'fptr->cptr :s->c (lambda (w s) (cptr->fptr w s)))
(defctype *_fpointer* "_fpointer" :ftype-pointer "fpointer"
  :c->s #'fptr->cptr :s->c (lambda (w s) (cptr->fptr w s))
  :ref (lambda (c offset)
	 (if (and (ffi-obj-p c) (eql offset 0))
	     c
	     (fptr->cptr (read-rep :ftype-pointer (cptr->fptr (rsym "_fpointer") c) offset)))))

(defctype *_scheme* "_scheme" :scheme-object "scheme" :nonatomic t)

(defvar *_void*
  (make-fctype :host-rep :void :basetype (rsym "void")
	       :c->scheme #'identity-c->scheme :scheme->c #'identity-scheme->c
	       :c->s #'identity-c->scheme :s->c #'identity-s->c
	       :ref (lambda (c offset) (declare (ignore c offset))
		      (arguments-error "_void" "cannnot dereference void"))
	       :set (lambda (w d o s) (declare (ignore w d o s))
		      (arguments-error "_void" "cannnot set void content"))))
(define-primitive "#%foreign" "_void" *_void*)

(defun octets-cp (octets) (make-cp octets 0))

(defctype *_bytes* "_bytes" :ftype-pointer "bytes" :nonatomic t
  :c->s (lambda (m) (or (nul-octets m 1) qs:false))
  :s->c (lambda (for-whom x)
	  (cond ((false-p x) (make-cp nil 0))
		((octets-p x) (octets-cp x))
		(t (bad-ctype-value (rsym "_bytes") for-whom x)))))

(defun encode-with-nul (x encoding)
  (sb-ext:string-to-octets (concatenate 'string x (string (code-char 0))) :external-format encoding))

(defctype *_string/utf-16* "_string/utf-16" :ftype-pointer "string/utf-16" :nonatomic t
  :c->s (lambda (m)
	  (let ((o (nul-octets m 2)))
	    (if o (sb-ext:octets-to-string o :external-format :utf-16le) qs:false)))
  :s->c (lambda (for-whom x)
	  (cond ((false-p x) (make-cp nil 0))
		((stringp x) (octets-cp (encode-with-nul x :utf-16le)))
		(t (bad-ctype-value (rsym "_string/utf-16") for-whom x)))))
(defctype *_string/ucs-4* "_string/ucs-4" :ftype-pointer "string/ucs-4" :nonatomic t
  :c->s (lambda (m)
	  (let ((o (nul-octets m 4)))
	    (if o (sb-ext:octets-to-string o :external-format :utf-32le) qs:false)))
  :s->c (lambda (for-whom x)
	  (cond ((false-p x) (make-cp nil 0))
		((stringp x) (octets-cp (encode-with-nul x :utf-32le)))
		(t (bad-ctype-value (rsym "_string/ucs-4") for-whom x)))))
(defctype *_symbol* "_symbol" :ftype-pointer "string" :nonatomic t
  :c->s (lambda (m)
	  (let ((o (nul-octets m 1)))
	    (unless o (arg-error "utf8->string" "bytes?" qs:false))
	    (rsym (utf8 o))))
  :s->c (lambda (for-whom x)
	  (if (racket-symbol-p x)
	      (octets-cp (sb-ext:string-to-octets (concatenate 'string (rname x) (string (code-char 0)))
						  :external-format :utf-8))
	      (bad-ctype-value for-whom (rsym "_symbol") x))))

;;; ------------------------------------------------------------------
;;; make-ctype

(defvar *the-void* nil)

(defun accepts-1-or-false-p (f) (or (false-p f) (accepts-arity-p f 1)))

(defforeign "make-ctype" (type racket-to-c c-to-racket) ()
  (check-ctype "make-ctype" type)
  (unless (accepts-1-or-false-p racket-to-c)
    (arg-error "make-ctype" "(or/c (procedure-arity-includes/c 1) #f)" racket-to-c))
  (unless (accepts-1-or-false-p c-to-racket)
    (arg-error "make-ctype" "(or/c (procedure-arity-includes/c 1) #f)" c-to-racket))
  (let* ((r->c (and (not (false-p racket-to-c)) racket-to-c))
	 (c->r (and (not (false-p c-to-racket)) c-to-racket))
	 (c->s (fct-c->s type))
	 (s->c (fct-s->c type))
	 (ref (fct-ref type))
	 (set (fct-set type))
	 (new (copy-fctype type)))
    (setf (fct-basetype new) type
	  (fct-c->scheme new) (or c->r qs:false)
	  (fct-scheme->c new) (or r->c qs:false)
	  (fct-c->s new) (if c->r (lambda (p) (funcall c->r (funcall c->s p))) c->s)
	  (fct-s->c new) (if r->c (lambda (who v) (funcall s->c who (funcall r->c v))) s->c)
	  (fct-ref new) (if c->r (lambda (p offset) (funcall c->r (funcall ref p offset))) ref)
	  (fct-set new) (if r->c (lambda (who p offset v) (funcall set who p offset (funcall r->c v))) set))
    new))

(defforeign "ctype?" (x) (:leaf t) (bool (fctype-p x)))
(defforeign "ctype-basetype" (c) (:leaf t) (check-ctype "ctype-basetype" c) (fct-basetype c))
(defforeign "ctype-c->scheme" (c) (:leaf t) (check-ctype "ctype-c->scheme" c) (fct-c->scheme c))
(defforeign "ctype-scheme->c" (c) (:leaf t) (check-ctype "ctype-scheme->c" c) (fct-scheme->c c))

(defun fct-sizeof (c)
  (if (fct-compound c) (fct-size c) (rep-size (fct-host-rep c))))

(defun fct-alignof (c)
  (if (fct-compound c)
      (fct-alignment c)
      (ecase (fct-host-rep c)
	((:boolean :float :integer-32 :unsigned-32) 4)
	((:integer-8 :unsigned-8) 1)
	((:integer-16 :unsigned-16) 2)
	((:double :integer-64 :unsigned-64 :void :fixnum :ftype-pointer
	  :ftype-scheme-object-pointer :scheme-object)
	 8))))

(defforeign "ctype-sizeof" (c) (:leaf t) (check-ctype "ctype-sizeof" c) (fct-sizeof c))
(defforeign "ctype-alignof" (c) (:leaf t) (check-ctype "ctype-alignof" c) (fct-alignof c))

(defun host-rep-name (rep) (string-downcase (symbol-name rep)))

(defforeign "assert-ctype-representation" (c1 c2) (:leaf t)
  (unless (and (fctype-p c1) (fctype-p c2) (eq (fct-host-rep c1) (fct-host-rep c2)))
    (racket-error "assert-ctype-representation" "mismatch between ~A vs. ~A"
		  (if (fctype-p c1) (host-rep-name (fct-host-rep c1)) "#f")
		  (if (fctype-p c2) (host-rep-name (fct-host-rep c2)) "#f")))
  c2)

(defun memory-ctype-p (c) (declare (ignore c)) t)

;;; Compound types

(defun struct-layout (types alignment)
  (flet ((align (size algn)
	   (let ((amt (mod size (or alignment algn))))
	     (if (zerop amt) size (+ size (- algn amt))))))
    (let ((size 0) (max-align 1))
      (dolist (ty types)
	(let ((sz (fct-sizeof ty)) (algn (fct-alignof ty)))
	  (setq size (+ (align size algn) sz)
		max-align (max algn max-align))))
      (values (align size max-align) max-align))))

(defun make-compound (rep basetype size alignment malloc-mode)
  (make-fctype
   :host-rep rep :basetype basetype :compound t
   :c->scheme #'identity-c->scheme :scheme->c #'identity-scheme->c
   :c->s #'fptr->cptr
   :s->c (lambda (for-whom s) (cptr->fptr for-whom s))
   :ref (lambda (c offset) (do-ptr-add c offset nil))
   :set (lambda (for-whom dest offset s)
	  (copy-memory for-whom (cptr->fptr for-whom dest) offset (cptr->fptr for-whom s) 0 size))
   :size size :alignment alignment :malloc-mode malloc-mode))

(defforeign "make-cstruct-type" (types &optional (abi qs:false) (alignment qs:false) (malloc-mode nil mode-p))
    (:leaf t)
  (declare (ignore abi))
  (unless (and (listp types) (every #'fctype-p types))
    (arg-error "make-cstruct-type" "(listof ctype?)" types))
  (multiple-value-bind (size align)
      (struct-layout types (and (not (false-p alignment)) alignment))
    (let ((c (make-compound :struct types size align
			    (if mode-p malloc-mode (rsym "atomic")))))
      (setf (fct-members c) types)
      c)))

(defforeign "make-union-type" (&rest types) (:leaf t)
  (dolist (type types) (check-ctype "make-union-type" type))
  (let ((c (make-compound :union types
			  (reduce #'max (mapcar #'fct-sizeof types) :initial-value 0)
			  (reduce #'max (mapcar #'fct-alignof types) :initial-value 1)
			  (rsym "atomic"))))
    (setf (fct-members c) types)
    c))

(defforeign "make-array-type" (type count) (:leaf t)
  (check-ctype "make-array-type" type)
  (unless (exact-nonneg-p count) (arg-error "make-array-type" "exact-nonnegative-integer?" count))
  (let ((size (* count (fct-sizeof type))))
    (unless (fixnum-p size)
      (arguments-error "make-array-type" "arithmetic overflow for overlarge array type" "size" size))
    (let ((c (make-compound :array (vector type count) size (fct-alignof type) qs:false)))
      (setf (fct-members c) (list type))
      c)))

(defforeign "compiler-sizeof" (sl) (:leaf t)
  (unless (or (racket-symbol-p sl) (listp sl))
    (arg-error "compiler-sizeof" "(or/c ctype-symbol? (listof ctype-symbol?))" sl))
  (flet ((size-of (name)
	   (cond ((string= name "short") 2) ((string= name "long") 8) ((string= name "long-long") 8)
		 ((string= name "int") 4) ((string= name "char") 1) ((string= name "wchar") 4)
		 ((string= name "float") 4) ((string= name "double") 8) (t 8)))
	 (fail (what) (arguments-error "compiler-sizeof" what)))
    (let ((base nil) (star nil) (size nil))
      (loop
	(when (null sl)
	  (return))
	(let ((s (if (consp sl) (car sl) sl)))
	  (unless (racket-symbol-p s)
	    (arg-error "compiler-sizeof" "(or/c ctype-symbol? (listof ctype-symbol?))" sl))
	  (let ((n (rname s)))
	    (cond ((member n '("int" "char" "wchar" "float" "double" "void") :test #'string=)
		   (when base (fail (format nil "extraneous type: '~A" n)))
		   (setq base n))
		  ((string= n "short")
		   (cond ((eq size :short) (fail "cannot handle more than one 'short"))
			 ((member size '(:long :long-long)) (fail "cannot use both 'short and 'long"))
			 (t (setq size :short))))
		  ((string= n "long")
		   (case size
		     (:short (fail "cannot use both 'short and 'long"))
		     (:long-long (fail "cannot handle more than two 'long"))
		     (:long (setq size :long-long))
		     (t (setq size :long))))
		  ((string= n "*")
		   (when star (fail "cannot handle more than one '*"))
		   (setq star t))
		  (t (arg-error "compiler-sizeof" "(or/c ctype-symbol? (listof ctype-symbol?))" sl))))
	  (setq sl (if (consp sl) (cdr sl) '()))))
      (let ((size-name (case size (:short "short") (:long "long") (:long-long "long-long") (t nil))))
	(cond ((equal base "void")
	       (when size (fail "cannot qualify 'void"))
	       (if star 8 (fail "cannot use 'void without a '*")))
	      ((or (not size) (equal base "int") (not base))
	       (if star 8 (size-of (or size-name base "int"))))
	      ((equal base "double")
	       (cond ((eq size :long) (if star 8 8))
		     (t (fail "bad qualifiers for 'double"))))
	      ((equal base "float") (fail "bad qualifiers for 'float"))
	      (t (fail (format nil "cannot qualify '~A" base))))))))

;;; ------------------------------------------------------------------
;;; Cpointer operations

(defforeign "cpointer?" (x) (:leaf t) (bool (cpointer-p x)))

(defforeign "cpointer-gcable?" (p) (:leaf t)
  (bool (cp-gcable-p (cptr->fptr "cpointer-gcable?" p))))

(defforeign "cpointer-tag" (p) (:leaf t)
  (cond ((cp-p p) (cp-tags p))
	((cpointer-p p) (let ((q (extract-cp p))) (if q (cp-tags q) qs:false)))
	(t (arg-error "cpointer-tag" "cpointer?" p))))

(defforeign "set-cpointer-tag!" (p tag) (:leaf t)
  (cond ((cp-p p) (setf (cp-tags p) tag) *void*)
	((cpointer-p p)
	 (let ((q (extract-cp p)))
	   (if q
	       (progn (setf (cp-tags q) tag) *void*)
	       (arguments-error "set-cpointer-tag!" "cannot set tag on given cpointer" "given" p "tag" tag))))
	(t (arg-error "set-cpointer-tag!" "cpointer?" p))))

(defforeign "offset-ptr?" (p) (:leaf t)
  (unless (cpointer-p p) (arg-error "offset-ptr?" "cpointer?" p))
  (bool (and (cp-p p) (cp-origin p))))

(defforeign "ptr-equal?" (a b) (:leaf t)
  (bool (ptr-equal-p (cptr->fptr "cpointer-accessor" a) (cptr->fptr "cpointer-accessor" b))))

(defforeign "ptr-offset" (p) (:leaf t)
  (cond ((and (cp-p p) (cp-origin p)) (- (cp-offset p) (cp-origin p)))
	((cpointer-p p)
	 (let ((q (extract-cp p)))
	   (if (and q (cp-origin q)) (- (cp-offset q) (cp-origin q)) 0)))
	(t (arg-error "ptr-offset" "cpointer?" p))))

(defforeign "set-ptr-offset!" (p n) (:leaf t)
  (unless (and (cp-p p) (cp-origin p))
    (arg-error "set-ptr-offset!" "(and/c cpointer? ptr-offset?)" p))
  (unless (integerp n) (arg-error "set-ptr-offset!" "exact-integer?" n))
  (setf (cp-offset p) (+ (cp-origin p) n))
  *void*)

(defforeign "ptr-add" (p n &optional (type nil type-p)) (:leaf t)
  (unless (cpointer-p p) (arg-error "ptr-add" "cpointer?" p))
  (unless (integerp n) (arg-error "ptr-add" "exact-integer?" n))
  (when type-p
    (unless (fctype-p type) (arg-error "ptr-add" "ctype?" type)))
  (do-ptr-add p (if type-p (* n (fct-sizeof type)) n) t))

(defforeign "ptr-add!" (p n &optional (type nil type-p)) (:leaf t)
  (unless (and (cp-p p) (cp-origin p))
    (arg-error "ptr-add!" "(and/c cpointer? offset-ptr?)" p))
  (unless (integerp n) (arg-error "ptr-add!" "exact-integer?" n))
  (when type-p
    (unless (fctype-p type) (arg-error "ptr-add!" "ctype?" type)))
  (incf (cp-offset p) (if type-p (* n (fct-sizeof type)) n))
  *void*)

(defforeign "ptr-ref" (p type &optional (a nil a-p) (b nil b-p)) ()
  ;; Racket's message names the accessor that unwraps the pointer
  (unless (cpointer-p p) (arg-error "cpointer-accessor" "cpointer?" p))
  (check-ctype "ptr-ref" type)
  (cond (b-p
	 (unless (symbol-named-p a "abs") (arg-error "ptr-ref" "'abs" a))
	 (unless (fixnum-p b) (arg-error "ptr-ref" "fixnum?" b))
	 (funcall (fct-ref type) p b))
	(a-p
	 (unless (fixnum-p a) (arg-error "ptr-ref" "fixnum?" a))
	 (funcall (fct-ref type) p (* (fct-sizeof type) a)))
	(t (funcall (fct-ref type) p 0))))

(defforeign "ptr-set!" (p type a &optional (b nil b-p) (c nil c-p)) ()
  (unless (cpointer-p p) (arg-error "cpointer-accessor" "cpointer?" p))
  (check-ctype "ptr-set!" type)
  (let ((who (rsym "ptr-set!")))
    (cond (c-p
	   (unless (symbol-named-p a "abs") (arg-error "ptr-set!" "'abs" a))
	   (unless (fixnum-p b) (arg-error "ptr-set!" "fixnum?" b))
	   (funcall (fct-set type) who p b c))
	  (b-p
	   (unless (fixnum-p a) (arg-error "ptr-set!" "fixnum?" a))
	   (funcall (fct-set type) who p (* (fct-sizeof type) a) b))
	  (t (funcall (fct-set type) who p 0 a)))
    *void*))

(defforeign "ffi2-ptr?" (v) (:leaf t) (bool (cp-p v)))
(defforeign "ffi2-ptr/gcable?" (v) (:leaf t) (bool (and (cp-p v) (cp-gcable-p v))))
(defforeign "cpointer->ffi2-ptr" (who p) (:leaf t) (cptr->fptr who p))
(defforeign "ffi2-ptr->cpointer" (p) (:leaf t) (fptr->cptr p))
(defforeign "ffi2-ptr->uintptr" (p) (:leaf t)
  (when (cp-gcable-p p) (racket-error "ffi2-ptr->uintptr" "cannot take the address of a GCable pointer"))
  (cp-address p))
(defforeign "ffi2-uintptr->ptr" (i) (:leaf t) (make-cp nil i))

;;; ------------------------------------------------------------------
;;; Memory

(defun copy-memory (who to to-off from from-off len)
  (when (plusp len)
    (call-with-cp-sap who to to-off len
      (lambda (dst)
	(call-with-cp-sap who from from-off len
	  (lambda (src)
	    (cffi:foreign-funcall "memmove" :pointer dst :pointer src :unsigned-long len :pointer))))))
  *void*)

(defun set-memory (who to to-off byte len)
  (when (plusp len)
    (call-with-cp-sap who to to-off len
      (lambda (dst)
	(cffi:foreign-funcall "memset" :pointer dst :int byte :unsigned-long len :pointer))))
  *void*)

(defun check-cp (who x) (unless (cpointer-p x) (arg-error who "cpointer?" x)) x)
(defun check-exact-int (who x) (unless (integerp x) (arg-error who "exact-integer?" x)) x)
(defun check-nonneg-int (who x)
  (unless (exact-nonneg-p x) (arg-error who "exact-nonnegative-integer?" x)) x)
(defun check-byte (who x) (unless (typep x '(integer 0 255)) (arg-error who "byte?" x)) x)

(defun memcpy-like (who to args)
  "memcpy's and memmove's argument forms after the destination."
  (check-cp who to)
  (let ((dst (cptr->fptr who to)) (n (length args)))
    (flet ((src (x) (cptr->fptr who x))
	   (size (type) (fct-sizeof type)))
      (ecase n
	(2 (destructuring-bind (s count) args
	     (check-cp who s) (check-nonneg-int who count)
	     (copy-memory who dst 0 (src s) 0 count)))
	(3 (destructuring-bind (x y z) args
	     (cond ((cpointer-p x)
		    (cond ((fctype-p z)
			   (check-nonneg-int who y)
			   (copy-memory who dst 0 (src x) 0 (* y (size z))))
			  (t (check-exact-int who y) (check-nonneg-int who z)
			     (copy-memory who dst 0 (src x) y z))))
		   (t (check-exact-int who x) (check-cp who y) (check-nonneg-int who z)
		      (copy-memory who dst x (src y) 0 z)))))
	(4 (destructuring-bind (offset s a b) args
	     (check-exact-int who offset) (check-cp who s)
	     (cond ((fctype-p b)
		    (check-nonneg-int who a)
		    (let ((sz (size b)))
		      (copy-memory who dst (* sz offset) (src s) 0 (* a sz))))
		   (t (check-exact-int who a) (check-nonneg-int who b)
		      (copy-memory who dst offset (src s) a b)))))
	(5 (destructuring-bind (offset s src-offset count type) args
	     (check-exact-int who offset) (check-cp who s) (check-exact-int who src-offset)
	     (unless (fctype-p type) (arg-error who "ctype?" type))
	     (let ((sz (size type)))
	       (copy-memory who dst (* offset sz) (src s) (* src-offset sz) (* count sz)))))))))

(defforeign "memcpy" (to a b &optional (c nil c-p) (d nil d-p) (e nil e-p)) (:leaf t)
  (memcpy-like "memcpy" to (append (list a b) (and c-p (list c)) (and d-p (list d)) (and e-p (list e)))))
(defforeign "memmove" (to a b &optional (c nil c-p) (d nil d-p) (e nil e-p)) (:leaf t)
  (memcpy-like "memmove" to (append (list a b) (and c-p (list c)) (and d-p (list d)) (and e-p (list e)))))

(defforeign "memset" (cptr a b &optional (c nil c-p) (d nil d-p)) (:leaf t)
  (check-cp "memset" cptr)
  (let ((m (cptr->fptr "memset" cptr)))
    (cond (d-p
	   (check-exact-int "memset" a) (check-byte "memset" b) (check-nonneg-int "memset" c)
	   (unless (fctype-p d) (arg-error "memset" "ctype?" d))
	   (set-memory "memset" m (* a (fct-sizeof d)) b (* c (fct-sizeof d))))
	  (c-p
	   (cond ((fctype-p c)
		  (check-byte "memset" a) (check-nonneg-int "memset" b)
		  (set-memory "memset" m 0 a (* b (fct-sizeof c))))
		 (t (check-exact-int "memset" a) (check-byte "memset" b) (check-nonneg-int "memset" c)
		    (set-memory "memset" m a b c))))
	  (t (check-byte "memset" a) (check-nonneg-int "memset" b)
	     (set-memory "memset" m 0 a b)))))

(defforeign "ffi2-memcpy" (to to-offset from from-offset len) (:leaf t)
  (copy-memory "ffi2-memcpy" to to-offset from from-offset len))
(defforeign "ffi2-memmove" (to to-offset from from-offset len) (:leaf t)
  (copy-memory "ffi2-memmove" to to-offset from from-offset len))
(defforeign "ffi2-memset" (to to-offset byte len) (:leaf t)
  (set-memory "ffi2-memset" to to-offset byte len))

(defun malloc-mode-name (x)
  (and (racket-symbol-p x)
       (find (rname x) '("raw" "atomic" "nonatomic" "tagged" "atomic-interior" "interior"
			 "zeroed-atomic" "zeroed-atomic-interior" "stubborn" "uncollectable" "eternal")
	     :test #'string=)))

(defun ctype-malloc-mode (c) (if (fct-nonatomic c) "nonatomic" "atomic"))

(defun malloc-interior (size zero)
  (let* ((address (qsffi:allocate size))
	 (keeper (list address)))
    (when zero
      (cffi:foreign-funcall "memset" :pointer (sb-sys:int-sap address) :int 0
			    :unsigned-long size :pointer))
    (sb-ext:finalize keeper (lambda () (qsffi:free address)) :dont-save t)
    (make-cp nil address qs:false keeper)))

(defun normalized-malloc (size mode)
  (unless (and (fixnum-p size) (< size (expt 2 40)))
    (raise-exn "exn:fail:out-of-memory"
	       (format nil "malloc: out of memory~%  allocation size: ~D" size)))
  (cond ((eql size 0) qs:false)
	((string= mode "raw") (make-cp nil (qsffi:allocate size)))
	((member mode '("atomic" "nonatomic" "zeroed-atomic") :test #'string=)
	 (make-cp (make-octets size) 0))
	((member mode '("atomic-interior" "interior") :test #'string=) (malloc-interior size nil))
	((string= mode "zeroed-atomic-interior") (malloc-interior size t))
	(t (unsupported "malloc" (format nil "'~A mode is not supported" mode)))))

(defun malloc-arguments (args)
  (flet ((duplicate (what a b)
	   (arguments-error "malloc" (format nil "multiple ~A arguments" what) "first" a "second" b)))
    (let ((count nil) (type nil) (copy-from nil) (mode nil) (fail-mode nil))
      (dolist (a args)
	(cond ((nonneg-fixnum-p a) (if count (duplicate "size" count a) (setq count a)))
	      ((fctype-p a) (if type (duplicate "type" type a) (setq type a)))
	      ((and (cpointer-p a) (not (false-p a)))
	       (if copy-from (duplicate "source for copy" copy-from a) (setq copy-from a)))
	      ((malloc-mode-name a) (if mode (duplicate "mode" mode a) (setq mode a)))
	      ((or (symbol-named-p a "failok") (symbol-named-p a "fail-ok"))
	       (if fail-mode (duplicate "failure mode" fail-mode a) (setq fail-mode a)))
	      (t (arg-error "malloc"
			    (format nil "(or/c (and/c exact-nonnegative-integer? fixnum?)~%      ctype? cpointer?~%      (or/c 'raw 'atomic 'nonatomic 'tagged~%            'atomic-interior 'interior~%            'zeroed-atomic 'zeroed-atomic-interior~%            'stubborn 'uncollectable 'eternal)~%      'fail-ok)")
			    a))))
      (unless (or count type) (arguments-error "malloc" "no size given"))
      (let* ((len (* (or count 1) (if type (fct-sizeof type) 1)))
	     (p (normalized-malloc len (cond (mode (rname mode))
					      (type (ctype-malloc-mode type))
					      (t "atomic")))))
	(when copy-from
	  (copy-memory "malloc" (cptr->fptr "malloc" p) 0 (cptr->fptr "malloc" copy-from) 0 len))
	p))))

(defforeign "malloc" (&rest args) (:leaf t)
  (when (or (null args) (> (length args) 5))
    (racket-arity-error "malloc" (length args)))
  (malloc-arguments args))

(defforeign "free" (p) (:leaf t)
  (let ((m (cptr->fptr "cpointer-accessor" p)))
    (when (cp-gcable-p m) (racket-error "free" "cannot free GCable memory"))
    (qsffi:free (cp-address m))
    *void*))
(defforeign "ffi2-free" (m) (:leaf t) (qsffi:free (cp-address m)) *void*)

(defforeign "malloc-immobile-cell" (v) (:leaf t)
  (let* ((address (qsffi:allocate 8))
	 (handle (scheme-handle v)))
    (setf (gethash handle *cells*) v)
    (setf (sb-sys:sap-ref-64 (sb-sys:int-sap address) 0) handle)
    (make-cp nil address)))

(defforeign "free-immobile-cell" (b) (:leaf t)
  (let* ((m (cptr->fptr "free-immobile-cell" b))
	 (handle (call-with-cp-sap "free-immobile-cell" m 0 8 (lambda (sap) (sb-sys:sap-ref-64 sap 0)))))
    (remhash handle *cells*)
    (unless (cp-base m) (qsffi:free (cp-address m)))
    *void*))

(defforeign "end-stubborn-change" (p) (:leaf t) (declare (ignore p)) (unsupported "end-stubborn-change"))
(defforeign "extflvector->cpointer" (v) (:leaf t) (declare (ignore v)) (unsupported "extflvector->cpointer"))
(defforeign "vector->cpointer" (v) (:leaf t) (declare (ignore v)) (unsupported "vector->cpointer"))
(defforeign "flvector->cpointer" (v) (:leaf t)
  (unless (typep v '(simple-array double-float (*))) (arg-error "flvector->cpointer" "flvector?" v))
  (make-cp v 0))
(defforeign "make-sized-byte-string" (cptr len) (:leaf t)
  (check-cp "make-sized-byte-string" cptr)
  (check-nonneg-int "make-sized-byte-string" len)
  (unsupported "make-sized-byte-string"))

(defforeign "make-late-weak-box" (b) (:leaf t) (sb-ext:make-weak-pointer b))
(defforeign "make-late-weak-hasheq" () (:leaf t) (rcall "make-weak-hasheq"))
(defforeign "make-late-will-executor" (&rest args) (:leaf t)
  (declare (ignore args))
  (if (gethash "make-will-executor" *natives*)
      (rcall "make-will-executor")
      (unsupported "make-late-will-executor")))

;;; ------------------------------------------------------------------
;;; errno

(defvar *saved-errno* 0)

(defforeign "saved-errno" (&optional (v nil v-p)) (:leaf t)
  (cond (v-p (check-exact-int "saved-errno" v) (setq *saved-errno* v) *void*)
	(t *saved-errno*)))

(defparameter *linux-errnos*
  '(("E2BIG" . 7) ("EACCES" . 13) ("EADDRINUSE" . 98) ("EADDRNOTAVAIL" . 99)
    ("EAFNOSUPPORT" . 97) ("EAGAIN" . 11) ("EALREADY" . 114) ("EBADF" . 9)
    ("EBADMSG" . 74) ("EBUSY" . 16) ("ECANCELED" . 125) ("ECHILD" . 10)
    ("ECONNABORTED" . 103) ("ECONNREFUSED" . 111) ("ECONNRESET" . 104) ("EDEADLK" . 35)
    ("EDESTADDRREQ" . 89) ("EDOM" . 33) ("EDQUOT" . 122) ("EEXIST" . 17) ("EFAULT" . 14)
    ("EFBIG" . 27) ("EHOSTUNREACH" . 113) ("EIDRM" . 43) ("EILSEQ" . 84)
    ("EINPROGRESS" . 115) ("EINTR" . 4) ("EINVAL" . 22) ("EIO" . 5) ("EISCONN" . 106)
    ("EISDIR" . 21) ("ELOOP" . 40) ("EMFILE" . 24) ("EMLINK" . 31) ("EMSGSIZE" . 90)
    ("EMULTIHOP" . 72) ("ENAMETOOLONG" . 36) ("ENETDOWN" . 100) ("ENETRESET" . 102)
    ("ENETUNREACH" . 101) ("ENFILE" . 23) ("ENOBUFS" . 105) ("ENODATA" . 61)
    ("ENODEV" . 19) ("ENOENT" . 2) ("ENOEXEC" . 8) ("ENOLCK" . 37) ("ENOLINK" . 67)
    ("ENOMEM" . 12) ("ENOMSG" . 42) ("ENOPROTOOPT" . 92) ("ENOSPC" . 28) ("ENOSR" . 63)
    ("ENOSTR" . 60) ("ENOSYS" . 38) ("ENOTCONN" . 107) ("ENOTDIR" . 20)
    ("ENOTEMPTY" . 39) ("ENOTRECOVERABLE" . 131) ("ENOTSOCK" . 88) ("ENOTSUP" . 95)
    ("ENOTTY" . 25) ("ENXIO" . 6) ("EOPNOTSUPP" . 95) ("EOVERFLOW" . 75)
    ("EOWNERDEAD" . 130) ("EPERM" . 1) ("EPIPE" . 32) ("EPROTO" . 71)
    ("EPROTONOSUPPORT" . 93) ("EPROTOTYPE" . 91) ("ERANGE" . 34) ("EROFS" . 30)
    ("ESPIPE" . 29) ("ESRCH" . 3) ("ESTALE" . 116) ("ETIME" . 62) ("ETIMEDOUT" . 110)
    ("ETXTBSY" . 26) ("EWOULDBLOCK" . 11) ("EXDEV" . 18)))

(defparameter *macos-errnos*
  '(("E2BIG" . 7) ("EACCES" . 13) ("EADDRINUSE" . 48) ("EADDRNOTAVAIL" . 49)
    ("EAFNOSUPPORT" . 47) ("EAGAIN" . 35) ("EALREADY" . 37) ("EBADF" . 9)
    ("EBADMSG" . 94) ("EBUSY" . 16) ("ECANCELED" . 89) ("ECHILD" . 10)
    ("ECONNABORTED" . 53) ("ECONNREFUSED" . 61) ("ECONNRESET" . 54) ("EDEADLK" . 11)
    ("EDESTADDRREQ" . 39) ("EDOM" . 33) ("EDQUOT" . 69) ("EEXIST" . 17) ("EFAULT" . 14)
    ("EFBIG" . 27) ("EHOSTUNREACH" . 65) ("EIDRM" . 90) ("EILSEQ" . 92)
    ("EINPROGRESS" . 36) ("EINTR" . 4) ("EINVAL" . 22) ("EIO" . 5) ("EISCONN" . 56)
    ("EISDIR" . 21) ("ELOOP" . 62) ("EMFILE" . 24) ("EMLINK" . 31) ("EMSGSIZE" . 40)
    ("EMULTIHOP" . 95) ("ENAMETOOLONG" . 63) ("ENETDOWN" . 50) ("ENETRESET" . 52)
    ("ENETUNREACH" . 51) ("ENFILE" . 23) ("ENOBUFS" . 55) ("ENODATA" . 96)
    ("ENODEV" . 19) ("ENOENT" . 2) ("ENOEXEC" . 8) ("ENOLCK" . 77) ("ENOLINK" . 97)
    ("ENOMEM" . 12) ("ENOMSG" . 91) ("ENOPROTOOPT" . 42) ("ENOSPC" . 28) ("ENOSR" . 98)
    ("ENOSTR" . 99) ("ENOSYS" . 78) ("ENOTCONN" . 57) ("ENOTDIR" . 20)
    ("ENOTEMPTY" . 66) ("ENOTRECOVERABLE" . 104) ("ENOTSOCK" . 38) ("ENOTSUP" . 45)
    ("ENOTTY" . 25) ("ENXIO" . 6) ("EOPNOTSUPP" . 102) ("EOVERFLOW" . 84)
    ("EOWNERDEAD" . 105) ("EPERM" . 1) ("EPIPE" . 32) ("EPROTO" . 100)
    ("EPROTONOSUPPORT" . 43) ("EPROTOTYPE" . 41) ("ERANGE" . 34) ("EROFS" . 30)
    ("ESPIPE" . 29) ("ESRCH" . 3) ("ESTALE" . 70) ("ETIME" . 101) ("ETIMEDOUT" . 60)
    ("ETXTBSY" . 26) ("EWOULDBLOCK" . 35) ("EXDEV" . 18)))

(defforeign "lookup-errno" (sym) (:leaf t)
  (unless (racket-symbol-p sym) (arg-error "lookup-errno" "symbol?" sym))
  (let ((entry (assoc (rname sym) #+darwin *macos-errnos* #-darwin *linux-errnos* :test #'string=)))
    (if entry (cdr entry) qs:false)))

;;; ------------------------------------------------------------------
;;; Libraries

(defstruct (flib (:constructor make-flib (handle name))) handle name)
(defmethod print-object ((x flib) stream) (format stream "#<ffi-lib>"))

(defun path-or-string (who x)
  (cond ((stringp x) x)
	((rpath-p x) (rpath-string x))
	(t (arg-error who "(or/c path-string? #f)" x))))

(defun raise-dll-error (who message system-error)
  (raise-exn "exn:fail:filesystem"
	     (format nil "~A: ~A~@[~%  system error: ~A~]" who message system-error)))

;; the fourth argument, as rumble's ffi-lib* takes: who is asking
(defforeign "ffi-lib" (name &optional (fail-as-false qs:false) (as-global qs:false) who) (:leaf t)
  (declare (ignore who))
  (let ((path (if (false-p name) nil (path-or-string "ffi-lib" name))))
    (multiple-value-bind (handle err)
	(qsffi:dlopen path (logior qsffi:+rtld-now+
				   (if (truthy as-global) qsffi:+rtld-global+ qsffi:+rtld-local+)))
      (cond (handle (make-flib handle (if (stringp name) (immutable-copy name) name)))
	    ((truthy fail-as-false) qs:false)
	    (t (raise-dll-error "ffi-lib"
				(format nil "could not load foreign library~%  path: ~A"
					(or path "[all opened]"))
				err))))))

(defforeign "ffi-lib?" (x) (:leaf t) (bool (flib-p x)))
(defforeign "ffi-lib-name" (lib) (:leaf t)
  (unless (flib-p lib) (arg-error "ffi-lib-name" "ffi-lib?" lib))
  (flib-name lib))
(defforeign "ffi-lib-unload" (lib) (:leaf t)
  (unless (flib-p lib) (arg-error "ffi-lib-unload" "ffi-lib?" lib))
  (cffi:foreign-funcall "dlclose" :pointer (sb-sys:int-sap (flib-handle lib)) :int)
  *void*)

(defun lookup-object (who name lib)
  "The address of NAME in LIB, or an exception."
  (unless (octets-p name) (arg-error who "bytes?" name))
  (unless (flib-p lib) (arg-error who "ffi-lib?" lib))
  (multiple-value-bind (address err) (qsffi:dlsym (flib-handle lib) (utf8 name))
    (or address
	(raise-dll-error who
			 (format nil "could not find export from foreign library~%  name: ~A~%  library: ~A"
				 (utf8 name)
				 (let ((n (flib-name lib)))
				   (cond ((stringp n) n) ((rpath-p n) (rpath-string n)) (t "[all opened]"))))
			 err))))

(defforeign "ffi-obj" (name lib) (:leaf t)
  (let ((address (lookup-object "ffi-obj" name lib)))
    (make-ffi-obj address lib (immutable! (copy-seq name)))))
(defforeign "ffi2-lib-ref" (lib name) (:leaf t)
  (make-cp nil (lookup-object "ffi2-lib-ref" name lib)))
(defforeign "ffi-obj?" (x) (:leaf t) (bool (ffi-obj-p x)))
(defforeign "ffi-obj-name" (o) (:leaf t)
  (unless (ffi-obj-p o) (arg-error "ffi-obj-name" "ffi-obj?" o))
  (cp-field o 6))
(defforeign "ffi-obj-lib" (o) (:leaf t)
  (unless (ffi-obj-p o) (arg-error "ffi-obj-lib" "ffi-obj?" o))
  (cp-field o 5))

;;; ------------------------------------------------------------------
;;; Calling C, and being called

(defvar *pointer-ffi-type*
  (list :pointer :pointer
	(lambda (fptr) (if (cp-base fptr)
			   (cons (cp-base fptr) (cp-offset fptr))
			   (ldb (byte 64 0) (cp-offset fptr))))
	(lambda (address) (make-cp nil address))))

(defun ffi-type (who ct)
  (ecase (fct-host-rep ct)
    (:boolean '(:int32 :number))
    (:integer-8 '(:int8 :number)) (:unsigned-8 '(:uint8 :number))
    (:integer-16 '(:int16 :number)) (:unsigned-16 '(:uint16 :number))
    (:integer-32 '(:int32 :number)) (:unsigned-32 '(:uint32 :number))
    ((:integer-64 :fixnum :scheme-object) '(:int64 :number))
    (:unsigned-64 '(:uint64 :number))
    (:double '(:double :float)) (:float '(:float :float))
    ((:ftype-pointer :ftype-scheme-object-pointer :array) *pointer-ffi-type*)
    (:void '(:void :void))
    ((:struct :union)
     (unsupported who "a struct or union by value is not supported"))))

(defun c-arg (rep v)
  (case rep
    (:boolean (if (false-p v) 0 1))
    (:scheme-object (scheme-handle v))
    (t v)))

(defun c-result (rep raw)
  (case rep
    (:boolean (if (zerop raw) qs:false qs:true))
    (:scheme-object (handle-object raw))
    (:void *void*)
    (t raw)))

(defvar *call-cache* (make-hash-table :test 'equal :synchronized t))

(defun foreign-caller (address in-sigs out-sig errno)
  (let ((key (list address in-sigs out-sig errno)))
    (or (gethash key *call-cache*)
	(setf (gethash key *call-cache*)
	      (qsffi:foreign-function address in-sigs out-sig :errno errno)))))

(defun check-ffi (who in-types out-type abi varargs-after)
  (unless (and (listp in-types) (every #'fctype-p in-types))
    (arg-error who "(listof ctype?)" in-types))
  (check-ctype who out-type)
  (unless (or (false-p abi) (some (lambda (n) (symbol-named-p abi n)) '("default" "stdcall" "sysv")))
    (arg-error who "(or/c #f 'default 'stdcall 'sysv)" abi))
  (unless (or (false-p varargs-after) (and (integerp varargs-after) (> varargs-after 0)))
    (arg-error who "(or/c #f exact-positive-integer?)" varargs-after))
  (when (and (not (false-p varargs-after)) (> varargs-after (length in-types)))
    (arguments-error who "varargs-after value is too large"
		     "given value" varargs-after "argument count" (length in-types))))

(defun check-ffi-call (who in-types out-type abi varargs-after save-errno lock-name)
  (check-ffi who in-types out-type abi varargs-after)
  (unless (or (false-p save-errno) (symbol-named-p save-errno "posix") (symbol-named-p save-errno "windows"))
    (arg-error who "(or/c #f 'posix 'windows)" save-errno))
  (unless (or (false-p lock-name) (stringp lock-name))
    (arg-error who "(or/c string? #f)" lock-name)))

(defun make-caller (in-types out-type save-errno)
  "A function of a cpointer returning the Racket procedure that calls C
there."
  (let* ((in-sigs (mapcar (lambda (c) (ffi-type "ffi-call" c)) in-types))
	 (out-sig (ffi-type "ffi-call" out-type))
	 (reps (mapcar #'fct-host-rep in-types))
	 (out-rep (fct-host-rep out-type))
	 (n (length in-types))
	 (errno (truthy save-errno)))
    (lambda (to-wrap)
      (let* ((proc-p (cptr->fptr "ffi-call" to-wrap))
	     (name (if (ffi-obj-p proc-p) (rsym (utf8 (cp-field proc-p 6))) qs:false))
	     (f (lambda (&rest args)
		  (unless (= (length args) n)
		    (racket-arity-error (if (false-p name) "proc" (rname name)) (length args) n))
		  (let ((cargs (loop for a in args for ct in in-types
				     collect (funcall (fct-s->c ct) name a))))
		    (when (cp-gcable-p proc-p)
		      (racket-error "ffi-call" "cannot call a pointer into GCable memory"))
		    (let ((function (foreign-caller (cp-address proc-p) in-sigs out-sig errno))
			  (pargs (loop for rep in reps for c in cargs collect (c-arg rep c))))
		      (multiple-value-bind (raw e) (apply function pargs)
			(when errno (setq *saved-errno* e))
			(funcall (fct-c->s out-type) (c-result out-rep raw))))))))
	(unless (false-p name) (setf (gethash f *renamed-procedures*) name))
	(with-arity-mask (ash 1 n) f)))))

(defforeign "ffi-call" (p in-types out-type &optional (abi qs:false) (save-errno qs:false)
			  (orig-place qs:false) (lock-name qs:false) (blocking qs:false)
			  (varargs-after qs:false) (exns qs:false)) ()
  (declare (ignore orig-place blocking exns))
  (check-cp "ffi-call" p)
  (check-ffi-call "ffi-call" in-types out-type abi varargs-after save-errno lock-name)
  (funcall (make-caller in-types out-type save-errno) p))

(defforeign "ffi-call-maker" (in-types out-type &optional (abi qs:false) (save-errno qs:false)
				(orig-place qs:false) (lock-name qs:false) (blocking qs:false)
				(varargs-after qs:false) (exns qs:false) (core qs:false)) (:leaf t)
  (declare (ignore orig-place blocking exns core))
  (check-ffi-call "ffi-call-maker" in-types out-type abi varargs-after save-errno lock-name)
  (make-caller in-types out-type save-errno))

(defforeign "ffi-maybe-call-and-callback-core"
    (must? abi varargs-after blocking? async-apply? save-errno out &rest ins) (:leaf t)
  (declare (ignore must?))
  (values qs:false ins out abi varargs-after blocking? async-apply? save-errno))

(defun check-ffi-callback (who in-types out-type abi varargs-after async-apply)
  (check-ffi who in-types out-type abi varargs-after)
  (unless (or (false-p async-apply) (rbox-p async-apply) (accepts-arity-p async-apply 1))
    (arg-error who "(or/c #f (procedure-arity-includes/c 1) box?)" async-apply)))

(defun make-callback-maker (in-types out-type)
  (let* ((in-sigs (mapcar (lambda (c) (ffi-type "ffi-callback" c)) in-types))
	 (out-sig (ffi-type "ffi-callback" out-type))
	 (reps (mapcar #'fct-host-rep in-types))
	 (out-rep (fct-host-rep out-type))
	 (callback (rsym "callback")))
    (lambda (proc)
      (unless (functionp proc) (arg-error "make-ffi-callback" "procedure?" proc))
      (let* ((wrapper
	       (lambda (&rest raws)
		 (let* ((args (loop for raw in raws for ct in in-types for rep in reps
				    collect (funcall (fct-c->s ct) (c-result rep raw))))
			(v (apply proc args)))
		   (c-arg out-rep (funcall (fct-s->c out-type) callback v)))))
	     (address (qsffi:foreign-callback wrapper in-sigs out-sig)))
	(make-callback address wrapper)))))

(defforeign "ffi-callback" (proc in-types out-type &optional (abi qs:false) (atomic? qs:false)
			      (async-apply qs:false) (varargs-after qs:false)) (:leaf t)
  (declare (ignore atomic?))
  (unless (functionp proc) (arg-error "ffi-callback" "procedure?" proc))
  (check-ffi-callback "ffi-callback" in-types out-type abi varargs-after async-apply)
  (funcall (make-callback-maker in-types out-type) proc))

(defforeign "ffi-callback-maker" (in-types out-type &optional (abi qs:false) (atomic? qs:false)
				    (async-apply qs:false) (varargs-after qs:false) (core qs:false))
    (:leaf t)
  (declare (ignore atomic? core))
  (check-ffi-callback "ffi-callback-maker" in-types out-type abi varargs-after async-apply)
  (make-callback-maker in-types out-type))

(defforeign "ffi-callback?" (x) (:leaf t) (bool (callback-p x)))

;;; _path is the io layer's (racket/src/io/path/ffi.rkt): bytes
;;; with a NUL, made from a path-string, and a path from bytes.
(defvar *_path*
  (funcall (native-function (gethash "make-ctype" *natives*))
	   *_bytes*
	   (lambda (p)
	     (cond ((false-p p) p)
		   ((or (stringp p) (rpath-p p))
		    (let ((s (if (stringp p) p (rpath-string p))))
		      (concatenate 'octets (sb-ext:string-to-octets s :external-format :utf-8)
				   (vector 0))))
		   (t (arg-error "_path" "(or/c path-string? #f)" p))))
	   (lambda (b) (if (false-p b) b (make-rpath (utf8 b))))))
(define-primitive "#%foreign" "_path" *_path*)
