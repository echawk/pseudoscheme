; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; The C half of (system foreign) and (system foreign-library), on the
;;;; foreign-function layer the Schemes share (src/ffi.lisp): pointer
;;;; objects, the foreign types, pointer->procedure and
;;;; procedure->pointer, dlopen and dlsym.
;;;;
;;;; A pointer is either an address, interned so that pointers to the
;;;; same address are eq? (and %null-pointer is (make-pointer 0)), or a
;;;; place in a bytevector (bytevector->pointer, string->pointer), which
;;;; is pinned whenever it is passed to C, so C reads and writes the
;;;; bytevector itself.  pointer->bytevector of foreign memory copies it:
;;;; a Lisp vector can't be laid over C's memory, so writes to the copy
;;;; don't reach C.  It returns the bytevector itself for a pointer to
;;;; the whole of one.

(in-package "PSEUDOSCHEME-GUILE")

(defstruct (gpointer (:constructor %make-gpointer (address &optional backing (offset 0)))
		     (:copier nil))
  address				; for foreign memory
  backing				; or the bytevector pointed into
  offset
  (finalizer nil))

(defun pointer-place (p)
  "What the shared layer takes: an address, or (bytevector . offset)."
  (if (gpointer-backing p) (cons (gpointer-backing p) (gpointer-offset p)) (gpointer-address p)))

(defun pointer-address* (p) (psffi:call-with-address (pointer-place p) #'identity))

(defmethod print-object ((p gpointer) stream)
  (format stream "#<pointer 0x~(~X~)>" (pointer-address* p)))

(defvar *pointers* (make-hash-table :test 'eql :weakness :value :synchronized t))

(defun make-pointer* (address)
  (or (gethash address *pointers*)
      (setf (gethash address *pointers*) (%make-gpointer address))))

(defun check-pointer (subr p &optional (pos 1))
  (unless (gpointer-p p) (wrong-type subr pos p))
  p)

(defun check-non-null (subr p)
  (check-pointer subr p)
  (when (eql (gpointer-address p) 0)
    (guile-error (ssym "null-pointer-error") subr "null pointer dereference" '()))
  p)

;;; scm->pointer: an object's "address" is a number standing for it
(defvar *object-addresses* (make-hash-table :test 'eq :weakness :key :synchronized t))
(defvar *address-objects* (make-hash-table :test 'eql :weakness :value :synchronized t))
(defvar *next-object-address* #x1000)

(defun object-address (x)
  (or (gethash x *object-addresses*)
      (let ((a (incf *next-object-address* 16)))
	(setf (gethash a *address-objects*) x (gethash x *object-addresses*) a))))

;;; ------------------------------------------------------------------
;;; Types: Guile's codes, '* for a pointer, a list for a struct

(defparameter *foreign-type-codes*
  '(("void" . 0) ("float" . 1) ("double" . 2) ("uint8" . 3) ("int8" . 4)
    ("uint16" . 5) ("int16" . 6) ("uint32" . 7) ("int32" . 8) ("uint64" . 9) ("int64" . 10)
    ("complex-float" . 11) ("complex-double" . 12)
    ;; the C types, by their size here
    ("short" . 6) ("unsigned-short" . 5) ("int" . 8) ("unsigned-int" . 7)
    ("long" . 10) ("unsigned-long" . 9) ("size_t" . 9) ("ssize_t" . 10) ("ptrdiff_t" . 10)
    ("intptr_t" . 10) ("uintptr_t" . 9)))

(defparameter *code-ctypes*
  #(:void :float :double :uint8 :int8 :uint16 :int16 :uint32 :int32 :uint64 :int64))

(defun pointer-type-p (type)
  (and (symbolp type) (not (keywordp type)) (string= (ps:scheme-symbol-name type) "*")))

(defun ffi-type (subr type)
  "The shared layer's type for Guile's foreign TYPE."
  (cond ((pointer-type-p type)
	 (list :pointer :pointer
	       (lambda (x) (pointer-place (check-pointer subr x)))
	       #'make-pointer*))
	((and (integerp type) (<= 1 type 2)) (list (svref *code-ctypes* type) :float))
	((eql type 0) (list :void :void))
	((and (integerp type) (<= 3 type 10)) (list (svref *code-ctypes* type) :number))
	(t (guile-error (ssym "misc-error") subr "unsupported foreign type: ~S (structs and complex numbers are passed by reference only here)"
			(list type)))))

(defun type-size* (type)
  (cond ((pointer-type-p type) 8)
	((listp type) (struct-layout type))
	((eql type 11) 8)
	((eql type 12) 16)
	((and (integerp type) (<= 1 type 10)) (psffi:type-size (svref *code-ctypes* type)))
	(t (wrong-type "sizeof" 1 type))))

(defun type-alignment* (type)
  (cond ((listp type) (if type (reduce #'max (mapcar #'type-alignment* type)) 1))
	((eql type 11) 4)
	((eql type 12) 8)
	(t (type-size* type))))

(defun struct-layout (types)
  "The size of a C struct of TYPES."
  (let ((offset 0))
    (dolist (type types)
      (let ((a (type-alignment* type)))
	(setq offset (+ (* a (ceiling offset a)) (type-size* type)))))
    (let ((a (type-alignment* types)))
      (* a (ceiling offset a)))))

;;; ------------------------------------------------------------------
;;; Strings

(defun encoding-name (encoding)
  (if (or (null encoding) (eq encoding ps:false))
      :utf-8
      (let ((name (string-upcase encoding)))
	(cond ((member name '("UTF-8" "UTF8") :test #'string=) :utf-8)
	      ((member name '("ISO-8859-1" "ISO8859-1" "LATIN1" "LATIN-1") :test #'string=) :latin-1)
	      ((member name '("ASCII" "US-ASCII" "ANSI_X3.4-1968") :test #'string=) :ascii)
	      (t (intern name "KEYWORD"))))))

(defun encode-string (subr string encoding)
  "STRING's bytes in ENCODING, NUL-terminated; characters ENCODING lacks
go as %default-port-conversion-strategy says."
  (let ((limit (case encoding (:latin-1 256) (:ascii 128))))
    (when (and limit (find-if (lambda (c) (>= (char-code c) limit)) string))
      (let ((strategy (ps:scheme-symbol-name (fluid-value (root-value "%default-port-conversion-strategy")))))
	(setq string
	      (with-output-to-string (out)
		(loop for c across string
		      do (cond ((< (char-code c) limit) (write-char c out))
			       ((string= strategy "escape") (format out "\\u~(~4,'0X~)" (char-code c)))
			       ((string= strategy "substitute") (write-char #\? out))
			       (t (guile-error (ssym "encoding-error") subr
					       "conversion to port encoding failed" '()))))))))
    (sb-ext:string-to-octets string :external-format encoding :null-terminate t)))

;;; ------------------------------------------------------------------

(defextension "scm_init_foreign"
  (append
   (loop for (name . code) in *foreign-type-codes* collect (cons name code))
   (list
    (cons "%null-pointer" (make-pointer* 0))
    (cons "pointer?" (lambda (x) (bool (gpointer-p x))))
    (cons "make-pointer" (lambda (address &optional finalizer)
			   (let ((p (make-pointer* address)))
			     (when (and finalizer (gpointer-p finalizer))
			       (set-pointer-finalizer p finalizer))
			     p)))
    (cons "pointer-address" (lambda (p) (pointer-address* (check-pointer "pointer-address" p))))
    (cons "scm->pointer" (lambda (x) (make-pointer* (object-address x))))
    (cons "pointer->scm" (lambda (p)
			   (multiple-value-bind (x found) (gethash (pointer-address* p) *address-objects*)
			     (if found x (wrong-type "pointer->scm" 1 p)))))
    (cons "set-pointer-finalizer!" (lambda (p finalizer)
				     (set-pointer-finalizer (check-pointer "set-pointer-finalizer!" p) finalizer)
				     *unspecified*))
    (cons "bytevector->pointer" (lambda (bv &optional (offset 0))
				  (unless (typep bv 'ps-r6rs::octets) (wrong-type "bytevector->pointer" 1 bv))
				  (%make-gpointer nil bv offset)))
    (cons "pointer->bytevector" #'pointer->bytevector)
    (cons "dereference-pointer" (lambda (p)
				  (check-non-null "dereference-pointer" p)
				  (make-pointer* (psffi:call-with-address
						  (pointer-place p)
						  (lambda (a) (psffi:mem-ref :pointer a))))))
    (cons "string->pointer" (lambda (string &optional encoding)
			      (%make-gpointer nil (encode-string "string->pointer" string (encoding-name encoding)))))
    (cons "pointer->string" (lambda (p &optional (length -1) encoding)
			      (check-non-null "pointer->string" p)
			      (psffi:call-with-address
			       (pointer-place p)
			       (lambda (a)
				 (psffi:read-c-string a :length (and (>= length 0) length)
							:encoding (encoding-name encoding))))))
    (cons "sizeof" #'type-size*)
    (cons "alignof" #'type-alignment*)
    (cons "pointer->procedure" #'pointer->procedure)
    (cons "procedure->pointer" (lambda (return procedure arg-types)
				 (make-pointer*
				  (psffi:foreign-callback
				   procedure
				   (mapcar (lambda (type) (ffi-type "procedure->pointer" type)) arg-types)
				   (ffi-type "procedure->pointer" return))))))))

(defun set-pointer-finalizer (p finalizer)
  "Call the C function FINALIZER on P's address once P is garbage."
  (setf (gpointer-finalizer p) finalizer)
  (let ((address (pointer-address* p))
	(function (pointer-address* finalizer)))
    (trivial-garbage:finalize p (lambda ()
				  (cffi:foreign-funcall-pointer (sb-sys:int-sap function) ()
								:pointer (sb-sys:int-sap address) :void)))))

(defun pointer->bytevector (p length &optional (offset 0) uvec-type)
  (declare (ignore uvec-type))
  (check-non-null "pointer->bytevector" p)
  (let ((bv (gpointer-backing p)))
    (if (and bv (zerop (+ offset (gpointer-offset p))) (= length (length bv)))
	bv
	(psffi:call-with-address
	 (pointer-place p)
	 (lambda (a)
	   (let ((octets (make-array length :element-type '(unsigned-byte 8)))
		 (sap (sb-sys:int-sap (+ a offset))))
	     (dotimes (i length octets)
	       (setf (aref octets i) (sb-sys:sap-ref-8 sap i)))))))))

(defun pointer->procedure (return p arg-types &rest options)
  (check-pointer "pointer->procedure" p 2)
  (let ((errno (loop for (k v) on options by #'cddr
		     thereis (and (keywordp k) (string-equal (symbol-name k) "RETURN-ERRNO?")
				  (not (eq v ps:false))))))
    (psffi:foreign-function (pointer-address* p)
			    (mapcar (lambda (type) (ffi-type "pointer->procedure" type)) arg-types)
			    (let ((r (ffi-type "pointer->procedure" return)))
			      (if (eq (second r) :void) (list :void :void) r))
			    :errno errno)))

(defextension "scm_init_system_foreign_library"
  (list
   (cons "dlopen" (lambda (name flags)
		    (multiple-value-bind (handle message)
			(psffi:dlopen (if (eq name ps:false) nil name) flags)
		      (if handle
			  (make-pointer* handle)
			  (guile-error (ssym "misc-error") "dlopen" "file: ~S, message: ~S"
				       (list name message))))))
   (cons "dlsym" (lambda (handle name)
		   (multiple-value-bind (address message)
		       (psffi:dlsym (pointer-address* (check-pointer "dlsym" handle)) name)
		     (if address
			 (make-pointer* address)
			 (guile-error (ssym "misc-error") "dlsym" "Error resolving ~S: ~S"
				      (list name message))))))
   (cons "RTLD_LAZY" psffi:+rtld-lazy+)
   (cons "RTLD_NOW" psffi:+rtld-now+)
   (cons "RTLD_GLOBAL" psffi:+rtld-global+)
   (cons "RTLD_LOCAL" psffi:+rtld-local+)))
