; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-FFI -*-

;;;; The foreign-function layer the Schemes share, on CFFI: calling the C
;;;; function at an address with its arguments and result converted,
;;;; making a C function pointer of a Lisp function, dlopen and dlsym,
;;;; and foreign memory by integer address.  Chez's FFI
;;;; (src/chez/ffi.lisp) and Guile's (system foreign)
;;;; (src/guile/foreign.lisp) are made of it.
;;;;
;;;; A type is a list (CTYPE KIND . OPTIONS): CTYPE is CFFI's (:int,
;;;; :double, :pointer, ...), KIND how a Scheme value crosses:
;;;;   :number   an exact integer
;;;;   :float    a real, coerced to CTYPE; results come back as doubles
;;;;   :boolean  #f is 0, anything else 1; a result is #f or #t
;;;;   :char     a character, by its code
;;;;   :string   a string or #f (NULL), in the encoding OPTIONS names
;;;;             (:utf-8 by default)
;;;;   :bytes    a bytevector or #f: its data, pinned for the call
;;;;   :pointer  anything, with OPTIONS (TO FROM): TO makes the argument
;;;;             an integer address or (bytevector . offset), which is
;;;;             pinned for the call; FROM makes a result's address a value
;;;;   :void     only a result: the unspecified value
;;;; A foreign function is compiled once per call to FOREIGN-FUNCTION.
;;;; Pinning is SBCL's, so C may keep a bytevector's address only for
;;;; the length of the call, as with Chez's u8*.

(defpackage "PSEUDOSCHEME-FFI"
  (:nicknames "PSFFI")
  (:use "CL")
  (:export "FOREIGN-FUNCTION" "FOREIGN-CALLBACK"
	   "DLOPEN" "DLSYM" "+RTLD-LAZY+" "+RTLD-NOW+" "+RTLD-GLOBAL+" "+RTLD-LOCAL+"
	   "LOAD-LIBRARY" "SYMBOL-ADDRESS"
	   "MEM-REF" "MEM-SET" "TYPE-SIZE" "TYPE-ALIGNMENT" "ALLOCATE" "FREE"
	   "VECTOR-ADDRESS" "CALL-WITH-ADDRESS" "READ-C-STRING"))

(in-package "PSEUDOSCHEME-FFI")

(defun false-p (x) (eq x ps:false))

(defun type-ctype (type) (first type))
(defun type-kind (type) (second type))
(defun type-options (type) (cddr type))

(defun vector-address (octets)
  "The address of OCTETS' data now; it holds only while OCTETS is pinned."
  (sb-sys:sap-int (sb-sys:vector-sap octets)))

(defun call-with-address (place function)
  "Call FUNCTION with the integer address PLACE denotes: an integer, or
(bytevector . offset), pinned for the call."
  (if (consp place)
      (let ((v (car place)))
	(sb-sys:with-pinned-objects (v)
	  (funcall function (+ (vector-address v) (cdr place)))))
      (funcall function place)))

(defun quiet-compile (form)
  "FORM, a lambda expression, compiled without the compiler's notes and warnings."
  (handler-bind ((warning #'muffle-warning)
		 (sb-ext:compiler-note #'muffle-warning))
    (let ((*error-output* (make-broadcast-stream)))
      (compile nil form))))

(defun encoding-format (encoding)
  (or encoding :utf-8))

;;; ------------------------------------------------------------------
;;; Arguments and results

(defun argument-place (type value)
  "For a pointer-like argument: an integer address, or (octets . offset)
to pin and point into."
  (ecase (type-kind type)
    (:string (if (false-p value) 0
		 (cons (sb-ext:string-to-octets value :external-format (encoding-format (first (type-options type)))
						      :null-terminate t)
		       0)))
    (:bytes (if (false-p value) 0 (cons value 0)))
    (:pointer (funcall (first (type-options type)) value))))

(defun pointer-kind-p (type) (member (type-kind type) '(:string :bytes :pointer)))

(defun argument-value (type value)
  "A non-pointer argument as CFFI passes it."
  (ecase (type-kind type)
    (:number (if (integerp value) value (error 'type-error :datum value :expected-type 'integer)))
    (:float (coerce value (if (eq (type-ctype type) :float) 'single-float 'double-float)))
    (:boolean (if (false-p value) 0 1))
    (:char (char-code value))))

(defun read-c-string (address &key (length nil) (encoding :utf-8))
  "The string of LENGTH bytes (up to a NUL if NIL) at ADDRESS."
  (let* ((sap (sb-sys:int-sap address))
	 (n (or length (loop for i from 0 until (zerop (sb-sys:sap-ref-8 sap i)) finally (return i))))
	 (octets (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref octets i) (sb-sys:sap-ref-8 sap i)))
    (sb-ext:octets-to-string octets :external-format (encoding-format encoding))))

(defun result-value (type raw)
  (ecase (type-kind type)
    (:void ps:unspecific)
    (:number raw)
    (:float (coerce raw 'double-float))
    (:boolean (if (zerop raw) ps:false t))
    (:char (code-char raw))
    (:string (let ((a (cffi:pointer-address raw)))
	       (if (zerop a) ps:false
		   (read-c-string a :encoding (first (type-options type))))))
    (:pointer (funcall (second (type-options type)) (cffi:pointer-address raw)))))

(defun call-ctype (type)
  (if (pointer-kind-p type) :pointer (type-ctype type)))

;;; ------------------------------------------------------------------
;;; Calling C

(defun foreign-function (address arguments result &key errno)
  "A function calling the C function at ADDRESS (an integer) with
ARGUMENTS and RESULT, types as above.  With ERRNO, it returns errno
after the call as a second value."
  (let* ((args (loop for nil in arguments collect (gensym "ARG")))
	 (vectors (loop for nil in arguments collect (gensym "V")))
	 (places (loop for nil in arguments collect (gensym "P")))
	 (call-args
	   (loop for a in args for v in vectors for p in places for type in arguments
		 append (if (pointer-kind-p type)
			    `(:pointer (sb-sys:int-sap (if ,v (+ (vector-address ,v) ,p) ,p)))
			    `(,(type-ctype type) (argument-value ',type ,a)))))
	 (call `(cffi:foreign-funcall-pointer (sb-sys:int-sap ,address) () ,@call-args
					      ,(call-ctype result)))
	 (body `(let ((raw ,call))
		  ,(if errno
		       `(let ((e (sb-alien:get-errno))) (values (result-value ',result raw) e))
		       `(result-value ',result raw)))))
    (quiet-compile
	       `(lambda ,args
		  (let* (,@(loop for p in places for type in arguments
				 when (pointer-kind-p type) collect `(,p 0))
			 ,@(loop for a in args for v in vectors for p in places for type in arguments
				when (pointer-kind-p type)
				  collect `(,v (let ((place (argument-place ',type ,a)))
						 (setq ,p (if (consp place) (cdr place) place))
						 (and (consp place) (car place))))))
		    (declare (ignorable ,@(loop for v in vectors for type in arguments
						 when (pointer-kind-p type) collect v)))
		    (sb-sys:with-pinned-objects
			,(loop for v in vectors for type in arguments when (pointer-kind-p type) collect v)
		      ,body))))))

(defvar *callback-functions* (make-hash-table :test 'eq)
  "Callback name -> the Lisp function it calls, which this keeps alive.")

(defun callback-argument (type raw)
  (ecase (type-kind type)
    ((:number) raw)
    (:float (coerce raw 'double-float))
    (:boolean (if (zerop raw) ps:false t))
    (:char (code-char raw))
    (:string (let ((a (cffi:pointer-address raw)))
	       (if (zerop a) ps:false (read-c-string a :encoding (first (type-options type))))))
    (:pointer (funcall (second (type-options type)) (cffi:pointer-address raw)))))

(defun callback-result (type value)
  (ecase (type-kind type)
    (:void nil)
    ((:number :float :boolean :char) (argument-value type value))
    (:pointer (let ((place (funcall (first (type-options type)) value)))
		(sb-sys:int-sap (if (consp place)
				    (+ (vector-address (car place)) (cdr place))
				    place))))))

(defun foreign-callback (function arguments result)
  "The address of a C function of ARGUMENTS returning RESULT that calls
FUNCTION.  The callback, and FUNCTION, are never freed."
  (let ((name (gensym "CALLBACK"))
	(vars (loop for nil in arguments collect (gensym "A"))))
    (setf (gethash name *callback-functions*) function)
    (funcall
       (quiet-compile
		`(lambda ()
		   (cffi:defcallback ,name ,(call-ctype result)
		       ,(loop for v in vars for type in arguments collect (list v (call-ctype type)))
		     (callback-result ',result
				      (funcall (gethash ',name *callback-functions*)
					       ,@(loop for v in vars for type in arguments
						       collect `(callback-argument ',type ,v))))))))
    (cffi:pointer-address (cffi:get-callback name))))

;;; ------------------------------------------------------------------
;;; Libraries

(defconstant +rtld-lazy+ 1)
(defconstant +rtld-now+ 2)
(defconstant +rtld-global+ #+darwin 8 #-darwin #x100)
(defconstant +rtld-local+ #+darwin 4 #-darwin 0)

(defun dlerror ()
  (let ((p (cffi:foreign-funcall "dlerror" :pointer)))
    (if (cffi:null-pointer-p p) "unknown error" (cffi:foreign-string-to-lisp p))))

(defun dlopen (name &optional (flags (logior +rtld-lazy+ +rtld-local+)))
  "The handle (an integer) of the library NAME, or of the program if NAME
is NIL; NIL and the error message if it can't be opened."
  (let ((handle (if name
		    (cffi:foreign-funcall "dlopen" :string name :int flags :pointer)
		    (cffi:foreign-funcall "dlopen" :pointer (cffi:null-pointer) :int flags :pointer))))
    (if (cffi:null-pointer-p handle)
	(values nil (dlerror))
	(cffi:pointer-address handle))))

(defun dlsym (handle name)
  "The address of NAME in the library HANDLE; NIL and the error message
if there is none."
  (cffi:foreign-funcall "dlerror" :pointer)
  (let ((p (cffi:foreign-funcall "dlsym" :pointer (sb-sys:int-sap handle) :string name :pointer)))
    (if (cffi:null-pointer-p p)
	(values nil (dlerror))
	(cffi:pointer-address p))))

(defun load-library (path)
  "Load the shared library PATH (CFFI's search), its symbols then global."
  (cffi:load-foreign-library path))

(defun symbol-address (name)
  "The address of the C symbol NAME in what is loaded, or NIL."
  (let ((p (cffi:foreign-symbol-pointer name)))
    (and p (cffi:pointer-address p))))

;;; ------------------------------------------------------------------
;;; Memory

(defun type-size (ctype) (cffi:foreign-type-size ctype))
(defun type-alignment (ctype) (cffi:foreign-type-alignment ctype))

(defun allocate (n) (cffi:pointer-address (cffi:foreign-alloc :uint8 :count (max n 1))))
(defun free (address) (cffi:foreign-free (sb-sys:int-sap address)))

(defun mem-ref (ctype address &optional (offset 0))
  (let ((v (cffi:mem-ref (sb-sys:int-sap address) ctype offset)))
    (if (eq ctype :pointer) (cffi:pointer-address v) v)))

(defvar *setters* (make-hash-table :test 'eq)
  "CTYPE -> a function of value, address and offset storing the value:
CFFI's (setf mem-ref) is a setf expansion, so it's compiled per type.")

(defun mem-set (ctype address offset value)
  (funcall (or (gethash ctype *setters*)
	       (setf (gethash ctype *setters*)
		     (quiet-compile `(lambda (value address offset)
				     (setf (cffi:mem-ref (sb-sys:int-sap address) ,ctype offset)
					   ,(if (eq ctype :pointer) '(sb-sys:int-sap value) 'value))))))
	   value address offset))
