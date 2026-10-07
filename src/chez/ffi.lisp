; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Chez Scheme's foreign-function interface, on CFFI: load-shared-object,
;;;; foreign-procedure, and foreign memory as integer addresses
;;;; (foreign-alloc, foreign-free, foreign-ref, foreign-set!,
;;;; foreign-sizeof).  CFFI is loaded the first time one of these is used,
;;;; so a program that doesn't use the FFI doesn't need it.
;;;;
;;;; A foreign procedure is compiled when it is made: a Lisp function
;;;; calling the entry point through CFFI with the argument and result
;;;; types translated from Chez's.  Not here: foreign-callable, ftypes,
;;;; and the types scheme-object, wchar_t and the u16*/u32* buffers.

(in-package "PSEUDOSCHEME-R6RS")

(defvar *cffi-loaded* nil)

(defun ensure-cffi ()
  (unless *cffi-loaded*
    (let ((*standard-output* (make-broadcast-stream)))
      (if (find-package "QL")
	  (uiop:symbol-call "QL" "QUICKLOAD" "cffi" :silent t)
	  (asdf:load-system "cffi")))
    (setq *cffi-loaded* t)))

(defun cffi (name)
  (or (find-symbol name "CFFI") (error "no CFFI:~A" name)))

(defun chez-ffi-type (type)
  "The CFFI type for Chez's foreign type TYPE (a Scheme symbol), and how
values cross: :int, :pointer for u8* (a bytevector's data), :string, :address
(an integer, as Chez gives pointers), :boolean, :void."
  (let ((name (ps:scheme-symbol-name type)))
    (cond ((member name '("int" "integer-32") :test #'string=) (values :int :number))
	  ((member name '("unsigned" "unsigned-int" "unsigned-32") :test #'string=) (values :unsigned-int :number))
	  ((member name '("long") :test #'string=) (values :long :number))
	  ((member name '("unsigned-long") :test #'string=) (values :unsigned-long :number))
	  ((member name '("short" "integer-16") :test #'string=) (values :short :number))
	  ((member name '("unsigned-short" "unsigned-16") :test #'string=) (values :unsigned-short :number))
	  ((member name '("integer-8") :test #'string=) (values :int8 :number))
	  ((member name '("unsigned-8") :test #'string=) (values :uint8 :number))
	  ((member name '("integer-64" "long-long") :test #'string=) (values :int64 :number))
	  ((member name '("unsigned-64" "unsigned-long-long") :test #'string=) (values :uint64 :number))
	  ((member name '("iptr" "ssize_t" "fixnum" "ptrdiff_t") :test #'string=) (values :intptr :number))
	  ((member name '("uptr" "size_t") :test #'string=) (values :uintptr :number))
	  ((member name '("void*") :test #'string=) (values :uintptr :number))
	  ((member name '("double" "double-float") :test #'string=) (values :double :float))
	  ((member name '("float" "single-float") :test #'string=) (values :float :float))
	  ((member name '("boolean") :test #'string=) (values :int :boolean))
	  ((member name '("char") :test #'string=) (values :uint8 :char))
	  ((member name '("string" "utf-8") :test #'string=) (values :pointer :string))
	  ((member name '("u8*") :test #'string=) (values :pointer :bytes))
	  ((member name '("void") :test #'string=) (values :void :void))
	  (t (ps:scheme-error "foreign-procedure: unsupported type ~A" name)))))

(defun foreign-procedure-lambda (pointer-form params result)
  "A lambda calling the function at POINTER-FORM with Chez types PARAMS,
returning RESULT."
  (let* ((args (loop for nil in params collect (gensym "ARG")))
	 (pinned '())
	 (call-args '()))
    ;; each argument: its CFFI type and the form passing it
    (loop for arg in args for type in params
	  do (multiple-value-bind (ctype kind) (chez-ffi-type type)
	       (case kind
		 (:bytes
		  (let ((p (gensym "P")))
		    (push (list arg p) pinned)
		    (setq call-args (list* p :pointer call-args))))
		 (:string
		  (let ((p (gensym "S")))
		    (push (list arg p :string) pinned)
		    (setq call-args (list* p :pointer call-args))))
		 (:boolean (setq call-args (list* `(if (eq ,arg ps:false) 0 1) ctype call-args)))
		 (:char (setq call-args (list* `(char-code ,arg) ctype call-args)))
		 (:float (setq call-args (list* `(coerce ,arg ',(if (eq ctype :double) 'double-float 'single-float))
						ctype call-args)))
		 (t (setq call-args (list* arg ctype call-args))))))
    (multiple-value-bind (rtype rkind) (chez-ffi-type result)
      (let* ((call `(,(cffi "FOREIGN-FUNCALL-POINTER") ,pointer-form () ,@(reverse call-args)
		     ,(if (eq rkind :string) :pointer rtype)))
	     (call (case rkind
		     (:void `(progn ,call ps:unspecific))
		     (:boolean `(if (zerop ,call) ps:false t))
		     (:char `(code-char ,call))
		     (:float `(coerce ,call 'double-float))
		     (:string `(let ((p ,call))
				 (if (,(cffi "NULL-POINTER-P") p) ps:false
				     (coerce (,(cffi "FOREIGN-STRING-TO-LISP") p) 'simple-string))))
		     (t call))))
	;; wrap the call in the pinning/conversion of each pointer argument
	(dolist (pin pinned)
	  (destructuring-bind (arg p &optional string) pin
	    (setq call
		  (if string
		      `(if (eq ,arg ps:false)
			   (let ((,p (,(cffi "NULL-POINTER")))) ,call)
			   (,(cffi "WITH-FOREIGN-STRING") (,p ,arg) ,call))
		      `(if (eq ,arg ps:false)
			   (let ((,p (,(cffi "NULL-POINTER")))) ,call)
			   (,(cffi "WITH-POINTER-TO-VECTOR-DATA") (,p ,arg) ,call))))))
	`(lambda ,args ,call)))))

(defprim "chez:load-shared-object" (path)
  (ensure-cffi)
  (funcall (cffi "LOAD-FOREIGN-LIBRARY") path)
  ps:unspecific)

(defprim "chez:foreign-procedure" (entry params result)
  (ensure-cffi)
  (let ((pointer (if (integerp entry)
		     (funcall (cffi "MAKE-POINTER") entry)
		     (funcall (cffi "FOREIGN-SYMBOL-POINTER") entry))))
    (unless pointer
      (ps:scheme-error "foreign-procedure: no entry for ~S" entry))
    (handler-bind ((warning #'muffle-warning))
      (compile nil (foreign-procedure-lambda `(quote ,pointer) params result)))))

(defprim "chez:foreign-entry?" (name)
  (ensure-cffi)
  (bool (funcall (cffi "FOREIGN-SYMBOL-POINTER") name)))

(defprim "chez:foreign-entry" (name)
  (ensure-cffi)
  (let ((p (funcall (cffi "FOREIGN-SYMBOL-POINTER") name)))
    (unless p (ps:scheme-error "foreign-entry: no entry for ~S" name))
    (funcall (cffi "POINTER-ADDRESS") p)))

;;; Memory, addressed by integers

(defun address-pointer (address) (funcall (cffi "MAKE-POINTER") address))

(defprim "chez:foreign-alloc" (n)
  (ensure-cffi)
  (funcall (cffi "POINTER-ADDRESS") (funcall (cffi "FOREIGN-ALLOC") :uint8 :count n)))

(defprim "chez:foreign-free" (address)
  (ensure-cffi)
  (funcall (cffi "FOREIGN-FREE") (address-pointer address))
  ps:unspecific)

(defprim "chez:foreign-sizeof" (type)
  (ensure-cffi)
  (funcall (cffi "FOREIGN-TYPE-SIZE") (chez-ffi-type type)))

(defprim "chez:foreign-ref" (type address offset)
  (ensure-cffi)
  (multiple-value-bind (ctype kind) (chez-ffi-type type)
    (let ((v (funcall (cffi "MEM-REF") (address-pointer address)
		      (if (member kind '(:string :bytes)) :uintptr ctype) offset)))
      (case kind
	(:char (code-char v))
	(:boolean (if (zerop v) ps:false t))
	(:float (coerce v 'double-float))
	(t v)))))

(defvar *foreign-setters* (make-hash-table :test 'eq)
  "CFFI type -> a function of value, pointer and offset storing the value:
CFFI's (setf mem-ref) is a setf expansion, so it's compiled per type.")

(defun foreign-setter (ctype)
  (or (gethash ctype *foreign-setters*)
      (setf (gethash ctype *foreign-setters*)
	    (compile nil `(lambda (value pointer offset)
			    (setf (,(cffi "MEM-REF") pointer ,ctype offset) value))))))

(defprim "chez:foreign-set!" (type address offset value)
  (ensure-cffi)
  (multiple-value-bind (ctype kind) (chez-ffi-type type)
    (funcall (foreign-setter (if (member kind '(:string :bytes)) :uintptr ctype))
	     (case kind
	       (:char (char-code value))
	       (:boolean (if (eq value ps:false) 0 1))
	       (:float (coerce value (if (eq ctype :double) 'double-float 'single-float)))
	       (t value))
	     (address-pointer address)
	     offset))
  ps:unspecific)
