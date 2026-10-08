; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; Chez Scheme's foreign-function interface, on CFFI: load-shared-object,
;;;; foreign-procedure, and foreign memory as integer addresses
;;;; (foreign-alloc, foreign-free, foreign-ref, foreign-set!,
;;;; foreign-sizeof), on the layer the Schemes share (src/ffi.lisp).
;;;;
;;;; A foreign procedure is compiled when it is made: a Lisp function
;;;; calling the entry point through CFFI with the argument and result
;;;; types translated from Chez's.  Not here: ftypes, and the types
;;;; scheme-object, wchar_t and the u16*/u32* buffers.

(in-package "PSEUDOSCHEME-R6RS")

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

(defun chez-type (type)
  "Chez's foreign TYPE as a type of the shared layer (src/ffi.lisp)."
  (multiple-value-list (chez-ffi-type type)))

(defprim "chez:load-shared-object" (path)
  (psffi:load-library path)
  ps:unspecific)

(defprim "chez:foreign-procedure" (entry params result)
  (let ((address (if (integerp entry) entry (psffi:symbol-address entry))))
    (unless address
      (ps:scheme-error "foreign-procedure: no entry for ~S" entry))
    (psffi:foreign-function address (mapcar #'chez-type params) (chez-type result))))

(defprim "chez:foreign-callable" (procedure params result)
  (psffi:foreign-callback procedure (mapcar #'chez-type params) (chez-type result)))

(defprim "chez:foreign-entry?" (name)
  (bool (psffi:symbol-address name)))

(defprim "chez:foreign-entry" (name)
  (or (psffi:symbol-address name)
      (ps:scheme-error "foreign-entry: no entry for ~S" name)))

;;; Memory, addressed by integers

(defprim "chez:foreign-alloc" (n) (psffi:allocate n))

(defprim "chez:foreign-free" (address)
  (psffi:free address)
  ps:unspecific)

(defprim "chez:foreign-sizeof" (type)
  (psffi:type-size (chez-ffi-type type)))

(defun memory-ctype (type)
  (multiple-value-bind (ctype kind) (chez-ffi-type type)
    (values (if (member kind '(:string :bytes)) :uintptr ctype) kind ctype)))

(defprim "chez:foreign-ref" (type address offset)
  (multiple-value-bind (ctype kind) (memory-ctype type)
    (let ((v (psffi:mem-ref ctype address offset)))
      (case kind
	(:char (code-char v))
	(:boolean (if (zerop v) ps:false t))
	(:float (coerce v 'double-float))
	(t v)))))

(defprim "chez:foreign-set!" (type address offset value)
  (multiple-value-bind (ctype kind) (memory-ctype type)
    (psffi:mem-set ctype address offset
		   (case kind
		     (:char (char-code value))
		     (:boolean (if (eq value ps:false) 0 1))
		     (:float (coerce value (if (eq ctype :double) 'double-float 'single-float)))
		     (t value))))
  ps:unspecific)
