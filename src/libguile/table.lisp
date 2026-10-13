; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-LIBGUILE -*-

;;;; libguile's functions, as a table (docs/libguile.md).
;;;;
;;;; Each entry is a C function our libguile-3.0.so.1 exports: its name,
;;;; its C return and argument types, and how the Lisp side implements
;;;; it.  gen.lisp writes api.c from the table, one stub per entry,
;;;; which passes the arguments to the Lisp side (psg_entry, core.c) with
;;;; the entry's index; bridge.lisp calls the entry's implementation.
;;;; So this file is loaded both by the generator (alone) and by the
;;;; system.
;;;;
;;;; Kinds of entries:
;;;;   DEFC     a function with C types; its implementation is a DEFIMPL
;;;;            in impl.lisp, given the arguments converted to Lisp.
;;;;   DEFS     a function that is a Scheme procedure of Guile's, called
;;;;            with SCM arguments: SCM_UNDEFINED for an optional one
;;;;            not given, a list for the rest.  Its implementation is
;;;;            that procedure.  (scheme.lisp has these, from Guile's
;;;;            manual, which lists each C function with its procedure.)
;;;;   DEFCORE  functions core.c defines in C alone (allocation).
;;;;   DEFMISSING  a function libguile exports that we don't implement
;;;;            yet: its stub raises an error saying so (missing.lisp
;;;;            lists them, from exports-3.0.11.txt, so that every
;;;;            program linked against Guile links against us).
;;;;   DEFVARARGS  a variadic function, written in core.c, which calls
;;;;            the Lisp side with the arguments' count and the
;;;;            arguments.
;;;;
;;;; Types: :scm, :void, :int, :uint, :long, :ulong, :size, :ssize,
;;;; :int8 ... :uint64, :intmax, :uintmax, :intptr, :uintptr, :double,
;;;; :char, :wchar (scm_t_wchar), :bool (an int), :ptr (any pointer),
;;;; :string (a const char *, NUL-terminated).
;;;; Options: :noreturn (it never returns: errors, throws), :shallow
;;;; (its SCM arguments are only read here, so they are synchronized
;;;; one level deep, not all through: bridge.lisp).

(defpackage "PSEUDOSCHEME-LIBGUILE"
  (:use "COMMON-LISP")
  (:export "*API*" "API-NAME" "API-RETURN" "API-ARGS" "API-KIND" "API-SCHEME"
	   "API-REQ" "API-OPT" "API-REST" "API-NORETURN" "API-SHALLOW"
	   "API-INDEX" "FIND-API"))

(in-package "PSEUDOSCHEME-LIBGUILE")

(defstruct (api (:constructor make-api (name return args kind &key scheme req opt rest
						noreturn shallow)))
  name return args kind scheme (req 0) (opt 0) rest noreturn shallow (index 0))

(defvar *api* (make-array 0 :adjustable t :fill-pointer t)
  "The table, in index order.")

(defvar *api-by-name* (make-hash-table :test 'equal))

(defun add-api (api)
  ;; the first entry for a name wins: table.lisp's over scheme.lisp's
  (unless (gethash (api-name api) *api-by-name*)
    (setf (api-index api) (fill-pointer *api*))
    (vector-push-extend api *api*)
    (setf (gethash (api-name api) *api-by-name*) api)))

(defun find-api (name) (gethash name *api-by-name*))

(defmacro defc (name return args &rest options)
  `(add-api (make-api ,name ,return ',args :c ,@options)))

(defmacro defmissing (name)
  `(add-api (make-api ,name :void '() :missing)))

(defmacro defcore (&rest names)
  `(progn ,@(loop for name in names collect `(add-api (make-api ,name :void '() :core)))))

(defmacro defvarargs (name return args &rest options)
  `(add-api (make-api ,name ,return ',args :varargs ,@options)))

(defmacro defs (name scheme req &optional (opt 0) rest &rest options)
  `(add-api (make-api ,name :scm
		      ',(make-list (+ req opt (if rest 1 0)) :initial-element :scm)
		      :scheme :scheme ,scheme :req ,req :opt ,opt :rest ,rest ,@options)))

;;; ------------------------------------------------------------------
;;; Written in core.c

(defcore "scm_gc_malloc" "scm_gc_malloc_pointerless" "scm_gc_calloc" "scm_gc_realloc"
  "scm_gc_free" "scm_gc_strdup" "scm_gc_strndup" "scm_malloc" "scm_calloc" "scm_realloc"
  "scm_strdup" "scm_strndup" "scm_gc_register_allocation")
(defvarargs "scm_list_n" :scm ())
(defvarargs "scm_call" :scm (:scm))
(defvarargs "scm_c_export" :void (:string))

;;; ------------------------------------------------------------------
;;; Initialization and evaluation

(defc "scm_init_guile" :void ())
(defc "scm_with_guile" :ptr (:ptr :ptr))
(defc "scm_boot_guile" :void (:int :ptr :ptr :ptr) :noreturn t)
(defc "scm_shell" :void (:int :ptr) :noreturn t)
(defc "scm_c_eval_string" :scm (:string))
(defc "scm_c_eval_string_in_module" :scm (:string :scm))
(defc "scm_eval_string" :scm (:scm))
(defc "scm_eval_string_in_module" :scm (:scm :scm))
(defc "scm_c_primitive_load" :scm (:string))
(defc "scm_c_primitive_load_path" :scm (:string))
(defc "scm_c_use_module" :void (:string))
(defc "scm_c_resolve_module" :scm (:string))
(defc "scm_c_lookup" :scm (:string))
(defc "scm_c_module_lookup" :scm (:scm :string))
(defc "scm_c_public_lookup" :scm (:string :string))
(defc "scm_c_private_lookup" :scm (:string :string))
(defc "scm_c_public_ref" :scm (:string :string))
(defc "scm_c_private_ref" :scm (:string :string))
(defc "scm_c_public_variable" :scm (:string :string))
(defc "scm_c_private_variable" :scm (:string :string))
(defc "scm_c_define" :scm (:string :scm))
(defc "scm_c_module_define" :scm (:scm :string :scm))
(defc "scm_c_define_gsubr" :scm (:string :int :int :int :ptr))
(defc "scm_c_make_gsubr" :scm (:string :int :int :int :ptr))
(defc "scm_c_define_module" :scm (:string :ptr :ptr))
(defc "scm_c_call_with_current_module" :scm (:scm :ptr :ptr))
(defc "scm_current_module" :scm ())
(defc "scm_set_current_module" :scm (:scm))
(defc "scm_c_value_ref" :scm (:scm :size))
(defc "scm_c_nvalues" :size (:scm))
(defc "scm_c_values" :scm (:ptr :size))

;;; Calling procedures
(defc "scm_call_0" :scm (:scm))
(defc "scm_call_1" :scm (:scm :scm))
(defc "scm_call_2" :scm (:scm :scm :scm))
(defc "scm_call_3" :scm (:scm :scm :scm :scm))
(defc "scm_call_4" :scm (:scm :scm :scm :scm :scm))
(defc "scm_call_5" :scm (:scm :scm :scm :scm :scm :scm))
(defc "scm_call_6" :scm (:scm :scm :scm :scm :scm :scm :scm))
(defc "scm_call_7" :scm (:scm :scm :scm :scm :scm :scm :scm :scm))
(defc "scm_call_8" :scm (:scm :scm :scm :scm :scm :scm :scm :scm :scm))
(defc "scm_call_9" :scm (:scm :scm :scm :scm :scm :scm :scm :scm :scm :scm))
(defc "scm_call_n" :scm (:scm :ptr :size))
(defc "scm_apply_0" :scm (:scm :scm))
(defc "scm_apply_1" :scm (:scm :scm :scm))
(defc "scm_apply_2" :scm (:scm :scm :scm :scm))
(defc "scm_apply_3" :scm (:scm :scm :scm :scm :scm))

;;; ------------------------------------------------------------------
;;; Conversions between C and Scheme values

(defc "scm_is_true" :int (:scm) :shallow t)
(defc "scm_is_false" :int (:scm) :shallow t)
(defc "scm_is_bool" :int (:scm) :shallow t)
(defc "scm_to_bool" :int (:scm) :shallow t)
(defc "scm_from_bool" :scm (:int))
(defc "scm_is_eq" :int (:scm :scm) :shallow t)
(defc "scm_is_null" :int (:scm) :shallow t)
(defc "scm_is_pair" :int (:scm) :shallow t)
(defc "scm_is_symbol" :int (:scm) :shallow t)
(defc "scm_is_keyword" :int (:scm) :shallow t)
(defc "scm_is_string" :int (:scm) :shallow t)
(defc "scm_is_number" :int (:scm) :shallow t)
(defc "scm_is_integer" :int (:scm) :shallow t)
(defc "scm_is_exact_integer" :int (:scm) :shallow t)
(defc "scm_is_rational" :int (:scm) :shallow t)
(defc "scm_is_real" :int (:scm) :shallow t)
(defc "scm_is_complex" :int (:scm) :shallow t)
(defc "scm_is_exact" :int (:scm) :shallow t)
(defc "scm_is_inexact" :int (:scm) :shallow t)
(defc "scm_is_vector" :int (:scm) :shallow t)
(defc "scm_is_simple_vector" :int (:scm) :shallow t)
(defc "scm_is_bytevector" :int (:scm) :shallow t)
(defc "scm_is_signed_integer" :int (:scm :intmax :intmax) :shallow t)
(defc "scm_is_unsigned_integer" :int (:scm :uintmax :uintmax) :shallow t)

(defc "scm_to_int8" :int8 (:scm) :shallow t)
(defc "scm_to_uint8" :uint8 (:scm) :shallow t)
(defc "scm_to_int16" :int16 (:scm) :shallow t)
(defc "scm_to_uint16" :uint16 (:scm) :shallow t)
(defc "scm_to_int32" :int32 (:scm) :shallow t)
(defc "scm_to_uint32" :uint32 (:scm) :shallow t)
(defc "scm_to_int64" :int64 (:scm) :shallow t)
(defc "scm_to_uint64" :uint64 (:scm) :shallow t)
(defc "scm_to_intmax" :intmax (:scm) :shallow t)
(defc "scm_to_uintmax" :uintmax (:scm) :shallow t)
(defc "scm_to_signed_integer" :intmax (:scm :intmax :intmax) :shallow t)
(defc "scm_to_unsigned_integer" :uintmax (:scm :uintmax :uintmax) :shallow t)
(defc "scm_to_double" :double (:scm) :shallow t)
(defc "scm_from_int8" :scm (:int8))
(defc "scm_from_uint8" :scm (:uint8))
(defc "scm_from_int16" :scm (:int16))
(defc "scm_from_uint16" :scm (:uint16))
(defc "scm_from_int32" :scm (:int32))
(defc "scm_from_uint32" :scm (:uint32))
(defc "scm_from_int64" :scm (:int64))
(defc "scm_from_uint64" :scm (:uint64))
(defc "scm_from_intmax" :scm (:intmax))
(defc "scm_from_uintmax" :scm (:uintmax))
(defc "scm_from_double" :scm (:double))
(defc "scm_c_make_rectangular" :scm (:double :double))
(defc "scm_c_real_part" :double (:scm) :shallow t)
(defc "scm_c_imag_part" :double (:scm) :shallow t)

(defc "scm_from_utf8_string" :scm (:string))
(defc "scm_from_utf8_stringn" :scm (:ptr :size))
(defc "scm_from_latin1_string" :scm (:string))
(defc "scm_from_latin1_stringn" :scm (:ptr :size))
(defc "scm_from_locale_string" :scm (:string))
(defc "scm_from_locale_stringn" :scm (:ptr :size))
(defc "scm_from_stringn" :scm (:ptr :size :string :int))
(defc "scm_from_utf32_string" :scm (:ptr))
(defc "scm_from_utf32_stringn" :scm (:ptr :size))
(defc "scm_to_utf8_string" :ptr (:scm) :shallow t)
(defc "scm_to_utf8_stringn" :ptr (:scm :ptr) :shallow t)
(defc "scm_to_latin1_string" :ptr (:scm) :shallow t)
(defc "scm_to_latin1_stringn" :ptr (:scm :ptr) :shallow t)
(defc "scm_to_locale_string" :ptr (:scm) :shallow t)
(defc "scm_to_locale_stringn" :ptr (:scm :ptr) :shallow t)
(defc "scm_to_stringn" :ptr (:scm :ptr :string :int) :shallow t)
(defc "scm_to_locale_stringbuf" :size (:scm :ptr :size) :shallow t)
(defc "scm_to_utf32_string" :ptr (:scm) :shallow t)
(defc "scm_to_utf32_stringn" :ptr (:scm :ptr) :shallow t)
(defc "scm_c_string_length" :size (:scm) :shallow t)
(defc "scm_c_string_ref" :scm (:scm :size) :shallow t)
(defc "scm_c_string_set_x" :void (:scm :size :scm) :shallow t)
(defc "scm_c_make_string" :scm (:size :scm))
(defc "scm_c_substring" :scm (:scm :size :size) :shallow t)
(defc "scm_c_symbol_length" :size (:scm) :shallow t)

(defc "scm_from_utf8_symbol" :scm (:string))
(defc "scm_from_utf8_symboln" :scm (:ptr :size))
(defc "scm_from_latin1_symbol" :scm (:string))
(defc "scm_from_latin1_symboln" :scm (:ptr :size))
(defc "scm_from_locale_symbol" :scm (:string))
(defc "scm_from_locale_symboln" :scm (:ptr :size))
(defc "scm_from_utf8_keyword" :scm (:string))
(defc "scm_from_latin1_keyword" :scm (:string))
(defc "scm_from_locale_keyword" :scm (:string))

(defc "scm_c_make_vector" :scm (:size :scm))
(defc "scm_c_vector_length" :size (:scm) :shallow t)
(defc "scm_c_vector_ref" :scm (:scm :size) :shallow t)
(defc "scm_c_vector_set_x" :void (:scm :size :scm) :shallow t)
(defc "scm_c_make_bytevector" :scm (:size))
(defc "scm_c_bytevector_length" :size (:scm) :shallow t)
(defc "scm_c_bytevector_ref" :uint8 (:scm :size) :shallow t)
(defc "scm_c_bytevector_set_x" :void (:scm :size :uint8) :shallow t)
(defc "scm_c_take_bytevector" :scm (:ptr :size :scm))
(defc "scm_c_make_char" :scm (:wchar))
(defc "scm_c_list_length" :long (:scm) :shallow t)
(defc "scm_ilength" :long (:scm) :shallow t)

;;; Pairs and lists (also inline in Guile's headers, for C compiled
;;; with them; these are for C that calls them)
(defc "scm_cons" :scm (:scm :scm))
(defc "scm_car" :scm (:scm) :shallow t)
(defc "scm_cdr" :scm (:scm) :shallow t)
(defc "scm_list_1" :scm (:scm))
(defc "scm_list_2" :scm (:scm :scm))
(defc "scm_list_3" :scm (:scm :scm :scm))
(defc "scm_list_4" :scm (:scm :scm :scm :scm))
(defc "scm_list_5" :scm (:scm :scm :scm :scm :scm))

;;; Hash tables
(defc "scm_c_make_hash_table" :scm (:ulong))

;;; Variables
(defc "scm_variable_ref" :scm (:scm) :shallow t)
(defc "scm_variable_set_x" :scm (:scm :scm))
(defc "scm_make_variable" :scm (:scm))
(defc "scm_variable_bound_p" :scm (:scm) :shallow t)

;;; ------------------------------------------------------------------
;;; Errors and non-local exits

(defc "scm_error" :void (:scm :string :string :scm :scm) :noreturn t)
(defc "scm_error_scm" :scm (:scm :scm :scm :scm :scm) :noreturn t)
(defc "scm_misc_error" :void (:string :string :scm) :noreturn t)
(defc "scm_wrong_type_arg" :void (:string :int :scm) :noreturn t)
(defc "scm_wrong_type_arg_msg" :void (:string :int :scm :string) :noreturn t)
(defc "scm_wrong_num_args" :void (:scm) :noreturn t)
(defc "scm_out_of_range" :void (:string :scm) :noreturn t)
(defc "scm_out_of_range_pos" :void (:string :scm :scm) :noreturn t)
(defc "scm_memory_error" :void (:string) :noreturn t)
(defc "scm_syserror" :void (:string) :noreturn t)
(defc "scm_syserror_msg" :void (:string :string :scm :int) :noreturn t)
(defc "scm_num_overflow" :void (:string) :noreturn t)
(defc "scm_throw" :scm (:scm :scm) :noreturn t)
(defc "scm_ithrow" :scm (:scm :scm :int) :noreturn t)
(defc "scm_c_catch" :scm (:scm :ptr :ptr :ptr :ptr :ptr :ptr))
(defc "scm_internal_catch" :scm (:scm :ptr :ptr :ptr :ptr))
(defc "scm_c_with_throw_handler" :scm (:scm :ptr :ptr :ptr :ptr :int))
(defc "scm_internal_dynamic_wind" :scm (:ptr :ptr :ptr :ptr :ptr))

;;; Dynamic extents (scm_dynwind_begin and friends)
(defc "scm_dynwind_begin" :void (:int))
(defc "scm_dynwind_end" :void ())
(defc "scm_dynwind_unwind_handler" :void (:ptr :ptr :int))
(defc "scm_dynwind_rewind_handler" :void (:ptr :ptr :int))
(defc "scm_dynwind_unwind_handler_with_scm" :void (:ptr :scm :int))
(defc "scm_dynwind_rewind_handler_with_scm" :void (:ptr :scm :int))
(defc "scm_dynwind_free" :void (:ptr))

;;; ------------------------------------------------------------------
;;; Memory and the collector

(defc "scm_gc_protect_object" :scm (:scm))
(defc "scm_gc_unprotect_object" :scm (:scm))
(defc "scm_permanent_object" :scm (:scm))
(defc "scm_remember_upto_here_1" :void (:scm))
(defc "scm_remember_upto_here_2" :void (:scm :scm))
(defc "scm_gc" :scm ())

;;; ------------------------------------------------------------------
;;; Foreign objects and smobs

(defc "scm_make_foreign_object_type" :scm (:scm :scm :ptr))
(defc "scm_assert_foreign_object_type" :void (:scm :scm) :shallow t)
(defc "scm_make_foreign_object_0" :scm (:scm))
(defc "scm_make_foreign_object_1" :scm (:scm :ptr))
(defc "scm_make_foreign_object_2" :scm (:scm :ptr :ptr))
(defc "scm_make_foreign_object_3" :scm (:scm :ptr :ptr :ptr))
(defc "scm_make_foreign_object_n" :scm (:scm :size :ptr))
(defc "scm_foreign_object_ref" :ptr (:scm :size) :shallow t)
(defc "scm_foreign_object_set_x" :void (:scm :size :ptr) :shallow t)
(defc "scm_foreign_object_signed_ref" :intptr (:scm :size) :shallow t)
(defc "scm_foreign_object_unsigned_ref" :uintptr (:scm :size) :shallow t)
(defc "scm_foreign_object_signed_set_x" :void (:scm :size :intptr) :shallow t)
(defc "scm_foreign_object_unsigned_set_x" :void (:scm :size :uintptr) :shallow t)

(defc "scm_make_smob_type" :uintptr (:string :size))
(defc "scm_set_smob_mark" :void (:uintptr :ptr))
(defc "scm_set_smob_free" :void (:uintptr :ptr))
(defc "scm_set_smob_print" :void (:uintptr :ptr))
(defc "scm_set_smob_equalp" :void (:uintptr :ptr))
(defc "scm_set_smob_apply" :void (:uintptr :ptr :uint :uint :uint))
(defc "scm_i_new_smob" :scm (:uintptr :uintptr))
(defc "scm_i_new_double_smob" :scm (:uintptr :uintptr :uintptr :uintptr))
(defc "scm_make_smob" :scm (:uintptr))
(defc "scm_assert_smob_type" :void (:uintptr :scm) :shallow t)

;;; Pointers
(defc "scm_from_pointer" :scm (:ptr :ptr))
(defc "scm_to_pointer" :ptr (:scm) :shallow t)

;;; Port types written in C
(defc "scm_make_port_type" :ptr (:string :ptr :ptr))
(defc "scm_c_make_port" :scm (:ptr :ulong :uintptr))
(defc "scm_c_make_port_with_encoding" :scm (:ptr :ulong :scm :scm :uintptr))
(defc "scm_set_port_close" :void (:ptr :ptr))
(defc "scm_set_port_needs_close_on_gc" :void (:ptr :int))
(defc "scm_set_port_print" :void (:ptr :ptr))
(defc "scm_set_port_seek" :void (:ptr :ptr))
(defc "scm_set_port_truncate" :void (:ptr :ptr))
(defc "scm_set_port_input_waiting" :void (:ptr :ptr))
(defc "scm_set_port_read_wait_fd" :void (:ptr :ptr))
(defc "scm_set_port_write_wait_fd" :void (:ptr :ptr))
(defc "scm_set_port_random_access_p" :void (:ptr :ptr))
(defc "scm_set_port_get_natural_buffer_sizes" :void (:ptr :ptr))
(defc "scm_c_read" :size (:scm :ptr :size))
(defc "scm_c_read_bytes" :size (:scm :scm :size :size))
(defc "scm_c_write_bytes" :void (:scm :scm :size :size))

;;; Ports, as far as C writes to them
(defc "scm_current_output_port" :scm ())
(defc "scm_current_input_port" :scm ())
(defc "scm_current_error_port" :scm ())
(defc "scm_puts" :void (:string :scm) :shallow t)
(defc "scm_c_write" :void (:scm :ptr :size) :shallow t)
(defc "scm_putc" :void (:char :scm) :shallow t)
(defc "scm_display" :scm (:scm :scm))
(defc "scm_write" :scm (:scm :scm))
(defc "scm_newline" :scm (:scm))
(defc "scm_simple_format" :scm (:scm :scm :scm))
(defc "scm_force_output" :scm (:scm))

;;; ------------------------------------------------------------------
;;; The table's signature: an FNV-1a hash of each entry's name and
;;; types, in order, which api.c records and the Lisp side checks.

(defun table-signature ()
  (let ((h #xcbf29ce484222325))
    (loop for api across *api*
	  do (loop for c across (with-standard-io-syntax
				  (format nil "~A ~S ~S;" (api-name api) (api-return api) (api-args api)))
		   do (setq h (ldb (byte 64 0) (* (logxor h (char-code c)) #x100000001b3)))))
    (format nil "~16,'0X" h)))
