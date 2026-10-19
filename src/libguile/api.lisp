; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-LIBGUILE -*-

;;;; The DEFC entries' implementations (table.lisp, docs/libguile.md).
;;;; Mostly they call Guile mode's procedures, as libguile's C would
;;;; call its own.

(in-package "QUASISCHEME-LIBGUILE")

(defvar *procedures* (make-hash-table :test 'equal))

(defun scm (name &rest args)
  "Call Guile's procedure NAME."
  (apply (the function (or (gethash name *procedures*)
			   (setf (gethash name *procedures*) (guile-procedure name))))
	 args))

(defun sym (name) (funcall (pg ssym) name))
(defun truthy (x) (not (eq x qs:false)))
(defun bool (x) (if x qs:true qs:false))

(defun guile-error (key subr message args &optional (rest qs:false))
  (funcall (pg guile-error) (sym key) (if subr subr qs:false) message args rest))

(defun c-string-or-false (address) (if (zerop address) qs:false (c-string address)))

(defun current-module () (scm "current-module"))

;;; ------------------------------------------------------------------
;;; Initialization and evaluation

;; Guile mode is up whenever this library is loaded into it.
(defimpl "scm_init_guile" () nil)

(defimpl "scm_with_guile" (fptr data)
  (c-funcall (sb-alien:sap-alien (address-sap fptr) (function sb-alien:unsigned-long sb-alien:unsigned-long))
			  data))

(defun c-argv (argc argv)
  (loop for i below argc collect (c-string (word argv i))))

(defun exit-from-c (status)
  "Leave the program as C's exit(STATUS) does, after Guile's ports are
flushed."
  (qsg::flush-all-ports)
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (c-call "exit" sb-alien:void sb-alien:int status))

;; Guile's own start: the C main_func with the arguments, then an exit
(defimpl "scm_boot_guile" (argc argv fptr closure)
  (setq qsg:*program-arguments* (c-argv argc argv))
  (c-funcall
   (sb-alien:sap-alien (address-sap fptr)
		       (function sb-alien:void sb-alien:unsigned-long sb-alien:int sb-alien:unsigned-long))
   closure argc argv)
  (exit-from-c 0))

;; The REPL, Guile's command line's when there are no switches
(defimpl "scm_shell" (argc argv)
  (setq qsg:*program-arguments* (c-argv argc argv))
  (scm "eval-string" "((@ (ice-9 top-repl) top-repl))")
  (exit-from-c 0))

(defimpl "scm_c_eval_string" (address) (scm "eval-string" (c-string address)))
(defimpl "scm_c_eval_string_in_module" (address module)
  (scm "eval-string" (c-string address) (list module)))
(defimpl "scm_eval_string" (s) (scm "eval-string" s))
(defimpl "scm_eval_string_in_module" (s module) (scm "eval-string" s module))
(defimpl "scm_c_primitive_load" (address) (scm "primitive-load" (c-string address)))
(defimpl "scm_c_primitive_load_path" (address) (scm "primitive-load-path" (c-string address)))

(defun module-name-list (address)
  "\"ice-9 match\" as (ice-9 match)."
  (mapcar #'sym (uiop:split-string (c-string address) :separator " ")))

(defimpl "scm_c_resolve_module" (address) (scm "resolve-module" (module-name-list address)))
(defimpl "scm_c_use_module" (address)
  (scm "module-use!" (current-module) (scm "resolve-interface" (module-name-list address))))
(defimpl "scm_current_module" () (current-module))
(defimpl "scm_set_current_module" (m)
  (prog1 (current-module) (scm "set-current-module" m)))

(defun lookup (module name)
  (let ((v (scm "module-variable" module (sym name))))
    (if (and (truthy v) (truthy (scm "variable-bound?" v)))
	v
	(guile-error "unbound-variable" "scm_c_lookup" "Unbound variable: ~S" (list (sym name))))))

(defimpl "scm_c_lookup" (address) (lookup (current-module) (c-string address)))
(defimpl "scm_c_module_lookup" (module address) (lookup module (c-string address)))
(defun module-ref-in (module-address name-address public)
  (let ((m (scm "resolve-module" (module-name-list module-address))))
    (lookup (if public (scm "resolve-interface" (scm "module-name" m)) m) (c-string name-address))))
(defimpl "scm_c_public_lookup" (m n) (module-ref-in m n t))
(defimpl "scm_c_private_lookup" (m n) (module-ref-in m n nil))
(defimpl "scm_c_public_variable" (m n) (module-ref-in m n t))
(defimpl "scm_c_private_variable" (m n) (module-ref-in m n nil))
(defimpl "scm_c_public_ref" (m n) (scm "variable-ref" (module-ref-in m n t)))
(defimpl "scm_c_private_ref" (m n) (scm "variable-ref" (module-ref-in m n nil)))

(defun define-variable (module name value)
  (scm "module-define!" module (sym name) value)
  (scm "module-variable" module (sym name)))

(defimpl "scm_c_define" (address value) (define-variable (current-module) (c-string address) value))
(defimpl "scm_c_module_define" (module address value) (define-variable module (c-string address) value))

(defimpl "scm_c_make_gsubr" (name req opt rest fptr)
  (make-subr (c-string name) req opt (/= rest 0) fptr))
(defimpl "scm_c_define_gsubr" (name req opt rest fptr)
  (let ((f (make-subr (c-string name) req opt (/= rest 0) fptr)))
    (when *trace-calls*
      (format *error-output* ";;   ~A in ~S~%" (c-string name) (scm "module-name" (current-module))))
    (scm "module-define!" (current-module) (sym (c-string name)) f)
    f))

(defimpl "scm_c_define_module" (name init data)
  ;; as define-module does: the module has a public interface, which
  ;; scm_c_export adds to
  (let ((m (let ((old (current-module)))
	     (unwind-protect (scm "define-module*" (module-name-list name))
	       (scm "set-current-module" old)))))
    (unless (zerop init)
      (call-with-module m (lambda ()
			    (c-funcall
			     (sb-alien:sap-alien (address-sap init) (function sb-alien:void sb-alien:unsigned-long))
			     data))))
    m))

(defun call-with-module (m thunk)
  (let ((old (current-module)))
    (scm "set-current-module" m)
    (unwind-protect (funcall thunk) (scm "set-current-module" old))))

(defimpl "scm_c_call_with_current_module" (m fptr data)
  (call-with-module m (lambda ()
			(import-scm (c-funcall
				     (sb-alien:sap-alien (address-sap fptr) (function sb-alien:unsigned-long sb-alien:unsigned-long))
				     data)))))

(defimpl "scm_c_values" (base n)
  (apply #'values (loop for i below n collect (import-scm (word base i)))))

;;; ------------------------------------------------------------------
;;; Calling procedures

(macrolet ((calls ()
	     `(progn
		,@(loop for n from 0 to 9
			collect (let ((args (loop for i below n collect (intern (format nil "A~D" i)))))
				  `(defimpl ,(format nil "scm_call_~D" n) (f ,@args) (funcall f ,@args)))))))
  (calls))

(defimpl "scm_call_n" (f argv n)
  (apply f (loop for i below n collect (import-scm (word argv i)))))
(defimpl "scm_apply_0" (f args) (apply f args))
(defimpl "scm_apply_1" (f a args) (apply f a args))
(defimpl "scm_apply_2" (f a b args) (apply f a b args))
(defimpl "scm_apply_3" (f a b c args) (apply f a b c args))

;;; ------------------------------------------------------------------
;;; Predicates and conversions

(defimpl "scm_is_true" (x) (truthy x))
(defimpl "scm_is_false" (x) (not (truthy x)))
(defimpl "scm_is_bool" (x) (or (eq x qs:true) (eq x qs:false)))
(defimpl "scm_to_bool" (x)
  (cond ((eq x qs:false) nil) ((eq x qs:true) t)
	(t (guile-error "wrong-type-arg" "scm_to_bool" "Wrong type argument: ~S" (list x) (list x)))))
(defimpl "scm_from_bool" (i) (bool (/= i 0)))
(defimpl "scm_is_eq" (a b) (eq a b))
(defimpl "scm_is_null" (x) (null x))
(defimpl "scm_is_pair" (x) (consp x))
(defimpl "scm_is_symbol" (x) (truthy (scm "symbol?" x)))
(defimpl "scm_is_keyword" (x) (truthy (scm "keyword?" x)))
(defimpl "scm_is_string" (x) (stringp x))
(defimpl "scm_is_number" (x) (numberp x))
(defimpl "scm_is_integer" (x) (truthy (scm "integer?" x)))
(defimpl "scm_is_exact_integer" (x) (integerp x))
(defimpl "scm_is_rational" (x) (truthy (scm "rational?" x)))
(defimpl "scm_is_real" (x) (realp x))
(defimpl "scm_is_complex" (x) (numberp x))
(defimpl "scm_is_exact" (x) (truthy (scm "exact?" x)))
(defimpl "scm_is_inexact" (x) (truthy (scm "inexact?" x)))
(defimpl "scm_is_vector" (x) (simple-vector-p x))
(defimpl "scm_is_simple_vector" (x) (simple-vector-p x))
(defimpl "scm_is_bytevector" (x) (typep x 'qs-r6rs::octets))
(defimpl "scm_is_signed_integer" (x min max) (and (integerp x) (<= min x max)))
(defimpl "scm_is_unsigned_integer" (x min max) (and (integerp x) (<= min x max)))

(defun to-integer (x min max who)
  (unless (integerp x)
    (if (and (floatp x) (= x (ffloor x)))
	(setq x (truncate x))
	(guile-error "wrong-type-arg" who "Wrong type argument in position ~A: ~S" (list 1 x) (list x))))
  (unless (<= min x max)
    (guile-error "out-of-range" who "Value out of range ~S to ~S: ~S" (list min max x) (list x)))
  x)

(macrolet ((conversions (&rest specs)
	     `(progn
		,@(loop for (c bits signed) in specs
			for lo = (if signed (- (ash 1 (1- bits))) 0)
			for hi = (if signed (1- (ash 1 (1- bits))) (1- (ash 1 bits)))
			collect `(defimpl ,(format nil "scm_to_~A" c) (x) (to-integer x ,lo ,hi ,(format nil "scm_to_~A" c)))
			collect `(defimpl ,(format nil "scm_from_~A" c) (n) n)))))
  (conversions ("int8" 8 t) ("uint8" 8 nil) ("int16" 16 t) ("uint16" 16 nil)
	       ("int32" 32 t) ("uint32" 32 nil) ("int64" 64 t) ("uint64" 64 nil)
	       ("intmax" 64 t) ("uintmax" 64 nil)))

(defimpl "scm_to_signed_integer" (x min max) (to-integer x min max "scm_to_signed_integer"))
(defimpl "scm_to_unsigned_integer" (x min max) (to-integer x min max "scm_to_unsigned_integer"))
(defimpl "scm_to_double" (x)
  (if (realp x)
      (coerce x 'double-float)
      (guile-error "wrong-type-arg" "scm_to_double" "Wrong type argument: ~S" (list x) (list x))))
(defimpl "scm_from_double" (d) d)
(defimpl "scm_c_make_rectangular" (re im) (if (zerop im) re (complex re im)))
(defimpl "scm_c_real_part" (z) (coerce (realpart z) 'double-float))
(defimpl "scm_c_imag_part" (z) (coerce (imagpart z) 'double-float))

;;; Strings
(defimpl "scm_from_utf8_string" (a) (c-string a))
(defimpl "scm_from_utf8_stringn" (a n) (c-string a (if (= n (ldb (byte 64 0) -1)) nil n)))
(defimpl "scm_from_locale_string" (a) (c-string a))
(defimpl "scm_from_locale_stringn" (a n) (c-string a (if (= n (ldb (byte 64 0) -1)) nil n)))
(defimpl "scm_from_latin1_string" (a)
  (latin1-string a (loop for i from 0 when (zerop (sb-sys:sap-ref-8 (address-sap a) i)) return i)))
(defimpl "scm_from_latin1_stringn" (a n)
  (if (= n (ldb (byte 64 0) -1))
      (latin1-string a (loop for i from 0 when (zerop (sb-sys:sap-ref-8 (address-sap a) i)) return i))
      (latin1-string a n)))
(defimpl "scm_from_stringn" (a n encoding handler)
  (declare (ignore handler))
  (let ((e (string-downcase (c-string encoding))))
    (if (member e '("iso-8859-1" "latin1" "latin-1") :test #'string=)
	(latin1-string a n)
	(c-string a n))))

(defun string-octets (s encoding)
  (sb-ext:string-to-octets s :external-format encoding))

(defun to-string-check (s who)
  (unless (stringp s)
    (guile-error "wrong-type-arg" who "Wrong type argument: ~S" (list s) (list s))))

(defun store-length (address n) (unless (zerop address) (setf (word address 0) n)))

(defimpl "scm_to_utf8_string" (s) (to-string-check s "scm_to_utf8_string") (malloc-octets (string-octets s :utf-8)))
(defimpl "scm_to_locale_string" (s) (to-string-check s "scm_to_locale_string") (malloc-octets (string-octets s :utf-8)))
(defimpl "scm_to_latin1_string" (s) (to-string-check s "scm_to_latin1_string")
  (malloc-octets (string-octets s '(:latin-1 :replacement #\?))))
(defimpl "scm_to_utf8_stringn" (s lenp)
  (to-string-check s "scm_to_utf8_stringn")
  (let ((o (string-octets s :utf-8))) (store-length lenp (length o)) (malloc-octets o (zerop lenp))))
(defimpl "scm_to_locale_stringn" (s lenp)
  (to-string-check s "scm_to_locale_stringn")
  (let ((o (string-octets s :utf-8))) (store-length lenp (length o)) (malloc-octets o (zerop lenp))))
(defimpl "scm_to_latin1_stringn" (s lenp)
  (to-string-check s "scm_to_latin1_stringn")
  (let ((o (string-octets s '(:latin-1 :replacement #\?)))) (store-length lenp (length o)) (malloc-octets o (zerop lenp))))
(defimpl "scm_to_stringn" (s lenp encoding handler)
  (declare (ignore handler))
  (let* ((e (string-downcase (c-string encoding)))
	 (o (string-octets s (if (member e '("iso-8859-1" "latin1") :test #'string=) '(:latin-1 :replacement #\?) :utf-8))))
    (store-length lenp (length o))
    (malloc-octets o (zerop lenp))))
(defimpl "scm_to_locale_stringbuf" (s buf max)
  (let ((o (string-octets s :utf-8)))
    (dotimes (i (min max (length o))) (setf (sb-sys:sap-ref-8 (address-sap buf) i) (aref o i)))
    (length o)))
(defimpl "scm_c_string_length" (s) (to-string-check s "scm_c_string_length") (length s))
(defimpl "scm_c_string_ref" (s i) (char s i))
(defimpl "scm_c_string_set_x" (s i c) (setf (char s i) c) nil)
(defimpl "scm_c_make_string" (n c) (make-string n :initial-element (if (characterp c) c #\Space)))
(defimpl "scm_c_substring" (s start end) (subseq s start end))
(defimpl "scm_c_symbol_length" (s) (length (qs:scheme-symbol-name s)))

;;; Symbols and keywords
(defimpl "scm_from_utf8_symbol" (a) (sym (c-string a)))
(defimpl "scm_from_utf8_symboln" (a n) (sym (c-string a (if (= n (ldb (byte 64 0) -1)) nil n))))
(defimpl "scm_from_locale_symbol" (a) (sym (c-string a)))
(defimpl "scm_from_locale_symboln" (a n) (sym (c-string a (if (= n (ldb (byte 64 0) -1)) nil n))))
(defimpl "scm_from_latin1_symbol" (a) (sym (c-string a)))
(defimpl "scm_from_latin1_symboln" (a n) (sym (latin1-string a n)))
(defimpl "scm_from_utf8_keyword" (a) (scm "symbol->keyword" (sym (c-string a))))
(defimpl "scm_from_locale_keyword" (a) (scm "symbol->keyword" (sym (c-string a))))
(defimpl "scm_from_latin1_keyword" (a) (scm "symbol->keyword" (sym (c-string a))))

;;; Vectors and bytevectors
(defimpl "scm_c_make_vector" (n fill) (make-array n :initial-element fill))
(defimpl "scm_c_vector_length" (v) (length v))
(defimpl "scm_c_vector_ref" (v i) (svref v i))
(defimpl "scm_c_vector_set_x" (v i x) (setf (svref v i) x) nil)
(defimpl "scm_c_make_bytevector" (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defimpl "scm_c_bytevector_length" (bv) (length bv))
(defimpl "scm_c_bytevector_ref" (bv i) (aref bv i))
(defimpl "scm_c_bytevector_set_x" (bv i b) (setf (aref bv i) b) nil)
(defimpl "scm_c_take_bytevector" (p n parent)
  (declare (ignore parent))
  (let ((bv (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n bv) (setf (aref bv i) (sb-sys:sap-ref-8 (address-sap p) i)))))
(defimpl "scm_c_make_char" (c) (code-char c))
(defimpl "scm_c_list_length" (l) (or (ignore-errors (list-length l)) -1))
(defimpl "scm_ilength" (l) (or (and (listp l) (ignore-errors (list-length l))) -1))

;;; Pairs
(defimpl "scm_cons" (a b) (cons a b))
(defimpl "scm_car" (p) (if (consp p) (car p) (guile-error "wrong-type-arg" "car" "Wrong type argument in position ~A (expecting pair): ~S" (list 1 p) (list p))))
(defimpl "scm_cdr" (p) (if (consp p) (cdr p) (guile-error "wrong-type-arg" "cdr" "Wrong type argument in position ~A (expecting pair): ~S" (list 1 p) (list p))))
(defimpl "scm_list_1" (a) (list a))
(defimpl "scm_list_2" (a b) (list a b))
(defimpl "scm_list_3" (a b c) (list a b c))
(defimpl "scm_list_4" (a b c d) (list a b c d))
(defimpl "scm_list_5" (a b c d e) (list a b c d e))

;;; Hash tables
(defimpl "scm_c_make_hash_table" (n) (scm "make-hash-table" n))

;;; Variables
(defimpl "scm_variable_ref" (v) (scm "variable-ref" v))
(defimpl "scm_variable_set_x" (v x) (scm "variable-set!" v x))
(defimpl "scm_make_variable" (x) (scm "make-variable" x))
(defimpl "scm_variable_bound_p" (v) (scm "variable-bound?" v))

;;; ------------------------------------------------------------------
;;; Errors

(defimpl "scm_error" (key subr message args rest)
  (funcall (pg guile-error) key (c-string-or-false subr) (c-string-or-false message) args rest))
(defimpl "scm_error_scm" (key subr message args rest)
  (funcall (pg guile-error) key subr message args rest))
(defimpl "scm_misc_error" (subr message args)
  (guile-error "misc-error" (c-string-or-false subr) (c-string-or-false message) args))
(defimpl "scm_wrong_type_arg" (subr pos x)
  (guile-error "wrong-type-arg" (c-string-or-false subr)
	       (if (zerop pos) "Wrong type argument: ~S" "Wrong type argument in position ~A: ~S")
	       (if (zerop pos) (list x) (list pos x)) (list x)))
(defimpl "scm_wrong_type_arg_msg" (subr pos x expected)
  (guile-error "wrong-type-arg" (c-string-or-false subr)
	       "Wrong type argument in position ~A (expecting ~A): ~S"
	       (list pos (c-string-or-false expected) x) (list x)))
(defimpl "scm_wrong_num_args" (proc)
  (guile-error "wrong-number-of-args" qs:false "Wrong number of arguments to ~A" (list proc)))
(defimpl "scm_out_of_range" (subr x)
  (guile-error "out-of-range" (c-string-or-false subr) "Value out of range: ~S" (list x) (list x)))
(defimpl "scm_out_of_range_pos" (subr x pos)
  (guile-error "out-of-range" (c-string-or-false subr) "Argument ~A out of range: ~S" (list pos x) (list x)))
(defimpl "scm_memory_error" (subr) (guile-error "memory-allocation-error" (c-string-or-false subr) "Memory allocation error" '()))
(defimpl "scm_syserror" (subr)
  (let ((errno (sb-alien:extern-alien "errno" sb-alien:int)))
    (guile-error "system-error" (c-string-or-false subr) "~A" (list (sb-int:strerror errno)) (list errno))))
(defimpl "scm_syserror_msg" (subr message args errno)
  (guile-error "system-error" (c-string-or-false subr) (c-string-or-false message) args (list errno)))
(defimpl "scm_num_overflow" (subr) (guile-error "numerical-overflow" (c-string-or-false subr) "Numerical overflow" '()))
(defimpl "scm_throw" (key args) (apply #'scm "throw" key args))
(defimpl "scm_ithrow" (key args noreturn) (declare (ignore noreturn)) (apply #'scm "throw" key args))

(defun c-handler (fptr data)
  "A Scheme handler (key . args) that calls C's scm_t_catch_handler."
  (lambda (key &rest args)
    (crossing
      (import-scm (c-funcall
		   (sb-alien:sap-alien (address-sap fptr) (function sb-alien:unsigned-long sb-alien:unsigned-long
								    sb-alien:unsigned-long sb-alien:unsigned-long))
		   data (export-scm key) (export-scm args))))))

(defun c-body (fptr data)
  (lambda ()
    (crossing
      (import-scm (c-funcall
		   (sb-alien:sap-alien (address-sap fptr) (function sb-alien:unsigned-long sb-alien:unsigned-long))
		   data)))))

(defimpl "scm_c_catch" (tag body body-data handler handler-data pre pre-data)
  (if (zerop pre)
      (scm "catch" tag (c-body body body-data) (c-handler handler handler-data))
      (scm "catch" tag (c-body body body-data) (c-handler handler handler-data) (c-handler pre pre-data))))
(defimpl "scm_internal_catch" (tag body body-data handler handler-data)
  (scm "catch" tag (c-body body body-data) (c-handler handler handler-data)))
(defimpl "scm_c_with_throw_handler" (tag body body-data handler handler-data lazy)
  (declare (ignore lazy))
  (scm "with-throw-handler" tag (c-body body body-data) (c-handler handler handler-data)))

(defun c-thunk (fptr data)
  (lambda () (unless (zerop fptr)
	       (c-funcall (sb-alien:sap-alien (address-sap fptr) (function sb-alien:void sb-alien:unsigned-long))
				       data))))
(defimpl "scm_internal_dynamic_wind" (before inner after inner-data guard-data)
  (scm "dynamic-wind" (c-thunk before guard-data) (c-body inner inner-data) (c-thunk after guard-data)))

;;; Dynwind frames
(defimpl "scm_dynwind_begin" (flags) (declare (ignore flags)) (push (make-dynwind) *dynwinds*) nil)
(defimpl "scm_dynwind_end" ()
  (let ((frame (pop *dynwinds*))) (when frame (close-dynwind frame nil))) nil)
(defimpl "scm_dynwind_unwind_handler" (fptr data flags)
  (when *dynwinds* (push (list :unwind fptr data flags) (dynwind-handlers (first *dynwinds*)))) nil)
(defimpl "scm_dynwind_rewind_handler" (fptr data flags)
  (declare (ignore fptr data flags)) nil)
(defimpl "scm_dynwind_unwind_handler_with_scm" (fptr x flags)
  (when *dynwinds* (push (list :unwind fptr (export-scm x) flags) (dynwind-handlers (first *dynwinds*)))) nil)
(defimpl "scm_dynwind_rewind_handler_with_scm" (fptr x flags) (declare (ignore fptr x flags)) nil)
(defimpl "scm_dynwind_free" (p)
  (when *dynwinds*
    (push (list :unwind (sb-sys:sap-int (sb-sys:foreign-symbol-sap "free")) p +wind-explicitly+)
	  (dynwind-handlers (first *dynwinds*))))
  nil)

;;; ------------------------------------------------------------------
;;; The collector

(defvar *protected* (make-hash-table :test 'eq) "Object -> (count . root), for scm_gc_protect_object.")

;; The object's mirror, if it has one C can reach, is kept by a root cell
;; for as long as it is protected: C may hold it where the collector
;; doesn't look (memory from new or malloc)
(defun protect (x)
  (let ((cell (or (gethash x *protected*) (setf (gethash x *protected*) (cons 0 nil)))))
    (incf (car cell))
    (unless (cdr cell)
      (let ((bits (export-scm x)))
	(when (and (not (nth-value 1 (bits-immediate bits))) (heap-object-p bits))
	  (setf (cdr cell) (c-call "psg_make_root" sb-alien:unsigned-long sb-sys:system-area-pointer
				   (address-sap bits))))))
    x))

(defun unprotect (x)
  (let ((cell (gethash x *protected*)))
    (when (and cell (<= (decf (car cell)) 0))
      (when (cdr cell) (c-call "psg_free_root" sb-alien:void sb-alien:unsigned-long (cdr cell)))
      (remhash x *protected*))
    x))

(defimpl "scm_gc_protect_object" (x) (protect x))
(defimpl "scm_gc_unprotect_object" (x) (unprotect x))
(defimpl "scm_permanent_object" (x) (protect x))
(defimpl "scm_remember_upto_here_1" (x) (declare (ignore x)) nil)
(defimpl "scm_remember_upto_here_2" (x y) (declare (ignore x y)) nil)
(defimpl "scm_gc" () (sb-ext:gc :full t) (c-call "psg_collect" sb-alien:void) qs:unspecific)

;;; ------------------------------------------------------------------
;;; Smobs: the descriptors are in scm_smobs (data.c), which inline
;;; scm_new_smob reads

(defvar *smob-count* 0)

(defun smob-field (n field)
  (+ (data-address "scm_smobs") (* n +smob-descriptor-size+) (getf *smob-descriptor-offsets* field)))

(defun smob-number (tag) (ldb (byte 8 8) tag))

(defimpl "scm_make_smob_type" (name size)
  (let ((n (incf *smob-count*)))
    (setf (sb-sys:sap-ref-64 (address-sap (smob-field n :name)) 0) (malloc-octets (string-octets (c-string name) :utf-8))
	  (sb-sys:sap-ref-64 (address-sap (smob-field n :size)) 0) size)
    (logior +tc7-smob+ (ash n 8))))

(macrolet ((setter (c field)
	     `(defimpl ,c (tag fptr)
		(setf (sb-sys:sap-ref-64 (address-sap (smob-field (smob-number tag) ,field)) 0) fptr)
		nil)))
  (setter "scm_set_smob_mark" :mark)
  (setter "scm_set_smob_free" :free)
  (setter "scm_set_smob_print" :print)
  (setter "scm_set_smob_equalp" :equalp))
(defvar *smob-apply-arguments* (make-hash-table) "Smob number -> (required optional rest).")

(defimpl "scm_set_smob_apply" (tag fptr req opt rest)
  (setf (gethash (smob-number tag) *smob-apply-arguments*) (list req opt (/= rest 0)))
  (setf (sb-sys:sap-ref-64 (address-sap (smob-field (smob-number tag) :apply)) 0) fptr)
  nil)

(defun smob-applicable-p (address)
  "Whether C's object at ADDRESS is a smob whose type has an apply function."
  (and (= (logand (word address 0) #x7f) +tc7-smob+)
       (/= 0 (sb-sys:sap-ref-64 (address-sap (smob-field (smob-number (word address 0)) :apply)) 0))))

(defun apply-c-object (o args)
  "C-OBJECT O called as a procedure with ARGS: a smob with an apply function."
  (let* ((a (c-object-address o))
	 (n (and (= (logand (word a 0) #x7f) +tc7-smob+) (smob-number (word a 0))))
	 (fptr (if n (sb-sys:sap-ref-64 (address-sap (smob-field n :apply)) 0) 0)))
    (when (zerop fptr)
      (guile-error "wrong-type-arg" qs:false "Wrong type to apply: ~S" (list o) (list o)))
    (destructuring-bind (req opt rest) (gethash n *smob-apply-arguments*)
      (apply (make-subr "smob-apply" (1+ req) opt rest fptr) o args))))

(defun new-smob (words)
  (let ((a (if (zerop (sb-sys:sap-ref-64 (address-sap (smob-field (smob-number (first words)) :mark)) 0))
	       (alloc (* 8 (length words)))
	       ;; of the kind whose mark procedure calls the type's mark function (core.c)
	       (sb-sys:sap-int (c-call "psg_alloc_marked" sb-sys:system-area-pointer
				       sb-alien:unsigned-long (* 8 (length words)))))))
    (loop for w in words for i from 0 do (setf (word a i) w))
    (let ((free (sb-sys:sap-ref-64 (address-sap (smob-field (smob-number (first words)) :free)) 0)))
      (unless (zerop free)
	(c-call "psg_finalize_smob" sb-alien:void sb-sys:system-area-pointer (address-sap a))))
    (c-object a)))

(defimpl "scm_i_new_smob" (tag data) (new-smob (list tag data)))
(defimpl "scm_i_new_double_smob" (tag a b c) (new-smob (list tag a b c 0)))
(defimpl "scm_make_smob" (tag)
  (let ((size (sb-sys:sap-ref-64 (address-sap (smob-field (smob-number tag) :size)) 0)))
    (new-smob (list tag (if (zerop size) 0 (alloc size))))))

(defimpl "scm_assert_smob_type" (tag x)
  (unless (and (smob-p x) (= (logand (word (c-object-address x) 0) #xffff) tag))
    (guile-error "wrong-type-arg" qs:false "Wrong type argument: ~S" (list x) (list x)))
  nil)

(defun smob-p (o) (and (c-object-p o) (= (logand (word (c-object-address o) 0) #x7f) +tc7-smob+)))

(defun print-smob (o)
  (let* ((a (c-object-address o))
	 (n (smob-number (word a 0)))
	 (print (sb-sys:sap-ref-64 (address-sap (smob-field n :print)) 0))
	 (name (let ((p (sb-sys:sap-ref-64 (address-sap (smob-field n :name)) 0)))
		 (if (zerop p) "smob" (c-string p)))))
    (if (zerop print)
	(format nil "#<~A ~(~X~)>" name a)
	(let ((port (scm "open-output-string")))
	  (crossing
	    (c-funcall
	     (sb-alien:sap-alien (address-sap print) (function sb-alien:int sb-alien:unsigned-long
								sb-alien:unsigned-long sb-alien:unsigned-long))
	     a (export-scm port) 0))
	  (scm "get-output-string" port)))))

;;; ------------------------------------------------------------------
;;; Foreign objects: structs whose fields are words (layout letter u)

(defimpl "scm_make_foreign_object_type" (name slots finalizer)
  (declare (ignore finalizer))
  (let ((vtable (scm "make-vtable" (apply #'concatenate 'string (loop repeat (length slots) collect "uw")))))
    (scm "set-struct-vtable-name!" vtable name)
    vtable))

(defun make-foreign-object (type words)
  (apply #'scm "make-struct/no-tail" type words))

(defimpl "scm_make_foreign_object_0" (type) (make-foreign-object type '()))
(defimpl "scm_make_foreign_object_1" (type a) (make-foreign-object type (list a)))
(defimpl "scm_make_foreign_object_2" (type a b) (make-foreign-object type (list a b)))
(defimpl "scm_make_foreign_object_3" (type a b c) (make-foreign-object type (list a b c)))
(defimpl "scm_make_foreign_object_n" (type n values)
  (make-foreign-object type (loop for i below n collect (word values i))))

(defun foreign-slots (x who)
  (unless (funcall (pg struct-p) x)
    (guile-error "wrong-type-arg" who "Wrong type argument: ~S" (list x) (list x)))
  (funcall (pg struct-slots-of) x))

(defimpl "scm_assert_foreign_object_type" (type x)
  (unless (and (funcall (pg struct-p) x) (eq (funcall (pg struct-vtable-of) x) type))
    (guile-error "wrong-type-arg" qs:false "Wrong type (expecting ~A): ~S"
		 (list (scm "struct-vtable-name" type) x) (list x)))
  nil)
(defimpl "scm_foreign_object_ref" (x n) (svref (foreign-slots x "foreign-object-ref") n))
(defimpl "scm_foreign_object_signed_ref" (x n) (signed (svref (foreign-slots x "foreign-object-ref") n) 64))
(defimpl "scm_foreign_object_unsigned_ref" (x n) (svref (foreign-slots x "foreign-object-ref") n))
(defimpl "scm_foreign_object_set_x" (x n v) (setf (svref (foreign-slots x "foreign-object-set!") n) v) nil)
(defimpl "scm_foreign_object_signed_set_x" (x n v) (setf (svref (foreign-slots x "foreign-object-set!") n) (ldb (byte 64 0) v)) nil)
(defimpl "scm_foreign_object_unsigned_set_x" (x n v) (setf (svref (foreign-slots x "foreign-object-set!") n) v) nil)

;;; Pointers
(defimpl "scm_from_pointer" (p finalizer)
  (declare (ignore finalizer))
  (scm "make-pointer" p))
(defimpl "scm_to_pointer" (x) (scm "pointer-address" x))

;;; ------------------------------------------------------------------
;;; Port types written in C.  A port type is C memory (a pointer C keeps)
;;; standing for a PORT-TYPE; a port of it is an R6RS custom binary
;;; port whose read!, write! and close call the type's C functions.  Its
;;; mirror's words 1 and 3 are the stream and the type, as SCM_STREAM
;;; and SCM_PORT_TYPE read them.

(defstruct (port-type (:constructor make-port-type (name read write)))
  name read write (close 0) (print 0) (seek 0))

(defvar *port-types* (make-hash-table) "Address -> PORT-TYPE.")
(defvar *c-ports* (make-hash-table :test 'eq :weakness :key)
  "A port of a C port type -> (type-address stream mode-bits).")

(defconstant +rdng+ #x200)		; SCM_RDNG
(defconstant +wrtng+ #x400)		; SCM_WRTNG

(defimpl "scm_make_port_type" (name read write)
  (let ((a (alloc 64)))
    (setf (gethash a *port-types*) (make-port-type (c-string name) read write))
    a))

(defun port-type-at (a)
  (or (gethash a *port-types*) (error "libguile: no port type at ~X" a)))

(macrolet ((setter (c slot)
	     `(defimpl ,c (type fptr) (setf (,slot (port-type-at type)) fptr) nil)))
  (setter "scm_set_port_close" port-type-close)
  (setter "scm_set_port_print" port-type-print)
  (setter "scm_set_port_seek" port-type-seek))
(macrolet ((ignore (&rest names)
	     `(progn ,@(loop for c in names collect `(defimpl ,c (type x) (declare (ignore type x)) nil)))))
  (ignore "scm_set_port_needs_close_on_gc" "scm_set_port_truncate" "scm_set_port_input_waiting"
	  "scm_set_port_read_wait_fd" "scm_set_port_write_wait_fd" "scm_set_port_random_access_p"
	  "scm_set_port_get_natural_buffer_sizes"))

(defun call-port-io (fptr port bv start count)
  "C's read or write function: size_t f (SCM port, SCM bv, size_t start, size_t count)."
  (incf *epoch*)
  (let* ((words (list (export-scm port) (export-scm bv) start count))
	 (n (c-funcall
	     (sb-alien:sap-alien (address-sap fptr)
				 (function sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:unsigned-long
					   sb-alien:unsigned-long sb-alien:unsigned-long))
	     (first words) (second words) start count)))
    (crossing (import-scm (second words)))
    n))

(defun make-c-port (type-address mode stream)
  (let* ((type (port-type-at type-address))
	 (port nil)
	 (read! (lambda (bv start count) (call-port-io (port-type-read type) port bv start count)))
	 (write! (lambda (bv start count) (call-port-io (port-type-write type) port bv start count)))
	 (close (lambda ()
		  (unless (zerop (port-type-close type))
		    (c-funcall
		     (sb-alien:sap-alien (address-sap (port-type-close type))
					 (function sb-alien:void sb-alien:unsigned-long))
		     (export-scm port)))))
	 (seek (unless (zerop (port-type-seek type))
		 (lambda (offset whence)
		   (let ((position (c-funcall
				    (sb-alien:sap-alien (address-sap (port-type-seek type))
							(function sb-alien:long sb-alien:unsigned-long
								  sb-alien:long sb-alien:int))
				    (export-scm port) offset whence)))
		     (and (>= position 0) position)))))
	 (name (port-type-name type)))
    ;; a Guile port of Guile mode's own, bivalent (bytes and characters)
    ;; as Guile's ports are, with the C type's functions as its backend
    (setq port (qsg::make-port :kind name
			       :input (logtest mode +rdng+) :output (logtest mode +wrtng+)
			       :buffering :full :encoding "UTF-8"
			       :read (and (logtest mode +rdng+) read!)
			       :write (and (logtest mode +wrtng+) write!)
			       :seek seek
			       :close close))
    (setf (gethash port *c-ports*) (list type-address stream mode))
    port))

(defimpl "scm_c_make_port" (type mode stream) (make-c-port type mode stream))
(defimpl "scm_c_make_port_with_encoding" (type mode encoding strategy stream)
  (declare (ignore encoding strategy))
  (make-c-port type mode stream))

(defimpl "scm_c_read" (port buffer size)
  (let* ((bv (make-array size :element-type '(unsigned-byte 8)))
	 (n (scm "get-bytevector-n!" port bv 0 size)))
    (if (eq n qs:eof-object)
	0
	(progn (dotimes (i n) (setf (sb-sys:sap-ref-8 (address-sap buffer) i) (aref bv i)))
	       n))))
(defimpl "scm_c_read_bytes" (port bv start count)
  (let ((n (scm "get-bytevector-n!" port bv start count)))
    (if (eq n qs:eof-object) 0 n)))
(defimpl "scm_c_write_bytes" (port bv start count)
  (scm "put-bytevector" port bv start count)
  nil)

;;; ------------------------------------------------------------------
;;; Ports

(defimpl "scm_current_output_port" () (scm "current-output-port"))
(defimpl "scm_current_input_port" () (scm "current-input-port"))
(defimpl "scm_current_error_port" () (scm "current-error-port"))
(defimpl "scm_puts" (s port) (scm "display" (c-string s) port) nil)
(defimpl "scm_c_write" (port p n)
  (scm "put-bytevector" port (let ((bv (make-array n :element-type '(unsigned-byte 8))))
			       (dotimes (i n bv) (setf (aref bv i) (sb-sys:sap-ref-8 (address-sap p) i)))))
  nil)
(defimpl "scm_putc" (c port) (scm "write-char" (code-char (ldb (byte 8 0) c)) port) nil)
(defimpl "scm_display" (x port) (scm "display" x (if (eq x +undefined+) (scm "current-output-port") port)))
(defimpl "scm_write" (x port) (scm "write" x port))
(defimpl "scm_newline" (port) (scm "newline" port))
(defimpl "scm_simple_format" (dest message args) (apply #'scm "simple-format" dest message args))
(defimpl "scm_force_output" (port) (scm "force-output" port))

;;; ------------------------------------------------------------------
;;; Variadic functions (core.c): the count, then the arguments

(defun varargs (sap)
  (let ((n (sb-sys:sap-ref-64 sap 0)))
    (loop for i from 1 to n collect (sb-sys:sap-ref-64 sap (* 8 i)))))

(defun install-varargs ()
  (setf (aref *impls* (api-index (find-api "scm_list_n")))
	(lambda (sap) (export-scm (mapcar #'import-scm (cdr (varargs* sap))))))
  (setf (aref *impls* (api-index (find-api "scm_call")))
	(lambda (sap)
	  (destructuring-bind (proc &rest args) (varargs* sap)
	    (export-scm (apply (import-scm proc) (mapcar #'import-scm args))))))
  (setf (aref *impls* (api-index (find-api "scm_c_export")))
	(lambda (sap)
	  (scm "module-export!" (current-module) (mapcar (lambda (a) (sym (c-string a))) (varargs sap)))
	  0)))

(defun varargs* (sap)
  "A variadic function's first argument (word 1), then the rest (2...)."
  (let ((n (sb-sys:sap-ref-64 sap 0)))
    (cons (sb-sys:sap-ref-64 sap 8) (loop for i from 2 below (+ 2 n) collect (sb-sys:sap-ref-64 sap (* 8 i))))))

;;; ------------------------------------------------------------------
;;; Functions LilyPond calls

(defimpl "scm_c_memq" (x list) (scm "memq" x list))
(defimpl "scm_cons2" (w x y) (cons w (cons x y)))
(defimpl "scm_c_with_fluid" (fluid value fptr data)
  (scm "with-fluid*" fluid value
       (lambda ()
	 (import-scm (c-funcall
		      (sb-alien:sap-alien (address-sap fptr) (function sb-alien:unsigned-long sb-alien:unsigned-long))
		      data)))))

(defimpl "scm_gc_register_collectable_memory" (mem size what)
  (declare (ignore mem size what))
  nil)
(defimpl "scm_gc_unregister_collectable_memory" (mem size what)
  (declare (ignore mem size what))
  nil)
(defimpl "scm_remember_upto_here" (x) (declare (ignore x)) nil)

(defimpl "scm_internal_hash_fold" (fptr closure init table)
  (scm "hash-fold"
       (lambda (key value result)
	 (import-scm (c-funcall-same-gen
		      (sb-alien:sap-alien (address-sap fptr)
					  (function sb-alien:unsigned-long sb-alien:unsigned-long
						    sb-alien:unsigned-long sb-alien:unsigned-long
						    sb-alien:unsigned-long))
		      closure (export-scm key) (export-scm value) (export-scm result))))
       init table))

(defimpl "scm_dynwind_fluid" (fluid value)
  (when *dynwinds*
    (let ((old (scm "fluid-ref" fluid)))
      (scm "fluid-set!" fluid value)
      (push (list :unwind (lambda (data) (declare (ignore data)) (scm "fluid-set!" fluid old)) nil 0)
	    (dynwind-handlers (first *dynwinds*)))))
  nil)

(defimpl "scm_c_read_string" (address)
  (scm "read" (scm "open-input-string" (c-string address))))

;; their argument is the list of the lists to join
(defimpl "scm_append" (lists) (apply #'scm "append" lists))
(defimpl "scm_append_x" (lists) (apply #'scm "append!" lists))

;; a smob that can be applied is still written by its print function
(setq qsg::*write-object-hook*
      (lambda (x stream) (when (c-object-p x) (print-object x stream) t)))
