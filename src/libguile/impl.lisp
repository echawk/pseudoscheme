; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-LIBGUILE -*-

;;;; The table's functions, implemented (docs/libguile.md): the entry
;;;; point C calls, each type's conversion, and a DEFIMPL for each DEFC.

(in-package "PSEUDOSCHEME-LIBGUILE")

(defvar *impls* (make-array 0 :adjustable t) "Index -> function of the args SAP.")

;;; ------------------------------------------------------------------
;;; Arguments and results

(defun c-string (address &optional length)
  "The UTF-8 string at ADDRESS (NUL-terminated unless LENGTH)."
  (let* ((sap (address-sap address))
	 (n (or length (loop for i from 0 when (zerop (sb-sys:sap-ref-8 sap i)) return i)))
	 (octets (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref octets i) (sb-sys:sap-ref-8 sap i)))
    (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?))))

(defun latin1-string (address length)
  (let ((s (make-string length)) (sap (address-sap address)))
    (dotimes (i length s) (setf (char s i) (code-char (sb-sys:sap-ref-8 sap i))))))

(defun malloc-octets (octets &optional (nul t))
  "A malloc'd copy of OCTETS (NUL-terminated), as C frees it."
  (let* ((n (length octets))
	 (p (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 8) (+ n (if nul 1 0)))))))
    (dotimes (i n) (setf (sb-sys:sap-ref-8 (address-sap p) i) (aref octets i)))
    (when nul (setf (sb-sys:sap-ref-8 (address-sap p) n) 0))
    p))

(defun signed (word bits)
  (let ((w (ldb (byte bits 0) word))) (if (logbitp (1- bits) w) (- w (ash 1 bits)) w)))

(defun arg-value (type word)
  "The Lisp value of a C argument of TYPE, passed as WORD."
  (ecase type
    (:scm (import-scm word))
    ((:int :bool :int32 :wchar) (signed word 32))
    ((:long :ssize :int64 :intmax :intptr) (signed word 64))
    (:int8 (signed word 8)) (:int16 (signed word 16)) (:char (signed word 8))
    ((:uint :uint32) (ldb (byte 32 0) word))
    (:uint8 (ldb (byte 8 0) word)) (:uint16 (ldb (byte 16 0) word))
    ((:ulong :size :uint64 :uintmax :uintptr :ptr :string) word)
    (:double (bits-double word))))

(defun result-word (type value)
  "VALUE, the result of a function returning TYPE, as a word."
  (ecase type
    (:scm (export-scm value))
    (:void 0)
    ((:int :bool) (ldb (byte 64 0) (if (eq value t) 1 (if (null value) 0 value))))
    ((:long :ssize :int8 :int16 :int32 :int64 :intmax :intptr :char :wchar
      :uint :ulong :size :uint8 :uint16 :uint32 :uint64 :uintmax :uintptr :ptr :string)
     (ldb (byte 64 0) value))
    (:double (ldb (byte 64 0) (sb-kernel:double-float-bits (coerce value 'double-float))))))

;;; ------------------------------------------------------------------
;;; The entry point

(sb-alien:define-alien-callable psg-entry sb-alien:unsigned-long
    ((id sb-alien:unsigned-long) (args sb-sys:system-area-pointer))
  (crossing
    (with-bridge (reap))
    (funcall (the function (aref *impls* id)) args)))

(defvar *impl-definitions* '() "(name . function), from DEFIMPL.")

(defmacro defimpl (name lambda-list &body body)
  "The implementation of table entry NAME (a DEFC): BODY, with the
arguments converted to Lisp, returning the Lisp value to convert back."
  `(let ((f (lambda ,lambda-list ,@body)))
     (setf *impl-definitions* (cons (cons ,name f) (remove ,name *impl-definitions* :key #'car :test #'string=)))))

(defun install-impls ()
  (loop for (name . f) in *impl-definitions* do (install-impl name f)))

(defun install-impl (name function)
  (let ((api (or (find-api name) (error "libguile: no table entry ~A" name))))
    (setf (aref *impls* (api-index api)) (make-entry-function api function))))

(defun make-entry-function (api function)
  (let ((types (api-args api)) (return (api-return api)) (shallow (api-shallow api)))
    (lambda (sap)
      (let* ((args (let ((*deep* (not shallow)))
		     (loop for type in types for i from 0
			   collect (arg-value type (sb-sys:sap-ref-64 sap (* 8 i))))))
	     (value (apply function args)))
	;; back to C: Lisp may have changed what it returns
	(incf *epoch*)
	(result-word return value)))))

;;; DEFS entries: the Scheme procedure of that name
(defun guile-procedure (name)
  (or (funcall (pg root-value) name)
      (gethash name (symbol-value (pg *guile-primitives*)))
      (ignore-errors (psx:host-ref name))
      (error "libguile: no procedure ~A" name)))

(defun scheme-impl (api)
  (let ((procedure nil) (req (api-req api)) (opt (api-opt api)) (rest (api-rest api))
	(name (api-scheme api)) (n (length (api-args api))))
    (lambda (sap)
      (let ((args (loop for i from 0 below n
			collect (sb-sys:sap-ref-64 sap (* 8 i)))))
	(unless procedure (setq procedure (guile-procedure name)))
	;; optional arguments not given are SCM_UNDEFINED: they and those
	;; after go
	(let* ((given (loop for w in (subseq args 0 (+ req opt))
			    for i from 0
			    until (and (>= i req) (= w +undefined+))
			    collect (import-scm w)))
	       (rest-list (and rest (import-scm (car (last args))))))
	  (export-scm (apply procedure (append given rest-list))))))))

(defun missing-impl (api)
  (lambda (sap)
    (declare (ignore sap))
    (funcall (pg guile-error) (funcall (pg ssym) "misc-error") (api-name api)
	     "not implemented in this libguile (docs/libguile.md)" '())))

(defun install-default-impls ()
  (setf *impls* (make-array (fill-pointer *api*) :initial-element nil))
  (loop for api across *api*
	do (setf (aref *impls* (api-index api))
		 (case (api-kind api)
		   (:scheme (scheme-impl api))
		   (t (missing-impl api))))))

;;; ------------------------------------------------------------------
;;; Loading the library

(defun load-libguile ()
  "Load our libguile-3.0.so.1 into this process, once: then extensions
built against Guile find their libguile already loaded."
  (unless *library*
    (let ((path (library-path)))
      (unless (probe-file path)
	(error "libguile: ~A isn't built (make -C src/libguile)" path))
      (sb-alien:load-shared-object path :dont-save t)
      (let ((signature (c-string (sb-sys:sap-int (sb-sys:foreign-symbol-sap "psg_table_signature" t)))))
	(unless (string= signature (table-signature))
	  (error "libguile: ~A was built from another table (rebuild it: make -C src/libguile)" path)))
      (install-default-impls)
      (install-impls)
      (install-varargs)
      (c-call "psg_set_entry" sb-alien:void sb-sys:system-area-pointer
	      (sb-alien:alien-sap (sb-alien:alien-callable-function 'psg-entry)))
      (setq *library* path)
      (initialize-data)))
  *library*)

(defun data-address (name) (sb-sys:sap-int (sb-sys:foreign-symbol-sap name t)))

(defun set-data (name value)
  (setf (sb-sys:sap-ref-64 (address-sap (data-address name)) 0) (export-scm value)))

(defun initialize-data ()
  (let ((ssym (pg ssym)))
    (loop for (c scheme) in '(("scm_sym_apply" "apply") ("scm_sym_args" "args") ("scm_sym_arrow" "=>")
			      ("scm_sym_at" "@") ("scm_sym_atat" "@@") ("scm_sym_begin" "begin")
			      ("scm_sym_case" "case") ("scm_sym_column" "column") ("scm_sym_cond" "cond")
			      ("scm_sym_copy" "copy") ("scm_sym_define" "define") ("scm_sym_dot" ".")
			      ("scm_sym_else" "else") ("scm_sym_eval_when" "eval-when")
			      ("scm_sym_filename" "filename") ("scm_sym_if" "if") ("scm_sym_lambda" "lambda")
			      ("scm_sym_let" "let") ("scm_sym_letrec" "letrec") ("scm_sym_letstar" "let*")
			      ("scm_sym_line" "line") ("scm_sym_name" "name")
			      ("scm_sym_quasiquote" "quasiquote") ("scm_sym_quote" "quote")
			      ("scm_sym_set_x" "set!") ("scm_sym_system_procedure" "system-procedure")
			      ("scm_sym_unquote" "unquote") ("scm_sym_uq_splicing" "unquote-splicing")
			      ("scm_arg_type_key" "wrong-type-arg") ("scm_args_number_key" "wrong-number-of-args")
			      ("scm_misc_error_key" "misc-error") ("scm_num_overflow_key" "numerical-overflow")
			      ("scm_out_of_range_key" "out-of-range") ("scm_system_error_key" "system-error")
			      ("scm_endianness_big" "big") ("scm_endianness_little" "little"))
	  do (set-data c (funcall ssym scheme)))
    (setf (sb-sys:signed-sap-ref-32 (address-sap (data-address "scm_initialized_p")) 0) 1)))

;;; ------------------------------------------------------------------
;;; Procedures written in C (gsubrs)

(defmacro c-apply-n (fptr words)
  "Call the C function at FPTR with WORDS (a list of up to 10 SCM words)."
  `(let ((f (address-sap ,fptr)) (w ,words))
     (case (length w)
       ,@(loop for n from 0 to 10
	       collect `(,n (sb-alien:alien-funcall
			     (sb-alien:sap-alien f (function sb-alien:unsigned-long
							     ,@(make-list n :initial-element 'sb-alien:unsigned-long)))
			     ,@(loop for i below n collect `(nth ,i w)))))
       (t (error "libguile: a subr of ~D arguments" (length w))))))

(defvar *dynwinds* '() "The open scm_dynwind_begin frames of this thread.")

(defun call-c (fptr words)
  "FPTR called with WORDS; then any dynwind frames it left open (a
non-local exit through C) are closed."
  (let ((depth (length *dynwinds*)) (done nil))
    (unwind-protect
	 (multiple-value-prog1 (c-apply-n fptr words) (setq done t))
      (loop while (> (length *dynwinds*) depth)
	    do (close-dynwind (pop *dynwinds*) (not done))))))

(defun make-subr (name req opt rest fptr)
  (let ((f (lambda (&rest args)
	     (let ((n (length args)))
	       (when (or (< n req) (and (not rest) (> n (+ req opt))))
		 (funcall (pg guile-error) (funcall (pg ssym) "wrong-number-of-args") name
			  "Wrong number of arguments to ~A" (list name))))
	     (incf *epoch*)
	     (let* ((fixed (loop for i below (+ req opt)
				 collect (if args (export-scm (pop args)) +undefined+)))
		    (words (if rest (append fixed (list (export-scm args))) fixed))
		    (result (call-c fptr words)))
	       (crossing
		 ;; what C did to the arguments shows
		 (dolist (w words) (unless (= w +undefined+) (import-scm w)))
		 (import-scm result))))))
    (setf (gethash f (symbol-value (pg *procedure-properties*)))
	  (list (cons (funcall (pg ssym) "name") (funcall (pg ssym) name))))
    f))

;;; ------------------------------------------------------------------
;;; Dynamic extents

(defstruct (dynwind (:constructor make-dynwind ())) (handlers '()))

(defun close-dynwind (frame non-local)
  (dolist (h (dynwind-handlers frame))
    (destructuring-bind (kind fptr data flags) h
      (when (and (eq kind :unwind) (or non-local (logtest +wind-explicitly+ flags)))
	(sb-alien:alien-funcall (sb-alien:sap-alien (address-sap fptr) (function sb-alien:void sb-alien:unsigned-long))
				data)))))

;;; ------------------------------------------------------------------
;;; load-extension, for libraries written in C against libguile

(defun find-extension (library)
  "The shared library file LIBRARY names, as Guile's load-extension
looks for it: as given, with .so, then in GUILE_EXTENSIONS_PATH."
  (let ((dirs (append (list "")
		      (mapcar (lambda (d) (concatenate 'string d "/"))
			      (remove "" (uiop:split-string (or (uiop:getenv "GUILE_EXTENSIONS_PATH") "")
							    :separator ":")
				      :test #'string=)))))
    (loop for dir in dirs
	  do (loop for ext in '("" ".so")
		   for path = (concatenate 'string dir library ext)
		   when (and (uiop:absolute-pathname-p path) (probe-file path))
		     do (return-from find-extension path)))
    nil))

(defun load-c-extension (library init)
  "Run INIT, a C function of LIBRARY, against our libguile; NIL if
LIBRARY isn't a shared library we can find."
  (let ((path (find-extension library)))
    (when path
      (load-libguile)
      (sb-alien:load-shared-object path :dont-save t)
      (let ((f (sb-sys:foreign-symbol-sap init)))
	(crossing
	  (sb-alien:alien-funcall (sb-alien:sap-alien f (function sb-alien:void))))
	t))))

(setf (symbol-value (pg *c-extension-loader*)) 'load-c-extension)
