; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-LIBGUILE -*-

;;;; The table's functions, implemented (docs/libguile.md): the entry
;;;; point C calls, each type's conversion, and a DEFIMPL for each DEFC.

(in-package "PSEUDOSCHEME-LIBGUILE")

(defvar *embedded* nil "True in the core a C program starts (START-EMBEDDED).")
(defvar *trace-calls* nil
  "Print each function C calls (PSEUDOSCHEME_LIBGUILE_TRACE=1), with its
arguments and result (=2).")
(defun brief (x depth)
  "X as text for the trace: lists and atoms, not objects."
  (cond ((null x) "()")
	((consp x) (if (zerop depth)
		       "(...)"
		       (format nil "(~{~A~^ ~}~A)"
			       (loop for y on x for n below 12
				     collect (brief (car y) (1- depth)))
			       (if (listp (cdr (last x))) "" " . ?"))))
	((stringp x) (format nil "~S" (subseq x 0 (min 40 (length x)))))
	((symbolp x) (symbol-name x))
	((numberp x) (format nil "~A" x))
	(t (format nil "<~(~A~)>" (type-of x)))))

(defvar *call-count* 0 "The calls from C so far, when tracing.")
(defvar *profile* nil
  "Count and time each function C calls (PSEUDOSCHEME_LIBGUILE_PROFILE=1);
the table is printed when the program exits.")
(defvar *profile-counts*) (defvar *profile-times*) (defvar *profile-child* 0)
(defvar *profile-refreshed*) (defvar *profile-absorbed*)
(defvar *subr-stats* (vector 0 0 0) "Calls of C subrs from Scheme, and the entries they write and read, for the profile.")
(defvar *trace-after* 0 "Trace at level 2 only from this call on (PSEUDOSCHEME_LIBGUILE_TRACE_AFTER).")
(defvar *entered* nil "True inside a call from C that embedded Lisp made Guile code.")
(defvar *impls* (make-array 0 :adjustable t) "Index -> function of the args SAP.")
(defvar *reader-ids* (make-array 0) "Index -> true if the function only reads (READER-FUNCTION-P).")

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
  (labels ((call ()
	   (when *trace-calls*
	     (incf *call-count*)
	     (format *error-output* "~&;; ~A~%" (api-name (aref *api* id)))
	     (when (and (eql *trace-calls* 2) (> *call-count* *trace-after*))
	       (format *error-output* ";;     ~{~A~^ ~}~%"
		       (loop for type in (api-args (aref *api* id)) for i from 0
			     collect (let ((w (sb-sys:sap-ref-64 args (* 8 i))))
				       (handler-case (if (eq type :scm) (brief (import-scm w) 4) w)
					 (error () w))))))
	     (finish-output *error-output*))
	   (crossing
	     (when (c-call "psg_reap_needed" sb-alien:int) (with-bridge (reap)))
	     ;; a function that only reads (*READER-FUNCTIONS*) runs no Lisp
	     ;; that could see a variable C wrote, or change one C reads
	     (if (aref *reader-ids* id)
		 (funcall (the function (aref *impls* id)) args)
		 (progn
		   (sync-variables-to-lisp)
		   (multiple-value-prog1 (funcall (the function (aref *impls* id)) args)
		     (sync-variables-to-c))))))
	 (profiled-call (id)
	   ;; the time of each function itself: what it calls back into
	   ;; (Scheme code, C subrs) is the callee's
	   (let ((start (get-internal-real-time)) (outer *profile-child*)
		 (refreshed *refreshed*) (absorbed *absorbed*))
	     (setq *profile-child* 0)
	     (multiple-value-prog1 (call)
	       (let ((total (- (get-internal-real-time) start)))
		 (incf (aref *profile-counts* id))
		 (incf (aref *profile-refreshed* id) (- *refreshed* refreshed))
		 (incf (aref *profile-absorbed* id) (- *absorbed* absorbed))
		 (when (zerop (mod (incf *call-count*) 20000))
		   (if *sprof* (when (>= *call-count* 140000) (psg-sprof-dump)) (psg-profile-dump)))
		 (incf (aref *profile-times* id) (- total *profile-child*))
		 (setq *profile-child* (+ outer total)))))))
    ;; A C program that embeds us makes the first call outside any
    ;; Guile code: errors become Guile exceptions from here
    (if (and *embedded* (not *entered*))
	(let ((*entered* t)) (psg::with-guile-errors (if *profile* (profiled-call id) (call))))
	(if *profile* (profiled-call id) (call)))))

(defun psg-profile-dump ()
  (when *profile*
    (let ((rows (loop for id below (length *profile-counts*)
		      when (plusp (aref *profile-counts* id))
			collect (list (api-name (aref *api* id)) (aref *profile-counts* id) (aref *profile-times* id)
				      (aref *profile-refreshed* id) (aref *profile-absorbed* id)))))
      (format *error-output* "~&;; calls ~D, ~,2F s in the functions themselves (milliseconds, calls):~%"
	      (reduce #'+ rows :key #'second) (/ (reduce #'+ rows :key #'third) internal-time-units-per-second))
      (format *error-output* ";; ~D calls of C functions from Scheme, writing ~D entries and reading ~D~%"
	      (svref *subr-stats* 0) (svref *subr-stats* 1) (svref *subr-stats* 2))
      (format *error-output* ";; ms, calls, entries written to / read from mirrors (with the calls inside):~%")
      (loop for (name n time written read) in (sort rows #'> :key #'third) repeat 30
	    do (format *error-output* ";; ~8,1F ~8D ~9D ~9D  ~A~%"
		       (/ (* 1000 time) internal-time-units-per-second) n written read name))
      (finish-output *error-output*))))
(defvar *sprof* nil "SBCL's statistical profiler runs (PSEUDOSCHEME_LIBGUILE_PROFILE=sprof).")
(defun psg-sprof-dump ()
  (let ((*standard-output* *error-output*))
	(handler-case
	    (progn (uiop:symbol-call "SB-SPROF" "STOP-PROFILING")
		   (uiop:symbol-call "SB-SPROF" "REPORT" :type :flat :max 70 :sort-by :cumulative-samples))
	  (error (c) (format t "~&;; sprof: ~A~%" c)))
	(finish-output *error-output*)))
(sb-alien:define-alien-callable psg-profile-dump-callback sb-alien:void ()
  (if *sprof* (psg-sprof-dump) (psg-profile-dump)))

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
  (let ((types (api-args api)) (return (api-return api)) (shallow (api-shallow api))
	(reader (reader-function-p api)))
    (lambda (sap)
      (let* ((args (let ((*deep* (not shallow)))
		     (loop for type in types for i from 0
			   collect (arg-value type (sb-sys:sap-ref-64 sap (* 8 i))))))
	     (value (progn (unless reader (lisp-ran)) (apply function args))))
	;; back to C: Lisp may have changed what it returns
	(incf *epoch*)
	(unless reader (lisp-ran))
	(result-word return value)))))

;;; Functions that only read (docs/libguile.md, "Dirty tracking"): they
;;; change no mirrored object, so nothing needs writing back to the
;;; mirrors after them, and the next export of an object can be skipped
;;; if no other Lisp code has run since the last one.  Anything not
;;; listed is assumed to change its arguments.
(defparameter *reader-procedures*
  '(("length" . nil) ("list-ref" . nil) ("list-tail" . nil) ("list-head" . nil) ("list-copy" . nil)
    ("assq" . nil) ("assv" . nil) ("assoc" . nil)
    ("sloppy-assq" . nil) ("sloppy-assv" . nil) ("sloppy-assoc" . nil)
    ("memq" . nil) ("memv" . nil) ("member" . nil)
    ("hashq-ref" . 0) ("hashq-get-handle" . 0) ("hashq-set!" . 0) ("hashq-create-handle!" . 0)
    ("hashv-ref" . 0) ("hashv-get-handle" . 0) ("hashv-set!" . 0) ("hashv-create-handle!" . 0)
    ("vector-ref" . nil) ("vector-length" . nil) ("string-length" . 0) ("string-ref" . 0)
    ("symbol->string" . 0) ("string->symbol" . 0) ("car" . 0) ("cdr" . 0) ("cons" . 0))
  "Scheme procedures that only read their arguments, with how many levels
below each argument they look (NIL: all through).")

(defun reader-depth (name)
  "For a Scheme procedure NAME that only reads its arguments: how many
levels below them it looks (NIL: all through); second value true if NAME
is such a procedure.  c[ad]+r looks one level for each letter after the
first."
  (let ((entry (assoc name *reader-procedures* :test #'string=)))
    (cond (entry (values (cdr entry) t))
	  ((and (<= 3 (length name) 6) (char= (char name 0) #\c) (char= (char name (1- (length name))) #\r)
		(every (lambda (c) (find c "ad")) (subseq name 1 (1- (length name)))))
	   (values (- (length name) 3) t))
	  ;; predicates
	  ((and (> (length name) 1) (char= (char name (1- (length name))) #\?))
	   (values nil t))
	  (t (values nil nil)))))

(defparameter *reader-functions*
  '("scm_i_new_smob" "scm_gc_register_collectable_memory" "scm_gc_unregister_collectable_memory"
    "scm_gc_protect_object" "scm_gc_unprotect_object" "scm_c_string_length" "scm_c_vector_ref"
    "scm_c_vector_length" "scm_ilength" "scm_cons2" "scm_c_make_hash_table" "scm_c_symbol_length"
    "scm_c_vector_ref" "scm_from_locale_string" "scm_from_locale_symbol")
  "Hand-written functions that change no mirrored object; so do those whose
names start scm_from_, scm_to_ and scm_is_.")

(defun reader-function-p (api)
  (let ((name (api-name api)))
    (or (api-shallow api)
	(find name *reader-functions* :test #'string=)
	(some (lambda (prefix) (eql 0 (search prefix name))) '("scm_from_" "scm_to_" "scm_is_"))
	(and (eq (api-kind api) :scheme) (nth-value 1 (reader-depth (api-scheme api)))))))

;;; DEFS entries: the Scheme procedure of that name
(defun guile-procedure (name)
  (or (funcall (pg root-value) name)
      (gethash name (symbol-value (pg *guile-primitives*)))
      (ignore-errors (psx:host-ref name))
      ;; defined in a module of Guile's own (ice-9 ports)
      (loop for module in '("guile" "ice-9 ports" "oop goops")
	    do (let ((value (scm "catch" ps:true
				 (lambda ()
				   (scm "module-ref" (scm "resolve-module" (mapcar (pg ssym) (uiop:split-string module)))
					(funcall (pg ssym) name)))
				 (lambda (&rest args) (declare (ignore args)) nil))))
		 (when value (return value))))
      (error "libguile: no procedure ~A" name)))

(defun scheme-impl (api)
  (let ((procedure nil) (req (api-req api)) (opt (api-opt api)) (rest (api-rest api))
	(name (api-scheme api)) (n (length (api-args api))))
    (multiple-value-bind (depth reader) (reader-depth name)
     (let ((depth (or depth most-positive-fixnum)))
    (lambda (sap)
      (let ((args (loop for i from 0 below n
			collect (sb-sys:sap-ref-64 sap (* 8 i)))))
	(unless procedure (setq procedure (guile-procedure name)))
	(unless reader (lisp-ran))
	;; optional arguments not given are SCM_UNDEFINED: they and those
	;; after go
	(let* ((given (loop for w in (subseq args 0 (+ req opt))
			    for i from 0
			    until (and (>= i req) (= w +undefined+))
			    collect (let ((*import-depth* depth)) (import-scm w))))
	       (rest-list (and rest (import-scm (car (last args))))))
	  (let ((result (apply procedure (append given rest-list))))
	    ;; back to C: what the procedure changed (reverse!, append!,
	    ;; vector-fill!) is written to the mirrors C holds, which
	    ;; the arguments' import this crossing had marked up to date
	    (incf *epoch*)
	    (unless reader (lisp-ran))
	    ;; a reader's arguments needn't be written again unless Lisp
	    ;; code has run since the last time (EXPORT-SCM checks)
	    (dolist (a given) (when (mutable-p a) (export-scm a)))
	    (export-scm result)))))))))

(defun missing-impl (api)
  (lambda (sap)
    (declare (ignore sap))
    (funcall (pg guile-error) (funcall (pg ssym) "misc-error") (api-name api)
	     "not implemented in this libguile (docs/libguile.md)" '())))

(defun install-default-impls ()
  (setf *impls* (make-array (fill-pointer *api*) :initial-element nil)
	*reader-ids* (make-array (fill-pointer *api*) :initial-element nil))
  (loop for api across *api*
	do (setf (aref *reader-ids* (api-index api)) (and (reader-function-p api) t)))
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
      (attach-library path)))
  *library*)

(defun attach-library (path)
  "Set up our libguile, now in this process, for this Lisp to serve."
  (let ((signature (c-string (sb-sys:sap-int (sb-sys:foreign-symbol-sap "psg_table_signature" t)))))
    (unless (string= signature (table-signature))
      (error "libguile: ~A was built from another table (rebuild it: make -C src/libguile)" path)))
  (install-default-impls)
  (install-impls)
  (install-varargs)
  (c-call "psg_set_entry" sb-alien:void sb-sys:system-area-pointer
	  (sb-alien:alien-sap (sb-alien:alien-callable-function 'psg-entry)))
  (setq *library* path)
  (initialize-data))

(defun start-embedded ()
  "The start-up of pseudoscheme-guile.core (src/libguile/build-core.lisp),
which a C program linked against our libguile starts at its first call
(core.c, boot_lisp): Lisp then serves the library already loaded."
  (sb-ext:disable-debugger)
  (setq *embedded* t
	*trace-calls* (let ((n (parse-integer (or (uiop:getenv "PSEUDOSCHEME_LIBGUILE_TRACE") "") :junk-allowed t)))
			(and n (plusp n) n)))
  (when (equal (uiop:getenv "PSEUDOSCHEME_LIBGUILE_PROFILE") "sprof")
    (require :sb-sprof)
    (setq *sprof* t *profile* t
	  *profile-counts* (make-array (fill-pointer *api*) :initial-element 0)
	  *profile-times* (make-array (fill-pointer *api*) :initial-element 0)
	  *profile-refreshed* (make-array (fill-pointer *api*) :initial-element 0)
	  *profile-absorbed* (make-array (fill-pointer *api*) :initial-element 0))
    (c-call "psg_at_exit" sb-alien:void sb-sys:system-area-pointer
	    (sb-alien:alien-sap (sb-alien:alien-callable-function 'psg-profile-dump-callback)))
    (uiop:symbol-call "SB-SPROF" "START-PROFILING" :mode :cpu :sample-interval 0.002 :threads :all))
  (when (equal (uiop:getenv "PSEUDOSCHEME_LIBGUILE_PROFILE") "1")
    (setq *profile* t
	  *profile-counts* (make-array (fill-pointer *api*) :initial-element 0)
	  *profile-times* (make-array (fill-pointer *api*) :initial-element 0)
	  *profile-refreshed* (make-array (fill-pointer *api*) :initial-element 0)
	  *profile-absorbed* (make-array (fill-pointer *api*) :initial-element 0))
    (c-call "psg_at_exit" sb-alien:void sb-sys:system-area-pointer
	    (sb-alien:alien-sap (sb-alien:alien-callable-function 'psg-profile-dump-callback))))
  (setq *trace-after* (or (parse-integer (or (uiop:getenv "PSEUDOSCHEME_LIBGUILE_TRACE_AFTER") "") :junk-allowed t) 0))
  (psg::add-environment-load-path)
  (attach-library "libguile-3.0.so.1"))

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
    ;; SCM_MODULEP compares a struct's first word with this: the module
    ;; type's vtable, tagged
    (setf (sb-sys:sap-ref-64 (address-sap (data-address "scm_module_tag")) 0)
	  (word (export-scm (scm "resolve-module" (list (sym "guile")))) 0))
    (setf (sb-sys:signed-sap-ref-32 (address-sap (data-address "scm_module_system_booted_p")) 0) 1)
    (setf (sb-sys:signed-sap-ref-32 (address-sap (data-address "scm_initialized_p")) 0) 1)))

;;; ------------------------------------------------------------------
;;; Procedures written in C (gsubrs)

(defmacro c-apply-n (fptr words)
  "Call the C function at FPTR with WORDS (a list of up to 10 SCM words)."
  `(let ((f (address-sap ,fptr)) (w ,words))
     (case (length w)
       ,@(loop for n from 0 to 10
	       collect `(,n (c-funcall
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
	     (lisp-ran)
	     (let* ((refreshed *refreshed*)
		    (fixed (loop for i below (+ req opt)
				 collect (if args (export-scm (pop args)) +undefined+)))
		    (words (if rest (append fixed (list (export-scm args))) fixed))
		    (written (- *refreshed* refreshed))
		    (result (with-pinned-words (words) (call-c fptr words))))
	       (crossing
		 ;; what C did to the arguments shows
		 (let ((absorbed *absorbed*))
		   (dolist (w words) (unless (= w +undefined+) (import-scm w)))
		   (prog1 (import-scm result)
		     (when *profile*
		       (incf (svref *subr-stats* 0))
		       (incf (svref *subr-stats* 1) written)
		       (incf (svref *subr-stats* 2) (- *absorbed* absorbed))))))))))
    (setf (gethash f (symbol-value (pg *procedure-properties*)))
	  (list (cons (funcall (pg ssym) "name") (funcall (pg ssym) name))))
    f))

;;; ------------------------------------------------------------------
;;; Dynamic extents

(defstruct (dynwind (:constructor make-dynwind ())) (handlers '()))

(defun close-dynwind (frame non-local)
  (dolist (h (dynwind-handlers frame))
    (destructuring-bind (kind fptr data flags) h
      (when (and (eq kind :unwind) (functionp fptr))
	;; a handler written in Lisp (scm_dynwind_fluid): always runs
	(funcall fptr data))
      (when (and (eq kind :unwind) (integerp fptr) (or non-local (logtest +wind-explicitly+ flags)))
	(c-funcall (sb-alien:sap-alien (address-sap fptr) (function sb-alien:void sb-alien:unsigned-long))
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
	  (c-funcall (sb-alien:sap-alien f (function sb-alien:void))))
	t))))

(setf (symbol-value (pg *c-extension-loader*)) 'load-c-extension)
