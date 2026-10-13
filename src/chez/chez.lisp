; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; Chez Scheme: the (chezscheme) library, and Chez's top level, which
;;;; pseudoscheme --chez runs scripts at.  docs/chez.md.
;;;;
;;;; (chezscheme) is R6RS, as Chez's re-exports it, plus the Scheme of
;;;; src/chez/*.scm, which uses the host procedures of host.lisp and
;;;; this file through (pseudoscheme chez host), prefixed %.
;;;;
;;;; Chez's top level is an interaction library whose unbound names come
;;;; from (chezscheme), as the R5RS one's come from (pseudoscheme r5rs).
;;;; A library file whose library a macro makes -- (import ...) and then
;;;; a use of a macro that expands into (library ...), as Chez allows --
;;;; is loaded at a fresh top level of its own (*LIBRARY-LOADERS*).

(in-package "PSEUDOSCHEME-R7RS")

(defparameter *chezscheme-sources*
  '("chezscheme.scm" "lists.scm" "records.scm" "numbers.scm" "hashtables.scm" "threads.scm"
    "conditions.scm" "top-level.scm" "format.scm" "ffi.scm")
  "The files of (chezscheme)'s body, in order.")

(defparameter *chezscheme-extras*
  '(;; chezscheme.scm
    "void" "add1" "sub1" "atom?" "syntax-object->datum" "datum->syntax-object" "clear-input-port" "clear-output-port" "trace-lambda" "trace-case-lambda" "trace-define" "trace-let" "trace-define-syntax" "indirect-export" "trace-output-port" "define-structure" "syntax-error" "unread-char" "make-compile-time-value" "compile-time-value?" "compile-time-value-value" "internal-defines-as-letrec*" "file-buffer-size" "print-select-flonum-exponential-format" "compress-level" "compress-format" "console-input-port" "console-output-port" "console-error-port" "current-transcoder" "oblist" "r6rs:dynamic-wind" "file-port?" "port-length" "port-has-port-length?" "port-has-set-port-length!?" "ephemeron-cons" "ephemeron-pair?" "library-list" "library-version" "library-object-filename" "transcoder?" "with-continuation-mark" "current-continuation-marks" "continuation-marks?" "continuation-next-marks" "continuation-marks-first" "continuation-marks->list" "continuation-marks->iterator" "call-with-immediate-continuation-mark" "port-file-compressed!" "r6rs:current-input-port" "r6rs:current-output-port" "r6rs:current-error-port" "call/1cc" "gensym" "getenv" "system" "open-process-ports" "process" "make-parameter"
    "with-input-from-string" "with-output-to-string" "current-directory"
    "file-directory?" "file-regular?" "file-symbolic-link?" "directory-list"
    "mkdir" "delete-directory" "rename-file" "file-modification-time"
    "directory-separator" "machine-type" "get-mode" "chmod" "file-change-time"
    "source-directories" "record-writer" "collect" "weak-cons" "weak-pair?"
    "make-guardian" "guardian?" "unregister-guardian" "bytes-allocated"
    "virtual-register-count" "virtual-register" "set-virtual-register!"
    "bwp-object?" "iota" "box" "box?" "unbox" "set-box!" "box-cas!" "pretty-print"
    "fxvector?" "make-fxvector" "fxvector" "list->fxvector" "fxvector-length" "fxvector-ref"
    "fxvector-set!" "fxvector->list" "fxvector-fill!" "fxvector-copy"
    "fluid-let" "time" "parameterize" "include"
    "open-input-string" "open-output-string" "get-output-string" "last-pair"
    "list-head" "remq!" "remv!" "remove!" "string-copy!"
    "fxquotient" "fxremainder" "fxmodulo" "fx1+" "fx1-" "fxlogand" "fxlogor"
    "fxlogxor" "fxsll" "fxsra" "make-list" "fxabs" "with-implicit"
    "make-time" "time?" "time-type" "time-second" "time-nanosecond"
    "set-time-type!" "set-time-second!" "set-time-nanosecond!" "current-time"
    "cpu-time" "real-time" "statistics" "sstats?" "sstats-cpu" "sstats-real"
    "sstats-bytes" "sstats-gc-count" "sstats-gc-cpu" "sstats-gc-real"
    "sstats-gc-bytes" "make-date" "date?" "date-nanosecond" "date-second"
    "date-minute" "date-hour" "date-day" "date-month" "date-year"
    "date-zone-offset" "time-utc->date" "current-date" "copy-time" "time-difference"
    "time-difference!" "add-duration" "add-duration!" "subtract-duration"
    "subtract-duration!" "time=?" "time<?" "time<=?" "time>?" "time>=?"
    "stencil-vector" "stencil-vector?" "stencil-vector-mask" "stencil-vector-length"
    "stencil-vector-ref" "stencil-vector-set!" "stencil-vector-mask-width"
    "stencil-vector-update" "stencil-vector-truncate!" "file-access-time" "pretty-format" "decode-float" "display-string"
    "r6rs:char=?" "r6rs:char<?" "r6rs:char>?" "r6rs:char<=?" "r6rs:char>=?"
    "r6rs:char-ci=?" "r6rs:char-ci<?" "r6rs:char-ci>?" "r6rs:char-ci<=?"
    "r6rs:char-ci>=?" "r6rs:string=?" "r6rs:string<?" "r6rs:string>?"
    "r6rs:string<=?" "r6rs:string>=?" "r6rs:string-ci=?" "r6rs:string-ci<?"
    "r6rs:string-ci>?" "r6rs:string-ci<=?" "r6rs:string-ci>=?" "r6rs:fx+" "r6rs:fx-"
    "r6rs:fx*" "char-name" "flonum->fixnum" "flsingle" "immutable-vector-append"
    "immutable-vector-copy" "vector-cas!" "bytevector-u24-ref" "bytevector-s24-ref"
    "bytevector-u24-set!" "bytevector-s24-set!" "bytevector-u40-ref"
    "bytevector-s40-ref" "bytevector-u40-set!" "bytevector-s40-set!"
    "bytevector-u48-ref" "bytevector-s48-ref" "bytevector-u48-set!"
    "bytevector-s48-set!" "bytevector-u56-ref" "bytevector-s56-ref"
    "bytevector-u56-set!" "bytevector-s56-set!"
    ;; hashtables.scm
    "make-weak-eq-hashtable" "make-weak-eqv-hashtable" "make-ephemeron-eq-hashtable"
    "make-ephemeron-eqv-hashtable" "hashtable-weak?" "hashtable-ephemeron?"
    "make-weak-hashtable" "make-ephemeron-hashtable" "collect-maximum-generation"
    "eq-hashtable?" "eq-hashtable-weak?" "eq-hashtable-ephemeron?" "eq-hashtable-ref"
    "eq-hashtable-set!" "eq-hashtable-contains?" "eq-hashtable-delete!"
    "eq-hashtable-update!" "symbol-hashtable?" "symbol-hashtable-ref"
    "symbol-hashtable-set!" "symbol-hashtable-contains?" "symbol-hashtable-delete!"
    "symbol-hashtable-update!" "hashtable-values" "hashtable-cells" "hashtable-cell"
    "eq-hashtable-cell" "symbol-hashtable-cell"
    ;; lists.scm
    "sort" "sort!" "merge" "merge!" "list*" "andmap" "ormap" "append!" "reverse!"
    "list-copy" "enumerate" "subst" "substq" "substv" "subst!" "substq!" "substv!"
    "vector-copy" "vector-append" "vector-set/copy" "vector->immutable-vector"
    "string->immutable-string" "bytevector->immutable-bytevector" "immutable-vector"
    "mutable-vector?" "immutable-vector?" "mutable-string?" "immutable-string?"
    "mutable-bytevector?" "immutable-bytevector?" "substring-fill!" "char-"
    "gensym->unique-string" "gensym?" "string->uninterned-symbol" "uninterned-symbol?"
    "property-list" "getprop" "putprop" "remprop"
    ;; records.scm
    "define-record" "prefix" "predicate" "constructor" "make-record-type" "csv7:record-field-accessor"
    "csv7:record-field-mutator" "csv7:record-field-accessible?" "csv7:record-field-mutable?"
    "csv7:record-type-field-names" "csv7:record-type-field-decls" "record-type-equal-procedure"
    "record-type-hash-procedure" "record-equal-procedure" "record-hash-procedure"
    "default-record-equal-procedure" "default-record-hash-procedure"
    "record-constructor-descriptor?" "r6rs:record-constructor" "r6rs:record?" "record-reader"
    ;; more of lists.scm, numbers.scm, chezscheme.scm
    "fxvector-copy!" "flvector?" "make-flvector" "flvector" "list->flvector" "flvector-length"
    "flvector-ref" "flvector-set!" "flvector->list" "flvector-fill!" "flvector-copy"
    "flvector-copy!" "string-append-immutable" "immutable-vector-set/copy"
    "list-assuming-immutable?" "fxpopcount32" "fxpopcount16" "fxlogbit0" "fxlogbit1"
    "fx+/wraparound" "fx-/wraparound" "fx*/wraparound" "fxsll/wraparound" "flbit-field"
    "cfl+" "cfl-" "cfl*" "cfl/" "cfl=" "cfl-real-part" "cfl-imag-part" "cfl-conjugate"
    "cfl-magnitude-squared" "fl-make-rectangular" "make-sstats" "sstats-difference"
    "sstats-print" "set-sstats-cpu!" "set-sstats-real!" "set-sstats-bytes!"
    "set-sstats-gc-count!" "set-sstats-gc-cpu!" "set-sstats-gc-real!" "set-sstats-gc-bytes!"
    "directory-separator?" "path-absolute?" "path-last" "path-parent" "path-first"
    "path-rest" "path-extension" "path-root" "path-build"
    ;; numbers.scm
    "1+" "1-" "-1+" "nonnegative?" "nonpositive?" "most-positive-fixnum"
    "most-negative-fixnum" "bignum?" "ratnum?" "cflonum?" "logand" "logior" "logor"
    "logxor" "lognot" "logbit?" "logtest" "logbit0" "logbit1" "ash" "integer-length"
    "isqrt" "expt-mod" "fx=" "fx<" "fx>" "fx<=" "fx>=" "fx/" "fxnonnegative?"
    "fxnonpositive?" "fxlogbit?" "fxlogtest" "fxlognot" "fxlogior" "fxsrl" "fxpopcount"
    "fl=" "fl<" "fl>" "fl<=" "fl>=" "flnonnegative?" "flnonpositive?"
    "random" "random-seed" "sinh" "cosh" "tanh" "asinh" "acosh" "atanh"
    ;; threads.scm
    "fork-thread" "thread?" "thread-join" "get-thread-id" "make-mutex" "mutex?"
    "mutex-name" "mutex-acquire" "mutex-release" "with-mutex" "make-condition"
    "thread-condition?" "condition-wait" "condition-signal" "condition-broadcast"
    "make-thread-parameter" "sleep" "threaded?"
    ;; conditions.scm
    "format" "printf" "fprintf" "errorf" "assertion-violationf" "warningf" "warning"
    "display-condition"
    ;; top-level.scm
    "interaction-environment" "eval" "interpret" "compile" "expand" "sc-expand"
    "scheme-environment" "copy-environment" "environment-symbols" "library-exports"
    "library-requirements" "library-requirements-options" "enum-set?" "char-ready?"
    "top-level-value" "set-top-level-value!" "define-top-level-value"
    "top-level-bound?" "top-level-mutable?"
    "load" "load-program" "load-library" "visit" "revisit" "compile-file"
    "compile-library" "compile-program" "compile-script" "compile-to-file" "compile-to-port" "load-compiled-from-port" "r6rs:command-line"
    "maybe-compile-file" "maybe-compile-library" "maybe-compile-program" "compile-program-handler"
    "library-directories" "library-extensions" "compile-imported-libraries"
    "generate-wpo-files" "library-search-handler" "default-library-search-handler" "compile-whole-library" "compile-whole-program" "concatenate-object-files" "verify-loadability"
    "compile-file-message" "compile-interpret-simple" "optimize-level" "debug-level"
    "commonization-level" "generate-inspector-information"
    "generate-procedure-source-information" "generate-interrupt-trap"
    "enable-object-counts" "undefined-variable-warnings" "import-notify"
    "library-timestamp-mode" "current-expand" "current-eval" "collect-request-handler"
    "collect-trip-bytes" "collect-rendezvous"
    "print-graph" "print-length" "print-level" "print-radix" "print-gensym"
    "print-brackets" "print-char-name" "print-vector-length" "print-precision"
    "print-extended-identifiers" "print-unicode" "pretty-line-length"
    "pretty-one-line-limit" "pretty-initial-indent" "pretty-standard-indent"
    "pretty-maximum-lines" "case-sensitive"
    "command-line-arguments" "get-process-id" "putenv" "scheme-version"
    "scheme-version-number" "petite?" "interactive?" "abort" "exit-handler"
    "reset-handler" "reset" "cd"
    "disable-interrupts" "enable-interrupts" "with-interrupts-disabled"
    "critical-section" "keyboard-interrupt-handler" "timer-interrupt-handler"
    "set-timer" "register-signal-handler" "make-engine"
    "datum" "rec" "eval-when" "$primitive" "syntax->list" "syntax->vector" "literal-identifier=?" "fluid-let-syntax"
    "inspect" "inspect/object" "debug" "break" "procedure-arity-mask"
    "make-wrapper-procedure" "make-arity-wrapper-procedure" "wrapper-procedure?"
    "wrapper-procedure-procedure" "wrapper-procedure-data" "set-wrapper-procedure-procedure!"
    "set-wrapper-procedure-data!"
    ;; ffi.scm
    "load-shared-object" "foreign-procedure" "foreign-entry?" "foreign-entry"
    "foreign-callable" "foreign-callable-entry-point" "foreign-callable-code-object"
    "foreign-alloc" "foreign-free" "foreign-sizeof" "foreign-ref" "foreign-set!"
    "open-fd-input-port" "open-fd-output-port" "open-fd-input/output-port"
    "port-file-descriptor" "set-port-nonblocking!" "port-nonblocking?"
    "lock-object" "unlock-object" "locked-object?"
    "char-grapheme-break-property" "char-extended-pictographic?" "bytevector"
    "bytevector-truncate!" "port-closed?" "fresh-line" "get-bytevector-some!"
    "put-bytevector-some" "truncate-file" "set-port-length!" "annotation?"
    "annotation-expression" "annotation-stripped" "annotation-source"
    "source-object-bfp" "source-object-efp" "source-file-descriptor"
    "make-annotation" "annotation-option-set" "annotation-options" "syntax->annotation"
    "source-file-descriptor?" "make-source-file-descriptor" "source-file-descriptor-path"
    "source-file-descriptor-checksum" "make-source-object" "source-object?" "source-object-sfd"
    "source-object-line" "source-object-column" "current-make-source-object"
    "locate-source" "locate-source-object-source" "current-locate-source-object-source"
    "open-source-file"
    "get-datum/annotations" "read-token" "compile-library-handler" "engine-block" "engine-return"
    "date->time-utc" "date-week-day" "define-values" "abort-handler")
  "What src/chez/*.scm defines that (chezscheme) exports, besides R6RS.")

(defparameter *chezscheme-from-psyntax*
  '("module" "import" "alias" "define-property" "library" "meta" "type-descriptor"
    "syntax-parameterize")
  "Chez's syntax that psyntax has, exported from (chezscheme) too.")

;;; ------------------------------------------------------------------
;;; The libraries

(defun chez-host-names ()
  "Every host global named chez:..., for (pseudoscheme chez host)."
  (let ((names (remove-duplicates
		(append (loop for (name) in ps-r6rs::*primitives*
			      when (and (> (length name) 5) (string= "chez:" name :end2 5))
				collect name)
			*chez-top-level-primitives*)
		:test #'string=)))
    (remove-if-not (lambda (n) (boundp (psx::location (ssym n)))) names)))

;;; Chez's extensions to R6RS procedures: in Chez, (rnrs) and
;;; (chezscheme) export the same bindings, whose procedures take Chez's
;;; extra arguments, and a program may import both.  So the extensions
;;; are made to the R6RS procedures themselves, which (chezscheme)
;;; re-exports: (dynamic-wind critical? before thunk after), the file
;;; procedures' options ('replace, 'append, ...), delete-file's error?
;;; and file-exists?'s follow?, and eval of one
;;; argument (through psyntax's eval-hook).  Without Chez's arguments they behave as before.  They are
;;; closed primitives (psx::*closed-primitives*): the translator's
;;; built-ins of those names, open-coded, wouldn't take the arguments.

(defun extend-host-procedure (name make &optional arity)
  "Replace host procedure NAME by (funcall MAKE old-procedure).  ARITY,
(min max), says with how many arguments the new procedure is the old:
calls with those are still open-coded."
  (let ((old (psx:host-ref name)))
    (psx:host-set! name (funcall make old))
    (pushnew name psx::*closed-primitives* :test #'string=)
    (when arity
      (pushnew (cons name arity) psx::*extended-primitives* :test #'equal))))

(defun chez-file-options-p (options)
  (and options (not (eq (car options) ps:false))))

;;; In the Chez mode, the mutators refuse immutable objects (host.lisp),
;;; the comparisons check their arguments' types, the vector and string
;;; iterators their arguments, and the constructors of an empty vector,
;;; string or bytevector give the single one.  A reference to such a
;;; primitive there is one to a host global chez:NAME
;;; (psx::*chez-renamed-primitives*), called directly, its usual case
;;; inlined (psx::*inline-primitives*).  Elsewhere they're as before.

(defparameter *chez-checked-mutators*
  '(("vector-set!" 0 "vector") ("vector-fill!" 0 "vector") ("string-set!" 0 "string")
    ("string-fill!" 0 "string") ("bytevector-u8-set!" 0 "bytevector")
    ("bytevector-s8-set!" 0 "bytevector") ("bytevector-fill!" 0 "bytevector")
    ("bytevector-copy!" 2 "bytevector") ("vector-sort!" 1 "vector"))
  "(name position kind): mutator NAME's argument at POSITION is a KIND.")

(defparameter *chez-empty-constructors*
  '("make-vector" "make-string" "make-bytevector" "list->vector" "list->string"
    "substring" "string-append" "string-copy" "bytevector-copy"))

(defparameter *chez-checked-comparisons*
  '(("char=?" . characterp) ("char<?" . characterp) ("char>?" . characterp)
    ("char<=?" . characterp) ("char>=?" . characterp) ("char-ci=?" . characterp)
    ("char-ci<?" . characterp) ("char-ci>?" . characterp) ("char-ci<=?" . characterp)
    ("char-ci>=?" . characterp) ("string=?" . stringp) ("string<?" . stringp)
    ("string>?" . stringp) ("string<=?" . stringp) ("string>=?" . stringp)
    ("string-ci=?" . stringp) ("string-ci<?" . stringp) ("string-ci>?" . stringp)
    ("string-ci<=?" . stringp) ("string-ci>=?" . stringp)))

(defun chez-rename (name function &optional nullary)
  (let ((global (concatenate 'string "chez:" name)))
    (psx:host-set! global function)
    (setf (gethash name psx::*chez-renamed-primitives*) (if nullary (list global) global))))

(defun wrong-argument (who x what)
  (ps-r6rs::r6rs-assertion-violation (ps:intern-scheme-symbol who) (format nil "~~s is not a ~A" what) x))

(defun install-chez-mutators ()
  (setq psx::*canonical-empty* 'ps-r6rs::empty-like)
  (pushnew (cons "base-rtd" ps-r6rs::*base-rtd*) ps:*sharp-bang-data* :test #'equal)
  (loop for (name position kind) in *chez-checked-mutators*
	do (let ((old (psx:host-ref name)) (name name) (position position) (kind kind))
	     (chez-rename name (lambda (&rest args)
				 (ps-r6rs::check-mutable-object name (nth position args) kind)
				 (apply old args)))))
  (flet ((canonical (x)
	   (if (and (typep x '(simple-array * (0))) (not (bit-vector-p x))) (ps-r6rs::empty-like x) x)))
    (dolist (name *chez-empty-constructors*)
      (let ((old (psx:host-ref name)))
	(chez-rename name (lambda (&rest args) (canonical (apply old args))))))
    ;; (vector) and (string): only calls with no arguments
    (chez-rename "vector" (lambda () ps-r6rs::*empty-vector*) t)
    (chez-rename "string" (lambda () ps-r6rs::*empty-string*) t))
  (loop for (name . type) in *chez-checked-comparisons*
	do (let ((old (psx:host-ref name)) (name name) (type type))
	     (chez-rename name (lambda (a &rest more)
				 (dolist (x (cons a more))
				   (unless (funcall type x)
				     (wrong-argument name x (if (eq type 'characterp) "character" "string"))))
				 (apply old a more)))))
  (flet ((iterator (name kind for-each)
	   (let ((old (psx:host-ref name)))
	     (chez-rename name (lambda (f s &rest more)
				 (psx::check-index-loop-arguments kind f (cons s more))
				 (let ((r (apply old f s more)))
				   (if for-each ps:unspecific r)))))))
    (iterator "vector-map" :vector-map nil)
    (iterator "vector-for-each" :vector-for-each t)
    (iterator "string-for-each" :string-for-each t))
  (let ((old (psx:host-ref "vector-sort")))
    (chez-rename "vector-sort" (lambda (proc v)
				 (ps-r6rs::check-procedure "vector-sort" proc)
				 (unless (simple-vector-p v) (wrong-argument "vector-sort" v "vector"))
				 (funcall old proc v)))))

(defun install-chez-extensions ()
  (flet ((dynamic-wind (old)
	   (lambda (a b c &optional (d nil d-p))
	     (if d-p (funcall old b c d) (funcall old a b c))))
	 (open-output (old)
	   (lambda (path &rest options)
	     (if (chez-file-options-p options)
		 (funcall (psx:host-ref "chez:open-output-file") path (car options))
		 (funcall old path))))
	 (call-with-output (old)
	   (lambda (path proc &rest options)
	     (if (chez-file-options-p options)
		 (let ((port (funcall (psx:host-ref "chez:open-output-file") path (car options))))
		   (multiple-value-prog1 (funcall proc port)
		     (close port)))
		 (funcall old path proc))))
	 (with-output (old)
	   (lambda (path thunk &rest options)
	     (if (chez-file-options-p options)
		 (let ((port (funcall (psx:host-ref "chez:open-output-file") path (car options))))
		   (unwind-protect
			(let ((*standard-output* port)) (funcall thunk))
		     (close port)))
		 (funcall old path thunk)))))
    (extend-host-procedure "dynamic-wind" #'dynamic-wind)
    (extend-host-procedure "%full-dynamic-wind" #'dynamic-wind)
    (extend-host-procedure "open-output-file" #'open-output)
    (extend-host-procedure "open-input-file"
			   (lambda (old) (lambda (path &rest options) (declare (ignore options)) (funcall old path))))
    (extend-host-procedure "call-with-input-file"
			   (lambda (old) (lambda (path proc &rest options) (declare (ignore options)) (funcall old path proc))))
    (extend-host-procedure "with-input-from-file"
			   (lambda (old) (lambda (path thunk &rest options) (declare (ignore options)) (funcall old path thunk))))
;; Chez's comparisons take one argument or more, its fx+, fx- and
    ;; fx* any number (fx- one or more)
    (dolist (name '("char=?" "char<?" "char>?" "char<=?" "char>=?"
		    "char-ci=?" "char-ci<?" "char-ci>?" "char-ci<=?" "char-ci>=?"
		    "string=?" "string<?" "string>?" "string<=?" "string>=?"
		    "string-ci=?" "string-ci<?" "string-ci>?" "string-ci<=?" "string-ci>=?"))
      (extend-host-procedure name
			     (lambda (old)
			       (lambda (x &rest more)
				 (if more
				     (apply old x more)
				     ;; the comparison of x with itself checks its type
				     (progn (funcall old x x) ps:true))))
			     '(2 nil)))
    (flet ((arithmetic (name zero one)
	     (extend-host-procedure name
				    (lambda (old)
				      (lambda (&rest args)
					(cond ((null args)
					       (if zero zero (ps:scheme-error "~A: no arguments" name)))
					      ((null (cdr args)) (funcall one old (car args)))
					      (t (reduce old args)))))
				    '(2 2))))
      (arithmetic "fx+" 0 (lambda (old x) (funcall old x 0)))
      (arithmetic "fx*" 1 (lambda (old x) (funcall old x 1)))
      (arithmetic "fx-" nil (lambda (old x) (funcall old 0 x))))
        ;; (delete-file path error?) and (file-exists? path follow?)
    (dolist (name '("delete-file" "file-exists?"))
      (let ((chez (psx:host-ref (concatenate 'string "chez:" name))))
	(extend-host-procedure name
			       (lambda (old)
				 (lambda (path &optional (arg nil arg-p))
				   (cond (arg-p (funcall chez path arg))
					 ;; Chez's delete-file: #f if it can't, unless error?
					 ((and ps:*chez-reader* (string= name "delete-file"))
					  (funcall chez path ps:false))
					 (t (funcall old path))))))))
    ;; (record-constructor rtd): a procedure of every field's value
    (let ((chez (psx:host-ref "chez:record-constructor")))
      (extend-host-procedure "record-constructor"
			     (lambda (old)
			       (lambda (x) (funcall (if (ps-r6rs::rtd-p x) chez old) x)))))
    ;; (record? x rtd): whether x is of type rtd, even if it is opaque
    (extend-host-procedure "record?"
			   (lambda (old)
			     (lambda (x &optional (rtd nil rtd-p))
			       (if rtd-p
				   (progn (ps-r6rs::check-rtd "record?" rtd)
					  (ps-r6rs::bool (ps-r6rs::record-of-type-p x rtd)))
				   ;; a record type descriptor is a record in Chez
				   (if (ps-r6rs::rtd-p x)
				       (ps-r6rs::bool (not (ps-r6rs::rtd-opaque (ps-r6rs::record-rtd x))))
				       (funcall old x)))))
			   '(1 1))
    (extend-host-procedure "call-with-output-file" #'call-with-output)
    (extend-host-procedure "with-output-to-file" #'with-output)
    ;; (rnrs eval)'s eval is psyntax's: with one argument, or the
    ;; interaction environment, it evaluates at the top level running,
    ;; Chez's or the REPL's
    (funcall (psx:host-ref "psyntax:eval-hook")
	     (lambda (form env)
	       (cond ((not (or (eq env ps:false) (eq env *interaction-environment*)))
		      (ps:scheme-error "eval: not an environment: ~S" env))
		     ((at-chez-top-level-p) (eval-at-chez-top-level form))
		     (t (eval-at-current-repl form)))))))

(defun install-chez-libraries ()
  "Install (pseudoscheme chez host), (chezscheme) and Chez's top level.
Called while the standard libraries are installed."
  (install-chez-extensions)
  (install-chez-mutators)
  (install-chez-top-level-primitives)
  (install-host-library '("pseudoscheme" "chez" "host")
			(mapcar (lambda (n) (cons n n)) (chez-host-names)))
  (let ((exports (install-chezscheme-library)))
    ;; (scheme), Chez's library of all of Chez, which the top-level
    ;; module scheme names too (import scheme)
    (psx:eval-library
     (list (ssym "library") (list (ssym "scheme"))
	   (cons (ssym "export") (mapcar #'ssym exports))
	   (list (ssym "import") (list (ssym "chezscheme"))))))
  (install-interaction-library *chez-interaction-library*)
  (pushnew 'load-library-file-at-top-level psx::*library-loaders*))

(defun install-chezscheme-library ()
  (let* ((r6rs (remove-duplicates (psx:table-exports '("r" "mp" "ms" "r5" "ev")) :test #'string=))
	 (exports (remove-duplicates
		   (append r6rs *chezscheme-extras* *chezscheme-from-psyntax*)
		   :test #'string= :from-end t)))
    (psx:eval-library
     (list* (ssym "library") (list (ssym "chezscheme"))
	    (cons (ssym "export")
		  (mapcar #'ssym exports))
	    (read-scheme (format nil "(import (rnrs) (rnrs mutable-pairs) (rnrs mutable-strings) (rnrs r5rs) (rnrs eval)
                                  (only (pseudoscheme r7rs syntax) parameterize include define-values)

                                  (only (pseudoscheme) ~{~A~^ ~})
                                  (prefix (pseudoscheme host) %)
                                  (prefix (pseudoscheme chez host) %))"
				 *chezscheme-from-psyntax*))
	    (loop for file in *chezscheme-sources*
		  append (read-forms (asdf:system-relative-pathname
				      :pseudoscheme (concatenate 'string "src/chez/" file))))))
    exports))

;;; ------------------------------------------------------------------
;;; The top level

(defparameter *chez-interaction-library* '("pseudoscheme" "chez" "interaction")
  "The interaction library of Chez's top level: what scripts and the
REPL run in, with (chezscheme)'s bindings.")

(defvar *chez-file-libraries* 0
  "How many fresh top levels library files have been loaded at.")

(defun install-interaction-library (name)
  (funcall (psx:host-ref "psyntax:install-library")
	   (funcall (psx:host-ref "gensym"))
	   (mapcar #'ssym name)
	   '() '() '() '() '() '() (lambda () ps:unspecific) (lambda () ps:unspecific) t))

(defun at-chez-top-level-p ()
  "Is what's running being evaluated at a Chez top level?"
  (and psx:*host*
       (let ((name (mapcar #'sname* (funcall (psx:host-ref "psyntax:interaction-library-name")))))
	 (and (>= (length name) 2)
	      (equal (subseq name 0 2) '("pseudoscheme" "chez"))))))

(defun eval-at-chez-top-level (form &optional library)
  "Evaluate FORM at Chez's top level: LIBRARY's (a list of strings), or
the top level already running, or the main one."
  (boot)
  (let ((library (or library
		     (and (at-chez-top-level-p)
			  (mapcar #'sname* (funcall (psx:host-ref "psyntax:interaction-library-name"))))
		     *chez-interaction-library*)))
    (with-psyntax-parameter ("psyntax:interaction-library-name" (mapcar #'ssym library))
      (with-psyntax-parameter ("psyntax:interaction-source-name" (list (ssym "chezscheme")))
	;; a body's definitions come before its expressions, as in Chez
	(with-psyntax-parameter ("psyntax:mixed-bodies" ps:false)
	  (eval-top-level-forms form))))))

(defun eval-top-level-forms (form)
  "Evaluate FORM; a (begin form ...) one form at a time, as Chez does at
top level, so that each is expanded after the ones before it have run, and
a library is invoked only when a form that runs references it."
  (when (and (ps::annotation-p form) (head-is (ps::annotation-stripped form) "begin"))
    ;; an annotated begin: its annotated forms
    (let ((parts (ps::annotation-expression form)))
      (when (and (consp parts) (consp (cdr parts)) (null (cdr (last parts))))
	(return-from eval-top-level-forms
	  (progn (mapc #'eval-top-level-forms (butlast (cdr parts)))
		 (eval-top-level-forms (car (last parts))))))))
  (if (and (head-is form "begin") (consp (cdr form)) (null (cdr (last form))))
      (progn (mapc #'eval-top-level-forms (butlast (cdr form)))
	     (eval-top-level-forms (car (last form))))
      (psx:eval-top-level form)))

(defun read-annotated-forms (path name)
  "PATH's forms, read as Chez's load reads them: each part annotated with
its source (src/core.lisp), the file named NAME in its descriptor."
  (let ((ps:*annotating-sfd* (ps::make-sfd name (ps::file-checksum path))))
    (read-forms path)))

(defun load-file-at-chez-top-level (path &optional library (name (namestring path)))
  "Load PATH's forms one by one at Chez's top level (see
EVAL-AT-CHEZ-TOP-LEVEL), annotated with their source in the Chez mode."
  (when (psx::compiled-file-p path)
    (return-from load-file-at-chez-top-level (psx::load-compiled-file path)))
  (let ((*include-directory* (make-pathname :name nil :type nil :defaults (pathname path)))
	(value ps:unspecific))
    (dolist (form (if ps:*chez-reader* (read-annotated-forms path name) (read-forms path)) value)
      (setq value (eval-at-chez-top-level form library)))))

;;; Library files whose library a macro makes, at their own top level

(defun first-datum (path)
  (handler-case (with-open-file (in path)
		  (ps:skip-script-header in)
		  (funcall ps:*scheme-read* in))
    (error () nil)))

(defun library-file-defines-at-top-level-p (path name)
  "Does the file at PATH, found for library NAME, have no library form
of that name, but forms to run at top level?  (Most library files begin
with their library form, and are rejected without reading the rest.)"
  (let ((first (first-datum path)))
    (and (consp first)
	 (not (head-is first "library"))
	 (not (head-is first "define-library"))
	 (notany (lambda (form)
		   (and (or (head-is form "library") (head-is form "define-library"))
			(consp (cdr form))
			(equal (translate-name (cadr form)) name)))
		 (handler-case (read-forms path) (error () nil))))))

(defun load-library-file-at-top-level (name)
  "A loader for psyntax (*LIBRARY-LOADERS*): if library NAME's file
defines it at top level, load the file at a fresh Chez top level, and
return true if that installed the library."
  (let ((name (if (listp (car (last name))) (butlast name) name)))
    (dolist (stem (stems name) nil)
      (dolist (path (psx:candidate-files stem))
	(when (probe-file path)
	  (when (library-file-defines-at-top-level-p path name)
	    (let ((library (list "pseudoscheme" "chez" "file"
				 (princ-to-string (incf *chez-file-libraries*)))))
	      (install-interaction-library library)
	      (load-file-at-chez-top-level path library)
	      (return-from load-library-file-at-top-level
		(ps:truep (funcall (psx:host-ref "psyntax:library-exists?") name)))))
	  ;; the first file found is the library's, whatever it holds
	  (return-from load-library-file-at-top-level nil))))))

;;; ------------------------------------------------------------------
;;; The top level's host procedures

(defparameter *chez-top-level-primitives*
  '("chez:eval-at-top-level" "chez:load" "chez:compile-program" "chez:library-directories" "chez:library-exports"
    "chez:set-library-directories!" "chez:top-level-symbols"
    "chez:set-top-level-value!" "chez:define-top-level-value!" "chez:top-level-bound?"))

(defun quoted (x) (list (ssym "quote") x))

(defun install-chez-top-level-primitives ()
  (psx:defhost "chez:eval-at-top-level" (form) (eval-at-chez-top-level form))
  (psx:defhost "chez:load" (path)
    (load-file-at-chez-top-level (merge-pathnames path) nil (if (stringp path) path (namestring path)))
    ps:unspecific)
  (psx:defhost "chez:compile-program" (in out)
    (compile-chez-program in out))
  (psx:defhost "chez:set-top-level-value!" (symbol value)
    (eval-at-chez-top-level (list (ssym "set!") symbol (quoted value)))
    ps:unspecific)
  (psx:defhost "chez:define-top-level-value!" (symbol value)
    (eval-at-chez-top-level (list (ssym "define") symbol (quoted value)))
    ps:unspecific)
  (psx:defhost "chez:top-level-bound?" (symbol)
    (ps:true? (handler-case (progn (eval-at-chez-top-level symbol) t)
		(error () nil))))
  (psx:defhost "chez:top-level-symbols" ()
    (funcall (psx:host-ref "psyntax:environment-symbols")
	     (funcall (psx:host-ref "psyntax:environment") (list (ssym "chezscheme")))))
  (psx:defhost "chez:library-exports" (name)
    (mapcar #'car (funcall (psx:host-ref "psyntax:library-export-bindings")
			   (translate-name name))))
  ;; Chez's library directories are (source . object) pairs: the sources
  ;; are *LIBRARY-PATH*, which the other modes search too, and the object
  ;; directories, where they differ, *OBJECT-DIRECTORIES*
  (psx:defhost "chez:library-directories" ()
    (mapcar (lambda (d)
	      (let* ((s (if (pathnamep d) (namestring d) d))
		     (o (or (cdr (assoc s *object-directories* :test #'string=)) s)))
		(flet ((chez (dir) (if (string= dir "./") "." dir))) ; as Chez shows it
		  (cons (chez s) (chez o)))))
	    psx:*library-path*))
  (psx:defhost "chez:set-library-directories!" (dirs)
    (let ((pairs (if (stringp dirs)
		     ;; "src1:src2::obj2": :: separates a source and an object directory
		     (let ((parts (uiop:split-string dirs :separator ":")) (pairs '()))
		       (loop while parts
			     do (let ((src (pop parts)))
				  (cond ((and parts (string= (car parts) "") (cdr parts))
					 (pop parts)
					 (push (cons src (pop parts)) pairs))
					((string/= src "") (push (cons src src) pairs)))))
		       (nreverse pairs))
		     (mapcar (lambda (d) (if (consp d) d (cons d d))) dirs))))
      (setf psx:*library-path* (mapcar #'car pairs)
	    *object-directories* (remove-if (lambda (p) (string= (car p) (cdr p))) pairs)))
    ps:unspecific))

(defvar *object-directories* '()
  "Chez's library directories' object directories, as (SOURCE . OBJECT),
where they differ from their source directories.")

;;; ------------------------------------------------------------------
;;; Errors, as Chez reports them (src/chez/conditions.scm says it in
;;; Scheme, for display-condition)

(defun chez-error-text (e)
  "Uncaught condition E as Chez's top level reports it: \"Exception in
car: ...\", \"Exception occurred with non-condition value 42\"."
  (flet ((component (c type) (ps-r6rs::component-of c type))
	 (value (s) (svref (ps-r6rs::record-values s) 0))
	 (written (x) (with-output-to-string (s) (funcall ps:*scheme-write* x s))))
    (if (typep e 'ps-r7rs::uncaught-raise)
	(let ((c (ps-r7rs::uncaught-payload e)))
	  (if (not (ps-r6rs::condition-p* c))
	      (format nil "Exception occurred with non-condition value ~A" (written c))
	      (let ((who (component c "&who"))
		    (message (component c "&message"))
		    (irritants (component c "&irritants")))
		(with-output-to-string (s)
		  (write-string (if (component c "&warning") "Warning" "Exception") s)
		  (let ((who (and who (value who))))
		    (when (and who (not (eq who ps:false)))
		      (format s " in ~A" (if (stringp who) who (written who)))))
		  (format s ": ~A" (if message (value message) ""))
		  (let ((irritants (and irritants (value irritants))))
		    (cond ((null irritants))
			  ((null (cdr irritants)) (format s " with irritant ~A" (written (car irritants))))
			  (t (format s " with irritants ~A" (written irritants)))))))))
	(format nil "Exception: ~A" (pseudoscheme-api-error-message e)))))

(defun pseudoscheme-api-error-message (e)
  ;; pseudoscheme-api is loaded after this system
  (uiop:symbol-call "PSEUDOSCHEME-API" "ERROR-MESSAGE" e))
