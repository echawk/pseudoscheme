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
  '("chezscheme.scm" "lists.scm" "numbers.scm" "hashtables.scm" "threads.scm"
    "conditions.scm" "top-level.scm" "ffi.scm")
  "The files of (chezscheme)'s body, in order.")

(defparameter *chezscheme-extras*
  '(;; chezscheme.scm
    "void" "add1" "sub1" "call/1cc" "gensym" "getenv" "system" "make-parameter"
    "with-input-from-string" "with-output-to-string" "current-directory"
    "file-directory?" "file-regular?" "file-symbolic-link?" "directory-list"
    "mkdir" "delete-directory" "rename-file" "file-modification-time"
    "directory-separator" "machine-type" "get-mode" "chmod" "file-change-time"
    "source-directories" "record-writer" "collect" "weak-cons" "weak-pair?"
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
    ;; hashtables.scm
    "make-weak-eq-hashtable" "make-weak-eqv-hashtable" "make-ephemeron-eq-hashtable"
    "make-ephemeron-eqv-hashtable" "hashtable-weak?" "hashtable-ephemeron?"
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
    "library-requirements" "library-requirements-options" "char-ready?"
    "top-level-value" "set-top-level-value!" "define-top-level-value"
    "top-level-bound?" "top-level-mutable?"
    "load" "load-program" "load-library" "visit" "revisit" "compile-file"
    "compile-library" "compile-program" "compile-script" "compile-to-file"
    "maybe-compile-file" "maybe-compile-library" "maybe-compile-program"
    "library-directories" "library-extensions" "compile-imported-libraries"
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
    "datum" "rec" "eval-when" "$primitive" "syntax->list" "syntax->vector"
    "inspect" "inspect/object" "debug" "break" "procedure-arity-mask"
    ;; ffi.scm
    "load-shared-object" "foreign-procedure" "foreign-entry?" "foreign-entry"
    "foreign-alloc" "foreign-free" "foreign-sizeof" "foreign-ref" "foreign-set!"
    "open-fd-input-port" "open-fd-output-port" "open-fd-input/output-port"
    "port-file-descriptor" "set-port-nonblocking!" "port-nonblocking?"
    "lock-object" "unlock-object" "locked-object?"
    "char-grapheme-break-property" "char-extended-pictographic?" "bytevector"
    "bytevector-truncate!" "port-closed?" "fresh-line" "get-bytevector-some!"
    "put-bytevector-some" "truncate-file" "set-port-length!" "annotation?"
    "annotation-expression" "annotation-stripped" "annotation-source"
    "source-object-bfp" "source-object-efp" "source-file-descriptor"
    "get-datum/annotations" "read-token" "compile-library-handler" "engine-block"
    "date->time-utc" "date-week-day" "define-values" "abort-handler")
  "What src/chez/*.scm defines that (chezscheme) exports, besides R6RS.")

(defparameter *chezscheme-from-psyntax*
  '("module" "import" "alias" "define-property" "library" "meta")
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
;;; procedures' options ('replace, 'append, ...), and eval of one
;;; argument (through psyntax's eval-hook).  Without Chez's arguments they behave as before.  They are
;;; closed primitives (psx::*closed-primitives*): the translator's
;;; built-ins of those names, open-coded, wouldn't take the arguments.

(defun extend-host-procedure (name make)
  "Replace host procedure NAME by (funcall MAKE old-procedure)."
  (let ((old (psx:host-ref name)))
    (psx:host-set! name (funcall make old))
    (pushnew name psx::*closed-primitives* :test #'string=)))

(defun chez-file-options-p (options)
  (and options (not (eq (car options) ps:false))))

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
  (install-chez-top-level-primitives)
  (install-host-library '("pseudoscheme" "chez" "host")
			(mapcar (lambda (n) (cons n n)) (chez-host-names)))
  (install-chezscheme-library)
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
				      :pseudoscheme (concatenate 'string "src/chez/" file))))))))

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
	(eval-top-level-forms form)))))

(defun eval-top-level-forms (form)
  "Evaluate FORM; a (begin form ...) one form at a time, as Chez does at
top level, so that each is expanded after the ones before it have run, and
a library is invoked only when a form that runs references it."
  (if (and (head-is form "begin") (consp (cdr form)) (null (cdr (last form))))
      (progn (mapc #'eval-top-level-forms (butlast (cdr form)))
	     (eval-top-level-forms (car (last form))))
      (psx:eval-top-level form)))

(defun load-file-at-chez-top-level (path &optional library)
  "Load PATH's forms one by one at Chez's top level (see
EVAL-AT-CHEZ-TOP-LEVEL)."
  (let ((*include-directory* (make-pathname :name nil :type nil :defaults (pathname path)))
	(value ps:unspecific))
    (dolist (form (read-forms path) value)
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
  '("chez:eval-at-top-level" "chez:load" "chez:library-directories" "chez:library-exports"
    "chez:set-library-directories!" "chez:top-level-symbols"
    "chez:set-top-level-value!" "chez:define-top-level-value!" "chez:top-level-bound?"))

(defun quoted (x) (list (ssym "quote") x))

(defun install-chez-top-level-primitives ()
  (psx:defhost "chez:eval-at-top-level" (form) (eval-at-chez-top-level form))
  (psx:defhost "chez:load" (path)
    (load-file-at-chez-top-level (merge-pathnames path))
    ps:unspecific)
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
  (psx:defhost "chez:library-directories" ()
    (mapcar (lambda (d) (let ((s (if (pathnamep d) (namestring d) d))) (cons s s)))
	    psx:*library-path*))
  (psx:defhost "chez:set-library-directories!" (dirs)
    (setf psx:*library-path*
	  (cond ((stringp dirs)
		 (remove "" (uiop:split-string dirs :separator ":") :test #'string=))
		(t (mapcar (lambda (d) (if (consp d) (car d) d)) dirs))))
    ps:unspecific))

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
