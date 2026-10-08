; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Booting Guile (docs/guile.md, stage 3): the module system's C half,
;;;; evaluation and loading, the root module's bindings, and loading an
;;;; installed Guile's own ice-9/boot-9.scm.
;;;;
;;;; Guile's Scheme sources are read from the Guile installation (or the
;;;; directory GUILE_SOURCE_DIR names), never copied here: they are
;;;; LGPL-3.0-or-later.  Where one of Guile's modules needs replacing,
;;;; because it is libguile's own business (the compiler, the VM), the
;;;; replacement is written afresh in src/guile/modules/, which comes
;;;; first on %load-path.

(in-package "PSEUDOSCHEME-GUILE")

;;; ------------------------------------------------------------------
;;; Modules
;;;
;;; Before boot-9 has made modules, there is one obarray, the
;;; "pre-modules obarray", of symbol -> variable, and the current module
;;; is #f.  boot-9 then makes the (guile) module around that obarray.
;;; Module records are boot-9's (its `module' record type); these are
;;; their fields, which libguile reads by position as modules.h says.

(defconstant +module-obarray+ 0)
(defconstant +module-uses+ 1)
(defconstant +module-binder+ 2)
(defconstant +module-transformer+ 4)
(defconstant +module-name+ 5)
(defconstant +module-duplicates-handlers+ 7)
(defconstant +module-import-obarray+ 8)
(defconstant +module-public-interface+ 14)

(defvar *obarray* (make-ghash) "The pre-modules obarray.")
(defvar *current-module* ps:false)

(defun obarray-variable (name)
  (let ((handle (gethash name (ghash-table-for *obarray* :eql))))
    (and handle (cdr handle))))

(defun obarray-define (name value)
  (let ((v (obarray-variable name)))
    (if v
	(setf (gvariable-value v) value)
	(setf (gethash name (ghash-table-for *obarray* :eql))
	      (cons name (setq v (make-gvariable value)))))
    v))

(defun root-value (name)
  (let ((v (obarray-variable (ssym name))))
    (and v (not (eq (gvariable-value v) +unbound+)) (gvariable-value v))))

(defun module-slot (m i) (svref (struct-slots-of m) i))

(defun ghash-lookup (h key)
  (if (ghash-p h)
      (let ((handle (gethash key (ghash-table-for h :eql))))
	(and handle (cdr handle)))
      nil))

(defun module-p* (x) (and (struct-p x) (not (vtable-p x))))

(defun module-variable* (module name)
  "libguile's scm_module_variable: MODULE's own variable for NAME, else
an imported one, else its binder's.  MODULE :root is (guile)."
  (when (eq module :root)
    (setq module (or (root-value "the-root-module") ps:false)))
  (if (not (module-p* module))
      (obarray-variable name)
      (or (ghash-lookup (module-slot module +module-obarray+) name)
	  (imported-variable module name)
	  (let ((binder (module-slot module +module-binder+)))
	    (and (functionp binder)
		 (let ((v (funcall binder module name ps:false)))
		   (and (gvariable-p v) v)))))))

(defun module-local-variable* (module name)
  (if (not (module-p* module))
      (obarray-variable name)
      (or (ghash-lookup (module-slot module +module-obarray+) name)
	  (let ((binder (module-slot module +module-binder+)))
	    (and (functionp binder)
		 (let ((v (funcall binder module name ps:false)))
		   (and (gvariable-p v) v)))))))

(defun imported-variable (module name)
  (let ((imports (module-slot module +module-import-obarray+)))
    (or (ghash-lookup imports name)
	(let ((found nil) (found-iface nil))
	  (dolist (iface (module-slot module +module-uses+))
	    (let ((v (module-variable* iface name)))
	      (when v
		(if found
		    (let ((winner (resolve-duplicate-binding module name found-iface found iface v)))
		      (when (eq winner v) (setq found-iface iface))
		      (setq found winner))
		    (setq found v found-iface iface)))))
	  (when (and found (ghash-p imports))
	    (let ((table (ghash-table-for imports :eql)))
	      (setf (gethash name table) (cons name found))))
	  found))))

(defun resolve-duplicate-binding (module name iface1 var1 iface2 var2)
  (if (eq var1 var2)
      var1
      (let ((handlers (module-slot module +module-duplicates-handlers+)))
	(unless (consp handlers)
	  (let ((default (root-value "default-duplicate-binding-procedures")))
	    (setq handlers (and default (funcall default)))))
	(flet ((val (v) (if (eq (gvariable-value v) +unbound+) ps:false (gvariable-value v))))
	  (dolist (h (if (listp handlers) handlers '()) var2)
	    (let ((r (funcall h module name iface1 (val var1) iface2 (val var2) ps:false ps:false)))
	      (when (gvariable-p r) (return r))))))))

(defun resolve-module* (name)
  "boot-9's resolve-module, for (@ (module) name)."
  (let ((resolve (root-value "resolve-module")))
    (if resolve
	(let ((m (funcall resolve name)))
	  (and (truthy m) m))
	nil)))

(defun module-public-interface* (module)
  (let ((iface (module-slot module +module-public-interface+)))
    (and (truthy iface) iface)))

(defparameter *boot-overrides*
  '(("dynamic-wind" . dynamic-wind-value)
    ("with-fluid*" . with-fluid-value)
    ("with-dynamic-state" . with-dynamic-state-value)
    ("call-with-current-continuation" . call/cc-value)
    ("call-with-values" . call-with-values-value)
    ("apply" . apply-value)
    ("call-with-prompt" . call-with-prompt-value))
  "Procedures boot-9 defines on libguile's internal primitives (wind,
push-fluid, ...), which are Pseudoscheme's own procedures instead: the
name, and a function returning the replacement.")

(defun host-procedure (name)
  "The host's procedure NAME, or with full continuations its frame-aware
version (src/continuations.lisp)."
  (let ((full (and psx::*full-continuations*
		   (cdr (assoc name psx::*full-replacements* :test #'string=)))))
    (psx:host-ref (or full name))))

(defun dynamic-wind-value () (host-procedure "dynamic-wind"))
(defun with-fluid-value () (gethash "with-fluid*" *guile-primitives*))
(defun with-dynamic-state-value () (gethash "with-dynamic-state" *guile-primitives*))
(defun call/cc-value () (host-procedure "call-with-current-continuation"))
(defun call-with-values-value () (host-procedure "call-with-values"))
(defun apply-value ()
  ;; #nil ends a list as () does, for Emacs Lisp's sake
  (let ((apply (psx:host-ref "apply")))
    (lambda (f &rest args)
      (let ((tail (last args)))
	(when (and tail (eq (car tail) *elisp-nil*))
	  (setq args (append (butlast args) (list '()))))
	(when (and tail (not (listp (car tail))))
	  (guile-error (ssym "wrong-type-arg") "apply" "Apply to non-list: ~S" (list (car tail))
		       (list (car tail)))))
      (apply apply f args))))
(defun call-with-prompt-value () (gethash "call-with-prompt" *guile-primitives*))

(defvar *raise-exception* nil "boot-9's raise-exception, once defined.")

(defun define-in-module (module name value)
  "A top-level definition of NAME in MODULE (#f before modules boot)."
  (let ((root (not (module-p* module))))
    (when root
      ;; before the module system: boot-9's own definitions
      (let ((override (cdr (assoc (ps:scheme-symbol-name name) *boot-overrides* :test #'string=))))
	(when override (setq value (funcall override)))))
    (when (and (functionp value) (not (gethash value *procedure-properties*)))
      (setf (gethash value *procedure-properties*) (list (cons (ssym "name") name))))
    (if root
	(obarray-define name value)
	(funcall (root-value "module-define!") module name value))
    (when (or root (eq module (root-value "the-root-module")))
      (let ((s (ps:scheme-symbol-name name)))
	(cond ((string= s "throw") (setq *throw* value))
	      ((string= s "raise-exception") (setq *raise-exception* value)))))
    *unspecified*))

;;; ------------------------------------------------------------------
;;; Evaluation

(defun current-transformer ()
  (or (and (module-p* *current-module*)
	   (let ((tr (module-slot *current-module* +module-transformer+)))
	     (and (functionp tr) tr)))
      (root-value "macroexpand")
      #'pre-expand))

(defun eval-tree-il (tree)
  (let* ((*lexicals* (make-hash-table :test 'eq))
	 (*compile-module* *current-module*)
	 (core (compile-tree-il tree)))
    (psx:host-eval (if (> (psx::tree-size core) psx::*letrec-definitions-limit*)
		       (flatten-top-level-lets core)
		       core))))

;;; A big top-level form is compiled in pieces: src/psyntax.lisp hoists
;;; the definitions of a top-level letrec* (each procedure its own
;;; code object), and here the lets around it, such as psyntax-pp.scm's
;;; (let ((syntax? ...) ...) (letrec* (...) ...)), are made top-level
;;; definitions first.  Lexical names are unique in the session (~1,
;;; ~2, ...), so the definitions can't collide.

(defun flatten-top-level-lets (form)
  (labels ((kw (x name) (and (symbolp x) (string= (symbol-name x) name)))
	   (false-p (x) (and (consp x) (kw (car x) "QUOTE") (eq (cadr x) ps:false)))
	   (let-p (x)
	     (and (consp x) (consp (car x)) (kw (caar x) "LAMBDA")
		  (listp (cadar x)) (null (cdr (last (cadar x))))
		  (= (length (cddar x)) 1)
		  (= (length (cadar x)) (length (cdr x)))
		  (cadar x)
		  (notevery #'false-p (cdr x)))))
    (if (let-p form)
	`(,(core "begin")
	  ,@(mapcar (lambda (v e) (list (core "define") v e)) (cadar form) (cdr form))
	  ,(flatten-top-level-lets (caddar form)))
	form)))

(defun primitive-eval (x)
  (eval-tree-il (if (node-type x) x (funcall (current-transformer) x))))

(defun guile-eval (x module)
  (let ((*current-module* (if (module-p* module) module *current-module*)))
    (primitive-eval x)))

;;; ------------------------------------------------------------------
;;; Errors: a Lisp error in Guile code is raised as a Guile exception,
;;; in the dynamic context of the error, as libguile's own errors are.

(defun condition-throw-arguments (c)
  "The key and throw arguments for Lisp condition C."
  (flet ((message (key subr message args)
	   (values (ssym key) (list subr message args ps:false))))
    (typecase c
      (guile-throw (values (guile-throw-key c) (guile-throw-args c)))
      (type-error
       (message "wrong-type-arg" ps:false "Wrong type argument: ~S"
		(list (type-error-datum c))))
      (division-by-zero (message "numerical-overflow" ps:false "Numerical overflow" '()))
      (sb-int:simple-program-error
       (let ((text (princ-to-string c)))
	 (if (search "number of arguments" text)
	     (message "wrong-number-of-args" ps:false "Wrong number of arguments (~A)" (list text))
	     (message "wrong-number-of-args" ps:false "~A" (list text)))))
      (t (let ((text (remove #\Newline (princ-to-string c))))
	   (if (or (search "isn't a pair" text) (search ": not a " text) (search ": not an " text))
	       (message "wrong-type-arg" ps:false "Wrong type argument: ~A" (list text))
	       (message "misc-error" ps:false "~A" (list text))))))))

(defvar *trace-lisp-errors* nil "Print a backtrace of each Lisp error raised in Guile code.")

(defun lisp-error->guile (c)
  (when (and *trace-lisp-errors* (not (typep c 'guile-throw)))
    (format *trace-output* "~&;; Lisp error: ~A~%" c)
    (sb-debug:print-backtrace :count 25 :stream *trace-output*))
  (when (and *throw* (not (typep c 'guile-throw)))
    (multiple-value-bind (key args) (condition-throw-arguments c)
      (handler-bind ((error #'lisp-error->guile))
	(call-throw key args)))))

(defmacro with-guile-errors (&body body)
  "Run BODY as Guile code: Lisp errors raised as Guile exceptions, and
inside one continuation base (src/continuations.lisp), so that the
forms it evaluates see the prompts (catch, with-exception-handler)
set up around them.  Floating-point traps are off, so that overflow
gives an infinity and 0.0/0.0 a NaN, as in Guile."
  `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
     (handler-bind ((error #'lisp-error->guile))
       (psx::call-with-continuation-base (lambda () ,@body)))))

;;; ------------------------------------------------------------------
;;; Loading

(defvar *guile-source-directory* nil "Where Guile's Scheme sources are.")

(defun find-guile-sources ()
  (let ((env (uiop:getenv "GUILE_SOURCE_DIR")))
    (or (and env (probe-file (uiop:ensure-directory-pathname env)))
	(let ((dir (ignore-errors
		    (string-trim '(#\Newline #\Space)
				 (uiop:run-program '("guile" "-c" "(display (%package-data-dir))")
						   :output :string :ignore-error-status t)))))
	  (and dir (plusp (length dir))
	       (probe-file (format nil "~A/3.0/ice-9/boot-9.scm" dir))
	       (uiop:ensure-directory-pathname (format nil "~A/3.0" dir))))
	(loop for d in '("/opt/homebrew/share/guile/3.0/" "/usr/local/share/guile/3.0/"
			 "/usr/share/guile/3.0/")
	      when (probe-file (merge-pathnames "ice-9/boot-9.scm" d)) return (pathname d))
	(error "Guile's Scheme sources weren't found: set GUILE_SOURCE_DIR to the directory with ice-9/boot-9.scm"))))

(defun our-modules-directory ()
  (asdf:system-relative-pathname :pseudoscheme "src/guile/modules/"))

(defun load-path ()
  (let ((v (obarray-variable (ssym "%load-path"))))
    (if v (gvariable-value v) '())))

(defun search-load-path (filename &optional (extensions '("" ".scm")))
  (if (uiop:absolute-pathname-p filename)
      (and (probe-file filename) filename)
      (loop for dir in (load-path)
	    do (loop for ext in extensions
		     for path = (format nil "~A/~A~A" (string-right-trim "/" dir) filename ext)
		     when (and (probe-file path) (not (uiop:directory-exists-p path)))
		       do (return-from search-load-path path)))))

(defvar *current-load-file* nil)
(defvar *current-reader* (make-fluid* ps:false) "Guile's current-reader fluid.")

(defvar *trace-loads* nil)

(defun primitive-load (filename)
  (when *trace-loads* (format *trace-output* "~&;; loading ~A~%" filename))
  (with-open-file (in filename :external-format :utf-8)
    (let ((*current-load-file* filename)
	  (*guile-fold-case* nil))
      (loop for form = (let ((reader (fluid-value *current-reader*)))
			 (if (functionp reader) (funcall reader in) (guile-read in)))
	    until (eq form ps:eof-object)
	    do (when *trace-loads*
		 (format *trace-output* "~&;;   ~A~%" (subseq (with-output-to-string (s) (funcall ps:*scheme-write* form s)) 0 (min 100 (length (with-output-to-string (s) (funcall ps:*scheme-write* form s)))))))
	       (primitive-eval form))))
  *unspecified*)

(defun primitive-load-path (name &optional (exception-on-not-found ps:true))
  (let ((path (search-load-path name)))
    (cond (path (primitive-load path))
	  ((truthy exception-on-not-found)
	   (guile-error (ssym "system-error") "primitive-load-path"
			"Unable to find file ~S in load path" (list name)))
	  (t ps:false))))

;;; ------------------------------------------------------------------
;;; The root module's bindings

(defparameter *not-aliased*
  '("raise" "sort" "sort!" "format" "simple-format" "eval" "gensym" "random"
    "string-for-each" "string-map" "read" "delete" "delete!" "string-index"
    "string-rindex" "make-hash-table" "hash-table?" "hash-ref" "hash-set!" "hash-remove!"
    "string-split" "string-join" "list-index" "write" "display" "1+" "1-"
    "string-copy!" "vector-copy!" "string-fill!" "vector-fill!" "copy-file"
    "keyword?" "symbol->keyword" "keyword->symbol" "make-struct" "dynamic-wind"
    "call-with-prompt" "abort-to-prompt" "abort-to-prompt*" "error" "exit" "load")
  "Root primitives Pseudoscheme has a procedure of the same name for, but
not with Guile's behaviour.")

(defun root-primitive-names ()
  (with-open-file (in (asdf:system-relative-pathname :pseudoscheme "src/guile/root-primitives.txt"))
    (loop for line = (read-line in nil) while line
	  unless (zerop (length line)) collect line)))

(defun host-bound-p (name)
  (let ((loc (ignore-errors (psx:location (ssym name)))))
    (and loc (boundp loc))))

(defun install-root-bindings ()
  (setq *obarray* (make-ghash) *current-module* ps:false *throw* nil *raise-exception* nil)
  ;; Pseudoscheme's own procedures, where they are Guile's
  (dolist (name (root-primitive-names))
    (when (and (not (member name *not-aliased* :test #'string=))
	       (host-bound-p name))
      (obarray-define (ssym name) (psx:host-ref name))))
  (obarray-define (ssym "call/cc") (call/cc-value))
  (obarray-define (ssym "call-with-current-continuation") (call/cc-value))
  (obarray-define (ssym "call-with-values") (call-with-values-value))
  (obarray-define (ssym "dynamic-wind") (dynamic-wind-value))
  ;; the ones written for Guile (src/guile/runtime.lisp)
  (install-library-bindings)
  (maphash (lambda (name f) (obarray-define (ssym name) f)) *guile-primitives*)
  (install-port-root-bindings)
  (install-regex-root-bindings)
  (install-root-variables))

;;; Root primitives that are SRFI 13's and SRFI 14's, as in Guile, taken
;;; from Pseudoscheme's (srfi 13) and (srfi 14).

(defparameter *library-bindings*
  '(("(srfi 13)" "string-null?" "string-tabulate" "reverse-list->string" "string-join"
     "substring/shared" "string-take" "string-take-right" "string-drop" "string-drop-right"
     "string-pad" "string-pad-right" "string-trim" "string-trim-right" "string-trim-both"
     "string-compare" "string-compare-ci" "string=" "string<>" "string<" "string>" "string<="
     "string>=" "string-ci=" "string-ci<>" "string-ci<" "string-ci>" "string-ci<=" "string-ci>="
     "string-hash" "string-hash-ci" "string-prefix-length" "string-suffix-length"
     "string-prefix-length-ci" "string-suffix-length-ci" "string-prefix?" "string-suffix?"
     "string-prefix-ci?" "string-suffix-ci?" "string-index" "string-index-right" "string-skip"
     "string-skip-right" "string-count" "string-contains" "string-contains-ci"
     "string-titlecase!" "string-upcase!" "string-downcase!" "string-reverse" "string-reverse!"
     "string-concatenate" "string-concatenate/shared" "string-concatenate-reverse"
     "string-concatenate-reverse/shared" "string-fold" "string-fold-right" "string-unfold"
     "string-unfold-right" "string-for-each" "string-map" "string-map!" "string-for-each-index"
     "string-replace" "string-tokenize" "string-filter" "string-delete" "xsubstring"
     "string-xcopy!")
    ("(srfi 14)" "char-set?" "char-set=" "char-set<=" "char-set-hash" "char-set-cursor"
     "char-set-ref" "char-set-cursor-next" "end-of-char-set?" "char-set-fold" "char-set-unfold"
     "char-set-unfold!" "char-set-for-each" "char-set-map" "char-set-copy" "char-set"
     "list->char-set" "string->char-set" "list->char-set!" "string->char-set!" "char-set-filter"
     "ucs-range->char-set" "->char-set" "char-set-filter!" "ucs-range->char-set!"
     "char-set->list" "char-set->string" "char-set-size" "char-set-count" "char-set-contains?"
     "char-set-every" "char-set-any" "char-set-adjoin" "char-set-delete" "char-set-adjoin!"
     "char-set-delete!" "char-set-complement" "char-set-union" "char-set-intersection"
     "char-set-complement!" "char-set-union!" "char-set-intersection!" "char-set-difference"
     "char-set-xor" "char-set-diff+intersection" "char-set-difference!" "char-set-xor!"
     "char-set-diff+intersection!" "char-set:lower-case" "char-set:upper-case"
     "char-set:title-case" "char-set:letter" "char-set:digit" "char-set:letter+digit"
     "char-set:graphic" "char-set:printing" "char-set:whitespace" "char-set:iso-control"
     "char-set:punctuation" "char-set:symbol" "char-set:hex-digit" "char-set:blank"
     "char-set:ascii" "char-set:empty" "char-set:full"))
  "(library name ...): root bindings that are these libraries' own.")

(defvar *library-values* (make-hash-table :test 'equal))

(defun library-values (library names)
  "The values of NAMES (strings) in LIBRARY (text, as R7RS names it)."
  (ps-r7rs::eval-forms
   (list (ps-r7rs::read-scheme (format nil "(import (only (scheme base) list) (only ~A ~{~A ~}))" library names))
	 (cons (ssym "list") (mapcar #'ssym names)))))

(defun library-value (library name)
  (or (gethash (cons library name) *library-values*)
      (setf (gethash (cons library name) *library-values*)
	    (car (library-values library (list name))))))

(defun install-library-bindings ()
  (loop for (library . names) in *library-bindings*
	do (loop for name in names
		 for value in (library-values library names)
		 do (setf (gethash (cons library name) *library-values*) value)
		    (obarray-define (ssym name) value))))

(defun install-root-variables ()
  (flet ((def (name value) (obarray-define (ssym name) value)))
    (def "%expanded-vtables" *expanded-vtables*)
    (def "<standard-vtable>" *standard-vtable*)
    (def "<applicable-struct-vtable>" *applicable-struct-vtable*)
    (def "<applicable-struct-with-setter-vtable>" *applicable-struct-with-setter-vtable*)
    (def "standard-vtable-fields" *standard-vtable-fields*)
    (def "vtable-offset-user" +vtable-offset-user+)
    (def "vtable-index-layout" +vtable-index-layout+)
    (def "vtable-index-printer" +vtable-index-printer+)
    (def "vtable-index-size" +vtable-index-size+)
    (def "most-positive-fixnum" most-positive-fixnum)
    (def "most-negative-fixnum" most-negative-fixnum)
    (def "*unspecified*" *unspecified*)
    (def "macroexpand" #'pre-expand)
    (def "%load-path" (list (namestring (our-modules-directory))
			    (namestring *guile-source-directory*)))
    (def "%load-extensions" (list ".scm" ""))
    (loop for (name . value) in *locale-categories* do (def name value))
    (def "%load-compiled-path" '())
    (def "%load-compiled-extensions" (list ".go"))
    (def "%load-should-auto-compile" ps:false)
    (def "%fresh-auto-compile" ps:false)
    (def "%load-verbosely" ps:false)
    (def "%load-hook" ps:false)
    (def "%stacks" (make-fluid* ps:false))
    (def "*random-state*" *guile-random-state*)
    (def "after-gc-hook" (make-hook* 0))
    (def "signal-handlers" (make-array 32 :initial-element ps:false))
    (def "source-whash" (make-ghash :key))
    (def "%sizeof-struct-pollfd" 8)
    (loop for (name . bit) in '(("validated" . 0) ("vtable" . 1) ("applicable-vtable" . 2)
				("applicable" . 3) ("setter-vtable" . 4) ("setter" . 5)
				("goops-class" . 9) ("goops-slot" . 10) ("goops-static-slot-allocation" . 11)
				("goops-indirect" . 12) ("goops-needs-migration" . 13))
	  do (def (format nil "vtable-flag-~A" name) (ash 1 bit)))
    (def "current-reader" *current-reader*)
    (def "%compile-fallback-path" ps:false)
    (def "%auto-compilation-options" '())
    (def "%file-port-name-canonicalization" (make-fluid* ps:false))
    (def "%guile-build-info" '())
    (def "%host-type" "aarch64-apple-darwin")
    (def "*features*" (mapcar #'ssym '("guile" "r7rs" "srfi-0" "srfi-4" "srfi-6" "srfi-13" "srfi-14")))
    (def "%exception-handler" (make-fluid* ps:false))
    (def "%exception-epoch" (make-fluid* 1))
    (def "%init-exceptions!" (lambda (&rest types) (declare (ignore types)) *unspecified*))
    (def "%read-hash-procedures" (make-fluid* '()))
    (def "read-hash-extend" (install-read-hash-extend))
    (def "file-name-separator-string" "/")
    (def "internal-time-units-per-second" internal-time-units-per-second)
    (def "%get-pre-modules-obarray" (lambda () *obarray*))
    (def "current-module" (lambda () *current-module*))
    (def "set-current-module" (lambda (m) (setq *current-module* m) *unspecified*))
    (def "module-variable" (lambda (m name) (or (module-variable* m name) ps:false)))
    (def "module-local-variable" (lambda (m name) (or (module-local-variable* m name) ps:false)))
    (def "module-transformer" (lambda (m) (if (module-p* m) (module-slot m +module-transformer+) ps:false)))
    (def "define!" (lambda (name value) (define-in-module *current-module* name value)))
    (def "primitive-eval" #'primitive-eval)
    (def "%eval-tree-il" (lambda (tree module)
			   (let ((*current-module* (if (module-p* module) module *current-module*)))
			     (eval-tree-il tree))))
    (def "eval" #'guile-eval)
    (def "primitive-load" #'primitive-load)
    (def "primitive-load-path" #'primitive-load-path)
    (def "%search-load-path" (lambda (name) (or (search-load-path name) ps:false)))
    (def "read" (lambda (&optional (port *standard-input*)) (guile-read port)))
    (def "primitive-read" (lambda (&optional (port *standard-input*)) (guile-read port)))
    (def "write" (lambda (x &optional (port *standard-output*)) (funcall ps:*scheme-write* x port) *unspecified*))
    (def "display" (lambda (x &optional (port *standard-output*)) (funcall ps:*scheme-display* x port) *unspecified*))
    (def "simple-format" #'simple-format)
    (def "format" #'simple-format)
    (def "gensym" (gethash "gensym" *guile-primitives*))
    (def "memoize-expression" (lambda (x) x))
    ;; until boot-9 defines its own, for errors while booting
    (def "print-exception" (lambda (port frame key args)
			     (declare (ignore frame))
			     (format port "~A: " key)
			     (funcall ps:*scheme-write* args port)
			     (terpri port)
			     *unspecified*))
    (def "macroexpanded?" (lambda (x) (bool (node-type x))))))

(defun simple-format (destination message &rest args)
  "Guile's simple-format: ~A and ~S (and ~%, ~~)."
  (let ((out (make-string-output-stream)))
    (loop with i = 0
	  while (< i (length message))
	  do (let ((c (char message i)))
	       (if (and (char= c #\~) (< (1+ i) (length message)))
		   (let ((d (char message (1+ i))))
		     (case (char-downcase d)
		       (#\a (funcall ps:*scheme-display* (pop args) out))
		       (#\s (funcall ps:*scheme-write* (pop args) out))
		       (#\% (terpri out))
		       (#\~ (write-char #\~ out))
		       (t (write-char c out) (write-char d out)))
		     (incf i 2))
		   (progn (write-char c out) (incf i)))))
    (let ((string (get-output-stream-string out)))
      (cond ((eq destination ps:false) string)
	    ((eq destination ps:true) (write-string string *standard-output*) *unspecified*)
	    (t (write-string string destination) *unspecified*)))))

;;; ------------------------------------------------------------------
;;; Boot

(defvar *booted* nil)

(defun boot (&key (verbose nil))
  "Load Guile's boot-9.scm, once."
  (unless *booted*
    (pseudoscheme-api::ensure-psyntax)
    (setq *guile-source-directory* (find-guile-sources))
    (install-compiler-primitives)
    (install-root-bindings)
    (let ((start (get-internal-real-time)))
      (with-guile-errors
	(primitive-load-path "ice-9/boot-9"))
      (when verbose
	(format *error-output* "~&;; boot-9 loaded in ~,1F s~%"
		(/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (setq *booted* t))
  t)

;;; ------------------------------------------------------------------
;;; Entry points

(defun call-with-guile-catch (thunk)
  "Call THUNK; a Guile exception it doesn't handle is signalled as a
Lisp GUILE-THROW, rather than ending the process as Guile's uncaught
exceptions do."
  (with-guile-errors
    (funcall (root-value "catch") ps:true thunk
	     (lambda (key &rest args) (error 'guile-throw :key key :args args)))))

(defun eval-string (string)
  "Evaluate the forms in STRING at Guile's top level (guile-user, after
boot); the last form's values."
  (boot)
  (with-input-from-string (in string)
    (let ((values (list *unspecified*)))
      (loop for form = (guile-read in)
	    until (eq form ps:eof-object)
	    do (setq values (multiple-value-list
			     (call-with-guile-catch (lambda () (primitive-eval form))))))
      (values-list values))))

(defun load-file (path)
  "Load the Guile source file PATH at the current top level."
  (boot)
  (call-with-guile-catch (lambda () (primitive-load (namestring (merge-pathnames path))))))

(defun error-text (condition)
  "CONDITION as Guile prints an uncaught exception: by print-exception."
  (if (and (typep condition 'guile-throw) (root-value "print-exception"))
      (with-output-to-string (s)
	(ignore-errors
	 (funcall (root-value "print-exception") s ps:false
		  (guile-throw-key condition) (guile-throw-args condition))))
      (princ-to-string condition)))

(defun repl (&key (input *standard-input*) (output *standard-output*))
  "Guile's REPL, more or less: each value printed as $N = value."
  (boot)
  (let ((n 0))
    (loop
      (format output "scheme@~A> "
	      (with-output-to-string (s)
		(funcall ps:*scheme-write*
			 (funcall (root-value "module-name") *current-module*) s)))
      (finish-output output)
      (let ((form (handler-case (guile-read input)
		    (error (e) (format output "~&ERROR: ~A~%" e) (clear-input input) nil))))
	(cond ((eq form ps:eof-object) (terpri output) (return))
	      ((null form))
	      (t (handler-case
		     (dolist (v (multiple-value-list
				 (call-with-guile-catch (lambda () (primitive-eval form)))))
		       (unless (eq v *unspecified*)
			 (format output "$~D = ~A~%" (incf n)
				 (with-output-to-string (s) (funcall ps:*scheme-write* v s)))))
		   (error (e) (format output "~&~A~%" (error-text e))))))))))
