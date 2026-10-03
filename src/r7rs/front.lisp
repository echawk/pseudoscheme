; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R7RS -*-

;;;; R7RS on psyntax
;;;;
;;;; R7RS libraries are R6RS libraries with a different surface, and
;;;; psyntax has the library system, so R7RS runs through it too:
;;;;
;;;;  * (pseudoscheme host) is a library psyntax installs at boot whose
;;;;    exports are host globals (procedures from the R7RS and R6RS layers
;;;;    that aren't in psyntax's own R6RS tables).
;;;;  * (pseudoscheme r7rs syntax) -- src/r7rs/syntax.sls -- defines the
;;;;    syntax R7RS adds or spells differently.
;;;;  * The (scheme ...) libraries are generated from Appendix A (exports.lisp):
;;;;    syntax from (rnrs ...) or (pseudoscheme r7rs syntax), procedures
;;;;    from (pseudoscheme host).
;;;;  * define-library is translated into an R6RS library form.  R7RS names
;;;;    with integers, (srfi 1), become R6RS-style (srfi :1), as Akku does,
;;;;    so the two naming conventions meet.
;;;;
;;;; Programs run as psyntax top-level programs; the REPL is psyntax's
;;;; interaction environment with an R7RS library of its own.

(in-package "PSEUDOSCHEME-R7RS")

(defun ssym (string) (ps:intern-scheme-symbol string))
(defun sname* (x) (if (symbolp x) (ps:scheme-symbol-name x) (princ-to-string x)))
(defun head-is (form name)
  (and (consp form) (symbolp (car form)) (string= (ps:scheme-symbol-name (car form)) name)))

;;; ------------------------------------------------------------------
;;; Synthesized libraries over host globals

(defun install-host-library (name exports)
  "Install library NAME (a list of strings) whose EXPORTS, a list of
(external-name . host-global-name) strings, are host globals."
  (let* ((gensym (psx:host-ref "gensym"))
	 (labels (mapcar (lambda (e) (declare (ignore e)) (funcall gensym)) exports))
	 (subst (mapcar (lambda (e l) (cons (ssym (car e)) l)) exports labels))
	 (env (mapcar (lambda (e l) (cons l (cons (ssym "core-prim") (ssym (cdr e))))) exports labels)))
    (funcall (psx:host-ref "psyntax:install-library")
	     (funcall gensym) (mapcar #'ssym name) '() '() '() '()
	     subst env (lambda () ps:unspecific) (lambda () ps:unspecific) t)))

(defparameter *host-extras*
  '("%parameterize" "$delay-force" "cond-expand-satisfied?" "read-file-forms"
    "r7rs:error" "r7rs:bytevector-copy!" "r7rs:load" "r7rs:translate-import-set"
    "make-parameter" "make-list" "gensym" "open-input-string" "open-output-string" "get-output-string"
    "chez:system" "chez:with-input-from-string" "chez:with-output-to-string"
    "chez:current-directory" "chez:file-directory?" "chez:file-regular?"
    "chez:file-symbolic-link?" "chez:directory-list" "chez:mkdir"
    "chez:delete-directory" "chez:rename-file" "chez:file-modification-time"
    "chez:library-directories" "chez:timezone-offset")
  "Host globals (pseudoscheme host) exports besides R7RS procedure names.")

(defun host-library-exports ()
  (let ((names (remove-duplicates
		(append (loop for (nil s) in *standard-libraries*
			      append (remove-if #'syntax-export-p (split-names s)))
			*host-extras*)
		:test #'string=)))
    (mapcar (lambda (n) (cons n n))
	    (remove-if-not (lambda (n) (boundp (psx::location (ssym n)))) names))))

;;; ------------------------------------------------------------------
;;; Names and import sets

(defun translate-name (name)
  "R7RS library name -> psyntax's: integers become :n symbols."
  (mapcar (lambda (part) (if (integerp part) (ssym (format nil ":~D" part)) part)) name))

(defun translate-import-set (spec)
  (cond ((or (head-is spec "only") (head-is spec "except"))
	 (list* (car spec) (translate-import-set (cadr spec)) (cddr spec)))
	((head-is spec "prefix")
	 (list (car spec) (translate-import-set (cadr spec)) (caddr spec)))
	((head-is spec "rename")
	 ;; R7RS (rename set (a b) ...) is R6RS's too
	 (list* (car spec) (translate-import-set (cadr spec)) (cddr spec)))
	((consp spec) (translate-name spec))
	(t spec)))

(defun translate-export-spec (spec)
  ;; R7RS (rename internal external) -> R6RS (rename (internal external))
  (if (head-is spec "rename")
      (list (car spec) (list (cadr spec) (caddr spec)))
      spec))

;;; ------------------------------------------------------------------
;;; define-library -> library

(defvar *include-directory* nil
  "Directory relative to which include files are resolved: the directory
of the library or program being processed.")

(defun read-forms (path &key fold-case)
  (let ((ps:*fold-case* fold-case))
    (with-open-file (in path)
      (loop for form = (funcall ps:*scheme-read* in)
	    until (eq form ps:eof-object)
	    collect form))))

(defun include-path (file)
  (merge-pathnames file (or *include-directory* *default-pathname-defaults*)))

(defun feature-satisfied-p (req)
  (cond ((symbolp req)
	 (or (string= (ps:scheme-symbol-name req) "else")
	     (member (ps:scheme-symbol-name req) psl:*scheme-features* :test #'string=)))
	((head-is req "and") (every #'feature-satisfied-p (cdr req)))
	((head-is req "or") (some #'feature-satisfied-p (cdr req)))
	((head-is req "not") (not (feature-satisfied-p (cadr req))))
	((head-is req "library") (library-available-p (translate-name (cadr req))))
	(t nil)))

(defun library-available-p (name)
  (or (ps:truep (funcall (psx:host-ref "psyntax:library-exists?") name))
      (and (locate-library-form name) t)))

(defun flatten-declarations (decls)
  "Expand cond-expand and include-library-declarations among define-library
declarations."
  (loop for d in decls
	append (cond ((head-is d "cond-expand")
		      (flatten-declarations
		       (loop for clause in (cdr d)
			     when (feature-satisfied-p (car clause)) return (cdr clause))))
		     ((head-is d "include-library-declarations")
		      (flatten-declarations
		       (loop for f in (cdr d) append (read-forms (include-path f)))))
		     (t (list d)))))

(defun translate-define-library (form)
  "An R6RS library form equivalent to the R7RS define-library FORM."
  (let ((exports '()) (imports '()) (body '()))
    (dolist (d (flatten-declarations (cddr form)))
      (cond ((head-is d "export") (setq exports (append exports (mapcar #'translate-export-spec (cdr d)))))
	    ((head-is d "import") (setq imports (append imports (mapcar #'translate-import-set (cdr d)))))
	    ((head-is d "begin") (setq body (append body (cdr d))))
	    ((or (head-is d "include") (head-is d "include-ci"))
	     (setq body (append body (loop for f in (cdr d)
					   append (read-forms (include-path f)
							      :fold-case (head-is d "include-ci"))))))
	    (t (error "define-library: unknown declaration ~S" d))))
    (list* (ssym "library") (translate-name (cadr form))
	   ;; An identical export listed twice is harmless in R7RS systems
	   ;; (and occurs in real libraries); psyntax rejects it.
	   (cons (ssym "export") (remove-duplicates exports :test #'equal :from-end t))
	   (cons (ssym "import") imports)
	   body)))

(defun resolve-includes (form)
  "Make the file names of (include \"file\" ...) forms in FORM absolute,
relative to *INCLUDE-DIRECTORY*: psyntax expands the include later, after
we no longer know which file the library came from."
  (cond ((and (consp form)
	      (or (head-is form "include") (head-is form "include-ci"))
	      (every #'stringp (cdr form)))
	 (cons (car form)
	       (mapcar (lambda (f) (namestring (merge-pathnames (psx::native-path f)
								(or *include-directory* *default-pathname-defaults*))))
		       (cdr form))))
	((consp form)
	 (let ((a (resolve-includes (car form))) (d (resolve-includes (cdr form))))
	   (if (and (eq a (car form)) (eq d (cdr form))) form (cons a d))))
	(t form)))

(defun library-form (form)
  "FORM as something psyntax's library expander accepts."
  (if (head-is form "define-library") (translate-define-library form) form))

;;; ------------------------------------------------------------------
;;; Finding libraries in files

(defun name-part-candidates (part)
  "File-name spellings to try for one part of a library name."
  (let ((name (sname* part)))
    (remove-duplicates
     (append (list name)
	     ;; (srfi :1): R7RS libraries are named (srfi 1) on disk;
	     ;; Akku spells the colon %3a.
	     (when (and (> (length name) 1) (char= (char name 0) #\:))
	       (list (subseq name 1) (concatenate 'string "%3a" (subseq name 1))
		     (concatenate 'string "%3A" (subseq name 1)))))
     :test #'string=)))

(defun stems (name)
  (if (null name)
      (list "")
      (loop for first in (name-part-candidates (car name))
	    append (loop for rest in (stems (cdr name))
			 collect (if (string= rest "") first (concatenate 'string first "/" rest))))))

(defun library-name-of (form)
  (cond ((head-is form "define-library") (translate-name (cadr form)))
	((head-is form "library")
	 (let ((n (cadr form)))		; drop an R6RS version
	   (if (listp (car (last n))) (butlast n) n)))))

(defvar *virtual-libraries* '()
  "Functions from a library name to a library form (or NIL), tried
before the file search: libraries made on demand rather than read from
files, such as (cl <package>) (src/interop.lisp).")

(defun locate-library-form (name)
  "Find library NAME (psyntax-style) on the library path; return its
form, translated to an R6RS library, or NIL."
  (let ((name (if (listp (car (last name))) (butlast name) name)))
    (dolist (hook *virtual-libraries*)
      (let ((form (funcall hook name)))
	(when form (return-from locate-library-form form))))
    (dolist (stem (stems name))
      (dolist (path (psx:candidate-files stem))
	(when (probe-file path)
	  (let ((form (find-if (lambda (f) (equal (library-name-of f) name))
			       (ignore-errors (read-forms path)))))
	    (when form
	      (return-from locate-library-form
		(let ((*include-directory* (make-pathname :name nil :type nil :defaults path)))
		  (resolve-includes (library-form form)))))))))
    (srfi-alias-form name)))

(defun library-exports (form)
  "The external names an R6RS library form exports."
  (loop for spec in (cdr (find-if (lambda (c) (head-is c "export")) (cddr form)))
	if (symbolp spec) collect spec
	else if (head-is spec "rename") append (mapcar #'second (cdr spec))))

(defparameter *library-aliases*
  '(("scheme list" . "srfi :1") ("scheme hash-table" . "srfi :125")
    ("scheme charset" . "srfi :14") ("scheme vector" . "srfi :133")
    ("scheme sort" . "srfi :132") ("scheme comparator" . "srfi :128")
    ("scheme generator" . "srfi :158") ("scheme stream" . "srfi :41")
    ("scheme box" . "srfi :111") ("scheme bitwise" . "srfi :151")
    ("scheme fixnum" . "srfi :143") ("scheme division" . "srfi :141"))
  "R7RS-large library names (Red and Tangerine editions) and the
libraries that implement them.")

(defun parse-library-name (string)
  (mapcar #'ssym (uiop:split-string string :separator " ")))

(defun alias-form (name base)
  "A library NAME re-exporting all of library BASE, or NIL if BASE isn't
found."
  (let ((form (locate-library-form base)))
    (when form
      (list (ssym "library") name
	    (cons (ssym "export") (library-exports form))
	    (list (ssym "import") base)))))

(defun srfi-alias-form (name)
  "Libraries known by another name: (srfi :n id ...), the R6RS SRFI
naming (SRFI 97), for a library found only as (srfi :n); and the R7RS-large
names of *LIBRARY-ALIASES*."
  (let ((alias (cdr (assoc name *library-aliases*
			   :test (lambda (n s) (equal n (parse-library-name s)))))))
    (cond (alias (alias-form name (parse-library-name alias)))
	  ((and (> (length name) 2) (head-is name "srfi")
		(symbolp (second name))
		(let ((s (sname* (second name))))
		  (and (> (length s) 1) (char= (char s 0) #\:)
		       (every #'digit-char-p (subseq s 1)))))
	   (alias-form name (list (first name) (second name)))))))

;;; ------------------------------------------------------------------
;;; The standard libraries

(defparameter *syntax-from-r7rs-syntax*
  '("define-record-type" "parameterize" "define-values" "case" "cond-expand"
    "syntax-error" "delay-force" "include" "include-ci"))

(defparameter *syntax-from-r5rs* '("delay"))

(defparameter *host-variants*
  '(("error" . "r7rs:error") ("bytevector-copy!" . "r7rs:bytevector-copy!")
    ("load" . "r7rs:load"))
  "R7RS names whose host global of the same name means something else.")

(defparameter *from-rnrs*
  '("eval" "null-environment" "scheme-report-environment")
  "Procedures taken from psyntax's own libraries: they deal in psyntax
environments.")

(defparameter *defined-locally*
  '(("environment" . "r7rs-environment")
    ("interaction-environment" . "r7rs-interaction-environment"))
  "Procedures each generated library defines in its body.")

(defun host-bound-p (name)
  (boundp (psx::location (ssym name))))

(defvar *missing* '()
  "(library . procedure) pairs left out of the generated libraries because
the host has no such procedure.")

(defun standard-library-form (name export-names)
  (let ((exports '()))
    (flet ((rename (from to) (push (format nil "(rename (~A ~A))" from to) exports)))
      (dolist (n export-names)
	(cond ((or (syntax-export-p n) (member n *from-rnrs* :test #'string=))
	       (push n exports))
	      ((assoc n *defined-locally* :test #'string=)
	       (rename (cdr (assoc n *defined-locally* :test #'string=)) n))
	      ((assoc n *host-variants* :test #'string=)
	       (rename (concatenate 'string "%" (cdr (assoc n *host-variants* :test #'string=))) n))
	      ((host-bound-p n) (rename (concatenate 'string "%" n) n))
	      (t (push (cons (mapcar #'sname* name) n) *missing*)))))
    (read-scheme
     (format nil "(library (~{~A~^ ~})
                    (export ~{~A~^ ~})
                    (import (except (rnrs) define-record-type case)
                            (only (rnrs r5rs) delay null-environment scheme-report-environment)
                            (only (rnrs eval) eval environment)
                            (pseudoscheme r7rs syntax)
                            (prefix (pseudoscheme host) %))
                    (define (r7rs-environment . specs)
                      (apply environment (map %r7rs:translate-import-set specs)))
                    (define (r7rs-interaction-environment)
                      (environment '(pseudoscheme r7rs))))"
	     (mapcar #'sname* name) (nreverse exports)))))

(defun write-scheme-to-string (x)
  (with-output-to-string (s) (funcall ps:*scheme-write* x s)))

(defun read-scheme (string)
  (with-input-from-string (in string)
    (funcall ps:*scheme-read* in)))

(defun install-standard-libraries ()
  (install-host-library '("pseudoscheme" "host") (host-library-exports))
  (dolist (form (read-forms (asdf:system-relative-pathname :pseudoscheme "src/r7rs/syntax.sls")))
    (psx:eval-library form))
  (loop for (name export-string) in *standard-libraries*
	do (psx:eval-library (standard-library-form name (split-names export-string))))
  ;; (pseudoscheme r7rs): all of them but (scheme r5rs), for the REPL
  (psx:eval-library
   (read-scheme
    ;; ... plus import, so (import ...) works at the REPL
    (format nil "(library (pseudoscheme r7rs) (export import ~{~A~^ ~}) (import (only (pseudoscheme) import) ~{~A~^ ~}))"
	    (remove-duplicates
	     (loop for (name s) in *standard-libraries*
		   unless (equal (mapcar #'sname* name) '("scheme" "r5rs"))
		     append (split-names s))
	     :test #'string=)
	    (loop for (name) in *standard-libraries*
		  unless (equal (mapcar #'sname* name) '("scheme" "r5rs"))
		    collect (format nil "(~{~(~A~)~^ ~})" (mapcar #'sname* name))))))
  (install-chezscheme-library)
  ;; the R7RS REPL's own interaction library
  (funcall (psx:host-ref "psyntax:install-library")
	   (funcall (psx:host-ref "gensym"))
	   (mapcar #'ssym '("pseudoscheme" "r7rs" "interaction"))
	   '() '() '() '() '() '() (lambda () ps:unspecific) (lambda () ps:unspecific) t))

;;; (chezscheme): what Chez variants of libraries (foo.chezscheme.sls)
;;; import.  R6RS, as Chez's re-exports it, plus src/compat/chezscheme.scm.

(defparameter *chezscheme-extras*
  '("void" "add1" "sub1" "call/1cc" "gensym" "getenv" "system"
    "with-input-from-string" "with-output-to-string" "current-directory"
    "file-directory?" "file-regular?" "file-symbolic-link?" "directory-list"
    "mkdir" "delete-directory" "rename-file" "file-modification-time"
    "directory-separator" "machine-type" "library-directories"
    "source-directories" "record-writer" "collect" "weak-cons" "weak-pair?"
    "bwp-object?" "iota" "box" "box?" "unbox" "set-box!" "format" "printf"
    "fprintf" "pretty-print" "errorf" "assertion-violationf" "warningf"
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
    "date-zone-offset" "time-utc->date" "current-date"))

(defun install-chezscheme-library ()
  (let ((r6rs (remove-duplicates (psx:table-exports '("r" "mp" "ms" "r5" "ev")) :test #'string=)))
    (psx:eval-library
     (list* (ssym "library") (list (ssym "chezscheme"))
	    (cons (ssym "export")
		  (append (mapcar #'ssym (remove-if (lambda (n) (member n *chezscheme-extras* :test #'string=)) r6rs))
			  (mapcar #'ssym *chezscheme-extras*)
			  (list (list (ssym "rename") (list (ssym "%make-parameter") (ssym "make-parameter"))))))
	    (read-scheme "(import (rnrs) (rnrs mutable-pairs) (rnrs mutable-strings) (rnrs r5rs) (rnrs eval)
                                  (only (pseudoscheme r7rs syntax) parameterize include)
                                  (prefix (pseudoscheme host) %))")
	    (read-forms (asdf:system-relative-pathname :pseudoscheme "src/compat/chezscheme.scm"))))))

;;; ------------------------------------------------------------------
;;; Host procedures the libraries above need

(defun install-front-primitives ()
  ;; R7RS-layer internals the generated libraries use
  (dolist (name '("%parameterize"))
    (psx:host-set! name (primitive name)))
  (psx:defhost "cond-expand-satisfied?" (req) (ps:true? (feature-satisfied-p req)))
  (psx:defhost "read-file-forms" (file fold-case)
    (read-forms (include-path file) :fold-case (ps:truep fold-case)))
  (psx:defhost "r7rs:translate-import-set" (spec) (translate-import-set spec))
  (psx:defhost "r7rs:load" (file &optional env)
    (declare (ignore env))
    (load-file file)
    ps:unspecific)
  ;; Promises: R7RS's, built natively in the host (delay-force is the
  ;; R7RS layer's own macro there).
  (psx::host-eval (read-scheme "(define ($delay-force thunk) (delay-force (thunk)))")))

;;; ------------------------------------------------------------------
;;; Programs, files, the REPL

(defun split-program (forms)
  "Leading define-library/library forms, then the import declarations,
then the rest."
  (let ((libraries '()) (imports '()))
    (loop while (or (head-is (car forms) "define-library") (head-is (car forms) "library"))
	  do (push (pop forms) libraries))
    (loop while (head-is (car forms) "import")
	  do (setq imports (append imports (cdr (pop forms)))))
    (values (nreverse libraries) imports forms)))

(defun eval-forms (forms)
  "Install any libraries in FORMS, then run the program after them."
  (multiple-value-bind (libraries imports body) (split-program forms)
    (dolist (l libraries) (psx:eval-library (library-form l)))
    (if (or imports body)
	(psx:eval-program (cons (cons (ssym "import") (mapcar #'translate-import-set imports))
				body))
	ps:unspecific)))

(defun load-file (path)
  (let ((*include-directory* (make-pathname :name nil :type nil :defaults (pathname path))))
    (eval-forms (read-forms path))))

(defmacro with-psyntax-parameter ((name value) &body body)
  `(let* ((param (psx:host-ref ,name))
	  (old (funcall param)))
     (funcall param ,value)
     (unwind-protect (progn ,@body) (funcall param old))))

(defun eval-at-repl (form)
  "Evaluate FORM in the R7RS REPL's environment.  (import ...) works."
  (with-psyntax-parameter ("psyntax:interaction-library-name"
			   (mapcar #'ssym '("pseudoscheme" "r7rs" "interaction")))
    (with-psyntax-parameter ("psyntax:interaction-source-name"
			     (mapcar #'ssym '("pseudoscheme" "r7rs")))
      (let ((form (if (head-is form "import")
		      (cons (car form) (mapcar #'translate-import-set (cdr form)))
		      form)))
	(if (head-is form "define-library")
	    (psx:eval-library (library-form form))
	    (psx:eval-top-level form))))))

(defvar *booted-host* nil
  "The psyntax host the R7RS libraries were installed in.")

(defun boot ()
  "Bring up psyntax (if needed) and install the R7RS libraries in it,
once per host."
  (unless psx:*host* (psx::boot))
  (unless (eq *booted-host* psx:*host*)
    (install-front-primitives)
    (setf *port-parameters*
	  (loop for (name . var) in '(("current-input-port" . *standard-input*)
				      ("current-output-port" . *standard-output*)
				      ("current-error-port" . *error-output*))
		when (boundp (psx:location (ssym name)))
		  collect (cons (psx:host-ref name) var)))
    (setf psx:*library-form-hook* #'locate-library-form)
    (install-standard-libraries)
    (setq *booted-host* psx:*host*))
  t)
