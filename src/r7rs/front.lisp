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
    "r7rs:interaction-environment" "r7rs:eval-at-repl"
    "make-parameter" "make-list" "gensym" "open-input-string" "open-output-string" "get-output-string"
    "chez:system" "chez:with-input-from-string" "chez:with-output-to-string"
    "chez:current-directory" "chez:file-directory?" "chez:file-regular?"
    "chez:file-symbolic-link?" "chez:directory-list" "chez:mkdir"
    "chez:delete-directory" "chez:rename-file" "chez:file-modification-time"
    "chez:library-directories" "chez:timezone-offset" "chez:get-mode"
    "chez:chmod" "chez:file-change-time" "chez:machine-type"
    "psyntax:environment?" "psyntax:environment-symbols"
    "r7rs:record-ref" "r7rs:record-set!" "r7rs:record?" "r7rs:make-record")
  "Host globals (pseudoscheme host) exports besides R7RS procedure names.")

(defun host-library-exports ()
  (let ((names (remove-duplicates
		(append (loop for (nil s) in *standard-libraries*
			      append (remove-if #'syntax-export-p (split-names s)))
			*host-extras*
			(ps-r6rs::numeric-vector-names))
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
      (ps:skip-script-header in)
      (loop for form = (funcall ps:*scheme-read* in)
	    until (eq form ps:eof-object)
	    collect form))))

(defun include-path (file)
  (merge-pathnames file (or *include-directory* *default-pathname-defaults*)))

(defun feature-satisfied-p (req)
  (cond ((symbolp req)
	 (or (member (ps:scheme-symbol-name req) psl:*scheme-features* :test #'string=)
	     ;; re-entrant continuations: as code is compiled now
	     (and (string= (ps:scheme-symbol-name req) "full-continuations")
		  psx::*full-continuations*)))
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
		       (loop for (clause . more) on (cdr d)
			     when (if (and (symbolp (car clause))
					   (string= (ps:scheme-symbol-name (car clause)) "else"))
				      (or (null more) (error "cond-expand: else clause is not last"))
				      (feature-satisfied-p (car clause)))
			       return (cdr clause))))
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

(defun percent-encode (name safe)
  "NAME with each character not in the string SAFE (or alphanumeric)
written %xx, as Akku names library files: let-optionals* is
let-optionals%2a."
  (with-output-to-string (out)
    (loop for c across name
	  do (if (or (and (char< c (code-char 128)) (alphanumericp c)) (find c safe))
		 (write-char c out)
		 (loop for byte across (if (< (char-code c) 256)
					   (vector (char-code c))
					   (funcall (primitive "string->utf8") (string c)))
		       do (format out "%~(~2,'0X~)" byte))))))

(defun name-part-candidates (part)
  "File-name spellings to try for one part of a library name."
  (let ((name (sname* part)))
    (remove-duplicates
     (append (list name)
	     ;; (srfi :1): R7RS libraries are named (srfi 1) on disk.
	     (when (and (> (length name) 1) (char= (char name 0) #\:))
	       (list (subseq name 1)))
	     ;; Akku escapes other characters, in psyntax's style or
	     ;; Ikarus's: (srfi :1) is srfi/%3a1.
	     (list (percent-encode name "-._~") (percent-encode name ".-+_")
		   (string-upcase-escapes (percent-encode name "-._~"))))
     :test #'string=)))

(defun string-upcase-escapes (name)
  "NAME with the hex digits of its %xx escapes upper-cased."
  (let ((s (copy-seq name)))
    (loop for i from 0 below (length s)
	  when (char= (char s i) #\%)
	    do (setf (subseq s (+ i 1) (min (length s) (+ i 3)))
		     (string-upcase (subseq s (+ i 1) (min (length s) (+ i 3))))))
    s))

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
			       (handler-case (read-forms path)
				 ;; reported, not taken for a missing library
				 (error (e)
				   (ps:scheme-error (format nil "~A: ~A" (namestring path)
							    (remove #\Newline (princ-to-string e)))))))))
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
    ("scheme fixnum" . "srfi :143") ("scheme division" . "srfi :141")
    ("scheme set" . "srfi :113") ("scheme rlist" . "srfi :101")
    ("scheme text" . "srfi :135") ("scheme mapping" . "srfi :146")
    ("scheme mapping hash" . "srfi :146 hash")
    ("scheme regex" . "srfi :115") ("scheme flonum" . "srfi :144")
    ("scheme list-queue" . "srfi :117") ("scheme lseq" . "srfi :127")
    ("scheme ideque" . "srfi :134")
    ("scheme vector u8" . "srfi :160 u8") ("scheme vector s8" . "srfi :160 s8")
    ("scheme vector u16" . "srfi :160 u16") ("scheme vector s16" . "srfi :160 s16")
    ("scheme vector u32" . "srfi :160 u32") ("scheme vector s32" . "srfi :160 s32")
    ("scheme vector u64" . "srfi :160 u64") ("scheme vector s64" . "srfi :160 s64")
    ("scheme vector f32" . "srfi :160 f32") ("scheme vector f64" . "srfi :160 f64")
    ("scheme vector c64" . "srfi :160 c64") ("scheme vector c128" . "srfi :160 c128"))
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
naming (SRFI 97), for a library found only as (srfi :n); (srfi srfi-n),
SRFI 261's; and the R7RS-large names of *LIBRARY-ALIASES*."
  (let ((alias (cdr (assoc name *library-aliases*
			   :test (lambda (n s) (equal n (parse-library-name s)))))))
    (cond (alias (alias-form name (parse-library-name alias)))
	  ((and (> (length name) 2) (head-is name "srfi")
		(symbolp (second name))
		(let ((s (sname* (second name))))
		  (and (> (length s) 1) (char= (char s 0) #\:)
		       (every #'digit-char-p (subseq s 1)))))
	   (alias-form name (list (first name) (second name))))
	  ;; SRFI 261's portable (srfi srfi-n)
	  ((and (= (length name) 2) (head-is name "srfi") (symbolp (second name))
		(let ((s (sname* (second name))))
		  (and (> (length s) 5) (string= "srfi-" s :end2 5)
		       (every #'digit-char-p (subseq s 5)))))
	   (alias-form name (list (first name)
				  (ssym (concatenate 'string ":" (subseq (sname* (second name)) 5)))))))))

;;; ------------------------------------------------------------------
;;; The standard libraries

(defparameter *syntax-from-r7rs-syntax*
  '("define-record-type" "parameterize" "define-values" "case" "cond-expand"
    "syntax-error" "delay-force" "include" "include-ci" "let-syntax" "letrec-syntax"))

(defparameter *syntax-from-r5rs* '("delay"))

(defparameter *host-variants*
  '(("error" . "r7rs:error") ("bytevector-copy!" . "r7rs:bytevector-copy!")
    ("load" . "r7rs:load"))
  "R7RS names whose host global of the same name means something else.")

(defparameter *from-rnrs*
  '("null-environment" "scheme-report-environment")
  "Procedures taken from psyntax's own libraries: they deal in psyntax
environments.")

(defparameter *defined-locally*
  '(("environment" . "r7rs-environment")
    ("interaction-environment" . "r7rs-interaction-environment")
    ("eval" . "r7rs-eval"))
  "Procedures the generated libraries take from (pseudoscheme r7rs
environments) (src/r7rs/syntax.sls), under these names.")

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
                    (import (except (rnrs) define-record-type case let-syntax letrec-syntax)
                            (only (rnrs r5rs) delay null-environment scheme-report-environment)
                            (only (rnrs eval) eval environment)
                            (pseudoscheme r7rs syntax)
                            (pseudoscheme r7rs environments)
                            (prefix (pseudoscheme host) %)))"
	     (mapcar #'sname* name) (nreverse exports)))))

(defun write-scheme-to-string (x)
  (with-output-to-string (s) (funcall ps:*scheme-write* x s)))

(defun read-scheme (string)
  (with-input-from-string (in string)
    (funcall ps:*scheme-read* in)))

(defparameter *pseudoscheme-r5rs-library*
  (format nil "(library (pseudoscheme r5rs)
     (export ~A
             open-output-string open-input-string get-output-string
             call-with-output-string with-output-to-string flush-output
             cond-expand)
     (import (scheme r5rs)
             (only (scheme base) cond-expand open-output-string open-input-string get-output-string
                   parameterize current-output-port flush-output-port))
     (define (call-with-output-string proc)
       (let ((port (open-output-string)))
         (proc port)
         (get-output-string port)))
     (define (with-output-to-string thunk)
       (let ((port (open-output-string)))
         (parameterize ((current-output-port port)) (thunk))
         (get-output-string port)))
     (define (flush-output . port)
       (apply flush-output-port port)))"
	  (second (assoc '(scheme r5rs) *standard-libraries* :test #'equal)))
  "What R5RS on psyntax starts with: (scheme r5rs), and as extensions the
string ports R5RS mode's classic environment has and SRFI 0's
cond-expand.")

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
  (install-ikarus-library)
  (psx:eval-library (read-scheme *pseudoscheme-r5rs-library*))
  ;; the interaction libraries of the R7RS REPL, and of R5RS on psyntax
  (dolist (name '(("pseudoscheme" "r7rs" "interaction") ("pseudoscheme" "r5rs" "interaction")))
    (funcall (psx:host-ref "psyntax:install-library")
	     (funcall (psx:host-ref "gensym"))
	     (mapcar #'ssym name)
	     '() '() '() '() '() '() (lambda () ps:unspecific) (lambda () ps:unspecific) t)))

;;; (chezscheme): what Chez variants of libraries (foo.chezscheme.sls)
;;; import.  R6RS, as Chez's re-exports it, plus src/compat/chezscheme.scm.

(defparameter *chezscheme-extras*
  '("void" "add1" "sub1" "call/1cc" "gensym" "getenv" "system"
    "with-input-from-string" "with-output-to-string" "current-directory"
    "file-directory?" "file-regular?" "file-symbolic-link?" "directory-list"
    "mkdir" "delete-directory" "rename-file" "file-modification-time"
    "directory-separator" "machine-type" "library-directories"
    "get-mode" "chmod" "file-change-time"
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

;;; (ikarus): what Ikarus variants of libraries (foo.ikarus.sls) import.
;;; R6RS, the extensions Ikarus shares with Chez, from (chezscheme), and
;;; src/compat/ikarus.scm.

(defparameter *ikarus-from-chezscheme*
  '("void" "add1" "sub1" "gensym" "getenv" "system" "with-input-from-string"
    "with-output-to-string" "current-directory" "file-directory?"
    "file-regular?" "file-symbolic-link?" "directory-list" "delete-directory"
    "rename-file" "format" "printf" "fprintf" "pretty-print" "fluid-let"
    "time" "parameterize" "make-parameter" "include" "last-pair" "make-list"
    "open-input-string" "open-output-string" "get-output-string"
    "library-directories"))

(defparameter *ikarus-extras*
  '("fxadd1" "fxsub1" "die" "port-closed?" "library-path" "stale-when"
    "read-annotated" "environment?" "environment-symbols" "print-condition"))

(defun install-ikarus-library ()
  (let ((r6rs (remove-duplicates (psx:table-exports '("r" "mp" "ms" "r5" "ev")) :test #'string=))
	(own (append *ikarus-from-chezscheme* *ikarus-extras*)))
    (psx:eval-library
     (list* (ssym "library") (list (ssym "ikarus"))
	    (cons (ssym "export")
		  (mapcar #'ssym (append (remove-if (lambda (n) (member n own :test #'string=)) r6rs)
					 own)))
	    (list* (ssym "import")
		   (list* (ssym "only") (list (ssym "chezscheme")) (mapcar #'ssym *ikarus-from-chezscheme*))
		   (read-scheme "((rnrs) (rnrs mutable-pairs) (rnrs mutable-strings) (rnrs r5rs) (rnrs eval)
                                  (prefix (pseudoscheme host) %))"))
	    (read-forms (asdf:system-relative-pathname :pseudoscheme "src/compat/ikarus.scm"))))))

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
  ;; R7RS's load: the file's forms, one by one, at the REPL (in the
  ;; interaction environment), whatever the environment
  (psx:defhost "r7rs:load" (file &optional env)
    (declare (ignore env))
    (load-file-at-repl file)
    ps:unspecific)
  (psx:defhost "r7rs:interaction-environment" () *interaction-environment*)
  (psx:defhost "r7rs:eval-at-repl" (form) (eval-at-repl form))
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
    (loop
      (cond ((head-is (car forms) "import")
	     (setq imports (append imports (cdr (pop forms)))))
	    ;; (cond-expand ((library (srfi 18)) (import (srfi 18))) ...)
	    ;; among the imports, as R7RS programs may: the chosen
	    ;; clause's import declarations
	    ((and (head-is (car forms) "cond-expand")
		  (let ((chosen (cond-expand-chosen (car forms))))
		    (and chosen
			 (every (lambda (f) (or (head-is f "import") (head-is f "cond-expand")))
				chosen))))
	     (setq forms (append (cond-expand-chosen (car forms)) (cdr forms))))
	    (t (return))))
    (values (nreverse libraries) imports forms)))

(defun cond-expand-chosen (form)
  "The forms of the clause of cond-expand FORM whose requirement holds."
  (loop for (clause . more) on (cdr form)
	when (if (and (symbolp (car clause))
		      (string= (ps:scheme-symbol-name (car clause)) "else"))
		 (or (null more) (error "cond-expand: else clause is not last"))
		 (feature-satisfied-p (car clause)))
	  return (cdr clause)))

(defun eval-forms (forms)
  "Install any libraries in FORMS, then run the program after them."
  (multiple-value-bind (libraries imports body) (split-program forms)
    (dolist (l libraries) (psx:eval-library (library-form l)))
    (if (or imports body)
	(psx:eval-program (cons (cons (ssym "import") (mapcar #'translate-import-set imports))
				body))
	ps:unspecific)))

(defvar *interaction-environment* (list 'interaction-environment)
  "What R7RS's interaction-environment returns: eval evaluates in the
REPL's environment for it.")

(defun load-file-at-repl (path)
  (let ((*include-directory* (make-pathname :name nil :type nil :defaults (pathname path)))
	(value ps:unspecific))
    (dolist (form (read-forms path) value)
      (setq value (eval-at-repl form)))))

(defun load-file (path)
  (let ((*include-directory* (make-pathname :name nil :type nil :defaults (pathname path))))
    (eval-forms (read-forms path))))

(defmacro with-psyntax-parameter ((name value) &body body)
  `(let* ((param (psx:host-ref ,name))
	  (old (funcall param)))
     (funcall param ,value)
     (unwind-protect (progn ,@body) (funcall param old))))

(defun eval-at-r5rs-repl (form)
  "Evaluate FORM at an R5RS top level on psyntax: the bindings of
(pseudoscheme r5rs), in an interaction library of its own.  Unlike R5RS mode's classic
translator, this goes through psyntax, so it can be compiled with full
continuations (src/continuations.lisp)."
  (boot)
  (with-psyntax-parameter ("psyntax:interaction-library-name"
			   (mapcar #'ssym '("pseudoscheme" "r5rs" "interaction")))
    (with-psyntax-parameter ("psyntax:interaction-source-name"
			     (mapcar #'ssym '("pseudoscheme" "r5rs")))
      (psx:eval-top-level form))))

(defun translate-repl-imports (form)
  "FORM's import sets, R7RS's library names made psyntax's, it being an
import or a begin of forms that may be."
  (cond ((head-is form "import")
	 (cons (car form) (mapcar #'translate-import-set (cdr form))))
	((head-is form "begin")
	 (cons (car form) (mapcar #'translate-repl-imports (cdr form))))
	(t form)))

(defun eval-at-repl (form)
  "Evaluate FORM in the R7RS REPL's environment.  (import ...) works."
  (with-psyntax-parameter ("psyntax:interaction-library-name"
			   (mapcar #'ssym '("pseudoscheme" "r7rs" "interaction")))
    (with-psyntax-parameter ("psyntax:interaction-source-name"
			     (mapcar #'ssym '("pseudoscheme" "r7rs")))
      (let ((form (translate-repl-imports form)))
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
    (let ((psx::*full-continuations* nil))	; re-exports, syntax, two definitions
      (boot-libraries))
    (setq *booted-host* psx:*host*))
  t)

(defun boot-libraries ()
  (install-front-primitives)
  (setf *port-parameters*
	(loop for (name . var) in '(("current-input-port" . *standard-input*)
				    ("current-output-port" . *standard-output*)
				    ("current-error-port" . *error-output*))
	      when (boundp (psx:location (ssym name)))
		collect (cons (psx:host-ref name) var)))
  (setf psx:*library-form-hook* #'locate-library-form)
  ;; the same ids every session, for compiled libraries
  (psx::with-boot-gensyms (install-standard-libraries)))

;;; Alternative readers (ps:reader-directive): the directive, the
;;; library and its procedure of a port that reads a datum.

(defparameter *alternative-readers*
  '(("sweet" "(srfi 110)" "sweet-read")		; sweet-expressions
    ("wisp" "(srfi 119)" "wisp-read-form")		; wisp
    ("srfi-49" "(srfi 49)" "i-expression-read"))) ; I-expressions

(defun library-procedure (library name)
  "The value of NAME in LIBRARY (R7RS names, as text)."
  (eval-forms (list (read-scheme (format nil "(import (only ~A ~A))" library name))
		    (ssym name))))

(setf ps:*array-literal-loader*
      (lambda () (library-procedure "(srfi 163)" "array-literal")))

(setf ps:*reader-directives*
      (loop for (directive library name) in *alternative-readers*
	    collect (let ((library library) (name name))
		      (cons directive (lambda () (library-procedure library name))))))

;;; Precompiling libraries: expanding and compiling every library in a
;;; directory (by default the bundled SRFIs, src/srfi/) once, so that the
;;; compiled-library cache (src/library-cache.lisp) holds them for later
;;; sessions.  Compiled for the current continuation mode, which the
;;; cache keeps apart.

(defun library-files (directory)
  "The .sld files under DIRECTORY, in order: by number where the name is
one, then by name; not those in a reference/ directory (upstream
sources kept for provenance, src/srfi/reference/)."
  (flet ((key (path)
	   (let ((n (ignore-errors (parse-integer (pathname-name path)))))
	     (format nil "~{~A/~}~10,'0D~A" (cdr (pathname-directory path)) (or n 0)
		     (if n "" (pathname-name path))))))
    (sort (remove-if (lambda (path) (member "reference" (pathname-directory path) :test #'equal))
		     (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
								:name :wild :type "sld")
						 directory)))
	  #'string< :key #'key)))

(defun precompile-libraries (&key (directory (asdf:system-relative-pathname :pseudoscheme "src/srfi/"))
				  (output *standard-output*))
  "Expand and compile every library in the .sld files under DIRECTORY
into the compiled-library cache, printing a line for each to OUTPUT (or
nothing if it is NIL).  Returns how many compiled and how many failed."
  (boot)
  (unless (psx::library-cache-p)
    (error "precompile-libraries: the library cache is off"))
  (let* ((names (loop for file in (library-files directory)
		      append (loop for form in (ignore-errors (read-forms file))
				   for name = (library-name-of form)
				   when name collect name)))
	 (total (length names))
	 (ok 0) (failed '())
	 ;; found there, as well as where libraries are looked for
	 (psx:*library-path* (append psx:*library-path*
				     (list (namestring (uiop:ensure-directory-pathname directory))))))
    (loop for name in names
	  for i from 1
	  do (let ((start (get-internal-real-time))
		   (shown (format nil "(~{~A~^ ~})" (mapcar #'sname* name))))
	       (when output
		 (format output "~&[~3D/~D] ~32A " i total shown)
		 (finish-output output))
	       (handler-case
		   (let ((*standard-output* (make-broadcast-stream)))
		     ;; installs it: expanded and compiled, or loaded from
		     ;; the cache; not run
		     (funcall (psx:host-ref "psyntax:environment") name)
		     (incf ok)
		     (when output
		       (format output "ok   ~5,1Fs~%"
			       (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
		 (serious-condition (e)
		   (push shown failed)
		   (when output
		     (let ((message (remove #\Newline (princ-to-string e))))
		       (format output "FAIL ~A~%" (subseq message 0 (min 100 (length message))))))))))
    (when output
      (format output "~&~%Libraries: ~D of ~D compiled into ~A~@[; failed: ~{~A~^ ~}~]~%"
	      ok total (namestring (psx::library-cache-directory)) (reverse failed)))
    (values ok (length failed))))

;;; SRFI 176

(defun srfi-feature-number (feature)
  "N, for a feature srfi-N."
  (and (> (length feature) 5) (string= "srfi-" feature :end2 5)
       (every #'digit-char-p (subseq feature 5))
       (parse-integer feature :start 5)))

(defun version-alist ()
  "SRFI 176's version properties, as Scheme data."
  (let ((translator (uiop:symbol-call "SCHEME-TRANSLATOR" "TRANSLATOR-VERSION")))
    `((,(sym "command") "pseudoscheme")
      (,(sym "scheme.id") ,(sym "pseudoscheme"))
      (,(sym "languages") ,@(mapcar #'sym '("scheme" "r5rs" "r6rs" "r7rs")))
      (,(sym "encodings") ,(sym "utf-8"))
      (,(sym "version") ,(subseq translator (1+ (position #\Space translator :from-end t))))
      (,(sym "install-dir") ,(namestring (asdf:system-source-directory :pseudoscheme)))
      (,(sym "scheme.srfi")
       ,@(sort (remove nil (mapcar #'srfi-feature-number psl:*scheme-features*)) #'<))
      (,(sym "scheme.features")
       ,@(mapcar #'sym (remove-if #'srfi-feature-number psl:*scheme-features*)))
      (,(sym "scheme.path") ,@(copy-list psx:*library-path*))
      (,(sym "build.platform")
       ,(format nil "~A ~A ~A ~A" (lisp-implementation-type) (lisp-implementation-version)
		(machine-type) (software-type))))))
