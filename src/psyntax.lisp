; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; psyntax: the R6RS expander as Pseudoscheme's front end
;;;;
;;;; vendor/psyntax/ is Ghuloum & Dybvig's portable R6RS library and
;;;; syntax-case system (2007, MIT license; see its README.txt and
;;;; README-pseudoscheme.md).  It is written as R6RS libraries and
;;;; distributed pre-expanded into plain R5RS core forms (a ".pp" file);
;;;; a host provides a handful of primitives and an EVAL for core forms.
;;;;
;;;; Here the host is a Pseudoscheme program environment, *HOST*:
;;;;
;;;;   source --psyntax--> core Scheme --translator--> Common Lisp
;;;;
;;;; psyntax does all macro expansion (syntax-case, syntax-rules,
;;;; identifier macros, libraries, phases); the translator only ever sees
;;;; lambda/if/set!/define/quote/begin/letrec and calls.  Every R6RS
;;;; library binding that isn't a macro is a "primitive": a global
;;;; variable of *HOST* with the standard name, which is what
;;;; src/r6rs/*.lisp define.

(defpackage "PSEUDOSCHEME-PSYNTAX"
  (:nicknames "PSX")
  (:use "COMMON-LISP")
  (:export "*HOST*" "EVAL-PROGRAM" "EVAL-LIBRARY" "EVAL-TOP-LEVEL"
	   "EVAL-FORMS" "LOAD-FILE" "EXPAND" "REBUILD" "*LIBRARY-PATH*" "*SYSTEM-LIBRARY-PATH*"
	   "DEFHOST" "HOST-REF" "HOST-SET!" "MISSING-PRIMITIVES"
	   "TABLE-EXPORTS" "*LIBRARY-FORM-HOOK*" "*LIBRARY-EXTENSIONS*" "CANDIDATE-FILES" "*IMPLEMENTATION-VARIANTS*" "LOCATION" "HOST-EVAL"))

(in-package "PSEUDOSCHEME-PSYNTAX")

(defun sym (string) (psl:scheme-symbol string))

(defvar *host* nil
  "The program environment psyntax and the code it expands run in.")

(defvar *shared-primitives* '()
  "Host primitives that are R5RS's own (integrated) bindings; see MAKE-HOST.")

(defun location (name)
  "The CL symbol holding the host global NAME (a Scheme symbol)."
  (psl:tr "PROGRAM-VARIABLE-LOCATION" (psl:tr "PROGRAM-ENV-ENSURE-DEFINED" *host* name)))

(defun host-ref (name)
  (symbol-value (location (if (stringp name) (sym name) name))))

(defun host-set! (name value)
  (let* ((name (if (stringp name) (sym name) name))
	 (loc (progn
		;; A primitive shared with R5RS (see MAKE-HOST) gets its own
		;; variable before being changed, so R5RS's isn't.
		(when (member name *shared-primitives*)
		  (setq *shared-primitives* (remove name *shared-primitives*))
		  (psl:install-variable! *host* name value))
		(location name))))
    (setf (symbol-value loc) value)
    (when (functionp value) (ps:set-function-from-value loc))
    value))

(defmacro defhost (name lambda-list &body body)
  "Define host primitive NAME (a string) as a Lisp function."
  `(host-set! ,name (lambda ,lambda-list ,@body)))

;;; ------------------------------------------------------------------
;;; The adapter: what psyntax's (psyntax system $bootstrap) and compat
;;; expect of a host (compare vendor/psyntax/scheme48.r6rs.ss).

(defvar *gensym-prefix*
  ;; Expanded code names library globals with gensyms, and expanded
  ;; code can be saved (the .pp itself, translated files), so names
  ;; must not repeat across sessions: prefix with the time.
  (format nil "g$~(~36R~)$" (get-universal-time)))
(defvar *gensym-count* 0)

(defun install-adapter ()
  (defhost "gensym" (&rest args)
    (declare (ignore args))
    (intern (format nil "~A~D" *gensym-prefix* (incf *gensym-count*)) "SCHEME"))
  (defhost "void" () ps:unspecific)
  (defhost "symbol-value" (s) (symbol-value (location s)))
  (defhost "set-symbol-value!" (s v) (host-set! s v) ps:unspecific)
  (defhost "eval-core" (x) (host-eval x))
  (defhost "lisp-keyword?" (x) (ps:true? (keywordp x)))
  (defhost "pretty-print" (x &optional (port *standard-output*))
    (funcall ps:*scheme-write* x port) (terpri port) ps:unspecific))

(defparameter *closed-primitives* '("assv" "memv" "map" "for-each")
  "Primitives whose host binding is one of the translator's integrated
built-ins but whose value the R6RS/R7RS layers replaced: OPEN-PRIMITIVES
must leave them as (primitive x), or the translator would open-code the
old built-in.")

(defun open-primitives (form)
  "FORM with each (primitive x) -- psyntax's reference to host global
x -- replaced by the plain variable reference x, so the translator
integrates x as it does in R5RS code ((primitive +) becomes CL's +
rather than a call through a function named PRIMITIVE).  Safe because
psyntax renames every user variable: nothing in its output can shadow a
host global."
  (cond ((atom form) form)
	((and (symbolp (car form)) (string= (symbol-name (car form)) "QUOTE")) form)
	((and (symbolp (car form)) (string= (symbol-name (car form)) "PRIMITIVE")
	      (consp (cdr form)) (symbolp (cadr form)) (null (cddr form))
	      (not (member (ps:scheme-symbol-name (cadr form)) *closed-primitives* :test #'string=)))
	 (cadr form))
	(t (let ((a (open-primitives (car form))) (d (open-primitives (cdr form))))
	     (if (and (eq a (car form)) (eq d (cdr form))) form (cons a d))))))

(defun host-eval (form)
  "Translate and evaluate core FORM in *HOST*.  The CL compiler's
style warnings about the generated code (an unknown arity, say) are
about psyntax's output, not the user's program, so they're muffled."
  (handler-bind ((warning #'muffle-warning))
    (ps:scheme-eval (open-primitives form) *host*)))

;;; ------------------------------------------------------------------
;;; Building the host environment

(defun all-primitive-names ()
  (remove-duplicates
   (append (mapcar #'sym (loop for (nil export-string) in ps-r7rs:*standard-libraries*
			       append (ps-r7rs:split-names export-string)))
	   (psl:tr "INTERFACE-NAMES"
		   (psl:tr "STRUCTURE-INTERFACE" (psl:base-structure))))))

(defun overridden-primitive-names ()
  "Names the host's INSTALL-PRIMITIVES (re)defines."
  (mapcar (lambda (p) (sym (car p))) ps-r6rs:*primitives*))

(defun make-host ()
  "The host environment: a copy of the R7RS implementation env's
bindings -- except that where a binding is still R5RS's own built-in
(same value, and not redefined by the R6RS layer), the host shares the
R5RS binding itself, so the translator open-codes it ((+ a b) becomes
CL's +, (vector-ref v i) SVREF) as it does in R5RS code.  A copy would
be a fresh variable, called out of line."
  (let ((env (psl:new-library-env "psyntax host"))
	(base (psl:base-structure))
	(overridden (overridden-primitive-names))
	(shared '()))
    (dolist (name (all-primitive-names))
      (when (psl:binding-defined-p ps-r7rs:*implementation-env* name)
	(let ((den (psl:tr "PROGRAM-ENV-LOOKUP" ps-r7rs:*implementation-env* name))
	      (base-den (and (member name (psl:tr "INTERFACE-NAMES" (psl:tr "STRUCTURE-INTERFACE" base)))
			     (psl:tr "STRUCTURE-REF" base name))))
	  (if (and base-den
		   (psl:variable-node-p den) (psl:variable-node-p base-den)
		   (boundp (psl:tr "PROGRAM-VARIABLE-LOCATION" den))
		   (boundp (psl:tr "PROGRAM-VARIABLE-LOCATION" base-den))
		   (eq (symbol-value (psl:tr "PROGRAM-VARIABLE-LOCATION" den))
		       (symbol-value (psl:tr "PROGRAM-VARIABLE-LOCATION" base-den)))
		   (not (member name overridden))
		   (not (member (ps:scheme-symbol-name name) *closed-primitives* :test #'string=)))
	      (progn (psl:tr "PROGRAM-ENV-DEFINE!" env name base-den) (push name shared))
	      (psl:copy-bindings! env ps-r7rs:*implementation-env* (list name))))))
    (setq *shared-primitives* shared)
    env))

(defun install-primitives (alist)
  "ALIST of (name-string . function), e.g. from DEFPRIM registries."
  (loop for (name . fn) in alist do (host-set! name fn)))

;;; ------------------------------------------------------------------
;;; Loading the expander

(defun vendor-file (name)
  (asdf:system-relative-pathname :pseudoscheme
				 (concatenate 'string "vendor/psyntax/" name)))

(defun read-scheme-text (string)
  (with-input-from-string (in string)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun read-file-forms (path)
  (with-open-file (in path)
    (loop for form = (funcall ps:*scheme-read* in)
	  until (eq form ps:eof-object)
	  collect form)))

(defun load-image (path &key (drop-last nil))
  "Evaluate the core forms of a psyntax .pp file in *HOST*.  (The
original pre-built images end with a form that runs a script named on
the command line and exits; DROP-LAST skips it.)"
  (let ((forms (read-file-forms path)))
    (dolist (form (if drop-last (butlast forms) forms))
      (host-eval form))))

(defun boot (&key seed)
  "Create *HOST* and load psyntax into it: our own rebuilt image if there
is one, else the original Scheme48 one (which lacks our entry points;
see REBUILD)."
  (setq *host* (make-host))
  (setf ps-r6rs::*globals-hook* #'host-ref)
  (ps-r6rs::install-exception-hooks)
  (install-primitives ps-r6rs:*primitives*)
  (install-adapter)
  (let ((own (vendor-file "psyntax-pseudoscheme.pp")))
    (cond ((and (not seed) (probe-file own))
	   (load-image own))
	  (t
	   (load-image (vendor-file "pre-built/psyntax-scheme48.pp") :drop-last t)
	   ;; The original image has none of our entry points, only its
	   ;; own script runner (the dropped last form), which calls
	   ;; EVAL-R6RS-TOP-LEVEL through this global.  That's enough to
	   ;; run the build script and get an image that has them.
	   (host-set! "psyntax:eval-r6rs-top-level" (host-ref "g$747$23171")))))
;; DELAY expands into ($delay thunk); promises are R7RS's.
  (host-eval (car (read-scheme-text
		   "(define ($delay thunk) (delay-force (make-promise (thunk))))")))
  (when (boundp (location (sym "psyntax:file-locator")))
    (funcall (host-ref "psyntax:file-locator") #'locate-library-file))
  (when (boundp (location (sym "psyntax:library-locator")))
    (funcall (host-ref "psyntax:library-locator") #'locate-library))
  *host*)

;;; ------------------------------------------------------------------
;;; Finding libraries in files

(defvar *library-path* (list "./")
  "Directories searched, in order, for library source files: (foo bar)
is looked for as foo/bar.sls, then .ss, .sld and .scm, in each.")

(defvar *system-library-path*
  (list (cons "srfi" (namestring (asdf:system-relative-pathname :pseudoscheme "src/srfi/"))))
  "Libraries that ship with Pseudoscheme, searched after *LIBRARY-PATH*.
An entry (PREFIX . DIR) roots names beginning with PREFIX at DIR: (srfi
1) is src/srfi/1.sld.")

(defparameter *library-extensions* '("sls" "ss" "sld" "scm"))

(defparameter *implementation-variants* '("pseudoscheme" nil "chezscheme" "ikarus")
  "Implementation-specific variants of a library file to try, in order:
foo.pseudoscheme.sls first, then the generic foo.sls (NIL), then other
systems' variants, as Akku lays them out.  Chez's is next because
nearly every Akku package has one, and src/compat/ supplies the
(chezscheme) library such variants import; Ikarus is psyntax-based.")

(defun native-path (string)
  "STRING as a pathname, with no CL wildcard syntax (* ? [ in Scheme
file names, like and-let*.sls, are just characters)."
  (uiop:parse-native-namestring string))

(defun candidate-files (stem &optional (dirs (append *library-path* *system-library-path*)))
  "Files that might hold the library whose name gives STEM (foo/bar), in
search order: each variant (see *IMPLEMENTATION-VARIANTS*) in every
directory before the next variant, so a generic file anywhere on the
path beats another system's variant.  A (PREFIX . DIR) entry of DIRS
applies only to stems under PREFIX (see *SYSTEM-LIBRARY-PATH*)."
  (let ((roots (loop for d in (if (listp dirs) dirs (list dirs))
		     for (prefix . dir) = (if (consp d) d (cons nil d))
		     for p = (and prefix (concatenate 'string prefix "/"))
		     when (or (null p) (and (> (length stem) (length p))
					    (string= p stem :end2 (length p))))
		       collect (cons (namestring (uiop:ensure-directory-pathname dir))
				     (if p (subseq stem (length p)) stem)))))
    (loop for variant in *implementation-variants*
	  append (loop for (dir . stem) in roots
		       append (if variant
				  (list (native-path (format nil "~A~A.~A.sls" dir stem variant)))
				  (loop for ext in *library-extensions*
					collect (native-path (format nil "~A~A.~A" dir stem ext))))))))

(defun library-name-file-stem (name)
  "(foo bar (1)) -> \"foo/bar\": the identifiers of a library name,
version dropped."
  (format nil "~{~A~^/~}"
	  (loop for part in name
		while (symbolp part)
		collect (ps:scheme-symbol-name part))))

(defvar *library-form-hook* nil
  "If set, a function from a library name to its (R6RS library) form or
NIL; used before the plain file search, e.g. to translate R7RS
define-library forms (src/r7rs/front.lisp).")

(defun locate-library (name)
  "psyntax's LIBRARY-LOCATOR: the form defining library NAME, or #f."
  (or (and *library-form-hook* (funcall *library-form-hook* name))
      (let ((file (locate-library-file name)))
	(and (stringp file)
	     (with-open-file (in file) (funcall ps:*scheme-read* in))))
      ps:false))

(defun locate-library-file (name)
  "psyntax's FILE-LOCATOR: a file name for library NAME, or #f."
  (let ((stem (library-name-file-stem name)))
    (or (loop for path in (candidate-files stem)
	      when (probe-file path) return (namestring path))
	ps:false)))

;;; ------------------------------------------------------------------
;;; Entry points

(defun entry (name)
  (let ((loc (location (sym name))))
    (unless (boundp loc)
      (error "psyntax entry point ~A is missing: the loaded image predates ~
              vendor/psyntax/psyntax/main.ss -- run (psx:rebuild)" name))
    (symbol-value loc)))


(defun eval-program (forms)
  "Run an R6RS top-level program: FORMS begin with (import ...)."
  (funcall (entry "psyntax:eval-r6rs-top-level") forms))

(defun eval-library (form)
  "Expand, install and (lazily) instantiate an R6RS (library ...) form."
  (funcall (entry "psyntax:library-expander") form))

(defun eval-top-level (form)
  "Evaluate FORM at the REPL, in (pseudoscheme interaction)."
  (funcall (entry "psyntax:eval-top-level") form))

(defun expand (form &optional (env (funcall (entry "psyntax:environment")
					     (list (mapcar #'sym '("rnrs"))))))
  (funcall (entry "psyntax:expand") form env))

(defun eval-forms (forms)
  "Evaluate a file's worth of forms: any (library ...) forms are
installed, and what follows them, if anything, is run as a program."
  (let ((result ps:unspecific))
    (loop while (and forms (psl:keyword-head-p (car forms) "library"))
	  do (eval-library (pop forms)))
    (when forms
      (setq result (eval-program forms)))
    result))

(defun load-file (path)
  (eval-forms (read-file-forms path)))



;;; ------------------------------------------------------------------
;;; Rebuilding the expander image on Pseudoscheme itself

(defun rebuild (&key seed)
  "Run vendor/psyntax/psyntax-buildscript.ss -- the expander expanding
its own sources -- writing vendor/psyntax/psyntax-pseudoscheme.pp.
SEED: bootstrap from the original Scheme48 image instead of our own."
  (boot :seed seed)
  (let ((*default-pathname-defaults* (vendor-file ""))
	(start (get-internal-real-time)))
    (eval-program (read-file-forms (vendor-file "psyntax-buildscript.ss")))
    (format t "~&Rebuilt psyntax in ~,1Fs~%"
	    (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
  (boot))

;;; ------------------------------------------------------------------
;;; psyntax's identifier table

(defun table-exports (keys)
  "Names (strings) the build script's identifier->library-map sends to
any of the library KEYS (strings: \"r\" for (rnrs), \"r5\", ...)."
  (let* ((forms (read-file-forms (vendor-file "psyntax-buildscript.ss")))
	 (def (find-if (lambda (f)
			 (and (consp f) (consp (cdr f)) (symbolp (cadr f))
			      (string= (ps:scheme-symbol-name (cadr f)) "identifier->library-map")))
		       forms)))
    (loop for (name . libs) in (cadr (caddr def))
	  when (some (lambda (k) (member (ps:scheme-symbol-name k) keys :test #'string=)) libs)
	    collect (ps:scheme-symbol-name name))))

;;; ------------------------------------------------------------------
;;; Diagnostics

(defun missing-primitives ()
  "Names exported by some psyntax library that have no host binding."
  (let ((pslib (funcall (entry "psyntax:environment") (list (list (sym "pseudoscheme"))))))
    (declare (ignore pslib))
    nil))
