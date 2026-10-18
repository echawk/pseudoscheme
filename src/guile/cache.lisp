; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-GUILE -*-

;;;; Compiled files: the Guile mode's .go files.  The first time a file is
;;;; loaded, each of its forms is expanded, translated to Lisp, evaluated
;;;; and kept; once the whole file has loaded, the kept Lisp forms are
;;;; compiled with COMPILE-FILE into a fasl under the cache directory
;;;; (~/.cache/pseudoscheme/guile/).  Loading the file again loads the
;;;; fasl: nothing is read, expanded or compiled.  This is what Guile
;;;; does with its compiled .go files, and as there, a form is compiled
;;;; with the macros the forms before it defined.
;;;;
;;;; A fasl is keyed by the file's name and date, the Pseudoscheme it was
;;;; compiled by (psx::build-signature) and the continuation mode.  The
;;;; literals in compiled code -- modules, syntax objects, sites, photons --
;;;; have load forms here; a file whose code holds anything else isn't
;;;; cached.  PSEUDOSCHEME_GUILE_CACHE=0 turns the cache off.

(in-package "PSEUDOSCHEME-GUILE")

(defvar *guile-cache* t "True to load and write compiled files.")
(defvar *cache-ready* nil "Set once Guile has booted: boot-9 itself isn't cached.")
(defvar *trace-cache* nil)

(defun guile-cache-p ()
  (and *guile-cache* *cache-ready*
       (not (member (uiop:getenv "PSEUDOSCHEME_GUILE_CACHE") '("0" "no" "off") :test #'equalp))))

(defun writable-directory-p (dir)
  (ignore-errors
   (ensure-directories-exist dir)
   (let ((probe (merge-pathnames (format nil ".write-~36R" (random (expt 36 8) (make-random-state t))) dir)))
     (with-open-file (out probe :direction :output :if-exists :supersede) (write-char #\x out))
     (delete-file probe)
     t)))

(defvar *cache-parents* '() "(env . xdg) -> the cache's directory, for GUILE-CACHE-PARENT.")

(defun guile-cache-parent ()
  "Where the cache goes: PSEUDOSCHEME_GUILE_CACHE_DIRECTORY, else the XDG
cache directory, else, if a program has pointed that at a directory it
can't write (LilyPond's is its installation's), ~/.cache/."
  (let* ((env (uiop:getenv "PSEUDOSCHEME_GUILE_CACHE_DIRECTORY"))
	 (xdg (uiop:xdg-cache-home "pseudoscheme" "guile/"))
	 (key (cons env xdg)))
    (cdr (or (assoc key *cache-parents* :test #'equal)
	     (car (push (cons key (cond ((and env (plusp (length env))) (uiop:ensure-directory-pathname env))
					((writable-directory-p xdg) xdg)
					(t (merge-pathnames ".cache/pseudoscheme/guile/" (user-homedir-pathname)))))
			*cache-parents*))))))

(defun guile-cache-directory ()
  "This build's generation of the cache (psx::CACHE-GENERATION): the old
ones are pruned."
  (psx::cache-generation (guile-cache-parent) (psx::build-signature)))

(defun cache-fasl (file)
  (let ((true (probe-file file)))
    (and true
	 (merge-pathnames
	  (format nil "~36R.fasl"
		  (psx::fnv-1a (format nil "~A ~A ~A ~A" (namestring true) (file-write-date true)
				       (psx::build-signature) psx::*full-continuations*)))
	  (guile-cache-directory)))))

;;; ------------------------------------------------------------------
;;; Load forms for the literals of compiled code

(defun cached-module (name)
  (or (resolve-module* name)
      (error "compiled Guile code refers to module ~S, which isn't there" name)))

(defmethod make-load-form ((s gstruct) &optional environment)
  (declare (ignore environment))
  ;; only a module registered under its name can be found again by it:
  ;; an anonymous one's name is a gensym (" g123"), new in each session
  (let ((name (and (module-p* s) (module-slot s +module-name+))))
    (if (and (consp name) (every #'symbolp name)
	     (notany (lambda (part) (let ((n (ps:scheme-symbol-name part))) (and (plusp (length n)) (char= (char n 0) #\Space))))
		     name)
	     (eq s (ignore-errors
		    (funcall (root-value "resolve-module") name ps:false ps:false :ensure ps:false))))
	`(cached-module ',name)
	(error "~S can't be in a compiled Guile file" s))))

(defmethod make-load-form ((s syntax-object) &optional environment)
  (make-load-form-saving-slots s :environment environment))

(defmethod make-load-form ((s site) &optional environment)
  (declare (ignore environment))
  `(make-site ',(site-module s) ',(site-name s) ',(site-public s) ',(site-kind s)))

(psx::register-named-photon "nil" *elisp-nil*)
(psx::register-named-photon "unbound" +unbound+)

;;; ------------------------------------------------------------------
;;; Evaluating a file's forms, keeping their code

(defun translate-top-level (core) (psx::translate-top-level core))

(defun run-cached (thunk) (psx::run-top-level thunk))

(defun eval-translated (forms)
  (let ((values '())
	(sb-ext:*evaluator-mode* :compile))
    (dolist (f forms)
      (setq values (multiple-value-list
		    (handler-bind ((warning #'muffle-warning))
		      (if psx::*full-continuations*
			  (psx::call-with-full-policy
			   (lambda () (psx::call-with-continuation-base (lambda () (psx::eval-compiled-or-interpreted f)))))
			  (psx::eval-compiled-or-interpreted f))))))
    (values-list values)))

(defun tree-il->core (tree)
  (let* ((*lexicals* (make-hash-table :test 'eq))
	 (*compile-module* *current-module*)
	 (core (compile-tree-il tree)))
    (if (> (psx::tree-size core) psx::*letrec-definitions-limit*)
	(flatten-top-level-lets core)
	core)))

;;; A file being loaded and cached is compiled form by form: each
;;; top-level form's Lisp code goes through COMPILE-FILE into a chunk
;;; fasl, which is then loaded to run it, and when the whole file has
;;; loaded the chunks, concatenated, are its fasl.  So the code is
;;; compiled once, and nothing of it is held until the file is done:
;;; Guix's package modules load each other in a cycle of some 400, all of
;;; them loading at once, and keeping each one's code for its fasl (as
;;; this did) or compiling it in memory as well took more than 11 GB.  A
;;; form that can't be compiled to a file (a literal with no load form)
;;; runs in memory, and the file isn't cached.

(defstruct (incremental-cache (:constructor make-incremental-cache (source)))
  source
  (dir nil)				; the chunks' directory, made at the first
  (fasls '())				; the chunks, newest first
  (count 0)
  (failed nil))				; NIL, or why the file can't be cached

(defun chunk-directory (cache)
  (or (incremental-cache-dir cache)
      (setf (incremental-cache-dir cache)
	    (let ((dir (uiop:ensure-directory-pathname
			(merge-pathnames (format nil "chunks-~36R" (random (expt 36 8) (make-random-state t)))
					 (guile-cache-directory)))))
	      (ensure-directories-exist dir)
	      dir))))

(defun compile-chunk (forms cache)
  "FORMS compiled into a chunk fasl of CACHE: its path, or NIL if they
can't be."
  (when (> (reduce #'+ forms :key #'psx::tree-size) *cache-size-limit*)
    (return-from compile-chunk nil))
  (let* ((n (incf (incremental-cache-count cache)))
	 (dir (chunk-directory cache))
	 (lisp (merge-pathnames (format nil "c~D.lisp" n) dir))
	 (fasl (merge-pathnames (format nil "c~D.fasl" n) dir)))
    (handler-case
	(progn
	  (with-open-file (out lisp :direction :output :if-exists :supersede)
	    (format out "(cl:in-package \"SCHEME\")~%")
	    (dotimes (i (length forms))
	      (format out "(pseudoscheme-guile::~A ~D)~%"
		      (if (= i (1- (length forms))) "cached-last-form" "cached-form") i)))
	  (let ((*cache-forms* (coerce forms 'simple-vector)))
	    (declare (special *cache-forms*))	; defined below
	    (handler-bind ((warning #'muffle-warning))
	      (with-standard-io-syntax
		(let ((*package* (find-package "SCHEME"))
		      (*error-output* (make-broadcast-stream))
		      (*standard-output* (make-broadcast-stream)))
		  (multiple-value-bind (output warnings-p failure-p)
		      (psx::call-with-full-policy (lambda () (compile-cache-file lisp fasl :collect nil)))
		    (declare (ignore warnings-p))
		    (when (or failure-p (null output)) (error "compiling failed")))))))
	  (ignore-errors (delete-file lisp))
	  fasl)
      (error (e)
	(when *trace-cache* (format *trace-output* "~&;; not cached ~A: ~A~%" (incremental-cache-source cache) e))
	(ignore-errors (delete-file lisp))
	nil))))

(defvar *chunk-values* nil "The values of the last form a chunk ran.")

(defmacro cached-last-form (n)
  "As CACHED-FORM, keeping the form's values for EVAL-KEEPING."
  `(setq *chunk-values* (multiple-value-list (run-cached (lambda () ,(svref *cache-forms* n))))))

(defun eval-keeping (x kept)
  "Evaluate X, a form read from a file being compiled and cached, KEPT
being its INCREMENTAL-CACHE: from a chunk fasl, unless it can't be."
  (let* ((tree (cond ((node-type x) x)
		     ((eq (current-transformer) #'pre-expand) (pre-expand x))
		     ;; as Guile's compile-file expands: a define-syntax is
		     ;; then code (it is also made a macro as it is expanded)
		     (t (funcall (current-transformer) x (ssym "c")
				 (list (ssym "compile") (ssym "load"))))))
	 (forms (translate-top-level (tree-il->core tree)))
	 (fasl (and (not (incremental-cache-failed kept)) (compile-chunk forms kept))))
    (if (null fasl)
	(progn (setf (incremental-cache-failed kept) t)
	       (eval-translated forms))
	(progn (push fasl (incremental-cache-fasls kept))
	       (let ((*chunk-values* (list *unspecified*)))
		 (handler-bind ((warning #'muffle-warning))
		   (load fasl))
		 (values-list *chunk-values*))))))

(defun finish-incremental-cache (cache fasl)
  "The chunks of CACHE as the file's FASL, if all its forms made one;
and the chunks gone."
  (unwind-protect
       (when (and fasl (not (incremental-cache-failed cache)) (incremental-cache-fasls cache))
	 (let ((temp (make-pathname :name (format nil "~A-~36R" (pathname-name fasl) (random (expt 36 8) (make-random-state t)))
				    :defaults fasl)))
	   (with-open-file (out temp :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
	     (dolist (chunk (reverse (incremental-cache-fasls cache)))
	       (write-sequence (psx::read-file-bytes chunk) out)))
	   (uiop:rename-file-overwriting-target temp fasl)
	   (when *trace-cache* (format *trace-output* "~&;; cached ~A~%" (incremental-cache-source cache)))))
    (when (incremental-cache-dir cache)
      (ignore-errors (uiop:delete-directory-tree (incremental-cache-dir cache) :validate t)))))

;;; ------------------------------------------------------------------
;;; Writing and loading fasls

(defvar *cache-forms* #() "The forms COMPILE-FILE is compiling, for CACHED-FORM.")

(defmacro cached-form (n)
  "The Nth form being compiled, with its literals as they are."
  `(run-cached (lambda () ,(svref *cache-forms* n))))

(defparameter *cache-size-limit* 1500000
  "The most code, in conses, a file's forms may have to be cached: SBCL's
COMPILE-FILE of more (Guix's (guix build syscalls), 2M) can exhaust an
8 GB heap, where compiling the same forms one at a time as they load
doesn't.  A bigger file is compiled each time it is loaded.")

(defun write-cached-file (fasl source forms)
  (when (> (reduce #'+ forms :key #'psx::tree-size) *cache-size-limit*)
    (when *trace-cache* (format *trace-output* "~&;; not cached ~A: too big~%" source))
    (return-from write-cached-file nil))
  (let* ((temp (format nil "~A-~36R" (pathname-name fasl) (random (expt 36 8) (make-random-state t))))
	 (lisp (make-pathname :name temp :type "lisp" :defaults fasl))
	 (temp-fasl (make-pathname :name temp :type "fasl" :defaults fasl)))
    (unwind-protect
	 (handler-case
	     (progn
	       (ensure-directories-exist lisp)
	       (with-open-file (out lisp :direction :output :if-exists :supersede)
		 (format out ";;; ~A, compiled by Pseudoscheme's Guile mode~%" source)
		 (format out "(cl:in-package \"SCHEME\")~%")
		 (dotimes (i (length forms)) (format out "(pseudoscheme-guile::cached-form ~D)~%" i)))
	       (let ((*cache-forms* (coerce forms 'simple-vector)))
		 (handler-bind ((warning #'muffle-warning))
		   (with-standard-io-syntax
		     (let ((*package* (find-package "SCHEME"))
			   (*error-output* (make-broadcast-stream))
			   (*standard-output* (make-broadcast-stream)))
		       (multiple-value-bind (output warnings-p failure-p)
			   (psx::call-with-full-policy
			    (lambda () (compile-cache-file lisp temp-fasl)))
			 (declare (ignore warnings-p))
			 (when (or failure-p (null output))
			   (error "compiling ~A failed" source))))))
		 (uiop:rename-file-overwriting-target temp-fasl fasl)
		 (when *trace-cache*
		   (format *trace-output* "~&;; cached ~A~@[ (heap ~D MB after a full GC)~]~%" source
			   #+sbcl (floor (sb-kernel:dynamic-usage) 1048576) #-sbcl nil))))
	   (error (e)
	     (when *trace-cache* (format *trace-output* "~&;; not cached ~A: ~A~%" source e))))
      (ignore-errors (delete-file lisp))
      (ignore-errors (when (probe-file temp-fasl) (delete-file temp-fasl))))))

(defun compile-cache-file (lisp fasl &key (collect t))
  "COMPILE-FILE's values for LISP.  A form whose code is too big for SBCL
(a branch past its reach, on arm64) is compiled again with
PSX::*COMPACT-FRAMES*.  COLLECT: a full GC after (not for one form's
chunk: a full GC per form makes loading Guix's package modules take
hours)."
  (flet ((try ()
	   (multiple-value-prog1
	       (with-compilation-unit (:override t)
		 (handler-case (compile-file lisp :output-file fasl :verbose nil :print nil)
		   (error () (values nil t t))))
	     ;; compiling a big file leaves much garbage in older
	     ;; generations, which a rebuild of the whole cache in one
	     ;; process would otherwise run out of heap with
	     #+sbcl (when collect (sb-ext:gc :full t)))))
    (multiple-value-bind (output warnings-p failure-p) (try)
      (if (and output (not failure-p))
	  (values output warnings-p failure-p)
	  (let ((psx::*compact-frames* t)) (try))))))

(defun load-cached (filename)
  "Load FILENAME's fasl if there is one: true if so."
  (let ((fasl (cache-fasl filename)))
    (when (and fasl (probe-file fasl))
      (when *trace-cache* (format *trace-output* "~&;; from the cache ~A~%" filename))
      (handler-bind ((warning #'muffle-warning))
	(load fasl))
      t)))
