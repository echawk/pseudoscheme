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

(defun guile-cache-directory ()
  (let ((dir (uiop:getenv "PSEUDOSCHEME_GUILE_CACHE_DIRECTORY")))
    (if (and dir (plusp (length dir)))
	(uiop:ensure-directory-pathname dir)
	(uiop:xdg-cache-home "pseudoscheme" "guile/"))))

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

(defvar *named-photons* '()
  "The photons compiled code may hold, by name: they are unique objects.")

(defun named-photon (name)
  (unless *named-photons* (register-photons))
  (or (cdr (assoc name *named-photons* :test #'string=))
      (error "no photon named ~S" name)))

(defun register-photons ()
  (setq *named-photons*
	(list (cons "unspecific" ps:unspecific) (cons "eof" ps:eof-object)
	      (cons "nil" *elisp-nil*) (cons "unbound" +unbound+))))

(defmethod make-load-form ((p ps::photon) &optional environment)
  (declare (ignore environment))
  (register-photons)
  (let ((entry (rassoc p *named-photons* :test #'eq)))
    (if entry
	`(named-photon ,(car entry))
	(error "~S can't be in a compiled Guile file" p))))

;;; ------------------------------------------------------------------
;;; Evaluating a file's forms, keeping their code

(defun translate-top-level (core)
  "CORE's Lisp forms, to be evaluated in order, as HOST-EVAL evaluates it."
  (handler-bind ((warning #'muffle-warning))
    (if psx::*full-continuations*
	(let ((form (psx::cc-transform (psx::open-top-level core))))
	  (mapcar (lambda (f) (psx::translate-core f t))
		  (if (and (consp form) (symbolp (car form))
			   (string= (symbol-name (car form)) "BEGIN") (cdr form))
		      (cdr form)
		      (list form))))
	(list (psx::translate-core (psx::open-top-level core) t)))))

(defun run-cached (thunk)
  "Run THUNK, a compiled top-level form, as HOST-EVAL runs one."
  (if psx::*full-continuations*
      (psx::call-with-continuation-base thunk)
      (funcall thunk)))

(defun eval-translated (forms)
  (let ((values '()))
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

(defun eval-keeping (x kept)
  "Evaluate X, a form read from a file being compiled, adding its Lisp
code to KEPT (a list in a cons, newest first)."
  (let* ((tree (cond ((node-type x) x)
		     ((eq (current-transformer) #'pre-expand) (pre-expand x))
		     ;; as Guile's compile-file expands: a define-syntax is
		     ;; then code (it is also made a macro as it is expanded)
		     (t (funcall (current-transformer) x (ssym "c")
				 (list (ssym "compile") (ssym "load"))))))
	 (forms (translate-top-level (tree-il->core tree))))
    (setf (car kept) (revappend forms (car kept)))
    (eval-translated forms)))

;;; ------------------------------------------------------------------
;;; Writing and loading fasls

(defvar *cache-forms* #() "The forms COMPILE-FILE is compiling, for CACHED-FORM.")

(defmacro cached-form (n)
  "The Nth form being compiled, with its literals as they are."
  `(run-cached (lambda () ,(svref *cache-forms* n))))

(defun write-cached-file (fasl source forms)
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
			    (lambda () (compile-file lisp :output-file temp-fasl :verbose nil :print nil)))
			 (declare (ignore warnings-p))
			 (when (or failure-p (null output))
			   (error "compiling ~A failed" source))))))
		 (uiop:rename-file-overwriting-target temp-fasl fasl)
		 (when *trace-cache* (format *trace-output* "~&;; cached ~A~%" source))))
	   (error (e)
	     (when *trace-cache* (format *trace-output* "~&;; not cached ~A: ~A~%" source e))))
      (ignore-errors (delete-file lisp))
      (ignore-errors (when (probe-file temp-fasl) (delete-file temp-fasl))))))

(defun load-cached (filename)
  "Load FILENAME's fasl if there is one: true if so."
  (let ((fasl (cache-fasl filename)))
    (when (and fasl (probe-file fasl))
      (when *trace-cache* (format *trace-output* "~&;; from the cache ~A~%" filename))
      (handler-bind ((warning #'muffle-warning))
	(load fasl))
      t)))
