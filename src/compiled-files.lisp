; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; Compiled files of top-level code: core forms translated to Lisp as
;;;; HOST-EVAL translates them, compiled with COMPILE-FILE into a fasl
;;;; that, loaded, runs them as HOST-EVAL would have.  Chez's
;;;; compile-program writes one (src/chez/compile.lisp); Guile's
;;;; compiled-module cache shares the translation (src/guile/cache.lisp).
;;;;
;;;; The forms aren't printed: the file COMPILE-FILE compiles holds
;;;; (COMPILED-FORM n), a macro that expands into the Nth form as it is,
;;;; so its literals are the objects themselves, dumped with their load
;;;; forms.  A form of a program translated from one big body is the
;;;; definitions HOIST-DEFINITIONS made of it, one at a time, and the
;;;; forms are compiled in chunks, so that compiling a program of
;;;; thousands of definitions (Idris 2's compiler, 11,000) takes no more
;;;; memory than its biggest definition does.  SBCL loads concatenated
;;;; fasls one after another, so the chunks' fasls are one file.

(in-package "PSEUDOSCHEME-PSYNTAX")

;;; ------------------------------------------------------------------
;;; Load forms for the literals of compiled code

(defvar *named-photons*
  (list (cons "unspecific" ps:unspecific) (cons "eof" ps:eof-object)
	;; a letrec variable's value before its init (compile.lisp)
	(cons "unassigned" ps:unassigned))
  "The photons compiled code may hold, by name: they are unique objects.")

(defun register-named-photon (name photon)
  (setq *named-photons* (cons (cons name photon) (remove name *named-photons* :key #'car :test #'string=))))

(defun named-photon (name)
  (or (cdr (assoc name *named-photons* :test #'string=))
      (error "no photon named ~S" name)))

(defmethod make-load-form ((p ps::photon) &optional environment)
  (declare (ignore environment))
  (let ((entry (rassoc p *named-photons* :test #'eq)))
    (if entry
	`(named-photon ,(car entry))
	(error "~S can't be in a compiled file" p))))

;;; ------------------------------------------------------------------
;;; Translating and running top-level forms

(defun translate-top-level (core)
  "CORE's Lisp forms, to be run in order (see RUN-TOP-LEVEL), as
HOST-EVAL runs it: escape-only, a top-level PROGN is its forms."
  (labels ((head-p (form name)
	     (and (consp form) (symbolp (car form)) (string= (symbol-name (car form)) name)))
	   (split (form)
	     (if (and (head-p form "PROGN") (cdr form)) (mapcan #'split (cdr form)) (list form))))
    (handler-bind ((warning #'muffle-warning))
      (if *full-continuations*
	  (let ((form (cc-transform (open-top-level core))))
	    (mapcar (lambda (f) (translate-core f t))
		    (if (and (head-p form "BEGIN") (cdr form)) (cdr form) (list form))))
	  (split (translate-core (open-top-level core) t))))))

(defun run-top-level (thunk)
  "Run THUNK, a compiled top-level form, as HOST-EVAL runs one."
  (if *full-continuations*
      (call-with-continuation-base thunk)
      (funcall thunk)))

;;; ------------------------------------------------------------------
;;; Writing compiled files

(defvar *compiled-forms* #() "The forms COMPILE-FILE is compiling, for COMPILED-FORM.")

(defmacro compiled-form (n)
  "The Nth form being compiled, with its literals as they are."
  `(run-top-level (lambda () ,(svref *compiled-forms* n))))

(defparameter *compiled-chunk-size* 500000
  "About how much code, in conses, one COMPILE-FILE compiles.")

(defun fasl-header-line-end (bytes)
  "The index after BYTES' first line if it is a #! line, else 0."
  (if (and (> (length bytes) 2) (= (aref bytes 0) (char-code #\#)) (= (aref bytes 1) (char-code #\!)))
      (let ((nl (position 10 bytes))) (if nl (1+ nl) 0))
      0))

(defun read-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defvar *accept-compile-failure* nil
  "True if a fasl whose compilation reported failure (SBCL turned a
compile-time error in some code into a call that signals it when it
runs, as EVAL does) is kept: a Racket linklet's, which has code that
is never reached.")

(defun compile-forms-chunk (forms lisp fasl source)
  (with-open-file (out lisp :direction :output :if-exists :supersede)
    (format out ";;; ~A, compiled by Pseudoscheme~%(cl:in-package \"SCHEME\")~%" source)
    (dotimes (i (length forms)) (format out "(pseudoscheme-psyntax::compiled-form ~D)~%" i)))
  (let ((*compiled-forms* (coerce forms 'simple-vector)))
    (handler-bind ((warning #'muffle-warning))
      (with-standard-io-syntax
	(let ((*package* (find-package "SCHEME"))
	      (*error-output* (make-broadcast-stream))
	      (*standard-output* (make-broadcast-stream)))
	  (flet ((try ()
		   (multiple-value-bind (output warnings-p failure-p)
		       (with-compilation-unit (:override t)
			 (handler-case
			     (if *full-continuations*
				 (call-with-full-policy
				  (lambda () (compile-file lisp :output-file fasl :verbose nil :print nil)))
				 (compile-file lisp :output-file fasl :verbose nil :print nil))
			   (error () (values nil t t))))
		     (declare (ignore warnings-p))
		     ;; a big chunk leaves much garbage in older generations;
		     ;; a small one (a --racket linklet) isn't worth a full GC
		     #+sbcl (when (> (reduce #'+ forms :key #'tree-size) 100000) (sb-ext:gc :full t))
		     (and output (or (not failure-p) *accept-compile-failure*)))))
	    ;; a form too big for SBCL (a branch past its reach, on arm64)
	    ;; goes again with compact frames
	    (or (try)
		(let ((*compact-frames* t)) (try))
		(error "compiling ~A failed" source))))))))

(defun write-compiled-file (path forms &key header (source path))
  "Compile FORMS, Lisp forms as TRANSLATE-TOP-LEVEL makes them, into a
fasl at PATH: loaded, it runs them in order.  HEADER, if a string, is the
file's first line (a #! line).  Signals an error if they can't be
compiled (a literal with no load form, say)."
  (let* ((chunks (let ((chunks '()) (chunk '()) (size 0))
		   (dolist (f forms)
		     (push f chunk)
		     (incf size (tree-size f))
		     (when (> size *compiled-chunk-size*)
		       (push (nreverse chunk) chunks)
		       (setq chunk '() size 0)))
		   (when chunk (push (nreverse chunk) chunks))
		   (nreverse chunks)))
	 (dir (uiop:ensure-directory-pathname
	       (uiop:merge-pathnames* (format nil "pseudoscheme-compile-~36R/" (random (expt 36 8) (make-random-state t)))
				      (uiop:temporary-directory))))
	 (temp (make-pathname :name (format nil "~A-~36R" (pathname-name path) (random (expt 36 8) (make-random-state t)))
			      :type (pathname-type path) :defaults path)))
    (unwind-protect
	 (progn
	   (ensure-directories-exist dir)
	   (with-open-file (out temp :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
	     (when header
	       (write-sequence (sb-ext:string-to-octets (format nil "~A~%" header) :external-format :utf-8) out))
	     (loop for chunk in chunks
		   for i from 0
		   do (let ((lisp (merge-pathnames (format nil "c~D.lisp" i) dir))
			    (fasl (merge-pathnames (format nil "c~D.fasl" i) dir)))
			(compile-forms-chunk chunk lisp fasl source)
			(let ((bytes (read-file-bytes fasl)))
			  (write-sequence bytes out :start (fasl-header-line-end bytes))))))
	   (uiop:rename-file-overwriting-target temp path))
      (ignore-errors (when (probe-file temp) (delete-file temp)))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))
    path))

(defun compiled-file-p (path)
  "Is the file at PATH a fasl (after a #! line, if any)?"
  (handler-case
      (with-open-file (in path :element-type '(unsigned-byte 8))
	(let ((bytes (make-array 1024 :element-type '(unsigned-byte 8))))
	  (let* ((n (read-sequence bytes in))
		 (bytes (subseq bytes 0 n))
		 (start (fasl-header-line-end bytes))
		 (magic (map '(vector (unsigned-byte 8)) #'char-code "# FASL")))
	    (and (<= (+ start (length magic)) n)
		 (equalp (subseq bytes start (+ start (length magic))) magic)))))
    (error () nil)))

(defun load-compiled-file (path)
  (handler-bind ((warning #'muffle-warning))
    (load path))
  ps:unspecific)
