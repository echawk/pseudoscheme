; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-PSYNTAX -*-

;;;; Compiled libraries: a cache of expanded, compiled library files
;;;;
;;;; Importing a library from source means expanding it with psyntax,
;;;; translating the result to Lisp and compiling that with COMPILE,
;;;; which takes most of the time.  After a library is expanded from a
;;;; file, its visit and invoke code are translated and compiled with
;;;; COMPILE-FILE into a fasl under *LIBRARY-CACHE-DIRECTORY*, along with
;;;; what psyntax's INSTALL-LIBRARY needs: its id, name, version, the
;;;; specs of the libraries it was expanded against, and its export
;;;; substitution and environment (plain data: psyntax's labels and marks
;;;; are interned gensyms, and its records are vectors).
;;;;
;;;; Before psyntax expands a library from source, LOAD-COMPILED-LIBRARY
;;;; looks for a fasl keyed by the library's form (after include and
;;;; cond-expand) and by BUILD-SIGNATURE.  It installs the library from
;;;; the fasl only if every library it was expanded against now has the
;;;; same id: a dependency that was expanded again has a new id, so its
;;;; dependents are expanded again too (and recompiled).
;;;;
;;;; Library ids are gensyms.  Those of psyntax's image and of the R7RS
;;;; standard libraries (expanded at boot with WITH-BOOT-GENSYMS) are the
;;;; same in every session; those of libraries loaded from the cache are
;;;; the ones they were compiled with.  A library expanded in this
;;;; session whose id is neither (a REPL definition, say) has dependents
;;;; that can't be cached, so they aren't written.

(in-package "PSEUDOSCHEME-PSYNTAX")

(defvar *library-cache* t
  "True to load and write compiled libraries, unless the environment
variable PSEUDOSCHEME_LIBRARY_CACHE is 0, no or off.")

(defvar *library-cache-directory* nil
  "Where compiled libraries are written; if NIL, the directory named by
the environment variable PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY, else
pseudoscheme/libraries/ in the XDG cache directory (~/.cache).")

;;; The environment is consulted when the cache is used, not when this
;;; file is loaded: a saved image (the command line's) reads it at run
;;; time.

(defun library-cache-p ()
  (and *library-cache*
       (not (member (uiop:getenv "PSEUDOSCHEME_LIBRARY_CACHE") '("0" "no" "off")
		    :test #'equalp))))

(defun library-cache-directory ()
  (or *library-cache-directory*
      (let ((dir (uiop:getenv "PSEUDOSCHEME_LIBRARY_CACHE_DIRECTORY")))
	(and dir (plusp (length dir)) (uiop:ensure-directory-pathname dir)))
      (uiop:xdg-cache-home "pseudoscheme" "libraries/")))

(defvar *pending-libraries* '()
  "(name key text form) for each library being looked for: LOCATE-LIBRARY
returns its form rather than reading it again, and the library is
written to the cache once it has been expanded.")

(defvar *cached-ids* (make-hash-table :test 'eq)
  "Ids of libraries this session expanded and wrote to the cache.")

(defvar *compiled-library* nil
  "Set by a compiled library's fasl as it loads.")

;;; ------------------------------------------------------------------
;;; Keys

(defun fnv-1a (string &optional (hash #xcbf29ce484222325))
  (declare (type (unsigned-byte 64) hash))
  (loop for c across string
	do (setq hash (ldb (byte 64 0) (* (logxor hash (char-code c)) #x100000001b3))))
  hash)

(defun compute-build-signature ()
  "A digest of what compiled code depends on: the Lisp, and the files
Pseudoscheme is built from (their names and dates)."
  (let ((root (asdf:system-source-directory :pseudoscheme)))
    (format nil "~36R"
	    (fnv-1a
	     (with-output-to-string (s)
	       (format s "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
	       (dolist (pattern '("src/*.lisp" "src/*.pso" "src/r6rs/*.lisp" "src/r7rs/*.*"
				  "src/compat/*.*" "src/interop/*.*"
				  "vendor/psyntax/psyntax-pseudoscheme.pp"))
		 (dolist (file (sort (mapcar #'namestring (directory (merge-pathnames pattern root)))
				     #'string<))
		   (format s "~A ~A~%" file (file-write-date file)))))))))

;;; Computed as this file loads, so that a saved image keeps the
;;; signature of what it was built from.
(defvar *build-signature* (compute-build-signature))

(defun build-signature () *build-signature*)

(defun form-text (form)
  "FORM printed readably, or NIL if it can't be (it holds a Lisp
object, such as the (cl <package>) bridge's functions)."
  (handler-case
      (with-standard-io-syntax
	(let ((*package* (find-package "SCHEME"))
	      (*print-circle* t)
	      (*print-readably* t))
	  (prin1-to-string form)))
    (error () nil)))

(defun library-key (text)
  (format nil "~36R" (fnv-1a text (fnv-1a (format nil "~A ~A" (build-signature)
						  (if *full-continuations* "full" "escape"))))))

(defun library-fasl (key)
  (merge-pathnames (make-pathname :name key :type "fasl") (library-cache-directory)))

;;; ------------------------------------------------------------------
;;; Which ids are the same from session to session

(defun session-gensym-p (symbol)
  (let ((name (symbol-name symbol)))
    (and (> (length name) (length *gensym-prefix*))
	 (string= *gensym-prefix* name :end2 (length *gensym-prefix*)))))

(defun stable-id-p (id)
  (or (not (session-gensym-p id)) (gethash id *cached-ids*)))

(defmacro with-boot-gensyms (&body body)
  "Run BODY with gensyms named the same in every session: for libraries
that are always expanded at boot, so that their ids, labels and
locations are the ones compiled libraries were expanded against."
  `(let ((*gensym-prefix* "g$boot$") (*gensym-count* 0))
     ,@body))

;;; ------------------------------------------------------------------
;;; Loading

(defun register-compiled-library (text &rest install-arguments)
  "Called by a compiled library's fasl."
  (setq *compiled-library* (cons text install-arguments)))

(defun load-compiled-library (name)
  "psyntax's LIBRARY-LOADER: install library NAME from the cache and
return #t, or return #f for psyntax to expand it from source."
  (let ((form (and (library-cache-p) (locate-library name))))
    (when (or (null form) (eq form ps:false))
      (return-from load-compiled-library ps:false))
    (let ((text (form-text form)))
      (unless text (return-from load-compiled-library ps:false))
      (let* ((key (library-key text))
	     (fasl (library-fasl key)))
	(push (list name key text form) *pending-libraries*)
	(let ((compiled (and (probe-file fasl)
			     (let ((*compiled-library* nil))
			       (handler-case
				   (handler-bind ((warning #'muffle-warning))
				     (load fasl)
				     *compiled-library*)
				 (error () nil))))))
	  (if (and compiled (equal (car compiled) text) (dependencies-current-p (cdr compiled)))
	      (destructuring-bind (id name version imp* vis* inv* subst env visit invoke)
		  (cdr compiled)
		(funcall (entry "psyntax:install-library")
			 id name version imp* vis* inv* subst env visit invoke t)
		(setf *pending-libraries* (remove name *pending-libraries* :key #'car :test #'equal))
		t)
	      ps:false))))))

(defun dependencies-current-p (install-arguments)
  "Whether each library a compiled library was expanded against is
installed (or can be) with the same id."
  (destructuring-bind (id name version imp* vis* inv* &rest more) install-arguments
    (declare (ignore id name version more))
    (every (lambda (spec)
	     (let ((current (handler-case (funcall (entry "psyntax:library-spec-by-name") (cadr spec))
			      (error () nil))))
	       (and current (eq (car current) (car spec)))))
	   (remove-duplicates (append imp* vis* inv*) :key #'car))))

;;; ------------------------------------------------------------------
;;; Writing

;;; The translator records each global it sees defined as a procedure,
;;; and compiles later calls to it as calls to a Lisp function; it then
;;; compiles another definition of it as an assignment.  So a library's
;;; code is translated once, here, and that translation serves both the
;;; fasl and this session: the library is installed again with thunks
;;; from the fasl (or, if it couldn't be written, thunks that evaluate
;;; the same translation).

(defun library-expanded (id name version imp* vis* inv* subst env visit-thunk invoke-thunk)
  "psyntax's LIBRARY-EXPANDED-HOOK: write library NAME to the cache if
it was being looked for (it came from a file) and everything it was
expanded against is the same from session to session."
  (let ((pending (assoc name *pending-libraries* :test #'equal)))
    (when pending
      (setf *pending-libraries* (remove pending *pending-libraries*))
      (when (and (library-cache-p)
		 (every (lambda (spec) (stable-id-p (car spec))) (append imp* vis* inv*)))
	(destructuring-bind (key text form) (cdr pending)
	  (declare (ignore form))
	  (let* ((data (list id name version imp* vis* inv* subst env))
		 (visit (thunk-form (definitions (funcall visit-thunk))))
		 (invoke (thunk-form (funcall invoke-thunk)))
		 (compiled (write-compiled-library key text data visit invoke)))
	    (if compiled
		(setf (gethash id *cached-ids*) t)
		(setq compiled (list* text (append data (list (eval visit) (eval invoke))))))
	    (apply (entry "psyntax:install-library") (append (cdr compiled) (list t)))))))
    ps:unspecific))

(defun definitions (visit-code)
  "VISIT-CODE, a sequence of global assignments of macro transformers,
with the assignments made definitions: psyntax gives the locations
their values directly, and the translator warns of assignments to
undefined variables."
  (cond ((and (consp visit-code) (keyword-p (car visit-code) "BEGIN"))
	 (cons (car visit-code) (mapcar #'definitions (cdr visit-code))))
	((and (consp visit-code) (keyword-p (car visit-code) "SET!"))
	 (cons (sym "define") (cdr visit-code)))
	(t visit-code)))

(defun thunk-form (core)
  "A Lisp lambda expression running CORE, a psyntax core form."
  (let* ((form (open-primitives core))
	 (body (handler-bind ((warning #'muffle-warning))
		 (psl:tr "TRANSLATE" (if *full-continuations* (cc-transform form) form) *host*))))
    (if *full-continuations*
	`(lambda () (call-with-continuation-base (lambda () ,body)))
	`(lambda () ,body))))

(defun write-compiled-library (key text data visit invoke)
  "Compile a library into its fasl and load it: what it registers, or
NIL if that didn't work."
  (let* ((fasl (library-fasl key))
	 (temp (format nil "~A-~36R" key (random (expt 36 8) (make-random-state t))))
	 (lisp (make-pathname :name temp :type "lisp" :defaults fasl))
	 (temp-fasl (make-pathname :name temp :type "fasl" :defaults fasl)))
    (prog1
	(handler-case
	    (let ((code `(register-compiled-library
			  ',text ,@(mapcar (lambda (x) `',x) data) ,visit ,invoke)))
	      (ensure-directories-exist lisp)
	      (with-open-file (out lisp :direction :output :if-exists :supersede)
		(with-standard-io-syntax
		  (let ((*package* (find-package "SCHEME"))
			(*print-circle* t)
			(*print-readably* t))
		    (format out ";;; ~A, compiled by Pseudoscheme.~%" (second data))
		    (print `(in-package "SCHEME") out)
		    (print code out))))
	      (handler-bind ((warning #'muffle-warning))
		(with-standard-io-syntax
		  (let ((*package* (find-package "SCHEME")))
		    (multiple-value-bind (output warnings-p failure-p)
			(if *full-continuations*
			    (call-with-full-policy (lambda () (compile-file lisp :output-file temp-fasl :verbose nil :print nil)))
			    (compile-file lisp :output-file temp-fasl :verbose nil :print nil))
		      (declare (ignore warnings-p))
		      (when (or failure-p (null output))
			(error "compiling ~A failed" lisp))))))
	      (uiop:rename-file-overwriting-target temp-fasl fasl)
	      (let ((*compiled-library* nil))
		(handler-bind ((warning #'muffle-warning))
		  (load fasl))
		*compiled-library*))
	  (error () nil))
      (ignore-errors (delete-file lisp))
      (ignore-errors (when (probe-file temp-fasl) (delete-file temp-fasl))))))

;;; ------------------------------------------------------------------

(defun install-library-cache-hooks ()
  "Point psyntax's library loader and expansion hook here (in an image
that has them)."
  (when (and (boundp (location (sym "psyntax:library-loader")))
	     (boundp (location (sym "psyntax:library-expanded-hook"))))
    (funcall (host-ref "psyntax:library-loader") #'load-compiled-library)
    (funcall (host-ref "psyntax:library-expanded-hook") #'library-expanded)))
