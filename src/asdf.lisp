; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-ASDF -*-

;;;; Scheme sources as ASDF components
;;;;
;;;;   (defsystem :my-app
;;;;     :defsystem-depends-on (:pseudoscheme/asdf)
;;;;     :depends-on (:alexandria)              ; Lisp libraries the Scheme uses
;;;;     :components ((:r7rs-library "lib/stats")  ; lib/stats.sld
;;;;                  (:r7rs-file "setup")         ; setup.scm, run at load
;;;;                  (:file "main")))             ; Lisp using both
;;;;
;;;; Component types (all in the ASDF package, so they can be written as
;;;; keywords in a defsystem):
;;;;
;;;;   :r7rs-library, :r6rs-library   a file of library definitions (.sld,
;;;;       .sls).  Loading the component puts the library's root
;;;;       directory -- the file's directory, less one level per part of
;;;;       the library's name -- on psx:*library-path* and installs the
;;;;       libraries, so Lisp code (r7rs:use-library) and Scheme code
;;;;       (import) later in the system can use them.
;;;;   :r7rs-file, :r6rs-file, :r5rs-file   a program or REPL-style
;;;;       source file (.scm, .sps), loaded with r7rs:load etc.
;;;;
;;;; This is how Scheme code that uses Lisp libraries is best
;;;; distributed: the system's :depends-on names the Lisp libraries, so
;;;; whatever the user installs systems with (Quicklisp, ocicl, qlot,
;;;; CLPM, a source registry) resolves them the usual way.
;;;;
;;;; Compiling a library component expands its libraries and compiles
;;;; them into the component's fasl, as the compiled-library cache
;;;; (src/library-cache.lisp) compiles libraries from the library path;
;;;; loading it installs them from there, unless a library it was
;;;; expanded against has changed since (or one of them can't be
;;;; compiled to a file: its expansion holds a Lisp object), when the
;;;; source is expanded again.  Program files (:r7rs-file ...) are run
;;;; from source when loaded.

(defpackage "PSEUDOSCHEME-ASDF"
  (:use "COMMON-LISP")
  (:export "SCHEME-SOURCE-FILE" "R7RS-FILE" "R6RS-FILE" "R5RS-FILE"
	   "R7RS-LIBRARY" "R6RS-LIBRARY"))

(in-package "PSEUDOSCHEME-ASDF")

(defclass scheme-source-file (asdf:source-file) ()
  (:documentation "A Scheme source file: compiling it does nothing, loading
it evaluates it (see PERFORM methods)."))

(defclass r7rs-file (scheme-source-file) () (:default-initargs :type "scm"))
(defclass r6rs-file (scheme-source-file) () (:default-initargs :type "sps"))
(defclass r5rs-file (scheme-source-file) () (:default-initargs :type "scm"))
(defclass r7rs-library (scheme-library-file) () (:default-initargs :type "sld"))
(defclass r6rs-library (scheme-library-file) () (:default-initargs :type "sls"))

(defclass scheme-library-file (scheme-source-file) ()
  (:documentation "A file of Scheme libraries, compiled into a fasl."))

(defmethod asdf:output-files ((o asdf:compile-op) (c scheme-source-file))
  (values nil t))

(defmethod asdf:perform ((o asdf:compile-op) (c scheme-source-file))
  nil)

(defmethod asdf:output-files ((o asdf:compile-op) (c scheme-library-file))
  (list (make-pathname :type "fasl" :defaults (asdf:component-pathname c))))

(defmethod asdf:perform ((o asdf:compile-op) (c scheme-library-file))
  (compile-library-file (asdf:component-pathname c) (first (asdf:output-files o c))))

(defmethod asdf:perform ((o asdf:load-op) (c scheme-library-file))
  (let ((fasl (first (asdf:output-files (asdf:make-operation 'asdf:compile-op) c))))
    (load-library-file (asdf:component-pathname c) fasl)))

(defun library-root (file name)
  "The directory FILE's library NAME is rooted in: (foo bar) in
.../lib/foo/bar.sld is rooted at .../lib/."
  (let ((dir (pathname-directory file))
	(depth (1- (count-if #'symbolp name))))
    (make-pathname :directory (butlast dir depth) :name nil :type nil :defaults file)))

(defun library-forms (file)
  "FILE's library forms, their library directories put on the library path."
  (let ((forms (ps-r7rs::read-forms file)))
    (dolist (form forms)
      (let ((name (ps-r7rs::library-name-of form)))
	(when name
	  (pseudoscheme-api:add-library-directory (library-root file name)))))
    (pseudoscheme-api::ensure-psyntax)
    (let ((ps-r7rs::*include-directory* (make-pathname :name nil :type nil :defaults file)))
      (mapcar (lambda (form) (ps-r7rs::resolve-includes (ps-r7rs::library-form form))) forms))))

(defun compile-library-file (file fasl)
  "Expand FILE's libraries and write their compiled code to FASL (the
cache's fasls of them, one after another); if one of them can't be
compiled to a file, FASL has none of them, and loading expands the
source."
  (let ((fasls (mapcar #'psx::compile-library-form (library-forms file))))
    (ensure-directories-exist fasl)
    (with-open-file (out fasl :direction :output :element-type '(unsigned-byte 8)
			      :if-exists :supersede)
      (when (every #'identity fasls)
	(dolist (f fasls)
	  (with-open-file (in f :element-type '(unsigned-byte 8))
	    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
	      (read-sequence buffer in)
	      (write-sequence buffer out))))))))

(defun load-library-file (file &optional fasl)
  "Install FILE's libraries: from FASL (COMPILE-LIBRARY-FILE's) if it has
them and what they were expanded against is unchanged, else from source."
  (let ((forms (library-forms file)))
    (unless (and fasl (probe-file fasl) (plusp (with-open-file (in fasl) (file-length in)))
		 (let ((psx::*compiled-libraries* '()))
		   (handler-case
		       (handler-bind ((warning #'muffle-warning))
			 (load fasl)
			 (and (= (length psx::*compiled-libraries*) (length forms))
			      (psx::install-compiled-libraries (reverse psx::*compiled-libraries*))))
		     (error () nil))))
      (dolist (form forms)
	(psx:eval-library form)))))

(defmethod asdf:perform ((o asdf:load-op) (c r7rs-file))
  (r7rs:load (asdf:component-pathname c)))

(defmethod asdf:perform ((o asdf:load-op) (c r6rs-file))
  (r6rs:load (asdf:component-pathname c)))

(defmethod asdf:perform ((o asdf:load-op) (c r5rs-file))
  (r5rs:load (asdf:component-pathname c)))

;;; ASDF looks component types up in its own package: make the names
;;; there denote these classes, so :r7rs-library etc. work in any
;;; defsystem.
(dolist (name '(r7rs-file r6rs-file r5rs-file r7rs-library r6rs-library))
  (setf (find-class (intern (symbol-name name) "ASDF/INTERFACE")) (find-class name)))
