; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Racket CS's schemify layer as a linklet (racket/src/schemify), for its
;;;; linklet interpreter (schemify/interpret.rkt): Racket CS runs a
;;;; linklet compiled 'quick (an expand-time one, run once) by
;;;; schemifying it for the interpreter and interpreting it, not compiling
;;;; it.  utils/racket/extract.sh schemify makes
;;;; build/external/racket-derived/schemify.rktl, a linklet with no
;;;; imports; SCHEMIFY-EXPORT instantiates it the first time it's asked,
;;;; as the regexp layer is (regexp-layer.lisp).

(in-package "QUASISCHEME-RACKET")

(defvar *schemify-instance* nil "The schemify layer's instance, once instantiated.")

(defun derived-schemify-file ()
  (asdf:system-relative-pathname :quasischeme "build/external/racket-derived/schemify.rktl"))

(defun instantiate-schemify-layer ()
  "Compile and instantiate the schemify layer: its instance, or NIL if it
hasn't been extracted or doesn't instantiate."
  (or *schemify-instance*
      (let ((file (probe-file (derived-schemify-file))))
	(when file
	  (let* ((text (uiop:read-file-string file :external-format :utf-8))
		 (form (with-input-from-string (in text) (read-racket in)))
		 (compiled (compile-linklet-form form :name (rsym "schemify")))
		 (instance (make-linklet-instance (rsym "schemify"))))
	    (setf (compiled-linklet-source-key compiled) text)
	    (linklet-function compiled)
	    (setq *uncaught* nil)
	    (call-with-continuation-prompt
	     (lambda () (with-racket-errors (instantiate-linklet compiled '() instance)))
	     *default-prompt-tag* nil)
	    (unless *uncaught*
	      (setq *schemify-instance* instance)))))))

(defun schemify-export (name)
  "The schemify layer's export NAME (instantiating it), or NIL."
  (let ((instance (instantiate-schemify-layer)))
    (and instance (instance-value instance (rsym name)))))
