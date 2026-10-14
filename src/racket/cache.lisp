; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Compiled linklets, for all three strategies (docs/racket.md,
;;;; "Compiled code"):
;;;;   - what a .zo holds: each linklet's S-expression after lowering
;;;;     (A, C: schemify's output; B: after our passes 1 to 3), which is
;;;;     machine-independent, so Racket's compilation manager and raco
;;;;     make work unchanged;
;;;;   - the SBCL fasl: in a cache directory (racket/ beside the library
;;;;     cache, src/library-cache.lisp), keyed by the linklet's source
;;;;     text, the build signature, the dates of src/racket/'s files, the
;;;;     continuations mode and the strategy.
;;;;
;;;; Written so far: the fasl of a big (split) linklet with a source text
;;;; to key it by, which is the expander's: compiling it takes 25 s,
;;;; loading its fasl a few.  The fasl, loaded, hands each top-level
;;;; form's function to RACKET:NOTE-SPLIT-FUNCTION.  The load forms of
;;;; the Racket values a literal can be are with their types (reader.lisp,
;;;; linklet/api.lisp).

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (compiled-linklet (:constructor make-compiled-linklet (form &key name strategy)))
  form					; the lowered S-expression
  name
  strategy				; "cs" / "linklet" / "hybrid"
  (imports '())				; per import set, the external names
  (exports '())				; the external names of the exports
  (export-internals '())		; ... and their names inside the linklet
  (source-key nil)			; a string identifying its source, for the cache
  (function nil))			; once loaded: the instantiation function

(defun racket-signature ()
  "What compiled linklets depend on besides their source: the build
signature, the continuations mode, and src/racket/'s files' dates."
  (let ((root (asdf:system-source-directory :pseudoscheme)))
    (format nil "~A ~A~{ ~A~}" (psx::build-signature) psx::*full-continuations*
	    (loop for file in (sort (mapcar #'namestring
					    (directory (merge-pathnames "src/racket/**/*.lisp" root)))
				    #'string<)
		  collect (format nil "~A:~A" (file-namestring file) (file-write-date file))))))

(defun linklet-key (compiled)
  "The cache's key for COMPILED, if it has a source key."
  (let ((source (compiled-linklet-source-key compiled)))
    (and source
	 (format nil "~36R-~36R"
		 (psx::fnv-1a source)
		 (psx::fnv-1a (format nil "~A ~A" (racket-signature) (compiled-linklet-strategy compiled)))))))

(defun linklet-fasl (key)
  (merge-pathnames (make-pathname :name key :type "fasl")
		   (uiop:merge-pathnames* "../racket/" (psx::library-cache-directory))))

(defvar *loading-split-functions* nil
  "While a split linklet's fasl loads: the vector its functions go in.")

(defun write-split-fasl (path forms)
  "FORMS, a split linklet's (lambda (instance cells) ...) forms, compiled
into the fasl at PATH."
  (psx::write-compiled-file
   path
   (loop for form in forms
	 for i from 0
	 append (psx::translate-top-level
		 (list (core "racket:note-split-function") (quoted i) form)))
   :source "a Racket linklet"))

(defun load-split-fasl (path n)
  "The N functions of the split linklet whose fasl is at PATH."
  (let ((*loading-split-functions* (make-array n :initial-element nil)))
    (psx::load-compiled-file path)
    (when (find nil *loading-split-functions*)
      (error "~A doesn't hold all ~D forms" path n))
    (coerce *loading-split-functions* 'list)))

(defun split-functions (compiled)
  "The functions of split linklet COMPILED's forms: from its fasl if
there is one, else compiled now, into the cache if COMPILED has a key."
  (let* ((form (compiled-linklet-form compiled))
	 (n (length (cddr form)))
	 (key (linklet-key compiled))
	 (fasl (and key (linklet-fasl key))))
    (or (and fasl (probe-file fasl)
	     (ignore-errors (load-split-fasl fasl n)))
	(and fasl
	     (ignore-errors
	      (ensure-directories-exist fasl)
	      (write-split-fasl fasl (cddr form))
	      (load-split-fasl fasl n)))
	(mapcar #'psx:host-eval (cddr form)))))

(defun write-linklet-bundle (bundle stream)
  "write-linklet-bundle-hash: BUNDLE (phase or key -> compiled linklet or
datum) to STREAM, in our .zo format."
  (declare (ignore bundle stream))
  (not-yet "write-linklet-bundle"))

(defun read-linklet-bundle (stream)
  (declare (ignore stream))
  (not-yet "read-linklet-bundle"))

;;; (system-type 'vm) answers this, so that Racket code that checks for
;;; chez-scheme doesn't take paths meant for Racket CS's machine code.
(defparameter *vm-name* "pseudoscheme")
