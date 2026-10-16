; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's regexp layer as a linklet.
;;;;
;;;; The layer is racket/src/regexp (the parser, the matcher compiler,
;;;; the drivers for strings, bytes and ports, regexp-replace), which
;;;; Racket's own build extracts, expanded and flattened, as a linklet
;;;; with no imports (utils/racket/extract.sh makes
;;;; build/external/racket-derived/regexp.rktl).  It uses only
;;;; primitives (bytes, strings, hash tables, struct types, the port
;;;; peeking procedures, ...), which are the natives'; its exports are
;;;; the regexp procedures of #%kernel.
;;;;
;;;; INSTALL-REGEXP-LAYER compiles and instantiates it, and makes its
;;;; exports the natives of those names (regexp.lisp's, on CL-PPCRE,
;;;; stay as the fallback when the file is absent, or when
;;;; PSEUDOSCHEME_RACKET_REGEXP=native).  Compiled code calls a native by
;;;; its host global racket:NAME, so replacing the global replaces it
;;;; for the expander's linklet too.  The compiled form is cached, as
;;;; the expander's is, keyed by the linklet's text.
;;;;
;;;; A #rx or #px literal that our reader makes (REGEXP-LITERAL, in
;;;; quoted data and compiled code) isn't one of the layer's regexps; the
;;;; replacements turn it into one, compiled once.

(in-package "PSEUDOSCHEME-RACKET")

(defparameter *regexp-layer-exports*
  '("regexp" "pregexp" "byte-regexp" "byte-pregexp"
    "regexp?" "pregexp?" "byte-regexp?" "byte-pregexp?"
    "regexp-max-lookbehind" "regexp-capture-group-count"
    "regexp-match" "regexp-match/end" "regexp-match-positions" "regexp-match-positions/end"
    "regexp-match?"
    "regexp-match-peek" "regexp-match-peek-positions" "regexp-match-peek-positions/end"
    "regexp-match-peek-immediate" "regexp-match-peek-positions-immediate"
    "regexp-match-peek-positions-immediate/end"
    "regexp-replace" "regexp-replace*")
  "The layer's exports that are #%kernel primitives (it also exports
regexp-place-init!, for a new place: there are none).")

(defvar *regexp-instance* nil "The regexp layer's instance, once instantiated.")

(defvar *regexp-layer* (make-hash-table :test 'equal)
  "Export name -> the layer's own procedure.")

(defun derived-regexp-file ()
  (asdf:system-relative-pathname :pseudoscheme "build/external/racket-derived/regexp.rktl"))

(defun regexp-linklet-text ()
  "The regexp linklet's source, if it has been extracted; else NIL."
  (let ((file (probe-file (derived-regexp-file))))
    (and file (uiop:read-file-string file :external-format :utf-8))))

;;; ------------------------------------------------------------------
;;; Our reader's literals

(defvar *regexp-literals* (make-hash-table :test 'equal :synchronized t)
  "(kind bytes? source as a string) -> the layer's regexp.")

(defun literal-regexp (x)
  "X, a layer's regexp if it is a REGEXP-LITERAL (compiled once), else X."
  (if (regexp-literal-p x)
      (let* ((source (regexp-literal-source x))
	     (bytes (not (stringp source)))
	     (key (list (regexp-literal-kind x) bytes (if bytes (latin-1 source) source))))
	(or (gethash key *regexp-literals*)
	    (setf (gethash key *regexp-literals*)
		  (funcall (gethash (if (eq (regexp-literal-kind x) :px)
					(if bytes "byte-pregexp" "pregexp")
					(if bytes "byte-regexp" "regexp"))
				    *regexp-layer*)
			   source))))
      x))

(defun layer-regexp-literal (x)
  "If X is one of the layer's regexps, the REGEXP-LITERAL it prints as
(what a serialized form holds in its place: cache.lisp); else NIL."
  (when (and *regexp-instance* (struct-instance-p x)
	     (or (truthy (funcall (gethash "regexp?" *regexp-layer*) x))
		 (truthy (funcall (gethash "byte-regexp?" *regexp-layer*) x))))
    ;; the struct's fields: bytes?, px?, source, ...
    (let ((fields (struct-fields x)))
      (make-regexp-literal (if (truthy (elt fields 1)) :px :rx) (elt fields 2)))))

;;; ------------------------------------------------------------------
;;; Instantiating it

(defun replace-native-with-layer (name f)
  "Make F, the layer's NAME, the native NAME: its primitive and host global."
  (let ((wrapper (lambda (x &rest rest)
		   (declare (dynamic-extent rest))
		   (apply f (literal-regexp x) rest)))
	(native (gethash name *natives*)))
    (setf (gethash wrapper *arity-masks*) (or (ignore-errors (arity-mask f)) -1)
	  (gethash wrapper *renamed-procedures*) (rsym name))
    (when native
      (setf (native-function native) wrapper)
      (define-primitive (native-instance native) name wrapper))
    (psx:host-set! (concatenate 'string "racket:" name) wrapper)))

(defun install-regexp-layer (&key (stream nil))
  "Compile and instantiate the regexp layer, and make its exports the
natives of their names; true if that's done.  NIL, leaving the
CL-PPCRE natives, if the layer hasn't been extracted, or isn't wanted
\(PSEUDOSCHEME_RACKET_REGEXP=native), or doesn't instantiate."
  (flet ((seconds (start) (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (cond (*regexp-instance* t)
	  ((equal (uiop:getenv "PSEUDOSCHEME_RACKET_REGEXP") "native") nil)
	  (t
	   (let ((text (regexp-linklet-text)))
	     (when text
	       (let* ((start (get-internal-real-time))
		      (form (with-input-from-string (in text) (read-racket in)))
		      (compiled (compile-linklet-form form :name (rsym "regexp")))
		      (instance (make-linklet-instance (rsym "regexp"))))
		 (setf (compiled-linklet-source-key compiled) text)
		 (linklet-function compiled)
		 (setq *uncaught* nil)
		 (handler-case
		     (progn
		       (call-with-continuation-prompt
			(lambda () (with-racket-errors (instantiate-linklet compiled '() instance)))
			*default-prompt-tag* nil)
		       (when *uncaught*
			 (format stream "~&regexp: instantiating failed~%")
			 (return-from install-regexp-layer nil)))
		   (error (e)
		     (format stream "~&regexp: instantiating failed: ~A~%" e)
		     (return-from install-regexp-layer nil)))
		 (dolist (name *regexp-layer-exports*)
		   (setf (gethash name *regexp-layer*) (instance-value instance (rsym name))))
		 (dolist (name *regexp-layer-exports*)
		   (replace-native-with-layer name (gethash name *regexp-layer*)))
		 (setq *regexp-instance* instance)
		 (format stream "~&regexp: loaded in ~,1Fs~%" (seconds start))
		 t)))))))
