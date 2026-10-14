; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; --racket: what the three strategies share (docs/racket.md).
;;;;
;;;;   A  cs        Racket CS's layers on --chez         src/racket/cs/
;;;;   B  linklet   our linklet compiler, native runtime  src/racket/linklet/, native/
;;;;   C  hybrid    A, with rumble's hot procedures       src/racket/hybrid/
;;;;                replaced by native/'s
;;;;
;;;; Each strategy is a STRATEGY, selected by --racket=NAME or
;;;; PSEUDOSCHEME_RACKET_STRATEGY (decision 8).  This file has the
;;;; registry, where Racket's sources and collections are, and the entry
;;;; points the command line calls.  Status: skeleton; nothing runs yet.

(defpackage "PSEUDOSCHEME-RACKET"
  (:nicknames "PSR")
  (:use "COMMON-LISP")
  (:export "*STRATEGY*" "SELECT-STRATEGY" "STRATEGY-NAMES"
	   "EVAL-STRING" "LOAD-FILE" "NOT-YET"))

(in-package "PSEUDOSCHEME-RACKET")

;;; ------------------------------------------------------------------
;;; Strategies

(defstruct (strategy (:constructor make-strategy (name description &key boot eval load)))
  name description
  boot					; () -> the instances, ready to run
  eval					; (string) -> the last form's values
  load)					; (path) -> runs a module or program

(defvar *strategies* '() "Name -> STRATEGY, in definition order.")
(defvar *strategy* nil "The selected STRATEGY.")
(defvar *booted* nil "The strategy that has booted, if any.")

(defmacro define-strategy (name description &rest functions)
  `(setf *strategies*
	 (append (remove ,name *strategies* :key #'car :test #'string=)
		 (list (cons ,name (make-strategy ,name ,description ,@functions))))))

(defun strategy-names () (mapcar #'car *strategies*))

(define-condition not-yet (error)
  ((what :initarg :what :reader not-yet-what))
  (:report (lambda (c stream)
	     (format stream "--racket: not implemented yet: ~A (docs/racket.md, \"Milestones\")"
		     (not-yet-what c)))))

(defun not-yet (what) (error 'not-yet :what what))

(defun select-strategy (&optional name)
  "The strategy NAME names (else PSEUDOSCHEME_RACKET_STRATEGY's, else
cs), made current."
  (let* ((name (or name (uiop:getenv "PSEUDOSCHEME_RACKET_STRATEGY") "cs"))
	 (s (cdr (assoc name *strategies* :test #'string=))))
    (unless s
      (error "--racket=~A: the strategies are ~{~A~^, ~}" name (strategy-names)))
    (setq *strategy* s)))

(defun boot ()
  (unless *strategy* (select-strategy))
  (unless (eq *booted* *strategy*)
    (funcall (strategy-boot *strategy*))
    (setq *booted* *strategy*)))

(defun eval-string (text)
  (boot)
  (funcall (strategy-eval *strategy*) text))

(defun load-file (path)
  (boot)
  (funcall (strategy-load *strategy*) path))

;;; ------------------------------------------------------------------
;;; Values and errors every strategy's Lisp side uses

(defun rsym (name) "The Racket symbol named NAME." (ps:intern-scheme-symbol name))
(defun rname (symbol) "SYMBOL's Racket name." (ps:scheme-symbol-name symbol))
(defun bool (x) (if x ps:true ps:false))
(defun truthy (x) (not (eq x ps:false)))

(defvar *void* ps:unspecific "Racket's #<void>.")

(define-condition racket-error (error)
  ((who :initarg :who :reader racket-error-who)
   (message :initarg :message :reader racket-error-message))
  (:report (lambda (c stream)
	     (format stream "~A: ~A" (racket-error-who c) (racket-error-message c)))))

(defun racket-error (who format &rest args)
  "Raise Racket's exn:fail (exn.lisp makes it a Racket exception once
exceptions are written; a Lisp condition until then)."
  (error 'racket-error :who who :message (apply #'format nil format args)))

;;; ------------------------------------------------------------------
;;; Racket's sources and collections

(defun racket-source-directory ()
  "Racket's source tree: build/external/racket/ as utils/racket/setup.sh
fetches it, or PSEUDOSCHEME_RACKET_SOURCE."
  (let ((dir (or (uiop:getenv "PSEUDOSCHEME_RACKET_SOURCE")
		 (namestring (asdf:system-relative-pathname :pseudoscheme "build/external/racket/")))))
    (or (probe-file (uiop:ensure-directory-pathname dir))
	(error "--racket: Racket's source isn't at ~A: run sh utils/racket/setup.sh" dir))))

(defun racket-source (relative)
  "A file of Racket's source tree, e.g. \"racket/src/cs/rumble.sls\"."
  (merge-pathnames relative (racket-source-directory)))

(defun collects-directories ()
  "Where racket/base and the rest are: PLTCOLLECTS, else what the
installed racket executable says (decision 3); never a search."
  (let ((env (uiop:getenv "PLTCOLLECTS")))
    (if (and env (plusp (length env)))
	(remove "" (uiop:split-string env :separator ":") :test #'string=)
	(let ((dir (ignore-errors
		    (uiop:run-program '("racket" "-e" "(display (find-system-path (quote collects-dir)))")
				      :output :string))))
	  (if (and dir (plusp (length dir)))
	      (list dir)
	      (error "--racket: no collections: set PLTCOLLECTS or install racket"))))))

;;; Compiled code goes in compiled/pseudoscheme/ beside Racket's own
;;; compiled/ directories (cache.lisp), never in them.
(defparameter *compiled-subdirectory* "compiled/pseudoscheme")
