; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; The native runtime: Racket's primitives written in Lisp, for SBCL.
;;;;
;;;; Two strategies use it:
;;;;   B  as rumble's replacement: each native procedure is a primitive
;;;;      of its instance (#%kernel unless said otherwise);
;;;;   C  as overrides of rumble: a native procedure that has a :RUMBLE
;;;;      name replaces rumble's procedure of that name after rumble
;;;;      loads (hybrid/overrides.lisp), so schemify's output and
;;;;      rumble's own callers get it.
;;;; So one definition serves both, and the two stay comparable.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (native (:constructor make-native (name function &key instance rumble)))
  name function (instance "#%kernel") rumble)

(defvar *natives* (make-hash-table :test 'equal) "Racket name -> NATIVE.")

(defmacro defnative (name lambda-list (&key (instance "#%kernel") rumble) &body body)
  "Define native procedure NAME (Racket's name) in INSTANCE.  RUMBLE is
the name rumble defines it under, if C may override it."
  `(setf (gethash ,name *natives*)
	 (make-native ,name (lambda ,lambda-list ,@body) :instance ,instance :rumble ,rumble)))

(defun install-natives ()
  "B: make each native procedure a primitive of its instance."
  (maphash (lambda (name n)
	     (define-primitive (native-instance n) name (native-function n)))
	   *natives*))
