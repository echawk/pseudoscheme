; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Compiled linklets, for all three strategies (docs/racket.md,
;;;; "Compiled code"):
;;;;   - what a .zo holds: each linklet's S-expression after lowering
;;;;     (A, C: schemify's output; B: after our passes 1 to 3), which is
;;;;     machine-independent, so Racket's compilation manager and raco
;;;;     make work unchanged;
;;;;   - the SBCL fasl: in the library cache (src/library-cache.lisp),
;;;;     keyed by a hash of that S-expression, the build signature, the
;;;;     continuations mode and the strategy.
;;;; Loading a linklet finds its fasl, or else translates and compiles it.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (compiled-linklet (:constructor make-compiled-linklet (form &key name strategy)))
  form					; the lowered S-expression
  name
  strategy				; "cs" / "linklet" / "hybrid"
  (function nil))			; once loaded: the instantiation function

(defun linklet-key (compiled)
  "The library cache's key for COMPILED."
  (declare (ignore compiled))
  (not-yet "linklet-key: hash of the form, build signature, continuations mode, strategy"))

(defun load-compiled-linklet (compiled)
  "COMPILED's instantiation function, from the fasl cache or compiled now."
  (or (compiled-linklet-function compiled)
      (not-yet "load-compiled-linklet: fasl cache lookup, else translate-core and compile")))

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
