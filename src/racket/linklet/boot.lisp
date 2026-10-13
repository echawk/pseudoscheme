; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B's start-up: the native runtime as the primitive instances,
;;;; then the extracted thread, io, regexp and expander linklets, compiled
;;;; by compiler.lisp (docs/racket-linklet-compiler.md, "Milestones").

(in-package "PSEUDOSCHEME-RACKET")

(defun linklet-boot ()
  (install-natives)
  (install-linklet-primitives)
  (not-yet "strategy linklet: instantiating the extracted layers (milestones 2 to 6)"))

(defun linklet-eval (text)
  (declare (ignore text))
  (not-yet "strategy linklet: eval (milestone 6)"))

(defun linklet-load (path)
  (declare (ignore path))
  (not-yet "strategy linklet: running a module (milestone 7)"))
