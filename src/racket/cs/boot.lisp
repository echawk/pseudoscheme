; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Strategy A's start-up, our counterpart of Racket CS's main.sps
;;;; (docs/racket.md, "Run time"): initialize rumble, read the primitive
;;;; instances from it, instantiate io, thread and the expander, set the
;;;; collection paths and use-compiled-file-paths, and run.

(in-package "QUASISCHEME-RACKET")

(defun cs-boot ()
  (not-yet "strategy cs: boot (milestones 1 to 4)"))

(defun cs-eval (text)
  (declare (ignore text))
  (not-yet "strategy cs: eval (milestone 4)"))

(defun cs-load (path)
  (declare (ignore path))
  (not-yet "strategy cs: running a module (milestone 5)"))
