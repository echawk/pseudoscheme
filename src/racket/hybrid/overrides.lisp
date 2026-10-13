; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy C: strategy A, with rumble's hot procedures replaced by the
;;;; native runtime's (src/racket/native/) after rumble loads
;;;; (docs/racket.md, "Three strategies").  schemify's output and
;;;; rumble's own code call rumble's procedures by name, so replacing a
;;;; definition reaches every caller.  Which ones: what profiling A says
;;;; (racket/base loading, bench/ as Racket modules); the first
;;;; candidates are struct operations, attachments and marks, wrapper
;;;; procedures, and hash tables.

(in-package "PSEUDOSCHEME-RACKET")

(defvar *overrides* '()
  "The rumble names C replaces, each with a native procedure (a NATIVE
with that :RUMBLE name).  Empty until A runs and is profiled.")

(defun install-overrides ()
  "Replace each of *OVERRIDES* in rumble, after rumble has loaded."
  (dolist (name *overrides*)
    (let ((native (loop for n being the hash-values of *natives*
			when (equal (native-rumble n) name) return n)))
      (unless native (error "--racket=hybrid: no native procedure for rumble's ~A" name))
      (not-yet (format nil "installing the override of rumble's ~A" name)))))

(defun hybrid-boot ()
  (cs-boot)
  (install-overrides))
