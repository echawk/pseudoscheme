; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategies A and C: the one hook we replace in Racket CS's linklet
;;;; layer, the step that turns a schemified linklet into code
;;;; (docs/racket.md, decision 4):
;;;;   stage A  the schemified form as Chez source: psyntax, the
;;;;            translator, SBCL (what contrib/racket-linklets did);
;;;;   stage B  the schemified form straight to the translator's core
;;;;            forms (translate-core), skipping psyntax.
;;;; PSEUDOSCHEME_RACKET_STAGE=a|b chooses; b once it works.

(in-package "PSEUDOSCHEME-RACKET")

(defun hook-stage ()
  (if (equal (uiop:getenv "PSEUDOSCHEME_RACKET_STAGE") "a") :a :b))

(defun compile-schemified (form)
  "FORM, a schemified linklet (a lambda over its imports and exports), as
a Lisp function."
  (declare (ignore form))
  (ecase (hook-stage)
    (:a (not-yet "compile-schemified, stage A"))
    (:b (not-yet "compile-schemified, stage B"))))
