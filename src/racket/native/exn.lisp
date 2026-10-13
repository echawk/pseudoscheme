; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Errors, natively: the exn struct hierarchy (exn, exn:fail,
;;;; exn:fail:contract, ...: struct types of structs.lisp), raise with
;;;; barriers, error-display-handler, and the messages Racket's
;;;; primitives raise ("application: not a procedure", "arity mismatch",
;;;; ...), which the linklet compiler's checks use too.

(in-package "PSEUDOSCHEME-RACKET")

(defnative "raise" (v &optional barrier?) (:rumble "raise")
  (declare (ignore v barrier?))
  (not-yet "raise"))

(defun raise-argument-error (who expected value)
  "The exn:fail:contract Racket's primitives raise for a bad argument."
  (declare (ignore who expected value))
  (not-yet "raise-argument-error"))
