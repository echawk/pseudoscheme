; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Procedures, natively: arity masks, procedure-reduce-arity, keyword
;;;; procedures' plumbing, and impersonators and chaperones of
;;;; procedures, structs, vectors, boxes and hash tables.  The plan:
;;;; SBCL funcallable instances, where rumble uses the Chez fork's
;;;; wrapper procedures.

(in-package "PSEUDOSCHEME-RACKET")

(defnative "procedure-arity-mask" (proc) (:rumble "procedure-arity-mask")
  (declare (ignore proc))
  (not-yet "procedure-arity-mask"))

(defnative "procedure-reduce-arity" (proc arity &optional name realm) (:rumble "procedure-reduce-arity")
  (declare (ignore proc arity name realm))
  (not-yet "procedure-reduce-arity"))

(defnative "impersonate-procedure" (proc wrapper &rest props) (:rumble "impersonate-procedure")
  (declare (ignore proc wrapper props))
  (not-yet "impersonate-procedure"))

(defnative "chaperone-procedure" (proc wrapper &rest props) (:rumble "chaperone-procedure")
  (declare (ignore proc wrapper props))
  (not-yet "chaperone-procedure"))
