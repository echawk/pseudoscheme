; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Procedures, natively: arity masks, procedure-reduce-arity, keyword
;;;; procedures' plumbing, and impersonators and chaperones of
;;;; procedures, structs, vectors, boxes and hash tables.  The plan:
;;;; SBCL funcallable instances, where rumble uses the Chez fork's
;;;; wrapper procedures.

(in-package "PSEUDOSCHEME-RACKET")

(defnative "procedure-reduce-arity" (proc arity &optional name realm) (:rumble "procedure-reduce-arity")
  (declare (ignore proc arity name realm))
  (not-yet "procedure-reduce-arity"))

(defnative "impersonate-procedure" (proc wrapper &rest props) (:rumble "impersonate-procedure")
  (declare (ignore proc wrapper props))
  (not-yet "impersonate-procedure"))

(defnative "chaperone-procedure" (proc wrapper &rest props) (:rumble "chaperone-procedure")
  (declare (ignore proc wrapper props))
  (not-yet "chaperone-procedure"))

;;; There are no impersonators or chaperones yet: nothing is one
(defnative "impersonator?" (x) (:leaf t) (declare (ignore x)) ps:false)
(defnative "chaperone?" (x) (:leaf t) (declare (ignore x)) ps:false)
(defnative "impersonator-of?" (a b) () (bool (equal* a b)))
(defnative "chaperone-of?" (a b) () (bool (equal* a b)))

