; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Hash tables, natively: mutable equal/eqv/eq and weak tables on
;;;; SBCL's, and Racket's immutable tables as a persistent map (a HAMT,
;;;; as Racket CS's are), with equal-hash and the iteration protocol
;;;; (hash-iterate-first and friends) the expander uses heavily.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (immutable-hash (:constructor %make-immutable-hash (kind root count)))
  kind					; :equal, :eqv, :eq
  root count)

(defnative "hash" (&rest keys-and-values) (:rumble "hash")
  (declare (ignore keys-and-values))
  (not-yet "hash"))

(defnative "hash-set" (table key value) (:rumble "hash-set")
  (declare (ignore table key value))
  (not-yet "hash-set"))

(defnative "hash-ref" (table key &optional failure) (:rumble "hash-ref")
  (declare (ignore table key failure))
  (not-yet "hash-ref"))

(defnative "hash-iterate-first" (table) (:rumble "hash-iterate-first")
  (declare (ignore table))
  (not-yet "hash-iterate-first"))
