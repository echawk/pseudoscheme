; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Racket's structs, natively (docs/racket-linklet-compiler.md, "The
;;;; runtime it calls"): make-struct-type with properties, guards,
;;;; inspectors, prefab keys and prop:procedure.  The plan: a struct type
;;;; is an SBCL structure class of its own (one object per instance,
;;;; typed slots: docs/optimizations.md, idea 2); an applicable one is a
;;;; funcallable instance, as Guile mode's applicable structs are.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (struct-type (:constructor %make-struct-type))
  name parent field-count auto-field-count auto-value
  properties				; alist: property -> value
  inspector immutables guard constructor-name
  prefab-key
  (class nil))				; the SBCL class its instances are

(defstruct (struct-type-property (:constructor %make-struct-type-property (name guard supers)))
  name guard supers)

(defnative "make-struct-type" (name parent-type init-field-count auto-field-count
				    &optional auto-v props inspector proc-spec immutables guard constructor-name)
    (:rumble "make-struct-type")
  (declare (ignore name parent-type init-field-count auto-field-count auto-v props inspector
		   proc-spec immutables guard constructor-name))
  (not-yet "make-struct-type"))

(defnative "make-struct-type-property" (name &optional guard supers can-impersonate?)
    (:rumble "make-struct-type-property")
  (declare (ignore name guard supers can-impersonate?))
  (not-yet "make-struct-type-property"))

(defnative "make-struct-field-accessor" (accessor-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-accessor")
  (declare (ignore accessor-proc field-pos field-name contract realm))
  (not-yet "make-struct-field-accessor"))

(defnative "make-struct-field-mutator" (mutator-proc field-pos &optional field-name contract realm)
    (:rumble "make-struct-field-mutator")
  (declare (ignore mutator-proc field-pos field-name contract realm))
  (not-yet "make-struct-field-mutator"))
