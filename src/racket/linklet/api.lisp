; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B: the #%linklet primitive instance, the interface between
;;;; Racket's expander and a backend (docs/racket.md, "Linklets"):
;;;; linklets, instances and variable objects, and the 29 functions.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (linklet-instance (:constructor make-linklet-instance (name &optional data)))
  name data
  (variables (make-hash-table :test 'eq)))	; symbol -> LINKLET-VARIABLE

(defstruct (linklet-variable (:constructor make-linklet-variable (name &optional (value nil))))
  name value
  (constant nil))			; set by instance-describe-variable! or analysis

(defparameter *linklet-primitives*
  '("primitive-table" "primitive->compiled-primitive" "primitive-in-category?"
    "primitive-lookup" "linklet?" "compile-linklet" "recompile-linklet" "eval-linklet"
    "instantiate-linklet" "linklet-import-variables" "linklet-export-variables"
    "linklet-directory?" "hash->linklet-directory" "linklet-directory->hash"
    "linklet-bundle?" "hash->linklet-bundle" "linklet-bundle->hash"
    "linklet-virtual-machine-bytes" "write-linklet-bundle-hash" "read-linklet-bundle-hash"
    "instance?" "make-instance" "instance-name" "instance-data" "instance-variable-names"
    "instance-variable-value" "instance-set-variable-value!" "instance-unset-variable!"
    "instance-describe-variable!" "linklet-performance-init!" "linklet-performance-report!"
    "variable-reference?" "variable-reference->instance" "variable-reference-constant?"
    "variable-reference-from-unsafe?" "correlated?" "correlated-source" "correlated-line"
    "correlated-column" "correlated-position" "correlated-span" "correlated-e"
    "correlated->datum" "datum->correlated" "correlated-property" "correlated-property-symbol-keys")
  "A first list of #%linklet's names, from Racket's reference (\"Linklets
and the Core Compiler\") and its correlated objects; milestone 1 replaces
it with (primitive-table '#%linklet) of the pinned release.")

(defun install-linklet-primitives ()
  "B: #%linklet, each name a stub until it's written."
  (dolist (name *linklet-primitives*)
    (let ((name name))
      (define-primitive "#%linklet" name
	(lambda (&rest args) (declare (ignore args)) (not-yet name))))))
