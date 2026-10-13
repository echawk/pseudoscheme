; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Primitive instances: #%kernel, #%unsafe, #%flfxnum, #%paramz,
;;;; #%linklet, ... (docs/racket.md, "Linklets").  Linklets refer to
;;;; primitives by name; an instance is a table of them.
;;;;
;;;; Who fills them depends on the strategy:
;;;;   A, C  rumble and the layers above it, as in Racket CS: BOOT reads
;;;;         them from rumble's tables after it loads;
;;;;   B     native/ (DEFNATIVE) and the extracted layers' exports.
;;;; Linklets, variables and instances themselves (instance objects,
;;;; variable objects) are in linklet/api.lisp for B, and rumble's for A
;;;; and C.

(in-package "PSEUDOSCHEME-RACKET")

(defparameter *instance-names*
  '("#%kernel" "#%unsafe" "#%flfxnum" "#%extfl" "#%paramz" "#%foreign" "#%network"
    "#%place" "#%futures" "#%terminal" "#%linklet")
  "The primitive instances, in Racket 9.3 (docs/racket.md has their sizes).")

(defvar *primitive-instances* (make-hash-table :test 'equal)
  "Instance name -> (hash table: primitive name -> value).")

(defun primitive-instance (name)
  (or (gethash name *primitive-instances*)
      (setf (gethash name *primitive-instances*) (make-hash-table :test 'equal))))

(defun define-primitive (instance name value)
  (setf (gethash name (primitive-instance instance)) value))

(defun primitive (name &optional (instances *instance-names*))
  "NAME's value in the first of INSTANCES that has it, and T; else NIL, NIL."
  (dolist (i instances (values nil nil))
    (multiple-value-bind (v found) (gethash name (primitive-instance i))
      (when found (return (values v t))))))

(defun missing-primitives (instance names)
  "Which of NAMES INSTANCE doesn't have: the coverage report, against
Racket's own (primitive-table ...) lists."
  (remove-if (lambda (n) (nth-value 1 (gethash n (primitive-instance instance)))) names))
