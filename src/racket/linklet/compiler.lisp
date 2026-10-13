; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; Strategy B: linklets to core Scheme, then the translator
;;;; (docs/racket-linklet-compiler.md).  Modelled on the Tree-IL compiler
;;;; (src/guile/compile.lisp).
;;;;
;;;;   1 DECORRELATE  correlated objects off; names and locations kept aside
;;;;   2 RENAME       lexicals renamed apart
;;;;   3 ANALYSE      per variable: defined, imported, exported, mutated,
;;;;                  used before definition, known value (procedure and
;;;;                  arity, constant, struct-type piece)
;;;;   4 LOWER        core Scheme, form by form (the document's table)
;;;;   5 translate-core, the continuation transformation, SBCL
;;;;
;;;; Passes 1 to 4 are here; each is a stub until milestone 1.

(in-package "PSEUDOSCHEME-RACKET")

(defstruct (linklet-info (:constructor make-linklet-info ()))
  (locations (make-hash-table :test 'eq))	; form -> source location (pass 1)
  (names (make-hash-table :test 'eq))		; lambda -> inferred name (pass 1)
  (variables (make-hash-table :test 'eq)))	; variable -> VARIABLE-INFO (pass 3)

(defstruct (variable-info (:constructor make-variable-info (name)))
  name
  (kind :local)				; :local :defined :imported :exported
  (mutated nil)				; set! anywhere
  (early-use nil)			; referenced before its definition runs
  (known nil))				; (:procedure arity) (:constant v) (:struct-accessor type i) ...

(defun decorrelate (form info)
  "Pass 1: FORM without correlated objects; their names and locations go
into INFO."
  (declare (ignore form info))
  (not-yet "linklet pass 1: decorrelate"))

(defun rename-apart (form info)
  "Pass 2."
  (declare (ignore form info))
  (not-yet "linklet pass 2: rename"))

(defun analyse (form info imports-known)
  "Pass 3.  IMPORTS-KNOWN is what get-import told us about the imported
linklets' variables (cross-linklet knowledge)."
  (declare (ignore form info imports-known))
  (not-yet "linklet pass 3: analyse"))

(defun lower (form info)
  "Pass 4: core Scheme for psyntax.lisp's TRANSLATE-CORE."
  (declare (ignore form info))
  (not-yet "linklet pass 4: lower"))

(defun compile-linklet-form (form &key name imports-known options)
  "A linklet S-expression to a COMPILED-LINKLET (cache.lisp).  OPTIONS
are compile-linklet's: serializable unsafe static use-prompt
uninterned-literal quick."
  (declare (ignore options))
  (let ((info (make-linklet-info)))
    (make-compiled-linklet
     (lower (analyse (rename-apart (decorrelate form info) info) info imports-known) info)
     :name name :strategy "linklet")))
