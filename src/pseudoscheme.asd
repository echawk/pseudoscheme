;;; -*- Mode: Lisp -*-

;;;; Pseudoscheme
;;;; System Definitions

(cl:defpackage :pseudoscheme-system
  (:use :common-lisp :asdf))

(cl:in-package :pseudoscheme-system)

(defclass pso-file (cl-source-file) ())

(defmethod source-file-type ((component pso-file) system)
  (declare (ignore component system))
  "pso")

;;; Pseudoscheme is split into a core run-time (rts), the self-hosted
;;; Scheme-to-Common-Lisp translator (translator), and an evaluator/REPL
;;; layer built on both.  Language-standard surfaces (r5rs, r6rs, r7rs)
;;; are secondary systems built on top of this shared core; see
;;; :pseudoscheme/r5rs below.

(defsystem :pseudoscheme/rts
  :author "Jonathan Rees"
  :components
    (
     (:file "pack")
     (:file "spack" :depends-on ("pack"))
     (:file "core" :depends-on ("pack"))
     (pso-file "closed" :depends-on ("spack" "core"))
     (:file "rts" :depends-on ("pack" "spack" "core"))
     (:file "readwrite" :depends-on ("pack" "core"))
     #+(or) (pso-file "read" :depends-on ("spack" "core"))
     #+(or) (pso-file "write" :depends-on ("spack" "core"))
     ))

(defsystem :pseudoscheme/translator
  :author "Jonathan Rees"
  :depends-on (:pseudoscheme/rts)
  :serial t                        ;[No time to figure out the deps... --TRC]
  :components
  (
   (pso-file "p-record")                ; record package
   (pso-file "p-utils")                 ; tables and fluids
   (pso-file "list")                    ; list utilities
   (pso-file "classes")                 ; expression classes
   (pso-file "form")                    ; expression stuff used by classifier
   (pso-file "classify")                ; expression classifier
   (pso-file "node")                    ; budding node abstraction
   (pso-file "module")                  ; signatures and structures
   (pso-file "ssig")                    ; Scheme signature
   (pso-file "alpha")                   ; front end
   (pso-file "rules")                   ; the (syntax-rules ...) macro
   (pso-file "derive")                  ; derived expression types
   (pso-file "strategy")                ; LETREC strategy anaylzer
   (pso-file "version")
   (pso-file "schemify")                ; degenerate back end

   ;; Common Lisp back end
   (pso-file "emit")                    ; code emission utilities
   (pso-file "generate")                ; CL code generator
   (pso-file "builtin")                 ; CL info about scheme built-ins
   (pso-file "translate")               ; phase coordination and file transducer
   (pso-file "reify")                   ; miscellaneous
   ))

(defsystem :pseudoscheme/evaluator
  :author "Jonathan Rees"
  :depends-on (:pseudoscheme/rts :pseudoscheme/translator)
  :components ((:file "eval")))

(defsystem :pseudoscheme
  :author "Jonathan Rees"
  :depends-on
    (:pseudoscheme/rts :pseudoscheme/translator :pseudoscheme/evaluator))

;;; The R5RS-conformant surface. REVISED^4-SCHEME-INTERFACE (ssig.scm)
;;; already includes R5RS's additions over R4RS (VALUES, DYNAMIC-WIND,
;;; EVAL, string ports, ...) in the one shared runtime structure, so
;;; this system is currently just the core stack under its R5RS name;
;;; tests/run-r5rs-tests.lisp (chibi's R5RS suite) is what actually
;;; gates this system's R5RS claim -- see its header comment and the
;;; project README for known gaps (the CL-reader-based `...`/string-escape
;;; limitation, symbol/number print case).
(defsystem :pseudoscheme/r5rs
  :author "Jonathan Rees"
  :depends-on (:pseudoscheme/rts :pseudoscheme/translator :pseudoscheme/evaluator))

;;; Regenerates translator.files' .pso bootstrap artifacts from their
;;; .scm sources using the already-loaded translator. See bootstrap.lisp.
(defsystem :pseudoscheme/bootstrap
  :author "Jonathan Rees"
  :depends-on (:pseudoscheme)
  :components ((:file "bootstrap")))
