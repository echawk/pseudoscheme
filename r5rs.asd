;;; -*- Mode: Lisp -*-
;;; Shorthand: (asdf:load-system :r5rs) loads Pseudoscheme's Lisp API,
;;; whose package R5RS has EVAL, LOAD, REPL, EXPAND, ... for R5RS
;;; Scheme (see src/api.lisp).  The same as :pseudoscheme/api.

(defsystem :r5rs
  :version "3.0"
  :description "R5RS Scheme in Common Lisp (Pseudoscheme): shorthand for pseudoscheme/api"
  :depends-on (:pseudoscheme/api))
