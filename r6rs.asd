;;; -*- Mode: Lisp -*-
;;; Shorthand: (asdf:load-system :r6rs) loads Pseudoscheme's Lisp API,
;;; whose package R6RS has EVAL, LOAD, REPL, EXPAND, ... for R6RS
;;; Scheme (see src/api.lisp).  The same as :pseudoscheme/api.

(defsystem :r6rs
  :version "3.0"
  :description "R6RS Scheme in Common Lisp (Pseudoscheme): shorthand for pseudoscheme/api"
  :depends-on (:pseudoscheme/api))
