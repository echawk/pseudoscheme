;;; -*- Mode: Lisp -*-
;;; Shorthand: (asdf:load-system :r7rs) loads Pseudoscheme's Lisp API,
;;; whose package R7RS has EVAL, LOAD, REPL, EXPAND, ... for R7RS
;;; Scheme (see src/api.lisp).  The same as :pseudoscheme/api.

(defsystem :r7rs
  :description "R7RS Scheme in Common Lisp (Pseudoscheme): shorthand for pseudoscheme/api"
  :depends-on (:pseudoscheme/api))
