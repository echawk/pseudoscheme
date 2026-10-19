;;; -*- Mode: Lisp -*-
;;; Shorthand: (asdf:load-system :r5rs) loads Quasischeme's Lisp API,
;;; whose package R5RS has EVAL, LOAD, REPL, EXPAND, ... for R5RS
;;; Scheme (see src/api.lisp).  The same as :quasischeme/api.

(defsystem :r5rs
  :version "1.0"
  :description "R5RS Scheme in Common Lisp (Quasischeme): shorthand for quasischeme/api"
  :depends-on (:quasischeme/api))
