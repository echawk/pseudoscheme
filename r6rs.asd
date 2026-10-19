;;; -*- Mode: Lisp -*-
;;; Shorthand: (asdf:load-system :r6rs) loads Quasischeme's Lisp API,
;;; whose package R6RS has EVAL, LOAD, REPL, EXPAND, ... for R6RS
;;; Scheme (see src/api.lisp).  The same as :quasischeme/api.

(defsystem :r6rs
  :version "1.0"
  :description "R6RS Scheme in Common Lisp (Quasischeme): shorthand for quasischeme/api"
  :depends-on (:quasischeme/api))
