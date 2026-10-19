;;; -*- Mode: Lisp -*-
;;; Shorthand: (asdf:load-system :r7rs) loads Quasischeme's Lisp API,
;;; whose package R7RS has EVAL, LOAD, REPL, EXPAND, ... for R7RS
;;; Scheme (see src/api.lisp).  The same as :quasischeme/api.

(defsystem :r7rs
  :version "1.0"
  :description "R7RS Scheme in Common Lisp (Quasischeme): shorthand for quasischeme/api"
  :depends-on (:quasischeme/api))
