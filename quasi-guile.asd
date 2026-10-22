;;; -*- Mode: Lisp -*-
;;; (asdf:load-system :quasi-guile): Quasischeme's Guile (src/guile/, not
;;; a binding to GNU Guile's libguile) and its Lisp API, the package
;;; QUASI-GUILE (QG): EVAL, LOAD, PROCEDURE, USE-MODULES (Guile macros as
;;; Lisp macros), MODULE-EXPORTS; and, from Guile, the modules (cl
;;; PACKAGE) (src/guile/interop.lisp, docs/interop.md).  Guile's own
;;; boot-9 and modules are loaded from a Guile installation.

(defsystem :quasi-guile
  :maintainer "Ethan Hawk <ethhawk@iu.edu>"
  :version "1.0"
  :description "GNU Guile on Common Lisp (Quasischeme's --guile), with Lisp and Guile calling each other"
  :depends-on (:quasischeme/guile))
