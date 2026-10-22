;;; -*- Mode: Lisp -*-
;;; (asdf:load-system :quasi-racket): Quasischeme's Racket (src/racket/,
;;; Racket's own expander and collections on our linklet compiler, not a
;;; binding to Racket CS) and its Lisp API, the package QUASI-RACKET (QR):
;;; EVAL, REQUIRE (Racket macros as Lisp macros), PROCEDURE,
;;; MODULE-EXPORTS; and, from Racket, the collection cl, whose modules
;;; cl/PACKAGE are Lisp packages (src/racket/linklet/interop.lisp,
;;; docs/interop.md).  Racket's source tree comes from utils/racket/setup.sh.

(defsystem :quasi-racket
  :maintainer "Ethan Hawk <ethhawk@iu.edu>"
  :version "1.0"
  :description "Racket on Common Lisp (Quasischeme's --racket), with Lisp and Racket calling each other"
  :depends-on (:quasischeme/racket))
