; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; Strategy A (and C): Racket CS's layers, compiled by --chez in Racket
;;;; CS's order (docs/racket.md, "Build time").  Each is compiled as a
;;;; whole library (compile-whole-library, Chez mode) when the image is
;;;; built, so start-up only instantiates.

(in-package "QUASISCHEME-RACKET")

(defparameter *cs-layers*
  '(("rumble" "racket/src/cs/rumble.sls")
    ("thread" "racket/src/cs/thread.sls")
    ("io" "racket/src/cs/io.sls")
    ("regexp" "racket/src/cs/regexp.sls")
    ("schemify" "racket/src/cs/schemify.sls")
    ("linklet" "racket/src/cs/linklet.sls")
    ("expander" "racket/src/cs/expander.sls"))
  "Racket CS's layers and their libraries in Racket's source tree, in
build order.  The .sls files include the schemified layers, which the
release tree has checked in (racket/src/cs/schemified/), and rumble.sls
uses chezpart.sls.")

(defun compile-cs-layers ()
  "Compile *CS-LAYERS* on --chez, with fork.scm loaded first and the
linklet layer's compile step replaced by HOOK.LISP's."
  (not-yet "compile-cs-layers (milestone 1: rumble loads)"))
