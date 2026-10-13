; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-RACKET -*-

;;;; rktio: Racket's portable operating-system layer (files, sockets,
;;;; processes, polling), in C, which the io layer calls through its
;;;; rktio_* primitives.  The plan (docs/racket-linklet-compiler.md):
;;;; build it from Racket's source (racket/src/rktio/) as a shared
;;;; library, and bind it with CFFI, so that io's behaviour is exactly
;;;; Racket's.  A and C get rktio through rumble; B through here.

(in-package "PSEUDOSCHEME-RACKET")

(defun rktio-library ()
  "The built librktio, under build/external/ (utils/racket/setup.sh's tree)."
  (not-yet "rktio-library: build racket/src/rktio as a shared library"))

(defun rktio-init ()
  (not-yet "rktio_init"))
