; -*- Mode: Lisp; Syntax: Common-Lisp; Package: QUASISCHEME-RACKET -*-

;;;; The three strategies (docs/racket.md, "Three strategies", decision 8),
;;;; each selectable with --racket=NAME.  A and C share rumble and
;;;; schemify; B and C share the native runtime.

(in-package "QUASISCHEME-RACKET")

(define-strategy "cs" "A: Racket CS's layers on --chez"
  :boot 'cs-boot :eval 'cs-eval :load 'cs-load)

(define-strategy "linklet" "B: our linklet compiler, on the native runtime"
  :boot 'linklet-boot :eval 'linklet-eval :load 'linklet-load)

(define-strategy "hybrid" "C: A, with rumble's hot procedures native"
  :boot 'hybrid-boot :eval 'cs-eval :load 'cs-load)
