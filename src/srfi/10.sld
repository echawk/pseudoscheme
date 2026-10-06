;;; SRFI 10: #, external form.  Written for Pseudoscheme: the reader
;;; reads #,(tag datum ...) as the result of applying the constructor
;;; define-reader-ctor gave tag to the data (src/read.scm, src/core.lisp).
;;;
;;; R6RS gives #, a meaning too, (unsyntax datum), so #,(tag ...) is that
;;; when tag has no constructor.  And a constructor applies only to what
;;; is read after it is defined: a program or library is read whole
;;; before it runs, so its own define-reader-ctor applies to data read
;;; later (with read, or at the REPL), not to its own text.
(define-library (srfi 10)
  (export define-reader-ctor)
  (import (scheme base) (only (pseudoscheme lisp) lisp-eval-string))
  (begin
    (define %define-reader-ctor
      (lisp-eval-string "(lambda (tag procedure) (ps:define-reader-ctor tag procedure))"))
    (define (define-reader-ctor tag procedure)
      (unless (symbol? tag) (error "define-reader-ctor: not a symbol" tag))
      (unless (procedure? procedure) (error "define-reader-ctor: not a procedure" procedure))
      (%define-reader-ctor tag procedure))))
