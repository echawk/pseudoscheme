;;; SRFI 229: tagged procedures.  Written for Pseudoscheme, on
;;; closer-mop (loaded from Quicklisp or ASDF the first time this
;;; library is imported).
;;;
;;; A tagged procedure is a funcallable instance: a CLOS object, with
;;; the tag in a slot, that is also a Lisp function.  So it is an
;;; ordinary procedure to Scheme, and an ordinary function to Lisp
;;; (FUNCALL, MAPCAR, FUNCTIONP).  A weak table from procedure to tag
;;; would not do: a lambda with no free variables can compile to one
;;; constant function object, so two tagged procedures made from the
;;; same lambda/tag would share their tag.
;;;
;;; The class and the three primitives are Lisp, in a package of their
;;; own, PSEUDOSCHEME-SRFI-229; they are made when the library is loaded.
(define-library (srfi 229)
  (export lambda/tag case-lambda/tag procedure/tag? procedure-tag)
  (import (scheme base) (scheme case-lambda)
          (only (pseudoscheme lisp) lisp-eval-string)
          ;; Loads closer-mop.
          (only (prefix (cl closer-mop) c2mop:) c2mop:funcallable-standard-class))
  (begin
    (lisp-eval-string
     "(defpackage \"PSEUDOSCHEME-SRFI-229\" (:use \"COMMON-LISP\"))")
    (lisp-eval-string
     "(defclass pseudoscheme-srfi-229::tagged-procedure ()
        ((pseudoscheme-srfi-229::tag :initarg :tag))
        (:metaclass closer-mop:funcallable-standard-class))")

    ;; Raw Lisp functions: Scheme calls them with no boolean conversion,
    ;; so a tag may be #f, and the procedure keeps its own results.
    (define %make
      (lisp-eval-string
       "(lambda (tag procedure)
          (let ((f (make-instance 'pseudoscheme-srfi-229::tagged-procedure :tag tag)))
            (closer-mop:set-funcallable-instance-function f procedure)
            f))"))
    (define %tagged?
      (lisp-eval-string
       "(lambda (x)
          (if (typep x 'pseudoscheme-srfi-229::tagged-procedure) t ps:false))"))
    (define %tag
      (lisp-eval-string
       "(lambda (x) (slot-value x 'pseudoscheme-srfi-229::tag))"))

    (define-syntax lambda/tag
      (syntax-rules ()
        ((_ tag formals body1 body2 ...)
         (%make tag (lambda formals body1 body2 ...)))))

    (define-syntax case-lambda/tag
      (syntax-rules ()
        ((_ tag clause ...)
         (%make tag (case-lambda clause ...)))))

    (define (procedure/tag? obj) (%tagged? obj))

    (define (procedure-tag proc)
      (if (%tagged? proc)
          (%tag proc)
          (error "procedure-tag: not a tagged procedure" proc)))))
