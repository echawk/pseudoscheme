;;; -*- Mode: Scheme; Syntax: Scheme -*-
;;;; R6RS (rnrs base) and (rnrs syntax-case): the procedures that are
;;;; easiest in Scheme.  Loaded natively into the R6RS implementation
;;;; environment, after the R7RS bindings and the primitives of
;;;; rts.lisp.  The syntactic half is in macros.ss.

;;; (rnrs syntax-case) over the vendored Dybvig/Hieb expander.  Its
;;; IMPLICIT-IDENTIFIER (id sym) makes SYM an identifier with ID's
;;; lexical context, which is DATUM->SYNTAX one symbol at a time.

(define (datum->syntax template-id datum)
  (let loop ((d datum))
    (cond ((symbol? d) (implicit-identifier template-id d))
          ((pair? d) (cons (loop (car d)) (loop (cdr d))))
          ((vector? d) (list->vector (map loop (vector->list d))))
          (else d))))

(define syntax->datum syntax-object->datum)

;; The 1992 expander lets any macro be used as a bare identifier, so a
;; "variable transformer" is just an ordinary transformer here; the
;; SET! half of R6RS's identifier macros is not supported.
(define (make-variable-transformer proc) proc)

;;; Strings, vectors: R6RS (rnrs base) has the same single-argument
;;; forms R7RS generalized, so the R7RS definitions carry over.  Where
;;; R6RS differs from R7RS it is in the *absence* of things (no
;;; SET-CAR!, no LIST-COPY, ...), which the export tables handle.
