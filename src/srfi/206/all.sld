;;; SRFI 206's (srfi 206 all), the SRFI's "poor man's" implementation
;;; (Marc Nieper-Wißkirchen's; MIT licence, in reference/srfi-206/):
;;; not the magic library that imports any name, but a fixed list,
;;; all-exports.scm.  Its exports are the same bindings wherever they
;;; also come from: else, =>, unquote, unquote-splicing, _ and ... are
;;; (scheme base)'s, unsyntax and unsyntax-splicing (rnrs syntax-case)'s,
;;; <> and <...> SRFI 26's.  The others (SRFI 190's yield and SRFI 204's
;;; $ ? get! *** ___ **1 =.. *.. struct object) are defined here, as in
;;; the sample's all-definitions.scm, as macros that are an error to use
;;; (Pseudoscheme has no SRFI 139 syntax parameters, so they aren't
;;; syntax parameters).  all-exports.scm is included unmodified.
(define-library (srfi 206 all)
  (include-library-declarations "../reference/srfi-206/all-exports.scm")
  (import (scheme base)
          (only (rnrs syntax-case) unsyntax unsyntax-splicing)
          (only (srfi 26) <> <...>))
  (begin
    (define-syntax define-auxiliary-syntax
      (syntax-rules ()
        ((_ name)
         (define-syntax name
           (syntax-rules ()
             ((_ . _) (syntax-error "invalid use of auxiliary syntax" name)))))))
    (define-auxiliary-syntax yield)
    (define-auxiliary-syntax $)
    (define-auxiliary-syntax ?)
    (define-auxiliary-syntax get!)
    (define-auxiliary-syntax ***)
    (define-auxiliary-syntax ___)
    (define-auxiliary-syntax **1)
    (define-auxiliary-syntax =..)
    (define-auxiliary-syntax *..)
    (define-auxiliary-syntax struct)
    (define-auxiliary-syntax object)))
