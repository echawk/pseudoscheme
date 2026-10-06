;;; SRFI 206: auxiliary syntax keywords.  Written for Pseudoscheme, and
;;; partial: the SRFI "cannot be implemented portably", and psyntax can't
;;; give two separately defined keywords the same binding (free-
;;; identifier=? compares their labels, and a definition always makes a
;;; new one).  So
;;;
;;;   (define-auxiliary-syntax keyword [symbol])
;;;
;;; binds KEYWORD to new auxiliary syntax (a syntax error when used as
;;; an expression), which matches itself as a literal, but not other
;;; auxiliary syntax of the same name.  auxiliary-syntax-name is bound
;;; (to auxiliary syntax), but Pseudoscheme has no SRFI 213 identifier
;;; properties to look it up with.
;;;
;;; (srfi 206 all) is the SRFI's portable "poor man's" library (see
;;; 206/all.sld): a fixed list of names with shared bindings.  The
;;; sample's (srfi 206) only raises an error (reference/srfi-206/).
(define-library (srfi 206)
  (export define-auxiliary-syntax auxiliary-syntax-name)
  (import (scheme base))
  (begin
    (define-syntax define-auxiliary-syntax
      (syntax-rules ()
        ((_ keyword) (define-auxiliary-syntax keyword keyword))
        ((_ keyword symbol)
         (define-syntax keyword
           (syntax-rules ()
             ((_ . args) (syntax-error "invalid use of auxiliary syntax" symbol)))))))
    (define-auxiliary-syntax auxiliary-syntax-name)))
