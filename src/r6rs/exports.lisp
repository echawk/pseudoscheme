; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PSEUDOSCHEME-R6RS -*-

;;;; R6RS standard libraries and their exports
;;;;
;;;; r6rs.pdf is the *language* report, which defines (rnrs base (6))
;;;; in chapter 11, the library and top-level-program forms in chapters
;;;; 7-8, and the expansion process in chapter 10.  The other standard
;;;; libraries -- (rnrs lists), (rnrs io simple), (rnrs records ...),
;;;; (rnrs conditions), (rnrs bytevectors), (rnrs hashtables), ... --
;;;; are specified in the separate "Revised^6 Report on Scheme:
;;;; Standard Libraries" document, which isn't in this repository, so
;;;; only the ones the language report itself pins down are tabulated
;;;; here; *PLANNED-LIBRARIES* lists the rest by name for the roadmap.
;;;;
;;;; (rnrs base) is cross-checked against the PDF by
;;;; tests/check-r6rs-exports.py.

(defpackage "PSEUDOSCHEME-R6RS"
  (:nicknames "R6RS")
  (:use "COMMON-LISP")
  (:export "*STANDARD-LIBRARIES*" "*PARTIAL-LIBRARIES*" "*PLANNED-LIBRARIES*" "INSTALL-R6RS"
	   "EVALUATE-LIBRARY-FORM" "R6RS-EVAL" "LOAD-R6RS-PROGRAM"))

(in-package "PSEUDOSCHEME-R6RS")

(defparameter *standard-libraries*
  '(((rnrs base)
     "* + - / < <= = > >= ... _ => abs acos and angle append apply asin assert
      assertion-violation atan begin boolean=? boolean? call-with-current-continuation
      call-with-values call/cc car case cdr caar cadr cdar cddr caaar caadr cadar
      caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr
      caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
      ceiling char->integer char<=? char<? char=? char>=? char>? char? complex?
      cond cons cos define define-syntax denominator div div-and-mod div0
      div0-and-mod0 dynamic-wind else eq? equal? eqv? error even? exact
      exact-integer-sqrt exact? exp expt finite? floor for-each gcd identifier-syntax
      if imag-part infinite? inexact inexact? integer->char integer-valued?
      integer? lambda lcm length let let* let*-values let-syntax let-values letrec
      letrec* letrec-syntax list list->string list->vector list-ref list-tail
      list? log magnitude make-polar make-rectangular make-string make-vector
      map max min mod mod0 nan? negative? not null? number->string number?
      numerator odd? or pair? positive? quasiquote quote rational-valued? rational?
      rationalize real-part real-valued? real? reverse round set! sin sqrt string
      string->list string->number string->symbol string-append string-copy
      string-for-each string-length string-ref string<=? string<? string=?
      string>=? string>? string? substring symbol->string symbol=? symbol?
      syntax-rules tan truncate unquote unquote-splicing values vector
      vector->list vector-fill! vector-for-each vector-length vector-map
      vector-ref vector-set! vector? zero?")
    ((rnrs syntax-case)
     "... _ bound-identifier=? datum->syntax free-identifier=? generate-temporaries
      identifier? make-variable-transformer syntax syntax->datum syntax-case
      syntax-violation with-syntax"))
  "Each entry is (library-name \"export ...\").  (rnrs base) is from
chapter 11 of the language report; (rnrs syntax-case) is the small
library whose bindings the language report's chapter 10 and 11.19
describe and which this repository's vendored expander implements.")

(defparameter *syntax-exports*
  "... _ => and assert begin case cond define define-syntax else identifier-syntax
   if lambda let let* let*-values let-syntax let-values letrec letrec* letrec-syntax
   guard or quasiquote quote set! syntax syntax-case syntax-rules unquote unquote-splicing
   with-syntax"
  "The syntactic keywords among the exports above.")

(defparameter *partial-libraries*
  '(((rnrs exceptions) "guard raise raise-continuable with-exception-handler")
    ((rnrs conditions)
     "assertion-violation? condition? condition-irritants condition-message
      condition-who error? irritants-condition? message-condition? who-condition?"))
  "Slices of libraries from the (separate, not in this repository)
standard-libraries report, containing only the names the (rnrs base)
material in the language report depends on -- ERROR and ASSERT raise
conditions, GUARD and WITH-EXCEPTION-HANDLER are referred to throughout --
and which are implemented here.  The names are from memory of that
report, hence kept apart from the cross-checked *STANDARD-LIBRARIES*.")

(defparameter *planned-libraries*
  '((rnrs) (rnrs arithmetic bitwise) (rnrs arithmetic fixnums)
    (rnrs arithmetic flonums) (rnrs bytevectors) (rnrs conditions)
    (rnrs control) (rnrs enums) (rnrs eval) (rnrs exceptions) (rnrs files)
    (rnrs hashtables) (rnrs io ports) (rnrs io simple) (rnrs lists)
    (rnrs mutable-pairs) (rnrs mutable-strings) (rnrs programs)
    (rnrs r5rs) (rnrs records inspection) (rnrs records procedural)
    (rnrs records syntactic) (rnrs sorting) (rnrs unicode))
  "Standard libraries from the (separate) library report, not yet started.")
