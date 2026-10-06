;;; SRFI 148: eager syntax-rules.  Marc Nieper-Wißkirchen's sample
;;; implementation (MIT licence, in reference/srfi-148/), on SRFI 147.
;;; 148.scm and 148.macros.scm are included unmodified, except that
;;; 148.macros.scm is split at its section headings into
;;; reference/srfi-148/148.macros.1.scm to .6.scm (concatenated, they
;;; are the original), and the library into three, (srfi private
;;; srfi-148-a), -b and -c, which this library re-exports.  As one
;;; library, its macro transformers overflow SBCL's limits on one
;;; compiled function (2047 entry points, and arm64's 1 MB branch
;;; range): a Pseudoscheme bug, see the report.  The library form
;;; is the sample's (reference/srfi-148/148.sld), taking its Chibi
;;; branch: the free-identifier=? and bound-identifier=? helpers are
;;; 148.er-macro-transformer.scm's, on SRFI 147's er-macro-transformer,
;;; copied into (srfi private srfi-148-a) with one change:
;;; bound-identifier=? compares identifiers with R6RS's
;;; bound-identifier=?, not eq?, since the input's identifiers are
;;; syntax objects here (anything else with eqv?).  (The portable 148.identifier.scm's helpers
;;; expand into definitions.)
;;;
;;; Since it is built on (srfi 147), a program using em-syntax-rules
;;; should import SRFI 147's define-syntax, let-syntax, letrec-syntax
;;; and syntax-rules too (see 147.sld).
(define-library (srfi 148)
  (export em-syntax-rules

	  ;; General
	  em
	  em-cut
	  em-cute
	  em-constant
	  em-quote
	  em-eval
	  em-apply
	  em-call
	  em-error
	  em-gensym
	  em-generate-temporaries

	  ;; Boolean logic
	  em-if
	  em-not
	  em-or
	  em-and
	  em-null?
	  em-pair?
	  em-list?
	  em-boolean?
	  em-vector?
	  em-symbol?
	  em-bound-identifier=?
	  em-free-identifier=?
	  em-equal?
	  
	  ;; Constructors
	  em-cons
	  em-cons*
	  em-list
	  em-make-list
	  
	  ;; Selectors
	  em-car
	  em-cdr
	  em-caar
	  em-cadr
	  em-cdar
	  em-cddr
	  em-first
	  em-second
	  em-third
	  em-fourth
	  em-fifth
	  em-sixth
	  em-seventh
	  em-eighth
	  em-ninth
	  em-tenth
	  em-list-tail
	  em-list-ref
	  em-take
	  em-drop
	  em-take-right
	  em-drop-right
	  em-last
	  em-last-pair

	  ;; Miscellaneous
	  em-append
	  em-reverse

	  ;; Folding, unfolding, and mapping
	  em-fold
	  em-fold-right
	  em-unfold
	  em-unfold-right
	  em-map
	  em-append-map
	  
	  ;; Filtering
	  em-filter
	  em-remove
	  
	  ;; Searching
	  em-find
	  em-find-tail
	  em-take-while
	  em-drop-while
	  em-any
	  em-every
	  em-member
	  
	  ;; Association lists
	  em-assoc
	  em-alist-delete

	  ;; Set operationse
	  em-set<=
	  em-set=
	  em-set-adjoin
	  em-set-union
	  em-set-intersection
	  em-set-difference
	  em-set-xor

	  ;; Vector processing
	  em-vector
	  em-list->vector
	  em-vector->list
	  em-vector-map
	  em-vector-ref
	  
	  ;; Combinatorics
	  em-0
	  em-1
	  em-2
	  em-3
	  em-4
	  em-5
	  em-6
	  em-7
	  em-8
	  em-9
	  em-10
	  em=
	  em<
	  em<=
	  em>
	  em>=
	  em-zero?
	  em-even?
	  em-odd?
	  em+
	  em-
	  em*
	  em-quotient
	  em-remainder
	  em-fact
	  em-binom
	  
	  ;; Auxiliary syntax
	  =>
	  <>
	  ...)
  (import (only (scheme base) => ...)
          (only (srfi 26) <>)
          (srfi private srfi-148-a)
          (srfi private srfi-148-b)
          (srfi private srfi-148-c)))
