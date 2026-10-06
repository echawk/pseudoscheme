;;; SRFI 211: Scheme macro libraries.  Written for Pseudoscheme.  The
;;; SRFI's implementation is "necessarily distinct for every Scheme
;;; implementation": re-exports of what the system has.  psyntax has
;;; syntax-case, identifier syntax and variable transformers; the rest
;;; is built on syntax-case in (srfi private srfi-211-transformers):
;;;
;;;   (srfi 211 syntax-case)          R6RS's, re-exported
;;;   (srfi 211 identifier-syntax)    R6RS's, re-exported
;;;   (srfi 211 variable-transformer) R6RS's, re-exported
;;;   (srfi 211 low-level)            syntax, unwrap-syntax, ...
;;;   (srfi 211 explicit-renaming)    er-macro-transformer, following
;;;                                   the SRFI's notes: raw symbols in
;;;                                   the output are injected, rename
;;;                                   takes any datum
;;;   (srfi 211 implicit-renaming)    ir-macro-transformer
;;;   (srfi 211 syntactic-closures)   MIT-style, on syntax-case
;;;   (srfi 211 define-macro)         define-macro, lisp-transformer
;;;   (srfi 211 presyntax)            preidentifier? and friends
;;;
;;; Not provided: (srfi 211 with-ellipsis) and (srfi 211
;;; syntax-parameter), which need support in the expander that psyntax
;;; doesn't have.  So this composite library exports everything except
;;; with-ellipsis, define-syntax-parameter and syntax-parameterize.
;;;
;;; er-macro-transformer, ir-macro-transformer, sc-macro-transformer and
;;; rsc-macro-transformer are macros, so that they can capture the
;;; place where the transformer is made (to rename symbols there).  Used
;;; as variables, they are procedures.  An input form is fully
;;; unwrapped: its leaves are identifiers, not symbols.
;;;
;;; generate-identifier is generate-temporaries': its name is a fresh
;;; symbol even when a symbol is given (psyntax can make a fresh
;;; identifier only that way).
(define-library (srfi 211)
  (export identifier? preidentifier? bound-identifier=? free-identifier=?
          syntax->datum presyntax->datum identifier->symbol preidentifier->symbol
          unwrap-syntax unwrap-presyntax datum->syntax construct-identifier
          generate-temporaries generate-identifier syntax-violation
          make-variable-transformer er-macro-transformer ir-macro-transformer
          lisp-transformer sc-macro-transformer rsc-macro-transformer
          make-syntactic-closure close-syntax capture-syntactic-environment
          sc-identifier=? make-synthetic-identifier
          syntax-case syntax with-syntax quasisyntax unsyntax unsyntax-splicing
          identifier-syntax define-macro)
  (import (rnrs syntax-case)
          (only (rnrs base) identifier-syntax)
          (except (srfi private srfi-211-transformers) unwrap-all close-presyntax)
          (srfi 211 define-macro)))
