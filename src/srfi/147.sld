;;; SRFI 147: custom macro transformers.  Marc Nieper-Wißkirchen's
;;; sample implementation (MIT licence, in reference/srfi-147/), adapted
;;; to psyntax in (srfi private srfi-147-implementation), which see for
;;; the changes.  As in the sample, define-syntax, let-syntax,
;;; letrec-syntax and syntax-rules are new bindings, so a program
;;; imports them in place of (scheme base)'s:
;;;
;;;   (import (except (scheme base) define-syntax let-syntax
;;;                   letrec-syntax syntax-rules)
;;;           (srfi 147))
;;;
;;; and Pseudoscheme doesn't claim the feature custom-macro-transformers.
;;; (psyntax's own define-syntax already takes a macro use that expands
;;; into syntax-rules or a procedure, but not one that expands into a
;;; keyword or a (begin <definition> ... <transformer spec>).)
;;;
;;; A transformer spec headed by lambda (a syntax-case transformer), the
;;; let family, er-/ir-/sc-/rsc-macro-transformer, identifier-syntax or
;;; make-variable-transformer is used as it is; any other macro use is
;;; expanded as a custom transformer.  (srfi 147 er-macro-transformer)
;;; is the sample's: an er-macro-transformer that can itself define
;;; custom transformers.
(define-library (srfi 147)
  (export define-syntax let-syntax letrec-syntax syntax-rules)
  (import (srfi private srfi-147-implementation)))
