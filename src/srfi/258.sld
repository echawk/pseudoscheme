;;; SRFI 258: Uninterned symbols.  Written for Pseudoscheme, after Wolfgang
;;; Corcoran-Mathe's Chez Scheme wrapper: Scheme symbols are Common Lisp
;;; symbols, and an uninterned one is a Lisp symbol with no package
;;; (MAKE-SYMBOL), named as STRING->SYMBOL names one: with its case
;;; inverted (ps:invert-case, src/core.lisp), so that SYMBOL->STRING gives
;;; the name back.
(define-library (srfi 258)
  (export string->uninterned-symbol symbol-interned? generate-uninterned-symbol)
  (import (scheme base) (scheme case-lambda)
          (only (srfi 27) random-integer)
          (prefix (cl common-lisp) cl:)
          (only (prefix (cl pseudoscheme) ps:) ps:invert-case))
  (begin
    (define (string->uninterned-symbol string)
      (unless (string? string) (error "string->uninterned-symbol: not a string" string))
      (cl:make-symbol (ps:invert-case string)))

    (define (symbol-interned? symbol)
      (unless (symbol? symbol) (error "symbol-interned?: not a symbol" symbol))
      (not (null? (cl:symbol-package symbol))))

    (define generate-uninterned-symbol
      (case-lambda
        (() (generate-uninterned-symbol ""))
        ((prefix)
         (let ((p (cond ((string? prefix) prefix)
                        ((symbol? prefix) (symbol->string prefix))
                        (else (error "generate-uninterned-symbol: not a string or symbol" prefix)))))
           (string->uninterned-symbol
            (string-append p (number->string (random-integer (expt 2 128)) 36)))))))))
