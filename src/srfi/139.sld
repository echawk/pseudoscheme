;;; SRFI 139: Syntax parameters.  syntax-parameterize is psyntax's own
;;; (vendor/psyntax): it rebinds each keyword's binding while its body is
;;; expanded, so that uses of the keyword there, including those a macro
;;; introduces, see the new transformer.  define-syntax-parameter is
;;; define-syntax; any keyword can be parameterized, not only one defined
;;; with define-syntax-parameter.
(define-library (srfi 139)
  (export define-syntax-parameter syntax-parameterize)
  (import (scheme base) (only (psyntax extensions) syntax-parameterize))
  (begin
    (define-syntax define-syntax-parameter
      (syntax-rules ()
        ((_ keyword transformer) (define-syntax keyword transformer))))))
