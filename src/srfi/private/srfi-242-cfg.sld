;;; (srfi private srfi-242-cfg): SRFI 242's (srfi cfg)
;;; (reference/srfi-242/cfg.sls), which re-exports (srfi cfg primitive)
;;; and (srfi cfg derived).  Here cfg is a macro of this library that
;;; passes its form to (srfi cfg primitive)'s, and whose transformer
;;; refers to a variable of (srfi private srfi-242-cfg-derived).  So,
;;; when cfg is used, psyntax invokes the derived library first, which
;;; registers its cfg syntax (bind, label*, permute) as identifier
;;; properties, even when the libraries come from the compiled cache
;;; (see (srfi private srfi-242-define-property)).
(define-library (srfi private srfi-242-cfg)
  (export cfg call execute finally halt label* labels
          bind permute
          define-cfg-label define-cfg-label*
          define-cfg-syntax define-cfg-syntax*)
  (import (rnrs)
          (rename (except (srfi private srfi-242-cfg-primitive) label* permute)
                  (cfg primitive-cfg))
          (srfi private srfi-242-cfg-derived))
  (begin
    (define-syntax cfg
      (lambda (stx)
        (if %srfi-242-derived
            (syntax-case stx ()
              ((_ . args) #'(primitive-cfg . args)))
            #f)))))
