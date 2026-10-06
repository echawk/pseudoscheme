;;; (srfi private srfi-242-with-implicit): SRFI 242's reference/srfi-242/with-implicit.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi with-implicit) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-with-implicit)
  (export with-implicit)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax))
  (begin

  (define-syntax with-implicit
    (lambda (x)
      (syntax-case x ()
        ((_ (k x* ...) e e* ...)
         #'(with-syntax ((x* (datum->syntax #'k 'x*)) ...)
             e e* ...))
        (_
         (syntax-violation 'with-implicit "illegal syntax" x)))))))
