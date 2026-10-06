;;; (srfi private srfi-242-make-identifier-hashtable): SRFI 242's reference/srfi-242/make-identifier-hashtable.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi make-identifier-hashtable) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-make-identifier-hashtable)
  (export make-identifier-hashtable)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax))
  (begin

  (define identifier-hash
    (lambda (id)
      (assert (identifier? id))
      (symbol-hash (syntax->datum id))))

  (define make-identifier-hashtable
    (lambda ()
      (make-hashtable identifier-hash bound-identifier=?)))))
