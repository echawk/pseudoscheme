;;; SRFI 212: Aliases.  alias is psyntax's own (vendor/psyntax: a body
;;; or top-level form that binds an identifier to another's binding).
;;; An unbound identifier can be aliased; the alias is then unbound too,
;;; and not free-identifier=? to it.
(define-library (srfi 212)
  (export alias)
  (import (only (psyntax extensions) alias)))
