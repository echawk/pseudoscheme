;;; SRFI 213: Identifier properties.  define-property is psyntax's own
;;; (vendor/psyntax): it gives the identifier a new label, bound as its
;;; old one is, carrying the property.  A transformer that returns a
;;; procedure is called with the lookup procedure, so capture-lookup is
;;; the identity, as in the SRFI's sample implementation for Chez Scheme.
;;;
;;; Properties are expansion-time data: a library's are not kept with its
;;; compiled form in the library cache, so a property defined in one
;;; library is seen by a program that imports it only when the library is
;;; expanded from source in the same session.
(define-library (srfi 213)
  (export define-property capture-lookup)
  (import (scheme base) (only (psyntax extensions) define-property))
  (begin
    (define (capture-lookup proc) proc)))
