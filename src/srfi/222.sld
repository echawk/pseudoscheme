;;; SRFI 222: compound objects.  John Cowan and Arvydas Silanskas's
;;; sample implementation, unmodified (reference/srfi-222/222-impl.scm;
;;; MIT licence, per the SRFI document, in reference/srfi-222/LICENSE).
;;; The library form is the shipped srfi/222.sld with its include path
;;; changed.
(define-library (srfi 222)
  (import (scheme base))
  (export
   make-compound
   compound?
   compound-subobjects
   compound-length
   compound-ref
   compound-map
   compound-map->list
   compound-filter
   compound-predicate
   compound-access)
  (include "reference/srfi-222/222-impl.scm"))
