;;; SRFI 117: queues based on lists.  John Cowan's reference
;;; implementation, unmodified (reference/srfi-117/list-queues-impl.scm;
;;; MIT, with R7RS shims taken from Chibi Scheme under BSD-3-Clause:
;;; reference/srfi-117/LICENSE.*).  The library form follows the shipped
;;; list-queues.sld; the file defines its own make-list and list-copy,
;;; so R7RS's are left out of the import.
(define-library (srfi 117)
  (export make-list-queue list-queue list-queue-copy list-queue-unfold
          list-queue-unfold-right list-queue? list-queue-empty?
          list-queue-front list-queue-back list-queue-list list-queue-first-last
          list-queue-add-front! list-queue-add-back! list-queue-remove-front!
          list-queue-remove-back! list-queue-remove-all! list-queue-set-list!
          list-queue-append list-queue-append! list-queue-concatenate
          list-queue-map list-queue-map! list-queue-for-each)
  (import (except (scheme base) make-list list-copy)
          (scheme case-lambda))
  (include "reference/srfi-117/list-queues-impl.scm"))
