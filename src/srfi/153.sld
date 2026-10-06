;;; SRFI 153: ordered sets.  John Cowan's sample implementation, on
;;; SRFIs 128 and 146, unmodified (reference/srfi-153/153-impl.scm; MIT
;;; licence in reference/srfi-153/LICENSE).  The library form is the
;;; shipped srfi/153.sld with its include path changed.
(define-library (srfi 153)
  (export oset oset/ordered oset-unfold oset-unfold/ordered
          oset-accumulate
          oset? oset-contains? oset-empty? oset-disjoint?
          oset-member oset-element-comparator
          oset-adjoin oset-adjoin/replace oset-delete oset-delete-all
          oset-pop oset-pop/reverse
          oset-size oset-find oset-count oset-any? oset-every?
          oset-map oset-map/monotone oset-for-each oset-fold oset-fold/reverse
          oset-filter oset-remove oset-partition
          oset->list list->oset list->oset/ordered
          oset=? oset<? oset>? oset<=? oset>=?
          oset-union oset-intersection oset-difference oset-xor
          oset-min-element oset-max-element
          oset-element-predecessor oset-element-successor
          oset-range= oset-range< oset-range> oset-range<= oset-range>=
          oset-split oset-catenate)
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (srfi 128)
          (srfi 146))
  (include "reference/srfi-153/153-impl.scm"))
