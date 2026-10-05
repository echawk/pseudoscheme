;;; SRFI 228: composing comparators.  Daphne Preston-Kendal's sample
;;; implementation, unmodified (reference/srfi-228/srfi-228.scm; MIT
;;; licence, per the SRFI document, in reference/srfi-228/LICENSE).  The
;;; library form is the shipped srfi/228.sld.
(define-library (srfi 228)
  (export make-wrapper-comparator make-product-comparator
          make-sum-comparator comparator-one comparator-zero)
  (import (scheme base)
          (srfi 1)
          (srfi 128)
          (srfi 151))
  (include "reference/srfi-228/srfi-228.scm"))
