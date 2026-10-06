;;; SRFI 217: integer sets.  Wolfgang Corcoran-Mathe's sample
;;; implementation (big-endian Patricia tries), on SRFIs 1, 143 and 145,
;;; unmodified (reference/srfi-217/trie.scm and 217.scm; MIT licence in
;;; reference/srfi-217/LICENSE).  The library form is the shipped
;;; srfi/217.sld with its include paths changed.  It also exports the
;;; implementation's debugging procedures (iset-trie, highest-set-bit,
;;; highest-bit-mask, lowest-bit-mask), as the shipped library does.

(define-library (srfi 217)
  (import (scheme base)
          (scheme case-lambda)
          (scheme inexact)
          (only (srfi 1) fold every xcons)
          (srfi 143))

  (cond-expand
    ((library (srfi 145))
     (import (srfi 145)))
    (else
     (begin
      (define-syntax assume
        (syntax-rules ()
          ((_ expr . _)
           (or expr (car 0))))))))

  (export iset list->iset
          list->iset!
          iset-unfold make-range-iset
          iset-member
          iset-min iset-max
          iset? iset-contains?
          iset-empty? iset-disjoint?
          iset-adjoin iset-adjoin! iset-delete iset-delete! iset-delete-all
          iset-delete-all!
          iset-search
          iset-search!
          iset-delete-min iset-delete-max
          iset-delete-min! iset-delete-max!
          iset-size
          iset-find
          iset-any? iset-every?
          iset-count iset-fold
          iset-fold-right
          iset-map iset-for-each
          iset-filter iset-remove
          iset-partition iset-partition!
          iset-copy
          iset->list
          iset=?
          iset<? iset>? iset<=? iset>=?
          iset-union iset-union!
          iset-intersection iset-intersection!
          iset-difference iset-difference!
          iset-xor iset-xor!
          iset-open-interval iset-closed-interval iset-open-closed-interval
          iset-closed-open-interval isubset= isubset< isubset<=
          isubset> isubset>=
          ;; debug
          iset-trie
          highest-set-bit
          highest-bit-mask
          lowest-bit-mask
          )

  (include "reference/srfi-217/trie.scm")
  (include "reference/srfi-217/217.scm"))
