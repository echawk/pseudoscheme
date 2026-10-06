;;; SRFI 209: enums and enum sets.  Wolfgang Corcoran-Mathe's sample
;;; implementation, on SRFIs 1, 125, 128 and 178, unmodified
;;; (reference/srfi-209/209.scm; MIT licence in
;;; reference/srfi-209/LICENSE).  The library form is the shipped
;;; srfi/209.sld with its include path changed.  (SRFI 162 is not
;;; available, so real-comparator is the library's own fallback.)
;;;
;;; enum-type? and the other names do not clash with (rnrs enums), whose
;;; procedures are all enum-set-... names; but (rnrs enums) and this
;;; library both export enum-set-universe, enum-set-indexer,
;;; enum-set-constructor, enum-set->list, enum-set-member?,
;;; enum-set-subset?, enum-set=?, enum-set-union, enum-set-intersection,
;;; enum-set-difference, enum-set-complement, enum-set-projection,
;;; make-enumeration and define-enumeration, with different contracts.

;;; SPDX-FileCopyrightText: 2020 Wolfgang Corcoran-Mathe <wcm@sigwinch.xyz>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 209)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 125)
          (srfi 128)
          (srfi 178))

  (cond-expand
    ((library (srfi 162))
     (import (srfi 162)))
    (else
     (begin
      (define real-comparator
        (make-comparator real? = < number-hash)))))

  (export enum-type? enum? enum-type-contains? enum=? enum<? enum>?
          enum<=? enum>=?

          make-enum-type

          enum-type enum-name enum-ordinal enum-value

          enum-name->enum enum-ordinal->enum enum-name->ordinal
          enum-name->value enum-ordinal->name enum-ordinal->value

          enum-type-size enum-min enum-max enum-type-enums
          enum-type-names enum-type-values

          enum-next enum-prev

          enum-type->enum-set enum-set list->enum-set enum-set-projection
          enum-set-copy enum-empty-set make-enumeration enum-set-universe
          enum-set-constructor enum-set-type enum-set-indexer

          enum-set? enum-set-contains? enum-set=? enum-set-member?
          enum-set-empty? enum-set-disjoint? enum-set<? enum-set>?
          enum-set<=? enum-set>=? enum-set-any? enum-set-every?
          enum-set-subset?

          enum-set-adjoin! enum-set-delete! enum-set-delete-all!
          enum-set-adjoin enum-set-delete enum-set-delete-all

          enum-set-size enum-set->list enum-set-map->list enum-set-for-each
          enum-set-filter enum-set-remove enum-set-count enum-set-fold
          enum-set->enum-list
          enum-set-filter! enum-set-remove!

          enum-set-union enum-set-intersection enum-set-difference
          enum-set-xor enum-set-complement enum-set-union!
          enum-set-intersection! enum-set-difference! enum-set-xor!
          enum-set-complement!

          make-enum-comparator

          define-enum define-enumeration
          )

  (include "reference/srfi-209/209.scm"))
