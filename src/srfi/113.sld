;;; SRFI 113: sets and bags.  John Cowan's reference implementation,
;;; unmodified (reference/srfi-113/sets-impl.scm; MIT licence in
;;; reference/srfi-113/LICENSE).  This library form follows the shipped
;;; sets/sets.sld, renamed (srfi 113), with (comparators) as (srfi 128)
;;; and SRFI 69 hash tables from (srfi 69), as in the reference.
;;; SRFI 69's string-hash and string-ci-hash are excluded in favour of
;;; SRFI 128's (the reference file uses neither).

;;; SPDX-FileCopyrightText: 2013 John Cowan <cowan@ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 113)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 128)
          (except (srfi 69) string-hash string-ci-hash))

  (export set set-unfold)
  (export set? set-contains? set-empty? set-disjoint?)
  (export set-member set-element-comparator)
  (export set-adjoin set-adjoin! set-replace set-replace!
          set-delete set-delete! set-delete-all set-delete-all! set-search!)
  (export set-size set-find set-count set-any? set-every?)
  (export set-map set-for-each set-fold
          set-filter set-remove set-partition
          set-filter! set-remove! set-partition!)
  (export set-copy set->list list->set list->set!)
  (export set=? set<? set>? set<=? set>=?)
  (export set-union set-intersection set-difference set-xor
          set-union! set-intersection! set-difference! set-xor!)
  (export set-comparator)

  (export bag bag-unfold)
  (export bag? bag-contains? bag-empty? bag-disjoint?)
  (export bag-member bag-element-comparator)
  (export bag-adjoin bag-adjoin! bag-replace bag-replace!
          bag-delete bag-delete! bag-delete-all bag-delete-all! bag-search!)
  (export bag-size bag-find bag-count bag-any? bag-every?)
  (export bag-map bag-for-each bag-fold
          bag-filter bag-remove bag-partition
          bag-filter! bag-remove! bag-partition!)
  (export bag-copy bag->list list->bag list->bag!)
  (export bag=? bag<? bag>? bag<=? bag>=?)
  (export bag-union bag-intersection bag-difference bag-xor
          bag-union! bag-intersection! bag-difference! bag-xor!)
  (export bag-comparator)

  (export bag-sum bag-sum! bag-product bag-product!
          bag-unique-size bag-element-count bag-for-each-unique bag-fold-unique
          bag-increment! bag-decrement! bag->set set->bag set->bag!
          bag->alist alist->bag)

  (include "reference/srfi-113/sets-impl.scm"))
