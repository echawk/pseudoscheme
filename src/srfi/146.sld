;;; SRFI 146: mappings.  Marc Nieper-Wißkirchen's reference
;;; implementation, unmodified (reference/srfi-146/146.scm, on the
;;; red-black trees of reference/srfi-146/rbtree.scm; MIT licence in
;;; reference/srfi-146/LICENSE).  This library form is the shipped
;;; srfi/146.sld, except that the helper library (nieper rbtree) is
;;; renamed (srfi 146 private rbtree), in 146/private/, so that it is
;;; found on the srfi library path.  (srfi 146 hash) is in 146/hash.sld.

;;; SPDX-FileCopyrightText: 2016 Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT

(define-library (srfi 146)
  (export mapping mapping-unfold
	  mapping/ordered mapping-unfold/ordered
	  mapping? mapping-contains? mapping-empty? mapping-disjoint?
	  mapping-ref mapping-ref/default mapping-key-comparator
	  mapping-adjoin mapping-adjoin!
	  mapping-set mapping-set!
	  mapping-replace mapping-replace!
	  mapping-delete mapping-delete! mapping-delete-all mapping-delete-all!
	  mapping-intern mapping-intern!
	  mapping-update mapping-update! mapping-update/default mapping-update!/default
	  mapping-pop mapping-pop!
	  mapping-search mapping-search!
	  mapping-size mapping-find mapping-count mapping-any? mapping-every?
	  mapping-keys mapping-values mapping-entries
	  mapping-map mapping-map->list mapping-for-each mapping-fold
	  mapping-filter mapping-filter!
	  mapping-remove mapping-remove!
	  mapping-partition mapping-partition!
	  mapping-copy mapping->alist alist->mapping alist->mapping!
	  alist->mapping/ordered alist->mapping/ordered!
	  mapping=? mapping<? mapping>? mapping<=? mapping>=?
	  mapping-union mapping-intersection mapping-difference mapping-xor
	  mapping-union! mapping-intersection! mapping-difference! mapping-xor!
	  make-mapping-comparator
	  mapping-comparator
	  mapping-min-key mapping-max-key
	  mapping-min-value mapping-max-value
	  mapping-key-predecessor mapping-key-successor
	  mapping-range= mapping-range< mapping-range> mapping-range<= mapping-range>=
	  mapping-range=! mapping-range<! mapping-range>! mapping-range<=! mapping-range>=!
	  mapping-split
	  mapping-catenate mapping-catenate!
	  mapping-map/monotone mapping-map/monotone!
	  mapping-fold/reverse
	  comparator?)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 1)
	  (srfi 8)
	  (srfi 128)
	  (srfi 145)
	  (srfi 146 private rbtree))
  (include "reference/srfi-146/146.scm"))
