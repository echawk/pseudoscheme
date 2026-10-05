;;; SRFI 146: hashmaps, (srfi 146 hash).  Arthur A. Gleckler's
;;; reference implementation on his persistent hash array mapped tries
;;; (after Marc Nieper-Wißkirchen's original), unmodified
;;; (reference/srfi-146/hash.scm, hamt*.scm, vector-edit.scm; MIT licence
;;; in reference/srfi-146/LICENSE).  This library form is the shipped
;;; srfi/146/hash.sld, except that the helper libraries (gleckler ...)
;;; are renamed (srfi 146 private ...), in 146/private/, so that they
;;; are found on the srfi library path.

;;; SPDX-FileCopyrightText: 2016 Marc Nieper-Wißkirchen
;;; SPDX-License-Identifier: MIT

(define-library (srfi 146 hash)
  (export hashmap hashmap-unfold
	  hashmap? hashmap-contains? hashmap-empty? hashmap-disjoint?
	  hashmap-ref hashmap-ref/default hashmap-key-comparator
	  hashmap-adjoin hashmap-adjoin!
	  hashmap-set hashmap-set!
	  hashmap-replace hashmap-replace!
	  hashmap-delete hashmap-delete! hashmap-delete-all hashmap-delete-all!
	  hashmap-intern hashmap-intern!
	  hashmap-update hashmap-update! hashmap-update/default hashmap-update!/default
	  hashmap-pop hashmap-pop!
	  hashmap-search hashmap-search!
	  hashmap-size hashmap-find hashmap-count hashmap-any? hashmap-every?
	  hashmap-keys hashmap-values hashmap-entries
	  hashmap-map hashmap-map->list hashmap-for-each hashmap-fold
	  hashmap-filter hashmap-filter!
	  hashmap-remove hashmap-remove!
	  hashmap-partition hashmap-partition!
	  hashmap-copy hashmap->alist alist->hashmap alist->hashmap!
	  hashmap=? hashmap<? hashmap>? hashmap<=? hashmap>=?
	  hashmap-union hashmap-intersection hashmap-difference hashmap-xor
	  hashmap-union! hashmap-intersection! hashmap-difference! hashmap-xor!
	  make-hashmap-comparator
	  hashmap-comparator
	  comparator?)
  (import (scheme base)
	  (scheme case-lambda)
	  (srfi 1)
	  (srfi 8)
	  (srfi 128)
	  (srfi 145)
	  (srfi 158)
	  (srfi 146 private hamt-map))
  (include "../reference/srfi-146/hash.scm"))
