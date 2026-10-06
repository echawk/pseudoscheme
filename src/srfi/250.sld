;;; SRFI 250: insertion-ordered hash tables.  Daphne Preston-Kendal's
;;; sample implementation (compact arrays on SRFI 160's u8/u16/u32/u64
;;; vectors), unmodified (reference/srfi-250/hash-tables.scm and
;;; srfi-compact-arrays.scm; MIT licence in reference/srfi-250/LICENSE).
;;; The library form is the shipped srfi/250.sld with its include paths
;;; changed.  Immutable tables (from hash-table-copy) are only flagged as
;;; such: the mutators check the flag, but the underlying vectors are
;;; ordinary mutable vectors.
;;;
;;; The names clash with SRFIs 69 and 125, which also export
;;; make-hash-table, hash-table-ref and so on.

; SPDX-FileCopyrightText: 2025 Daphne Preston-Kendal
;
; SPDX-License-Identifier: MIT

(define-library (srfi 250)
  (export
   ;; Constructors
   make-hash-table
   (rename prefilled-hash-table hash-table)
   hash-table-unfold
   alist->hash-table
   ;; Predicates
   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table-mutable?
   ;; Accessors
   hash-table-ref
   hash-table-ref/default
   hash-table-comparator
   ;; Mutators
   hash-table-add!
   hash-table-replace!
   hash-table-set!
   hash-table-delete!
   hash-table-intern!
   hash-table-update!
   hash-table-update!/default
   hash-table-pop!
   hash-table-clear!
   ;; The whole hash table
   hash-table-size
   hash-table=
   hash-table-find
   hash-table-count
   hash-table-keys
   hash-table-values
   hash-table-entries
   ;; Low-level iteration
   hash-table-cursor-first
   hash-table-cursor-last
   hash-table-cursor-for-key
   hash-table-cursor-next
   hash-table-cursor-previous
   hash-table-cursor-key
   hash-table-cursor-value
   hash-table-cursor-key+value
   hash-table-cursor-value-set!
   hash-table-cursor-at-end?
   ;; Mapping and folding
   hash-table-map
   hash-table-map!
   hash-table-for-each
   hash-table-map->list
   hash-table-fold
   hash-table-fold-left
   hash-table-fold-right
   hash-table-prune!
   ;; Copying and conversion
   hash-table-copy
   hash-table-empty-copy
   hash-table->alist
   ;; Hash tables as sets
   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!)
  (import (scheme base)
          (scheme case-lambda)

          (srfi 1)
          (srfi 128)
          (srfi 151)
          (srfi 160 base))

  (cond-expand (chibi (import (only (chibi ast) make-immutable!)))
               (else))

  (begin
    (define (void . ignored) (if #f #f))
    (define (hash-truncate h) (bitwise-and (abs h) #xFFFFFFFF))

    (define-record-type Hash-Table
      (%make-hash-table type-test-function hash-function same?-function
                        size next-entry compact-index compact-index-max-fill
                        keys-vector values-vector mutable?)
      hash-table?
      (type-test-function hash-table-type-test-function)
      (hash-function hash-table-hash-function)
      (same?-function hash-table-same?-function)
      (size hash-table-size hash-table-size-set!)
      (next-entry hash-table-next-entry hash-table-next-entry-set!)
      (compact-index hash-table-compact-index hash-table-compact-index-set!)
      (compact-index-max-fill hash-table-compact-index-max-fill hash-table-compact-index-max-fill-set!)
      (keys-vector hash-table-keys-vector hash-table-keys-vector-set!)
      (values-vector hash-table-values-vector hash-table-values-vector-set!)
      (mutable? hash-table-mutable? hash-table-mutable?-set!))

    (define *unfilled*
      (let ()
        (define-record-type Unfilled (make-unfilled) unfilled?)
        (make-unfilled)))
    (define (unfilled? obj) (eq? obj *unfilled*))

    (define *deletion*
      (let ()
        (define-record-type Deletion (make-deletion) deletion?)
        (make-deletion)))
    (define (deletion? obj) (eq? obj *deletion*))

    (define *growth-rate* 3/2)
    (define *default-k* 7)

    (define (assertion-violation who message . irritants)
      (apply error
             (string-append (if (symbol? who)
                                (symbol->string who)
                                who)
                            ": "
                            message)
             irritants)))

  (cond-expand
   (chibi
    (begin
      (define (hash-table-immutablize! ht)
        (make-immutable! (hash-table-keys-vector ht))
        (make-immutable! (hash-table-values-vector ht))
        (make-immutable! (hash-table-compact-index ht))
        (hash-table-mutable?-set! ht #f)
        (make-immutable! ht))))
   (else
    (begin
      (define (hash-table-immutablize! ht)
        (hash-table-mutable?-set! ht #f)))))

  (include "reference/srfi-250/srfi-compact-arrays.scm")
  (include "reference/srfi-250/hash-tables.scm"))
