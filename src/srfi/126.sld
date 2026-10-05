;;; SRFI 126: R6RS-based hashtables.  Taylan Kammer's reference
;;; implementation (reference/srfi-126/126.body.scm; MIT licence in
;;; reference/srfi-126/LICENSE), on Pseudoscheme's (rnrs hashtables):
;;; SRFI 126 hashtables are R6RS hashtables.  This library form follows
;;; the shipped srfi/126.sld, importing (rnrs hashtables) and
;;; (rnrs enums) where that imports (r6rs ...).
;;;
;;; Modifications to the reference file, each marked PSEUDOSCHEME there:
;;; weak and ephemeral hashtables are supported for eq? and eqv? tables
;;; (make-eq-hashtable, make-eqv-hashtable, make-hashtable with no hash
;;; function, the alist-> constructors, hashtable-copy and
;;; hashtable-empty-copy), using trivial-garbage's weak CL hash tables;
;;; see 126/private/weak.sld.  hashtable-weakness reports the weakness.
;;; A table with a custom hash function still cannot be weak, and asking
;;; for one is an error, as in the reference implementation.
;;;
;;; The procedures the reference file defines as plain aliases of R6RS
;;; ones (hashtable?, hashtable-set!, hashtable-keys, equal-hash, ...)
;;; are exported as R6RS's very bindings, so (rnrs) and (srfi 126) can
;;; be imported together except for the procedures SRFI 126 extends
;;; (make-eq-hashtable, hashtable-ref, hashtable-update!,
;;; hashtable-copy, ...), which a program has to exclude from one side.

;;; SPDX-FileCopyrightText: 2015 - 2016 Taylan Kammer <taylan.kammer@gmail.com>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 126)
  (export
   make-eq-hashtable make-eqv-hashtable make-hashtable
   alist->eq-hashtable alist->eqv-hashtable alist->hashtable
   weakness
   (rename rnrs-hashtable? hashtable?)
   (rename rnrs-hashtable-size hashtable-size)
   hashtable-ref
   (rename rnrs-hashtable-set! hashtable-set!)
   (rename rnrs-hashtable-delete! hashtable-delete!)
   (rename rnrs-hashtable-contains? hashtable-contains?)
   hashtable-lookup hashtable-update! hashtable-intern!
   hashtable-copy hashtable-clear! hashtable-empty-copy
   (rename rnrs-hashtable-keys hashtable-keys)
   hashtable-values
   (rename rnrs-hashtable-entries hashtable-entries)
   hashtable-key-list hashtable-value-list hashtable-entry-lists
   hashtable-walk hashtable-update-all! hashtable-prune! hashtable-merge!
   hashtable-sum hashtable-map->lset hashtable-find
   hashtable-empty? hashtable-pop! hashtable-inc! hashtable-dec!
   (rename rnrs-hashtable-equivalence-function hashtable-equivalence-function)
   (rename rnrs-hashtable-hash-function hashtable-hash-function)
   hashtable-weakness
   (rename rnrs-hashtable-mutable? hashtable-mutable?)
   hash-salt
   (rename rnrs-equal-hash equal-hash)
   (rename rnrs-string-hash string-hash)
   (rename rnrs-string-ci-hash string-ci-hash)
   (rename rnrs-symbol-hash symbol-hash))
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme process-context)
   (srfi 1)
   (srfi 27)
   (rnrs enums)
   (prefix (rnrs hashtables) rnrs-)
   (srfi 126 private weak))
  (begin
    ;; Smallest allowed in R6RS.
    (define (greatest-fixnum) (expt 23 2)))
  (include "reference/srfi-126/126.body.scm"))
