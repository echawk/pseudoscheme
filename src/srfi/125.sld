;;; SRFI 125: intermediate hash tables.  William D Clinger's reference
;;; implementation, unmodified (reference/srfi-125/125.body.scm; its
;;; licence is in reference/srfi-125/LICENSE).
;;;
;;; The reference implementation is written on top of SRFI 126.  Every
;;; SRFI 126 name it uses is also an R6RS (rnrs hashtables) name with
;;; the same meaning (it always passes hashtable-ref a default), except
;;; symbol-hash, which here comes from SRFI 128 instead (Pseudoscheme's
;;; (rnrs hashtables) exports a symbol-hash of its own, excluded).  So this
;;; library imports (rnrs hashtables) where the shipped srfi/125.sld
;;; imports (srfi 126); SRFI 125 hash tables are R6RS hashtables.
;;;
;;; The deprecated string-hash and string-ci-hash are SRFI 128's (whose
;;; exported versions accept SRFI 125's optional second argument), as in
;;; chibi, so (srfi 125) and (srfi 128) -- (scheme hash-table) and
;;; (scheme comparator) -- can be imported together.

;;; SPDX-FileCopyrightText: 2015 William D Clinger <will@ccs.neu.edu>
;;;
;;; SPDX-License-Identifier: LicenseRef-Clinger

(define-library (srfi 125)
  (export
   make-hash-table hash-table hash-table-unfold alist->hash-table
   hash-table? hash-table-contains? hash-table-empty? hash-table=?
   hash-table-mutable?
   hash-table-ref hash-table-ref/default
   hash-table-set! hash-table-delete! hash-table-intern! hash-table-update!
   hash-table-update!/default hash-table-pop! hash-table-clear!
   hash-table-size hash-table-keys hash-table-values hash-table-entries
   hash-table-find hash-table-count
   hash-table-map hash-table-for-each hash-table-map! hash-table-map->list
   hash-table-fold hash-table-prune!
   hash-table-copy hash-table-empty-copy hash-table->alist
   hash-table-union! hash-table-intersection! hash-table-difference!
   hash-table-xor!
   ;; The following procedures are deprecated by SRFI 125:
   (rename deprecated:hash hash)
   string-hash string-ci-hash
   (rename deprecated:hash-by-identity hash-by-identity)
   (rename deprecated:hash-table-equivalence-function
           hash-table-equivalence-function)
   (rename deprecated:hash-table-hash-function hash-table-hash-function)
   (rename deprecated:hash-table-exists? hash-table-exists?)
   (rename deprecated:hash-table-walk hash-table-walk)
   (rename deprecated:hash-table-merge! hash-table-merge!))
  (import (scheme base)
          (scheme char)
          (scheme write) ; for warnings about deprecated features
          (except (rnrs hashtables) symbol-hash string-hash string-ci-hash)
          (srfi 128))
  (include "reference/srfi-125/125.body.scm"))
