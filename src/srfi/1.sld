;;; SRFI 1: list library.  Olin Shivers' reference implementation,
;;; unmodified (reference/srfi-1.scm).  Where R7RS's procedure already
;;; meets SRFI 1's spec (make-list, list-copy, map, member and assoc
;;; with a comparison), the library exports that very binding, so that
;;; the common (import (scheme base) (srfi 1)) does not conflict.
(define-library (srfi 1)
  (export xcons list-tabulate cons*
          proper-list? circular-list? dotted-list? not-pair? null-list? list=
          circular-list length+ iota
          first second third fourth fifth sixth seventh eighth ninth tenth
          car+cdr take drop take-right drop-right take! drop-right!
          split-at split-at! last last-pair
          zip unzip1 unzip2 unzip3 unzip4 unzip5 count
          append! append-reverse append-reverse! concatenate concatenate!
          unfold fold pair-fold reduce unfold-right fold-right pair-fold-right reduce-right
          append-map append-map! map! pair-for-each filter-map map-in-order
          filter partition remove filter! partition! remove!
          find find-tail any every list-index
          take-while drop-while take-while! span break span! break!
          delete delete! alist-cons alist-copy
          delete-duplicates delete-duplicates! alist-delete alist-delete!
          reverse! lset<= lset= lset-adjoin
          lset-union lset-intersection lset-difference lset-xor lset-diff+intersection
          lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!
          (rename r7:make-list make-list) (rename r7:list-copy list-copy)
          (rename r7:map map) (rename r7:member member) (rename r7:assoc assoc)
          for-each
          ;; the rest of R7RS's list procedures, as SRFI 1 specifies
          cons pair? null? car cdr set-car! set-cdr! list length append reverse
          caar cadr cdar cddr list-ref memq memv assq assv)
  (import (except (scheme base) make-list list-copy map member assoc)
          (prefix (only (scheme base) make-list list-copy map member assoc) r7:)
          (scheme cxr)
          (srfi private shim))
  (include "reference/srfi-1.scm"))
