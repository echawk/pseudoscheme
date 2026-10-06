;;; SRFI 116: immutable list library.  John Cowan's reference
;;; implementation (after Olin Shivers' SRFI 1)
;;; (reference/srfi-116/ilists-base.scm and ilists-impl.scm; MIT licence
;;; in reference/srfi-116/LICENSE).  The library form is the shipped
;;; ilists/ilists.sld, renamed (srfi 116), with its include paths changed
;;; and its Gauche-only cond-expand clause dropped.
;;;
;;; Two changes to ilists-impl.scm, marked PSEUDOSCHEME there: it defined
;;; iappend-reverse twice and imap a second time (after ilists-base.scm),
;;; and a library may not define a name twice, so the earlier
;;; iappend-reverse and the second imap (both equivalent) are commented
;;; out.
;;;
;;; ipairs are records, so the global equal? does not look inside them
;;; (it is eqv? on two ipairs), although the SRFI asks, after
;;; finalization, that equal? descend into them; compare ilists with
;;; ilist= or ilist-comparator.  Inside this library, equal? is an
;;; ilist-aware version (defined below), so the procedures whose
;;; equality defaults to equal? (imember, iassoc, idelete, ...) do
;;; descend into ipairs.

(define-library (srfi 116)
  (import (except (scheme base) equal?)
          (rename (only (scheme base) equal?) (equal? r7:equal?)))
  (import (srfi 128))
  (import (scheme write))  ; for write-ipair, which the shipped library leaves unbound
  (export iq)
  (export ipair ilist xipair ipair* make-ilist ilist-copy ilist-tabulate iiota)
  (export ipair?)
  (export proper-ilist? ilist? dotted-ilist? not-ipair? null-ilist? ilist=)
  (export icar icdr ilist-ref)
  (export ifirst isecond ithird ifourth ififth isixth iseventh ieighth ininth itenth)
  (export icaar icadr icdar icddr)
  (export icaaar icaadr icadar icaddr icdaar icdadr icddar icdddr)
  (export icaaaar icaaadr icaadar icaaddr icadaar icadadr icaddar icadddr)
  (export icdaaar icdaadr icdadar icdaddr icddaar icddadr icdddar icddddr)
  (export icar+icdr itake idrop ilist-tail)
  (export itake-right idrop-right isplit-at ilast last-ipair)
  (export ilength iappend iconcatenate ireverse iappend-reverse)
  (export izip iunzip1 iunzip2 iunzip3 iunzip4 iunzip5)
  (export icount imap ifor-each ifold iunfold ipair-fold ireduce )
  (export ifold-right iunfold-right ipair-fold-right ireduce-right )
  (export iappend-map ipair-for-each ifilter-map imap-in-order)
  (export ifilter ipartition iremove imember imemq imemv)
  (export ifind ifind-tail iany ievery)
  (export ilist-index itake-while idrop-while ispan ibreak)
  (export idelete idelete-duplicates )
  (export iassoc iassq iassv ialist-cons ialist-delete)
  (export replace-icar replace-icdr)
  (export pair->ipair ipair->pair list->ilist ilist->list)
  (export tree->itree itree->tree gtree->itree gtree->tree)
  (export iapply)
  (export ipair-comparator ilist-comparator)
  (export make-ilist-comparator make-improper-ilist-comparator)
  (export make-ipair-comparator make-icar-comparator make-icdr-comparator)
  (begin
    ;; equal? that descends into ipairs, as the SRFI asks (after
    ;; finalization); the default equality of imember, iassoc, idelete,
    ;; ialist-delete and idelete-duplicates.
    (define (equal? a b)
      (cond ((and (ipair? a) (ipair? b))
             (and (equal? (icar a) (icar b)) (equal? (icdr a) (icdr b))))
            ((and (pair? a) (pair? b))
             (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
            ((and (vector? a) (vector? b))
             (let ((n (vector-length a)))
               (and (= n (vector-length b))
                    (let loop ((i 0))
                      (or (= i n)
                          (and (equal? (vector-ref a i) (vector-ref b i))
                               (loop (+ i 1))))))))
            (else (r7:equal? a b)))))
  (include "reference/srfi-116/ilists-base.scm")
  (include "reference/srfi-116/ilists-impl.scm")

)
