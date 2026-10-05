;;; SRFI 127: lazy sequences.  John Cowan's reference implementation,
;;; unmodified (reference/srfi-127/lseqs-impl.scm; MIT licence in
;;; reference/srfi-127/LICENSE).  The library form follows the shipped
;;; lseqs.sld.
(define-library (srfi 127)
  (export generator->lseq lseq? lseq=?
          lseq-car lseq-first lseq-cdr lseq-rest lseq-ref lseq-take lseq-drop
          lseq-realize lseq->generator lseq-length lseq-append lseq-zip
          lseq-map lseq-for-each lseq-filter lseq-remove
          lseq-find lseq-find-tail lseq-take-while lseq-drop-while
          lseq-any lseq-every lseq-index lseq-member lseq-memq lseq-memv)
  (import (scheme base)
          (scheme case-lambda))
  (include "reference/srfi-127/lseqs-impl.scm"))
