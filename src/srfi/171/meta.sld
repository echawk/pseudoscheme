;;; SRFI 171: transducers -- the (srfi 171 meta) helpers.  Linus
;;; Björnstam's reference implementation, unmodified
;;; (reference/srfi-171/srfi-171-meta.scm; MIT licence per the SRFI
;;; document, or the permissive licence in the file header; see
;;; reference/srfi-171/LICENSE).  The library form is the shipped
;;; srfi/171/meta.sld (reference/srfi-171/171-meta.sld).
(define-library (srfi 171 meta)
  (import (scheme base) (srfi 9))
  (export reduced reduced?
          unreduce
          ensure-reduced
          preserving-reduced

          list-reduce
          vector-reduce
          string-reduce
          bytevector-u8-reduce
          port-reduce
          generator-reduce)
  (include "../reference/srfi-171/srfi-171-meta.scm"))
