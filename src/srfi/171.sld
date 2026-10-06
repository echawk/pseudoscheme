;;; SRFI 171: transducers.  Linus Björnstam's reference implementation,
;;; unmodified (reference/srfi-171/171-impl.scm; MIT licence per the SRFI
;;; document, or the permissive licence in the file header; see
;;; reference/srfi-171/LICENSE).  The library form follows the shipped
;;; srfi/171.sld, taking its chibi branch: fold and reverse! come from
;;; (srfi 1), compose is the definition given there (private), and the
;;; hash tables tdelete-duplicates and treplace use are SRFI 69's.
(define-library (srfi 171)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 9)
          (only (srfi 133) vector->list)
          (srfi 69)
          (only (srfi 1) fold reverse!)
          (srfi 171 meta))
  (export rcons reverse-rcons
          rcount
          rany
          revery

          list-transduce
          vector-transduce
          string-transduce
          bytevector-u8-transduce
          port-transduce
          generator-transduce

          tmap
          tfilter
          tremove
          treplace
          tfilter-map
          tdrop
          tdrop-while
          ttake
          ttake-while
          tconcatenate
          tappend-map
          tdelete-neighbor-duplicates
          tdelete-duplicates
          tflatten
          tsegment
          tpartition
          tadd-between
          tenumerate
          tlog)
  (begin
    (define (compose . functions)
      (define (make-chain thunk chain)
        (lambda args
          (call-with-values (lambda () (apply thunk args)) chain)))
      (if (null? functions)
          values
          (fold make-chain (car functions) (cdr functions)))))
  (include "reference/srfi-171/171-impl.scm"))
