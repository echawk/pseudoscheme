;;; SRFI 66: octet vectors.  Written for Pseudoscheme, as a thin layer
;;; over bytevectors (the SRFI's reference implementation wraps a vector
;;; in a record instead).  An octet vector is a bytevector, which is
;;; also SRFI 4's u8vector, so the procedures SRFI 4 has too are SRFI 4's
;;; very bindings and (srfi 4) and (srfi 66) can be imported together.
;;; u8vector=? and u8vector-copy! are R6RS's bytevector=? and
;;; bytevector-copy!, which have SRFI 66's argument order (source
;;; source-start target target-start n) and handle overlap;
;;; u8vector-copy is bytevector-copy.
;;;
;;; SRFI 160's (srfi 160 u8) also exports u8vector-copy!, with R7RS's
;;; argument order (to at from [start end]), and its own u8vector-copy;
;;; import one of them with except or prefix.
(define-library (srfi 66)
  (export u8vector? make-u8vector u8vector u8vector->list list->u8vector
          u8vector-length u8vector-ref u8vector-set!
          u8vector=? u8vector-compare u8vector-copy! u8vector-copy)
  (import (scheme base)
          (only (srfi 4) u8vector? make-u8vector u8vector u8vector->list
                list->u8vector u8vector-length u8vector-ref u8vector-set!)
          (rename (only (rnrs bytevectors) bytevector=? bytevector-copy!)
                  (bytevector=? u8vector=?)
                  (bytevector-copy! u8vector-copy!)))
  (begin
    (define (u8vector-copy u8vector) (bytevector-copy u8vector))

    ;; -1, 0 or 1, as SRFI 67's vector-compare: a shorter vector is
    ;; smaller; vectors of equal length compare lexicographically.
    (define (u8vector-compare u8vector-1 u8vector-2)
      (let ((length-1 (bytevector-length u8vector-1))
            (length-2 (bytevector-length u8vector-2)))
        (cond ((< length-1 length-2) -1)
              ((> length-1 length-2) 1)
              (else
               (let loop ((i 0))
                 (if (= i length-1)
                     0
                     (let ((elt-1 (bytevector-u8-ref u8vector-1 i))
                           (elt-2 (bytevector-u8-ref u8vector-2 i)))
                       (cond ((< elt-1 elt-2) -1)
                             ((> elt-1 elt-2) 1)
                             (else (loop (+ i 1)))))))))))))
