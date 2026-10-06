;;; SRFI 25: multi-dimensional array primitives.  Jussi Piitulainen's
;;; reference implementation, unmodified (MIT licence, per the SRFI
;;; document, in reference/srfi-25/LICENSE).  Of its interchangeable
;;; parts this library uses the ones the shipped srfi-25-reference.scm
;;; does -- ix-ctor.scm (indexing by a vector of coefficients),
;;; op-ctor.scm (its optimizer) and array.scm -- except that the
;;; representation is as-srfi-9-record.scm, a record type, rather than
;;; as-procedure.scm, whose arrays are pairs (so array? would be true
;;; of some lists).  test.scm, also unmodified, is the reference test
;;; suite that tests/25.scm is made from.  The one change: array.scm
;;; is converted from Latin-1 to UTF-8 (an acute accent in a comment).
;;;
;;; The SRFI's make-array and array clash with SRFI 231's and SRFI
;;; 179's procedures of the same names; they are different libraries.
(define-library (srfi 25)
  (export array? make-array shape array array-rank array-start array-end
          array-ref array-set! share-array)
  (import (scheme base) (scheme write))
  (include "reference/srfi-25/as-srfi-9-record.scm"
           "reference/srfi-25/ix-ctor.scm"
           "reference/srfi-25/op-ctor.scm"
           "reference/srfi-25/array.scm"))
