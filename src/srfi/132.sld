;;; SRFI 132: sort libraries.  The SRFI's sample implementation (Olin
;;; Shivers' SRFI 32 code, revised by John Cowan; median and selection
;;; code by Will Clinger), unmodified (reference/srfi-132/*.scm; MIT
;;; licence, per the SRFI document, in reference/srfi-132/LICENSE).
;;;
;;; This follows the shipped sorting/132.sld's portable configuration
;;; (it does not use (rnrs sorting)), as a single (srfi 132) library
;;; rather than (srfi 132 sorting) plus a re-exporting (srfi 132).
;;; select.scm wants SRFI 27's random-integer only to pick quickselect
;;; pivots; if (srfi 27) is not available a small generator stands in.
(define-library (srfi 132)
  (export list-sorted?               vector-sorted?
          list-sort                  vector-sort
          list-stable-sort           vector-stable-sort
          list-sort!                 vector-sort!
          list-stable-sort!          vector-stable-sort!
          list-merge                 vector-merge
          list-merge!                vector-merge!
          list-delete-neighbor-dups  vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median         vector-find-median!
          vector-select!             vector-separate!)
  (import (except (scheme base) vector-copy vector-copy!)
          (rename (only (scheme base) vector-copy vector-copy! vector-fill!)
                  (vector-copy  r7rs-vector-copy)
                  (vector-copy! r7rs-vector-copy!)
                  (vector-fill! r7rs-vector-fill!))
          (scheme cxr))
  (cond-expand
   ((library (srfi 27))
    (import (only (srfi 27) random-integer)))
   (else
    (begin
      ;; A Park-Miller generator; only pivot choice depends on it.
      (define %random-state 1)
      (define (random-integer n)
        (set! %random-state (modulo (* %random-state 16807) 2147483647))
        (modulo %random-state n)))))
  (begin
    (define (assert x)
      (if (not x)
          (error "assertion failure"))))
  (include "reference/srfi-132/delndups.scm")     ; list-delete-neighbor-dups etc
  (include "reference/srfi-132/lmsort.scm")       ; list-merge, list-merge!
  (include "reference/srfi-132/sortp.scm")        ; list-sorted?, vector-sorted?
  (include "reference/srfi-132/vector-util.scm")
  (include "reference/srfi-132/vhsort.scm")
  (include "reference/srfi-132/visort.scm")
  (include "reference/srfi-132/vmsort.scm")       ; vector-merge, vector-merge!
  (include "reference/srfi-132/vqsort2.scm")
  (include "reference/srfi-132/vqsort3.scm")
  (include "reference/srfi-132/sort.scm")
  (include "reference/srfi-132/select.scm"))
