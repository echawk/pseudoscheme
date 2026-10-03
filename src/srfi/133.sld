;;; SRFI 133: vector library (R7RS-compatible).  The SRFI's reference
;;; implementation (Taylor Campbell, Will Clinger, John Cowan),
;;; unmodified (reference/srfi-133/vectors-impl.scm; public domain, or
;;; the MIT licence in reference/srfi-133/LICENSE where that is not
;;; possible).
;;;
;;; The reference file also defines its own versions of the R7RS vector
;;; procedures; those are excluded from the (scheme base) import, so
;;; they stay private to this library, and the library exports R7RS's
;;; very bindings instead (they meet SRFI 133's spec), so that
;;; (import (scheme base) (srfi 133)) does not conflict.
(define-library (srfi 133)
  (export
   ;; Constructors
   vector-unfold vector-unfold-right vector-reverse-copy
   vector-concatenate vector-append-subvectors
   ;; Predicates
   vector-empty? vector=
   ;; Iteration
   vector-fold vector-fold-right vector-map! vector-count vector-cumulate
   ;; Searching
   vector-index vector-index-right vector-skip vector-skip-right
   vector-binary-search vector-any vector-every vector-partition
   ;; Mutators
   vector-swap! vector-reverse! vector-reverse-copy!
   vector-unfold! vector-unfold-right!
   ;; Conversion
   reverse-vector->list reverse-list->vector
   ;; R7RS's own, as SRFI 133 specifies
   make-vector vector vector? vector-ref vector-set! vector-length
   (rename r7:vector-copy vector-copy)
   (rename r7:vector-append vector-append)
   (rename r7:vector-map vector-map)
   (rename r7:vector-for-each vector-for-each)
   (rename r7:vector-fill! vector-fill!)
   (rename r7:vector-copy! vector-copy!)
   (rename r7:vector->list vector->list)
   (rename r7:list->vector list->vector)
   (rename r7:vector->string vector->string)
   (rename r7:string->vector string->vector))
  (import (except (scheme base)
                  vector-copy vector-append vector-map vector-for-each
                  vector-fill! vector-copy! vector->list list->vector
                  vector->string string->vector)
          (prefix (only (scheme base)
                        vector-copy vector-append vector-map vector-for-each
                        vector-fill! vector-copy! vector->list list->vector
                        vector->string string->vector)
                  r7:)
          (scheme cxr))
  (include "reference/srfi-133/vectors-impl.scm"))
