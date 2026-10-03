;;; SRFI 43: vector library.  Taylor Campbell's reference implementation
;;; (reference/srfi-43.scm), public domain.
;;;
;;; Where R7RS's procedure already meets SRFI 43's spec (make-vector,
;;; vector, vector?, vector-ref, vector-length, vector-set!, vector-fill!,
;;; vector-copy!, vector->list, list->vector, vector-append) the library
;;; exports that very binding.  SRFI 43's vector-map and vector-for-each
;;; pass the index to the procedure, and its vector-copy takes a fill
;;; argument and allows END past the vector's length, so those three
;;; differ from R7RS's: a program importing (scheme base) along with
;;; (srfi 43) has to exclude one side's.
;;;
;;; Modification to the reference code:
;;; - the definitions of the eleven procedures above are commented out.
;;;   The reference redefines R5RS's at top level, e.g.
;;;   (define vector-ref vector-ref), or wraps the native one, which is
;;;   impossible inside a library body; R7RS's versions are exported.
(define-library (srfi 43)
  (export vector-unfold vector-unfold-right vector-copy vector-reverse-copy
          vector-concatenate
          vector-empty? vector=
          vector-fold vector-fold-right vector-map vector-map!
          vector-for-each vector-count
          vector-index vector-skip vector-index-right vector-skip-right
          vector-binary-search vector-any vector-every
          vector-swap! vector-reverse! vector-reverse-copy!
          reverse-vector->list reverse-list->vector
          ;; R7RS's, which meet SRFI 43's spec
          make-vector vector vector? vector-ref vector-length vector-set!
          vector-fill! vector-copy! vector->list list->vector vector-append)
  (import (except (scheme base) vector-copy vector-map vector-for-each)
          (scheme cxr))
  (include "reference/srfi-43.scm"))
