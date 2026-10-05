;;; SRFI 0: cond-expand (R7RS's, which extends it with (library ...)
;;; requirements).  The features are psl:*scheme-features*: srfi-N for
;;; each SRFI that ships, besides R7RS appendix B's.
(define-library (srfi 0)
  (export cond-expand)
  (import (scheme base)))
