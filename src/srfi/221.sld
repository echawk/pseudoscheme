;;; SRFI 221: generator/accumulator sub-library.  Arvydas Silanskas's
;;; sample implementation, unmodified (reference/srfi-221/221-impl.scm;
;;; MIT licence, per the SRFI document, in reference/srfi-221/LICENSE).
;;; The library form is the shipped srfi/221.sld.
(define-library (srfi 221)
  (import
    (scheme base)
    (scheme case-lambda)
    (srfi 1) ;; lists
    (srfi 41) ;; streams
    (srfi 158)) ;; generators
  (export
    accumulate-generated-values
    gdelete-duplicates
    genumerate
    gcompose-left
    gcompose-right
    gchoice
    generator->stream
    stream->generator)
  (include "reference/srfi-221/221-impl.scm"))
