;;; SRFI 129: titlecase procedures.  John Cowan's sample implementation,
;;; unmodified (reference/srfi-129/titlemaps.scm and titlecase-impl.scm;
;;; MIT licence, per the SRFI document, in reference/srfi-129/LICENSE).
;;; The sample places the procedures in a (titlecase) library; the SRFI
;;; names it (srfi 129).
;;;
;;; SRFI 129 specifies char-title-case? and char-titlecase to be the same
;;; as R6RS's, so the library exports (rnrs unicode)'s bindings for those
;;; two (the sample's versions are defined but not exported); then
;;; (import (rnrs unicode) (srfi 129)) is no conflict for them.
;;; string-titlecase is the sample's: SRFI 129 titlecases any character
;;; that follows a caseless one and uses Unicode's multi-character
;;; titlecase mappings, which R6RS's string-titlecase (word-break based)
;;; does not, so it clashes with (rnrs unicode)'s.
(define-library (srfi 129)
  (export (rename r6:char-title-case? char-title-case?)
          (rename r6:char-titlecase char-titlecase)
          string-titlecase)
  (import (scheme base)
          (scheme char)
          (rename (only (rnrs unicode) char-title-case? char-titlecase)
                  (char-title-case? r6:char-title-case?)
                  (char-titlecase r6:char-titlecase)))
  (include "reference/srfi-129/titlemaps.scm")
  (include "reference/srfi-129/titlecase-impl.scm"))
