;;; SRFI 225: dictionaries.  Arvydas Silanskas's sample implementation
;;; (MIT licence, per the SRFI document, in reference/srfi-225/LICENSE).
;;; The library files are the shipped srfi/225.sld (this file) and
;;; srfi/225/*.sld (225/), with include paths pointing to the shared
;;; .scm files, unmodified, in reference/srfi-225/.  One change, in
;;; 225/default-impl.sld: the default dict->generator collects the
;;; entries with one dict-for-each up front, because the sample's
;;; coroutine re-enters continuations, which Pseudoscheme can't.
;;;
;;; Every optional DTO is present here: srfi-69-dto, hash-table-dto
;;; (SRFI 125), srfi-126-dto, mapping-dto and hash-mapping-dto
;;; (SRFI 146), besides the alist DTOs.

(define-library
  (srfi 225)

  (import
    (scheme base)
    (srfi 1)
    (srfi 128)
    (srfi 225 core)
    (srfi 225 default-impl)
    (srfi 225 indexes))

  (include-library-declarations "reference/srfi-225/core-exports.scm")
  (include-library-declarations "reference/srfi-225/indexes-exports.scm")
  (export make-dto)

  ;; common implementations
  (import (srfi 225 alist-impl))
  (export
    make-alist-dto
    eqv-alist-dto
    equal-alist-dto)

  ;; library-dependent DTO exports
  ;; and implementations
  ;;
  ;;srfi-69-dto
  ;;hash-table-dto
  ;;srfi-126-dto
  ;;mapping-dto
  ;;hash-mapping-dto

  (cond-expand
    ((library (srfi 69))
     (import (srfi 225 srfi-69-impl))
     (export srfi-69-dto))
    (else))

  (cond-expand
    ((library (srfi 125))
     (import (srfi 225 srfi-125-impl))
     (export hash-table-dto))
    (else))

  (cond-expand
    ((library (srfi 126))
     (import (srfi 225 srfi-126-impl))
     (export srfi-126-dto))
    (else))

  (cond-expand
    ((and (library (srfi 146))
          (library (srfi 146 hash)))
     (import (srfi 225 srfi-146-impl)
             (srfi 225 srfi-146-hash-impl))
     (export mapping-dto
             hash-mapping-dto))
    (else)))
