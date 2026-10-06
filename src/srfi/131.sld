;;; SRFI 131: ERR5RS record syntax (reduced).  Marc Nieper-Wißkirchen's
;;; implementation on SRFI 136, from the SRFI 136 repository, unmodified
;;; (reference/srfi-131/131.scm; MIT licence in
;;; reference/srfi-131/LICENSE); the library form is that repository's
;;; srfi/131.sld.  So SRFI 131 record types are SRFI 136 ones.  The
;;; SRFI 131 repository's own srfi-131.sld (William D Clinger's, on SRFI
;;; 99's procedural layer; kept as reference/srfi-131/srfi-131-clinger.sld)
;;; isn't used: its define-record-type-helper1 refers to rev-specs where
;;; it binds revspecs, so no definition expands.
;;;
;;; define-record-type is a different binding from (scheme base)'s, so
;;; import (except (scheme base) define-record-type) with this library.
(define-library (srfi 131)
  (export define-record-type)
  (import (except (scheme base) define-record-type)
          (rename (srfi 136) (define-record-type define-record-type/136)))
  (include "reference/srfi-131/131.scm"))
