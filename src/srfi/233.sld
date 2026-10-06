;;; SRFI 233: INI files.  Arvydas Silanskas's sample implementation
;;; (reference/srfi-233/233-impl.scm, unmodified; MIT licence, per the
;;; SRFI document, in reference/srfi-233/LICENSE).  The library form is
;;; the implementation's srfi/233.sld.
(define-library (srfi 233)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write))
  (export make-ini-file-generator
          make-ini-file-accumulator)
  (include "reference/srfi-233/233-impl.scm"))
