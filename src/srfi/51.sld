;;; SRFI 51: handling rest list.  Joo ChurlSoo's implementation,
;;; unmodified from the SRFI document (reference/srfi-51/srfi-51.scm;
;;; MIT licence, per the SRFI document, in reference/srfi-51/LICENSE).
(define-library (srfi 51)
  (export rest-values arg-and arg-ands err-and err-ands arg-or arg-ors
          err-or err-ors)
  (import (scheme base)
          (only (srfi 1) every append-reverse))
  (include "reference/srfi-51/srfi-51.scm"))
