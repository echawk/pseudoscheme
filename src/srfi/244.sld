;;; SRFI 244: multiple-value definitions.  The SRFI brings R7RS's
;;; define-values to R6RS, so this library exports (scheme base)'s
;;; define-values, which meets its specification (and so doesn't
;;; conflict with (scheme base)).  The sample implementation, an R6RS
;;; syntax-case macro, isn't needed.  (srfi 244 define-values), the R6RS
;;; (srfi :244 define-values), is 244/define-values.sld.
(define-library (srfi 244)
  (export define-values)
  (import (only (scheme base) define-values)))
