;;; SRFI 98: environment variables.
(define-library (srfi 98)
  (export get-environment-variable get-environment-variables)
  (import (scheme process-context)))
