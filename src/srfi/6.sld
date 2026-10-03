;;; SRFI 6: basic string ports (in R7RS (scheme base) already).
(define-library (srfi 6)
  (export open-input-string open-output-string get-output-string)
  (import (scheme base)))
