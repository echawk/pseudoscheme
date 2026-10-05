;;; SRFI 34: exception handling for programs (R7RS's with-exception-handler,
;;; guard and raise, which extend SRFI 34's).
(define-library (srfi 34)
  (export with-exception-handler guard raise)
  (import (scheme base)))
