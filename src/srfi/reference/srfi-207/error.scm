(define-record-type <bytestring-error>
  (raw-bytestring-error message irritants)
  bytestring-error?
  (message bytestring-error-message)
  (irritants bytestring-error-irritants))

(define (bytestring-error message . irritants)
  (raise (raw-bytestring-error message irritants)))
