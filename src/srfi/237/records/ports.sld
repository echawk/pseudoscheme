;;; (srfi 237 records ports), also (srfi :237 records ports): SRFI 237's
;;; reading and writing of records, which the sample implementation
;;; doesn't support (reference/srfi-237/records-ports.sls raises an
;;; implementation restriction when the library is instantiated).  Here
;;; port-read-rtd and port-write-rtd are procedures that raise it when
;;; called, so that importing the composite libraries is harmless.
(define-library (srfi 237 records ports)
  (export port-write-rtd port-read-rtd)
  (import (rnrs base) (rnrs exceptions) (rnrs conditions))
  (begin
    (define (unsupported who)
      (raise
       (condition
        (make-implementation-restriction-violation)
        (make-who-condition who)
        (make-message-condition "reading and writing of records not supported by the sample implementation"))))
    (define (port-write-rtd . args) (unsupported 'port-write-rtd))
    (define (port-read-rtd . args) (unsupported 'port-read-rtd))))
