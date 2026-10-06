;;; (srfi 237 port): SRFI 237's R7RS name for (srfi 237 records
;;; ports); see 237/records/ports.sld.
(define-library (srfi 237 port)
  (export
          port-write-rtd port-read-rtd)
  (import (srfi 237 records)))
