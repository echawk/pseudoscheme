;;; (srfi 237 procedural): SRFI 237's R7RS name for its procedural
;;; layer; see 237/records.sld.
(define-library (srfi 237 procedural)
  (export
          make-record-type-descriptor record-type-descriptor?
          make-record-descriptor make-record-constructor-descriptor
          record-descriptor-rtd record-descriptor-parent
          record-descriptor? record-constructor-descriptor?
          record-constructor record-predicate record-accessor
          record-mutator)
  (import (srfi 237 records)))
