;;; (srfi 237 records procedural): SRFI 237's procedural layer, also
;;; (srfi :237 records procedural); see 237/records.sld.
(define-library (srfi 237 records procedural)
  (export
          make-record-type-descriptor record-type-descriptor?
          make-record-descriptor make-record-constructor-descriptor
          record-descriptor-rtd record-descriptor-parent
          record-descriptor? record-constructor-descriptor?
          record-constructor record-predicate record-accessor
          record-mutator)
  (import (srfi 237 records)))
