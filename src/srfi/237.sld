;;; (srfi 237): SRFI 237: R6RS records (refined).  The composite
;;; library, also (srfi :237); the implementation is in
;;; 237/records.sld, which see.
(define-library (srfi 237)
  (export
          define-record-type define-record-name fields mutable
          immutable parent protocol sealed opaque nongenerative
          generative parent-rtd record-type-descriptor
          record-constructor-descriptor make-record-type-descriptor
          record-type-descriptor? make-record-descriptor
          make-record-constructor-descriptor record-descriptor-rtd
          record-descriptor-parent record-descriptor?
          record-constructor-descriptor? record-constructor
          record-predicate record-accessor record-mutator record?
          record-rtd record-type-name record-type-parent
          record-type-uid record-type-generative? record-type-sealed?
          record-type-opaque? record-type-field-names
          record-field-mutable? record-uid->rtd port-write-rtd
          port-read-rtd)
  (import (srfi 237 records)))
