;;; (srfi 237 syntactic): SRFI 237's R7RS name for its syntactic
;;; layer; see 237/records.sld.
(define-library (srfi 237 syntactic)
  (export
          define-record-type define-record-name fields mutable
          immutable parent protocol sealed opaque nongenerative
          generative parent-rtd record-type-descriptor
          record-constructor-descriptor)
  (import (srfi 237 records)))
