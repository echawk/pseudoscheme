;;; (srfi 237 records syntactic): SRFI 237's syntactic layer, also
;;; (srfi :237 records syntactic); see 237/records.sld.
(define-library (srfi 237 records syntactic)
  (export
          define-record-type define-record-name fields mutable
          immutable parent protocol sealed opaque nongenerative
          generative parent-rtd record-type-descriptor
          record-constructor-descriptor)
  (import (srfi 237 records)))
