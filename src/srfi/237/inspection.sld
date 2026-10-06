;;; (srfi 237 inspection): SRFI 237's R7RS name for its inspection
;;; layer; see 237/records.sld.
(define-library (srfi 237 inspection)
  (export
          record? record-rtd record-type-name record-type-parent
          record-type-uid record-type-generative? record-type-sealed?
          record-type-opaque? record-type-field-names
          record-field-mutable? record-uid->rtd)
  (import (srfi 237 records)))
