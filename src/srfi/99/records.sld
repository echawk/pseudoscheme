;;; (srfi 99 records): SRFI 99's composite library, as in William D
;;; Clinger's reference implementation (reference/srfi-99/srfi-99.sls).
;;; define-record-type is a different binding from (scheme base)'s, so
;;; import (except (scheme base) define-record-type) with this library.
(define-library (srfi 99 records)
  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?

          make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator

          define-record-type)
  (import (srfi 99 records inspection)
          (srfi 99 records procedural)
          (srfi 99 records syntactic)))
