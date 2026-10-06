;;; SRFI 99: ERR5RS records.  William D Clinger's reference
;;; implementation (reference/srfi-99/srfi-99.sls; licence, from the SRFI
;;; document, in reference/srfi-99/LICENSE), which is built on the R6RS
;;; procedural and inspection layers, so SRFI 99 record types are R6RS
;;; record types.  Its libraries are (srfi 99 records),
;;; (srfi 99 records procedural), (srfi 99 records inspection) and
;;; (srfi 99 records syntactic), in 99/; each includes its library's
;;; body, split unmodified out of srfi-99.sls into reference/srfi-99/.
;;; The (err5rs records ...) aliases aren't provided.
;;;
;;; define-record-type is a different binding from (scheme base)'s, so
;;; import (except (scheme base) define-record-type) with this library.
(define-library (srfi 99)
  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?

          make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator

          define-record-type)
  (import (srfi 99 records)))
