;;; For the SRFI 226 sample implementation: Chez Scheme's define-property,
;;; which is SRFI 213's here.
(define-library (srfi private srfi-226-define-property)
  (export define-property)
  (import (srfi 213)))
