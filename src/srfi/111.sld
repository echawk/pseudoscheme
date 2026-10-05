;;; SRFI 111: boxes.  SRFI 111's boxes are SRFI 195's single-value
;;; boxes, so this library re-exports (srfi 195)'s box, box?, unbox and
;;; set-box!, as SRFI 195's sample implementation does.  A box made by
;;; either library is a box to both.
(define-library (srfi 111)
  (export box box? unbox set-box!)
  (import (only (srfi 195) box box? unbox set-box!)))
