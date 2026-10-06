;;; For the SRFI 226 sample implementation: Chez Scheme's
;;; make-ephemeron-eq-hashtable, an R6RS eq hashtable whose keys are
;;; held weakly (SBCL's weak tables are ephemeral: a value doesn't keep
;;; its key), by SRFI 126.
(define-library (srfi private srfi-226-make-ephemeron-eq-hashtable)
  (export make-ephemeron-eq-hashtable)
  (import (scheme base) (only (srfi 126) make-eq-hashtable))
  (begin
    (define (make-ephemeron-eq-hashtable) (make-eq-hashtable #f 'ephemeral-key))))
