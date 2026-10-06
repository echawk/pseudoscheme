;;; SRFI 5: a compatible let form with signatures and rest arguments.
;;; Andy Gaynor's syntax-rules implementation (the 2003 revision), from
;;; the SRFI document, unmodified (reference/srfi-5/srfi-5.scm; SRFI
;;; copyright notice in reference/srfi-5/LICENSE).  As its header asks,
;;; R7RS's let is supplied to it as standard-let.
;;;
;;; The exported let is a different binding from (scheme base)'s, so
;;; import (except (scheme base) let) with this library.
(define-library (srfi 5)
  (export let)
  (import (except (scheme base) let)
          (rename (only (scheme base) let) (let standard-let)))
  (include "reference/srfi-5/srfi-5.scm"))
