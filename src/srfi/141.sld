;;; SRFI 141: integer division.  Taylor R. Campbell's reference
;;; implementation, unmodified (reference/srfi-141/srfi-141-impl.scm).
;;; R7RS's floor/ and truncate/ families already meet SRFI 141, so the
;;; library exports those very bindings (the implementation's own
;;; definitions of them stay private), and (import (scheme base)
;;; (srfi 141)) does not conflict.
(define-library (srfi 141)
  (export ceiling/ ceiling-quotient ceiling-remainder
          round/ round-quotient round-remainder
          euclidean/ euclidean-quotient euclidean-remainder
          balanced/ balanced-quotient balanced-remainder
          (rename r7:floor/ floor/)
          (rename r7:floor-quotient floor-quotient)
          (rename r7:floor-remainder floor-remainder)
          (rename r7:truncate/ truncate/)
          (rename r7:truncate-quotient truncate-quotient)
          (rename r7:truncate-remainder truncate-remainder))
  (import (except (scheme base)
                  floor/ floor-quotient floor-remainder
                  truncate/ truncate-quotient truncate-remainder
                  exact-integer?)
          (prefix (only (scheme base)
                        floor/ floor-quotient floor-remainder
                        truncate/ truncate-quotient truncate-remainder)
                  r7:))
  (include "reference/srfi-141/srfi-141-impl.scm"))
