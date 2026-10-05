;;; SRFI 235: combinators.  The SRFI's sample implementation (John
;;; Cowan, Arvydas Silanskas), unmodified (reference/srfi-235/235-impl.scm;
;;; MIT licence, per the SRFI document, in reference/srfi-235/LICENSE).
;;; The library form follows the shipped srfi/235.sld.
(define-library (srfi 235)
  (export constantly complement swap flip on-left on-right
          conjoin disjoin each-of all-of any-of on
          left-section right-section apply-chain
          arguments-drop arguments-drop-right arguments-take arguments-take-right
          group-by
          begin-procedure if-procedure when-procedure unless-procedure
          value-procedure case-procedure and-procedure eager-and-procedure
          or-procedure eager-or-procedure funcall-procedure loop-procedure
          while-procedure until-procedure
          always never boolean)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1))
  (include "reference/srfi-235/235-impl.scm"))
