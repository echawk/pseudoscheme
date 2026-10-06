;;; SRFI 165: the environment monad.  Marc Nieper-Wißkirchen's sample
;;; implementation, unmodified (reference/srfi-165/165.scm; MIT licence,
;;; in reference/srfi-165/LICENSE).  The library form is the shipped
;;; reference/srfi-165/165.sld; its dependencies, SRFIs 1, 111, 125, 128
;;; and 146, are Pseudoscheme's own.
(define-library (srfi 165)
  (export make-computation-environment-variable
          make-computation-environment computation-environment-ref
          computation-environment-update
          computation-environment-update! computation-environment-copy
          make-computation computation-each computation-each-in-list
          computation-pure computation-bind computation-sequence
          computation-run computation-ask computation-local
          computation-fn computation-with computation-with!
          computation-forked computation-bind/forked
          default-computation
          define-computation-type)
  (import (scheme base)
          (srfi 1)
          (srfi 111)
          (srfi 125)
          (srfi 128)
          (srfi 146))
  (include "reference/srfi-165/165.scm"))
