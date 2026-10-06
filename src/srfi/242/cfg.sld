;;; SRFI 242's (srfi 242 cfg), (srfi :242 cfg) in R6RS: see 242.sld.
;;; After reference/srfi-242/:242/cfg.sls, which re-exports (srfi cfg).
(define-library (srfi 242 cfg)
  (export cfg call execute finally halt label* labels
          bind permute
          define-cfg-label define-cfg-label*
          define-cfg-syntax define-cfg-syntax*)
  (import (srfi private srfi-242-cfg)))
