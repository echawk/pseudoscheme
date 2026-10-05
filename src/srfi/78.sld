;;; SRFI 78: lightweight testing.  Sebastian Egner's reference
;;; implementation, unmodified (reference/srfi-78/check.scm, MIT
;;; licence in its header).  check-ec is written on SRFI 42's
;;; comprehensions: with (srfi 42) available it works; without, using
;;; check-ec is an error at run time (the rest of the library works).
(define-library (srfi 78)
  (export check check-ec check-report check-set-mode! check-reset!
          check-passed?)
  (import (scheme base) (scheme cxr) (scheme write))
  (cond-expand
   ((library (srfi 42))
    (import (only (srfi 42) first-ec :let nested :)))
   (else
    (begin
      (define-syntax first-ec
        (syntax-rules ()
          ((_ . args) (error "check-ec needs SRFI 42, which is not available")))))))
  (include "reference/srfi-78/check.scm"))
