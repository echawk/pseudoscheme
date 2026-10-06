;;; (srfi private srfi-201-match): Alex Shinn's portable hygienic
;;; pattern matcher (match.scm from chibi-scheme, public domain;
;;; reference/srfi-201/match.scm, unmodified), the SRFI 200-style
;;; matcher that SRFI 201 and SRFI 202 are built on (their sample
;;; implementations use Guile's (ice-9 match), which is this matcher).
;;; The library form follows chibi's (chibi match).  The record
;;; patterns ($ and @) need the host's is-a?, slot-ref and slot-set!,
;;; and aren't supported.
(define-library (srfi private srfi-201-match)
  (export match match-lambda match-lambda* match-let match-letrec match-let*)
  (import (scheme base))
  (include "../reference/srfi-201/match.scm"))
