;;; Tests for SRFI 129: the sample implementation's test file
;;; (reference/srfi-129/titlecase-test.scm), converted from the Chicken
;;; test egg and \u escapes to SRFI 64 and R7RS \x escapes, plus checks
;;; of the sample's char-title-case? and char-titlecase against R6RS's.
(import (scheme base) (scheme char) (scheme process-context)
        (srfi 64) (srfi 129))
(test-begin "srfi-129")

(test-group "titlecase/predicate"
  (test-assert (char-title-case? #\x01C5))
  (test-assert (char-title-case? #\x1FFC))
  (test-assert (not (char-title-case? #\Z)))
  (test-assert (not (char-title-case? #\z))))

(test-group "titlecase/char"
  (test-eqv #\x01C5 (char-titlecase #\x01C4))
  (test-eqv #\x01C5 (char-titlecase #\x01C6))
  (test-eqv #\Z (char-titlecase #\Z))
  (test-eqv #\Z (char-titlecase #\z)))

(define Floo "\xFB02;oo")
(define Floo-bar "\xFB02;oo bar")
(define Baffle "Ba\xFB04;e")
(define LJUBLJANA "\x01C7;ub\x01C7;ana")
(define Ljubljana "\x01C8;ub\x01C9;ana")
(define ljubljana "\x01C9;ub\x01C9;ana")

(test-group "titlecase/string"
  (test-equal "\x01C5;" (string-titlecase "\x01C5;"))
  (test-equal "\x01C5;" (string-titlecase "\x01C4;"))
  (test-equal "Ss" (string-titlecase "\x00DF;"))
  (test-equal "Xi\x0307;" (string-titlecase "x\x0130;"))
  (test-equal "\x1F88;" (string-titlecase "\x1F80;"))
  (test-equal "\x1F88;" (string-titlecase "\x1F88;"))
  (test-equal "Bar Baz" (string-titlecase "bAr baZ"))
  (test-equal "Floo" (string-titlecase "floo"))
  (test-equal "Floo" (string-titlecase "FLOO"))
  (test-equal "Floo" (string-titlecase Floo))
  (test-equal "Floo Bar" (string-titlecase "floo bar"))
  (test-equal "Floo Bar" (string-titlecase "FLOO BAR"))
  (test-equal "Floo Bar" (string-titlecase Floo-bar))
  (test-equal Baffle (string-titlecase Baffle))
  (test-equal Ljubljana (string-titlecase LJUBLJANA))
  (test-equal Ljubljana (string-titlecase Ljubljana))
  (test-equal Ljubljana (string-titlecase ljubljana)))

;; From the SRFI document's rationale
(test-equal "Floo Powder" (string-titlecase "\xFB02;oo powDER"))
(test-equal "" (string-titlecase ""))
(test-equal "Don'T" (string-titlecase "don't"))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-129")
  (exit (if (zero? failures) 0 1)))
