;;; Tests for SRFI 169, underscores in numbers: the SRFI's examples.
;;; The syntax is the reader's and string->number's; there's no library.
(import (scheme base) (scheme read) (scheme process-context) (srfi 64))
(define (r s) (read (open-input-string s)))
(test-begin "srfi-169")

(test-eqv 123 0_1_2_3)
(test-eqv 123 01_23)
(test-eqv 123 +0_123)
(test-eqv -123 -0_123)
(test-eqv 123/4567 1_2_3/4_5_6_7)
(test-eqv 123.456 0_1_23.4_5_6)
(test-eqv 123.5e6 1_2_3.5e6)
(test-eqv 12e12 1_2e1_2)
(test-eqv -123.0-1234.5678i -12_3.0_00_00-12_34.56_78i)
(test-eqv 42 #b10_10_10)
(test-eqv #o234567 #o23_45_67)
(test-eqv #xABCDEF #xAB_CD_EF)
(test-eqv -32 #x-2_0)
(test-eqv 1000000 (string->number "1_000_000"))
(test-eqv 255 (string->number "f_f" 16))

;; non-conforming
(for-each (lambda (s) (test-eqv s #f (string->number s)))
          '("0123_" "01__23" "+_0123" "-0123_" "1_2_3/_4_5_6_7" "0123_.456"
            "0123._456" "123.5_e6" "123.5e_6" "12_e12" "#x-_2" "#d_45_67_89"
            "#e#x1234_" "-12_3.0_00_00-12_34.56_78_i"))
(test-assert "_0123 is a symbol" (symbol? (r "_0123")))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-169")
  (exit (if (zero? failures) 0 1)))
