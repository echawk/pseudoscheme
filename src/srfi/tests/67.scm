;;; Tests for SRFI 67: the reference confidence test
;;; (reference/srfi-67/examples.scm, which needs SRFI 42), run silently,
;;; plus the examples from the SRFI document.
(import (except (scheme base) newline)
        (except (scheme write) display write)
        (scheme char) (scheme complex) (scheme cxr) (scheme inexact)
        (scheme process-context)
        (srfi 27) (srfi 42) (srfi 64) (srfi 67))

;; examples.scm prints every check; silence it.
(define (display . args) #f)
(define (write . args) #f)
(define (newline . args) #f)
(define (pretty-write . args) #f)
(define exact->inexact inexact)   ; R5RS name examples.scm uses

(test-begin "srfi-67")

(include "../reference/srfi-67/examples.scm")

(test-assert "reference examples ran" (> my-check-correct 90000))
(test-equal "reference examples: none wrong" 0 my-check-wrong)

;; From the SRFI document.
(test-equal -1 (integer-compare 1 2))
(test-equal 0 (string-compare "abc" "abc"))
(test-equal 1 (char-compare-ci #\B #\a))
(test-equal -1 (list-compare '(1 2) '(1 3)))
(test-equal -1 (list-compare '(1) '(1 2)))
(test-equal 1 (vector-compare '#(1 2) '#(3)))
(test-equal -1 (vector-compare-as-list '#(1 2) '#(3)))
(test-equal -1 (default-compare '() #t))
(test-equal #t (<? integer-compare 1 2))
(test-equal #t ((<? integer-compare) 1 2))
(test-equal #t (</<? integer-compare 1 2 3))
(test-equal #t (chain<=? real-compare 1 1 2.5))
(test-equal #t (pairwise-not=? integer-compare 1 3 2))
(test-equal #f (pairwise-not=? integer-compare 1 3 1))
(test-equal 1 (min-compare integer-compare 3 1 2))
(test-equal 3 (max-compare integer-compare 3 1 2))
(test-equal 'less (if3 (integer-compare 1 2) 'less 'equal 'greater))
(test-equal 'yes (if<? (integer-compare 1 2) 'yes 'no))
(test-equal -1 ((compare-by< <) 1 2))
(test-equal 0 ((compare-by=/< = <) 2 2))
(test-equal -1 (refine-compare (integer-compare 1 1) (string-compare "a" "b")))
(test-equal 1 (pair-compare '(1 . 3) '(1 . 2)))
(test-error (integer-compare 1 'a))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-67")
  (exit (if (zero? failures) 0 1)))
