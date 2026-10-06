;;; SRFI 66 tests, after the SRFI document's specification (the SRFI
;;; has no test suite).
(import (scheme base) (scheme process-context) (srfi 64) (srfi 66))

(test-begin "srfi-66")

(test-assert (u8vector? (u8vector 1 2 3)))
(test-assert (u8vector? (make-u8vector 3 0)))
(test-assert (not (u8vector? (vector 1 2 3))))
(test-assert (not (u8vector? '(1 2 3))))
(test-assert "octet vectors are bytevectors" (bytevector? (u8vector 1)))

(test-equal 3 (u8vector-length (make-u8vector 3 7)))
(test-equal '(7 7 7) (u8vector->list (make-u8vector 3 7)))
(test-equal 0 (u8vector-length (u8vector)))
(test-equal '(1 2 255) (u8vector->list (u8vector 1 2 255)))
(test-equal '(0 128 255) (u8vector->list (list->u8vector '(0 128 255))))
(test-equal '() (u8vector->list (list->u8vector '())))

(let ((v (u8vector 10 20 30)))
  (test-equal 20 (u8vector-ref v 1))
  (u8vector-set! v 1 99)
  (test-equal 99 (u8vector-ref v 1))
  (test-equal '(10 99 30) (u8vector->list v)))

(test-error (u8vector-set! (u8vector 1 2) 0 256))
(test-error (u8vector-ref (u8vector 1 2) 2))

(test-assert (u8vector=? (u8vector 1 2 3) (u8vector 1 2 3)))
(test-assert (u8vector=? (u8vector) (u8vector)))
(test-assert (not (u8vector=? (u8vector 1 2 3) (u8vector 1 2 4))))
(test-assert (not (u8vector=? (u8vector 1 2 3) (u8vector 1 2))))

(test-equal 0 (u8vector-compare (u8vector 1 2 3) (u8vector 1 2 3)))
(test-equal -1 (u8vector-compare (u8vector 1 2) (u8vector 1 2 3)))
(test-equal 1 (u8vector-compare (u8vector 9 9 9) (u8vector 1 2)))
(test-equal -1 (u8vector-compare (u8vector 1 2 3) (u8vector 1 3 0)))
(test-equal 1 (u8vector-compare (u8vector 2 0 0) (u8vector 1 9 9)))
(test-equal 0 (u8vector-compare (u8vector) (u8vector)))

(let* ((v (u8vector 1 2 3))
       (c (u8vector-copy v)))
  (test-assert (u8vector=? v c))
  (test-assert (not (eq? v c)))
  (u8vector-set! c 0 9)
  (test-equal 1 (u8vector-ref v 0)))

(let ((target (make-u8vector 5 0)))
  (u8vector-copy! (u8vector 1 2 3 4) 1 target 2 3)
  (test-equal '(0 0 2 3 4) (u8vector->list target)))

;; Overlapping copies, in both directions.
(let ((v (u8vector 1 2 3 4 5)))
  (u8vector-copy! v 0 v 1 4)
  (test-equal '(1 1 2 3 4) (u8vector->list v)))
(let ((v (u8vector 1 2 3 4 5)))
  (u8vector-copy! v 1 v 0 4)
  (test-equal '(2 3 4 5 5) (u8vector->list v)))
(let ((v (u8vector 1 2 3)))
  (u8vector-copy! v 0 v 0 0)
  (test-equal '(1 2 3) (u8vector->list v)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-66")
  (exit (if (zero? failures) 0 1)))
