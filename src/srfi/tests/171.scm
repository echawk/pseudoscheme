;;; Tests for SRFI 171: the reference test suite
;;; (reference/srfi-171/tests.scm, included unmodified; it needs compose,
;;; which tests-r7rs.scm defines for chibi as below), plus tests of the
;;; reducers, tlog and (srfi 171 meta).
(import (scheme base) (scheme char) (scheme read) (scheme write)
        (scheme process-context)
        (srfi 1) (srfi 69) (srfi 64) (srfi 171) (srfi 171 meta))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))

(test-begin "srfi-171")
(include "../reference/srfi-171/tests.scm")

(test-begin "reducers")
(test-equal '(3 2 1) (list-transduce (tmap values) reverse-rcons '(1 2 3)))
(test-equal 3 (list-transduce (tmap values) rcount '(1 2 3)))
(test-equal 3 (list-transduce (tmap values) (rany (lambda (x) (and (odd? x) x))) '(2 3 4)))
(test-equal #f (list-transduce (tmap values) (rany odd?) '(2 4)))
(test-equal #t (list-transduce (tmap values) (revery odd?) '(1 3)))
(test-equal #f (list-transduce (tmap values) (revery odd?) '(1 2 3)))
(test-equal 6 (bytevector-u8-transduce (tmap values) + (bytevector 1 2 3)))
(test-equal '(2 1 0) (list-transduce (tmap values) rcons
                                     (list-transduce (ttake 3) reverse-rcons
                                                     (iota 10))))
(test-equal '(0 1 2) (list-transduce (ttake 3) rcons (iota 10)))
(test-equal '((0 . a) (1 . b)) (list-transduce (tenumerate) rcons '(a b)))
(test-equal 2 (list-transduce (tdelete-duplicates string-ci=?) rcount
                              '("a" "A" "b" "B")))
(test-end "reducers")

(test-begin "tlog")
(test-equal "1 2 "
  (let ((port (open-output-string)))
    (list-transduce (tlog (lambda (result input)
                            (write input port) (display " " port)))
                    rcons '(1 2))
    (get-output-string port)))
(test-end "tlog")

(test-begin "meta")
(test-assert (reduced? (reduced 1)))
(test-equal 1 (unreduce (reduced 1)))
(test-assert (reduced? (ensure-reduced 1)))
(test-equal 1 (unreduce (ensure-reduced (reduced 1))))
(test-equal 6 (list-reduce + 0 '(1 2 3)))
(test-equal 3 (list-reduce (lambda (acc x) (if (> x 2) (reduced acc) (+ acc x)))
                           0 '(1 2 3 4)))
(test-equal 6 (vector-reduce + 0 #(1 2 3)))
(test-equal '(#\b #\a) (string-reduce (lambda (a c) (cons c a)) '() "ab"))
(test-equal 3 (bytevector-u8-reduce + 0 (bytevector 1 2)))
(test-equal 6 (port-reduce + 0 read (open-input-string "1 2 3")))
(test-equal 6 (let ((xs '(1 2 3)))
                (generator-reduce + 0
                                  (lambda ()
                                    (if (null? xs)
                                        (eof-object)
                                        (let ((x (car xs)))
                                          (set! xs (cdr xs)) x))))))
(test-end "meta")

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-171")
  (exit (if (zero? failures) 0 1)))
