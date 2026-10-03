;;; SRFI 64: a Scheme API for test suites.  Taylan Kammer's
;;; implementation (srfi-64/contrib/taylan.kammer, MIT license; after
;;; Per Bothner's reference implementation).  This file is that
;;; implementation's 64.sld, but for test-runner-create (below).
;;; The component libraries, (srfi 64 test-runner),
;;; (srfi 64 test-runner-simple), (srfi 64 source-info) and
;;; (srfi 64 execution), are in 64/; their *.body.scm and *.exports.sld
;;; files are in reference/srfi-64/, unmodified.  The changes, in
;;; 64/*.sld only: (srfi 35) and (srfi 48) are replaced by
;;; (srfi 64 compat), and test-runner.sld's make-parameter by compat's
;;; settable-parameter; see 64/compat.sld for why.
;;;
;;; test-runner-create: (srfi 64 test-runner-simple) installs the
;;; default test-runner-factory when its body runs, but psyntax runs a
;;; library's body only when a program refers to one of its variables.
;;; A program that calls test-runner-create before test-begin may refer
;;; to none, and would then find no factory.  So test-runner-create is
;;; wrapped here to install the simple runner's factory first if need be.
;;;
;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;; SPDX-License-Identifier: MIT

(define-library (srfi 64)
  (import
   (scheme base)
   (except (srfi 64 test-runner) test-runner-create)
   (prefix (only (srfi 64 test-runner) test-runner-create) %)
   (srfi 64 test-runner-simple)
   (srfi 64 execution))
  (export
   ;; Execution
   test-begin test-end test-group test-group-with-cleanup

   test-skip test-expect-fail
   test-match-name test-match-nth
   test-match-all test-match-any

   test-assert test-eqv test-eq test-equal test-approximate
   test-error test-read-eval-string

   test-apply test-with-runner

   test-exit

   ;; Test runner
   test-runner-null test-runner? test-runner-reset

   test-result-alist test-result-alist!
   test-result-ref test-result-set!
   test-result-remove test-result-clear

   test-runner-pass-count
   test-runner-fail-count
   test-runner-xpass-count
   test-runner-xfail-count
   test-runner-skip-count

   test-runner-test-name

   test-runner-group-path
   test-runner-group-stack

   test-runner-aux-value test-runner-aux-value!

   test-result-kind test-passed?

   test-runner-on-test-begin test-runner-on-test-begin!
   test-runner-on-test-end test-runner-on-test-end!
   test-runner-on-group-begin test-runner-on-group-begin!
   test-runner-on-group-end test-runner-on-group-end!
   test-runner-on-final test-runner-on-final!
   test-runner-on-bad-count test-runner-on-bad-count!
   test-runner-on-bad-end-name test-runner-on-bad-end-name!

   test-runner-factory test-runner-create
   test-runner-current test-runner-get

   ;; Simple test runner
   test-runner-simple
   test-on-group-begin-simple test-on-group-end-simple test-on-final-simple
   test-on-test-begin-simple test-on-test-end-simple
   test-on-bad-count-simple test-on-bad-end-name-simple
   )
  (begin
    (define (test-runner-create)
      (if (not (test-runner-factory))
          (test-runner-factory test-runner-simple))
      (%test-runner-create))))
