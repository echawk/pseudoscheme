;;; Taylan Kammer's 64/test-runner.sld, with include paths pointing
;;; into ../reference/srfi-64/, and make-parameter replaced by
;;; settable-parameter from (srfi 64 compat), whose parameters can be
;;; set by calling them with an argument, as the body does.
;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;; SPDX-License-Identifier: MIT
(define-library (srfi 64 test-runner)
  (import
   (except (scheme base) make-parameter)
   (rename (only (srfi 64 compat) settable-parameter)
           (settable-parameter make-parameter))
   (scheme case-lambda)
   (srfi 1))
  (include-library-declarations "../reference/srfi-64/test-runner.exports.sld")
  (include "../reference/srfi-64/test-runner.body.scm"))
