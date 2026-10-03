;;; Taylan Kammer's 64/execution.sld, with include paths pointing into
;;; ../reference/srfi-64/, and (srfi 35) and (srfi 48) replaced by
;;; (srfi 64 compat).
;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;; SPDX-License-Identifier: MIT
(define-library (srfi 64 execution)
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme complex)
   (scheme eval)
   (scheme process-context)
   (scheme read)
   (srfi 1)
   (srfi 64 compat)
   (srfi 64 source-info)
   (srfi 64 test-runner)
   (srfi 64 test-runner-simple))
  (include-library-declarations "../reference/srfi-64/execution.exports.sld")
  (include "../reference/srfi-64/execution.body.scm"))
