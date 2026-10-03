;;; Taylan Kammer's 64/test-runner-simple.sld, with include paths
;;; pointing into ../reference/srfi-64/, and (srfi 48) replaced by
;;; (srfi 64 compat), which supplies the format it needs.
;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;; SPDX-License-Identifier: MIT
(define-library (srfi 64 test-runner-simple)
  (import
   (scheme base)
   (scheme file)
   (scheme write)
   (srfi 64 compat)
   (srfi 64 test-runner))
  (include-library-declarations "../reference/srfi-64/test-runner-simple.exports.sld")
  (include "../reference/srfi-64/test-runner-simple.body.scm"))
