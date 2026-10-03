;;; Taylan Kammer's 64/source-info.sld, with include paths pointing
;;; into ../reference/srfi-64/.  (No source locations are recorded:
;;; the body's cond-expand takes its portable `else' branches.)
;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;; SPDX-License-Identifier: MIT
(define-library (srfi 64 source-info)
  (import
   (rnrs syntax-case (6))
   (scheme base)
   (srfi 64 test-runner))
  (export source-info set-source-info!)
  (include "../reference/srfi-64/source-info.body.scm"))
