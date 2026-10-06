;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 internal)
  (import (scheme base))
  (export argcheck!)
  (begin
    (define (length>=? im-list end)
      (or (zero? end)
          (and (positive? end)
               (pair? im-list)
               (length>=? (cdr im-list) (- end 1)))))
    (define (argcheck! who start end im-list)
      (cond
        ((not (exact-integer? start))
         (error who "start must be an exact integer"))
        ((not (exact-integer? end))
         (error who "end must be an exact integer"))
        ((not (<= 0 start end))
         (error who
                "invariant: (<= 0 start end)"
                start end))
        ((not (length>=? im-list end))
         (error who
                "list is not long enough"
                im-list
                end))))))