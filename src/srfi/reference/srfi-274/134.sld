;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 134)
  (import (scheme base) (scheme case-lambda)
          (srfi 274 internal)
          (except (srfi 134) list->ideque)
          (prefix (only (srfi 134) list->ideque) srfi-134:))
  (export list->ideque)
  (begin
    (define list->ideque
      (case-lambda
        ((lst) (srfi-134:list->ideque lst))
        ((lst start) (srfi-134:list->ideque (list-tail lst start)))
        ((lst start end)
         (argcheck! 'list->ideque start end lst)
         (ideque-unfold (lambda (seed)
                          (= (vector-ref seed 1) end))
                        (lambda (seed)
                          (car (vector-ref seed 0)))
                        (lambda (seed)
                          (vector (cdr (vector-ref seed 0))
                                  (+ 1 (vector-ref seed 1))))
                        (vector (list-tail lst start)
                                start)))))))