;;; SRFI 274: extended list conversion procedures.  Peter McGoron's
;;; sample implementation of this library, unmodified (a copy of
;;; reference/srfi-274/158.sld; MIT licence in reference/srfi-274/LICENSE).
;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 158)
  (import (scheme base) (scheme case-lambda)
          (srfi 274 internal)
          (prefix (only (srfi 158) list->generator) srfi-158:))
  (export list->generator)
  (begin
    (define list->generator
      (case-lambda
        ((lst) (srfi-158:list->generator lst))
        ((lst start) (srfi-158:list->generator (list-tail lst start)))
        ((lst start end)
         (argcheck! 'list->generator start end lst)
         (let ((lst (list-tail lst start)))
           (lambda ()
             (if (= start end)
                 (eof-object)
                 (let ((el (car lst)))
                   (set! lst (cdr lst))
                   (set! start (+ start 1))
                   el)))))))))
