;;; SRFI 274: extended list conversion procedures.  Peter McGoron's
;;; sample implementation of this library, unmodified (a copy of
;;; reference/srfi-274/41.sld; MIT licence in reference/srfi-274/LICENSE).
;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 41)
  (import (scheme base) (scheme case-lambda)
          (srfi 274 internal)
          (except (srfi 41) list->stream)
          (prefix (only (srfi 41) list->stream) srfi-41:))
  (export list->stream)
  (begin
    (define list->stream
      (case-lambda
        ((lst) (srfi-41:list->stream lst))
        ((lst start) (srfi-41:list->stream (list-tail lst start)))
        ((lst start end)
         (argcheck! 'list->stream start end lst)
         (letrec ((loop (stream-lambda (lst start)
                        (if (= start end)
                            stream-null
                            (stream-cons (car lst)
                                         (loop (cdr lst)
                                               (+ start 1)))))))
           (loop (list-tail lst start) start)))))))
