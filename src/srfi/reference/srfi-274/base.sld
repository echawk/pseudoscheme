;;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;;; SPDX-License-Identifier: MIT
(define-library (srfi 274 base)
  (import (except (scheme base)
                  list-copy list->string list->vector)
          (scheme case-lambda)
          (srfi 274 internal)
          (prefix (only (scheme base)
                        list-copy list->string list->vector)
                  r7rs:))
  (export list-copy list->string list->vector)
  (begin
    (define list-copy
      (case-lambda
        ((lst) (r7rs:list-copy lst))
        ((lst start) (r7rs:list-copy (list-tail lst start)))
        ((lst start end)
         (argcheck! 'list-copy start end lst)
         (let ((returned (make-list (- end start))))
           (do ((head returned (cdr head))
                (i 0 (+ i 1))
                (lst (list-tail lst start) (cdr lst)))
               ((null? head) returned)
             (set-car! head (car lst)))))))
    (define list->string
      (case-lambda
        ((lst) (r7rs:list->string lst))
        ((lst start) (r7rs:list->string (list-tail lst start)))
        ((lst start end)
         ;; NOTE: This uses `list-copy` as an intermediary, because
         ;; `string-ref` and `string-set!` may be inefficient.
         ;; Efficient generators for strings are in SRFIs.
         (r7rs:list->string (list-copy lst start end)))))
    (define list->vector
      (case-lambda
        ((lst) (r7rs:list->vector lst))
        ((lst start) (r7rs:list->vector (list-tail lst start)))
        ((lst start end)
         (argcheck! 'list->vector start end lst)
         (do ((returned (make-vector (- end start)))
              (i 0 (+ i 1))
              (lst (list-tail lst start) (cdr lst)))
             ((= i (- end start)) returned)
           (vector-set! returned i (car lst))))))))