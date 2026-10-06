;;; (srfi private srfi-57-labels): expand-time list operations on field
;;; labels for (srfi 57); see 57.sld.  Labels compare with
;;; free-identifier=?, which is what the reference implementation's
;;; if-free= and top:if-free= test.
(define-library (srfi private srfi-57-labels)
  (export label=? label-member? label-dedupe label-filter label-lookup)
  (import (scheme base)
          (only (rnrs syntax-case) identifier? free-identifier=?
                syntax->datum))
  (begin
    (define (label=? a b)
      (if (and (identifier? a) (identifier? b))
          (free-identifier=? a b)
          (equal? (syntax->datum a) (syntax->datum b))))

    (define (label-member? x lst)
      (let loop ((lst lst))
        (cond ((null? lst) #f)
              ((label=? x (car lst)) #t)
              (else (loop (cdr lst))))))

    ;; LST without later duplicates, in order.
    (define (label-dedupe lst)
      (let loop ((lst lst) (done '()))
        (cond ((null? lst) (reverse done))
              ((label-member? (car lst) done) (loop (cdr lst) done))
              (else (loop (cdr lst) (cons (car lst) done))))))

    ;; The elements of LST for which (KEEP? x) is true.
    (define (label-filter keep? lst)
      (let loop ((lst lst) (acc '()))
        (cond ((null? lst) (reverse acc))
              ((keep? (car lst)) (loop (cdr lst) (cons (car lst) acc)))
              (else (loop (cdr lst) acc)))))

    ;; The cdr of the first (label . value) in ALIST whose label is
    ;; LABEL, or FAIL.
    (define (label-lookup label alist fail)
      (let loop ((alist alist))
        (cond ((null? alist) fail)
              ((label=? label (car (car alist))) (cdr (car alist)))
              (else (loop (cdr alist))))))))
