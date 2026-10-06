;;; SRFI 185: linear adjustable-length strings.  The portable
;;; implementation from the SRFI document, verbatim.
;;; Copyright (C) Per Bothner 2015, John Cowan 2019.  MIT licence; see
;;; LICENSE.

(define (string-append-linear! . args)
  (apply string-append
    (map (lambda (x) (if (char? x) (string x) x)) args)))

(define string-replace-linear!
  (case-lambda
    ((dst dst-start dst-end src)
     (string-replace dst src dst-start dst-end 0 (string-length src)))
    ((dst dst-start dst-end src src-start)
     (string-replace dst src dst-start dst-end src-start (string-length src)))
    ((dst dst-start dst-end src src-start src-end)
     (string-replace dst src dst-start dst-end src-start src-end))))

(define-syntax string-append!
  (syntax-rules ()
    ((_ place . args)
     (set! place (string-append-linear! place . args)))))

(define-syntax string-replace!
  (syntax-rules ()
    ((_ place . args)
     (set! place (string-replace-linear! place . args)))))
