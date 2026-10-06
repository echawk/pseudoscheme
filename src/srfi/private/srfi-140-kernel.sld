;;; (srfi private srfi-140-kernel): a kernel for SRFI 135's sample
;;; implementation (reference/srfi-140/135.body.scm) in which texts are
;;; simply Scheme strings, so that its textual procedures become SRFI
;;; 140's string procedures.  It stands in for SRFI 135's kernel0, with
;;; the same exports (minus the debugging ones).  Written for
;;; Pseudoscheme.
;;;
;;; Every string this kernel returns is newly allocated (subtext always
;;; copies, %string->text copies), so a result never shares storage with
;;; a string the caller might mutate.
(define-library (srfi private srfi-140-kernel)
  (export complain
          %text-length %text-ref %string->text
          N the-empty-text
          text? text-tabulate text-length text-ref subtext
          textual-concatenate)
  (import (scheme base))
  (begin
    (define (complain name . args)
      (apply error
             (string-append (symbol->string name) ": illegal arguments")
             args))

    (define N 128)
    (define the-empty-text "")

    (define (text? x) (string? x))

    (define (%text-length txt) (string-length txt))
    (define (%text-ref txt i) (string-ref txt i))

    (define (%string->text s)
      (if (string? s)
          (string-copy s)
          (complain 'string->text s)))

    (define (text-length txt)
      (if (string? txt)
          (string-length txt)
          (error "text-length: not a text" txt)))

    (define (text-ref txt i)
      (if (and (string? txt) (exact-integer? i) (<= 0 i))
          (if (< i (string-length txt))
              (string-ref txt i)
              (error "text-ref: index out of range" txt i))
          (error "text-ref: illegal arguments" txt i)))

    (define (text-tabulate proc len)
      (let ((s (make-string len)))
        (do ((i 0 (+ i 1)))
            ((= i len) s)
          (let ((c (proc i)))
            (if (char? c)
                (string-set! s i c)
                (error "text-tabulate: proc returned a non-character"
                       proc len c))))))

    (define (subtext txt start end)
      (if (and (string? txt)
               (exact-integer? start)
               (exact-integer? end)
               (<= 0 start end))
          (if (<= end (string-length txt))
              (substring txt start end)
              (error "subtext: end out of range" txt start end))
          (complain 'subtext txt start end)))

    (define (textual-concatenate texts)
      (cond ((not (list? texts))
             (complain 'textual-concatenate texts))
            ((let loop ((ts texts))
               (or (null? ts) (and (string? (car ts)) (loop (cdr ts)))))
             (apply string-append texts))
            (else (complain 'textual-concatenate texts))))))
