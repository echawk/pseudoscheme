;;; SRFI 180: JSON.  Amirouche Boubekki's sample implementation
;;; (reference/srfi-180/body.scm, unmodified; MIT licence in
;;; reference/srfi-180/LICENSE).  The library form follows the sample's
;;; srfi/180.sld, with these changes: it imported (scheme text) and the
;;; sample's test library (check), neither of which the body uses; and
;;; its chibi-specific (srfi 180 helpers) is replaced by
;;; (srfi private srfi-180-helpers) (see that file).  And integer->char
;;; is shadowed by one that raises a json-error for a surrogate code
;;; point, which the body may pass it for a lone \uD800-style escape:
;;; Pseudoscheme's characters exclude surrogates, and its integer->char
;;; would raise an assertion violation, not a json-error.  Likewise
;;; string->number is shadowed by one that gives 0.0 or an infinity
;;; for a decimal number whose exponent puts it far outside the
;;; flonums' range: Pseudoscheme's computes 10^|exponent| exactly, which
;;; takes 50 seconds for 123e-10000000 (a test input).
;;;
;;; JSON objects are association lists with symbol keys, arrays are
;;; vectors, null is the symbol null, as the SRFI specifies.  Reading a
;;; byte sequence that isn't UTF-8 from a file port raises a json-error.
(define-library (srfi 180)
  (export json-number-of-character-limit
          json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-fold
          json-generator
          json-read
          json-lines-read
          json-sequence-read
          json-accumulator
          json-write)
  (import (except (scheme base) integer->char string->number)
          (rename (only (scheme base) integer->char string->number)
                  (integer->char %integer->char)
                  (string->number %string->number))
          (scheme inexact)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (srfi 145)
          (srfi private srfi-180-helpers)
          (only (srfi 151) arithmetic-shift bitwise-ior))
  (begin
    ;; S is a JSON number (see valid-number?) or a hex escape.
    (define (string->number s . radix)
      (let ((e (let loop ((i 0))
                 (cond ((= i (string-length s)) #f)
                       ((memv (string-ref s i) '(#\e #\E)) i)
                       (else (loop (+ i 1)))))))
        (if (or (pair? radix) (not e))
            (apply %string->number s radix)
            (let* ((exponent (%string->number (substring s (+ e 1) (string-length s))))
                   (mantissa (substring s 0 e))
                   (negative? (and (> (string-length mantissa) 0)
                                   (char=? (string-ref mantissa 0) #\-)))
                   (digits (string-length mantissa)))
              (cond ((not exponent) #f)
                    ((< (+ exponent digits) -1000)
                     (if negative? -0.0 0.0))
                    ((and (> (- exponent digits) 1000)
                          (%string->number mantissa)
                          (not (zero? (%string->number mantissa))))
                     (if negative? -inf.0 +inf.0))
                    (else (%string->number s)))))))

    (define (integer->char n)
      (if (<= #xD800 n #xDFFF)
          (raise (make-json-error "Lone surrogate in a \\u escape."))
          (%integer->char n))))
  (include "reference/srfi-180/body.scm"))
