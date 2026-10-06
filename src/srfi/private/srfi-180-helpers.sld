;;; (srfi private srfi-180-helpers): what SRFI 180's sample
;;; implementation imports from its (srfi 180 helpers), written for
;;; Pseudoscheme: that library is chibi-specific (chibi ast, chibi
;;; regexp; reference/srfi-180/helpers.sld).
;;;
;;; - (%read-error? x): the sample guards each read-char with it, to turn
;;;   a failure to read (a read error, or here a Lisp stream decoding
;;;   error on bytes that aren't UTF-8) into a json-error.  Any condition
;;;   raised by read-char is such a failure, so it is always true.
;;; - (valid-number? string): whether STRING is a JSON number, by the
;;;   same SRE as the original, with SRFI 115, except that its digit
;;;   class, numeric, is (/ "09"): SRFI 115's numeric is every Unicode
;;;   decimal digit, which would admit, e.g., a fullwidth 1 (U+FF11).
(define-library (srfi private srfi-180-helpers)
  (export %read-error? valid-number?)
  (import (scheme base)
          (srfi 115))
  (begin
    (define (%read-error? x) #t)

    (define json-number
      (regexp '(seq
                (? #\-)
                (or #\0 (seq (/ "19")
                             (* (/ "09"))))
                (? (seq #\. (+ (/ "09"))))
                (? (seq (or #\e #\E)
                        (? (or #\- #\+))
                        (+ (/ "09")))))))

    (define (valid-number? string)
      (and (regexp-matches json-number string) #t))))
