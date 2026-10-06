;;; (srfi 140 char): (scheme char).  Its string procedures (string-upcase,
;;; string-ci=?, ...) already meet SRFI 140's specification, so this is
;;; (scheme char) itself.  See ../140.sld.
(define-library (srfi 140 char)
  (export
          char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
          char-downcase char-foldcase char-lower-case? char-numeric?
          char-upcase char-upper-case? char-whitespace? digit-value
          string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
          string-downcase string-foldcase string-upcase)
  (import (scheme char)))
