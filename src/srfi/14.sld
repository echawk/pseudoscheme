;;; SRFI 14: character-set library.  Olin Shivers' reference
;;; implementation (reference/srfi-14.scm), MIT Scheme / scsh copyright,
;;; BSD-style; see the end of that file.
;;;
;;; The reference represents a char set as a 256-element Latin-1 string,
;;; so sets hold Latin-1 characters only: char-set:full and complements
;;; are complete over U+0000..U+00FF, and the standard sets
;;; (char-set:letter etc.) are their Latin-1 subsets.
;;;
;;; Modification to the reference code:
;;; - char-set-contains? returns #f for a character above U+00FF instead
;;;   of an index error, so SRFI 13's char-set arguments work on any
;;;   string.  Adding a non-Latin-1 character to a set is still an error.
(define-library (srfi 14)
  (export char-set? char-set= char-set<= char-set-hash
          char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
          char-set-fold char-set-unfold char-set-unfold!
          char-set-for-each char-set-map
          char-set-copy char-set
          list->char-set string->char-set list->char-set! string->char-set!
          char-set-filter ucs-range->char-set ->char-set
          char-set-filter! ucs-range->char-set!
          char-set->list char-set->string
          char-set-size char-set-count char-set-contains?
          char-set-every char-set-any
          char-set-adjoin char-set-delete char-set-adjoin! char-set-delete!
          char-set-complement char-set-union char-set-intersection
          char-set-complement! char-set-union! char-set-intersection!
          char-set-difference char-set-xor char-set-diff+intersection
          char-set-difference! char-set-xor! char-set-diff+intersection!
          char-set:lower-case char-set:upper-case char-set:title-case
          char-set:letter char-set:digit char-set:letter+digit
          char-set:graphic char-set:printing char-set:whitespace
          char-set:iso-control char-set:punctuation char-set:symbol
          char-set:hex-digit char-set:blank char-set:ascii
          char-set:empty char-set:full)
  (import (scheme base)
          (only (rnrs arithmetic bitwise) bitwise-and)
          (srfi private shim))
  (include "reference/srfi-14.scm"))
