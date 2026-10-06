;;; (srfi 140 istrings): the procedures that SRFI 140 says return
;;; istrings where R7RS or SRFI 13 return mutable strings, in their
;;; istring-returning versions (as in (srfi 140)).  (srfi 140 mstrings)
;;; exports the same names returning mutable strings.  See ../140.sld.
(define-library (srfi 140 istrings)
  (export
          string list->string vector->string utf8->string utf16->string
          utf16be->string utf16le->string string-upcase string-downcase
          string-foldcase string-titlecase string-append substring string-map
          string-tabulate string-unfold string-unfold-right
          reverse-list->string string-take string-drop string-take-right
          string-drop-right string-pad string-pad-right string-trim
          string-trim-right string-trim-both string-replace string-concatenate
          string-concatenate-reverse string-join string-filter string-remove
          string-reverse string-repeat xsubstring string-map-index)
  (import (only (srfi 140)
                string list->string vector->string utf8->string
                utf16->string utf16be->string utf16le->string string-upcase
                string-downcase string-foldcase string-titlecase string-append
                substring string-map string-tabulate string-unfold
                string-unfold-right reverse-list->string string-take
                string-drop string-take-right string-drop-right string-pad
                string-pad-right string-trim string-trim-right
                string-trim-both
                string-replace string-concatenate string-concatenate-reverse
                string-join string-filter string-remove string-reverse
                string-repeat xsubstring string-map-index)))
