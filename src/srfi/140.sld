;;; SRFI 140: immutable strings.  The SRFI says a portable implementation
;;; is not possible, and its repository has only the test suite; this
;;; one is written for Pseudoscheme, with most procedures taken from
;;; William D Clinger's SRFI 135 sample implementation (texts) running on
;;; a kernel whose texts are plain strings: see
;;; private/srfi-140-strings.sld, private/srfi-140-texts.sld and
;;; private/srfi-140-kernel.sld, and reference/srfi-140 (MIT licence).
;;; Where R7RS's own procedure meets SRFI 140's specification (string,
;;; string-append, substring, string-upcase, string=?, ...)
;;; this library exports R7RS's binding.
;;;
;;; What is not immutable in practice: istrings are ordinary, mutable
;;; Pseudoscheme strings.  string-set!, string-fill! and string-copy!
;;; work on any string, string literals and istrings included; nothing
;;; is enforced.  istring? reports provenance instead: it is false for
;;; the strings made by this library's make-string and string-copy (and
;;; resized by string-append! and string-replace!) and by (srfi 140
;;; mstrings), which are recorded in a weak table, and true for every
;;; other string.  So a string from R7RS's own make-string or
;;; string-copy (imported from (scheme base), not from here) counts as
;;; an istring.  The istring-returning procedures never return one of
;;; the caller's mutable strings, though, so their results are not
;;; changed by later mutation of their arguments.
;;;
;;; Strings are fixed-length, so string-append! and string-replace! are
;;; macros, as in (srfi 118): given a variable they set! it to the
;;; resized (new) string; other references to the old string do not see
;;; the change, and they cannot be applied or passed as procedures.
;;;
;;; make-string, string-copy, list->string (which takes start and end)
;;; and string-map (whose proc may return a string) replace (scheme
;;; base)'s, so a program importing both excludes those four from
;;; (scheme base), or imports (srfi 140 base) instead of it.  The SRFI 152/13/130 names
;;; (string-index, string-join, ...) clash with those SRFIs.
(define-library (srfi 140)
  (export
          istring? list->string make-string reverse-list->string string
          string->list string->utf16 string->utf16be string->utf16le
          string->utf8 string->vector string-any string-append string-append!
          string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
          string-concatenate string-concatenate-reverse string-contains
          string-contains-right string-copy string-copy! string-count
          string-downcase string-drop string-drop-right string-every
          string-fill! string-filter string-fold string-fold-right
          string-foldcase
          string-for-each string-for-each-index string-index
          string-index-right string-join string-length string-map
          string-map-index
          string-null? string-pad string-pad-right string-prefix-length
          string-prefix? string-ref string-remove string-repeat string-replace
          string-replace! string-reverse string-set! string-skip
          string-skip-right string-split string-suffix-length string-suffix?
          string-tabulate string-take string-take-right string-titlecase
          string-trim string-trim-both string-trim-right string-unfold
          string-unfold-right string-upcase string<=? string<? string=?
          string>=? string>? string? substring utf16->string utf16be->string
          utf16le->string utf8->string vector->string xsubstring)
  (import (only (scheme base)
                string? string->vector string->list vector->string
                string->utf8 utf8->string string string-length string-ref
                substring string=? string<? string>? string<=? string>=?
                string-append string-for-each string-set!
                string-fill! string-copy!)
          (only (scheme char)
                string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
                string-upcase string-downcase string-foldcase)
          (only (srfi private srfi-140-strings)
                istring? make-string string-copy list->string string-null?
                string-every string-any string-tabulate string-unfold
                string-unfold-right reverse-list->string string->utf16
                string->utf16be string->utf16le utf16->string
                utf16be->string utf16le->string string-take string-drop
                string-take-right string-drop-right string-pad
                string-pad-right string-trim string-trim-right
                string-trim-both string-replace string-prefix-length
                string-suffix-length string-prefix? string-suffix?
                string-index string-index-right
                string-skip string-skip-right string-contains
                string-contains-right string-titlecase string-map string-concatenate
                string-concatenate-reverse string-join string-fold
                string-fold-right string-map-index string-for-each-index
                string-count
                string-filter string-remove string-reverse string-repeat
                xsubstring string-split string-append! string-replace!)))
