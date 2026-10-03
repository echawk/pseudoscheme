;;; SRFI 13: string library.  Olin Shivers' reference implementation
;;; (reference/srfi-13.scm), MIT Scheme / scsh copyright, BSD-style; see
;;; the end of that file.
;;;
;;; Where R7RS's procedure already meets SRFI 13's spec (string->list,
;;; string-copy, string-copy! and string-fill! with optional start/end,
;;; plus the plain R5RS string procedures) the library exports that very
;;; binding, so those names never conflict with (scheme base).  SRFI 13's
;;; string-map and string-for-each (one string, optional start/end) and
;;; string-upcase/string-downcase (optional start/end) differ from R7RS's,
;;; so a program importing (scheme base) or (scheme char) along with
;;; (srfi 13) has to exclude one side's.
;;;
;;; Char sets are SRFI 14's, so they hold Latin-1 characters only.
;;; char-cased? is char-upper-case? or char-lower-case?; char-titlecase is
;;; char-upcase.
;;;
;;; Modification to the reference code:
;;; - its definitions of string-copy, string->list, string-fill! and
;;;   string-copy! are commented out, since R7RS's are the same and are
;;;   exported instead.
(define-library (srfi 13)
  (export string-null? string-every string-any
          string-tabulate reverse-list->string string-join
          substring/shared
          string-take string-take-right string-drop string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both
          string-compare string-compare-ci
          string= string<> string< string> string<= string>=
          string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>=
          string-hash string-hash-ci
          string-prefix-length string-suffix-length
          string-prefix-length-ci string-suffix-length-ci
          string-prefix? string-suffix? string-prefix-ci? string-suffix-ci?
          string-index string-index-right string-skip string-skip-right
          string-count string-contains string-contains-ci
          string-titlecase string-upcase string-downcase
          string-titlecase! string-upcase! string-downcase!
          string-reverse string-reverse!
          string-concatenate string-concatenate/shared string-append/shared
          string-concatenate-reverse string-concatenate-reverse/shared
          string-map string-map! string-fold string-fold-right
          string-unfold string-unfold-right
          string-for-each string-for-each-index
          xsubstring string-xcopy! string-replace string-tokenize
          string-filter string-delete
          ;; low-level procedures
          string-parse-start+end string-parse-final-start+end
          let-string-start+end check-substring-spec substring-spec-ok?
          make-kmp-restart-vector kmp-step string-kmp-partial-search
          ;; R7RS's, which meet SRFI 13's spec
          string->list string-copy string-copy! string-fill!
          string? make-string string string-length string-ref string-set!
          string-append list->string)
  (import (except (scheme base) string-map string-for-each)
          (only (scheme char) char-upcase char-downcase char-ci=? char-ci<?
                char-upper-case? char-lower-case?)
          (only (rnrs arithmetic bitwise) bitwise-and)
          (srfi private shim)
          (srfi 14))
  (begin
    (define (char-cased? c) (or (char-upper-case? c) (char-lower-case? c)))
    (define (char-titlecase c) (char-upcase c)))
  (include "reference/srfi-13.scm"))
