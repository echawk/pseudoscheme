;;; (srfi 140 mstrings): the procedures that SRFI 140 says return
;;; istrings where R7RS or SRFI 13 return mutable strings, in versions
;;; that return newly allocated mutable strings (recorded as such, so
;;; istring? is false of them).  See ../140.sld.
(define-library (srfi 140 mstrings)
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
  (import (only (scheme base) define apply string-copy)
          (prefix (srfi 140 istrings) i:)
          (only (srfi private srfi-140-strings) mstring!))
  (begin
    (define (mutable s) (mstring! (string-copy s)))
    (define (string . args) (mutable (apply i:string args)))
    (define (list->string . args) (mutable (apply i:list->string args)))
    (define (vector->string . args) (mutable (apply i:vector->string args)))
    (define (utf8->string . args) (mutable (apply i:utf8->string args)))
    (define (utf16->string . args) (mutable (apply i:utf16->string args)))
    (define (utf16be->string . args) (mutable (apply i:utf16be->string args)))
    (define (utf16le->string . args) (mutable (apply i:utf16le->string args)))
    (define (string-upcase . args) (mutable (apply i:string-upcase args)))
    (define (string-downcase . args) (mutable (apply i:string-downcase args)))
    (define (string-foldcase . args) (mutable (apply i:string-foldcase args)))
    (define (string-titlecase . args) (mutable (apply i:string-titlecase
    args)))
    (define (string-append . args) (mutable (apply i:string-append args)))
    (define (substring . args) (mutable (apply i:substring args)))
    (define (string-map . args) (mutable (apply i:string-map args)))
    (define (string-tabulate . args) (mutable (apply i:string-tabulate args)))
    (define (string-unfold . args) (mutable (apply i:string-unfold args)))
    (define (string-unfold-right . args) (mutable (apply i:string-unfold-right
    args)))
    (define (reverse-list->string . args) (mutable (apply
    i:reverse-list->string args)))
    (define (string-take . args) (mutable (apply i:string-take args)))
    (define (string-drop . args) (mutable (apply i:string-drop args)))
    (define (string-take-right . args) (mutable (apply i:string-take-right
    args)))
    (define (string-drop-right . args) (mutable (apply i:string-drop-right
    args)))
    (define (string-pad . args) (mutable (apply i:string-pad args)))
    (define (string-pad-right . args) (mutable (apply i:string-pad-right
    args)))
    (define (string-trim . args) (mutable (apply i:string-trim args)))
    (define (string-trim-right . args) (mutable (apply i:string-trim-right
    args)))
    (define (string-trim-both . args) (mutable (apply i:string-trim-both
    args)))
    (define (string-replace . args) (mutable (apply i:string-replace args)))
    (define (string-concatenate . args) (mutable (apply i:string-concatenate
    args)))
    (define (string-concatenate-reverse . args) (mutable (apply
    i:string-concatenate-reverse args)))
    (define (string-join . args) (mutable (apply i:string-join args)))
    (define (string-filter . args) (mutable (apply i:string-filter args)))
    (define (string-remove . args) (mutable (apply i:string-remove args)))
    (define (string-reverse . args) (mutable (apply i:string-reverse args)))
    (define (string-repeat . args) (mutable (apply i:string-repeat args)))
    (define (xsubstring . args) (mutable (apply i:xsubstring args)))
    (define (string-map-index . args) (mutable (apply i:string-map-index
    args)))))
