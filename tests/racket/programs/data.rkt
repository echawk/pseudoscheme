#lang racket/base
;; Strings, characters, vectors, bytes, dates, SHA, converters and stencil
;; vectors (src/racket/native/data.lisp).  The .expected file is what
;; Racket 9.3 prints.
(require (only-in '#%kernel vector-extend vector-set/copy vector-append vector-copy))

(define-syntax-rule (show e)
  (with-handlers ([exn:fail? (lambda (x) (printf "~s: error: ~a\n" 'e (exn-message x)))])
    (call-with-values (lambda () e)
                      (lambda vs (printf "~s => ~s\n" 'e vs)))))

;; comparisons, one argument or more
(show (string<? "a"))
(show (string<? "a" 2))
(show (string-ci<? "a" "B" "c"))
(show (string>=? "c" "b" "b"))
(show (char-ci<? #\a #\B #\c))
(show (bytes<? #"a" #"b" #"c"))
(show (bytes>? #"b" #"a" #"a"))

;; strings and Unicode
(show (string-titlecase "hello wORLD foo-bar it's x1y"))
(show (string-utf-8-length (string #\a #\é #\€ #\😀)))
(show (string-utf-8-length (string #\a #\é #\€ #\😀) 1 3))
(show (string-grapheme-count "éa\r\n"))
(show (string-grapheme-span "éa" 0))
(show (char-grapheme-step #\a 0))
(show (char-grapheme-step #\newline 0))
(show (string-normalize-nfd "é"))
(show (string-length (string-normalize-nfc (string #\e #\u301))))
(show (bytes-utf-8-index #"a\303\251b" 2))
(show (bytes-utf-8-ref #"a\303\251b" 1))
(show (bytes-utf-8-ref #"a\303\251b" 5))

;; vectors
(show (vector-extend #(1 2) 5 0))
(show (vector-extend #(1 2) 1))
(show (vector->values #(1 2 3) 1))
(show (vector-set/copy #(1 2 3) 1 'x))
(show (vector-set/copy #(1 2 3) 3 'x))
(show (vector-append))
(show (vector-append #(1) #(2 3)))
(show (vector-append #(1) 2))
(show (vector-copy #(1 2 3) 1))
(show (vector-copy #(1 2 3) 2 1))
(show (let ([v (make-vector 4 0)]) (vector-copy! v 1 #(a b c) 1) v))
(show (vector-copy! (make-vector 3 0) 5 #(1 2)))
(show (string-copy! (make-string 3) 0 "abcd"))
(show (string-fill! "abc" #\a))
(show (let ([s (make-string 3 #\a)]) (string-fill! s #\z) s))

;; SHA
(show (sha1-bytes #"abc"))
(show (sha224-bytes #"abc"))
(show (sha256-bytes #"abc"))
(show (sha256-bytes #"abcdef" 1 3))
(show (sha256-bytes "abc"))

;; converters
(show (let ([c (bytes-open-converter "UTF-8" "UTF-16")])
        (bytes-convert c (bytes 97 195 169))))
(show (let ([c (bytes-open-converter "UTF-8" "UTF-8")]) (bytes-convert c #"a\303")))
(show (let ([c (bytes-open-converter "UTF-8" "UTF-8")]) (bytes-convert c #"a\377b")))
(show (let ([c (bytes-open-converter "UTF-8-permissive" "UTF-8")]) (bytes-convert c #"a\377b")))
(show (bytes-open-converter "UTF-8" "nonsense"))

;; stencil vectors
(show (stencil-vector #b101 'a 'b))
(show (stencil-vector-update (stencil-vector 5 'a 'b) 0 2 'c))
(show (stencil-vector-mask-width))
(show (equal? (stencil-vector 3 1 2) (stencil-vector 3 1 2)))
(show (stencil-vector 1))

;; dates
(show (let ([d (seconds->date 1700000000 #f)])
        (list (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d)
              (date-second d) (date-week-day d) (date-year-day d) (date-dst? d)
              (date-time-zone-offset d) (date*-nanosecond d) (date*-time-zone-name d))))
(show (date 1 2 3 4 5 6 7 8 #f 0))
(show (struct->vector (date 1 2 3 4 5 6 1 8 #f 0)))
(show (procedure-arity date))

;; the rest
(show (real->double-flonum (expt 10 400)))
(show (real->double-flonum 'a))
(show (string->path-element "a/b" #t))
(show (pseudo-random-generator-vector? #(1 2 3 4 5 0)))
(show (pseudo-random-generator-vector? #(0 0 0 1 2 3)))
