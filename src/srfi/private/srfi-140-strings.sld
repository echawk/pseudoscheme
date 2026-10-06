;;; (srfi private srfi-140-strings): SRFI 140's procedures, for (srfi
;;; 140) and its sublibraries.  Written for Pseudoscheme: most are SRFI
;;; 135's textual procedures, from (srfi private srfi-140-texts), under
;;; their SRFI 140 names.
;;;
;;; Istrings are ordinary Pseudoscheme strings; nothing stops a program
;;; from mutating one.  What istring? reports is provenance: the strings
;;; made by (srfi 140)'s make-string and string-copy (and resized by its
;;; string-append! and string-replace!), and by the (srfi 140 mstrings)
;;; procedures, are recorded as mutable strings, in a weak eq hash table
;;; (SBCL's, through (pseudoscheme lisp)), and istring? is true of every
;;; other string, string literals and the results of R7RS procedures
;;; included.
(define-library (srfi private srfi-140-strings)
  (export istring? mstring! istring-result
          make-string string-copy list->string
          string-null? string-every string-any
          string-tabulate string-unfold string-unfold-right
          reverse-list->string
          string->utf16 string->utf16be string->utf16le
          utf16->string utf16be->string utf16le->string
          string-take string-drop string-take-right string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both
          string-replace
          string-prefix-length string-suffix-length
          string-prefix? string-suffix?
          string-index string-index-right string-skip string-skip-right
          string-contains string-contains-right
          string-titlecase string-map
          string-concatenate string-concatenate-reverse string-join
          string-fold string-fold-right
          string-map-index string-for-each-index
          string-count string-filter string-remove
          string-reverse string-repeat xsubstring string-split
          string-append! string-replace!)
  (import (except (scheme base) make-string string-copy list->string
                  string-map)
          (rename (only (scheme base) make-string string-copy)
                  (make-string r7:make-string)
                  (string-copy r7:string-copy))
          (scheme case-lambda)
          (only (rnrs syntax-case) syntax-case syntax identifier?)
          (pseudoscheme lisp)
          (prefix (only (cl common-lisp) gethash) cl:)
          (srfi private srfi-140-texts))
  (begin
    ;; The registry of mutable strings: weak keys, so it holds no string
    ;; alive.
    (define mstrings
      (lisp-funcall (lisp-function "make-hash-table" "common-lisp")
                    #:test (lisp-function "eq" "common-lisp")
                    #:weakness #:key))

    (define (mstring! s)
      (lisp-set! (cl:gethash s mstrings) #t)
      s)

    (define (mstring? s)
      (lisp-true? (lisp-funcall (lisp-function "gethash" "common-lisp")
                                s mstrings)))

    (define (istring? obj)
      (and (string? obj) (not (mstring? obj))))

    ;; A result of an istring-returning procedure must not be one of the
    ;; caller's mutable strings (SRFI 135's code may return its argument).
    (define (istring-result s)
      (if (and (string? s) (mstring? s)) (r7:string-copy s) s))

    (define (wrap proc)
      (lambda args (istring-result (apply proc args))))

    (define make-string
      (case-lambda
        (() (mstring! (r7:make-string 0)))
        ((k) (mstring! (r7:make-string k)))
        ((k char) (mstring! (r7:make-string k char)))))

    (define (string-copy . args)
      (mstring! (apply r7:string-copy args)))

    (define (list->string chars . start/end)
      (istring-result (apply list->text chars start/end)))

    (define string-null? textual-null?)
    (define string-every textual-every)
    (define string-any textual-any)
    (define string-tabulate text-tabulate)
    (define string-unfold (wrap text-unfold))
    (define string-unfold-right (wrap text-unfold-right))
    (define reverse-list->string reverse-list->text)
    (define string->utf16be textual->utf16be)
    (define string->utf16le textual->utf16le)
    (define utf16be->string utf16be->text)
    (define utf16le->string utf16le->text)
    (define string-take (wrap textual-take))
    (define string-drop (wrap textual-drop))
    (define string-take-right (wrap textual-take-right))
    (define string-drop-right (wrap textual-drop-right))
    (define string-pad (wrap textual-pad))
    (define string-pad-right (wrap textual-pad-right))
    (define string-trim (wrap textual-trim))
    (define string-trim-right (wrap textual-trim-right))
    (define string-trim-both (wrap textual-trim-both))
    (define string-replace (wrap textual-replace))
    (define string-prefix-length textual-prefix-length)
    (define string-suffix-length textual-suffix-length)
    (define string-prefix? textual-prefix?)
    (define string-suffix? textual-suffix?)
    (define string-index textual-index)
    (define string-index-right textual-index-right)
    (define string-skip textual-skip)
    (define string-skip-right textual-skip-right)
    (define string-contains textual-contains)
    (define string-contains-right textual-contains-right)
    (define string-titlecase (wrap textual-titlecase))
    ;; R7RS's string-map wants proc to return a character; SRFI 140's
    ;; may return a string too.
    (define string-map (wrap textual-map))

    ;; string->utf16 writes a BOM and the machine's own byte order, which
    ;; utf16->string assumes when there is no BOM (SRFI 135's use big
    ;; endianness).
    (define native-little-endian?
      (cond-expand (little-endian #t) (else #f)))

    (define (string->utf16 s . start/end)
      (if native-little-endian?
          (bytevector-append (bytevector #xff #xfe)
                             (apply textual->utf16le s start/end))
          (apply textual->utf16 s start/end)))

    (define (utf16->string bv . start/end)
      (let* ((start (if (pair? start/end) (car start/end) 0))
             (end (if (and (pair? start/end) (pair? (cdr start/end)))
                      (cadr start/end)
                      (bytevector-length bv)))
             (bom? (and (<= (+ start 2) end)
                        (let ((b0 (bytevector-u8-ref bv start))
                              (b1 (bytevector-u8-ref bv (+ start 1))))
                          (or (and (= b0 #xfe) (= b1 #xff))
                              (and (= b0 #xff) (= b1 #xfe)))))))
        (cond (bom? (utf16->text bv start end))
              (native-little-endian? (utf16le->text bv start end))
              (else (utf16be->text bv start end)))))
    (define string-concatenate (wrap textual-concatenate))
    (define string-concatenate-reverse (wrap textual-concatenate-reverse))
    (define string-join (wrap textual-join))
    (define string-fold textual-fold)
    (define string-fold-right textual-fold-right)
    (define string-map-index (wrap textual-map-index))
    (define string-for-each-index textual-for-each-index)
    (define string-count textual-count)
    (define string-filter (wrap textual-filter))
    (define string-remove (wrap textual-remove))
    (define string-split textual-split)

    (define (string-reverse s . start/end)
      (let* ((start (if (pair? start/end) (car start/end) 0))
             (end (if (and (pair? start/end) (pair? (cdr start/end)))
                      (cadr start/end)
                      (string-length s))))
        (list->text (reverse (string->list s start end)))))

    ;; (xsubstring string [from to [start end]])
    (define xsubstring
      (case-lambda
        ((s) (r7:string-copy s))
        ((s from) (istring-result (textual-replicate s from (+ from (string-length s)))))
        ((s from to . start/end)
         (istring-result (apply textual-replicate s from to start/end)))))

    (define (string-repeat s n)
      (let ((t (if (char? s) (string s) s)))
        (istring-result (textual-replicate t 0 (* n (string-length t))))))

    ;; string-append! and string-replace!, as in (srfi 118): macros that
    ;; set! a variable to the resized string (Pseudoscheme's strings are
    ;; fixed-length), registered as mutable.  A string not in a variable
    ;; can only be changed without changing its length.
    (define (%string-append dst . values)
      (let ((tail (apply string-append
                         (map (lambda (x) (if (char? x) (string x) x))
                              values))))
        (if (zero? (string-length tail))
            dst
            (mstring! (string-append dst tail)))))

    (define %string-replace
      (case-lambda
        ((dst dst-start dst-end src)
         (%string-replace dst dst-start dst-end src 0 (string-length src)))
        ((dst dst-start dst-end src src-start)
         (%string-replace dst dst-start dst-end src src-start
                          (string-length src)))
        ((dst dst-start dst-end src src-start src-end)
         (unless (<= 0 dst-start dst-end (string-length dst))
           (error "string-replace!: bad destination range" dst-start dst-end))
         (unless (<= 0 src-start src-end (string-length src))
           (error "string-replace!: bad source range" src-start src-end))
         (if (= (- dst-end dst-start) (- src-end src-start))
             (begin (string-copy! dst dst-start src src-start src-end) dst)
             (mstring!
              (string-append (substring dst 0 dst-start)
                             (substring src src-start src-end)
                             (substring dst dst-end (string-length dst))))))))

    (define (resized-in-place who dst result)
      (unless (eq? dst result)
        (error (string-append
                who ": cannot change the length of a string that is not "
                "in a variable (Pseudoscheme's strings are fixed-size)")
               dst)))

    (define-syntax string-append!
      (lambda (form)
        (syntax-case form ()
          ((_ place value ...)
           (identifier? #'place)
           #'(set! place (%string-append place value ...)))
          ((_ dst value ...)
           #'(let ((d dst))
               (resized-in-place "string-append!" d
                                 (%string-append d value ...)))))))

    (define-syntax string-replace!
      (lambda (form)
        (syntax-case form ()
          ((_ place arg ...)
           (identifier? #'place)
           #'(set! place (%string-replace place arg ...)))
          ((_ dst arg ...)
           #'(let ((d dst))
               (resized-in-place "string-replace!" d
                                 (%string-replace d arg ...)))))))))
