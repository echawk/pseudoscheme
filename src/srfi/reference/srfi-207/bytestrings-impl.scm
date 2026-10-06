;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;; Utility

(define (exact-natural? x)
  (and (exact-integer? x) (not (negative? x))))

(define (u8-or-ascii-char? obj)
  (or (and (char? obj) (char<=? obj #\delete))
      (and (exact-natural? obj) (< obj 256))))

(define (string-ascii? obj)
  (and (string? obj)
       (string-every (lambda (c) (char<=? c #\delete)) obj)
       #t))

(define (valid-bytestring-segment? obj)
  (or (bytevector? obj)
      (u8-or-ascii-char? obj)
      (string-ascii? obj)))

(define (%bytestring-null? bstring)
  (zero? (bytevector-length bstring)))

(define (%bytestring-last bstring)
  (assume (not (%bytestring-null? bstring)) "empty bytestring")
  (bytevector-u8-ref bstring (- (bytevector-length bstring) 1)))

(define (negate pred)
  (lambda (obj)
    (not (pred obj))))

;;;; Constructors

(define (make-bytestring lis)
  (assume (or (pair? lis) (null? lis)))
  (call-with-port
   (open-output-bytevector)
   (lambda (out)
     (for-each (lambda (seg) (%write-bytestring-segment seg out)) lis)
     (get-output-bytevector out))))

(define (make-bytestring! bvec at lis)
  (assume (bytevector? bvec))
  (assume (and (exact-natural? at)
               (< at (bytevector-length bvec))))
  (bytevector-copy! bvec at (make-bytestring lis)))

(define (%write-bytestring-segment obj port)
  ((cond ((and (exact-natural? obj) (< obj 256)) write-u8)
         ((and (char? obj) (char<=? obj #\delete)) write-char-binary)
         ((bytevector? obj) write-bytevector)
         ((string-ascii? obj) write-string-binary)
         (else
          (bytestring-error "invalid bytestring element" obj)))
   obj
   port))

;; If your Scheme allows binary ports to function as textual ports,
;; get rid of this dance.
(define (write-char-binary c port)
  (write-u8 (char->integer c) port))

(define (write-string-binary s port)
  (string-for-each (lambda (c)
                     (write-char-binary c port))
                   s))

(define (bytestring . args)
  (if (null? args) (bytevector) (make-bytestring args)))

;;;; Conversion

;;; Hex string conversion

;; Convert an unsigned integer n to a bytevector representing
;; the base-256 big-endian form (the zero index holds the MSB).
(define (integer->bytevector n)
  (assume (and (integer? n) (not (negative? n))))
  (if (zero? n)
      (make-bytevector 1 0)
      (u8-list->bytevector
       (unfold-right zero?
                     (lambda (n) (truncate-remainder n 256))
                     (lambda (n) (truncate-quotient n 256))
                     n))))

(define (integer->hex-string n)
  (cond ((number->string n 16) =>
         (lambda (res)
           (if (even? (string-length res))
               res
               (string-append "0" res))))
        (else (bytestring-error "not an integer" n))))

(define (bytevector->hex-string bv)
  (assume (bytevector? bv))
  (string-concatenate
   (list-tabulate (bytevector-length bv)
                  (lambda (i)
                    (integer->hex-string (bytevector-u8-ref bv i))))))

(define (hex-string->bytevector hex-str)
  (assume (string? hex-str))
  (let ((len (string-length hex-str)))
    (unless (even? len)
      (bytestring-error "incomplete hexadecimal string" hex-str))
    (u8vector-unfold
     (lambda (_ i)
       (let* ((end (+ i 2))
              (s (substring hex-str i end))
              (n (string->number s 16)))
         (if n
             (values n end)
             (bytestring-error "invalid hexadecimal sequence" s))))
     (truncate-quotient len 2)
     0)))

(define bytevector->base64
  (case-lambda
    ((bvec) (bytevector->base64 bvec "+/"))
    ((bvec digits)
     (assume (bytevector? bvec))
     (assume (string? digits))
     (utf8->string (base64-encode-bytevector bvec digits)))))

(define base64->bytevector
  (case-lambda
    ((base64-string) (base64->bytevector base64-string "+/"))
    ((base64-string digits)
     (assume (string? base64-string))
     (assume (string? digits))
     (decode-base64-string base64-string digits))))

(define bytestring->list
  (case-lambda
    ((bstring) (bytestring->list bstring 0 (bytevector-length bstring)))
    ((bstring start)
     (bytestring->list bstring start (bytevector-length bstring)))
    ((bstring start end)
     (assume (bytevector? bstring))
     (assume (and (exact-natural? start) (>= start 0))
             "invalid start index"
             start
             bstring)
     (assume (and (exact-natural? end) (<= end (bytevector-length bstring)))
             "invalid end index"
             end
             bstring)
     (assume (>= end start) "invalid indices" start end)
     (unfold (lambda (i) (= i end))
             (lambda (i)
               (let ((b (bytevector-u8-ref bstring i)))
                 (if (and (>= b #x20) (< b #x7f))
                     (integer->char b)
                     b)))
             (lambda (i) (+ i 1))
             start))))

;; Lazily generate the bytestring constructed from objs.
(define (make-bytestring-generator . objs)
  (list->generator (flatten-bytestring-segments objs)))

;; Convert and flatten chars and strings, and flatten bytevectors
;; to yield a flat list of bytes.
(define (flatten-bytestring-segments objs)
  (fold-right
   (lambda (x res)
     (cond ((and (exact-natural? x) (< x 256)) (cons x res))
           ((and (char? x) (char<=? x #\delete))
            (cons (char->integer x) res))
           ((bytevector? x)
            (append (bytevector->u8-list x) res))
           ((string-ascii? x)
            (append (map char->integer (string->list x)) res))
           (else
            (bytestring-error "invalid bytestring segment" x))))
   '()
   objs))

;;;; Selection

(define (%bytestring-pad-left-or-right bstring len char-or-u8 right)
  (assume (bytevector? bstring))
  (assume (exact-natural? len))
  (assume (u8-or-ascii-char? char-or-u8))
  (let ((pad-len (- len (bytevector-length bstring)))
        (pad-byte (if (char? char-or-u8)
                      (char->integer char-or-u8)
                      char-or-u8)))
    (if (<= pad-len 0)
        (bytevector-copy bstring)
        (let ((padded (make-bytevector len pad-byte)))
          (bytevector-copy! padded (if right 0 pad-len) bstring)
          padded))))

(define (bytestring-pad bstring len char-or-u8)
  (%bytestring-pad-left-or-right bstring len char-or-u8 #f))

(define (bytestring-pad-right bstring len char-or-u8)
  (%bytestring-pad-left-or-right bstring len char-or-u8 #t))

(define (bytestring-trim bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((new-start (bytestring-index bstring (negate pred))))
    (if new-start
        (bytevector-copy bstring new-start)
        (bytevector))))

(define (bytestring-trim-right bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (cond ((bytestring-index-right bstring (negate pred)) =>
         (lambda (end-1)
           (bytevector-copy bstring 0 (+ 1 end-1))))
        (else (bytevector))))

(define (bytestring-trim-both bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (let ((neg-pred (negate pred)))
    (cond ((bytestring-index bstring neg-pred) =>
           (lambda (start)
             (bytevector-copy bstring
                              start
                              (+ (bytestring-index-right bstring neg-pred)
                                 1))))
          (else (bytevector)))))

;;;; Replacement

(define bytestring-replace
  (case-lambda
    ((bstring1 bstring2 start end)
     (bytestring-replace bstring1
                         bstring2
                         start
                         end
                         0
                         (bytevector-length bstring2)))
    ((bstring1 bstring2 start1 end1 start2 end2)
     (assume (bytevector? bstring1))
     (assume (bytevector? bstring2))
     (assume (and (exact-natural? start1) (>= start1 0) (<= start1 end1))
             "invalid start index"
             start1)
     (assume (and (exact-natural? end1)
                  (<= end1 (bytevector-length bstring1)))
             "invalid end index"
             bstring1)
     (assume (and (exact-natural? start2) (>= start2 0) (<= start2 end2))
             "invalid start index"
             start2)
     (assume (and (exact-natural? end2)
                  (<= end2 (bytevector-length bstring2)))
             "invalid end index"
             bstring2)
     (if (and (= start1 end1) (= start2 end2))
         (bytevector-copy bstring1)    ; replace no bits with no bits
         (let* ((b1-len (bytevector-length bstring1))
                (sub-len (- end2 start2))
                (new-len (+ sub-len (- b1-len (- end1 start1))))
                (bs-new (make-bytevector new-len)))
           (bytevector-copy! bs-new 0 bstring1 0 start1)
           (bytevector-copy! bs-new start1 bstring2 start2 end2)
           (bytevector-copy! bs-new (+ start1 sub-len) bstring1 end1 b1-len)
           bs-new)))))

;;;; Comparison

(define (%bytestring-prefix-length bstring1 bstring2)
  (let ((end (min (bytevector-length bstring1)
                  (bytevector-length bstring2))))
    (if (eqv? bstring1 bstring2)  ; fast path
        end
        (let lp ((i 0))
          (if (or (>= i end)
                  (not (= (bytevector-u8-ref bstring1 i)
                          (bytevector-u8-ref bstring2 i))))
              i
              (lp (+ i 1)))))))

;;; Primitive bytevector comparison functions.

(define (%bytestring-compare bstring1 bstring2 res< res= res>)
  (let ((len1 (bytevector-length bstring1))
        (len2 (bytevector-length bstring2)))
    (let ((match (%bytestring-prefix-length bstring1 bstring2)))
      (if (= match len1)
          (if (= match len2) res= res<)
          (if (= match len2)
              res>
              (if (< (bytevector-u8-ref bstring1 match)
                     (bytevector-u8-ref bstring2 match))
                  res<
                  res>))))))

(define (bytestring<? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (and (not (eqv? bstring1 bstring2))
       (%bytestring-compare bstring1 bstring2 #t #f #f)))

(define (bytestring>? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (and (not (eqv? bstring1 bstring2))
       (%bytestring-compare bstring1 bstring2 #f #f #t)))

(define (bytestring<=? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (or (eqv? bstring1 bstring2)
      (%bytestring-compare bstring1 bstring2 #t #t #f)))

(define (bytestring>=? bstring1 bstring2)
  (assume (bytevector? bstring1))
  (assume (bytevector? bstring2))
  (or (eqv? bstring1 bstring2)
      (%bytestring-compare bstring1 bstring2 #f #t #t)))

;;;; Searching

(define bytestring-index
  (case-lambda
    ((bstring pred) (bytestring-index bstring pred 0))
    ((bstring pred start)
     (bytestring-index bstring pred start (bytevector-length bstring)))
    ((bstring pred start end)
     (assume (bytevector? bstring))
     (assume (procedure? pred))
     (assume (exact-natural? start))
     (assume (exact-natural? end))
     (let lp ((i start))
       (and (< i end)
            (if (pred (bytevector-u8-ref bstring i))
                i
                (lp (+ i 1))))))))

(define bytestring-index-right
  (case-lambda
    ((bstring pred) (bytestring-index-right bstring pred 0))
    ((bstring pred start)
     (bytestring-index-right bstring pred start (bytevector-length bstring)))
    ((bstring pred start end)
     (assume (bytevector? bstring))
     (assume (procedure? pred))
     (assume (exact-natural? start))
     (assume (exact-natural? end))
     (let lp ((i (- end 1)))
       (and (>= i start)
            (if (pred (bytevector-u8-ref bstring i))
                i
                (lp (- i 1))))))))

(define (bytestring-break bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (cond ((bytestring-index bstring pred) =>
         (lambda (len)
           (values (bytevector-copy bstring 0 len)
                   (bytevector-copy bstring len))))
        (else (values (bytevector-copy bstring) (bytevector)))))

(define (bytestring-span bstring pred)
  (assume (bytevector? bstring))
  (assume (procedure? pred))
  (cond ((bytestring-index bstring (negate pred)) =>
         (lambda (len)
           (values (bytevector-copy bstring 0 len)
                   (bytevector-copy bstring len))))
        (else (values (bytevector-copy bstring) (bytevector)))))

;;;; Joining & Splitting

(define (%bytestring-join-nonempty bstrings delimiter grammar)
  (call-with-port
   (open-output-bytevector)
   (lambda (out)
     (when (eqv? grammar 'prefix) (write-bytevector delimiter out))
     (write-bytevector (car bstrings) out)
     (for-each (lambda (bstr)
                 (write-bytevector delimiter out)
                 (write-bytevector bstr out))
               (cdr bstrings))
     (when (eqv? grammar 'suffix) (write-bytevector delimiter out))
     (get-output-bytevector out))))

(define bytestring-join
  (case-lambda
    ((bstrings delimiter) (bytestring-join bstrings delimiter 'infix))
    ((bstrings delimiter grammar)
     (assume (or (pair? bstrings) (null? bstrings)))
     (unless (memv grammar '(infix strict-infix prefix suffix))
       (bytestring-error "invalid grammar" grammar))
     (let ((delim-bstring (bytestring delimiter)))
       (if (pair? bstrings)
           (%bytestring-join-nonempty bstrings delim-bstring grammar)
           (if (eqv? grammar 'strict-infix)
               (bytestring-error "empty list with strict-infix grammar")
               (bytevector)))))))

(define (%find-right bstring byte end)
  (bytestring-index-right bstring (lambda (b) (= b byte)) 0 end))

(define (%bytestring-infix-split bstring delimiter)
  (let lp ((token-end (bytevector-length bstring)) (split '()))
    (cond ((< token-end 0) split)
          ((%find-right bstring delimiter token-end) =>
           (lambda (token-start-1)
             (lp token-start-1
                 (cons (bytevector-copy bstring (+ 1 token-start-1)
                                                token-end)
                       split))))
          (else (cons (bytevector-copy bstring 0 token-end) split)))))

(define (%trim-byte bstring byte)
  (bytestring-trim bstring (lambda (b) (= b byte))))

(define (%trim-right-byte bstring byte)
  (bytestring-trim-right bstring (lambda (b) (= b byte))))

(define (%bytestring-split/trim-outliers bstring delimiter grammar)
  (let ((trimmed (case grammar
                  ((infix strict-infix) bstring)
                  ((prefix) (%trim-byte bstring delimiter))
                  ((suffix) (%trim-right-byte bstring delimiter)))))
    (%bytestring-infix-split trimmed delimiter)))

(define bytestring-split
  (case-lambda
    ((bstring delimiter) (bytestring-split bstring delimiter 'infix))
    ((bstring delimiter grammar)
     (assume (bytevector? bstring))
     (assume (u8-or-ascii-char? delimiter))
     (unless (memv grammar '(infix strict-infix prefix suffix))
       (bytestring-error "invalid grammar" grammar))
     (if (%bytestring-null? bstring)
         '()
         (%bytestring-split/trim-outliers
          bstring
          (if (char? delimiter) (char->integer delimiter) delimiter)
          grammar)))))

;;;; I/O

(define backslash-codepoints
  '((7 . #\a) (8 . #\b) (9 . #\t) (10 . #\n) (13 . #\r)
    (34 . #\") (92 . #\\) (124 . #\|)))

(define write-textual-bytestring
  (case-lambda
   ((bstring)
    (write-textual-bytestring bstring (current-output-port)))
   ((bstring port)
    (parameterize ((current-output-port port))
      (write-string "#u8\"")
      (u8vector-for-each
       (lambda (b)
         (cond ((assv b backslash-codepoints) =>
                (lambda (p)
                  (write-char #\\)
                  (write-char (cdr p))))
               ((and (>= b #x20) (<= b #x7e))
                (write-char (integer->char b)))
               (else
                (write-string "\\x")
                (write-string (number->string b 16))
                (write-char #\;))))
       bstring)
      (write-char #\")))))

(define (write-binary-bytestring port . args)
  (assume (binary-port? port))
  (for-each (lambda (arg)
              (unless (valid-bytestring-segment? arg)
                (bytestring-error "invalid bytestring element" arg)))
            args)
  (for-each (lambda (seg) (%write-bytestring-segment seg port)) args))
