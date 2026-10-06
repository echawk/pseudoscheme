;;; Tests for SRFI 181 and 192: the sample implementation's test.scm (by
;;; Shiro Kawai, MIT licence; in reference/srfi-181/), with its
;;; implementation-specific imports replaced.  Changes, marked
;;; PSEUDOSCHEME: its textual read! procedures, when given a string (as
;;; here; Gauche and the sample's adapter give vectors), didn't advance
;;; their positions.
(import (scheme base) (scheme process-context) (srfi 64) (scheme write) (scheme read) (scheme char) (srfi 1) (only (srfi 13) string-tabulate) (srfi 181) (srfi 192))

(test-begin "srfi-181")

(test-group
 "Binary input, no port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define pos 0)
 (define closed #f)
 (define p (make-custom-binary-input-port
            "binary-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            #f                          ; get-position
            #f                          ; set-position
            (lambda () (set! closed #t)))) ; close
 (test-assert "port?" (port? p))
 (test-assert "input-port?" (input-port? p))
 (test-assert "not output port?" (not (output-port? p)))
 (test-assert "not has position?" (not (port-has-port-position? p)))
 (test-assert "not set position?" (not (port-has-set-port-position!? p)))

 (test-eqv 0 (read-u8 p))
 (test-eqv 1 (read-u8 p))
 (test-eqv 2 (peek-u8 p))
 (test-eqv 2 (read-u8 p))

 (test-equal (bytevector-copy data 3)
             (read-bytevector 997 p))
 (test-equal (eof-object) (read-u8 p))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Binary input, port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define p (make-custom-binary-input-port
            "binary-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t)) ;close
            ))
 (test-assert "port?" (port? p))
 (test-assert "input-port?" (input-port? p))
 (test-assert "not output port?" (not (output-port? p)))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (test-eqv 0 (read-u8 p))
 (test-eqv 1 (read-u8 p))
 (test-eqv 2 (peek-u8 p))
 (set! saved-pos (port-position p))
 (test-eqv 2 (read-u8 p))

 (test-equal (bytevector-copy data 3)
             (read-bytevector 997 p))
 (test-equal (eof-object) (read-u8 p))

 (set-port-position! p saved-pos)
 (test-eqv 2 (read-u8 p))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Textual input, no port positioning"
 (define data (string-tabulate (lambda (i)
                                 (integer->char
                                  (cond-expand
                                   (full-unicode (+ #x3000 i))
                                   (else (modulo i 256)))))
                               1000))
 (define pos 0)
 (define closed #f)
 (define p (make-custom-textual-input-port
            "textual-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (string-length data) pos))))
                (unless (zero? size)
                  (if (string? buf)
                    (begin    ; PSEUDOSCHEME: added the begin and set!
                      (string-copy! buf start data pos (+ pos size))
                      (set! pos (+ pos size)))
                    (do ((i 0 (+ i 1))
                         (j pos (+ j 1)))
                        ((= i size) (set! pos j))
                      (vector-set! buf (+ start i) (string-ref data j)))))
                size))
            #f                          ; get-position
            #f                          ; set-position
            (lambda () (set! closed #t)); close
            ))
 (test-assert "port?" (port? p))
 (test-assert "input-port?" (input-port? p))
 (test-assert "not output port?" (not (output-port? p)))
 (test-assert "not has position?" (not (port-has-port-position? p)))
 (test-assert "not set position?" (not (port-has-set-port-position!? p)))

 (test-eqv (string-ref data 0) (read-char p))
 (test-eqv (string-ref data 1) (read-char p))
 (test-eqv (string-ref data 2) (peek-char p))
 (test-eqv (string-ref data 2) (read-char p))

 (test-equal (string-copy data 3)
             (read-string 997 p))
 (test-equal (eof-object) (read-char p))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Textual input, port positioning"
 (define data (string-tabulate (lambda (i)
                                 (integer->char
                                  (cond-expand
                                   (full-unicode (+ #x3000 i))
                                   (else (modulo i 256)))))
                               1000))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define p (make-custom-textual-input-port
            "textual-input"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- (string-length data) pos))))
                (unless (zero? size)
                  (if (string? buf)
                    (begin    ; PSEUDOSCHEME: added the begin and set!
                      (string-copy! buf start data pos (+ pos size))
                      (set! pos (+ pos size)))
                    (do ((i 0 (+ i 1))
                         (j pos (+ j 1)))
                        ((= i size) (set! pos j))
                      (vector-set! buf (+ start i) (string-ref data j)))))
                size))
            (lambda () pos)             ; get-position
            (lambda (k) (set! pos k))   ; set-position
            (lambda () (set! closed #t)); close
            ))
 (test-assert (port? p))
 (test-assert (input-port? p))
 (test-assert (not (output-port? p)))
 (test-assert (port-has-port-position? p))
 (test-assert (port-has-set-port-position!? p))

 (test-eqv (string-ref data 0) (read-char p))
 (test-eqv (string-ref data 1) (read-char p))
 (test-eqv (string-ref data 2) (peek-char p))
 (set! saved-pos (port-position p))
 (test-eqv (string-ref data 2) (read-char p))
 (test-eqv (string-ref data 3) (peek-char p))

 (test-equal (string-copy data 3)
             (read-string 997 p))
 (test-equal (eof-object) (read-char p))

 (set-port-position! p saved-pos)
 (test-eqv (string-ref data 2) (peek-char p))

 (test-assert (begin (close-port p)
                     closed))
 )

(test-group
 "Binary output, port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define sink (make-vector 2000 #f))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define flushed #f)
 (define p (make-custom-binary-output-port
            "binary-output"
            (lambda (buf start count)   ;write!
              (do ((i start (+ i 1))
                   (j pos (+ j 1)))
                  ((>= i (+ start count)) (set! pos j))
                (vector-set! sink j (bytevector-u8-ref buf i)))
              count)
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "not input-port?" (not (input-port? p)))
 (test-assert "output port?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (write-u8 3 p)
 (write-u8 1 p)
 (write-u8 4 p)
 (flush-output-port p)
 (test-assert "flush" flushed)
 (set! saved-pos (port-position p))

 (test-equal '#(3 1 4)
             (vector-copy sink 0 pos))
 (write-bytevector '#u8(1 5 9 2 6) p)
 (flush-output-port p)
 (test-equal '#(3 1 4 1 5 9 2 6)
             (vector-copy sink 0 pos))

 (set-port-position! p saved-pos)
 (for-each (lambda (b) (write-u8 b p)) '(5 3 5))
 (flush-output-port p)
 (test-equal '#(3 1 4 5 3 5)
             (vector-copy sink 0 pos))
 (test-equal '#(3 1 4 5 3 5 2 6)
             (vector-copy sink 0 (+ pos 2)))

 (test-assert "close" (begin (close-port p)
                             closed))
 )

(test-group
 "Textual output, port positioning"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define sink (make-vector 2000 #f))
 (define pos 0)
 (define saved-pos #f)
 (define closed #f)
 (define flushed #f)
 (define p (make-custom-textual-output-port
            "textual-output"
            (lambda (buf start count)   ;write!
              (do ((i start (+ i 1))
                   (j pos (+ j 1)))
                  ((>= i (+ start count)) (set! pos j))
                (vector-set! sink j
                             (if (string? buf)
                               (string-ref buf i)
                               (vector-ref buf i))))
              count)
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "not input?" (not (input-port? p)))
 (test-assert "output?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (write-char #\a p)
 (write-char #\b p)
 (write-char #\c p)
 (flush-output-port p)
 (test-assert "flush" flushed)
 (set! saved-pos (port-position p))

 (test-equal '#(#\a #\b #\c)
             (vector-copy sink 0 pos))
 (write-string "Quack" p)
 (flush-output-port p)
 (test-equal '#(#\a #\b #\c #\Q #\u #\a #\c #\k)
             (vector-copy sink 0 pos))

 (set-port-position! p saved-pos)
 (write-string "Cli" p)
 (flush-output-port p)
 (test-equal '#(#\a #\b #\c #\C #\l #\i)
             (vector-copy sink 0 pos))
 (test-equal '#(#\a #\b #\c #\C #\l #\i #\c #\k)
             (vector-copy sink 0 (+ pos 2)))

 (test-assert (begin (close-port p)
                     closed))
 )

;; NB: Gauche doesn't support input/output port yet.
(cond-expand
 (gauche)
 (else
(test-group
 "binary input/output"
 (define data (apply bytevector
                     (list-tabulate 1000 (lambda (i) (modulo i 256)))))
 (define original-size 500)             ;writing may extend the size
 (define pos 0)
 (define saved-pos #f)
 (define flushed #f)
 (define closed #f)
 (define p (make-custom-binary-input/output-port
            "binary i/o"
            (lambda (buf start count)   ; read!
              (let ((size (min count (- original-size pos))))
                (bytevector-copy! buf start data pos (+ pos size))
                (set! pos (+ pos size))
                size))
            (lambda (buf start count)   ;write!
              (let ((size (min count (- (bytevector-length data) pos))))
                (bytevector-copy! data pos buf start (+ start size))
                (set! pos (+ pos size))
                (set! original-size (max original-size pos))
                size))
            (lambda () pos)             ;get-position
            (lambda (k) (set! pos k))   ;set-position!
            (lambda () (set! closed #t)) ; close
            (lambda () (set! flushed #t)) ; flush
            ))
 (test-assert "port?" (port? p))
 (test-assert "input?" (input-port? p))
 (test-assert "output?" (output-port? p))
 (test-assert "has position?" (port-has-port-position? p))
 (test-assert "set position?" (port-has-set-port-position!? p))

 (test-eqv 0 (read-u8 p))
 (test-eqv 1 (read-u8 p))
 (test-eqv 2 (read-u8 p))
 (set! saved-pos (port-position p))
 (test-equal "rest of input"
             (bytevector-copy data 3 original-size)
             (read-bytevector 1000 p))
 (write-bytevector '#u8(255 255 255) p)
 (test-equal "appended"
             '#u8(255 255 255)
             (bytevector-copy data 500 503))
 (write-u8 254 p)
 (test-equal "appended more"
             '#u8(255 255 255 254)
             (bytevector-copy data 500 504))
 (write-u8 254 p)
 (test-eqv "still eof" (eof-object) (read-u8 p))

 (set-port-position! p saved-pos)
 (test-eqv "rewind & peek" 3 (peek-u8 p))
 (write-u8 100 p)
 (set-port-position! p saved-pos)
 (test-eqv "overwritten" 100 (read-u8 p))
 (test-eqv "overwritten" 4 (read-u8 p))
 )

)) ; cond expand

(test-group
 "file-error"
 (test-assert "file-error" (file-error? (make-file-error "bad"))))

(test-group
 "i/o-invalid-position-error"
 (test-assert "i/o-invalid-position-error" 
              (i/o-invalid-position-error? 
               (make-i/o-invalid-position-error 0))))

(test-group
 "high-level i/o"
 (define source
   (let ((orig "((a b c . d)
                  (a . (b . (c . (d . ()))))
                  #(a b c d)
                  (#\\a #\\b #\\c #\\d #\\x07 #\\) #\\#)
                  \"abcd\"
                  #t #f #true #false
                  |a\\x07;bcd|
                  ;;comment
                  #u8(1 2 3 4 5)
                  #0=(a) #0#
                  1 1.111 3e5 #xff #b-0101 -5/3+3.9i +inf.0 +nan.0
                  )"))
     (string-append orig "\n" orig "\n;end of input")))
 (define data (read (open-input-string source)))
 (define source-pos 0)
 (define sink (open-output-string))
 (define inp (make-custom-textual-input-port
              "textual-input"
              (lambda (buf start count) ;read!
                (let ((size (min count (- (string-length source) source-pos))))
                  (unless (zero? size)
                    (if (string? buf)
                      (begin  ; PSEUDOSCHEME: was count, and no set!
                        (string-copy! buf start source
                                      source-pos (+ source-pos size))
                        (set! source-pos (+ source-pos size)))
                      (do ((i 0 (+ i 1))
                           (j source-pos (+ j 1)))
                          ((= i size) (set! source-pos j))
                        (vector-set! buf (+ start i) (string-ref source j)))))
                  size))
              #f #f #f))
 (define outp (make-custom-textual-output-port
               "textual-output"
               (lambda (buf start count)   ;write!
                 (do ((i start (+ i 1)))
                     ((>= i (+ start count)))
                   (write-char (if (string? buf)
                                 (string-ref buf i)
                                 (vector-ref buf i))
                               sink))
                 count)
               #f #f #f))

 (let ((expr1 (read inp)))
   (test-equal "read first expr" data expr1)
   (write expr1 outp))
 (let ((expr2 (read inp)))
   (test-equal "read second expr" data expr2)
   (write expr2 outp))
 (test-assert "finishing" (eof-object? (read inp)))
 (let ((p (open-input-string (get-output-string sink))))
   (test-equal "written expr 1" data (read p))
   (test-equal "written expr 2" data (read p)))
 )

(test-group
 "transcoded input"
 ;; This test assumes native transcoder supports ascii range
 (test-equal "native" "ABCD"
             (bytevector->string '#u8(#x41 #x42 #x43 #x44)
                                 (native-transcoder)))
 ;; PSEUDOSCHEME: the sample assumed a host whose characters are ASCII
 ;; (see its README), so that every non-ASCII character decoded is
 ;; replaced or raises an error.  Pseudoscheme has all of Unicode: these
 ;; five tests' expectations are changed to the characters themselves.
 (test-equal "latin1 -> ascii" "ABC\xA1;\xA2;\xA3;XYZ\xC1;\xC2;\xC3;"
             (bytevector->string '#u8(#x41 #x42 #x43 #xa1 #xa2 #xa3
                                      #x58 #x59 #x5a #xc1 #xc2 #xc3)
                                 (make-transcoder (latin-1-codec)
                                                  (native-eol-style)
                                                  'replace)))
 (test-assert "latin1 raise"
              (guard (e ((i/o-decoding-error? e) #f))
                (bytevector->string '#u8(#xc1)
                                    (make-transcoder (latin-1-codec)
                                                     (native-eol-style)
                                                     'raise))
                #t))
 (test-equal "utf-16 (bom, be) -> ascii" "AB\x3000;\xC1;CD"
             (bytevector->string '#u8(#xfe #xff #x00 #x41 #x00 #x42
                                      #x30 #x00 #x00 #xc1 #x00 #x43 #x00 #x44)
                                 (make-transcoder (utf-16-codec)
                                                  (native-eol-style)
                                                  'replace)))
 (test-equal "utf-16 (bom, le) -> ascii" "AB\x3000;\xC1;CD"
             (bytevector->string '#u8(#xff #xfe #x41 #x00 #x42 #x00
                                      #x00 #x30 #xc1 #x00 #x43 #x00 #x44 #x00)
                                 (make-transcoder (utf-16-codec)
                                                  (native-eol-style)
                                                  'replace)))
 (test-equal "utf-16 (native) -> ascii" "AB\x3000;\xC1;CD"
             (bytevector->string
              (if (equal? (bytevector->string '#u8(#x00 #x41)
                                              (make-transcoder (utf-16-codec)
                                                               (native-eol-style)
                                                               'replace))
                          "A")
                '#u8(#x00 #x41 #x00 #x42 #x30 #x00 #x00 #xc1 #x00 #x43 #x00 #x44)
                '#u8(#x41 #x00 #x42 #x00 #x00 #x30 #xc1 #x00 #x43 #x00 #x44 #x00))
              (make-transcoder (utf-16-codec)
                               (native-eol-style)
                               'replace)))

 (test-equal "eol-style none, lf, crlf" '("A\nB\rC\r\nD"
                                          "A\nB\nC\nD"
                                          "A\nB\nC\nD")
             (map
              (lambda (style)
                (bytevector->string #u8(#x41 #x0a #x42 #x0d #x43 #x0d #x0a #x44)
                                    (make-transcoder (latin-1-codec)
                                                     style
                                                     'raise)))
              '(none lf crlf)))
 )

(test-group
 "transcoded output"
 ;; This test assumes native transcoder supports ascii range
 (test-equal "native" '#u8(#x41 #x42 #x43 #x44)
             (string->bytevector "ABCD"
                                 (native-transcoder)))
 (test-equal "ascii -> latin1" #u8(#x41 #x42 #x43 #x44)
             (string->bytevector "ABCD"
                                 (make-transcoder (latin-1-codec)
                                                  (native-eol-style)
                                                  'raise)))
 (test-equal "ascii -> utf-16" #f
             (not
              (member
               (string->bytevector "ABCD"
                                   (make-transcoder (utf-16-codec)
                                                    (native-eol-style)
                                                    'raise))
               '(#u8(#x00 #x41 #x00 #x42 #x00 #x43 #x00 #x44)
                 #u8(#x41 #x00 #x42 #x00 #x43 #x00 #x44 #x00)
                 #u8(#xfe #xff #x00 #x41 #x00 #x42 #x00 #x43 #x00 #x44)
                 #u8(#xff #xfe #x41 #x00 #x42 #x00 #x43 #x00 #x44 #x00)))))

 (test-equal "eol-style none, lf, crlf"
             '(#u8(#x41 #x0a #x42 #x0d #x43 #x0d #x0a #x44 #x0d #x0d #x0a)
               #u8(#x41 #x0a #x42 #x0a #x43 #x0a #x44 #x0a #x0a)
               #u8(#x41 #x0d #x0a #x42 #x0d #x0a #x43 #x0d #x0a #x44 #x0d #x0a #x0d #x0a))
             (map
              (lambda (style)
                (string->bytevector "A\nB\rC\r\nD\r\r\n"
                                    (make-transcoder (latin-1-codec)
                                                     style
                                                     'raise)))
              '(none lf crlf)))
 )



;; More tests, for what the sample's tests leave out.
(test-group
 "codecs and errors"
 (test-eqv (utf-8-codec) (make-codec "UTF-8"))
 (test-eqv (utf-8-codec) (make-codec "utf8"))
 (test-eqv (latin-1-codec) (make-codec "ISO-8859-1"))
 (test-eqv (latin-1-codec) (make-codec "latin1"))
 (test-eqv (utf-16-codec) (make-codec "utf-16"))
 (test-equal "EBCDIC-42"
   (guard (e ((unknown-encoding-error? e) (unknown-encoding-error-name e)))
     (make-codec "EBCDIC-42")))
 (test-assert (error-object? (guard (e (#t e)) (make-codec "nonesuch"))))
 (test-assert (file-error? (make-file-error "foo.txt")))
 (test-assert (file-error? (make-file-error)))
 (test-assert (guard (e ((file-error? e) #t)) (raise (make-file-error "x"))))
 (test-equal "caf\xE9;" (bytevector->string (bytevector 99 97 102 195 169)
                                         (make-transcoder (utf-8-codec) 'lf 'replace)))
 (test-equal (bytevector 99 97 102 233)
   (string->bytevector "caf\xE9;" (make-transcoder (latin-1-codec) 'lf 'raise)))
 (test-assert (guard (e ((i/o-encoding-error? e)
                         (eqv? #\x3000 (i/o-encoding-error-char e))))
                (string->bytevector "a\x3000;" (make-transcoder (latin-1-codec) 'lf 'raise))
                #f))
 (test-assert (guard (e ((i/o-decoding-error? e) #t))
                (bytevector->string (bytevector 255) (make-transcoder (utf-8-codec) 'lf 'raise))
                #f)))

(test-group
 "transcoded ports"
 (let* ((bytes (bytevector 104 105 13 10 116 104 101 114 101))
        (p (transcoded-port (open-input-bytevector bytes)
                            (make-transcoder (utf-8-codec) 'crlf 'replace))))
   (test-equal "hi" (read-line p))
   (test-equal "there" (read-line p))
   (test-assert (eof-object? (read-line p))))
 (let* ((sink (open-output-bytevector))
        (bin (make-custom-binary-output-port
              "sink" (lambda (bv start count)
                       (write-bytevector bv sink start (+ start count))
                       count)
              #f #f #f))
        (p (transcoded-port bin (make-transcoder (utf-8-codec) 'crlf 'replace))))
   (write-string "a\nb" p)
   (flush-output-port p)
   (test-equal (bytevector 97 13 10 98) (get-output-bytevector sink))))

(test-group
 "flush and close"
 (let* ((flushed 0) (closed #f) (got '())
        (p (make-custom-textual-output-port
            "t" (lambda (s start count)
                  (set! got (cons (substring s start (+ start count)) got))
                  count)
            #f #f (lambda () (set! closed #t)) (lambda () (set! flushed (+ flushed 1))))))
   (write 'sym p)
   (display " " p)
   (write "str" p)
   (flush-output-port p)
   (test-assert (> flushed 0))
   (test-equal "sym \"str\"" (apply string-append (reverse got)))
   (close-port p)
   (test-assert closed))
 ;; the flush argument may be omitted
 (let ((p (make-custom-binary-output-port "b" (lambda (bv s c) c) #f #f #f)))
   (write-u8 1 p)
   (flush-output-port p)
   (test-assert (output-port? p))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-181")
  (exit (if (zero? failures) 0 1)))
