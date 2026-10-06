;;; Simple parser for string-notated bytevectors.

(define (parse prefix)
  (when prefix (consume-prefix))
  (consume-quote)
  (let lp ((c (read-char)))
    (cond ((eof-object? c) (bytestring-error "unexpected EOF"))
          ((char=? c #\") (if #f #f))  ; terminating quote
          ((char=? c #\\)
           (let ((c* (read-char)))
             (cond ((eof-object? c*)
                    (bytestring-error "incomplete escape sequence"))
                   ((escape c*) =>
                    (lambda (b)
                      (write-u8 b)
                      (lp (read-char))))
                   (else (lp (read-char))))))
          ((and (char>=? c #\space) (char<=? c #\~))
           (write-u8 (char->integer c))
           (lp (read-char)))
          (else (bytestring-error "invalid character" c)))))

(define (consume-quote)
  (let ((c (read-char)))
    (cond ((eof-object? c) (bytestring-error "unexpected EOF"))
          ((char=? c #\") #t)
          (else
           (bytestring-error "invalid character (expected #\\\")" c)))))

(define (consume-prefix)
  (let ((s (read-string 3)))
    (cond ((eof-object? s) (bytestring-error "unexpected EOF"))
          ((string=? s "#u8") #t)
          (else (bytestring-error "invalid bytestring prefix" s)))))

(define (escape c)
  (case c
    ((#\a) 7)
    ((#\b) 8)
    ((#\t) 9)
    ((#\n) 10)
    ((#\r) 13)
    ((#\") 34)
    ((#\\) 92)
    ((#\|) 124)
    ((#\x) (parse-hex))
    ((#\newline)
     (skip-horizontal-whitespace)
     #f)                              ; skip
    (else
     (cond ((char-whitespace? c)
            (skip-horizontal-whitespace)
            (skip-line-break)
            #f)
           (else (bytestring-error "invalid escaped character" c))))))

(define (parse-hex)
  (let* ((hex1 (read-char))
         (hex2 (read-char)))
    (when (or (eof-object? hex1) (eof-object? hex2))
      (bytestring-error "incomplete hexadecimal sequence"))
    (if (char=? hex2 #\;)
        (or (string->number (string hex1) 16)
            (bytestring-error "invalid hexadecimal sequence"))
        (let ((term (read-char)))
          (if (eqv? term #\;)
              (or (string->number (string hex1 hex2) 16)
                  (bytestring-error "invalid hexadecimal sequence"))
              (bytestring-error
               "overlong or unterminated hexadecimal sequence"))))))

(define (skip-line-break)
  (let ((c (read-char)))
    (unless (eqv? #\newline c)
      (bytestring-error "expected newline" c)))
  (skip-horizontal-whitespace))

(define (skip-horizontal-whitespace)
  (let lp ((c (peek-char)))
    (when (and (char-whitespace? c) (not (char=? c #\newline)))
      (read-char)
      (lp (peek-char)))))

(define read-textual-bytestring
  (case-lambda
   ((prefix) (read-textual-bytestring prefix (current-input-port)))
   ((prefix in)
    (assume (boolean? prefix))
    (call-with-port
     (open-output-bytevector)
     (lambda (out)
       (parameterize ((current-input-port in)
                      (current-output-port out))
         (parse prefix)
         (get-output-bytevector out)))))))
