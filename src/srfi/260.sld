;;; SRFI 260: Generated symbols.  Written for Pseudoscheme, after Marc
;;; Nieper-Wißkirchen's portable sample implementation (MIT), which this
;;; follows: an interned symbol named with 128 random bits from
;;; /dev/random, written in hexadecimal after "g:".
(define-library (srfi 260)
  (export generate-symbol)
  (import (scheme base) (scheme case-lambda) (scheme file)
          (only (rnrs io ports) open-file-input-port get-bytevector-n))
  (begin
    (define random-port #f)

    (define (random-octets n)
      (unless random-port
        (set! random-port (open-file-input-port "/dev/random")))
      (get-bytevector-n random-port n))

    (define (hex-digit n)
      (string-ref "0123456789abcdef" n))

    (define generate-symbol
      (case-lambda
        (()
         (let ((octets (random-octets 16)) (name (make-string 32)))
           (do ((i 0 (+ i 1))) ((= i 16))
             (let ((o (bytevector-u8-ref octets i)))
               (string-set! name (* 2 i) (hex-digit (quotient o 16)))
               (string-set! name (+ (* 2 i) 1) (hex-digit (remainder o 16)))))
           (string->symbol (string-append "g:" name))))
        ((pretty-name)
         (unless (string? pretty-name) (error "generate-symbol: not a string" pretty-name))
         (generate-symbol))))))
