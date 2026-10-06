;;; SRFI 270: Hexadecimal floating-point constants.
;;;
;;; The syntax -- #x1.8p3, and string->number in radix 16 -- is the
;;; reader's (parse-hex-float in src/numbers.lisp), including the R6RS
;;; form with an exponent marker after p (#x1.921fb54442d18pd+1).
;;;
;;; write-hexadecimal-float is from the SRFI's sample implementation (an
;;; R6RS library, lib/srfi/:270.sls), Copyright 2026 Peter McGoron, MIT
;;; licence:
;;;
;;;   Permission is hereby granted, free of charge, to any person obtaining a
;;;   copy of this software and associated documentation files (the
;;;   "Software"), to deal in the Software without restriction, including
;;;   without limitation the rights to use, copy, modify, merge, publish,
;;;   distribute, sublicense, and/or sell copies of the Software, and to
;;;   permit persons to whom the Software is furnished to do so, subject to
;;;   the following conditions:
;;;
;;;   The above copyright notice and this permission notice shall be included
;;;   in all copies or substantial portions of the Software.
;;;
;;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;;   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;
;;; Changes: it is a define-library body here, importing the (rnrs ...)
;;; libraries it uses; to-hex-digit is a string lookup; and the "i" of a
;;; complex number is written to PORT (the sample writes it to the
;;; current output port).
(define-library (srfi 270)
  (export write-hexadecimal-float)
  (import (rnrs base) (rnrs control) (rnrs io simple) (rnrs bytevectors)
          (rnrs arithmetic fixnums) (rnrs exceptions))
  (begin
    (define (to-hex-digit n) (string-ref "0123456789ABCDEF" n))

    (define (decode-float fv)
      (let ((bv (make-bytevector 8)))
        (bytevector-ieee-double-set! bv 0 fv (endianness big))
        (let ((sign? (fxbit-set? (bytevector-u8-ref bv 0) 7))
              (exponent (fxior
                         (fxarithmetic-shift-left
                          (fxand (bytevector-u8-ref bv 0) #x7F)
                          4)
                         (fxarithmetic-shift-right
                          (bytevector-u8-ref bv 1)
                          4)))
              (mantissa (do ((i 2 (fx+ i 1))
                             (m (fxand (bytevector-u8-ref bv 1) #xF)
                                (fxior
                                 (fxarithmetic-shift-left m 8)
                                 (bytevector-u8-ref bv i))))
                            ((fx=? i 8) m))))
          (values mantissa (- exponent 1023) (if sign? -1 1)))))

    (define write-hexadecimal-float
      (case-lambda
        ((n) (write-hexadecimal-float n (current-output-port)))
        ((n port)
         (cond
           ((nan? n) (display "+nan.0" port))
           ((equal? n +inf.0) (display "+inf.0" port))
           ((equal? n -inf.0) (display "-inf.0" port))
           ((eqv? n 0.0) (display "0p0" port))
           ((eqv? n -0.0) (display "-0p0" port))
           ((and (complex? n) (not (real? n)))
            (write-hexadecimal-float (real-part n) port)
            (unless (negative? (imag-part n))
              (display "+" port))
            (write-hexadecimal-float (imag-part n) port)
            (display "i" port))
           ((inexact? n)    ; Assuming flonum
            (let-values (((m e sign) (decode-float n)))
              (when (negative? sign)
                (display "-" port))
              (if (= e -1023)
                  (display "0." port)
                  (display "1." port))
              (do ((l '()
                      (let ((n (mod m #x10)))
                        (if (and (null? l) (zero? n))
                            l
                            (cons (to-hex-digit n) l))))
                   (i 0 (fx+ i 1))
                   (m m (div m #x10)))
                  ((= i 52/4)
                   (display (list->string l) port)))
              (display "p" port)
              (if (= e -1023)
                  (display -1022 port)
                  (display e port))))
           ((exact? n) (write-hexadecimal-float (inexact n) port))
           (else (assertion-violation 'write-hexadecimal-float
                                      "not a number"
                                      n))))))))
