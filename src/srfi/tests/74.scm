;;; SRFI 74 tests: the SRFI's example.scm, with its check on SRFI 64,
;;; and a few more for what it leaves out.
;;;
;; Copyright (C) Michael Sperber (2005). All Rights Reserved. 
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (scheme base) (scheme process-context) (srfi 64) (srfi 74))

(define-syntax check
  (syntax-rules (=>)
    ((_ expr => expected) (test-equal expected expr))))

(test-begin "srfi-74")

(define b1 (make-blob 16))

(check (blob-length b1) => 16)

(blob-u8-set! b1 0 223)
(blob-s8-set! b1 1 123)
(blob-s8-set! b1 2 -123)
(blob-u8-set! b1 3 15)

(check (list (blob-u8-ref b1 0)
	     (blob-s8-ref b1 1)
	     (blob-u8-ref b1 1)
	     (blob-s8-ref b1 2)
	     (blob-u8-ref b1 2)
	     (blob-u8-ref b1 3))
       => '(223 123 123 -123 133 15))

(blob-uint-set! 16 (endianness little)
		b1 0 (- (expt 2 128) 3))

(check (blob-uint-ref 16 (endianness little) b1 0)
       =>  (- (expt 2 128) 3))

(check (blob-sint-ref 16 (endianness little) b1 0)
       =>  -3)
		
(check (blob->u8-list b1)
       => '(253 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255))

(blob-uint-set! 16 (endianness big)
		b1 0 (- (expt 2 128) 3))

(check (blob-uint-ref 16 (endianness big) b1 0)
       =>  (- (expt 2 128) 3))

(check (blob-sint-ref 16 (endianness big) b1 0)
       =>  -3)

(check (blob->u8-list b1)
       => '(255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253))

(check (blob-u16-ref (endianness little) b1 14)
       => 65023)
(check (blob-s16-ref (endianness little) b1 14)
       => -513)
(check (blob-u16-ref (endianness big) b1 14)
       => 65533)
(check (blob-s16-ref (endianness big) b1 14)
       => -3)

(blob-u16-set! (endianness little) b1 0 12345)

(blob-u16-native-set! b1 0 12345)

(check (blob-u16-native-ref b1 0)
       => 12345)

(check (blob-u32-ref (endianness little) b1 12)
       => 4261412863)
(check (blob-s32-ref (endianness little) b1 12)
       => -33554433)
(check (blob-u32-ref (endianness big) b1 12)
       => 4294967293)
(check (blob-s32-ref (endianness big) b1 12)
       => -3)

(blob-u32-set! (endianness little) b1 0 12345)

(blob-u32-native-set! b1 0 12345)

(check (blob-u32-native-ref b1 0)
       => 12345)

(check (blob-u64-ref (endianness little) b1 8)
       => 18302628885633695743)
(check (blob-s64-ref (endianness little) b1 8)
       => -144115188075855873)
(check (blob-u64-ref (endianness big) b1 8)
       => 18446744073709551613)
(check (blob-s64-ref (endianness big) b1 8)
       => -3)

(blob-u64-set! (endianness little) b1 0 12345)

(blob-u64-native-set! b1 0 12345)

(check (blob-u64-native-ref b1 0)
       => 12345)

(define b2 (u8-list->blob '(1 2 3 4 5 6 7 8)))
(define b3 (blob-copy b2))

(check (blob=? b2 b3) => #t)
(check (blob=? b1 b2) => #f)

(blob-copy! b3 0 b3 4 4)

(check (blob->u8-list b3) => '(1 2 3 4 1 2 3 4))

(blob-copy! b3 0 b3 2 6)

(check (blob->u8-list b3) => '(1 2 1 2 3 4 1 2))

(blob-copy! b3 2 b3 0 6)

(check (blob->u8-list b3) => '(1 2 3 4 1 2 1 2))

(check (blob->uint-list 1 (endianness little) b3)
       => '(1 2 3 4 1 2 1 2))

(check (blob->uint-list 2 (endianness little) b3)
       => '(513 1027 513 513))

(define b4 (u8-list->blob '(0 0 0 0 0 0 48 57 255 255 255 255 255 255 255 253)))

(check (blob->sint-list 2 (endianness little) b4)
       => '(0 0 0 14640 -1 -1 -1 -513))


;; Not in example.scm.
(check (blob? b1) => #t)
(check (blob? (vector 1 2)) => #f)
(check (bytevector? (make-blob 2)) => #t)
(check (blob->u8-list (make-blob 3)) => '(0 0 0))
(check (memq (endianness native) '(big little)) => (list (endianness native)))
(check (blob->s8-list (s8-list->blob '(-1 2 -128))) => '(-1 2 -128))
(check (blob->u8-list (uint-list->blob 2 (endianness big) '(1 258)))
       => '(0 1 1 2))
(check (blob->sint-list 2 (endianness big) (sint-list->blob 2 (endianness big) '(-2 3)))
       => '(-2 3))
(let ((b (make-blob 4)))
  (blob-s16-set! (endianness big) b 0 -2)
  (blob-u16-set! (endianness little) b 2 258)
  (check (blob->u8-list b) => '(255 254 2 1))
  (blob-sint-set! 3 (endianness little) b 1 -2)
  (check (blob-sint-ref 3 (endianness little) b 1) => -2)
  (check (blob-uint-ref 3 (endianness little) b 1) => (- (expt 2 24) 2)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-74")
  (exit (if (zero? failures) 0 1)))
