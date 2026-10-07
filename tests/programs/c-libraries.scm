;;; Scheme calling C through CFFI: zlib and the C library.
;;;
;;;   bin/pseudoscheme --quicklisp tests/programs/c-libraries.scm
;;;
;;; CFFI's macros (defcfun, defcstruct, defcallback, with-foreign-object,
;;; foreign-funcall) are used from Scheme as Lisp macros; their bodies
;;; are Lisp code that sees Scheme's variables and procedures.  Bytes
;;; cross between Scheme bytevectors and foreign memory both ways, C
;;; calls back into Scheme closures (qsort), and C structs are filled
;;; from Scheme (struct tm).  Results are checked against the same
;;; computations written in Scheme.

(import (scheme base) (scheme inexact) (scheme process-context)
        (srfi 1) (srfi 64) (only (srfi 151) bitwise-xor bitwise-and arithmetic-shift)
        (pseudoscheme lisp)
        (prefix (cl common-lisp) cl:)
        (prefix (cl cffi) ffi:))

(test-begin "c-libraries")

(ffi:define-foreign-library libz
  (#:darwin "libz.dylib")
  (#:unix (#:or "libz.so.1" "libz.so"))
  (cl:t (#:default "libz")))
(ffi:use-foreign-library libz)

;; (define-c (name "c_name") return-type (arg type) ...) binds the C
;; function as a Lisp function, with DEFCFUN, and NAME as the Scheme
;; procedure that calls it.
(define-syntax define-c
  (syntax-rules ()
    ((_ (name c-name) return (arg type) ...)
     (begin
       (ffi:defcfun (c-name name) return (arg type) ...)
       (define name (lisp-function 'name))))))

;;; ------------------------------------------------------------------
;;; zlib: bytevectors through foreign memory

(define-c (zlib-version "zlibVersion") #:string)
(define-c (compress-bound "compressBound") #:unsigned-long (n #:unsigned-long))
(define-c (compress2 "compress2") #:int
  (dest #:pointer) (dest-len #:pointer) (src #:pointer) (src-len #:unsigned-long) (level #:int))
(define-c (uncompress "uncompress") #:int
  (dest #:pointer) (dest-len #:pointer) (src #:pointer) (src-len #:unsigned-long))
(define-c (zlib-crc32 "crc32") #:unsigned-long (crc #:unsigned-long) (buf #:pointer) (len #:unsigned-int))
(define-c (zlib-adler32 "adler32") #:unsigned-long (adler #:unsigned-long) (buf #:pointer) (len #:unsigned-int))

(define (pointer->bytevector pointer n)
  (let ((bv (make-bytevector n)))
    (do ((i 0 (+ i 1))) ((= i n) bv)
      (bytevector-u8-set! bv i (ffi:mem-aref pointer #:uint8 i)))))

;; Call (f pointer) with a pointer to BV's bytes, which C may read
;; (SBCL's octet vectors can be pinned and shared with C).
(define (with-bytes bv f)
  (ffi:with-pointer-to-vector-data (p bv) (f p)))

(define (zlib-error who code)
  (error (string-append who ": zlib error") code))

(define (deflate bv level)
  (let ((capacity (compress-bound (bytevector-length bv))))
    (ffi:with-foreign-objects ((out #:uint8 capacity) (out-len #:unsigned-long))
      (cl:setf (ffi:mem-ref out-len #:unsigned-long) capacity)
      (let ((code (with-bytes bv (lambda (in) (compress2 out out-len in (bytevector-length bv) level)))))
        (if (zero? code)
            (pointer->bytevector out (ffi:mem-ref out-len #:unsigned-long))
            (zlib-error "compress2" code))))))

(define (inflate bv size)
  (ffi:with-foreign-objects ((out #:uint8 size) (out-len #:unsigned-long))
    (cl:setf (ffi:mem-ref out-len #:unsigned-long) size)
    (let ((code (with-bytes bv (lambda (in) (uncompress out out-len in (bytevector-length bv))))))
      (if (zero? code)
          (pointer->bytevector out (ffi:mem-ref out-len #:unsigned-long))
          (zlib-error "uncompress" code)))))

(define (crc32/c bv) (with-bytes bv (lambda (p) (zlib-crc32 0 p (bytevector-length bv)))))
(define (adler32/c bv) (with-bytes bv (lambda (p) (zlib-adler32 1 p (bytevector-length bv)))))

;; The same checksums in Scheme
(define crc-table
  (let ((table (make-vector 256)))
    (do ((n 0 (+ n 1))) ((= n 256) table)
      (vector-set! table n
                   (let loop ((c n) (k 0))
                     (cond ((= k 8) c)
                           ((odd? c) (loop (bitwise-xor #xEDB88320 (arithmetic-shift c -1)) (+ k 1)))
                           (else (loop (arithmetic-shift c -1) (+ k 1)))))))))

(define (crc32/scheme bv)
  (let loop ((i 0) (c #xFFFFFFFF))
    (if (= i (bytevector-length bv))
        (bitwise-xor c #xFFFFFFFF)
        (loop (+ i 1)
              (bitwise-xor (vector-ref crc-table (bitwise-and #xFF (bitwise-xor c (bytevector-u8-ref bv i))))
                           (arithmetic-shift c -8))))))

(define (adler32/scheme bv)
  (let loop ((i 0) (a 1) (b 0))
    (if (= i (bytevector-length bv))
        (+ (* b 65536) a)
        (let ((a (modulo (+ a (bytevector-u8-ref bv i)) 65521)))
          (loop (+ i 1) a (modulo (+ b a) 65521))))))

;; Some text with structure (so it compresses) and non-ASCII characters
(define text
  (string->utf8
   (apply string-append
          (map (lambda (i)
                 (string-append "line " (number->string i) ": "
                                (if (even? i) "λx.x applied to " "Grüße, ")
                                (number->string (* i i)) "\n"))
               (iota 500)))))

(test-assert "zlib has a version" (string? (zlib-version)))
(test-equal "compress then uncompress" text
            (inflate (deflate text 9) (bytevector-length text)))
(test-assert "compression helps"
             (< (bytevector-length (deflate text 9)) (quotient (bytevector-length text) 3)))
(test-assert "level 9 is no bigger than level 1"
             (<= (bytevector-length (deflate text 9)) (bytevector-length (deflate text 1))))
(test-equal "an empty bytevector" (bytevector) (inflate (deflate (bytevector) 6) 10))
(test-equal "the zlib header" #x78 (bytevector-u8-ref (deflate text 6) 0))
(test-equal "CRC-32 of the check string" #xCBF43926 (crc32/c (string->utf8 "123456789")))
(test-equal "CRC-32, C and Scheme agree" (crc32/scheme text) (crc32/c text))
(test-equal "Adler-32, C and Scheme agree" (adler32/scheme text) (adler32/c text))
(test-equal "Adler-32 is the stream's trailer"
            (adler32/c text)
            (let* ((z (deflate text 9)) (n (bytevector-length z)))
              (fold (lambda (i acc) (+ (* acc 256) (bytevector-u8-ref z i)))
                    0 (iota 4 (- n 4)))))
(test-error "corrupt input is a zlib error, raised in Scheme" #t
            (inflate (bytevector 1 2 3 4 5) 100))
(test-equal "a raised zlib error is a Scheme error object"
            '("uncompress: zlib error" (-3))
            (guard (e ((error-object? e) (list (error-object-message e) (error-object-irritants e))))
              (inflate (bytevector #x78 #x9c 1 2 3 4 5) 100)))

;;; ------------------------------------------------------------------
;;; qsort: C calling back into Scheme

;; The comparison is a Scheme procedure, kept in a Lisp special
;; variable that the C callback reads, so each sort can use its own.
(cl:defvar *compare* cl:nil)

(ffi:defcallback compare-ints #:int ((a #:pointer) (b #:pointer))
  (cl:funcall *compare* (ffi:mem-ref a #:int) (ffi:mem-ref b #:int)))

(define (c-sort numbers compare)
  (let ((n (length numbers)))
    (ffi:with-foreign-object (array #:int n)
      (for-each (lambda (x i) (cl:setf (ffi:mem-aref array #:int i) x))
                numbers (iota n))
      (cl:let ((*compare* compare))
        (ffi:foreign-funcall "qsort" #:pointer array #:size n #:size (ffi:foreign-type-size #:int)
                             #:pointer (ffi:callback compare-ints) #:void))
      (map (lambda (i) (ffi:mem-aref array #:int i)) (iota n)))))

(define (three-way < a b) (cond ((< a b) -1) ((< b a) 1) (else 0)))

(define numbers '(31 -4 15 92 -65 35 89 79 32 38 46 -26 43 38 32 79 50 28 84 19))

(test-equal "qsort with a Scheme comparison"
            (cl:sort (list-copy numbers) cl:<)
            (c-sort numbers (lambda (a b) (three-way < a b))))
(test-equal "descending, by a closure over a Scheme procedure"
            (cl:sort (list-copy numbers) cl:>)
            (c-sort numbers (lambda (a b) (three-way > a b))))
(test-equal "by absolute value, then sign"
            '(-4 15 19 -26 28 31 32 32 35 38 38 43 46 50 -65 79 79 84 89 92)
            (c-sort numbers (lambda (a b)
                              (if (= (abs a) (abs b)) (three-way < a b) (three-way < (abs a) (abs b))))))
(test-assert "the comparison's state, counted in Scheme"
             (let ((calls 0))
               (c-sort numbers (lambda (a b) (set! calls (+ calls 1)) (three-way < a b)))
               (> calls (length numbers))))
(test-equal "a Scheme escape out of C's qsort" 'escaped
            (call/cc (lambda (k) (c-sort numbers (lambda (a b) (k 'escaped))))))
(test-equal "and qsort still works afterwards" '(1 2 3)
            (c-sort '(3 1 2) (lambda (a b) (three-way < a b))))

;;; ------------------------------------------------------------------
;;; struct tm: a C struct filled from Scheme

;; (Slot names that are Scheme variables, like min, would be Scheme's
;; min inside the Lisp form; C's own names avoid that.)
(ffi:defcstruct tm
  (tm-sec #:int) (tm-min #:int) (tm-hour #:int) (tm-mday #:int) (tm-mon #:int)
  (tm-year #:int) (tm-wday #:int) (tm-yday #:int) (tm-isdst #:int)
  (tm-gmtoff #:long) (tm-zone #:pointer))

(define-c (timegm "timegm") #:long (tm #:pointer))
(define-c (gmtime-r "gmtime_r") #:pointer (time #:pointer) (result #:pointer))
(define-c (strftime "strftime") #:size
  (buffer #:pointer) (size #:size) (format #:string) (tm #:pointer))

;; Seconds since 1970 by C's timegm
(define (seconds/c year month day hour minute second)
  (ffi:with-foreign-object (tm '(#:struct tm))
    (cl:loop for i below (ffi:foreign-type-size '(#:struct tm)) do (cl:setf (ffi:mem-aref tm #:uint8 i) 0))
    (ffi:with-foreign-slots ((tm-sec tm-min tm-hour tm-mday tm-mon tm-year) tm (#:struct tm))
      (cl:setf tm-sec second tm-min minute tm-hour hour
               tm-mday day tm-mon (- month 1) tm-year (- year 1900)))
    (timegm tm)))

;; ... and by Scheme: days from the civil calendar (Howard Hinnant's)
(define (days-from-civil y m d)
  (let* ((y (if (<= m 2) (- y 1) y))
         (era (floor-quotient y 400))
         (yoe (- y (* era 400)))
         (doy (+ (quotient (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- d 1)))
         (doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(define (seconds/scheme year month day hour minute second)
  (+ (* 86400 (days-from-civil year month day)) (* 3600 hour) (* 60 minute) second))

;; C's strftime, with the result read back as a Scheme string
(define (format-time seconds format)
  (ffi:with-foreign-objects ((time #:long) (tm '(#:struct tm)) (buffer #:char 100))
    (cl:setf (ffi:mem-ref time #:long) seconds)
    (gmtime-r time tm)
    (let ((n (strftime buffer 100 format tm)))
      (ffi:foreign-string-to-lisp buffer #:count n))))

(define dates
  '((1970 1 1 0 0 0) (2000 2 29 12 30 45) (1999 12 31 23 59 59)
    (2038 1 19 3 14 8) (1960 6 15 8 0 0) (2026 10 6 21 55 39)))

(test-equal "timegm (C) and the civil calendar (Scheme) agree"
            (map (lambda (d) (apply seconds/scheme d)) dates)
            (map (lambda (d) (apply seconds/c d)) dates))
(test-equal "strftime on a time computed in Scheme"
            "Tuesday 2026-10-06 21:55:39"
            (format-time (seconds/scheme 2026 10 6 21 55 39) "%A %Y-%m-%d %H:%M:%S"))
(test-equal "the day of the year, from gmtime_r's struct"
            60
            (ffi:with-foreign-objects ((time #:long) (tm '(#:struct tm)))
              (cl:setf (ffi:mem-ref time #:long) (seconds/scheme 2000 2 29 0 0 0))
              (gmtime-r time tm)
              (+ 1 (ffi:foreign-slot-value tm '(#:struct tm) 'tm-yday))))

;;; ------------------------------------------------------------------
;;; strings: UTF-8 both ways, and C's own string functions

(define-c (strlen "strlen") #:size (s #:string))
(define-c (strtol "strtol") #:long (s #:string) (end #:pointer) (base #:int))
(define-c (getenv/c "getenv") #:string (name #:string))

(test-equal "strlen counts UTF-8 bytes" (bytevector-length (string->utf8 "Grüße λ")) (strlen "Grüße λ"))
(test-equal "strtol in several bases" '(255 255 -42 493)
            (list (strtol "ff" (ffi:null-pointer) 16) (strtol "0xff" (ffi:null-pointer) 0)
                  (strtol "  -42" (ffi:null-pointer) 10) (strtol "755" (ffi:null-pointer) 8)))
(test-equal "getenv: C and Scheme see the same environment"
            (get-environment-variable "HOME") (getenv/c "HOME"))
(test-equal "a NULL char* is ()" '() (getenv/c "PSEUDOSCHEME_SURELY_UNSET_VARIABLE"))
(test-equal "snprintf, a varargs function"
            "pi is about 3.14159, 255 is ff"
            (ffi:with-foreign-object (buffer #:char 64)
              ;; The variadic arguments are marked as such: on some
              ;; platforms (Apple's arm64) they're passed differently.
              ;; Longs, which fill the slot each takes there.
              (ffi:foreign-funcall-varargs
               "snprintf" (#:pointer buffer #:size 64 #:string "%s is about %.5f, %ld is %lx")
               #:string "pi" #:double (* 4 (atan 1)) #:long 255 #:long 255 #:int)
              (ffi:foreign-string-to-lisp buffer)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "c-libraries")
  (exit (if (zero? failures) 0 1)))
