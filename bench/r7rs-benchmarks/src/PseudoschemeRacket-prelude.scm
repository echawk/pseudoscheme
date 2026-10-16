#lang racket/base
;; PSEUDOSCHEME: the R7RS names the benchmarks use, for racket/base
;; (Racket's own #lang r7rs is a separate package).  Shared by
;; pseudoscheme-racket and racket-base.  Pairs are Racket's immutable
;; ones; set-car! and set-cdr! mutate them anyway, unsafely.
(require (only-in racket/unsafe/ops
                  unsafe-set-immutable-car! unsafe-set-immutable-cdr!)
         (only-in racket/vector vector-map vector-copy)
         (only-in racket/base [if r:if]))
;; R7RS's if may leave out the else
(define-syntax if
  (syntax-rules ()
    [(_ c t) (r:if c t (void))]
    [(_ c t e) (r:if c t e)]))
(define-syntax-rule (import . _) (begin))
(define (exact x) (inexact->exact x))
(define (inexact x) (exact->inexact x))
(define (square x) (* x x))
(define (current-jiffy) (current-inexact-milliseconds))
(define (jiffies-per-second) 1000)
(define (current-second) (/ (current-inexact-milliseconds) 1000.))
(define (flush-output-port [p (current-output-port)]) (flush-output p))
(define (set-car! p v) (unsafe-set-immutable-car! p v))
(define (set-cdr! p v) (unsafe-set-immutable-cdr! p v))
(define (exact-integer-sqrt n) (integer-sqrt/remainder n))
(define (truncate/ n d) (quotient/remainder n d))
(define (floor-quotient n d) (floor (/ n d)))
(define (floor-remainder n d) (modulo n d))
(define (truncate-quotient n d) (quotient n d))
(define (truncate-remainder n d) (remainder n d))
(define (list-copy l) (if (pair? l) (cons (car l) (list-copy (cdr l))) l))
(define (string-for-each f s . ss)
  (apply for-each f (string->list s) (map string->list ss)))
(define (string-map f s . ss)
  (list->string (apply map f (string->list s) (map string->list ss))))
(define (vector-for-each f v . vs)
  (apply for-each f (vector->list v) (map vector->list vs)))
(define (bytevector . bs) (apply bytes bs))
(define bytevector? bytes?)
(define make-bytevector make-bytes)
(define bytevector-length bytes-length)
(define bytevector-u8-ref bytes-ref)
(define bytevector-u8-set! bytes-set!)
(define (utf8->string b [s 0] [e (bytes-length b)]) (bytes->string/utf-8 b #f s e))
(define (string->utf8 str [s 0] [e (string-length str)]) (string->bytes/utf-8 str #f s e))
(define (read-u8 [p (current-input-port)]) (read-byte p))
(define (write-u8 b [p (current-output-port)]) (write-byte b p))
;; fields are found by position in the field list
(define (field-index f fields)
  (let loop ([fs fields] [i 0]) (if (eq? (car fs) f) i (loop (cdr fs) (+ i 1)))))
(define-syntax define-record-type
  (syntax-rules ()
    [(_ type (ctor cf ...) pred (field acc . mod) ...)
     (begin
       (define fields '(field ...))
       (define-values (st make pred ref set)
         (make-struct-type 'type #f (length fields) 0 #f '() #f))
       (define (ctor cf ...) (make (record-arg field (cf ...)) ...))
       (define-record-field ref set (field-index 'field fields) acc . mod) ...)]))
;; a field's constructor argument, or #f when the constructor has none
(define-syntax record-arg
  (syntax-rules ()
    [(_ f ()) #f]
    [(_ f (c . cs))
     (let-syntax ([same? (syntax-rules (f) [(_ f) c] [(_ x) (record-arg f cs)])])
       (same? c))]))
(define-syntax define-record-field
  (syntax-rules ()
    [(_ ref set i acc) (define acc (make-struct-field-accessor ref i))]
    [(_ ref set i acc mod)
     (begin (define acc (make-struct-field-accessor ref i))
            (define mod (make-struct-field-mutator set i)))]))
