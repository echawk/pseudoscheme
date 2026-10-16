;;; -*- Mode: Scheme -*-
;;;; Part of the body of Pseudoscheme's (chezscheme) library: more of
;;;; the names Chez has that its mats use, which the other files don't
;;;; have: the r6rs: versions of procedures Chez extends, the old hash
;;;; table interface, the hashtable cells, signed byte lists.

;;; The R6RS procedures (rnrs) has under their own names, which Chez
;;; extends, and its r6rs: names for them.  Here the extensions are made
;;; to the R6RS procedures, so these are the strict versions.
(define (r6rs-number-check who args)
  (for-each (lambda (x)
              (unless (number? x) (assertion-violation who "not a number" x)))
            args))
(define-syntax define-r6rs-comparison
  (syntax-rules ()
    ((_ name op)
     (define name
       (case-lambda
         ((x y) (r6rs-number-check 'name (list x y)) (op x y))
         ((x y . rest)
          (r6rs-number-check 'name (cons x (cons y rest)))
          (apply op x y rest)))))))
(define-r6rs-comparison r6rs:= =)
(define-r6rs-comparison r6rs:< <)
(define-r6rs-comparison r6rs:> >)
(define-r6rs-comparison r6rs:<= <=)
(define-r6rs-comparison r6rs:>= >=)

(define (r6rs-radix who radix)
  (unless (memv radix '(2 8 10 16))
    (assertion-violation who "invalid radix" radix)))
;; R6RS number syntax has no # digit placeholders
(define (r6rs-placeholder? s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (and (< i n)
           (or (and (char=? (string-ref s i) #\#)
                    (not (and (< (+ i 1) n)
                              (memv (char-downcase (string-ref s (+ i 1))) '(#\b #\o #\d #\x #\e #\i)))))
               (loop (+ i 1)))))))
(define r6rs:string->number
  (case-lambda
    ((s) (r6rs:string->number s 10))
    ((s radix)
     (unless (string? s) (assertion-violation 'r6rs:string->number "not a string" s))
     (r6rs-radix 'r6rs:string->number radix)
     (and (not (r6rs-placeholder? s)) (string->number s radix)))))
(define r6rs:number->string
  (case-lambda
    ((z) (r6rs:number->string z 10))
    ((z radix) (r6rs:number->string z radix #f))
    ((z radix precision)
     (unless (number? z) (assertion-violation 'r6rs:number->string "not a number" z))
     (r6rs-radix 'r6rs:number->string radix)
     (number->string z radix))))
(define (r6rs:eval x env) (eval x env))
(define r6rs:hashtable-keys hashtable-keys)
(define r6rs:hashtable-entries hashtable-entries)
(define r6rs:flush-output-port flush-output-port)
(define (r6rs:delete-file path) (delete-file path #t))
(define (r6rs:file-exists? path) (file-exists? path))
(define r6rs:exit exit)
(define r6rs:standard-input-port standard-input-port)
(define r6rs:standard-output-port standard-output-port)
(define r6rs:standard-error-port standard-error-port)
(define r6rs:call-with-input-file call-with-input-file)
(define r6rs:call-with-output-file call-with-output-file)
(define r6rs:with-input-from-file with-input-from-file)
(define r6rs:with-output-to-file with-output-to-file)
(define r6rs:open-input-file open-input-file)
(define r6rs:open-output-file open-output-file)
(define r6rs:make-record-type-descriptor make-record-type-descriptor)
(define-syntax r6rs:case
  (syntax-rules () ((_ . x) (case . x))))

;;; Chez's old hash tables: eq hashtables with another interface
(define make-hash-table
  (case-lambda
    (() (make-eq-hashtable))
    ((weak?) (if weak? (make-weak-eq-hashtable) (make-eq-hashtable)))))
(define (hash-table? x) (eq-hashtable? x))
(define (check-old-hash-table who h)
  (unless (eq-hashtable? h) (assertion-violation who "not an eq hashtable" h)))
(define (put-hash-table! h k v)
  (check-old-hash-table 'put-hash-table! h)
  (hashtable-set! h k v))
(define (get-hash-table h k default)
  (check-old-hash-table 'get-hash-table h)
  (hashtable-ref h k default))
(define (remove-hash-table! h k)
  (check-old-hash-table 'remove-hash-table! h)
  (hashtable-delete! h k))
(define (hash-table-map h p)
  (check-old-hash-table 'hash-table-map h)
  (unless (procedure? p) (assertion-violation 'hash-table-map "not a procedure" p))
  (let-values (((keys vals) (hashtable-entries h)))
    (let f ((i (vector-length keys)) (ls '()))
      (if (= i 0)
          ls
          (let ((i (- i 1)))
            (f i (cons (p (vector-ref keys i) (vector-ref vals i)) ls)))))))
(define (hash-table-for-each h p)
  (check-old-hash-table 'hash-table-for-each h)
  (unless (procedure? p) (assertion-violation 'hash-table-for-each "not a procedure" p))
  (let-values (((keys vals) (hashtable-entries h)))
    (vector-for-each p keys vals)))

;;; Cells without adding one
(define (hashtable-ref-cell h k)
  (and (hashtable-contains? h k) (hashtable-cell h k #f)))
(define eq-hashtable-ref-cell hashtable-ref-cell)
(define symbol-hashtable-ref-cell hashtable-ref-cell)
(define (eq-hashtable-try-atomic-cell h k default) (hashtable-cell h k default))

;;; Signed byte lists
(define (bytevector->s8-list bv)
  (let loop ((i (- (bytevector-length bv) 1)) (ls '()))
    (if (< i 0) ls (loop (- i 1) (cons (bytevector-s8-ref bv i) ls)))))
(define (s8-list->bytevector ls)
  (let ((bv (make-bytevector (length ls))))
    (let loop ((i 0) (ls ls))
      (if (null? ls)
          bv
          (begin (bytevector-s8-set! bv i (car ls)) (loop (+ i 1) (cdr ls)))))))

;;; Generic ports (host.lisp): Chez's make-input-port and the like, with
;;; the accessors of their buffers; the textual- names are the same here.
(define make-input-port %chez:make-input-port)
(define make-output-port %chez:make-output-port)
(define make-input/output-port %chez:make-input/output-port)
(define port-handler %chez:port-handler)
(define port-input-buffer %chez:port-input-buffer)
(define port-input-size %chez:port-input-size)
(define port-input-index %chez:port-input-index)
(define port-input-count %chez:port-input-count)
(define port-input-empty? %chez:port-input-empty?)
(define port-output-buffer %chez:port-output-buffer)
(define port-output-size %chez:port-output-size)
(define port-output-index %chez:port-output-index)
(define set-port-input-index! %chez:set-port-input-index!)
(define set-port-input-size! %chez:set-port-input-size!)
(define set-port-input-buffer! %chez:set-port-input-buffer!)
(define set-port-output-index! %chez:set-port-output-index!)
(define set-port-output-size! %chez:set-port-output-size!)
(define set-port-output-buffer! %chez:set-port-output-buffer!)
(define mark-port-closed! %chez:mark-port-closed!)
(define port-name %chez:port-name)
(define set-port-name! %chez:set-port-name!)
(define textual-port-input-buffer %chez:port-input-buffer)
(define textual-port-input-size %chez:port-input-size)
(define textual-port-input-index %chez:port-input-index)
(define textual-port-input-count %chez:port-input-count)
(define textual-port-output-buffer %chez:port-output-buffer)
(define textual-port-output-size %chez:port-output-size)
(define textual-port-output-index %chez:port-output-index)
(define set-textual-port-input-index! %chez:set-port-input-index!)
(define set-textual-port-input-size! %chez:set-port-input-size!)
(define set-textual-port-input-buffer! %chez:set-port-input-buffer!)
(define set-textual-port-output-index! %chez:set-port-output-index!)
(define set-textual-port-output-size! %chez:set-port-output-size!)
(define set-textual-port-output-buffer! %chez:set-port-output-buffer!)
(define block-read
  (case-lambda
    ((p s) (%chez:block-read p s (string-length s)))
    ((p s n) (%chez:block-read p s n))))
(define block-write
  (case-lambda
    ((p s) (%chez:block-write p s (string-length s)))
    ((p s n) (%chez:block-write p s n))))
(define file-position
  (case-lambda
    ((p) (%chez:file-position p))
    ((p pos) (%chez:file-position p pos))))
(define file-length %chez:file-length)
