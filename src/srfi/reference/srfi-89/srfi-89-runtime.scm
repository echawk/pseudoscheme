; Procedures needed at run time (called by the expanded code):

; Perfect hash-tables with keyword keys.

(define ($hash-keyword key n)
  (let ((str (keyword->string key)))
    (let loop ((h 0) (i 0))
      (if (< i (string-length str))
          (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                        n)
                (+ i 1))
          h))))

(define ($perfect-hash-table-lookup table key)
  (let* ((n (quotient (vector-length table) 2))
         (x (* 2 ($hash-keyword key n))))
    (and (eq? (vector-ref table x) key)
         (vector-ref table (+ x 1)))))

; Handling of named parameters.

(define $undefined (list 'undefined))

(define ($req-key key-values i)
  (let ((val (vector-ref key-values i)))
    (if (eq? val $undefined)
        (error "a required named parameter was not provided")
        val)))

(define ($opt-key key-values i default)
  (let ((val (vector-ref key-values i)))
    (if (eq? val $undefined)
        (default)
        val)))

(define ($process-keys args key-hash-table key-values)
  (let loop ((args args))
    (if (null? args)
        args
        (let ((k (car args)))
          (if (not (keyword? k))
              args
              (let ((i ($perfect-hash-table-lookup key-hash-table k)))
                (if (not i)
                    (error "unknown parameter keyword" k)
                    (if (null? (cdr args))
                        (error "a value was expected after keyword" k)
                        (begin
                          (if (eq? (vector-ref key-values i) $undefined)
                              (vector-set! key-values i (cadr args))
                              (error "duplicate parameter" k))
                          (loop (cddr args)))))))))))

;------------------------------------------------------------------------------
