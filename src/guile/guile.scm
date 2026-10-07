;;; -*- Mode: Scheme -*-
;;;; (guile): the body of the library, which src/guile/guile.lisp makes
;;;; when (guile) is first imported, exporting R6RS and these.  Built on
;;;; what Pseudoscheme already has: (pseudoscheme control)'s prompts
;;;; (src/control.sls), (chezscheme)'s format, SRFI 1 and SRFI 13.

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules () ((_ . pattern) template))))))

(define (1+ n) (+ n 1))
(define (1- n) (- n 1))
(define (identity x) x)
(define (const x) (lambda args x))
(define (negate pred) (lambda args (not (apply pred args))))
(define (compose . procs)
  (if (null? procs)
      identity
      (let ((f (car procs)) (g (apply compose (cdr procs))))
        (lambda args (call-with-values (lambda () (apply g args)) f)))))

;;; catch and throw.  (throw key arg ...) raises a Guile exception;
;;; (catch key thunk handler) calls (handler key arg ...) for one of
;;; KEY, or of any key if KEY is #t.  Other conditions are caught by
;;; #t too, as Guile 3's catch does: an R6RS condition with a message
;;; as (misc-error who message irritants).

(define-record-type guile-exception
  (fields key args))

(define (throw key . args)
  (raise (make-guile-exception key args)))

(define (exception-key+args e)
  (cond ((guile-exception? e) (values (guile-exception-key e) (guile-exception-args e)))
        ((and (condition? e) (message-condition? e))
         (values 'misc-error
                 (list (and (who-condition? e) (condition-who e))
                       (condition-message e)
                       (if (irritants-condition? e) (condition-irritants e) '()))))
        (else (values 'non-condition (list e)))))

(define catch
  (case-lambda
    ((key thunk handler) (catch key thunk handler #f))
    ((key thunk handler pre-unwind)
     (guard (e ((let-values (((k args) (exception-key+args e)))
                  (or (eq? key #t) (eq? key k)))
                (let-values (((k args) (exception-key+args e)))
                  (apply handler k args))))
       (if pre-unwind
           (with-throw-handler key thunk pre-unwind)
           (thunk))))))

;; the handler runs where the exception is raised, before unwinding;
;; the exception then goes on to outer handlers
(define (with-throw-handler key thunk handler)
  (with-exception-handler
   (lambda (e)
     (let-values (((k args) (exception-key+args e)))
       (when (or (eq? key #t) (eq? key k))
         (apply handler k args)))
     (raise e))
   thunk))

(define-syntax false-if-exception
  (syntax-rules ()
    ((_ expr) (guard (e (#t #f)) expr))))

;;; Association lists

(define (alist-ref assx) (lambda (alist key) (let ((p (assx key alist))) (and p (cdr p)))))
(define assq-ref (alist-ref assq))
(define assv-ref (alist-ref assv))
(define assoc-ref (alist-ref assoc))
(define (acons key value alist) (cons (cons key value) alist))
(define (alist-set! assx)
  (lambda (alist key value)
    (let ((p (assx key alist)))
      (if p (begin (set-cdr! p value) alist) (acons key value alist)))))
(define assq-set! (alist-set! assq))
(define assv-set! (alist-set! assv))
(define assoc-set! (alist-set! assoc))
(define (alist-remove! same?)
  (lambda (alist key)
    (remp (lambda (p) (same? (car p) key)) alist)))
(define assq-remove! (alist-remove! eq?))
(define assv-remove! (alist-remove! eqv?))
(define assoc-remove! (alist-remove! equal?))

;;; Hash tables: R6RS hashtables.  hash-ref compares keys with equal?,
;;; hashq-ref with eq?, hashv-ref with eqv?; a table here is made for
;;; one of them (make-hash-table: equal?), as Guile's programs use them.

(define make-hash-table
  (case-lambda
    (() (make-hashtable equal-hash equal?))
    ((size) (make-hashtable equal-hash equal? size))))
(define (hash-table? x) (hashtable? x))
(define hash-ref
  (case-lambda
    ((table key) (hashtable-ref table key #f))
    ((table key default) (hashtable-ref table key default))))
(define (hash-set! table key value) (hashtable-set! table key value) value)
(define (hash-remove! table key) (hashtable-delete! table key))
(define (hash-create-handle! table key init)
  (unless (hashtable-contains? table key) (hashtable-set! table key init))
  (cons key (hashtable-ref table key init)))
(define hashq-ref hash-ref)
(define hashq-set! hash-set!)
(define hashq-remove! hash-remove!)
(define hashv-ref hash-ref)
(define hashv-set! hash-set!)
(define hashv-remove! hash-remove!)
(define (hash-for-each proc table)
  (let-values (((keys values) (hashtable-entries table)))
    (vector-for-each proc keys values)))
(define (hash-map->list proc table)
  (let-values (((keys values) (hashtable-entries table)))
    (map proc (vector->list keys) (vector->list values))))
(define (hash-fold proc init table)
  (let-values (((keys values) (hashtable-entries table)))
    (let loop ((i 0) (acc init))
      (if (= i (vector-length keys))
          acc
          (loop (+ i 1) (proc (vector-ref keys i) (vector-ref values i) acc))))))
(define hash-count
  (case-lambda
    ((table) (hashtable-size table))
    ((pred table) (hash-fold (lambda (k v n) (if (pred k v) (+ n 1) n)) 0 table))))
(define (hash-clear! table) (hashtable-clear! table))

;;; Strings and symbols

;; (string-split "a b" #\space) => ("a" "b"); the delimiter may be a
;; character, a list of them, or a predicate
(define (string-split s delimiter)
  (let ((delimiter? (cond ((char? delimiter) (lambda (c) (char=? c delimiter)))
                          ((list? delimiter) (lambda (c) (memv c delimiter)))
                          (else delimiter))))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((= i (string-length s))
             (reverse (cons (substring s start i) acc)))
            ((delimiter? (string-ref s i))
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

;;; Output

;; simple-format: ~a and ~s only, as Guile's
(define (simple-format dest fmt . args) (apply format dest fmt args))

;; (pk x ...): write ";;; (x ...)" to the error port and return the last
(define (pk . xs)
  (let ((p (current-error-port)))
    (display ";;; " p) (write xs p) (newline p))
  (if (null? xs) (if #f #f) (car (reverse xs))))
