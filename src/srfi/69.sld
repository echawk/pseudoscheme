;;; SRFI 69: basic hash tables.  Written for Pseudoscheme on top of R6RS
;;; (rnrs hashtables), after the interface of Panu Kalliokoski's
;;; reference implementation (MIT licence, in the SRFI document), which
;;; is not used: its hash-by-identity is its equal?-hash, so eq? tables
;;; hash by content (and cannot hold procedures as keys at all).
;;;
;;; An SRFI 69 table is a record wrapping an R6RS hashtable plus the
;;; equivalence predicate and hash function it was made with:
;;; - eq? and eqv? tables are R6RS eq/eqv hashtables (true identity
;;;   hashing), whatever hash function was given, since any acceptable
;;;   one agrees with them;
;;; - equal?, string=? and string-ci=? tables without an explicit hash use
;;;   R6RS equal-hash, string-hash and string-ci-hash;
;;; - any other table calls its hash function as (hash key bound), as the
;;;   reference implementation does, so hash functions that require the
;;;   bound (e.g. modulo) work too.  With no hash function given, hash
;;;   (equal?-hashing) is used.
;;; hash-by-identity is hash: acceptable for eq?, as the SRFI requires.
;;; string-hash conflicts with SRFI 13's: exclude one when importing both.
(define-library (srfi 69)
  (export make-hash-table hash-table? alist->hash-table
          hash-table-equivalence-function hash-table-hash-function
          hash-table-ref hash-table-ref/default hash-table-set!
          hash-table-delete! hash-table-exists?
          hash-table-update! hash-table-update!/default
          hash-table-size hash-table-keys hash-table-values
          hash-table-walk hash-table-fold hash-table->alist
          hash-table-copy hash-table-merge!
          hash string-hash string-ci-hash hash-by-identity)
  (import (scheme base)
          (only (scheme char) string-ci=?)
          (prefix (only (rnrs hashtables)
                        make-eq-hashtable make-eqv-hashtable make-hashtable
                        hashtable-ref hashtable-set! hashtable-delete!
                        hashtable-contains? hashtable-size hashtable-copy
                        hashtable-keys hashtable-entries
                        equal-hash string-hash string-ci-hash)
                  r6:))
  (begin
    (define *default-bound* (- (expt 2 29) 3))

    (define (bound-of maybe-bound)
      (if (pair? maybe-bound) (car maybe-bound) *default-bound*))

    (define (hash obj . maybe-bound)
      (modulo (r6:equal-hash obj) (bound-of maybe-bound)))

    (define (string-hash s . maybe-bound)
      (modulo (r6:string-hash s) (bound-of maybe-bound)))

    (define (string-ci-hash s . maybe-bound)
      (modulo (r6:string-ci-hash s) (bound-of maybe-bound)))

    (define hash-by-identity hash)

    (define-record-type <srfi-69-hash-table>
      (%make-hash-table table equiv hash)
      hash-table?
      (table hash-table-table)
      (equiv hash-table-equivalence-function)
      (hash hash-table-hash-function))

    (define (check-table who t)
      (if (not (hash-table? t)) (error "not a hash table" who t)))

    ;; (make-hash-table [equiv [hash . args]])
    (define (make-hash-table . args)
      (let* ((equiv (if (pair? args) (car args) equal?))
             (given (and (pair? args) (pair? (cdr args)) (cadr args))))
        (cond ((eq? equiv eq?)
               (%make-hash-table (r6:make-eq-hashtable) equiv
                                 (or given hash-by-identity)))
              ((eq? equiv eqv?)
               (%make-hash-table (r6:make-eqv-hashtable) equiv (or given hash)))
              ((and (not given) (eq? equiv equal?))
               (%make-hash-table (r6:make-hashtable r6:equal-hash equal?)
                                 equiv hash))
              ((and (not given) (eq? equiv string=?))
               (%make-hash-table (r6:make-hashtable r6:string-hash string=?)
                                 equiv string-hash))
              ((and (not given) (eq? equiv string-ci=?))
               (%make-hash-table (r6:make-hashtable r6:string-ci-hash string-ci=?)
                                 equiv string-ci-hash))
              (else
               (let ((h (or given hash)))
                 (%make-hash-table
                  (r6:make-hashtable (lambda (key) (h key *default-bound*)) equiv)
                  equiv h))))))

    (define missing (list 'missing))

    (define (hash-table-ref table key . maybe-thunk)
      (check-table 'hash-table-ref table)
      (let ((v (r6:hashtable-ref (hash-table-table table) key missing)))
        (cond ((not (eq? v missing)) v)
              ((pair? maybe-thunk) ((car maybe-thunk)))
              (else (error "hash-table-ref: no value associated with" key)))))

    (define (hash-table-ref/default table key default)
      (check-table 'hash-table-ref/default table)
      (r6:hashtable-ref (hash-table-table table) key default))

    (define (hash-table-set! table key value)
      (check-table 'hash-table-set! table)
      (r6:hashtable-set! (hash-table-table table) key value))

    (define (hash-table-delete! table key)
      (check-table 'hash-table-delete! table)
      (r6:hashtable-delete! (hash-table-table table) key))

    (define (hash-table-exists? table key)
      (check-table 'hash-table-exists? table)
      (r6:hashtable-contains? (hash-table-table table) key))

    (define (hash-table-update! table key function . maybe-thunk)
      (check-table 'hash-table-update! table)
      (let* ((ht (hash-table-table table))
             (v (r6:hashtable-ref ht key missing)))
        (r6:hashtable-set!
         ht key
         (function
          (cond ((not (eq? v missing)) v)
                ((pair? maybe-thunk) ((car maybe-thunk)))
                (else (error "hash-table-update!: no value exists for key"
                             key)))))))

    (define (hash-table-update!/default table key function default)
      (hash-table-update! table key function (lambda () default)))

    (define (hash-table-size table)
      (check-table 'hash-table-size table)
      (r6:hashtable-size (hash-table-table table)))

    (define (hash-table-keys table)
      (check-table 'hash-table-keys table)
      (vector->list (r6:hashtable-keys (hash-table-table table))))

    (define (hash-table-values table)
      (check-table 'hash-table-values table)
      (call-with-values (lambda () (r6:hashtable-entries (hash-table-table table)))
        (lambda (keys vals) (vector->list vals))))

    (define (hash-table-walk table proc)
      (check-table 'hash-table-walk table)
      (call-with-values (lambda () (r6:hashtable-entries (hash-table-table table)))
        (lambda (keys vals)
          (let ((n (vector-length keys)))
            (do ((i 0 (+ i 1))) ((= i n))
              (proc (vector-ref keys i) (vector-ref vals i)))))))

    (define (hash-table-fold table f acc)
      (hash-table-walk table (lambda (key value) (set! acc (f key value acc))))
      acc)

    (define (hash-table->alist table)
      (hash-table-fold table (lambda (key val acc) (cons (cons key val) acc)) '()))

    ;; The first association for a key takes precedence.
    (define (alist->hash-table alist . args)
      (let* ((table (apply make-hash-table args))
             (ht (hash-table-table table)))
        (for-each (lambda (elem)
                    (if (not (r6:hashtable-contains? ht (car elem)))
                        (r6:hashtable-set! ht (car elem) (cdr elem))))
                  alist)
        table))

    (define (hash-table-copy table)
      (check-table 'hash-table-copy table)
      (%make-hash-table (r6:hashtable-copy (hash-table-table table) #t)
                        (hash-table-equivalence-function table)
                        (hash-table-hash-function table)))

    (define (hash-table-merge! table1 table2)
      (check-table 'hash-table-merge! table1)
      (hash-table-walk table2 (lambda (key value)
                                (hash-table-set! table1 key value)))
      table1)))
