;;; SRFI 90: extensible hash table constructor.  Written for
;;; Pseudoscheme, after the implementation in the SRFI document (Marc
;;; Feeley, 2006; MIT licence), which passes the parameters on to SRFI 69's
;;; make-hash-table; the tables are SRFI 69 tables.
;;;
;;; The SRFI's named parameters are SRFI 88 keywords (test: eq?), which
;;; Pseudoscheme's reader does not have: there test: is an ordinary
;;; identifier.  So this library exports test:, hash:, size:, min-load:,
;;; max-load:, weak-keys: and weak-values: as variables whose values are
;;; the symbols of the same names, and (make-table test: eq? size: 10)
;;; works as written.  A hash function given to make-table takes one
;;; argument, as SRFI 90 says; it is wrapped to take SRFI 69's optional
;;; bound too, so hash-table-hash-function returns the wrapper.  make-table also accepts Pseudoscheme's own
;;; keywords (#:test eq?) and the quoted symbols ('test: eq?).
;;;
;;; size, min-load, max-load, weak-keys and weak-values are checked but,
;;; as the SRFI allows, only advisory: they are ignored.  In particular
;;; the tables are never weak.
(define-library (srfi 90)
  (export make-table
          test: hash: size: min-load: max-load: weak-keys: weak-values:)
  (import (scheme base) (srfi 69))
  (begin
    (define test: 'test:)
    (define hash: 'hash:)
    (define size: 'size:)
    (define min-load: 'min-load:)
    (define max-load: 'max-load:)
    (define weak-keys: 'weak-keys:)
    (define weak-values: 'weak-values:)

    ;; test: or #:test -> the symbol test
    (define parameter-names
      '((test: . test) (hash: . hash) (size: . size)
        (min-load: . min-load) (max-load: . max-load)
        (weak-keys: . weak-keys) (weak-values: . weak-values)
        (#:test . test) (#:hash . hash) (#:size . size)
        (#:min-load . min-load) (#:max-load . max-load)
        (#:weak-keys . weak-keys) (#:weak-values . weak-values)))

    (define (parameter-name key)
      (let ((entry (assq key parameter-names)))
        (and entry (cdr entry))))

    ;; SRFI 90's hash takes one argument; SRFI 69 calls a table's hash
    ;; function with a bound as well.
    (define (one-argument-hash hash)
      (lambda (key . bound)
        (let ((h (hash key)))
          (if (pair? bound) (modulo h (car bound)) h))))

    (define (load-factor? x) (and (real? x) (<= 0 x 1)))

    (define (make-table . args)
      (let loop ((args args) (test #f) (hash #f) (min-load 0) (max-load 1))
        (cond
         ((null? args)
          (unless (< min-load max-load)
            (error "make-table: min-load must be less than max-load"
                   min-load max-load))
          (cond ((not test)
                 (if hash
                     (make-hash-table equal? (one-argument-hash hash))
                     (make-hash-table)))
                ((not hash) (make-hash-table test))
                (else (make-hash-table test (one-argument-hash hash)))))
         ((null? (cdr args))
          (error "make-table: named parameter without a value" (car args)))
         (else
          (let ((name (parameter-name (car args)))
                (value (cadr args))
                (rest (cddr args)))
            (case name
              ((test)
               (unless (procedure? value) (error "make-table: bad test" value))
               (loop rest value hash min-load max-load))
              ((hash)
               (unless (procedure? value) (error "make-table: bad hash" value))
               (loop rest test value min-load max-load))
              ((size)
               (unless (and (exact-integer? value) (>= value 0))
                 (error "make-table: bad size" value))
               (loop rest test hash min-load max-load))
              ((min-load)
               (unless (load-factor? value) (error "make-table: bad min-load" value))
               (loop rest test hash value max-load))
              ((max-load)
               (unless (load-factor? value) (error "make-table: bad max-load" value))
               (loop rest test hash min-load value))
              ((weak-keys weak-values)
               (loop rest test hash min-load max-load))
              (else
               (error "make-table: unknown named parameter" (car args)))))))))))
