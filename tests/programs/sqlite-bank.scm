;;; A bank in SQLite, from Scheme, through cl-sqlite (which binds the C
;;; library with CFFI).
;;;
;;;   bin/pseudoscheme --quicklisp tests/programs/sqlite-bank.scm
;;;
;;; Transfers run inside cl-sqlite's with-transaction, a Lisp macro whose
;;; body is Lisp code calling Scheme procedures.  A CHECK constraint
;;; failing in C becomes a Lisp error, then a Scheme condition, and the
;;; macro's UNWIND-PROTECT rolls the transaction back -- as it does when
;;; Scheme raises its own error, or escapes with a continuation.  Two
;;; hundred pseudo-random transfers are checked against a model of the
;;; bank kept in Scheme.

(import (scheme base) (scheme process-context)
        (srfi 1) (srfi 64)
        (pseudoscheme lisp)
        (prefix (cl common-lisp) cl:)
        (prefix (cl sqlite) sql:))

(test-begin "sqlite-bank")

(define db (sql:connect ":memory:"))

(sql:execute-non-query db "create table accounts (
  id integer primary key,
  owner text not null,
  balance integer not null check (balance >= 0),
  note text,
  photo blob)")

(define owners '("ana" "björn" "chen" "dmitri" "eve" "fatima"))

(for-each (lambda (owner balance)
            (sql:execute-non-query db "insert into accounts (owner, balance) values (?, ?)"
                                   owner balance))
          owners '(500 300 1000 0 250 750))

(define (balance id) (sql:execute-single db "select balance from accounts where id = ?" id))
(define (balances) (map car (sql:execute-to-list db "select balance from accounts order by id")))
(define (total) (sql:execute-single db "select sum(balance) from accounts"))

(test-equal "six accounts" 6 (sql:execute-single db "select count(*) from accounts"))
(test-equal "the owners, Unicode intact" owners
            (map car (sql:execute-to-list db "select owner from accounts order by id")))
(test-assert "TEXT comes back as Scheme strings"
             (every string? (map car (sql:execute-to-list db "select owner from accounts"))))
(test-equal "the money" 2800 (total))
(test-equal "last-insert-rowid" 6 (sql:last-insert-rowid db))

;;; NULL, #f and ()
(test-equal "NULL is NIL, which is ()" '() (sql:execute-single db "select note from accounts where id = 1"))
(sql:execute-non-query db "update accounts set note = ? where id = ?" #f 2)
(test-equal "#f is bound as NULL (it's NIL to Lisp)" 1
            (sql:execute-single db "select note is null from accounts where id = 2"))
(sql:execute-non-query db "update accounts set note = ? where id = ?" "VIP ✓" 3)
(test-equal "a note" "VIP ✓" (sql:execute-single db "select note from accounts where id = 3"))

;;; Blobs are bytevectors
(define photo (let ((bv (make-bytevector 256)))
                (do ((i 0 (+ i 1))) ((= i 256) bv) (bytevector-u8-set! bv i (- 255 i)))))
(sql:execute-non-query db "update accounts set photo = ? where id = ?" photo 4)
(test-equal "a blob, round trip" photo (sql:execute-single db "select photo from accounts where id = 4"))
(test-assert "read back as a bytevector"
             (bytevector? (sql:execute-single db "select photo from accounts where id = 4")))
(test-equal "length(blob) in SQL" 256 (sql:execute-single db "select length(photo) from accounts where id = 4"))

;;; ------------------------------------------------------------------
;;; Transfers

(define (adjust! id amount)
  (sql:execute-non-query db "update accounts set balance = balance + ? where id = ?" amount id))

;; Credit first, then debit, so a failed debit has something to undo.
;; with-transaction's body is Lisp code; adjust! is Scheme's.
(define (transfer! from to amount)
  (sql:with-transaction db
    (adjust! to amount)
    (adjust! from (- amount))))

(define (constraint-error? e) (cl:typep e sql:sqlite-constraint-error))

(transfer! 1 2 200)
(test-equal "a transfer" '(300 500) (list (balance 1) (balance 2)))

(test-assert "an overdraft violates the CHECK constraint, a Lisp error caught in Scheme"
             (guard (e ((constraint-error? e) #t))
               (transfer! 4 1 50)
               #f))
(test-equal "the handler's condition is cl-sqlite's again when passed to Lisp"
            '(#:constraint #t)
            (guard (e (#t (list (sql:sqlite-error-code e)
                                (error-object? e))))   ; and to Scheme, an error object
              (transfer! 4 1 50)))
(test-equal "and the credit before it was rolled back" '(300 0) (list (balance 1) (balance 4)))

(test-equal "a Scheme error inside the transaction rolls it back too" '(caught 300 500)
            (guard (e ((string? e) (list 'caught (balance 1) (balance 2))))
              (sql:with-transaction db
                (adjust! 2 1000)
                (raise "changed my mind"))))

(test-equal "so does escaping it with a continuation" '(escaped 300 500)
            (let ((result (call/cc (lambda (k)
                                     (sql:with-transaction db
                                       (adjust! 2 1000)
                                       (k 'escaped))))))
              (list result (balance 1) (balance 2))))

(test-equal "a committed transaction stays committed" 1300
            (begin (sql:with-transaction db (adjust! 3 300)) (balance 3)))
(adjust! 3 -300)

;;; ------------------------------------------------------------------
;;; Two hundred transfers, against a model in Scheme

;; A linear congruential generator, so the run is the same everywhere
(define seed 20261006)
(define (next-random! n)
  (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
  (modulo (quotient seed 65536) n))

(define model (list->vector (cons #f (balances))))   ; 1-based, like the ids

(define (model-transfer! from to amount)
  (if (< (vector-ref model from) amount)
      #f
      (begin (vector-set! model from (- (vector-ref model from) amount))
             (vector-set! model to (+ (vector-ref model to) amount))
             #t)))

(define outcomes
  (let loop ((i 0) (ok 0) (refused 0))
    (if (= i 200)
        (list ok refused)
        (let* ((from (+ 1 (next-random! 6)))
               (to (+ 1 (modulo (+ from (next-random! 5)) 6)))   ; another account
               (amount (+ 1 (next-random! 400)))
               (expected (model-transfer! from to amount))
               (actual (guard (e ((constraint-error? e) #f))
                         (transfer! from to amount)
                         #t)))
          (unless (eq? expected actual)
            (error "the bank and the model disagree" i from to amount))
          (if actual (loop (+ i 1) (+ ok 1) refused) (loop (+ i 1) ok (+ refused 1)))))))

(test-equal "two hundred transfers" 200 (apply + outcomes))
(test-assert "some refused, most not" (< 0 (cadr outcomes) (car outcomes)))
(test-equal "the bank's balances are the model's" (cdr (vector->list model)) (balances))
(test-equal "no money made or lost" 2800 (total))
(test-assert "no balance below zero" (every (lambda (b) (>= b 0)) (balances)))

;;; ------------------------------------------------------------------
;;; A prepared statement, stepped by hand

;; step-statement is a predicate whose name doesn't say so: its NIL,
;; "no more rows", is () in Scheme, so ask lisp-true?.
(define (rich-owners minimum)
  (let ((statement (sql:prepare-statement
                    db "select owner, balance from accounts where balance >= ? order by balance desc, id")))
    (sql:bind-parameter statement 1 minimum)
    (let loop ((rows '()))
      (if (lisp-true? (sql:step-statement statement))
          (loop (cons (cons (sql:statement-column-value statement 0)
                            (sql:statement-column-value statement 1))
                      rows))
          (begin (sql:finalize-statement statement)
                 (reverse rows))))))

(define (sort-descending numbers) (cl:sort (list-copy numbers) cl:>))

(test-equal "a prepared statement agrees with execute-to-list"
            (map (lambda (row) (cons (car row) (cadr row)))
                 (sql:execute-to-list
                  db "select owner, balance from accounts where balance >= 400 order by balance desc, id"))
            (rich-owners 400))
(test-assert "sorted by balance"
             (let ((amounts (map cdr (rich-owners 0))))
               (equal? amounts (sort-descending amounts))))

(sql:disconnect db)

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "sqlite-bank")
  (exit (if (zero? failures) 0 1)))
