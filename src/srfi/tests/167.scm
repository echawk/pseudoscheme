;;; Tests for SRFI 167: the sample implementation's tests,
;;; srfi/memory/test.sld and srfi/pack/test.sld (in reference/srfi-167/),
;;; with chibi's (test [name] expected expr) defined as test-equal.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 158)
        (srfi 167) (srfi 167 pack))

(define-syntax test
  (syntax-rules ()
    ((_ expected expr) (test-equal expected expr))
    ((_ name expected expr) (test-equal name expected expr))))


    (define (run-memory-tests)

      (define engine (make-default-engine))

      (test
       #t
       (let ((okvs (engine-open engine #f)))
         (engine-close engine okvs)
         #t))

      (test
       #u8(1 2 3 42)
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(13 37) #u8(1 2 3 42))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (engine-ref engine transaction #u8(13 37))))))
           (engine-close engine okvs)
           out)))

      (test
       #u8(42)
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(13 37) #u8(1 2 3 42))))
         ;; overwrite
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(13 37) #u8(42))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (engine-ref engine transaction #u8(13 37))))))
           (engine-close engine okvs)
           out)))

      (test
       (list (cons #u8(20 16) #u8(2)) (cons #u8(20 17) #u8(3)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 18) #u8(4))
                                  (engine-set! engine transaction #u8(20 16) #u8(2))
                                  (engine-set! engine transaction #u8(20 15) #u8(1))
                                  (engine-set! engine transaction #u8(20 19) #u8(5))
                                  (engine-set! engine transaction #u8(20 17) #u8(3))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list
                                              (engine-range engine transaction #u8(20 16) #t #u8(20 18) #f))))))
           (engine-close engine okvs)
           out)))

      (test
       (list (cons #u8(20 16) #u8(2)) (cons #u8(20 17 01) #u8(3)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 18) #u8(4))
                                  (engine-set! engine transaction #u8(20 16) #u8(2))
                                  (engine-set! engine transaction #u8(20 15) #u8(1))
                                  (engine-set! engine transaction #u8(20 19) #u8(5))
                                  ;; #u8(20 17 01) lexicographically less than #u8(20 18)
                                  (engine-set! engine transaction #u8(20 17 01) #u8(3))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list
                                              (engine-range engine transaction #u8(20 16) #t #u8(20 18) #f))))))
           (engine-close engine okvs)
           out)))

      (test
       '((#u8(01 02) . #u8(1))
         (#u8(20 16) . #u8(2))
         (#u8(20 16 1) . #u8(2))
         (#u8(20 17) . #u8(3))
         (#u8(20 17 1) . #u8(2))
         (#u8(42 42) . #u8(5)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 17 01) #u8(2))
                                  (engine-set! engine transaction #u8(20 17) #u8(3))
                                  (engine-set! engine transaction #u8(42 42) #u8(5))
                                  (engine-set! engine transaction #u8(01 02) #u8(1))
                                  (engine-set! engine transaction #u8(20 16) #u8(2))
                                  (engine-set! engine transaction #u8(20 16 01) #u8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction #u8()))))))
           (engine-close engine okvs)
           out)))

      (test
       '((#u8(20 16) . #u8(2))
         (#u8(20 16 1) . #u8(2))
         (#u8(20 17) . #u8(3))
         (#u8(20 17 1) . #u8(2)))
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 17 01) #u8(2))
                                  (engine-set! engine transaction #u8(20 17) #u8(3))
                                  (engine-set! engine transaction #u8(42 42) #u8(5))
                                  (engine-set! engine transaction #u8(01 02) #u8(1))
                                  (engine-set! engine transaction #u8(20 16) #u8(2))
                                  (engine-set! engine transaction #u8(20 16 01) #u8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction #u8(20)))))))
           (engine-close engine okvs)
           out)))

      (test
       '(
         (#u8(20 17) . #u8(3))
         (#u8(20 16 1) . #u8(2))
         )
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 17 01) #u8(2))
                                  (engine-set! engine transaction #u8(20 17) #u8(3))
                                  (engine-set! engine transaction #u8(42 42) #u8(5))
                                  (engine-set! engine transaction #u8(01 02) #u8(1))
                                  (engine-set! engine transaction #u8(20 16) #u8(2))
                                  (engine-set! engine transaction #u8(20 16 01) #u8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction
                                                                                   #u8(20)
                                                                                   '((offset . 1)
                                                                                     (limit . 2)
                                                                                     (reverse? #t))))))))
           (engine-close engine okvs)
           out)))

      (test
       '(
         (#u8(20 16 01) . #u8(2))
         (#u8(20 17 01) . #u8(2))
         )
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 17 01) #u8(2))
                                  (engine-set! engine transaction #u8(20 16 01) #u8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction
                                                                                   #u8(20)
                                                                                   '((limit . 3))))))))
           (engine-close engine okvs)
           out)))

      (test
       '()
       (let ((okvs (engine-open engine #f)))
         ;; set
         (engine-in-transaction engine okvs
                                (lambda (transaction)
                                  (engine-set! engine transaction #u8(20 17 01) #u8(2))
                                  (engine-set! engine transaction #u8(20 16 01) #u8(2))))
         ;; get
         (let ((out (engine-in-transaction engine okvs
                                           (lambda (transaction)
                                             (generator->list (engine-prefix-range engine transaction
                                                                                   #u8(20)
                                                                                   '((offset . 3))))))))
           (engine-close engine okvs)
           out)))

      (test
       '()
       (let ((keys '(#u8(1 42 0 20 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102 21 103)
                         #u8(1 42 0 21 1 21 102 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0)
                         #u8(1 42 0 21 2 21 103 2 55 97 98 53 118 54 110 103 113 119 49 117 53 121 111 57 50 104 110 107 105 109 112 105 104 0 21 102))))
         (let ((okvs (engine-open engine #f)))
           ;; set
           (engine-in-transaction engine okvs
                                  (lambda (transaction)
                                    (let loop ((keys keys))
                                      (unless (null? keys)
                                        (engine-set! engine transaction (car keys) #u8(2))
                                        (loop (cdr keys))))))
           ;; get
           (let* ((prefix #u8(1 42 0 20 2 57 98 57 55 54 97 104 97 104 50 51 113 110 52 102 121 97 99 49 53 120 99 118 48 100 0))
                  (out (engine-in-transaction engine okvs
                                              (lambda (transaction)
                                                (generator->list (engine-prefix-range engine transaction prefix))))))
             (engine-close engine okvs)
             out)))))


    (define expected
      (list *null*
            #t
            #f
            0
            #u8(42 101 255)
            "hello world"
            'symbol
            42
            (expt 2 64)
            -42
            (- (expt 2 64))))

    (define (run-pack-tests)
(run-okvs-tests)
      (test expected (unpack (apply pack expected))))

;; More tests, of the okvs procedures directly, after the SRFI document.
(define (run-okvs-tests)
  (define db (okvs-open #f))
  (define (bv . xs) (apply bytevector xs))
  (test-assert (okvs? db))
  (okvs-in-transaction db
    (lambda (tx)
      (test-assert (okvs-transaction? tx))
      (okvs-set! tx (bv 1) (bv 10))
      (okvs-set! tx (bv 2) (bv 20))
      (okvs-set! tx (bv 2 5) (bv 25))
      (okvs-set! tx (bv 3) (bv 30))))
  (test-equal (bv 20) (okvs-in-transaction db (lambda (tx) (okvs-ref tx (bv 2)))))
  (test-equal #f (okvs-in-transaction db (lambda (tx) (okvs-ref tx (bv 9)))))
  (test-equal (list (cons (bv 1) (bv 10)) (cons (bv 2) (bv 20)) (cons (bv 2 5) (bv 25)))
    (okvs-in-transaction db
      (lambda (tx) (generator->list (okvs-range tx (bv 1) #t (bv 3) #f)))))
  (test-equal (list (cons (bv 2) (bv 20)) (cons (bv 2 5) (bv 25)))
    (okvs-in-transaction db
      (lambda (tx) (generator->list (okvs-prefix-range tx (bv 2))))))
  (test-equal (list (cons (bv 3) (bv 30)) (cons (bv 2 5) (bv 25)))
    (okvs-in-transaction db
      (lambda (tx) (generator->list
                    (okvs-range tx (bv 2) #f (bv 3) #t '((reverse? . #t)))))))
  ;; a transaction that fails changes nothing
  (test-equal 'failed
    (okvs-in-transaction db
      (lambda (tx) (okvs-set! tx (bv 1) (bv 99)) (raise 'oops))
      (lambda (e) 'failed)))
  (test-equal (bv 10) (okvs-in-transaction db (lambda (tx) (okvs-ref tx (bv 1)))))
  (okvs-in-transaction db (lambda (tx) (okvs-delete! tx (bv 1))))
  (test-equal #f (okvs-in-transaction db (lambda (tx) (okvs-ref tx (bv 1)))))
  (okvs-in-transaction db (lambda (tx) (okvs-range-remove! tx (bv 2) #t (bv 3) #f)))
  (test-equal (list (cons (bv 3) (bv 30)))
    (okvs-in-transaction db
      (lambda (tx) (generator->list (okvs-range tx (bv 0) #t (bv 9) #t)))))
  (okvs-close db)
  ;; packed keys sort as their values
  (test-assert (equal? '(1 "a" b) (unpack (pack 1 "a" 'b)))))

(test-begin "srfi-167")
(run-memory-tests)
(run-pack-tests)
(run-okvs-tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-167")
  (exit (if (zero? failures) 0 1)))
