;;; Tests for SRFI 239: the sample implementation's test suite
;;; (reference/srfi-239/tests.sps), as SRFI 64 tests, plus a few more.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 239)
        (only (rnrs conditions) assertion-violation?)
        (prefix (srfi :239 list-case) r6:))

(define type-of
  (lambda (x)
    (list-case x
      [(_ . _) 'pair]
      [() 'null]
      [_ 'atom])))

(define fold
  (lambda (proc seed ls)
    (let f ([acc seed] [ls ls])
      (list-case ls
        [(h . t) (f (proc h acc) t)]
        [() acc]
        [_ (error "not a list" ls)]))))

(define (tests)
  (test-eq 'pair (type-of '(a . b)))
  (test-eq 'null (type-of '()))
  (test-eq 'atom (type-of 'x))
  (test-equal '(3 2 1 . 0) (fold cons 0 '(1 2 3)))
  (test-error (fold cons 0 '(1 2 . 3)))
  (test-assert (list-case '(1 . 2)
                 [_ #f]
                 [(_ . _) #t]))
  (test-assert "unmatched: an assertion violation"
    (guard (exc
            [(assertion-violation? exc) #t]
            [else #f])
      (list-case 0
        [(_ . _) #f]
        [() #f])))
  ;; Not in tests.sps.
  (test-equal "head only" 1 (list-case '(1 2) [(h . _) h] [_ #f]))
  (test-equal "tail only" '(2) (list-case '(1 2) [(_ . t) t] [_ #f]))
  (test-equal "dotted binds" 5 (list-case 5 [x x] [() #f]))
  (test-equal "expression evaluated once" 1
    (let ((n 0))
      (list-case (begin (set! n (+ n 1)) '(a))
        [(h . t) n]
        [() #f]
        [_ #f])))
  (test-equal "sublibrary" 'null (r6:list-case '() [() 'null] [_ 'other])))

(test-begin "srfi-239")
(tests)
(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-239")
  (exit (if (zero? failures) 0 1)))
