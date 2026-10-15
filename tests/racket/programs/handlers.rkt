#lang racket/base
;; Error raisers and their messages, error parameters, print parameters,
;; loggers, equal-always?, composable continuations and the like
;; (src/racket/native/handlers.lisp, exn.lisp, printer.lisp).  The
;; .expected file is what Racket 9.3 prints.
(require (only-in '#%kernel true-object?))

(define-syntax-rule (show e)
  (with-handlers ([(lambda (x) #t)
                   (lambda (x)
                     (printf "~s: ~a: ~a\n" 'e
                             (cond [(exn:fail:contract:arity? x) "arity"]
                                   [(exn:fail:contract? x) "contract"]
                                   [(exn:fail:user? x) "user"]
                                   [(exn:fail? x) "fail"]
                                   [else "raise"])
                             (if (exn? x) (exn-message x) x)))])
    (call-with-values (lambda () e)
                      (lambda vs (printf "~s => ~s\n" 'e vs)))))

(define (printed thunk)
  (let ([o (open-output-string)])
    (parameterize ([current-output-port o]) (thunk))
    (get-output-string o)))

;; raisers
(show (raise-argument-error 'f "string?" 5))
(show (raise-argument-error 'f "string?" 1 "a" 5 6))
(show (raise-argument-error 'f 'x 'x))
(show (raise-arguments-error 'f "bad thing" "a" 1 "b" '(1 2)))
(show (raise-arguments-error 'f "bad thing" "a" 1 "b" (unquoted-printing-string "xx\nyy")))
(show (raise-result-error 'f "string?" 5))
(show (raise-range-error 'f "vector" "" 5 (vector 1 2) 0 1))
(show (raise-range-error 'f "vector" "" 5 (vector) 0 -1))
(show (raise-range-error 'f "vector" "starting " 5 (vector 1 2 3) 2 3 1))
(show (raise-arity-error 'f 2 1 2 3))
(show (raise-arity-error 'f (arity-at-least 1)))
(show (raise-arity-mask-error 'f 5 1 2))
(show (raise-result-arity-error 'f 2 "\n  in: here" 1 2 3))
(show (raise-mismatch-error 'f "bad: " 5 " and " 6))
(show (raise-type-error 'f "string" 1 "a" 5))
(show (raise-user-error 'f "hi ~a" 5))
(show (raise-user-error 'f))
(show (error "x" 1 "s" 'a))
(show (error 'f "a~a" 1))
(show (string-length (exn-message (with-handlers ([values values])
                                    (raise-argument-error 'f "string?" (make-string 300 #\a))))))
(show (parameterize ([error-print-width 10]) (raise-argument-error 'f "string?" "abcdefghijklmnop")))
(show ((error-value->string-handler) "abcdef" 5))
(show ((error-value->string-handler) '(1 "a" b) 100))
(show (error-print-width 2))

;; adjusters
(show (parameterize ([current-error-message-adjuster
                      (lambda (m) (and (eq? m 'name) (lambda (n r) (values 'zzz r))))])
        (raise-argument-error 'f "string?" 5)))
(show (error-message->adjusted-string 'f 'racket "msg" 'racket))
(show (error-message->adjusted-string #f 'racket "msg" 'racket))

;; srcloc
(show (srcloc->string (srcloc "f.rkt" 1 2 3 4)))
(show (srcloc->string (srcloc "f.rkt" 1 #f 3 4)))
(show (srcloc->string (srcloc #f 1 2 3 4)))
(show (srcloc->string 5))

;; parameters
(show (list (read-accept-bar-quote) (read-case-sensitive) (print-graph) (print-struct)
            (print-box) (print-vector-length) (print-hash-table) (print-unreadable)
            (print-pair-curly-braces) (print-mpair-curly-braces) (print-reader-abbreviations)
            (print-boolean-long-form) (print-syntax-width) (print-as-expression)
            (compile-enforce-module-constants) (error-print-context-length)
            (current-thread-initial-stack-size)))
(show (current-locale 5))
(show (compile-target-machine? 'x))
(show (list (true-object? #t) (true-object? 1)))

;; properties and procedures
(show (custom-write? 5))
(show (custom-write-accessor 5))
(show (procedure-struct-type? struct:arity-at-least))
(show (let-values ([(p ? a) (make-struct-type-property 'x)])
        (list (struct-type-property-accessor-procedure? a)
              (struct-type-property-predicate-procedure? ?)
              (struct-type-property-accessor-procedure? ?))))
(show (procedure-rename car "foo"))
(show ((procedure-rename (lambda (x) x) 'foo) 1 2))
(show (object-name (procedure-rename car 'foo)))
(show (procedure-arity (procedure-rename (lambda (x) x) 'foo)))

;; continuations
(show (call-with-continuation-prompt
       (lambda () (+ 1 (call-with-composable-continuation (lambda (k) (k 10)))))))
(show (call-with-continuation-prompt
       (lambda () (call-with-composable-continuation (lambda (k) (list (continuation? k) (procedure? k)))))))
(show (call-with-composable-continuation (lambda (k) 5) (make-continuation-prompt-tag 'p)))
(show (let ([k (call/cc values)]) (if (procedure? k) (call-in-continuation k (lambda () 7)) k)))
(show (with-continuation-mark 'a 1
        (with-continuation-mark 'b 2
          (let-values ([(v next) ((continuation-mark-set->iterator (current-continuation-marks) (list 'a 'b) 'none))])
            v))))

;; loggers
(show (let* ([l (make-logger 'foo)] [r (make-log-receiver l 'info)])
        (log-message l 'info 'foo "hi" 5)
        (log-message l 'debug "dbg" 6)
        (log-message l 'warning #f "w" 7 #f)
        (list (sync/timeout 0 r) (sync/timeout 0 r) (sync/timeout 0 r))))
(show (let ([l (make-logger 'foo)]) (list (log-level? l 'info) (log-max-level l))))
(show (let* ([l (make-logger 'foo)] [r (make-log-receiver l 'info)])
        (list (log-level? l 'info) (log-level? l 'debug) (log-max-level l))))
(show (let* ([l (make-logger 'foo)] [c (make-logger 'child l)] [r (make-log-receiver l 'warning)])
        (log-message c 'error 'x "yy" 1)
        (list (sync/timeout 0 r) (log-max-level c) (logger-name c))))
(show (make-log-receiver (current-logger) 'foo))
(show (parameterize ([exit-handler (lambda (c) (list 'exit c))]) (exit 5)))

;; equality
(show (list (equal-always? (vector 1) (vector 1)) (equal-always? (box 1) (box 1))
            (equal-always? (string #\a) (string #\a))
            (equal-always? (string->immutable-string "a") (string->immutable-string "a"))
            (equal-always? (vector->immutable-vector (vector 1)) (vector->immutable-vector (vector 1)))
            (equal-always? "a" "a")
            (equal? (vector 1) (vector->immutable-vector (vector 1)))))
(show (= (equal-always-hash-code '(1 2)) (equal-always-hash-code (list 1 2))))
(show (equal?/recur '(1 2) '(1 3) (lambda (a b) #t)))

;; print parameters
(show (printed (lambda () (parameterize ([print-boolean-long-form #t]) (print (list #t #f)) (write #t) (display #f)))))
(show (printed (lambda () (parameterize ([print-vector-length #t]) (print (vector 1 2)) (write (vector)) (write (vector 1 1 1)) (write (vector 1 2 2 2))))))
(show (printed (lambda () (parameterize ([print-box #f]) (print (box 1)) (write (box 1))))))
(show (printed (lambda () (parameterize ([print-pair-curly-braces #t]) (print (list 1 2)) (write '(1 . 2))))))
(show (printed (lambda () (print (mcons 1 2)) (write (mcons 1 (mcons 2 '()))))))
(show (printed (lambda () (parameterize ([print-as-expression #f]) (print '(a "b" #\c)) (print 'a)))))
(show (printed (lambda () (parameterize ([print-reader-abbreviations #t]) (write ''a) (write '`(a ,b ,@c)) (print ''a)))))
(show (printed (lambda () (write ''a) (print ''a))))
(show (printed (lambda () (parameterize ([print-unreadable #f]) (display car) (write car)))))
(show (printed (lambda () (print (list (unquoted-printing-string "a b"))))))
(show (printed (lambda () (write "a\r\n\t\v\a\b\e\f\u0000x") (write #"a\r\n\0001\1a\377"))))
