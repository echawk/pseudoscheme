;;; Tests for SRFI 215: the sample implementation's srfi-215-tests.scm,
;;; with its checks made SRFI 64 tests, and more after the SRFI document.
(import (scheme base) (scheme process-context) (scheme write)
        (srfi 64) (srfi 215))

(test-begin "srfi-215")

(current-log-fields '(FACILITY 1))

(define (find proc list)
  (if (null? list)
      #f
      (if (proc (car list))
          #t
          (find proc (cdr list)))))

;; This log message should be buffered until the callback is set
(send-log DEBUG "first message")
(let ((msgs '()))
  (parameterize ((current-log-callback
                  (lambda (msg)
                    (set! msgs (cons msg msgs)))))
    ;; This should should also be handled by the callback
    (send-log INFO "second message" 'FOO 'bar)
    (test-assert "the first message"
      (find (lambda (msg)
              (and (eqv? 7 (cdr (assq 'SEVERITY msg)))
                   (string=? "first message"
                             (cdr (assq 'MESSAGE msg)))))
            msgs))
    (test-assert "the second message"
      (find (lambda (msg)
              (and (eqv? 6 (cdr (assq 'SEVERITY msg)))
                   (string=? "second message"
                             (cdr (assq 'MESSAGE msg)))
                   (equal? "bar"
                           (cond ((assq 'FOO msg) => cdr)
                                 (else #f)))))
            msgs))))

;; The example from the document, to a string port.
(define out (open-output-string))
(current-log-callback
 (lambda (msg)
   (let ((p out))
     (display "<" p)
     (display (cdr (assq 'SEVERITY msg)) p)
     (display ">" p)
     (display (cdr (assq 'MESSAGE msg)) p)
     (newline p))))
(send-log DEBUG "Log callback configured")
(test-equal "<7>Log callback configured\n" (get-output-string out))

;; Fields: values are converted, #f values dropped, current-log-fields
;; appended.
(define last #f)
(current-log-callback (lambda (msg) (set! last msg)))
(current-log-fields '())
(send-log ERROR "m" 'A "s" 'B 42 'C '(x y) 'D #f 'E (bytevector 1))
(test-equal '((SEVERITY . 3) (MESSAGE . "m") (A . "s") (B . 42)
              (C . "(x y)") (E . #u8(1)))
  last)
(parameterize ((current-log-fields '(SUBSYSTEM "net")))
  (send-log WARNING "w" 'K 1)
  (test-equal '((SEVERITY . 4) (MESSAGE . "w") (K . 1) (SUBSYSTEM . "net"))
    last))
(send-log NOTICE "n")
(test-equal '((SEVERITY . 5) (MESSAGE . "n")) last)

(test-equal '(0 1 2 3 4 5 6 7)
  (list EMERGENCY ALERT CRITICAL ERROR WARNING NOTICE INFO DEBUG))

(test-error (send-log 8 "bad severity"))
(test-error (send-log INFO 'not-a-string))
(test-error (send-log INFO "odd fields" 'A))
(test-error (send-log INFO "bad key" "A" 1))
(test-error (current-log-fields '(A)))
(test-error (current-log-callback 'not-a-procedure))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-215")
  (exit (if (zero? failures) 0 1)))
