;;; Tests for SRFI 139, from the SRFI's examples.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 139))
(test-begin "srfi-139")

;; anaphoric if
(define-syntax-parameter it
  (syntax-rules () ((_ . _) (syntax-error "it used outside of aif"))))
(define-syntax aif
  (syntax-rules ()
    ((_ test then else)
     (let ((t test))
       (syntax-parameterize ((it (syntax-rules () ((_) t))))
         (if t then else))))))
(test-equal 3 (aif (assq 'b '((a . 1) (b . 3))) (cdr (it)) #f))
(test-equal 'none (aif (assq 'c '((a . 1))) (cdr (it)) 'none))

;; return from a procedure
(define-syntax-parameter return
  (syntax-rules () ((_ . _) (syntax-error "return used outside of lambda*"))))
(define-syntax lambda*
  (syntax-rules ()
    ((_ formals body ...)
     (lambda formals
       (call-with-current-continuation
        (lambda (k)
          (syntax-parameterize ((return (syntax-rules () ((_ v) (k v)))))
            body ...)))))))
(define find-even
  (lambda* (ls)
    (for-each (lambda (x) (if (even? x) (return x))) ls)
    #f))
(test-equal 4 (find-even '(1 3 4 5 6)))
(test-equal #f (find-even '(1 3 5)))

;; a macro defined outside, used inside, sees the new binding
(define-syntax-parameter colour (syntax-rules () ((_) 'red)))
(define-syntax get-colour (syntax-rules () ((_) (colour))))
(test-equal 'red (get-colour))
(test-equal 'blue (syntax-parameterize ((colour (syntax-rules () ((_) 'blue)))) (get-colour)))
(test-equal '(green red)
  (list (syntax-parameterize ((colour (syntax-rules () ((_) 'green)))) (get-colour))
        (get-colour)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-139")
  (exit (if (zero? failures) 0 1)))
