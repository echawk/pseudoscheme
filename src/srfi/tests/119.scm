;;; Tests for SRFI 119: wisp's rules, as the SRFI describes them, read
;;; with wisp-scheme-read-string and wisp-read-form.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 119))
(define (wisp s) (wisp-scheme-read-string s))
(test-begin "srfi-119")

;; indentation makes lists; a colon opens one that closes at line end
(test-equal '((define (fac x) (if (<= x 1) 1 (* x (fac (- x 1))))))
  (wisp "define : fac x\n  if : <= x 1\n    . 1\n    * x : fac : - x 1\n"))
;; a line starting with . continues its parent: its items aren't a list
(test-equal '((list 1 2 3)) (wisp "list 1\n  . 2 3\n"))
;; parenthesized code is read as usual
(test-equal '((display (+ 1 2))) (wisp "display (+ 1 2)\n"))
;; a lone colon makes a list of the lines under it
(test-equal '((let ((a 1) (b 2)) (+ a b)))
  (wisp "let\n  :\n    a 1\n    b 2\n  + a b\n"))
;; underscores hold indentation
(test-equal '((a (b (c)))) (wisp "a\n__ b\n____ c\n"))
;; quote before a list
(test-equal '((list '(1 2))) (wisp "list\n  ' 1 2\n"))
;; two forms
(test-equal '((define x 1) (display x)) (wisp "define x 1\n\ndisplay x\n"))
;; one form at a time
(let ((p (open-input-string "define x 1\n\ndisplay x\n")))
  (test-equal '(define x 1) (wisp-read-form p))
  (test-equal '(display x) (wisp-read-form p))
  (test-assert (eof-object? (wisp-read-form p))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-119")
  (exit (if (zero? failures) 0 1)))
