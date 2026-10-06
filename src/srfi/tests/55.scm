;;; Tests for SRFI 55.  require-extension works at the REPL's top level,
;;; so these tests evaluate forms there, through the R7RS front end's
;;; REPL entry point (Lisp's pseudoscheme-r7rs::eval-at-repl).  The
;;; examples are the SRFI document's.
(import (scheme base) (scheme process-context) (srfi 64)
        (pseudoscheme lisp))

(define repl (lisp-function "eval-at-repl" "pseudoscheme-r7rs"))

(test-begin "srfi-55")

(repl '(import (scheme base) (srfi 55)))

;; (require-extension (srfi 1)): make the SRFI 1 List Library available
(repl '(require-extension (srfi 1)))
(test-equal '(0 1 2) (repl '(iota 3)))
(test-equal 3 (repl '(fold + 0 '(1 1 1))))

;; (require-extension (srfi 1 13 14)): several at once
(repl '(require-extension (srfi 8 26 2)))
(test-equal '(2 3) (repl '(receive (a . b) (values 1 2 3) b)))
(test-equal 11 (repl '((cut + 1 <>) 10)))
(test-equal 4 (repl '(and-let* ((x 2)) (* x x))))

;; several clauses, and a library name as a clause
(repl '(require-extension (srfi 151) (scheme char)))
(test-equal 1 (repl '(bitwise-and 3 5)))
(test-equal #\A (repl '(char-upcase #\a)))

;; a missing extension is an error
(test-error (repl '(require-extension (srfi 99999))))
(test-error (repl '(require-extension (srfi foo))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-55")
  (exit (if (zero? failures) 0 1)))
