;;; Tests for SRFI 193, run as a program by bin/pseudoscheme: this file
;;; is then the script, and the command.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 193))
(test-begin "srfi-193")

(test-assert (pair? (command-line)))
(test-equal (cdr (command-line)) (command-args))
(test-equal "193" (command-name))
(test-assert (string? (script-file)))
(test-eqv #\/ (string-ref (script-file) 0))
(test-assert "script-file ends with the program's name"
  (let ((f (script-file)) (n (string-length "/193.scm")))
    (string=? "/193.scm" (substring f (- (string-length f) n) (string-length f)))))
(test-assert "script-directory ends with a slash"
  (let ((d (script-directory)))
    (char=? #\/ (string-ref d (- (string-length d) 1)))))
(test-equal (script-file) (string-append (script-directory) "193.scm"))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-193")
  (exit (if (zero? failures) 0 1)))
