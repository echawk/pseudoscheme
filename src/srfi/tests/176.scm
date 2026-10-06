;;; Tests for SRFI 176.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 176))
(test-begin "srfi-176")

(define alist (version-alist))
(test-assert (list? alist))
(test-assert "every property is a list headed by a symbol"
  (let loop ((ps alist))
    (or (null? ps) (and (pair? (car ps)) (symbol? (caar ps)) (list? (car ps)) (loop (cdr ps))))))
(test-equal '(command "pseudoscheme") (assq 'command alist))
(test-equal '(scheme.id pseudoscheme) (assq 'scheme.id alist))
(test-assert (memq 'r7rs (cdr (assq 'languages alist))))
(test-assert (string? (cadr (assq 'version alist))))
(test-assert "scheme.srfi lists this SRFI" (memv 176 (cdr (assq 'scheme.srfi alist))))
(test-assert (memq 'r7rs (cdr (assq 'scheme.features alist))))
(test-assert (not (memq 'srfi-1 (cdr (assq 'scheme.features alist)))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-176")
  (exit (if (zero? failures) 0 1)))
