;;; Tests for SRFI 59, from the SRFI document's specification and its
;;; one example.
(import (scheme base) (scheme char) (scheme process-context) (srfi 64) (srfi 59))

(define (ends-with-slash? s)
  (and (> (string-length s) 0)
       (char=? #\/ (string-ref s (- (string-length s) 1)))))

(test-begin "srfi-59")
(test-equal "/usr/local/lib/scm/"
  (pathname->vicinity "/usr/local/lib/scm/Link.scm"))
(test-equal "" (pathname->vicinity "Link.scm"))
(test-equal "" (user-vicinity))
(test-assert (vicinity:suffix? #\/))
(test-assert (not (vicinity:suffix? #\a)))
(test-equal "/a/b/c.scm" (in-vicinity "/a/b/" "c.scm"))
(test-equal "/a/b/c/" (sub-vicinity "/a/b/" "c"))
(test-equal "/a/b/c/d.scm" (in-vicinity (sub-vicinity "/a/b/" "c") "d.scm"))
(test-equal "/x/y/" (make-vicinity "/x/y/"))
(test-equal "c.scm" (in-vicinity (user-vicinity) "c.scm"))
;; home-vicinity is $HOME with a trailing slash
(let ((home (get-environment-variable "HOME")))
  (if home
      (begin
        (test-assert (ends-with-slash? (home-vicinity)))
        (test-assert (string=? (home-vicinity)
                               (if (ends-with-slash? home) home (string-append home "/")))))
      (test-equal #f (home-vicinity))))
(test-assert (string? (library-vicinity)))
(test-assert (or (not (implementation-vicinity))
                 (ends-with-slash? (implementation-vicinity))))
;; program-vicinity is the directory of this program
(test-assert (string? (program-vicinity)))
(test-equal (program-vicinity)
  (pathname->vicinity (in-vicinity (program-vicinity) "59.scm")))
(let ((v (program-vicinity)))
  (test-assert (or (string=? v "")
                   (let ((n (string-length v)))
                     (and (>= n 6) (string=? "tests/" (substring v (- n 6) n)))))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-59")
  (exit (if (zero? failures) 0 1)))
