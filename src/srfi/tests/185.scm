;;; SRFI 185 tests, after the SRFI document's specification and example
;;; (the SRFI has no test suite).
(import (scheme base) (scheme process-context) (srfi 64) (srfi 185))

(test-begin "srfi-185")

(test-equal "abc" (string-append-linear! (string-copy "a") "b" #\c))
(test-equal "a" (string-append-linear! (string-copy "a")))
(test-equal "xyz" (string-append-linear! (make-string 0) #\x "yz"))

(test-equal "aXYd" (string-replace-linear! (string-copy "abcd") 1 3 "XY"))
(test-equal "aYd" (string-replace-linear! (string-copy "abcd") 1 3 "XY" 1))
(test-equal "aXd" (string-replace-linear! (string-copy "abcd") 1 3 "XYZ" 0 1))
(test-equal "abXYZcd" (string-replace-linear! (string-copy "abcd") 2 2 "XYZ"))
(test-equal "ad" (string-replace-linear! (string-copy "abcd") 1 3 "XYZ" 1 1))
(test-equal "abcdef"
            (let ((s (string-copy "abc")))
              (string-replace-linear! s (string-length s) (string-length s) "def")))

(define (translate-space-to-newline str)
  (let ((result (make-string 0)))
    (string-for-each
     (lambda (ch)
       (cond ((char=? ch #\space)
              (string-append! result #\newline))
             ((char=? ch #\return)) ; Ignore
             (else
              (string-append! result ch))))
     str)
    result))

(test-equal "a\nb\nc" (translate-space-to-newline "a b c\r"))
(test-equal "a\nbc" (translate-space-to-newline "a b\rc"))
(test-equal "" (translate-space-to-newline ""))

(let ((s (string-copy "hello world")))
  (string-replace! s 0 5 "goodbye")
  (test-equal "goodbye world" s)
  (string-replace! s 7 13 "!" 0)
  (test-equal "goodbye!" s)
  (string-append! s " " "bye" #\.)
  (test-equal "goodbye! bye." s))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-185")
  (exit (if (zero? failures) 0 1)))
