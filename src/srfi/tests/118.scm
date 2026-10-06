;;; SRFI 118 tests, after the SRFI document's specification and example
;;; (the SRFI has no test suite).  Only uses that Pseudoscheme's
;;; approximation supports: the string being resized is in a variable.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 118))

(test-begin "srfi-118")

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

(let ((s (make-string 0)))
  (string-append! s "abc" #\d "" "ef")
  (test-equal "abcdef" s)
  (string-append! s)
  (test-equal "abcdef" s))

;; Insertion, deletion, growing and shrinking replacement.
(let ((s (string-copy "abcd")))
  (string-replace! s 2 2 "XYZ")
  (test-equal "abXYZcd" s)
  (string-replace! s 2 5 "")
  (test-equal "abcd" s)
  (string-replace! s 1 3 "123456" 1 4)
  (test-equal "a234d" s)
  (string-replace! s 1 4 "Q" 0)
  (test-equal "aQd" s)
  (string-replace! s 3 3 "end")
  (test-equal "aQdend" s))

;; (string-append! dst value) = (string-replace! dst len len value)
(let ((a (string-copy "abc")) (b (string-copy "abc")))
  (string-append! a "de")
  (string-replace! b (string-length b) (string-length b) "de")
  (test-equal a b))

;; A same-length replacement happens in place, so every reference sees it,
;; including when the string is not in a variable.
(let* ((s (string-copy "abcdef")) (alias s))
  (string-replace! s 1 3 "XY")
  (test-equal "aXYdef" alias)
  (test-assert (eq? s alias)))
(let ((v (vector (string-copy "abcdef"))))
  (string-replace! (vector-ref v 0) 0 2 "zz")
  (test-equal "zzcdef" (vector-ref v 0))
  (test-error (string-replace! (vector-ref v 0) 0 2 "longer")))

;; Overlapping source and destination.
(let ((s (string-copy "abcdef")))
  (string-replace! s 2 6 s 0 4)
  (test-equal "ababcd" s))
(let ((s (string-copy "abcdef")))
  (string-replace! s 0 4 s 2 6)
  (test-equal "cdefef" s))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-118")
  (exit (if (zero? failures) 0 1)))
