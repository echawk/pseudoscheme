;;; Tests for SRFI 233: the SRFI's test suite, srfi-233-test.scm.
(import (scheme base) (scheme process-context) (srfi 64) (srfi 233))

(test-begin "srfi-233")

(test-group
 "make-ini-file-accumulator"

 (define result
   (let* ((port (open-output-string))
          (acc (make-ini-file-accumulator port)))

     ;; write leading section-less data
     (acc '(|| key1 "value1"))

     ;; write comment
     (acc "test comment")

     ;; write new section
     (acc '(section key2 "value2"))

     (get-output-string port)))

 (test-equal
     "key1=value1\n; test comment\n[section]\nkey2=value2\n"
   result))

(test-group
 "make-ini-file-generator"

 (define (read-to-list generator)
   (let loop ((lst '()))
     (define entry-lst (generator))
     (cond
      ((eof-object? entry-lst)
       (reverse lst))
      (else (loop (cons entry-lst lst))))))

 (define source "key1 = value1\n
; comment\n
\n
[section]\n
 key2 = value2\n
[section2]\n
key3\n
[key]4\n
\n
\n")

 (define result (read-to-list (make-ini-file-generator (open-input-string source))))

 (test-equal
     '((|| key1 "value1")
       (section key2 "value2")
       (section2 #f "key3")
       (section2 #f "[key]4"))
   result))


(test-group
 "make-ini-file-accumulator custom delimeters"

 (define result
   (let* ((port (open-output-string))
          (acc (make-ini-file-accumulator port #\- #\#)))

     ;; write leading section-less data
     (acc '(|| key1 "value1"))

     ;; write comment
     (acc "test comment")

     ;; write new section
     (acc '(section key2 "value2"))

     (get-output-string port)))

 (test-equal
     "key1-value1\n# test comment\n[section]\nkey2-value2\n"
   result))

(test-group
 "make-ini-file-generator custom delimeters"

 (define (read-to-list generator)
   (let loop ((lst '()))
     (define entry-lst (generator))
     (cond
      ((eof-object? entry-lst)
       (reverse lst))
      (else (loop (cons entry-lst lst))))))

 (define source "key1 - value1\n
# comment\n
\n
[section]\n
 key2 - value2\n
[section2]\n
key3\n
[key]4\n
\n
\n")

 (define result (read-to-list (make-ini-file-generator (open-input-string source) #\- #\#)))

 (test-equal
     '((|| key1 "value1")
       (section key2 "value2")
       (section2 #f "key3")
       (section2 #f "[key]4"))
   result))



;; More tests, after the SRFI document.
(test-group
 "more"
 (let* ((port (open-output-string))
        (acc (make-ini-file-accumulator port #\: #\#)))
   (acc '(s a "1"))
   (acc '(s b "2"))
   (acc "note")
   (acc '(t c "3"))
   (test-equal "[s]\na:1\nb:2\n# note\n[t]\nc:3\n" (get-output-string port)))
 (let ((g (make-ini-file-generator (open-input-string "[a]\nx=1\n"))))
   (test-equal '(a x "1") (g))
   (test-assert (eof-object? (g)))
   (test-assert (eof-object? (g)))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-233")
  (exit (if (zero? failures) 0 1)))
