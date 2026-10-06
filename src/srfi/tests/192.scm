;;; Tests for SRFI 192, after the SRFI document; tests/181.scm (the
;;; sample implementation's tests) tests positions on custom ports too.
(import (scheme base) (scheme process-context) (scheme file) (scheme write)
        (srfi 64) (srfi 181) (srfi 192))

(test-begin "srfi-192")

;; Bytevector input ports
(let ((p (open-input-bytevector (bytevector 10 11 12 13 14))))
  (test-assert (port-has-port-position? p))
  (test-assert (port-has-set-port-position!? p))
  (test-eqv 0 (port-position p))
  (read-u8 p)
  (test-eqv 1 (port-position p))
  (peek-u8 p)
  (test-eqv 1 (port-position p))
  (set-port-position! p 3)
  (test-eqv 13 (read-u8 p))
  (set-port-position! p 0)
  (test-eqv 10 (read-u8 p)))

;; String ports
(let ((p (open-input-string "hello")))
  (test-assert (port-has-port-position? p))
  (test-assert (port-has-set-port-position!? p))
  (read-char p)
  (read-char p)
  (let ((pos (port-position p)))
    (test-eqv #\l (read-char p))
    (read-char p)
    (set-port-position! p pos)
    (test-eqv #\l (read-char p))))
(let ((p (open-output-string)))
  (write-string "abc" p)
  (test-assert (port-has-port-position? p))
  (test-eqv 3 (port-position p)))

;; File ports
(define file "srfi-192-test.tmp")
(call-with-output-file file
  (lambda (p) (write-string "0123456789" p)))
(call-with-input-file file
  (lambda (p)
    (test-assert (port-has-port-position? p))
    (test-assert (port-has-set-port-position!? p))
    (set-port-position! p 7)
    (test-eqv #\7 (read-char p))
    (test-eqv 8 (port-position p))))
(let ((p (open-binary-input-file file)))
  (set-port-position! p 2)
  (test-eqv (char->integer #\2) (read-u8 p))
  (test-eqv 3 (port-position p))
  (close-port p))
(let ((p (open-binary-output-file file)))
  (write-bytevector (bytevector 65 66 67 68) p)
  (set-port-position! p 1)
  (write-u8 120 p)
  (close-port p))
(test-equal "AxCD" (call-with-input-file file (lambda (p) (read-line p))))
(delete-file file)

;; Custom ports without positioning
(let ((p (make-custom-binary-input-port "none" (lambda (bv s c) 0) #f #f #f)))
  (test-assert (not (port-has-port-position? p)))
  (test-assert (not (port-has-set-port-position!? p)))
  (test-error (set-port-position! p 0)))

;; A custom textual port: the position after peek-char is that of the
;; peeked character.
(let* ((data "abcdef") (pos 0)
       (p (make-custom-textual-input-port
           "text"
           (lambda (s start count)
             (if (>= pos (string-length data))
                 0
                 (begin (string-set! s start (string-ref data pos))
                        (set! pos (+ pos 1))
                        1)))
           (lambda () pos)
           (lambda (k) (set! pos k))
           #f)))
  (test-eqv #\a (read-char p))
  (test-eqv 1 (port-position p))
  (test-eqv #\b (peek-char p))
  (test-eqv 1 (port-position p))
  (test-eqv #\b (read-char p))
  (test-eqv 2 (port-position p))
  (set-port-position! p 0)
  (test-equal "abcdef" (read-string 10 p)))

;; Invalid positions
(test-assert (i/o-invalid-position-error? (make-i/o-invalid-position-error 5)))
(test-assert (not (i/o-invalid-position-error? (make-file-error))))
(define (picky-port fail)
  (make-custom-binary-input-port
   "picky" (lambda (bv s c) 0) (lambda () 0)
   (lambda (k) (unless (eqv? k 0) (fail k)))
   #f))
(let ((p (picky-port (lambda (k) (raise (make-i/o-invalid-position-error k))))))
  (set-port-position! p 0)
  (test-assert (guard (e ((i/o-invalid-position-error? e) #t))
                 (set-port-position! p 100)
                 #f)))
(let ((p (picky-port (lambda (k) (error "no such position" k)))))
  (test-assert (guard (e ((i/o-invalid-position-error? e) #t))
                 (set-port-position! p 100)
                 #f)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-192")
  (exit (if (zero? failures) 0 1)))
