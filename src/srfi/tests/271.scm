;;; Tests for SRFI 271: the sample implementation's run-tests.scm (without its custom test runner), and the example from the SRFI document.
(import (scheme base) (scheme process-context) (srfi 64) (scheme read) (scheme write) (prefix (srfi 271 randomized) r:) (prefix (srfi 271 determinized) d:) (prefix (only (srfi 271) make-random-port) s:))

(test-begin "srfi-271")

(test-assert "randomized random ports are input ports"
  (input-port? (r:make-random-port)))

(test-assert "determinized random ports are input ports"
  (input-port? (d:make-random-port)))

(test-assert "make-random-port (determinized) accepts an input port"
  (let ((p1 (d:make-random-port)))
    (d:make-random-port p1)))

(test-assert "random-port?"
  (call-with-port (d:make-random-port) d:random-port?))

(test-assert "random-port-initialization-error?"
  (guard (con
           ((d:random-port-initialization-error? con) #t)
           (else #f))
    (call-with-port
     (open-input-bytevector '#u8())
     d:make-random-port)))

(test-assert "random-port-state?"
  (call-with-port (d:make-random-port)
                  (lambda (p)
                    (d:random-port-state? (d:random-port-state p)))))

(test-assert "det. ports with equal states give same initial bytes"
  (let* ((p1 (d:make-random-port))
         (p2 (d:make-random-port (d:random-port-state p1))))
    (equal? (read-bytevector 8 p1) (read-bytevector 8 p2))))

(test-assert "det. ports with equal states (random-port-state=?)"
  (let* ((p1 (d:make-random-port))
         (p2 (d:make-random-port (d:random-port-state p1)))
         (p3 (d:make-random-port (d:random-port-state p2))))
    (read-u8 p1)
    (read-u8 p2)
    (read-u8 p3)
    (d:random-port-state=? (d:random-port-state p1)
                           (d:random-port-state p2)
                           (d:random-port-state p3))))



;; The example of usage from the SRFI document.
(define rport (d:make-random-port))
(test-assert "different initial ports give different bytes"
  (let ((rport2 (d:make-random-port (r:make-random-port))))
    (not (equal? (read-bytevector 100 rport)
                 (read-bytevector 100 rport2)))))
(define rport-state (d:random-port-state rport))
(define rport3 (d:make-random-port rport-state))
(test-equal (read-bytevector 100 rport) (read-bytevector 100 rport3))
(test-assert (d:random-port-state=? (d:random-port-state rport)
                                    (d:random-port-state rport3)))
(test-assert "write/read invariance"
  (let ((output (open-output-string)))
    (write rport-state output)
    (let* ((str (get-output-string output))
           (input (open-input-string str))
           (serialized-state (read input)))
      (d:random-port-state=? rport-state serialized-state))))

;; More
(test-assert (input-port? (s:make-random-port)))
(test-assert (binary-port? (r:make-random-port)))
(test-eqv 1000 (bytevector-length (read-bytevector 1000 (r:make-random-port))))
(test-assert (not (d:random-port? (r:make-random-port))))
(test-assert (not (d:random-port? 42)))
(test-assert (not (d:random-port-state? (make-bytevector 32 0))))
(test-assert (not (d:random-port-state? (make-bytevector 31 1))))
(test-assert (d:random-port-state? (make-bytevector 32 1)))
;; Ports initialized from the same data have the same state and bytes.
(let* ((seed (let ((bv (make-bytevector 64)))
               (do ((i 0 (+ i 1))) ((= i 64) bv)
                 (bytevector-u8-set! bv i (modulo (* i 37) 256)))))
       (p1 (d:make-random-port (open-input-bytevector seed)))
       (p2 (d:make-random-port (open-input-bytevector seed))))
  (test-assert (d:random-port-state=? (d:random-port-state p1)
                                      (d:random-port-state p2)))
  (test-equal (read-bytevector 50 p1) (read-bytevector 50 p2)))
;; A state is a snapshot: reading the port doesn't change it.
(let* ((p (d:make-random-port))
       (s (d:random-port-state p)))
  (read-bytevector 10 p)
  (test-assert (not (d:random-port-state=? s (d:random-port-state p))))
  (test-equal (read-bytevector 10 (d:make-random-port s))
              (begin (read-bytevector 10 (d:make-random-port s)))))
;; A fixed state gives a fixed sequence (xoshiro256++ from state 1..32).
(let* ((s (let ((bv (make-bytevector 32)))
            (do ((i 0 (+ i 1))) ((= i 32) bv) (bytevector-u8-set! bv i (+ i 1)))))
       (a (read-bytevector 16 (d:make-random-port s)))
       (b (read-bytevector 16 (d:make-random-port s))))
  (test-equal a b))
(test-assert (not (eof-object? (read-u8 (d:make-random-port)))))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-271")
  (exit (if (zero? failures) 0 1)))
