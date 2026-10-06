;;; Tests for SRFI 5, from the examples in the SRFI document.
(import (except (scheme base) let) (scheme process-context) (scheme write)
        (srfi 64) (srfi 5))
(test-begin "srfi-5")

(test-equal "unnamed" 3 (let ((a 1) (b 2)) (+ a b)))
(test-equal "no bindings" 'ok (let () 'ok))
(test-equal "named, defun-style" 55
  (let fibonacci ((n 10) (i 0) (f0 0) (f1 1))
    (if (= i n) f0 (fibonacci n (+ i 1) f1 (+ f0 f1)))))
(test-equal "named, signature-style" 55
  (let (fibonacci (n 10) (i 0) (f0 0) (f1 1))
    (if (= i n) f0 (fibonacci n (+ i 1) f1 (+ f0 f1)))))
(test-equal "signature-style, no bindings" 'done
  (let (loop) 'done))
(test-equal "signature-style, rest argument" "345"
  (let ((port (open-output-string)))
    (let (blast (port port) . (x (+ 1 2) 4 5))
      (if (null? x)
          (get-output-string port)
          (begin
            (write (car x) port)
            (apply blast port (cdr x)))))))
(test-equal "defun-style, rest argument" '(1 (2 3))
  (let loop ((a 1) . (r 2 3))
    (list a r)))
(test-equal "defun-style, rest argument looping" 10
  (let loop ((acc 0) . (xs 1 2 3 4))
    (if (null? xs) acc (apply loop (+ acc (car xs)) (cdr xs)))))
(test-equal "unnamed, rest binding" '(1 (2 3))
  (let ((a 1) . (r 2 3)) (list a r)))
(test-equal "named, only a rest binding" '(1 2 3)
  (let (f . (r 1 2 3)) r))
(test-equal "defun-style, only a rest binding" '()
  (let f (r) r))
(test-equal "hygiene: standard let still there" 7
  (let ((let 3)) (+ let 4)))

(let ((failures (test-runner-fail-count (test-runner-current))))
  (test-end "srfi-5")
  (exit (if (zero? failures) 0 1)))
