;;; tests/libguile/ext-ports.c's port types, in real Guile and in ours.
;;; EXT_PORTS names the compiled extension.

(use-modules (ice-9 rdelim) (rnrs bytevectors) (rnrs io ports))
(load-extension (getenv "EXT_PORTS") "init_ports")

(define (show label thunk)
  (write label)
  (display " => ")
  (write (catch #t thunk (lambda (key . args) (list 'error key))))
  (newline))

(show 'read-line (lambda () (read-line (open-source "first line\nsecond"))))
(show 'read-all (lambda () (let ((p (open-source "abc\ndef"))) (list (read-line p) (read-line p) (eof-object? (read-line p))))))
(show 'get-bytevector-all (lambda () (get-bytevector-all (open-source "xyz"))))
(show 'read-from-c (lambda () (read-from-c (open-source "hello, world") 5)))
(show 'write (lambda ()
               (let ((p (open-sink)))
                 (display "Hello, " p)
                 (write 'world p)
                 (close-port p)
                 (list (sink-contents) (sink-closes)))))
(show 'port? (lambda () (list (port? (open-source "")) (input-port? (open-source "")) (output-port? (open-sink)))))
