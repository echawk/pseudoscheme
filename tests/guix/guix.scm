;;; The `guix' command on --guile: Guix's own bin/guix script, which
;;; defines `main' and is run by Guile as `guile -e main -s'.
;;;
;;;   quasischeme --guile tests/guix/guix.scm build -d hello
;;;
;;; GUIX_SCRIPT names the script (tests/guix/run.sh sets it).
;;; GUIX_TRACE_LOADS=1 prints each file primitive-load-path loads, and
;;; when it's done, with the time and the heap in use (MB), on the error
;;; port.

(when (getenv "GUIX_TRACE_LOADS")
  (let ((load-path-file primitive-load-path))
    (set! primitive-load-path
          (lambda (name . rest)
            (format (current-error-port) "~a load ~a~%" (current-time) name)
            (call-with-values (lambda () (apply load-path-file name rest))
              (lambda results
                (format (current-error-port) "~a done ~a ~aMB~%" (current-time) name
                        (let ((stats (gc-stats)))
                          (quotient (- (assq-ref stats 'heap-size) (assq-ref stats 'heap-free-size))
                                    1048576)))
                (apply values results)))))))

(load (or (getenv "GUIX_SCRIPT") "/root/.config/guix/current/bin/guix"))
(main (cons "guix" (cdr (command-line))))
