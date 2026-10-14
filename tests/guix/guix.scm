;;; The `guix' command on --guile: Guix's own bin/guix script, which
;;; defines `main' and is run by Guile as `guile -e main -s'.
;;;
;;;   pseudoscheme --guile tests/guix/guix.scm build -d hello
;;;
;;; GUIX_SCRIPT names the script (tests/guix/run.sh sets it).
;;; GUIX_TRACE_LOADS=1 prints each file primitive-load-path loads, and
;;; when it's done, with the time, on the error port.

(when (getenv "GUIX_TRACE_LOADS")
  (let ((load-path-file primitive-load-path))
    (set! primitive-load-path
          (lambda (name . rest)
            (format (current-error-port) "~a load ~a~%" (current-time) name)
            (call-with-values (lambda () (apply load-path-file name rest))
              (lambda results
                (format (current-error-port) "~a done ~a~%" (current-time) name)
                (apply values results)))))))

(load (or (getenv "GUIX_SCRIPT") "/root/.config/guix/current/bin/guix"))
(main (cons "guix" (cdr (command-line))))
