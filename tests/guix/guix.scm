;;; The `guix' command on --guile: Guix's own bin/guix script, which
;;; defines `main' and is run by Guile as `guile -e main -s'.
;;;
;;;   pseudoscheme --guile tests/guix/guix.scm build -d hello
;;;
;;; GUIX_SCRIPT names the script (tests/guix/run.sh sets it).

(load (or (getenv "GUIX_SCRIPT") "/root/.config/guix/current/bin/guix"))
(main (cons "guix" (cdr (command-line))))
