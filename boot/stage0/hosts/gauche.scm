;;; boot/stage0/hosts/gauche.scm -- stage0 on Gauche (R7RS, no R6RS).
;;;
;;; Run in a directory holding psyntax's sources (psyntax/*.ss and
;;; psyntax-buildscript.ss):  gosh .../boot/stage0/hosts/gauche.scm
;;; It writes psyntax-pseudoscheme.pp there.  See boot/stage0/README.md.

(define (s0:host-eval x) (eval x (interaction-environment)))
(define (s0:make-table) (make-hash-table 'eq?))
(define (s0:table-ref t k default) (hash-table-get t k default))
(define (s0:table-set! t k v) (hash-table-put! t k v))
(define (s0:table-delete! t k) (hash-table-delete! t k))
(define (s0:table-keys t) (hash-table-keys t))
(define (s0:host-delete-file name) (sys-unlink name))

(load (string-append (sys-dirname (current-load-path)) "/../stage0.scm"))
(s0:build)
