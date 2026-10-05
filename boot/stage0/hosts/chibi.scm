;;; boot/stage0/hosts/chibi.scm -- stage0 on Chibi Scheme (R7RS-small).
;;;
;;; Run in a directory holding psyntax's sources (psyntax/*.ss and
;;; psyntax-buildscript.ss):  chibi-scheme .../boot/stage0/hosts/chibi.scm
;;; It writes psyntax-pseudoscheme.pp there.  See boot/stage0/README.md.
;;;
;;; stage0 evaluates what it expands with (s0:host-eval form), so it and
;;; this adapter's definitions are loaded into the interaction
;;; environment, where that evaluation happens.

(import (scheme base) (scheme eval) (scheme repl) (scheme file) (scheme load)
        (scheme process-context) (chibi filesystem) (chibi pathname))

(define env (interaction-environment))

(eval '(import (scheme base) (scheme char) (scheme cxr) (scheme case-lambda)
               (scheme eval) (scheme repl) (scheme file) (scheme read)
               (scheme write) (srfi 69))
      env)

(for-each
 (lambda (form) (eval form env))
 '((define (s0:host-eval x) (eval x (interaction-environment)))
   (define (s0:make-table) (make-hash-table eq?))
   (define (s0:table-ref t k default) (hash-table-ref/default t k default))
   (define (s0:table-set! t k v) (hash-table-set! t k v))
   (define (s0:table-delete! t k) (hash-table-delete! t k))
   (define (s0:table-keys t) (hash-table-keys t))
   (define (s0:host-delete-file name) (delete-file name))
   (define (s0:host-file-exists? name) (file-exists? name))))

(load (path-resolve "../stage0.scm" (path-directory (car (command-line)))) env)
(eval '(s0:build) env)
