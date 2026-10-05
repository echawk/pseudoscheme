; boot/stage0/hosts/scheme48.scm -- stage0 on Scheme 48 (R5RS).
;
; Fed to Scheme 48's REPL, in a directory holding psyntax's sources
; (psyntax/*.ss, psyntax-buildscript.ss) and a copy of boot/stage0/ as
; stage0/, which is how boot/psyntax.sh --seed=stage0 lays it out:
;   scheme48 -h 0 < stage0/hosts/scheme48.scm
; It writes psyntax-pseudoscheme.pp there.  ,batch makes an error exit
; rather than enter a nested REPL.  See boot/stage0/README.md.

,batch on
,open tables signals srfi-6 posix-files

(define (s0:host-eval x) (eval x (interaction-environment)))

; Scheme 48's TABLE-REF answers #f for a missing key, so values are boxed.
(define (s0:make-table) (make-table))
(define (s0:table-ref t k default)
  (let ((box (table-ref t k)))
    (if box (car box) default)))
(define (s0:table-set! t k v) (table-set! t k (list v)))
(define (s0:table-delete! t k) (table-set! t k #f))
(define (s0:table-keys t)
  (let ((keys '()))
    (table-walk (lambda (k v) (set! keys (cons k keys))) t)
    keys))

(define (s0:host-delete-file name) (unlink name))
(define (s0:host-file-exists? name) (accessible? name (access-mode exists)))

,load stage0/stage0.scm
(s0:build)
,exit
