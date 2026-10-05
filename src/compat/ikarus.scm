;;; -*- Mode: Scheme -*-
;;;; The body of Pseudoscheme's (ikarus) library: the Ikarus Scheme
;;;; extensions that the Ikarus variants of libraries (foo.ikarus.sls)
;;;; import.  The library form around this, which re-exports R6RS as
;;;; Ikarus's does, is built by src/r7rs/front.lisp; what Ikarus shares
;;;; with Chez comes from (chezscheme), host procedures with a % prefix.
;;;;
;;;; Only what libraries actually use is here: xitomatl's compat
;;;; libraries, mostly.  No FFI, no engines (timer interrupts), no
;;;; (ikarus system $...) internals.

(define (fxadd1 n) (fx+ n 1))
(define (fxsub1 n) (fx- n 1))

(define (die who message . irritants)
  (apply assertion-violation who message irritants))

(define (port-closed? port)
  (not (if (input-port? port) (%input-port-open? port) (%output-port-open? port))))

;; The directories libraries are searched for in.
(define (library-path) (map car (library-directories)))

;; (stale-when guard e ...) makes a compiled library stale when guard
;; is true; libraries here are always expanded from source.
(define-syntax stale-when
  (syntax-rules ()
    ((_ guard e ...) (begin e ...))))

;; Ikarus's reader attaches source positions; ours doesn't.
(define read-annotated
  (case-lambda
    (() (read))
    ((port) (read port))))

(define environment? %psyntax:environment?)
(define environment-symbols %psyntax:environment-symbols)

(define print-condition
  (case-lambda
    ((c) (print-condition c (current-error-port)))
    ((c port)
     (display "Condition:" port)
     (for-each (lambda (part) (display " " port) (write part port))
               (if (condition? c)
                   (append (if (who-condition? c) (list (condition-who c)) '())
                           (if (message-condition? c) (list (condition-message c)) '())
                           (if (irritants-condition? c) (condition-irritants c) '()))
                   (list c)))
     (newline port))))
