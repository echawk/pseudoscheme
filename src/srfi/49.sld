;;; SRFI 49: Indentation-sensitive syntax (I-expressions).  The reader
;;; of sugar.scm, the implementation in the SRFI document by Egil Möller
;;; (MIT; reference/srfi-49/), adapted in sugar-body.scm: Guile's t, #\ht
;;; and sugar-read-save are else, #\tab and read, '. is a symbol made
;;; with string->symbol (R7RS doesn't read . as one), and its Guile module
;;; and loading procedures are left out.
;;;
;;; A file or port is read as I-expressions after #!srfi-49 (the reader
;;; loads this library then; src/core.lisp, reader-directive).
;;; i-expression-read reads one, skipping empty lines first (sugar-read,
;;; the document's name, doesn't).
(define-library (srfi 49)
  (export i-expression-read sugar-read group)
  (import (scheme base) (scheme read))
  (include "reference/srfi-49/sugar-body.scm")
  (begin
    (define group 'group)
    ;; sugar-read, after any empty lines (as after #!srfi-49), which it
    ;; would read as ()
    (define (i-expression-read . port)
      (let ((port (if (null? port) (current-input-port) (car port))))
        (let skip ()
          (when (eqv? (peek-char port) #\newline)
            (read-char port)
            (skip)))
        (sugar-read port)))))
