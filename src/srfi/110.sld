;;; SRFI 110: Sweet-expressions (t-expressions).  The SRFI's
;;; implementation, kernel.scm of the readable project, by David A.
;;; Wheeler and Alan Manuel K. Gloria (MIT; reference/srfi-110/), its R5RS
;;; branch, included whole.  Two changes, marked PSEUDOSCHEME: the
;;; Guile-only cond-expand has an empty else clause; and replace-read-with,
;;; which assigned read (imported here), is an error.  The Guile
;;; procedures it uses even so (catch, throw, debug-set!, force-output,
;;; and for its writers type-of and the like, which find no record types
;;; here) are defined here.
;;;
;;; A file or port is read as sweet-expressions after #!sweet (the
;;; reader loads this library then; src/core.lisp, reader-directive).
;;; sweet-read, neoteric-read and curly-infix-read read one datum.
(define-library (srfi 110)
  (export curly-infix-read neoteric-read sweet-read
          curly-write-simple neoteric-write-simple
          curly-write curly-write-cyclic curly-write-shared
          neoteric-write neoteric-write-cyclic neoteric-write-shared)
  (import (scheme base) (scheme char) (scheme cxr) (scheme read) (scheme write)
          (scheme case-lambda) (only (srfi 4) list->u8vector) (srfi 1)
          (rename (srfi 69) (make-hash-table srfi-69-make-hash-table)
                  (hash-table? srfi-69-hash-table?)))
  (begin
    ;; Guile procedures the kernel uses even in its R5RS branch
    (define-syntax debug-set!           ; (debug-set! stack 500000)
      (syntax-rules () ((_ . options) #f)))
    (define (force-output port) (flush-output-port port))
    ;; records' printers and slots, for the writers (its type-of is #f)
    (define (type-of x) #f)
    (define (type? x) #f)
    (define (type-printer type) #f)
    (define (type-num-slots type) 0)
    (define (slot-ref type x i) #f)
    (define (throw key . args)
      (raise (cons 'srfi-110-throw (cons key args))))
    (define (catch key thunk handler)
      (guard (e ((and (pair? e) (eq? (car e) 'srfi-110-throw) (eq? (cadr e) key))
                 (apply handler (cdr e))))
        (thunk))))
  (include "reference/srfi-110/kernel.scm"))
