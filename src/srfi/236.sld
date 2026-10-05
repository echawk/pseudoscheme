;;; SRFI 236: evaluating expressions in an unspecified order.  The
;;; SRFI's R7RS sample implementation by Marc Nieper-Wißkirchen,
;;; verbatim from the SRFI document (MIT licence: Copyright (C) 2022
;;; Marc Nieper-Wißkirchen).  It evaluates left to right.
(define-library (srfi 236)
  (export independently)
  (import (scheme base))
  (begin
    (define-syntax independently
      (syntax-rules ()
	((independently expr ...)
	 (independently-aux (expr ...)))))
    (define-syntax independently-aux
      (syntax-rules ()
	((independently-aux () (expr tmp) ...)
	 (let ((tmp (begin expr #f)) ...) (values)))
	((independently-aux (expr . exprs) . binds)
	 (independently-aux exprs (expr tmp) . binds))))))
