;;; SRFI 112: environment inquiry.  Written for Pseudoscheme: the SRFI's
;;; implementation is a sketch for an imaginary Scheme, and its
;;; procedures correspond to Common Lisp's, so here they are those
;;; functions of the host Lisp (software-type, software-version,
;;; machine-type, machine-instance), with NIL (no answer) returned as #f.
(define-library (srfi 112)
  (export implementation-name implementation-version
          cpu-architecture machine-name os-name os-version)
  (import (scheme base)
          (prefix (only (cl common-lisp) software-type software-version
                        machine-type machine-instance)
                  cl:))
  (begin
    (define (string-or-false x) (if (string? x) x #f))
    (define (implementation-name) "pseudoscheme")
    (define (implementation-version) "3.0")
    (define (cpu-architecture) (string-or-false (cl:machine-type)))
    (define (machine-name) (string-or-false (cl:machine-instance)))
    (define (os-name) (string-or-false (cl:software-type)))
    (define (os-version) (string-or-false (cl:software-version)))))
