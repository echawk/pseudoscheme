;;; SRFI 26: cut, cute.  Written for Pseudoscheme, after the SRFI's
;;; reference implementation.
(define-library (srfi 26)
  (export cut cute <> <...>)
  (import (scheme base))
  (begin
    (define-syntax <> (syntax-rules ()))
    (define-syntax <...> (syntax-rules ()))
    (define-syntax srfi-26-internal-cut
      (syntax-rules (<> <...>)
        ((_ (slot-name ...) (proc arg ...))
         (lambda (slot-name ...) ((begin proc) arg ...)))
        ((_ (slot-name ...) (proc arg ...) <...>)
         (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
        ((_ (slot-name ...) (position ...) <> . se)
         (srfi-26-internal-cut (slot-name ... x) (position ... x) . se))
        ((_ (slot-name ...) (position ...) nse . se)
         (srfi-26-internal-cut (slot-name ...) (position ... nse) . se))))
    (define-syntax srfi-26-internal-cute
      (syntax-rules (<> <...>)
        ((_ (slot-name ...) nse-bindings (position ...))
         (let nse-bindings (lambda (slot-name ...) (position ...))))
        ((_ (slot-name ...) nse-bindings (position ...) <...>)
         (let nse-bindings (lambda (slot-name ... . x) (apply position ... x))))
        ((_ (slot-name ...) nse-bindings (position ...) <> . se)
         (srfi-26-internal-cute (slot-name ... x) nse-bindings (position ... x) . se))
        ((_ slot-names nse-bindings (position ...) nse . se)
         (srfi-26-internal-cute slot-names ((x nse) . nse-bindings) (position ... x) . se))))
    (define-syntax cut
      (syntax-rules ()
        ((_ . slots-or-exprs) (srfi-26-internal-cut () () . slots-or-exprs))))
    (define-syntax cute
      (syntax-rules ()
        ((_ . slots-or-exprs) (srfi-26-internal-cute () () () . slots-or-exprs))))))
