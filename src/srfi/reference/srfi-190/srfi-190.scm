;;; The sample implementation from the SRFI 190 document
;;; (https://srfi.schemers.org/srfi-190/srfi-190.html), verbatim.
;;; Copyright (C) Marc Nieper-Wißkirchen (2020); MIT licence, see LICENSE.
;;; It needs SRFI 139 syntax parameters, which Pseudoscheme lacks, so
;;; (srfi 190) does not include it; see 190.sld.

(define-syntax-parameter yield
  (lambda (stx)
    (syntax-case stx ()
      (_ (error "yield used outside coroutine generator" stx)))))

(define-syntax coroutine-generator
  (lambda (stx)
    (syntax-case stx ()
      ((_ . body)
       #'(make-coroutine-generator
          (lambda (%yield)
            (syntax-parameterize ((yield (identifier-syntax %yield)))
              . body)))))))

(define-syntax define-coroutine-generator
  (lambda (stx)
    (syntax-case stx ()
      ((_ name . body) #'(define name (coroutine-generator . body))))))
