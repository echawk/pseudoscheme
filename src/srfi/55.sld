;;; SRFI 55: require-extension.  Written for Pseudoscheme: the SRFI's
;;; implementation is a sketch on LOAD, while here an extension is a
;;; library, so (require-extension (srfi 1 13)) is (import (srfi 1)
;;; (srfi 13)).  A clause other than (srfi N ...) is taken to be a
;;; library name: (require-extension (scheme char)) imports (scheme
;;; char).
;;;
;;; The scope that supports it is the REPL's top level (the SRFI asks
;;; for at least one), where an import may appear anywhere.  In a
;;; program or library body, where imports must come first, the
;;; expansion is an error; use import there.
(define-library (srfi 55)
  (export require-extension)
  (import (scheme base)
          (rnrs syntax-case)
          (rename (only (pseudoscheme) import) (import %import)))
  (begin
    (define-syntax require-extension
      (lambda (stx)
        ;; R7RS's (srfi 1) is psyntax's (srfi :1), as the R7RS front
        ;; end translates import sets.
        (define (srfi-library n)
          (unless (and (integer? n) (exact? n) (>= n 0))
            (syntax-violation 'require-extension "not a SRFI number" stx n))
          (list 'srfi (string->symbol (string-append ":" (number->string n)))))
        (define (translate-name name)
          (map (lambda (part)
                 (if (and (integer? part) (exact? part))
                     (string->symbol (string-append ":" (number->string part)))
                     part))
               name))
        (define (clause->libraries clause)
          (cond ((and (pair? clause) (eq? (car clause) 'srfi))
                 (map srfi-library (cdr clause)))
                ((and (list? clause) (pair? clause))
                 (list (translate-name clause)))
                (else
                 (syntax-violation 'require-extension "bad clause" stx clause))))
        (syntax-case stx ()
          ((_ clause ...)
           (with-syntax (((lib ...)
                          (datum->syntax
                           #'here
                           (apply append
                                  (map clause->libraries
                                       (syntax->datum #'(clause ...)))))))
             #'(%import lib ...))))))))
