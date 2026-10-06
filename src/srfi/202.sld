;;; SRFI 202: pattern-matching variant of the and-let* form that
;;; supports multiple values.  Panicz Maciej Godek's Guile sample
;;; implementation (reference/srfi-202/srfi-202.scm; MIT licence, per
;;; the SRFI document, in reference/srfi-202/LICENSE): its and-let*/match
;;; macro, unmodified but for indentation, copied here because the file
;;; is a Guile module.  Its (ice-9 match) is Alex Shinn's matcher, here
;;; (srfi private srfi-201-match).
;;;
;;; and-let* is a different binding from SRFI 2's, so don't import both
;;; (srfi 2) and (srfi 202) unqualified.
(define-library (srfi 202)
  (export (rename and-let*/match and-let*))
  (import (scheme base)
          (only (rnrs syntax-case) syntax-case syntax identifier?)
          (srfi private srfi-201-match))
  (begin
    (define-syntax and-let*/match
      (lambda (stx)
        (syntax-case stx (values)

          ((_)
           #'#t)

          ((_ ())
           #'#t)

          ((_ () body ...)
           #'(let () body ...))

          ((_ ((name binding) rest ...) body ...)
           (identifier? #'name)
           #'(let ((name binding))
               (and name
                    (and-let*/match (rest ...)
                                    body ...))))

          ((_ (((values . structure) binding) rest ...)
              body ...)
           #'(call-with-values (lambda () binding)
               (lambda args
                 (match args
                   (structure
                    (and-let*/match (rest ...)
                                    body ...))
                   (_ #f)))))

          ((_ ((value binding) rest ...) body ...)
           #'(match binding
               (value
                (and-let*/match (rest ...)
                                body ...))
               (_ #f)))

          ((_ ((condition) rest ...)
              body ...)
           #'(and condition
                  (and-let*/match (rest ...)
                                  body ...)))

          ((_ ((value * ... expression) rest ...) body ...)
           (identifier? #'value)
           #'(call-with-values (lambda () expression)
               (lambda args
                 (match args
                   ((value * ... . _)
                    (and value
                         (and-let*/match (rest ...)
                                         body ...)))
                   (_ #f)))))

          ((_ ((value ... expression) rest ...) body ...)
           #'(call-with-values (lambda () expression)
               (lambda args
                 (match args
                   ((value ... . _)
                    (and-let*/match (rest ...)
                                    body ...))
                   (_ #f)))))

          )))))
