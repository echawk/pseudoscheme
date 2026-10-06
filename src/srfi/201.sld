;;; SRFI 201: syntactic extensions to the core Scheme bindings.  Panicz
;;; Maciej Godek's Guile sample implementation
;;; (reference/srfi-201/srfi-201.scm; MIT licence, per the SRFI
;;; document, in reference/srfi-201/LICENSE), copied into this file
;;; because it is a Guile module.  Its (ice-9 match) is Alex Shinn's
;;; matcher, here (srfi private srfi-201-match).  Changes:
;;;   - match-let/error's error call leaves out Guile's
;;;     (current-source-location);
;;;   - mlambda's fenders test (null? (syntax->datum x)) where the
;;;     original tests (null? x) of a syntax object, which psyntax wraps,
;;;     so that a lambda whose parameters are all identifiers (or empty)
;;;     expands to the core lambda as intended, not to a match;
;;;   - for the same reason, the fenders' (every identifier? #'(x ...))
;;;     are (every identifier? (syntax->list #'(x ...))), with
;;;     syntax->list from (srfi private srfi-201-syntax).
;;; The optional multiple-value or is provided.
;;;
;;; The exported lambda, define, let, let* and or replace the core
;;; forms, as the SRFI intends: import (except (scheme base) lambda
;;; define let let* or) with this library.  Patterns are SRFI 200
;;; (Wright-style) patterns, as Shinn's match accepts them, e.g.
;;; (lambda (`(,x . ,y)) ...).
(define-library (srfi 201)
  (export (rename mlambda lambda)
          (rename cdefine define)
          (rename named-match-let-values let)
          (rename match-let*-values let*)
          (rename or/values or))
  (import (scheme base)
          (only (rnrs syntax-case) syntax-case syntax identifier?
                syntax->datum)
          (only (srfi 1) every any)
          (srfi private srfi-201-match)
          (srfi private srfi-201-syntax))
  (begin
    (define-syntax mlambda
      (lambda (stx)
        (syntax-case stx ()
          ;; lambda with no body is treated as a pattern predicate,
          ;; returning #t if the argument matches, and #f otherwise
          ((_ args)
           #'(lambda args*
               (match args*
                 (args #t)
                 (_ #f))))

          ;; in case of patterns consisting entitely
          ;; of identifiers, we desugar to the core lambda
          ((_ (first-arg ... last-arg . rest-args) . body)
           (and (every identifier? (syntax->list #'(first-arg ... last-arg)))
                (or (identifier? #'rest-args)
                    (null? (syntax->datum #'rest-args))))
           #'(lambda (first-arg ... last-arg . rest-args) . body))

          ((_ arg body ...)
           (or (identifier? #'arg) (null? (syntax->datum #'arg)))
           #'(lambda arg body ...))

          ((_ pattern body ...)
           #'(lambda args
               (match args
                 (pattern body ...)
                 (_ (error
                     `((mlambda pattern body ...) ,args)))))))))

    (define-syntax cdefine
      (syntax-rules ()
        ;; for consistency with bodyless lambdas,
        ;; bodyless curried definitions only pattern-match
        ;; on their arguments, and return #t if all matches
        ;; succeed, or #f if some of the matches fail
        ((_ (prototype . args))
         (cdefine-bodyless (prototype . args) #t))

        ((_ ((head . tail) . args) body ...)
         (cdefine (head . tail)
                  (mlambda args body ...)))
        ((_ (function . args) body ...)
         (define function
           (mlambda args body ...)))
        ((_ . rest)
         (define . rest))))

    (define-syntax cdefine-bodyless
      (syntax-rules ()
        ((_ (function . pattern) result . args)
         (cdefine-bodyless function
                           (match arg
                             (pattern result)
                             (_ #f))
                           arg . args))

        ((_ name result args ... arg)
         (cdefine-bodyless name (lambda arg result) args ...))

        ((_ name value)
         (define name value))))

    (define-syntax match-let/error
      (syntax-rules ()
        ((_ ((structure expression) ...)
            body + ...)
         ((lambda args
            (match args
              ((structure ...) body + ...)
              (_ (error 'match-let/error
                        '((structure expression) ...)
                        expression ...))))
          expression ...))))

    (define-syntax named-match-let-values
      (lambda (stx)
        (syntax-case stx (values)
          ((_ ((identifier expression) ...)
              body + ...)
           (every identifier? (syntax->list #'(identifier ...)))
           ;; optimization: plain "let" form
           #'(let ((identifier expression) ...)
               body + ...))

          ((_ name ((identifier expression) ...)
              body + ...)
           (and (identifier? #'name)
                (every identifier? (syntax->list #'(identifier ...))))
           ;; optimization: regular named-let
           #'(let name ((identifier expression) ...)
               body + ...))

          ((_ name (((values . structure) expression))
              body + ...)
           (identifier? #'name)
           #'(letrec ((name (mlambda structure body + ...)))
               (call-with-values (lambda () expression) name)))

          ((_ (((values . structure) expression)) body + ...)
           #'(call-with-values (lambda () expression)
               (mlambda structure body + ...)))

          ((_ name ((structure expression) ...)
              body + ...)
           (and (identifier? #'name)
                (any (lambda (pattern)
                       (and (pair? pattern)
                            (eq? (car pattern) 'values)))
                     (syntax->datum #'(structure ...))))
           #'(syntax-error "let can only handle one binding \
in the presence of a multiple-value binding"))

          ((_ name ((structure expression) ...)
              body + ...)
           (identifier? #'name)
           #'(letrec ((name (mlambda (structure ...) body + ...)))
               (name expression ...)))

          ((_ ((structure expression) ...)
              body + ...)
           (any (lambda (pattern)
                  (and (pair? pattern)
                       (eq? (car pattern) 'values)))
                (syntax->datum #'(structure ...)))
           #'(syntax-error "let can only handle one binding \
in the presence of a multiple-value binding"))

          ((_ ((structure expression) ...)
              body + ...)
           #'(match-let/error ((structure expression) ...)
                              body + ...))

          ((_ ((structure structures ... expression)) body + ...)
           #'(call-with-values (lambda () expression)
               (mlambda (structure structures ... . _)
                        body + ...)))

          ((_ name ((structure structures ... expression))
              body + ...)
           (identifier? #'name)
           #'(letrec ((name (mlambda (structure structures ...)
                                     body + ...)))
               (call-with-values (lambda () expression) name))))))

    (define-syntax or/values
      (syntax-rules ()
        ((_)
         #false)

        ((or/values final)
         final)

        ((or/values first . rest)
         (call-with-values (lambda () first)
           (lambda result
             (if (and (pair? result) (car result))
                 (apply values result)
                 (or/values . rest)))))))

    (define-syntax match-let*-values
      (lambda (stx)
        (syntax-case stx ()
          ((_ ((identifier expression) ...) body + ...)
           (every identifier? (syntax->list #'(identifier ...)))
           ;; optimization/base case: regular let*
           #'(let* ((identifier expression) ...)
               body + ...))
          ((_ (binding . bindings) body + ...)
           #'(named-match-let-values (binding)
                                     (match-let*-values
                                      bindings
                                      body + ...))))))))
