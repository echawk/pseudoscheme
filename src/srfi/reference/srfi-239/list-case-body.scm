;;; The body of the library (srfi :239 list-case), from Marc
;;; Nieper-Wißkirchen's sample implementation of SRFI 239
;;; (list-case.sls here), unmodified.

  (define-syntax list-case
    (lambda (stx)
      (define who 'list-case)
      (define rename-identifiers
        (lambda (x*)
          (map (lambda (x y)
                 (if (free-identifier=? x #'_)
                     y
                     x))
               x* (generate-temporaries x*))))
      (define compile-clauses
        (lambda (clauses)
          (let f ([clauses clauses] [pair-clause #f] [null-clause #f] [dotted-clause #f])
            (if (null? clauses)
                (let ([clauses
                       (filter values (list pair-clause null-clause dotted-clause))])
                  (if (fx=? (length clauses) 3)
                      clauses
                      (append clauses
                              (list #'[else (assertion-violation 'list-case "unhandled expression" tmp)]))))
                (let ([clause (car clauses)] [clauses (cdr clauses)])
                  (define duplicate-clause-violation
                    (lambda ()
                      (syntax-violation who "duplicate clause of the same type" stx clause)))
                  (syntax-case clause ()
                    [[(h . t) body1 ... body2]
                     (and (identifier? #'h)
                          (identifier? #'t))
                     (if pair-clause
                         (duplicate-clause-violation)
                         (f clauses
                            (with-syntax ([(h t) (rename-identifiers #'(h t))])
                              #'[(pair? tmp)
                                 (let ([h (car tmp)] [t (cdr tmp)])
                                   body1 ... body2)])
                            null-clause
                            dotted-clause))]
                    [[() body1 ... body2]
                     (if null-clause
                         (duplicate-clause-violation)
                         (f clauses
                            pair-clause
                            #'[(null? tmp)
                               (letrec* ()
                                 body1 ... body2)]
                            dotted-clause))]
                    [[x body1 ... body2]
                     (identifier? #'x)
                     (if dotted-clause
                         (duplicate-clause-violation)
                         (f clauses
                            pair-clause
                            null-clause
                            (with-syntax ([(x) (rename-identifiers #'(x))])
                              #'[(and (not (null? tmp)) (not (pair? tmp)))
                                 (let ([x tmp])
                                   body1 ... body2)])))]
                    [_
                     (syntax-violation who "invalid clause" stx clause)]))))))
      (syntax-case stx ()
        [(_ expr clause ...)
         (with-syntax ([(clause ...) (compile-clauses #'(clause ...))])
           #'(let ([tmp expr])
               (cond
                clause ...)))])))
