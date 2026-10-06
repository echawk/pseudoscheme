#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :239 list-case)
  (export list-case _)
  (import (rnrs (6)))

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

  )
