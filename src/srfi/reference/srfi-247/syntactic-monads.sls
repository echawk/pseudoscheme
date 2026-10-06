#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2023).  All Rights Reserved.
;;
;; SPDX-License-Identifier: MIT

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

(library (srfi :247 syntactic-monads)
  (export
    define-syntactic-monad)
  (import
    (rnrs))

  (define-syntax define-syntactic-monad
    (lambda (x)
      (define who 'define-syntactic-monad)
      (syntax-case x ()
        [(_ name formal ...)
         (for-all identifier? #'(name formal ...))
         #'(define-syntax name
             (lambda (stx)
               (define who 'name)
               (... (define-syntax with-implicit
                      (lambda (x)
                        (syntax-case x ()
                          [(_ (k x ...) e1 ... e2)
                           (for-all identifier? #'(k x ...))
                           #'(with-syntax ([x (datum->syntax #'k 'x)] ...)
                               e1 ... e2)]))))
               (syntax-case stx (lambda case-lambda let let*-values define)
                 [(k lambda formals . body)
                  (with-implicit (k formal ...)
                    #'(lambda (formal ... . formals) . body))]
                 [(k case-lambda [formals . body] (... ...))
                  (with-implicit (k formal ...)
                    #'(case-lambda [(formal ... . formals) . body] (... ...)))]
                 [(k define (proc-name . formals) . body)
                  (with-implicit (k formal ...)
                    #'(define (proc-name formal ... . formals) . body))]
                 [(k let*-values ([formals init] (... ...)) . body)
                  (with-implicit (k formal ...)
                    #'(let*-values ([(formal ... . formals) init] (... ...)) . body))]
                 [(k proc-expr ([x e] (... ...)) arg (... ...))
                  (for-all identifier? #'(x (... ...)))
                  (with-implicit (k formal ...)
                    (define state-variable?
                      (lambda (x)
                        (find (lambda (y)
                                (bound-identifier=? x y))
                              #'(formal ...))))
                    (for-each
                      (lambda (x)
                        (unless (state-variable? x)
                          (syntax-violation who "undefined state variable" stx x)))
                      #'(x (... ...)))
                    #'(let ([proc proc-expr])
                        (let ([x e] (... ...))
                          (proc formal ... arg (... ...)))))]
                 [(k proc-expr)
                  (with-implicit (k formal ...)
                    #'(proc-expr formal ...))]
                 [(k let f ([x e] (... ...)) . body)
                  (for-all identifier? #'(f x (... ...)))
                  (with-implicit (k formal ...)
                    (define state-variable?
                      (lambda (x)
                        (find (lambda (y)
                                (bound-identifier=? x y))
                              #'(formal ...))))
                    (let-values ([(bindings more-bindings)
                                  (partition (lambda (binding)
                                               (syntax-case binding ()
                                                 [[x e] (state-variable? #'x)]))
                                             #'([x e] (... ...)))])
                      (with-syntax ([([x e] (... ...)) bindings]
                                    [([more-x more-e] (... ...)) more-bindings])
                        #'(letrec ([f (lambda (formal ... more-x (... ...)) . body)])
                            (let ([proc f])
                              (let ([x e] (... ...))
                                (proc formal ... more-e (... ...))))))))]
                 [_ (syntax-violation who "invalid syntax" stx)])))]
        [_ (syntax-violation who "invalid syntax" x)]))))

;; Local Variables:
;; mode: scheme
;; End:
