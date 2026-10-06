#!r6rs

;; © 2022 Marc Nieper-Wißkirchen.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including
;; the next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi cfg primitive)
  (export cfg
	  call execute finally halt label* labels permute
	  define-cfg-label define-cfg-label*
          define-cfg-syntax define-cfg-syntax*)
  (import (rnrs)
	  (srfi cfg compile)
	  (srfi cfg parse)
	  (srfi cfg expand)
	  (srfi define-who))

  (define-syntax/who cfg
    (lambda (stx)
      (syntax-case stx ()
	[(_ cfg-term result-expr)
	 #'(expand cfg-step result-expr cfg-term)]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define-syntax cfg-step
    (lambda (stx)
      (syntax-case stx ()
        [(_ expr cfg-term)
         (lambda (lookup)
           (let* ([ast (parse lookup #'cfg-term)])
	     ;; TODO: Optimize by determining SCCs.
	     (compile! #'expr ast)))]
        [_ (assert #f)])))

  (define-syntax/who define-cfg-label
    (lambda (stx)
      (syntax-case stx ()
	[(_ name)
	 (identifier? #'name)
	 #'(begin
	     (define-syntax/who name
	       (lambda (stx)
		 (syntax-violation who "invalid use of cfg label" stx)))
	     (define-cfg-label* name))]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who define-cfg-label*
    (lambda (stx)
      (syntax-case stx ()
	[(_ name)
	 (identifier? #'name)
	 #'(define-cfg-label-property name)]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who define-cfg-syntax
    (lambda (stx)
      (syntax-case stx ()
        [(_ name transformer-expr)
         (identifier? #'name)
         #'(begin
             (define-syntax/who name
               (lambda (stx)
                 (syntax-violation who "invalid use of cfg syntax" stx)))
             (define-cfg-syntax* name transformer-expr))]
        [_
         (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who define-cfg-syntax*
    (lambda (stx)
      (syntax-case stx ()
        [(_ name transformer-expr)
         (identifier? #'name)
         #'(define-cfg-syntax-property name
             (let ([transformer transformer-expr])
               (unless (procedure? transformer)
                 (assertion-violation 'define-cfg-syntax* "invalid cfg syntax transformer" transformer))
               transformer))]
        [_
         (syntax-violation who "invalid syntax" stx)])))

  )

;; Local Variables:
;; mode: scheme
;; End:
