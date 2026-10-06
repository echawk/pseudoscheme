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

(library (srfi list-case)
  (export list-case)
  (import (rnrs)
          (srfi define-who))

  (define-syntax/who list-case
    (lambda (x)
      (syntax-case x ()
        [(_ e
            [(a . b) e1 ... e2]
            [() e3 ... e4])
	 (for-all identifier? #'(a b))
         #'(let ([tmp e])
             (cond
              [(pair? tmp)
               (let ([a (car tmp)] [b (cdr tmp)])
                 e1 ... e2)]
              [(null? tmp) e3 ... e4]
              [else
               (assertion-violation 'list-case "invalid list" tmp)]))]
        [_
         (syntax-violation who "invalid syntax" x)]))))

;; Local Variables:
;; mode: scheme
;; End:
