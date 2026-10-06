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

(library (srfi cfg derived)
  (export label* bind permute)
  (import (rnrs)
	  (srfi formals)
          (rename (srfi cfg primitive)
                  (permute permute*)
		  (label* label)))

  (define-cfg-syntax permute
    (lambda (stx)
      (define who 'permute)
      (syntax-case stx ()
        [(_ [(lbl cfg1) ...] cfg2)
         (for-all identifier? #'(lbl ...))
         (fold-right
          (lambda (lbl head tail)
            (with-syntax ([lbl lbl] [head head] [tail tail])
              #'(permute* ([lbl head]) tail)))
          #'cfg2 #'(lbl ...) #'(cfg1 ...))]
        [_
         (syntax-violation who "invalid permute syntax" stx)]

        )))

  (define-cfg-syntax bind
    (lambda (stx)
      (define who 'bind)
      (syntax-case stx ()
	[(_ ([formals expr] ...) cfg)
	 (for-all formals? #'(formals ...))
	 (with-syntax ([((var ...) ...)
			(map formals->list #'(formals ...))])
	   #'(execute (lambda (e)
			(let-values ([formals expr] ...)
			  (e var ... ... )))
	       [(var ... ...) cfg]))]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  (define-cfg-syntax label*
    (lambda (stx)
      (define who 'label*)
      (syntax-case stx ()
	[(_ ([lbl cfg1] ...) cfg2)
	 (for-all identifier? #'(lbl ...))
	 (fold-right
	  (lambda (lbl cfg1 cfg2)
	    (with-syntax ([lbl lbl] [cfg1 cfg1] [cfg2 cfg2])
	      #'(label ([lbl cfg1]) cfg2)))
	  #'cfg2 #'(lbl ...) #'(cfg1 ...))]
	[_
	 (syntax-violation who "invalid syntax" stx)])))

  )

;; Local Variables:
;; mode: scheme
;; End:
