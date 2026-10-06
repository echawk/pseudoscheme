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

(library (srfi cfg parse)
  (export parse
	  define-cfg-label-property)
  (import (rnrs)
	  (srfi :213 define-property)
          (srfi cfg ast)
	  (srfi cfg expand)
          (srfi make-identifier-hashtable)
	  (srfi formals))

  ;; Labels

  (define cfg-label-key)

  (define-syntax define-cfg-label-property
    (syntax-rules ()
      [(define-cfg-syntax-property kwd)
       (define-property kwd cfg-label-key
	 (with-syntax ([(label) (generate-temporaries '(kwd))])
	   #'label))]))

  ;; Environments

  (define empty-environment
    (lambda ()
      '()))

  (define add-frame
    (lambda (env id* lbl*)
      (define frame (make-identifier-hashtable))
      (for-each
       (lambda (id lbl)
	 (when (hashtable-ref frame id #f)
	   (syntax-violation #f "multiple label" id))
	 (hashtable-set! frame id lbl))
       id* lbl*)
      (cons frame env)))

  (define environment-lookup
    (lambda (env id)
      (let f ([env env])
        (unless (pair? env)
          (syntax-violation #f "undefined label" id))
        (or (hashtable-ref (car env) id #f)
            (f (cdr env))))))

  ;; Parser

  (define parse
    (lambda (lookup cfg-term)
      (define lookup-cfg-label
	(lambda (id)
	  (or (guard (exc [(syntax-violation? exc) #f])
		(lookup id #'cfg-label-key))
	      id)))
      (let f ([cfg-frag cfg-term] [env (empty-environment)])
	(define g (lambda (cfg-frag) (f cfg-frag env)))
	(syntax-case cfg-frag (call execute finally halt label* labels permute)
	  [(call tgt)
	   (identifier? #'tgt)
	   (make-call-ast (environment-lookup env (lookup-cfg-label #'tgt)))]
	  [(execute proc-expr [formals next-cfg-frag] ...)
	   (for-all formals? #'(formals ...))
	   (make-execute-ast #'proc-expr #'(formals ...) (map g #'(next-cfg-frag ...)))]
	  [(finally formals expr body-cfg-frag)
	   (formals? #'formals)
	   (make-finally-ast #'formals #'expr (g #'body-cfg-frag))]
	  [(halt)
	   (make-halt-ast)]
	  [(label* [(id init-cfg-frag)] body-cfg-frag)
	   (identifier? #'lbl)
	   (let ([lbl (car (generate-temporaries #'(id)))])
	     (define extended-env (add-frame env (list (lookup-cfg-label #'id)) (list lbl)))
	     (make-label*-ast lbl (f #'init-cfg-frag env) (f #'body-cfg-frag extended-env)))]
	  [(labels [(id init-cfg-frag) ...] body-cfg-frag)
	   (for-all identifier? #'(id ...))
	   (let ([lbl* (generate-temporaries #'(id ...))])
	     (define extended-env (add-frame env (map lookup-cfg-label #'(id ...)) lbl*))
	     (make-labels-ast lbl*
			      (map (lambda (cfg-frag)
				     (f cfg-frag extended-env))
				   #'(init-cfg-frag ...))
			      (f #'body-cfg-frag extended-env)))]
	  [(permute ([id cfg1]) cfg2)
	   (identifier? #'id)
	   (let ([lbl (car (generate-temporaries #'(id)))])
	     (define extended-env (add-frame env (list (lookup-cfg-label #'id)) (list lbl)))
	     (make-permute-ast lbl (f #'cfg1 extended-env) (f #'cfg2 env)))]
	  [_ (assert #f)]))))

 )

;; Local Variables:
;; mode: scheme
;; End:
