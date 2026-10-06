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

(library (srfi cfg expand)
  (export expand
	  call execute finally halt label* labels permute
          define-cfg-syntax-property)
  (import (rnrs)
          (srfi :213)
	  (srfi define-who)
	  (srfi formals))

  (define cfg-syntax-key)

  (define-syntax define-cfg-syntax-property
    (syntax-rules ()
      [(define-cfg-syntax-property kwd transformer-expr)
       (...
	(begin
	  (define-syntax expander
	    (let ([transformer transformer-expr])
	      (lambda (stx)
		(syntax-case stx ()
		  [(_ k ... cfg)
		   (with-syntax ([out (transformer #'cfg)])
		     #'(k ... out))]))))
	  (define-property kwd cfg-syntax-key #'expander)))]))

  (define-syntax/who call
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax/who execute
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax/who finally
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax/who halt
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax/who label*
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax/who labels
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax/who permute
    (lambda (stx)
      (syntax-violation who "invalid use of cfg syntax" stx)))

  (define-syntax expand
    (lambda (stx)
      (define expand-call
	(lambda (k* stx)
	  (define who 'call)
	  (syntax-case stx ()
	    [(_ lbl)
	     (identifier? #'lbl)
	     (with-syntax ([(k ...) k*])
	       #'(k ... (call lbl)))]
	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (define expand-execute
	(lambda (k* stx)
	  (define who 'execute)
	  (syntax-case stx ()
	    [(_ proc-expr [formals cfg] ...)
	     (for-all formals? #'(formals ...))
	     (with-syntax ([(k ...) k*])
	       #'(expand-step expand-execute-step k ... proc-expr [formals ...] (cfg ...) ()))]
	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (define expand-finally
	(lambda (k* stx)
	  (define who 'finally)
	  (syntax-case stx ()
	    [(_ formals expr cfg)
	     (formals? #'formals)
	     (with-syntax ([(k ...) k*])
	       #'(expand-step expand-finally-step k ... formals expr (cfg) ()))]
	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (define expand-halt
	(lambda (k* stx)
	  (define who 'halt)
	  (syntax-case stx ()
	    [(_)
	     (with-syntax ([(k ...) k*])
	       #'(k ... (halt)))]
	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (define expand-label*
	(lambda (k* stx)
	  (define who 'label*)
	  (syntax-case stx ()
	    [(_ ([lbl cfg1]) cfg2)
	     (identifier? #'lbl)
	     (with-syntax ([(k ...) k*])
	       #'(expand-step expand-label*-step k ... (lbl) (cfg1 cfg2) ()))]
	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (define expand-labels
	(lambda (k* stx)
	  (define who 'labels)
	  (syntax-case stx ()
	    [(_ ([lbl cfg1] ...) cfg2)
	     (for-all identifier? #'(lbl ...))
	     (with-syntax ([(k ...) k*])
	       #'(expand-step expand-labels-step k ... (lbl ...) (cfg1 ... cfg2) ()))]
	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (define expand-permute
	(lambda (k* stx)
	  (define who 'permute)
	  (syntax-case stx ()
	    [(_ ([lbl cfg1]) cfg2)
	     (identifier? #'lbl)
	     (with-syntax ([(k ...) k*])
	       #'(expand-step expand-permute-step k ... lbl (cfg1 cfg2) ()))]

	    [_
	     (syntax-violation who "invalid cfg syntax" stx)])))
      (lambda (lookup)
	(define lookup-cfg-syntax
          (lambda (id)
            (guard (exc [(syntax-violation? exc)
                         (syntax-violation #f "unbound keyword" id)])
              (lookup id #'cfg-syntax-key))))
	(define do-expand
	  (lambda (k* kwd stx)
	    (with-syntax ([(k ...) k*]
                          [cfg-stx stx])
	      (cond
	       [(lookup-cfg-syntax kwd) =>
		(lambda (expander-name)
		  (with-syntax ([expander expander-name])
		    #'(expander expand k ... cfg-stx)))]
	       [else
		(syntax-case kwd (call execute finally halt labels label* permute)
		  [call (expand-call k* stx)]
		  [execute (expand-execute k* stx)]
		  [finally (expand-finally k* stx)]
                  [halt (expand-halt k* stx)]
		  [labels (expand-labels k* stx)]
		  [label* (expand-label* k* stx)]
		  [permute (expand-permute k* stx)]
		  [_
		   (syntax-violation #f "invalid cfg syntax keyword" stx kwd)])]))))
	(syntax-case stx ()
	  [(_ k ... cfg-stx)
	   (syntax-case #'cfg-stx ()
	     [kwd
	      (identifier? #'kwd)
	      (do-expand #'(k ...) #'kwd #'cfg-stx)]
	     [(kwd . args)
	      (identifier? #'kwd)
	      (do-expand #'(k ...) #'kwd #'cfg-stx)]
	     [_
	      (syntax-violation #f "invalid cfg syntax" #'cfg-stx)])]
	  [_ (assert #f)]))))

  (define-syntax expand-step
    (syntax-rules ()
      [(expand-step k ... () (cfg ...))
       (k ... (cfg ...))]
      [(expand-step k ... (cfg1 cfg2 ...) cfg3)
       (expand expand-cont k ... (cfg2 ...) cfg3 cfg1)]))

  (define-syntax expand-cont
    (syntax-rules ()
      [(expand-cont k ... cfg2 (cfg3 ...) cfg1)
       (expand-step k ... cfg2 (cfg3 ... cfg1))]))

  (define-syntax expand-execute-step
    (syntax-rules ()
      [(expand-execute-step k ... proc-expr [formals ...] (cfg ...))
       (k ... (execute proc-expr [formals cfg] ...))]))

  (define-syntax expand-finally-step
    (syntax-rules ()
      [(expand-finally-step k ... formals expr (cfg))
       (k ... (finally formals expr cfg))]))

  (define-syntax expand-label*-step
    (syntax-rules ()
      [(expand-label*-step k ... (lbl) (cfg1 cfg2))
       (k ... (label* ([lbl cfg1]) cfg2))]))

  (define-syntax expand-labels-step
    (syntax-rules ()
      [(expand-labels-step k ... (lbl ...) (cfg1 ... cfg2))
       (k ... (labels ([lbl cfg1] ...) cfg2))]))

  (define-syntax expand-permute-step
    (syntax-rules ()
      [(expand-permute-step k ... lbl (cfg1 cfg2))
       (k ... (permute ([lbl cfg1]) cfg2))]))

  )

;; Local Variables:
;; mode: scheme
;; End:
