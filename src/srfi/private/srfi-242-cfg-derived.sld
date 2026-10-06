;;; (srfi private srfi-242-cfg-derived): SRFI 242's reference/srfi-242/cfg/derived.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi cfg derived) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged, but for
;;; the variable %srfi-242-derived added at its end.
(define-library (srfi private srfi-242-cfg-derived)
  (export label* bind permute
          %srfi-242-derived)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
	  (srfi private srfi-242-formals)
          (rename (srfi private srfi-242-cfg-primitive)
                  (permute permute*)
		  (label* label)))
  (begin

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

  ;; PSEUDOSCHEME: referred to by (srfi private srfi-242-cfg), so that
  ;; this library is invoked, registering its cfg syntax, before cfg runs.
  (define %srfi-242-derived #t)))
