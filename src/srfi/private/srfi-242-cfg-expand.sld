;;; (srfi private srfi-242-cfg-expand): SRFI 242's reference/srfi-242/cfg/expand.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi cfg expand) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-cfg-expand)
  (export expand
	  call execute finally halt label* labels permute
          define-cfg-syntax-property)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
          (srfi private srfi-242-define-property)
	  (srfi private srfi-242-define-who)
	  (srfi private srfi-242-formals))
  (begin

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
       (k ... (permute ([lbl cfg1]) cfg2))]))))
