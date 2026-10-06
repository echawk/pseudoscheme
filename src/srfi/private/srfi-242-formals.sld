;;; (srfi private srfi-242-formals): SRFI 242's reference/srfi-242/formals.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi formals) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-formals)
  (export formals?
	  formals->list
          map-formals)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax))
  (begin

  (define formals?
    (lambda (stx)
      (syntax-case stx ()
	[(var ... )
	 (for-all identifier? #'(var ...))
	 #t]
	[(var1 ... . var2)
	 (for-all identifier? #'(var1 ... var2))
	 #t])))

  (define formals->list
    (lambda (stx)
      (syntax-case stx ()
	[(var ... )
	 (for-all identifier? #'(var ...))
	 #'(var ...)]
	[(var1 ... . var2)
	 (for-all identifier? #'(var1 ... var2))
	 #'(var1 ... var2)]
	[_ (assert #f)])))

  (define map-formals
    (lambda (proc stx)
      (syntax-case stx ()
	[(var ... )
	 (for-all identifier? #'(var ...))
	 (map proc #'(var ...))]
	[(var1 ... . var2)
	 (for-all identifier? #'(var1 ... var2))
	 (append (map proc #'(var1 ...)) (proc #'var2))]
	[_ (assert #f)])))))
