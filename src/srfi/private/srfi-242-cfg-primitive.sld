;;; (srfi private srfi-242-cfg-primitive): SRFI 242's reference/srfi-242/cfg/primitive.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi cfg primitive) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-cfg-primitive)
  (export cfg
	  call execute finally halt label* labels permute
	  define-cfg-label define-cfg-label*
          define-cfg-syntax define-cfg-syntax*)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
	  (srfi private srfi-242-cfg-compile)
	  (srfi private srfi-242-cfg-parse)
	  (srfi private srfi-242-cfg-expand)
	  (srfi private srfi-242-define-who))
  (begin

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
         (syntax-violation who "invalid syntax" stx)])))))
