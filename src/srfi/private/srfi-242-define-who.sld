;;; (srfi private srfi-242-define-who): SRFI 242's reference/srfi-242/define-who.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi define-who) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-define-who)
  (export define/who define-syntax/who)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
          (srfi private srfi-242-with-implicit))
  (begin

  (define-syntax define/who
    (lambda (x)
      (define out
        (lambda (k f e)
          (with-syntax ((k k) (f f) (e e))
            (with-implicit (k who)
              #'(define f
                  (let ((who 'f)) e))))))
      (syntax-case x ()
        ((k (f . u*) e e* ...)
	 (identifier? #'f)
	 (out #'k #'f #'(lambda u* e e* ...)))
        ((k f e)
	 (identifier? #'f)
         (out #'k #'f #'e))
        (_
         (syntax-violation 'define/who "invalid syntax" x)))))

  (define-syntax define-syntax/who
    (lambda (x)
      (syntax-case x ()
	[(k name expr)
	 (identifier? #'name)
	 (with-implicit (k who)
	   #'(define-syntax name
	       (let ([who 'name])
		 expr)))]
	[_
	 (syntax-violation 'define-syntax/who "invalid syntax" x)])))))
