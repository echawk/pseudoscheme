;;; (srfi private srfi-242-list-case): SRFI 242's reference/srfi-242/list-case.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi list-case) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-list-case)
  (export list-case)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
          (srfi private srfi-242-define-who))
  (begin

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
         (syntax-violation who "invalid syntax" x)])))))
