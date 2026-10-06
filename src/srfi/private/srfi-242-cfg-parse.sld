;;; (srfi private srfi-242-cfg-parse): SRFI 242's reference/srfi-242/cfg/parse.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi cfg parse) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-cfg-parse)
  (export parse
	  define-cfg-label-property)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
	  (srfi private srfi-242-define-property)
          (srfi private srfi-242-cfg-ast)
	  (srfi private srfi-242-cfg-expand)
          (srfi private srfi-242-make-identifier-hashtable)
	  (srfi private srfi-242-formals))
  (begin

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
	  [_ (assert #f)]))))))
