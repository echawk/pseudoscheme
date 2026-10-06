;;; (srfi private srfi-242-cfg-compile): SRFI 242's reference/srfi-242/cfg/compile.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi cfg compile) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-cfg-compile)
  (export compile!
	  $call
	  $execute
	  $finally
	  $halt
	  $label*
	  $labels
	  $permute)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
          (srfi private srfi-242-box)
	  (srfi private srfi-242-cfg-ast)
	  (srfi private srfi-242-cfg-infer-types)
          (srfi private srfi-242-formals)
          (srfi private srfi-242-make-identifier-hashtable)
          (srfi private srfi-242-renamer))
  (begin

  (define-syntax $call
    (syntax-rules ()
      [($call id arg ...)
       (id arg ...)]))

  (define-syntax $execute
    (syntax-rules ()
      [($execute (([var tmp] ...)
		  proc-expr)
		 [formals next-expr] ...)
       ((let ([var tmp] ...) proc-expr)
	(lambda formals next-expr) ...)]))

  ;; FIXME: We need to somehow rename (some) invars.

  (define-syntax $finally
    (syntax-rules ()
      [($finally ([(intmp ...) body]
		  [(var tmp) ...]
		  [(invar orig)  ...]
		  [formals expr])
		 outvar ...)
       (let-values ([(intmp ...) body]
                    [(var) tmp] ...)
         (let ([invar orig] ...)
           (let-values ([formals expr])
             (values outvar ...))))]))

  (define-syntax $label*
    (syntax-rules ()
      [($labels ([id (var ...) init-expr]) body-expr)
       (let ([id (lambda (var ...)
                   init-expr)])
         body-expr)]))

  (define-syntax $labels
    (syntax-rules ()
      [($labels ([id (var ...) init-expr] ...) body-expr)
       (letrec ([id (lambda (var ...)
                      init-expr)] ...)
         body-expr)]))

  (define-syntax $halt
    (syntax-rules ()
      [($halt)
       (values)]))

  (define-syntax $permute
    (syntax-rules ()
      [($permute (id head) (var ...) tail)
       (let ([id (lambda (var ...) tail)])
	 head)]))

  (define compile!
    (lambda (result-expr ast)
      (define label->identifier (renamer))
      (define variable->identifier (renamer))
      (define label-arguments-table (make-identifier-hashtable))
      (define label-arguments
        (lambda (lbl)
          (assert (identifier? lbl))
          (assert (hashtable-ref label-arguments-table lbl #f))))
      (define result-vars (infer-types! ast))
      (define loop-expr
        (let f ([ast ast])
          (cond
           [(call-ast? ast)
            (let ([tgt (call-ast-target ast)])
              (with-syntax ([id (label->identifier tgt)]
                            [(arg ...) (label-arguments tgt)])
                #'($call id arg ...)))]
           [(execute-ast? ast)
            (let ([sigma-set (unbox (execute-ast-sigma ast))])
              (with-syntax
                  ([(var ...) sigma-set]
                   [(tmp ...) (map variable->identifier sigma-set)]
                   [proc-expr (execute-ast-proc-expr ast)]
                   [((formals next-expr) ...)
                    (map
                     (lambda (edge)
                       (with-syntax ([formals
                                      (map-formals variable->identifier (exit-edge-formals edge))]
                                     [next-expr (f (exit-edge-next edge))])
                         #'(formals next-expr)))
                     (execute-ast-exit-edges ast))])
		#'($execute ([(var tmp) ...] proc-expr)
			    [formals next-expr] ...)))]
           [(finally-ast? ast)
            (let ([sigma (unbox (finally-ast-sigma ast))])
              (define input-psi (unbox (finally-ast-psi-input ast)))
	      (define input-epsilon (unbox (finally-ast-epsilon-input ast)))
              (with-syntax ([(var ...) sigma]
                            [(tmp ...) (map variable->identifier sigma)]
                            [body (f (finally-ast-body ast))]
                            [(invar ...) input-psi]
                            [(intmp ...) (map variable->identifier input-epsilon)]
			    [(orig ...) (map variable->identifier input-psi)]
                            [(outvar ...) (map variable->identifier (unbox (finally-ast-psi-output ast)))]
                            [formals (map-formals variable->identifier (finally-ast-formals ast))]
                            [expr (finally-ast-expr ast)])
		#'($finally ([(intmp ...) body]
			     [(var tmp) ...]
			     [(invar orig) ...]
			     [formals expr])
			    outvar ...)))]
           [(halt-ast? ast)
            #'($halt)]
	   [(label*-ast? ast)
	    (let ([bdg (label*-ast-binding ast)])
	      (define lbl (binding-label bdg))
	      (hashtable-set! label-arguments-table lbl
			      (map variable->identifier (unbox (binding-delta bdg))))
	      (with-syntax ([id (label->identifier lbl)]
                            [(var ...) (label-arguments lbl)]
                            [init-expr (f (binding-init bdg))]
                            [body-expr (f (label*-ast-body ast))])
		#'($label* ([id (var ...) init-expr])
			   body-expr)))]
           [(labels-ast? ast)
            (let ([bdg* (labels-ast-bindings ast)])
              (define lbl* (map binding-label bdg*))
              (for-each
               (lambda (bdg lbl)
                 (hashtable-set! label-arguments-table lbl
                                 (map variable->identifier (unbox (binding-delta bdg)))))
               bdg* lbl*)
              (with-syntax ([(id ...) (map label->identifier lbl*)]
                            [((var ...) ...) (map label-arguments lbl*)]
                            [(init-expr ...)
                             (map
                              (lambda (bdg)
                                (f (binding-init bdg)))
                              bdg*)]
                            [body-expr (f (labels-ast-body ast))])
		#'($labels ([id (var ...) init-expr] ...)
			   body-expr)))]
	   [(permute-ast? ast)
	    (let ([bdg (permute-ast-binding ast)])
	      (define lbl (binding-label bdg))
	      (hashtable-set! label-arguments-table lbl
			      (map variable->identifier (unbox (binding-delta bdg))))
	      (with-syntax ([id (label->identifier lbl)]
			    [(var ...) (label-arguments lbl)]
			    [head (f (permute-ast-pending ast))]
			    [tail (f (binding-init bdg))])
		#'($permute (id head)
			    (var ...) tail)))]
           [else (assert #f)])))
      (with-syntax ([result result-expr]
                    [loop loop-expr]
                    [(var ...) result-vars])
        #'(let-values ([(var ...) loop])
            result))))))
