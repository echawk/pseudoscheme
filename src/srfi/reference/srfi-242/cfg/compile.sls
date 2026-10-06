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

(library (srfi cfg compile)
  (export compile!
	  $call
	  $execute
	  $finally
	  $halt
	  $label*
	  $labels
	  $permute)
  (import (rnrs)
          (srfi box)
	  (srfi cfg ast)
	  (srfi cfg infer-types)
          (srfi formals)
          (srfi make-identifier-hashtable)
          (srfi renamer))

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
            result))))

  )

;; Local Variables:
;; mode: scheme
;; End:
