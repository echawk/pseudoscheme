;;; (srfi private srfi-242-cfg-ast): SRFI 242's reference/srfi-242/cfg/ast.sls
;;; (Marc Nieper-Wißkirchen; MIT licence, in reference/srfi-242/LICENSE),
;;; as an R7RS library.  Changes, made mechanically: the library name
;;; (srfi cfg ast) and those it imports are renamed (see 242.sld); R6RS
;;; (rename (a b)) exports are R7RS (rename a b); define-syntax and
;;; syntax come from (srfi private srfi-242-define-property), which
;;; gives macro transformers SRFI 213's lookup procedure and makes
;;; syntax's lists proper, as Chez's are.  The body is unchanged.
(define-library (srfi private srfi-242-cfg-ast)
  (export ast? binding?
          binding-label binding-init
	  binding-delta binding-rho binding-sigma
          binding-psi binding-phi binding-epsilon
	  exit-edge?
          exit-edge-formals exit-edge-next
          make-label*-ast label*-ast?
          label*-ast-binding label*-ast-body
          make-labels-ast labels-ast?
          labels-ast-bindings labels-ast-body
          make-execute-ast execute-ast?
	  execute-ast-sigma
          execute-ast-proc-expr execute-ast-exit-edges
          make-finally-ast finally-ast?
          finally-ast-formals finally-ast-expr
          finally-ast-body
          finally-ast-sigma
          finally-ast-psi-input
          finally-ast-psi-output
	  finally-ast-epsilon-input
          make-call-ast call-ast? call-ast-target
          make-halt-ast halt-ast?
	  make-permute-ast permute-ast?
	  permute-ast-pending permute-ast-binding)
  (import (except (rnrs) define-syntax syntax)
          (only (srfi private srfi-242-define-property) define-syntax syntax)
	  (srfi private srfi-242-box)
          (srfi private srfi-242-formals))
  (begin

  (define-record-type ast (nongenerative) (opaque #t))

  (define identifier-list?
    (lambda (obj)
      (and (list? obj) (for-all identifier? obj))))

  (define ast-list?
    (lambda (obj)
      (and (list? obj) (for-all ast? obj))))

  (define formals-list?
    (lambda (obj)
      (and (list? obj) (for-all formals? obj))))

  (define-record-type binding
    (nongenerative) (sealed #t) (opaque #t)
    (fields label delta rho sigma psi phi epsilon init)
    (protocol
     (lambda (p)
       (lambda (lbl init)
         (assert (identifier? lbl))
         (assert (ast? init))
         (p lbl (box 0) (box -1) (box -1) (box -1) (box -1) (box -1) init)))))

  (define-record-type labels-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (fields bindings body)
    (protocol
     (lambda (n)
       (lambda (lbl* init* body)
         (assert (identifier-list? lbl*))
         (assert (ast-list? init*))
         (assert (ast? body))
         ((n) (map make-binding lbl* init*) body)))))

  (define-record-type exit-edge
    (nongenerative) (sealed #t) (opaque #t)
    (fields formals next)
    (protocol
     (lambda (p)
       (lambda (formals next)
         (assert (formals? formals))
         (assert (ast? next))
         (p formals next)))))

  (define-record-type execute-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (fields sigma proc-expr exit-edges)
    (protocol
     (lambda (n)
       (lambda (proc-expr formals* next*)
         (assert (formals-list? formals*))
         (assert (ast-list? next*))
         ((n) (box -1) proc-expr (map make-exit-edge formals* next*))))))

  (define-record-type finally-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (fields sigma psi-input psi-output epsilon-input formals expr body)
    (protocol
     (lambda (n)
       (lambda (formals expr body)
         (assert (formals? formals))
         (assert (ast? body))
         ((n) (box -1) (box -1) (box -1) (box -1) formals expr body)))))

  (define-record-type call-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (fields target)
    (protocol
     (lambda (n)
       (lambda (tgt)
         (assert (identifier? tgt))
         ((n) tgt)))))

  (define-record-type halt-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (protocol
     (lambda (n)
       (lambda ()
         ((n))))))

  (define-record-type permute-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (fields pending binding)
    (protocol
     (lambda (n)
       (lambda (label pending tail)
	 (assert (identifier? label))
	 (assert (ast? pending))
	 (assert (ast? tail))
	 ((n) pending (make-binding label tail))))))

  (define-record-type label*-ast
    (nongenerative) (sealed #t) (opaque #t)
    (parent ast)
    (fields binding body)
    (protocol
     (lambda (n)
       (lambda (lbl init body)
	 (assert (identifier? lbl))
	 (assert (ast? init))
	 (assert (ast? body))
	 ((n) (make-binding lbl init) body)))))))
