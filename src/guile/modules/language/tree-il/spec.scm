;;; (language tree-il spec) for Pseudoscheme.  Guile's compiles Tree-IL
;;; to CPS and bytecode; here Tree-IL compiles to Common Lisp through
;;; Pseudoscheme's core Scheme (src/guile/compile.lisp), so the language
;;; compiles straight to `value'.  Written for Pseudoscheme (not derived
;;; from Guile's source).

(define-module (language tree-il spec)
  #:use-module (system base language)
  #:use-module (language tree-il)
  #:export (tree-il))

(define (write-tree-il exp . port)
  (apply write (unparse-tree-il exp) port))

(define (join exps env)
  (cond ((null? exps) (make-void #f))
        ((null? (cdr exps)) (car exps))
        (else (make-seq #f (car exps) (join (cdr exps) env)))))

(define (compile-value exp env opts)
  (let ((value (%eval-tree-il exp env)))
    (values value env env)))

(define-language tree-il
  #:title       "Tree Intermediate Language"
  #:reader      (lambda (port env) (read port))
  #:printer     write-tree-il
  #:parser      parse-tree-il
  #:joiner      join
  #:compilers   `((value . ,compile-value))
  #:for-humans? #f)
