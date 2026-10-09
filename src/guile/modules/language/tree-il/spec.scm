;;; (language tree-il spec) for Pseudoscheme.  Guile's goes from Tree-IL
;;; to CPS or bytecode, for its VM, and so does this one, to `value' too
;;; (the bytecode loaded and run by the VM); or, if
;;; %compile-value-via-bytecode? says not, Tree-IL goes to `value'
;;; directly, compiled to native code by the host (%eval-tree-il).  Compiling to CPS
;;; or bytecode, as Guile's tests of its compiler do, chooses Guile's own
;;; passes, as Guile's spec does; and Tree-IL is analyzed (for warnings)
;;; by Guile's own analyzer.  Guile's lowerer (peval and the rest) runs
;;; only before its own passes: what it makes of dynamic extents
;;; (push-fluid and pop-fluid, prompts) is for its VM.  Written for
;;; Pseudoscheme (not derived from Guile's source).

(define-module (language tree-il spec)
  #:use-module (system base language)
  #:use-module (language tree-il)
  #:use-module ((language tree-il analyze) #:select (make-analyzer))
  #:use-module ((language tree-il optimize) #:select (make-lowerer))
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

;; Whether the compiler last chosen is Guile's: (system base compile)
;; makes the lowerer just after choosing the compiler.
(define lowering-for-guile? #f)

(define (lower optimization-level opts)
  (if lowering-for-guile?
      (make-lowerer optimization-level opts)
      (lambda (exp env) exp)))

(define (choose-compiler target optimization-level opts)
  (define (load-compiler compiler)
    (module-ref (resolve-interface `(language tree-il ,compiler)) compiler))
  (define via-host?
    (and (eq? (language-name target) 'value)
         (not (%compile-value-via-bytecode?))))
  (set! lowering-for-guile? (not via-host?))
  (cond
   (via-host?
    (cons 'value compile-value))
   ((let ((cps? (memq #:cps? opts)))
      (if cps? (cadr cps?) (<= 2 optimization-level)))
    (cons 'cps (load-compiler 'compile-cps)))
   (else
    (cons 'bytecode (load-compiler 'compile-bytecode)))))

(define-language tree-il
  #:title             "Tree Intermediate Language"
  #:reader            (lambda (port env) (read port))
  #:printer           write-tree-il
  #:parser            parse-tree-il
  #:joiner            join
  #:compiler-chooser  choose-compiler
  #:analyzer          make-analyzer
  #:lowerer           lower
  #:for-humans?       #f)
