;;; Racket's schemify, running here: a Racket linklet schemified, and the
;;; result run here too.  Load after regexp.scm, known.scm and
;;; schemify.scm (run.sh).

;; Racket's tables of known primitives (primitive/*.ss), with known.scm's
;; constructors: what schemify knows of each primitive
(define-syntax define-primitive-table
  (syntax-rules () [(_ id [prim known] ...) (define id (list (cons 'prim known) ...))]))
(for-each load '("primitive/kernel.ss" "primitive/unsafe.ss" "primitive/flfxnum.ss" "primitive/paramz.ss"
                 "primitive/extfl.ss" "primitive/network.ss" "primitive/futures.ss" "primitive/place.ss"
                 "primitive/foreign.ss" "primitive/linklet.ss" "primitive/internal.ss"))
(define prim-knowns (make-eq-hashtable))
(for-each (lambda (t) (for-each (lambda (p) (hashtable-set! prim-knowns (car p) (cdr p))) t))
          (list kernel-table unsafe-table flfxnum-table paramz-table extfl-table network-table
                futures-table place-table foreign-table linklet-table internal-table))
(printf ";; ~a known primitives\n" (hashtable-size prim-knowns))
(define (strip x)
  (cond [(syntax? x) (strip (syntax-e x))]
        [(pair? x) (cons (strip (car x)) (strip (cdr x)))]
        [else x]))
(define (run-schemify lk)
  (call-with-values
    (lambda ()
      (schemify-linklet lk (box #f) #t 'compile #f #f #t #t #f prim-knowns (hasheq)
                        (lambda (v) #f) (lambda (key) (values #f #f #f)) #f))
    (lambda (impl-lam importss exports new-import-keys importss-abi exports-info)
      (strip impl-lam))))
(define sample
  '(linklet () (fact point? make-point)
     (define-values (fact) (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n))))))
     (define-values (struct:point make-point point? point-ref point-set!)
       (make-struct-type 'point #f 2 0))
     (define-values (total) (let-values ([(a b) (values 1 2)]) (+ a b)))
     (fact 5)))
(define out (run-schemify sample))
(pretty-print out)
;; and run it: the schemified lambda takes the instance's variable
;; reference and a variable for each export
(define exports (map make-instance-variable '(fact point? make-point)))
(define racket-env
  (let* ([ours (library-exports '(racket-lite))]
         [clash (filter (lambda (n) (memq n ours)) (library-exports '(chezscheme)))])
    (environment `(except (chezscheme) ,@clash) '(racket-lite))))
(define result (apply (eval out racket-env) #f exports))
(printf "body's value: ~s\n" result)
(let ([fact (instance-variable-value (car exports))]
      [point? (instance-variable-value (cadr exports))]
      [make-point (instance-variable-value (caddr exports))])
  (printf "(fact 20) => ~s\n" (fact 20))
  (printf "(point? (make-point 1 2)) => ~s, (point? 5) => ~s\n" (point? (make-point 1 2)) (point? 5)))
