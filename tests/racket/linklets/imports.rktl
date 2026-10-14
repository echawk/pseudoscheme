;;; Imports and exports: one linklet's instance (define-linklet, the
;;; harness's: src/racket/linklet/boot.lisp) imported by the next, with
;;; renaming both ways, and a set! of an exported variable seen through
;;; the instance.
(define-linklet counter
  (linklet
   ()
   (count (incr! increment!) (get get-count))
   (define-values (count) 0)
   (define-values (incr!) (lambda (n) (set! count (+ count n)) count))
   (define-values (get) (lambda () count))))
(linklet
 ((counter (increment! bump!) get-count count))
 (total)
 (define-values (total) (list (bump! 5) (bump! 10)))
 (list total (get-count) count))
