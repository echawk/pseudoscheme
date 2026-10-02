;;; Chez Scheme Version 3.2 hooks.ss

(define eval-hook
   (let ((real-eval *eval*))
      (lambda (x)
         (real-eval x))))

(define expand-install-hook
   (lambda (expand)
      (set! *eval* (lambda (x) (eval-hook (expand x))))))

(define error-hook
   (lambda (who why what)
      (error who "~a ~s" why what)))

(define new-symbol-hook
   (lambda (string)
      (string->uninterned-symbol string)))

(define put-global-definition-hook
   (lambda (symbol binding)
      (putprop symbol '*macro-transformer* binding)))

(define get-global-definition-hook
   (lambda (symbol)
      (getprop symbol '*macro-transformer*)))
