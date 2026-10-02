;;; hooks.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;; This file contains procedures that are best defined using
;;; nonstandard features.
;;; The following work in Chez Scheme.

;;; eval-hook should be a one-argument "eval".  It is used to evaluate
;;; macro definitions during macro expansion.  Since it receives
;;; an expression that has already been expanded, there is no need
;;; to reexpand.

(define eval-hook
   (let ((real-eval (current-eval)))
      (lambda (x)
         (real-eval x))))

;;; expand-install-hook takes a one-argument "expand" procedure as
;;; an argument, and installs it as the system expander to be
;;; invoked on all expressions prior to evaluation.  In Chez Scheme,
;;; we redefine the current evaluator (used by the read-eval-print
;;; loop and load) to invoke expand and call eval-hook on the result.

(define expand-install-hook
   (lambda (expand)
      (current-eval (lambda (x) (eval-hook (expand x))))))

;;; In Chez Scheme, the following reports:
;;;           "Error in <who>: <why> <what>."
;;; "who" is a symbol, "why" is a string, and "what" is an arbitrary object.

(define error-hook
   (lambda (who why what)
      (error who "~a ~s" why what)))

;;; New symbols are used to generate non-capturing bindings.  If it is
;;; impossible to generate unique symbols, output identifiers during
;;; expansion and either feed the result directly into the compiler or
;;; make another pass to perform alpha substitution.

(define new-symbol-hook
   (lambda (string)
      (string->uninterned-symbol string)))

;;; "put-global-definition-hook" should overwrite existing definitions.

(define put-global-definition-hook
   (lambda (symbol binding)
      (putprop symbol '*macro-transformer* binding)))

;;; "get-global-definition-hook" should return "#f" if no binding
;;; has been established "put-global-definition-hook" for the symbol.

(define get-global-definition-hook
   (lambda (symbol)
      (getprop symbol '*macro-transformer*)))
